open System.IO
open System.Diagnostics
open System
open System.Text.RegularExpressions
open Argu
open YamlDotNet.Core
open YamlDotNet.Serialization

module Os = 
    let WithDir p =
        let old = Directory.GetCurrentDirectory()
        Directory.SetCurrentDirectory(p)
        {new IDisposable with
            member x.Dispose() = Directory.SetCurrentDirectory old}

module Subprocess = 
    exception ProcessFailedException of string*string*string[]
    
    let mutable Verbose = false

    let patchProcInfo (pi: ProcessStartInfo) = 
        pi.UseShellExecute <- false
        pi.RedirectStandardOutput <- true
        pi.RedirectStandardError <- true
        pi.WindowStyle <- ProcessWindowStyle.Hidden
        pi.WorkingDirectory <- Directory.GetCurrentDirectory()

        
    let Exec cmd args =
        if Verbose then 
            printfn "> %s %s" cmd args
        use proc = new Process()
        patchProcInfo proc.StartInfo
        let out = ResizeArray<string>()
        let err = ResizeArray<string>()

        proc.StartInfo.FileName <- cmd
        proc.StartInfo.Arguments <- args

        proc.OutputDataReceived.Add(fun d -> out.Add(d.Data))
        proc.ErrorDataReceived.Add(fun d -> err.Add(d.Data)) 

        let started = proc.Start()
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()
        proc.WaitForExit()
        let skipNulls = Seq.filter (fun e -> not <| obj.ReferenceEquals(e, null))
        proc.ExitCode, Array.ofSeq <| skipNulls out , Array.ofSeq <| skipNulls err  

    let CheckOutput cmd args =
        let status, out, err = Exec cmd args
        if status <> 0 then raise <| ProcessFailedException(cmd, args, err)
        out

module String =
    // omg it's hard
    let iterLines (s: string) = seq {
        use rd = new StringReader(s)
        let mutable exit = false
        while not exit do
            let line = rd.ReadLine()
            match line with 
            | null -> exit <- true
            | l -> yield l
}

type ModuleYaml() =
    member val Mod = "" with get,set
    member val Dirs = ResizeArray<string>() with get,set


type ModuleSpec = {
    Name: string
    Dirs: string[]
}
with
    static member FromYaml(ent: ModuleYaml) =
        {
            Name = ent.Mod
            Dirs = ent.Dirs.ToArray()
        }


type RuleOp =
| If 
| IfAnythingElse

type RuleYaml() = 
    member val Target = "" with get,set
    member val Op = "" with get,set
    member val Mods = ResizeArray<string>() with get,set
    member val If = ResizeArray<string>() with get,set
    member val IfAnythingElse = ResizeArray<string>() with get,set



type ConfigYaml() =
    member val Modules = ResizeArray<ModuleYaml>() with get,set
    member val Rules = ResizeArray<RuleYaml>() with get,set



type RuleSpec = {
    Target: string
    Op: RuleOp
    Mods: string[]
}
with
    static member FromYaml(ent: RuleYaml) = 
        let op, listprop = match ent with
                       | _ when ent.If.Count > 0 -> If, ent.If
                       | _ when ent.IfAnythingElse.Count > 0 -> IfAnythingElse,  ent.IfAnythingElse

                       
        {
            Target = ent.Target
            Mods = listprop.ToArray()
            Op = op 
        }


let getReader fname =
    let file = new FileStream(fname, FileMode.Open, FileAccess.Read)
    new StreamReader(file) 

let readYaml (pth: string) = 
    let rd = new Deserializer()
    use reader = getReader pth
    try
        let res = rd.Deserialize<ConfigYaml> reader
        Some res
    with 
    | :? YamlException as e -> 
        printfn "Yaml parse error:\n%s" e.Message
        None


let readConfig (pth: string) =
    let yaml = readYaml pth

    let mods = yaml.Value.Modules |> Seq.map ModuleSpec.FromYaml
    let rules = yaml.Value.Rules |> Seq.map RuleSpec.FromYaml
    mods, rules

let dirtyFilesFromGit (fromBranch: string) (toBranch: string) =

    (Subprocess.CheckOutput "git" (sprintf "diff --name-only %s...%s" toBranch fromBranch)) 

module Git =
    let ParseCommit (ref: string) =
        let out = Subprocess.CheckOutput "git" (sprintf "show %s" ref)
        out 
        |> Array.choose(fun l -> 
                        let matched = Regex.Match(l, @"^(\w+):(.*)$")
                        if (not matched.Success) then None else
                            (matched.Groups.[1].Value, matched.Groups.[2].Value.Trim()) |> Some               
                        ) 
    let ParseMergeCommit (ref: string) = 
        let parsed = ParseCommit ref
        let mergeval = parsed |> Array.find (fun e -> (fst e) = "Merge") |> snd |> fun s -> s.Split(' ') |> fun arr -> (arr.[1], arr.[0])
        mergeval


let matchRules (mods: string[]) (rules: RuleSpec seq) =
    let modSet = Set.ofArray mods
    seq {
        for r in rules do
            let tries = Set.ofArray r.Mods

            match r.Op with 
            | If -> 
                if not <| Set.isEmpty (Set.intersect tries modSet) then
                    yield r.Target
            | IfAnythingElse -> 
                if not <| Set.isEmpty (Set.difference modSet tries) then
                    yield r.Target


    }
    
let scanModules modSpec fromBranch toBranch = 
    let root = Subprocess.CheckOutput "git" "rev-parse --show-toplevel" |> Array.head
    let files = dirtyFilesFromGit fromBranch toBranch 
                |> Array.map (fun f -> f.ToLowerInvariant())
                |> Set.ofArray

    //let mods, rules = readConfig configFile
    let scanForOne (trie: string) (arr: string Set) =
        arr |> Set.filter (fun l -> not (l.StartsWith trie || Regex.IsMatch (l, trie, RegexOptions.IgnoreCase)))

    let scanForMany (tries: string[]) (arr: string Set) =
        tries |> Array.fold (fun remainingArr s -> scanForOne s remainingArr) arr
    
    let mutable remaining = files
    let emittedModules =
        seq {
            for m in modSpec do
                let filtered = scanForMany m.Dirs remaining
                let diff = Set.difference remaining filtered
                if filtered.Count < remaining.Count then 
                    yield (m.Name, diff)
                remaining <- filtered
        } |> Array.ofSeq
    
    emittedModules, remaining, files

let findTargets ruleSpec modules = 
    let targets = matchRules (modules |> Array.map fst) ruleSpec |> Array.ofSeq
    targets

type CLIArguments = 
    | From of string
    | To of string
    | Commit of string
    | [<Mandatory>] Config of string
    | Dir of string
    | Modules
    | Targets
    | Files
    | Leftover
    | Verbose
with
    interface IArgParserTemplate with
        member s.Usage = 
            match s with 
            | From _ -> "Source branch"
            | To _ -> "Target branch"
            | Commit _ -> "Specify merge commit to analyze"
            | Config _ -> "Config .yaml file with module descriptions"
            | Dir _ -> "Directory to run in"
            | Modules -> "Show changed modules"
            | Targets -> "Show rules triggered by changed modules"
            | Files -> "Show list of modules and associated files"
            | Leftover -> "Show unrecognized files"
            | Verbose -> "Log diagnostics to console"



exception InvalidCli of string

let handleCli (res: ParseResults<CLIArguments>) =
    let dir = res.GetResult(<@ Dir @>, ".")

    if res.Contains <@ Verbose @> then
        Subprocess.Verbose <- true

    let ensureFile argname f = 
        match f with
        | None -> 
            failwithf "Need to specify argument '%s'" argname
        | Some f ->
            if File.Exists f then f else failwithf "Argument %s, file doesn't exist: %s" argname f

    let modSpec, ruleSpec = readConfig (res.TryGetResult(<@ Config @>) |> ensureFile "--config")

    use d = Os.WithDir dir

    let mutable fromRef = res.GetResult( <@ From @>, "")
    let mutable toRef = res.GetResult( <@ To @>, "")

    let mergeCommit = res.TryGetResult <@ Commit @>
    if (mergeCommit.IsSome) then
        let (p1, _) = Git.ParseMergeCommit mergeCommit.Value
        
        fromRef <- p1
        toRef <- mergeCommit.Value

    if fromRef = "" || toRef = "" then
        raise <| InvalidCli("Need to specify either --commit or --from and --to")
        
    let modules, leftovers, files = scanModules modSpec fromRef toRef

    if res.Contains <@ Modules @> then
        modules |> Seq.iter (fst >> (printfn "%s"))
    if res.Contains <@ Targets @> then
        let targets = findTargets ruleSpec modules
        targets |> Seq.iter (printfn "%s")
    if res.Contains <@ Files @> then
        modules |> Seq.iter (fun (m, files) ->
            printfn "%s" m
            files |> Set.iter (printfn "    %s"))
    if res.Contains <@ Leftover @> then
        leftovers |> Seq.iter (printfn "%s")

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "modulize.exe")
    try
        let res = parser.Parse argv
        handleCli res
        0
    with
        | :? ArguParseException as e -> 
            printfn "%s" e.Message
            1
        | InvalidCli(message) ->
            printfn "%s" message
            1

        | Subprocess.ProcessFailedException(cmd, args, err) ->
            printfn "FAILED: %s %s\n%A" cmd args err 
            2


            
