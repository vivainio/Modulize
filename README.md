# Modulize
Map source trees changes (from git) to dir based modules, and map those to "build tasks"

## Install

```
$ zippim get https://github.com/vivainio/Modulize/releases/download/v0.1.1/Modulize.zip
```

## Usage
```
USAGE: modulize.exe [--help] [--from <string>] [--to <string>] [--commit <string>] --config <string>
                    [--dir <string>] [--modules] [--targets] [--files] [--leftover]

OPTIONS:

    --from <string>       Source branch
    --to <string>         Target branch
    --commit <string>     Specify merge commit to analyze
    --config <string>     Config .yaml file with module descriptions
    --dir <string>        Directory to run in
    --modules             Show changed modules
    --targets             Show rules triggered by changed modules
    --files               Show list of modules and associated files
    --leftover            Show unrecognized files
    --help                display this list of options.
```    
    
