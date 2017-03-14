setlocal
set P=Modulize
msbuild %P%.sln /p:Configuration=Release
cd %P%\bin
rm -R %P%
rm %P%.zip
mv Release %P%
zip -ur %P%.zip %P%