call %~dp0Dependencies-Set.bat
@echo on
dir /s /b *.dproj > All.Dproj.files.txt 
%BeSharpNet%\Native\Delphi\Apps\Console\GenerateGroupProjConsoleProject\Win32\Debug\GenerateGroupProjConsoleProject.exe All.groupproj All.Dproj.files.txt
pause

