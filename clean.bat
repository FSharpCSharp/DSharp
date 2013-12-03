@echo off
pushd %~dp0
echo Cleaning files ...
echo.

del /S /Q *.tvsconfig >nul 2>&1
del /S /Q *.identcache >nul 2>&1
del /S /Q *.local >nul 2>&1
del /S /Q *.2007 >nul 2>&1
del /S /Q *.???_ >nul 2>&1
del /S /Q *.~* >nul 2>&1
del /S /Q *.bak >nul 2>&1
del /S /Q *.bk? >nul 2>&1
del /S /Q *.cfg >nul 2>&1
del /S /Q *.dcu >nul 2>&1
del /S /Q *.ddp >nul 2>&1
del /S /Q *.dof >nul 2>&1
del /S /Q *.dpu >nul 2>&1
del /S /Q *.drc >nul 2>&1
del /S /Q *.dsk >nul 2>&1
del /S /Q *.elf >nul 2>&1
del /S /Q *.kof >nul 2>&1
del /S /Q *.log >nul 2>&1
del /S /Q *.mad >nul 2>&1
del /S /Q *.map >nul 2>&1
del /S /Q *.mes >nul 2>&1
del /S /Q *.mps >nul 2>&1
del /S /Q *.mpt >nul 2>&1
del /S /Q *.prf >nul 2>&1
del /S /Q *.stat >nul 2>&1
del /S /Q *.tci >nul 2>&1
del /S /Q *.tmp >nul 2>&1
del /S /Q /A Thumbs.db >nul 2>&1

echo Removing build folders ...
echo.

for /d /r . %%d in (__history Debug Release) do @if exist "%%d" echo "%%d" && rd /Q /S "%%d"

echo Done!
echo.
popd
pause