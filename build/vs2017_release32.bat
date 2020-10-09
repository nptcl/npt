@echo off
set build_path=%~dp0
cd /d %build_path%


REM *** Microsoft Visual Studio 2017
set build_mode=release
set build_bit=32

set build_vs2017=Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build
set build_base=%ProgramFiles(x86)%\%build_vs2017%
set build_call=vcvars%build_bit%.bat
set build_file=%build_base%\%build_call%
set build_batch=windows_%build_mode%%build_bit%.bat

echo %build_batch% Build
call "%build_file%"
if errorlevel 1 (
	echo call error: %build_call%
	goto error
)


REM *** Call
call %build_batch% no-clean
if errorlevel 1 (
	echo build error
	goto error
)


REM *** Finish
pause
exit /b 0
:error
pause
exit /b 1

