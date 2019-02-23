@echo off
cd /d %~dp0\..

REM *** Target
set build_bit=32
set build_platform=Win32
set build_type=release
set build_name=npt


REM *** Microsoft Visual Studio 2017
set build_vc2017=Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build
set build_base=%ProgramFiles(x86)%\%build_vc2017%
set build_call=vcvars%build_bit%.bat
set build_file=%build_base%\%build_call%
set LISP_WINDOWS_COMPILE=%~dp0\..


REM *** Project file
echo Windows %build_bit%bit Build
call "%build_file%"
if errorlevel 1 (
	echo call error: %build_call%
	goto error
)


REM *** Clean
set build_proj=build\windows_degrade.cproj
msbuild %build_proj% /t:clean
if errorlevel 1 (
	echo msbuild clean error
	goto error
)


REM *** Build
set build_proj=build\windows_degrade.cproj
msbuild %build_proj% /t:build /p:configuration=%build_type% /p:platform=%build_platform% /property:TargetName=%build_name%
if errorlevel 1 (
	echo msbuild build error
	goto error
)


REM *** Finish
pause
exit /b 0
:error
pause
exit /b 1

