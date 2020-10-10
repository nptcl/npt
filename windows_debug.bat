@echo off
set build_path=%~dp0
cd /d %build_path%


REM *** Target
set build_mode=debug
set build_make=build\Makefile.windows_%build_mode%


REM *** Clean
if not "%1" == "no-clean" (
	nmake /f %build_make% clean
	if errorlevel 1 (
		echo nmake clean error
		exit /b 1
	)
)


REM *** Build
nmake /f %build_make%
if errorlevel 1 (
	echo nmake error
	exit /b 1
)


REM *** Finish
exit /b 0

