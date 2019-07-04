@echo off
cd /d %~dp0

call force_windows_release64.bat no-clean
if errorlevel 1 (
	echo build error
	exit /b 1
)
exit /b 0

