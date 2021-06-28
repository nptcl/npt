@echo off
cd /d %~dp0
cd ..

if not exist npt.exe (
  echo directory error
  goto error
)

.\npt.exe --standalone
if errorlevel 1 (
  echo test error
  goto error
)

pause
exit /b 0
:error
pause
exit /b 1

