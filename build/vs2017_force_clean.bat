@echo off
cd /d %~dp0
cd ..

del /q _debug.txt
del /q npt.* *.pdb *.tmp
del /q *.obj

echo OK
pause
exit /b 0

