@echo off
cd /d %~dp0

del /q _debug.txt
del /q npt.* *.pdb *.tmp
del /q *.obj

echo OK
exit /b 0

