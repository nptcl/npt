@echo off
cd /d %~dp0
cd build\

if errorlevel 1 (
	echo cd error: build\
	goto error
)

if exist "Debug\" (
	rmdir /s /q Debug\
		if errorlevel 1 (
		echo rmdir error: Debug
		goto error
	)
)

if exist "Release\" (
	rmdir /s /q Release\
	if errorlevel 1 (
		echo rmdir error: Release
		goto error
	)
)

if exist "x64\" (
	rmdir /s /q x64\
	if errorlevel 1 (
		echo rmdir error: x64
		goto error
	)
)

if exist "..\_debug.txt" (
	del /q ..\_debug.txt
	if errorlevel 1 (
		echo rmdir error: _debug.txt
		goto error
	)
)

echo OK
pause
exit /b 0

:error
echo ERROR
exit /b 1

