#include "build.h"
#include "windows_arch.h"
#include "windows_screen.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Windows.h>

void exit_windows(int code)
{
	exit(1);
}

static int WriteFile_force(HANDLE file, const void *ptr, DWORD size, int *eof)
{
	BOOL check;
	const char *str;
	DWORD count, value, diff;

	str = (const char *)ptr;
	for (count = 0; count < size; count += value) {
		diff = size - count;
		check = WriteFile(file, (LPCVOID)str, diff, &value, NULL);
		if (check == 0) {
			ResultIf(eof, 0);
			return 1;  /* write error */
		}
		if (value == 0) {
			ResultIf(eof, 1);
			return 1;  /* write error */
		}
		str += value;
	}

	ResultIf(eof, 0);
	return 0;
}

void stdout_windows(const char *msg)
{
	HANDLE file;
	DWORD size;

	file = GetStdHandle(STD_OUTPUT_HANDLE);
	size = (DWORD)strlen(msg);
	(void)WriteFile_force(file, msg, size, NULL);
}

void stderr_windows(const char *msg)
{
	HANDLE file;
	DWORD size;

	file = GetStdHandle(STD_ERROR_HANDLE);
	size = (DWORD)strlen(msg);
	(void)WriteFile_force(file, msg, size, NULL);
}

int getwidth_windows(unsigned *rx, unsigned *ry)
{
	*rx = Window_SizeX;
	*ry = Window_SizeY;
	return 0;
}

