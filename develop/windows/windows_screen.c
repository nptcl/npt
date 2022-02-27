#include "windows_screen.h"
#include "terme_arch.h"
#include "typedef.h"
#include <Windows.h>

unsigned Window_SizeX;
unsigned Window_SizeY;
unsigned Window_CursorX;
unsigned Window_CursorY;
static CRITICAL_SECTION Windows_Lock;

void windows_screen_init(void)
{
	Window_SizeX = 80;
	Window_SizeY = 24;
	Window_CursorX = 0;
	Window_CursorY = 0;
	terme_arch_size_update();
	cleartype(Windows_Lock);
}

void windows_screen_lock_init(void)
{
	InitializeCriticalSection(&Windows_Lock);
}

void windows_screen_lock_free(void)
{
	DeleteCriticalSection(&Windows_Lock);
}

void windows_screen_enter(void)
{
	EnterCriticalSection(&Windows_Lock);
}

void windows_screen_leave(void)
{
	LeaveCriticalSection(&Windows_Lock);
}
