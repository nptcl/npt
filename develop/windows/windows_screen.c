#include "windows_screen.h"
#include "terme_arch.h"
#include "typedef.h"
#include <Windows.h>

int Window_Mode;
int Window_Exit;
int Window_Update;
unsigned Window_SizeX;
unsigned Window_SizeY;
unsigned Window_CursorX;
unsigned Window_CursorY;
static CRITICAL_SECTION Windows_Lock;
COLORREF Window_Color1_Default;
COLORREF Window_Color2_Default;
COLORREF Window_Color1;
COLORREF Window_Color2;

void windows_screen_init(void)
{
	Window_Mode = 0;
	Window_Exit = 0;
	Window_Update = 0;
	Window_SizeX = 80;
	Window_SizeY = 24;
	Window_CursorX = 0;
	Window_CursorY = 0;
	terme_arch_size_update();
	cleartype(Windows_Lock);
	Window_Color1_Default = RGB(0xFF, 0xFF, 0xFF);
	Window_Color2_Default = RGB(0x00, 0x00, 0x00);
	Window_Color1 = Window_Color1_Default;
	Window_Color2 = Window_Color2_Default;
}

void windows_screen_begin(void)
{
	Window_Mode = 0;
}

void windows_screen_end(void)
{
	Window_Mode = 0;
}

void windows_screen_textmode(void)
{
	windows_screen_enter();
	Window_Mode = 0;
	windows_screen_leave();
}

void windows_screen_rawmode(void)
{
	windows_screen_enter();
	Window_Mode = 1;
	windows_screen_leave();
}

int windows_screen_getmode(void)
{
	int value;

	windows_screen_enter();
	value = Window_Mode;
	windows_screen_leave();

	return value;
}

void windows_screen_setmode(int mode)
{
	windows_screen_enter();
	Window_Mode = mode;
	windows_screen_leave();
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
