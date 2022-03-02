#include "encode.h"
#include "encode_unicode.h"
#include "typedef.h"
#include "windows_error.h"
#include "windows_input.h"
#include "windows_window.h"
#include "windows_write.h"
#include <Windows.h>
#include <math.h>

#ifdef LISP_DEBUG
#define WINDOWS_INPUT_SIZE		3
#else
#define WINDOWS_INPUT_SIZE		4096
#endif

static CONDITION_VARIABLE Windows_Input_Condition;
static CRITICAL_SECTION Windows_Input_Lock;
static unsigned Windows_Input_Begin;
static unsigned Windows_Input_End;
static unsigned Windows_Input_Size;
static int Windows_Input_Break;
static byte Windows_Input_Data[WINDOWS_INPUT_SIZE];

void windows_input_init(void)
{
	InitializeConditionVariable(&Windows_Input_Condition);
	InitializeCriticalSection(&Windows_Input_Lock);
	Windows_Input_Begin = 0;
	Windows_Input_End = 0;
	Windows_Input_Size = 0;
	Windows_Input_Break = 0;
}


/*
 *  Ring Buffer
 */
static unsigned windows_ring_space(void)
{
	return WINDOWS_INPUT_SIZE - Windows_Input_Size;
}

static void windows_ring_push(const byte *str, size_t size, size_t *ret)
{
	byte *str1, *str2;
	unsigned tail, size0, size1, size2;

	/* size */
	tail = windows_ring_space();
	if (size == 0 || tail == 0) {
		*ret = 0;
		return;
	}
	if (size < tail)
		size0 = (unsigned)size;
	else
		size0 = tail;

	/* copy */
	tail = Windows_Input_End + size0;
	if (tail <= WINDOWS_INPUT_SIZE) {
		str1 = Windows_Input_Data + Windows_Input_End;
		memcpy(str1, str, size0);
	}
	else {
		str1 = Windows_Input_Data + Windows_Input_End;
		str2 = Windows_Input_Data;
		size1 = WINDOWS_INPUT_SIZE - Windows_Input_End;
		size2 = size0 - size1;
		memcpy(str1, str, size1);
		memcpy(str2, str + size1, size2);
	}
	Windows_Input_Size += size0;
	Windows_Input_End += size0;
	Windows_Input_End %= WINDOWS_INPUT_SIZE;
	*ret = (size_t)size0;
}

static void windows_ring_pop(byte *str, size_t size, size_t *ret)
{
	const byte *str1, *str2;
	unsigned tail, size0, size1, size2;

	/* size */
	if (size == 0 || Windows_Input_Size == 0) {
		*ret = 0;
		return;
	}
	if (size < Windows_Input_Size)
		size0 = (unsigned)size;
	else
		size0 = Windows_Input_Size;

	/* copy */
	tail = Windows_Input_Begin + size0;
	if (tail <= WINDOWS_INPUT_SIZE) {
		str1 = Windows_Input_Data + Windows_Input_Begin;
		memcpy(str, str1, size0);
	}
	else {
		str1 = Windows_Input_Data + Windows_Input_Begin;
		str2 = Windows_Input_Data;
		size1 = WINDOWS_INPUT_SIZE - Windows_Input_Begin;
		size2 = size0 - size1;
		memcpy(str, str1, size1);
		memcpy(str + size1, str2, size2);
	}
	Windows_Input_Size -= size0;
	if (Windows_Input_Size) {
		Windows_Input_Begin += size0;
		Windows_Input_Begin %= WINDOWS_INPUT_SIZE;
	}
	else {
		Windows_Input_Begin = 0;
		Windows_Input_End = 0;
	}
	*ret = (size_t)size0;
}

static int windows_ring_write_loop(const byte *str, size_t size, size_t *ret)
{
	BOOL check;

	while (windows_ring_space() == 0) {
		check = SleepConditionVariableCS(
			&Windows_Input_Condition,
			&Windows_Input_Lock,
			INFINITE);
		if (check == 0) {
			*ret = 0;
			return 1;
		}
	}
	windows_ring_push(str, size, ret);
	return 0;
}

static int windows_ring_write(const byte *str, size_t size)
{
	int check;
	size_t value;

	while (size) {
		EnterCriticalSection(&Windows_Input_Lock);
		check = windows_ring_write_loop(str, size, &value);
		LeaveCriticalSection(&Windows_Input_Lock);
		if (check)
			return check;
		WakeConditionVariable(&Windows_Input_Condition);
		str += value;
		size -= value;
	}

	return 0;
}

static int windows_ring_select_call(int *ret, DWORD wait)
{
	BOOL check;

	while (Windows_Input_Size == 0 && Windows_Input_Break == 0) {
		check = SleepConditionVariableCS(
			&Windows_Input_Condition,
			&Windows_Input_Lock,
			wait);
		if (check == 0) {
			if (GetLastError() == ERROR_TIMEOUT) {
				*ret = 0;
				return 0;
			}
			else {
				*ret = 0;
				return 1;
			}
		}
	}
	if (Windows_Input_Break) {
		Windows_Input_Break = 0;
		*ret = 0;
		return -1;
	}
	*ret = 1;
	return 0;
}

static int windows_ring_select(int *ret, DWORD wait)
{
	int check;

	EnterCriticalSection(&Windows_Input_Lock);
	check = windows_ring_select_call(ret, wait);
	LeaveCriticalSection(&Windows_Input_Lock);

	return check;
}

static int windows_ring_read_call(byte *str, size_t size, size_t *ret)
{
	BOOL check;

	/* zero */
	if (size == 0) {
		*ret = 0;
		return 0;
	}

	/* wait */
	while (Windows_Input_Size == 0 && Windows_Input_Break == 0) {
		check = SleepConditionVariableCS(
			&Windows_Input_Condition,
			&Windows_Input_Lock,
			INFINITE);
		if (check == 0) {
			*ret = 0;
			return 1;
		}
	}
	if (Windows_Input_Break) {
		Windows_Input_Break = 0;
		*ret = 0;
		return -1;
	}

	/* read */
	windows_ring_pop(str, size, ret);
	return 0;
}

static int windows_ring_read(void *str, size_t size, size_t *ret)
{
	int check;

	if (size == 0) {
		*ret = 0;
		return 0;
	}

	EnterCriticalSection(&Windows_Input_Lock);
	check = windows_ring_read_call(str, size, ret);
	LeaveCriticalSection(&Windows_Input_Lock);
	if (check == 0)
		WakeConditionVariable(&Windows_Input_Condition);

	return check;
}


/*
 *  Input Clipboard
 */
static int windows_input_escape(const void *str, size_t size)
{
	return windows_ring_write(str, size);
}

static int windows_input_unicode(unicode c)
{
	byte data[8];
	size_t size;

	if (encode_utf8(c, data, &size))
		return 1 ; /* error */
	if (windows_input_escape((const void *)data, size))
		return 1;

	return 0;
}

static int windows_input_ctrl_c(void)
{
	SHORT s1, s2;

	s1 = GetAsyncKeyState(VK_CONTROL);
	s2 = GetAsyncKeyState(0x43); /* C key */
	return s1 < 0 && s2 < 0;
}

static int windows_input_utf16(uint16_t *ptr)
{
	byte16 a, b;
	unicode c;

	while (! windows_input_ctrl_c()) {
		a = *(ptr++);
		if (a == 0)
			break;
		if (UTF16low(a))
			continue; /* error */
		if (UTF16high(a)) {
			b = *(ptr++);
			if (! UTF16low(b))
				continue; /* error */
			c = UTF16unicode(a, b);
		}
		else {
			c = (unicode)a;
		}
		if (windows_input_unicode(c))
			return 1;
	}

	return 0;
}

static LRESULT windows_input_paste(HWND hWnd)
{
	BOOL check;
	HANDLE handle;
	uint16_t *ptr;

	check = OpenClipboard(hWnd);
	if (check == 0)
		return 0;
	handle = GetClipboardData(CF_UNICODETEXT);
	if (handle) {
		ptr = (uint16_t *)GlobalLock(handle);
		if (ptr) {
			if (windows_input_utf16(ptr))
				windows_error("windows_input_utf16 error.");
			GlobalUnlock(handle);
		}
	}
	(void)CloseClipboard();

	return 0;
}


/*
 *  Input from Window
 */
static LRESULT windows_input_string(const char *str)
{
	size_t size;

	size = strlen(str);
	if (windows_input_escape((const void *)str, size))
		windows_error("windows_input_escape error.");

	return 0;
}

static LRESULT windows_input_byte(byte c)
{
	if (windows_input_escape((const void *)&c, 1))
		windows_error("windows_input_escape error.");

	return 0;
}

LRESULT windows_input_char(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp)
{
	byte c;

	c = (byte)wp;
	if (c <= 0x7F)
		return windows_input_byte(c);
	else
		return DefWindowProcW(hWnd, msg, wp, lp);
}

static LRESULT windows_input_insert(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp)
{
	int state1, state2;

	state1 = (GetKeyState(VK_SHIFT) < 0);
	state2 = (GetKeyState(VK_CONTROL) < 0);

	if (state1 && (! state2))
		return windows_input_paste(hWnd);
	if ((! state1) && (! state2))
		return windows_input_string("\x1B\x5B\x32\x7E");

	return DefWindowProcW(hWnd, msg, wp, lp);
}

static LRESULT windows_input_delete(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp)
{
	int state1, state2;

	state1 = (GetKeyState(VK_SHIFT) < 0);
	state2 = (GetKeyState(VK_CONTROL) < 0);

	if ((! state1) && (! state2))
		return windows_input_byte(0x7F);

	return DefWindowProcW(hWnd, msg, wp, lp);
}

LRESULT windows_input_keydown(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp)
{
	switch (wp) {
	case VK_LEFT:
		return windows_input_string("\x1B\x5B\x44");
	case VK_RIGHT:
		return windows_input_string("\x1B\x5B\x43");
	case VK_UP:
		return windows_input_string("\x1B\x5B\x41");
	case VK_DOWN:
		return windows_input_string("\x1B\x5B\x42");
	case VK_PRIOR:
		return windows_input_string("\x1B\x5B\x35\x7E");
	case VK_NEXT:
		return windows_input_string("\x1B\x5B\x36\x7E");
	case VK_HOME:
		return windows_input_string("\x1B\x5B\x31\x7E");
	case VK_END:
		return windows_input_string("\x1B\x5B\x34\x7E");
	case VK_INSERT:
		return windows_input_insert(hWnd, msg, wp, lp);
	case VK_DELETE:
		return windows_input_delete(hWnd, msg, wp, lp);

	case VK_F1:
		return windows_input_string("\x1B\x4F\x50");
	case VK_F2:
		return windows_input_string("\x1B\x4F\x51");
	case VK_F3:
		return windows_input_string("\x1B\x4F\x52");
	case VK_F4:
		return windows_input_string("\x1B\x4F\x53");

	default:
		return DefWindowProcW(hWnd, msg, wp, lp);
	}
}


/*
 *  terme interface
 */
static int windows_input_select_value(int *ret, DWORD wait)
{
	return windows_ring_select(ret, wait);
}

int windows_input_select(int *ret)
{
	return windows_input_select_value(ret, 0);
}

int windows_input_wait_integer(int *ret, int value)
{
	DWORD x, y;

#if 0xFFFFFFFFUL < INT_MAX
	if (0xFFFFFFFFULL < value) {
		*ret = 0;
		return 1;
	}
#endif
	x = (DWORD)value;
	y = value * 1000UL;
	if (y < x) {
		*ret = 0;
		return 1;
	}

	return windows_input_select_value(ret, y);
}

int windows_input_wait_float(int *ret, double value)
{
	DWORD sec;

	if (value <= 0.0)
		return windows_input_select_value(ret, 0);

	/* msec = (expt 10 -3) sec */
	value *= 1000.0;
	value = trunc(value);
	if (((double)0xFFFFFFFFUL) < value) {
		*ret = 0;
		return 1;
	}
	sec = (DWORD)value;
	return windows_input_select_value(ret, sec);
}

int windows_input_read(void *data, size_t size, size_t *ret)
{
	return windows_ring_read(data, size, ret);
}
