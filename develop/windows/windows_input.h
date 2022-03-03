#ifndef __WINDOWS_INPUT_HEADER__
#define __WINDOWS_INPUT_HEADER__

#include <Windows.h>
#include "typedef.h"

#define windows_input_init _n(windows_input_init)
#define windows_input_paste _n(windows_input_paste)
#define windows_input_char _n(windows_input_char)
#define windows_input_keydown _n(windows_input_keydown)
#define windows_input_select _n(windows_input_select)
#define windows_input_wait_integer _n(windows_input_wait_integer)
#define windows_input_wait_float _n(windows_input_wait_float)
#define windows_input_read _n(windows_input_read)
#define windows_input_discard _n(windows_input_discard)

void windows_input_init(void);
LRESULT windows_input_paste(HWND hWnd);
LRESULT windows_input_char(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp);
LRESULT windows_input_keydown(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp);
int windows_input_select(int *ret);
int windows_input_wait_integer(int *ret, int value);
int windows_input_wait_float(int *ret, double value);
int windows_input_read(void *data, size_t size, size_t *ret);
int windows_input_discard(void);

#endif

