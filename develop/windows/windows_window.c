#include "encode_unicode.h"
#include "typedef.h"
#include "windows_display.h"
#include "windows_error.h"
#include "windows_input.h"
#include "windows_main.h"
#include "windows_screen.h"
#include "windows_window.h"
#include "windows_write.h"
#include <Windows.h>
#include <string.h>

/* Window */
static HINSTANCE Window_hInstance;
static HWND Window_hWnd;
static int Window_Sizing;
static int Window_Sized;
static int Window_Show;
static unsigned Window_SpaceX1;
static unsigned Window_SpaceY1;
static unsigned Window_SpaceX2;
static unsigned Window_SpaceY2;
static unsigned Window_SpaceCharacterX;
static unsigned Window_SpaceCharacterY;
static int Window_Cursor;

/* Font */
#define WINDOWS_WINDOW_FONT_SIZE	64
static WCHAR Window_FontName[WINDOWS_WINDOW_FONT_SIZE];
static unsigned Window_FontSize;
static unsigned Window_FontX;
static unsigned Window_FontY;
static HFONT Window_hFont;
static HPEN Window_hPen;
static HBRUSH Window_hBrush;

static int window_window_set_color2(void)
{
	HPEN hPen;
	HBRUSH hBrush;

	hPen = CreatePen(PS_SOLID, 1, Window_Color2);
	if (hPen == NULL)
		return 1;
	hBrush = CreateSolidBrush(Window_Color2);
	if (hBrush == NULL) {
		DeleteObject(hPen);
		return 1;
	}
	Window_hPen = hPen;
	Window_hBrush = hBrush;

	return 0;
}

int windows_window_init(void)
{
	Window_hInstance = NULL;
	Window_hWnd = NULL;
	Window_Sizing = 0;
	Window_Sized = 0;
	Window_Show = 0;
	Window_SpaceX1 = 2;
	Window_SpaceY1 = 2;
	Window_SpaceX2 = 2;
	Window_SpaceY2 = 2;
	Window_SpaceCharacterX = 0;
	Window_SpaceCharacterY = 2;
	Window_Cursor = 0;

	/* Font */
	Window_FontName[0] = 0;
	Window_FontSize = 20;
	Window_FontX = 0;
	Window_FontY = 0;
	Window_hFont = NULL;
	Window_hPen = NULL;
	Window_hBrush = NULL;
	return window_window_set_color2();
}

static void windows_window_title(HWND hWnd)
{
	SetWindowTextA(hWnd, Lispname);
}

static void windows_window_paint_call(HWND hWnd, HDC hDC)
{
	RECT rect;
	HPEN hPen;
	HBRUSH hBrush;

	GetClientRect(hWnd, &rect);
	hPen = SelectObject(hDC, Window_hPen);
	hBrush = SelectObject(hDC, Window_hBrush);
	Rectangle(hDC, rect.left, rect.top, rect.right, rect.bottom);
	SelectObject(hDC, hPen);
	SelectObject(hDC, hBrush);
}

static LRESULT windows_window_paint(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp)
{
	HDC hDC;
	PAINTSTRUCT ps;

	windows_screen_enter();
	hDC = BeginPaint(hWnd, &ps);
	windows_draw_cursor_off_nolock(hDC);
	windows_window_paint_call(hWnd, hDC);
	windows_display_paint_nolock(hDC);
	windows_draw_cursor_on_nolock(hDC);
	EndPaint(hWnd, &ps);
	windows_screen_leave();

	return DefWindowProcW(hWnd, msg, wp, lp);
}

static void windows_window_setsize(HWND hWnd, unsigned x, unsigned y)
{
	int w, h;
	unsigned x1, y1;
	RECT a, b;

	GetWindowRect(hWnd, &a);
	GetClientRect(hWnd, &b);
	x1 = (a.right - a.left) - (b.right - b.left);
	y1 = (a.bottom - a.top) - (b.bottom - b.top);
	x1 += Window_SpaceX1 + Window_SpaceX2;
	y1 += Window_SpaceY1 + Window_SpaceY2;
	x1 += (Window_FontX + Window_SpaceCharacterX) * x;
	y1 += (Window_FontY + Window_SpaceCharacterY) * y;
	w = (int)x1;
	h = (int)y1;
	SetWindowPos(hWnd, NULL, 0, 0, w, h, SWP_NOMOVE | SWP_NOZORDER);
}

static void windows_window_getsize(HWND hWnd, unsigned *rx, unsigned *ry)
{
	RECT a, b;
	unsigned x, y, x0, y0, x1, y1, x2, y2, x3, y3;

	GetWindowRect(hWnd, &a);
	GetClientRect(hWnd, &b);
	x0 = (a.right - a.left) - (b.right - b.left);
	y0 = (a.bottom - a.top) - (b.bottom - b.top);
	x1 = a.right - a.left;
	y1 = a.bottom - a.top;
	x2 = x0 + Window_SpaceX1 + Window_SpaceX2;
	y2 = y0 + Window_SpaceY1 + Window_SpaceY2;
	x3 = Window_FontX + Window_SpaceCharacterX;
	y3 = Window_FontY + Window_SpaceCharacterY;
	if (x1 < x2)
		x = 1;
	else
		x = (x1 - x2) / x3;
	if (y1 < y2)
		y = 1;
	else
		y = (y1 - y2) / y3;
	if (x < 1)
		x = 1;
	if (y < 1)
		y = 1;
	*rx = x;
	*ry = y;
}

static LRESULT windows_window_size(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp)
{
	char data[64];
	unsigned x, y;

	windows_window_getsize(hWnd, &x, &y);
	windows_window_setsize(hWnd, x, y);
	windows_screen_enter();
	Window_SizeX = x;
	Window_SizeY = y;
	windows_screen_leave();

	if (Window_Sizing) {
		windows_window_getsize(hWnd, &x, &y);
		snprintf(data, 64, "%s [%d x %d]", Lispname, x, y);
		SetWindowTextA(hWnd, data);
	}
	Window_Sized = 1;

	return DefWindowProcW(hWnd, msg, wp, lp);
}

static LRESULT windows_winodw_entersizemove(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp)
{
	Window_Sizing = 1;
	Window_Sized = 0;
	return DefWindowProcW(hWnd, msg, wp, lp);
}

static LRESULT windows_winodw_exitsizemove(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp)
{
	HDC hDC;
	unsigned x, y;

	Window_Sizing = 0;
	if (Window_Sized == 0)
		return DefWindowProcW(hWnd, msg, wp, lp);

	windows_window_getsize(hWnd, &x, &y);
	windows_window_setsize(hWnd, x, y);
	windows_screen_enter();
	Window_SizeX = x;
	Window_SizeY = y;
	(void)windows_write_clear_nolock();

	/* Redraw */
	hDC = GetDC(hWnd);
	if (hDC) {
		windows_window_paint_call(hWnd, hDC);
		ReleaseDC(hWnd, hDC);
	}
	windows_screen_leave();

	/* Text */
	windows_window_title(hWnd);
	return DefWindowProcW(hWnd, msg, wp, lp);

}

static LRESULT CALLBACK WndProc(HWND hWnd, UINT msg, WPARAM wp, LPARAM lp)
{
	switch (msg) {
		case WM_CHAR:
			return windows_input_char(hWnd, msg, wp, lp);

		case WM_KEYDOWN:
			return windows_input_keydown(hWnd, msg, wp, lp);

		case WM_PAINT:
			return windows_window_paint(hWnd, msg, wp, lp);

		case WM_SIZE:
			return windows_window_size(hWnd, msg, wp, lp);

		case WM_ENTERSIZEMOVE:
			return windows_winodw_entersizemove(hWnd, msg, wp, lp);

		case WM_EXITSIZEMOVE:
			return windows_winodw_exitsizemove(hWnd, msg, wp, lp);

		case WM_RBUTTONDOWN:
			return windows_input_paste(hWnd);

		case WM_DESTROY:
			PostQuitMessage(0);
			break;

		default:
			return DefWindowProcW(hWnd, msg, wp, lp);
	}

	return 0;
}

static int windows_window_class(void)
{
	WNDCLASSW wc;

	wc.style = 0;
	wc.lpfnWndProc = (WNDPROC)WndProc;
	wc.cbClsExtra = 0;
	wc.cbWndExtra = 0;
	wc.hInstance = Window_hInstance;
	wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
	wc.hCursor = LoadCursor(NULL, IDC_IBEAM);
	wc.hbrBackground = GetStockObject(BLACK_BRUSH);
	wc.lpszMenuName = LispnameW;
	wc.lpszClassName = LispnameW;

	return RegisterClassW(&wc) == 0;
}

static int windows_window_rosource_size(HDC hDC, HFONT hFont)
{
	SIZE a, b;
	LONG x, y;

	x = y = 0;
	if (GetTextExtentPoint32A(hDC, "M", 1, &a) == 0)
		return 1;
	if (GetTextExtentPoint32A(hDC, "W", 1, &b) == 0)
		return 1;
	x = (a.cx < b.cx) ? b.cx : a.cx;
	y = (a.cy < b.cy) ? b.cy : a.cy;
	Window_hFont = hFont;
	Window_FontX = (unsigned)x;
	Window_FontY = (unsigned)y;

	return 0;
}

static int windows_window_resource_select(HDC hDC, HFONT hFont)
{
	int check;
	HFONT rollback;

	rollback = SelectObject(hDC, hFont);
	if (rollback == NULL)
		return 1;
	check = windows_window_rosource_size(hDC, hFont);
	SelectObject(hDC, rollback);

	return check;
}

static int windows_window_rosource_font(HDC hDC)
{
	int check;
	HFONT hFont;

	hFont = CreateFontW(Window_FontSize, 0, 0, 0, FW_NORMAL,
		FALSE, FALSE, FALSE,
		DEFAULT_CHARSET,
		OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		DEFAULT_QUALITY,
		FIXED_PITCH | FF_MODERN,
		Window_FontName[0] ? Window_FontName : NULL);
	if (hFont == NULL)
		return 1;

	check = windows_window_resource_select(hDC, hFont);
	if (check) {
		DeleteObject(hFont);
		return 1;
	}

	return 0;
}

static int windows_window_resource(HWND hWnd)
{
	int check;
	HDC hDC;

	hDC = GetDC(hWnd);
	if (hDC == NULL)
		return 1;
	check = windows_window_rosource_font(hDC);
	ReleaseDC(hWnd, hDC);

	return check;
}

static void windows_window_resource_free(void)
{
	if (Window_hFont) {
		DeleteObject(Window_hFont);
		Window_hFont = NULL;
	}
	if (Window_hPen) {
		DeleteObject(Window_hPen);
		Window_hPen = NULL;
	}
	if (Window_hBrush) {
		DeleteObject(Window_hBrush);
		Window_hBrush = NULL;
	}
}

static int windows_window_window(void)
{
	HWND hWnd;
	unsigned x, y;

	hWnd = CreateWindowW(LispnameW,
			LispnameW,
			WS_OVERLAPPEDWINDOW,
			CW_USEDEFAULT, CW_USEDEFAULT,
			CW_USEDEFAULT, CW_USEDEFAULT,
			NULL, NULL, Window_hInstance, NULL);
	if (hWnd == NULL)
		return 1;
	if (windows_window_resource(hWnd)) {
		DestroyWindow(hWnd);
		return 1;
	}
	x = Window_SizeX;
	y = Window_SizeY;
	windows_window_setsize(hWnd, x, y);
	windows_window_title(hWnd);
	Window_hWnd = hWnd;

	return 0;
}

static int windows_main_loop(void)
{
	MSG msg;
	BOOL check;

	memset(&msg, 0, sizeof(msg));
	for (;;) {
		check = GetMessageW(&msg, NULL, 0, 0);
		if (check < 0)
			continue;
		if (check == 0)
			break;
		TranslateMessage(&msg);
		DispatchMessageW(&msg);
	}
	Window_hWnd = NULL;

	/* Exit */
	Window_Exit = 1;
	windows_input_wake();
	return (int)msg.wParam;
}

static int windows_window_call(void)
{
	/* hInstance */
	Window_hInstance = (HINSTANCE)GetModuleHandleW(NULL);

	/* RegisterClass */
	if (windows_window_class())
		return windows_error("RegisterClass error.");
	if (windows_window_window())
		return windows_error("CreateWindow error.");

	/* start */
	if (windows_main_start())
		return windows_error("windows_main_start error.");

	return windows_main_loop();
}

int windows_window(void)
{
	int check;

	/* screen */
	if (windows_display_update())
		return windows_error("windows_display_update error.");

	/* window */
	windows_screen_lock_init();
	check = windows_window_call();
	windows_screen_lock_free();

	/* free */
	windows_window_resource_free();

	return check;
}



/*
 *  ShowWindow
 */
static int windows_window_show_value(int mode)
{
	if (Window_hWnd == NULL)
		return 1;
	ShowWindow(Window_hWnd, mode);
	if (UpdateWindow(Window_hWnd) == 0)
		return 1;

	return 0;
}

int windows_window_show_default(void)
{
	return windows_window_show_value(SW_SHOWDEFAULT);
}

int windows_window_show_show(void)
{
	return windows_window_show_value(SW_SHOW);
}

int windows_window_show_hide(void)
{
	return windows_window_show_value(SW_HIDE);
}

int windows_window_size_update(unsigned x, unsigned y)
{
	HDC hDC;
	HWND hWnd;

	hWnd = Window_hWnd;
	windows_window_setsize(hWnd, x, y);
	windows_screen_enter();
	Window_SizeX = x;
	Window_SizeY = y;
	(void)windows_write_clear_nolock();

	/* Redraw */
	hDC = GetDC(hWnd);
	if (hDC) {
		windows_window_paint_call(hWnd, hDC);
		ReleaseDC(hWnd, hDC);
	}
	windows_screen_leave();
	windows_window_title(hWnd);

	return 0;
}

void windows_window_error(const char *str)
{
	(void)MessageBoxA(Window_hWnd, str, LispName, MB_ICONERROR | MB_OK);
}


/*
 *  Draw
 */
static unsigned windows_draw_x(unsigned x)
{
	return Window_SpaceX1 + (Window_FontX + Window_SpaceCharacterX) * x;
}

static unsigned windows_draw_y(unsigned y)
{
	return Window_SpaceY1 + (Window_FontY + Window_SpaceCharacterY) * y;
}

static void windows_draw_cursor_reverse(HDC hDC)
{
	unsigned x, y;

	if (hDC == NULL)
		return;
	x = windows_draw_x(Window_CursorX);
	y = windows_draw_y(Window_CursorY);
	BitBlt(hDC, x, y, Window_FontX, Window_FontY, hDC, x, y, NOTSRCCOPY);
}

void windows_draw_cursor_on_nolock(HDC hDC)
{
	int releasep;

	if (Window_Cursor != 0)
		return;
	releasep = (hDC == NULL);
	if (releasep)
		hDC = GetDC(Window_hWnd);
	if (hDC)
		windows_draw_cursor_reverse(hDC);
	Window_Cursor = 1;
	if (releasep)
		ReleaseDC(Window_hWnd, hDC);
}

void windows_draw_cursor_off_nolock(HDC hDC)
{
	int releasep;

	if (Window_Cursor == 0)
		return;
	releasep = (hDC == NULL);
	if (releasep)
		hDC = GetDC(Window_hWnd);
	if (hDC)
		windows_draw_cursor_reverse(hDC);
	Window_Cursor = 0;
	if (releasep)
		ReleaseDC(Window_hWnd, hDC);
}

void windows_draw_cursor_on_lock(HDC hDC)
{
	windows_screen_enter();
	windows_draw_cursor_on_nolock(hDC);
	windows_screen_leave();
}

void windows_draw_cursor_off_lock(HDC hDC)
{
	windows_screen_enter();
	windows_draw_cursor_off_nolock(hDC);
	windows_screen_leave();
}

static void windows_draw_textout(HDC hDC,
	unsigned x, unsigned y, WCHAR *data, size_t size)
{
	int xp, yp;
	HFONT hFont;

	xp = windows_draw_x(x);
	yp = windows_draw_y(y);

	SetTextColor(hDC, Window_Color1);
	SetBkColor(hDC, Window_Color2);
	SetBkMode(hDC, OPAQUE);
	hFont = SelectObject(hDC, Window_hFont);
	TextOutW(hDC, xp, yp, data, (int)size);
	SelectObject(hDC, hFont);
}

int windows_draw_character_nolock(HDC hDC, unsigned x, unsigned y, unicode c)
{
	int releasep;
	WCHAR data[8];
	size_t size;

	if (c < 0x20)
		return 0;
	if (Window_SizeX <= x)
		return 0;
	if (Window_SizeY <= y)
		return 0;
	if (encode_utf16b(c, (byte16 *)data, &size))
		return 1;
	releasep = (hDC == NULL);
	if (releasep)
		hDC = GetDC(Window_hWnd);
	if (hDC)
		windows_draw_textout(hDC, x, y, data, size);
	if (releasep)
		ReleaseDC(Window_hWnd, hDC);

	return 0;
}

int windows_draw_line_feed_nolock(void)
{
	unsigned x1, y1, x2, y2, y3, w, h;
	HWND hWnd;
	HDC hDC;

	if (Window_SizeX == 0)
		return 0;
	if (Window_SizeY == 0)
		return 0;
	hWnd = Window_hWnd;
	hDC = GetDC(hWnd);
	if (hDC == NULL)
		return 1;

	/* Scroll */
	x1 = windows_draw_x(0);
	x2 = windows_draw_x(Window_SizeX);
	y1 = windows_draw_y(0);
	y2 = windows_draw_y(1);
	y3 = windows_draw_y(Window_SizeY - 1);
	if (Window_SizeY != 1) {
		w = x2 - x1;
		h = y3 - y1;
		BitBlt(hDC, x1, y1, w, h, hDC, x1, y2, SRCCOPY);
	}

	/* Erase */
	x2 = Window_SizeX;
	y1 = Window_SizeY - 1;
	y2 = Window_SizeY;
	windows_draw_delete_nolock(hDC, 0, y1, x2, y2);

	/* Release */
	ReleaseDC(hWnd, hDC);
	return 0;
}

int windows_draw_line_back_nolock(void)
{
	unsigned x1, y1, x2, y2, y3, w, h;
	HWND hWnd;
	HDC hDC;

	if (Window_SizeX == 0)
		return 0;
	if (Window_SizeY == 0)
		return 0;
	hWnd = Window_hWnd;
	hDC = GetDC(hWnd);
	if (hDC == NULL)
		return 1;

	/* Scroll */
	x1 = windows_draw_x(0);
	x2 = windows_draw_x(Window_SizeX);
	y1 = windows_draw_y(0);
	y2 = windows_draw_y(1);
	y3 = windows_draw_y(Window_SizeY);
	if (Window_SizeY != 1) {
		w = x2 - x1;
		h = y3 - y2;
		BitBlt(hDC, x1, y2, w, h, hDC, x1, y1, SRCCOPY);
	}

	/* Erase */
	x2 = Window_SizeX;
	windows_draw_delete_nolock(hDC, 0, 0, x2, 1);

	/* Release */
	ReleaseDC(hWnd, hDC);
	return 0;
}

int windows_draw_delete_nolock(HDC hDC, unsigned x1, unsigned y1, unsigned x2, unsigned y2)
{
	int releasep;
	unsigned a, b, c, d;
	if (! (x1 < Window_SizeX))
		return 0;
	if (! (x2 <= Window_SizeX))
		x2 = Window_SizeX;
	if (! (y1 < Window_SizeY))
		return 0;
	if (! (y2 <= Window_SizeY))
		y2 = Window_SizeY;
	if (x2 <= x1 || y2 <= y1)
		return 0;
	a = windows_draw_x(x1);
	b = windows_draw_y(y1);
	c = windows_draw_x(x2);
	d = windows_draw_y(y2);

	/* hDC */
	releasep = (hDC == NULL);
	if (releasep)
		hDC = GetDC(Window_hWnd);
	if (hDC == NULL)
		return 1;

	/* Erase */
	SelectObject(hDC, Window_hPen);
	SelectObject(hDC, Window_hBrush);
	Rectangle(hDC, a, b, c, d);

	/* Release */
	if (releasep)
		ReleaseDC(Window_hWnd, hDC);

	return 0;
}
