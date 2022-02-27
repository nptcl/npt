#include "prompt.h"
#include "typedef.h"
#include "windows_display.h"
#include "windows_screen.h"
#include "windows_window.h"
#include <Windows.h>

#ifdef LISP_DEBUG
#define WINDOWS_DISPLAY_X		3
#define WINDOWS_DISPLAY_Y		3
#define WINDOWS_DISPLAY_RING	5
#else
#define WINDOWS_DISPLAY_X		32
#define WINDOWS_DISPLAY_Y		128
#define WINDOWS_DISPLAY_RING	10
#endif
#define WINDOWS_DISPLAY_SIZE(x, y) (((x / y) + 1) * y)

enum windows_color {
	windows_color_default,
	windows_color_index,
	windows_color_mode,
	windows_color_color_256,
	windows_color_color_rgb
};

struct windows_character {
	unsigned empty : 1;
	unsigned data : 1;
	unsigned wide : 1;
	unsigned range : 1;
	unsigned eol : 1;
	enum windows_color am : 4;
	enum windows_color bm : 4;
	byte a1, a2, a3, b1, b2, b3;
	unicode c : 24;
};

struct windows_line {
	unsigned write, size;
	struct windows_character *ptr;
};

struct windows_character Display_Font;
static struct windows_line *Display_Root;
static unsigned Display_AllocX, Display_AllocY;
static unsigned Display_SizeX, Display_SizeY;
static unsigned Display_WriteY;
static unsigned Display_Ring;
static unsigned Display_RingSize;

void windows_display_init(void)
{
	Display_Root = NULL;
	Display_AllocX = 0;
	Display_AllocY = 0;
	Display_SizeX = 0;
	Display_SizeY = 0;
	Display_WriteY = 0;
	Display_Ring = 0;
	Display_RingSize = 0;
	cleartype(Display_Font);
	Display_Font.am = windows_color_default;
	Display_Font.bm = windows_color_default;
}

static int windows_display_malloc_y(unsigned y2, unsigned *ret)
{
	struct windows_line *array;
	size_t size;

	size = y2 * sizeof(struct windows_line);
	array = (struct windows_line *)malloc(size);
	if (array == NULL)
		return 1;
	memset(array, 0, size);
	Display_Root = array;
	*ret = y2;

	return 0;
}

static int windows_display_realloc_y(unsigned y2, unsigned *ret)
{
	unsigned i;
	struct windows_line *array;
	size_t size;

	size = y2 * sizeof(struct windows_line);
	array = (struct windows_line *)realloc(Display_Root, size);
	if (array == NULL)
		return 1;
	if (Display_AllocY < y2) {
		size = (y2 - Display_AllocY) * sizeof(struct windows_line);
		memset(array + Display_AllocY, 0, size);
	}
	for (i = 0; i < Display_AllocY; i++)
		array[i].write = 0;
	Display_Root = array;
	*ret = y2;

	return 0;
}

static int windows_display_clear_y(unsigned *ret)
{
	unsigned i, y2;
	struct windows_line *array;

	array = Display_Root;
	y2 = Display_AllocY;
	for (i = 0; i < y2; i++)
		array[i].write = 0;
	*ret = y2;

	return 0;
}

static int windows_display_update_y(unsigned y, unsigned *ret)
{
	unsigned y1, y2;

	y1 = y * WINDOWS_DISPLAY_RING;
	y2 = WINDOWS_DISPLAY_SIZE(y1, WINDOWS_DISPLAY_Y);

	if (Display_Root == NULL)
		return windows_display_malloc_y(y2, ret);
	if (Display_AllocY < y2)
		return windows_display_realloc_y(y2, ret);
	else
		return windows_display_clear_y(ret);
}

int windows_display_update(void)
{
	int check;
	unsigned x, y, x1, x2, y2;

	x = Window_SizeX;
	y = Window_SizeY;
	x1 = x + 1;
	x2 = WINDOWS_DISPLAY_SIZE(x1, WINDOWS_DISPLAY_X);
	if (x2 < Display_AllocX)
		x2 = Display_AllocX;
	check = windows_display_update_y(y, &y2);
	Display_SizeX = x;
	Display_SizeY = y;
	Display_AllocX = x2;
	Display_AllocY = y2;
	Display_WriteY = 0;
	Display_Ring = 0;
	Display_RingSize = 0;

	return check;
}


/*
 *  Update X
 */
static int windows_display_malloc_x(struct windows_line *ptr)
{
	struct windows_character *data;
	size_t size;

	size = Display_AllocX * sizeof(struct windows_character);
	data = (struct windows_character *)malloc(size);
	if (data == NULL)
		return 1;
	ptr->ptr = data;
	ptr->size = Display_AllocX;
	ptr->write = 0;

	return 0;
}

static int windows_display_realloc_x(struct windows_line *ptr)
{
	struct windows_character *data;
	size_t size;

	size = Display_AllocX * sizeof(struct windows_character);
	data = (struct windows_character *)realloc(ptr->ptr, size);
	if (data == NULL)
		return 1;
	ptr->ptr = data;
	ptr->size = Display_AllocX;
	ptr->write = 0;

	return 0;
}

static int windows_display_clear_x(struct windows_line *ptr)
{
	ptr->write = 0;
	return 0;
}

static int windows_display_update_x(unsigned y)
{
	struct windows_line *ptr;

	ptr = Display_Root + y;
	if (ptr->ptr == NULL)
		return windows_display_malloc_x(ptr);
	if (Display_AllocX < ptr->size)
		return windows_display_realloc_x(ptr);
	else
		return windows_display_clear_x(ptr);
}

static int window_display_ring_y(unsigned y, unsigned *ret)
{
	unsigned y1;

	if (Display_WriteY <= y) {
		*ret = 0;
		return 1;
	}
	y1 = y + Display_Ring;
	if (Display_WriteY <= y1) {
		*ret = y1 - Display_WriteY;
		return 0;
	}
	else {
		*ret = Display_AllocY + y1 - Display_WriteY;
		return 0;
	}
}

int windows_display_line_feed(void)
{
	if (windows_display_update_x(Display_Ring))
		return 1;
	Display_Ring++;
	if (Display_AllocY <= Display_Ring)
		Display_Ring = 0;
	if (Display_RingSize < Display_AllocY)
		Display_RingSize++;
	if (Display_WriteY < Display_SizeY)
		Display_WriteY++;

	return 0;
}

static struct windows_line *windows_display_set_y(unsigned y)
{
	if (Display_SizeY <= y)
		return NULL;
	while (Display_WriteY <= y)
		windows_display_line_feed();
	if (window_display_ring_y(y, &y))
		return NULL;

	return Display_Root + y;
}

static struct windows_character *windows_display_set_x(
	struct windows_line *ptr, unsigned x)
{
	unsigned i, x2;
	struct windows_character *data;

	if (Display_SizeX <= x)
		return NULL;
	if (ptr->write <= x) {
		data = ptr->ptr;
		x2 = x + 1;
		for (i = ptr->write; i < x2; i++)
			data[i].empty = 1;
		ptr->write = x2;
	}

	return ptr->ptr + x;
}

static struct windows_character *windows_display_set(unsigned x, unsigned y)
{
	struct windows_line *ptr;

	if (Display_SizeX <= x || Display_SizeY <= y)
		return NULL;
	ptr = windows_display_set_y(y);
	if (ptr == NULL)
		return NULL;

	return windows_display_set_x(ptr, x);
}

static struct windows_line *windows_display_get_y(unsigned y)
{
	if (Display_SizeY <= y)
		return NULL;
	if (Display_WriteY <= y)
		return NULL;
	if (window_display_ring_y(y, &y))
		return NULL;

	return Display_Root + y;
}

static struct windows_character *windows_display_get_x(
	struct windows_line *ptr, unsigned x)
{
	if (Display_SizeX <= x)
		return NULL;
	if (ptr->write <= x)
		return NULL;

	return ptr->ptr + x;
}

static struct windows_character *windows_display_get(unsigned x, unsigned y)
{
	struct windows_line *ptr;

	if (Display_SizeX <= x || Display_SizeY <= y)
		return NULL;
	ptr = windows_display_get_y(y);
	if (ptr == NULL)
		return NULL;

	return windows_display_get_x(ptr, x);
}


/*
 *  Draw
 */
int windows_display_character(unsigned x, unsigned y, unsigned width, unicode c)
{
	struct windows_character *ptr;
	
	ptr = windows_display_set(x, y);
	if (ptr == NULL)
		return 0;
	*ptr = Display_Font;
	ptr->c = c;
	ptr->empty = 0;
	ptr->data = 1;
	if (width < 2) {
		ptr->wide = 0;
	}
	else {
		ptr->wide = 1;
		ptr = windows_display_set(x + 1, y);
		if (ptr) {
			ptr->c = 0;
			ptr->empty = 1;
		}
	}

	return 0;
}

void windows_display_paint_nolock(HDC hDC)
{
	unsigned x, y;
	struct windows_character *ptr;

	for (y = 0; y < Window_SizeY; y++) {
		for (x = 0; x < Window_SizeX; x++) {
			ptr = windows_display_get(x, y);
			if (ptr == NULL)
				break;
			if (ptr->empty)
				continue;
			(void)windows_draw_character_nolock(hDC, x, y, ptr->c);
		}
	}
}
