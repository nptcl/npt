#include "heap.h"
#include "prompt.h"
#include "terme_arch.h"
#include "terme_display.h"
#include "terme_object.h"

#ifdef LISP_DEBUG
#define TERME_DISPLAY_ALLOC_X	3
#define TERME_DISPLAY_ALLOC_Y	3
#else
#define TERME_DISPLAY_ALLOC_X	64
#define TERME_DISPLAY_ALLOC_Y	64
#endif
#define TERME_DISPLAY_SIZE(x, y) (((x / y) + 1) * y)

/*
 *  line
 */
struct terme_line_body {
	unsigned wide : 1;
	unsigned ignore : 1;
	PromptMode mode : 5;
	unicode c : 21;
};
struct terme_line_root {
	unsigned alloc, size, now;
};

static struct terme_line_root *struct_terme_line(addr pos)
{
	Check(! terme_line_p(pos), "type error");
	return (struct terme_line_root *)terme_pointer(pos);
}

static struct terme_line_body *struct_terme_body(addr pos)
{
	Check(! terme_line_p(pos), "type error");
	return (struct terme_line_body *)
		(terme_pointer(pos) + sizeoft(struct terme_line_root));
}

static void terme_line_heap(addr *ret, unsigned alloc)
{
	addr pos;
	size_t size;
	struct terme_line_root *str;

	size = sizeoft(struct terme_line_root);
	size += sizeoft(struct terme_line_body) * alloc;
	heap_body(&pos, LISPSYSTEM_TERME, size);
	terme_set_type(pos, terme_type_line);
	str = struct_terme_line(pos);
	str->alloc = alloc;
	str->size = 0;
	str->now = 0;
	*ret = pos;
}

static void terme_line_resize_x(addr *ret, addr src_line,
		unsigned alloc_x, unsigned new_x)
{
	addr new_line;
	struct terme_line_root *str;
	struct terme_line_body *body1, *body2;

	str = struct_terme_line(src_line);
	if (alloc_x <= str->alloc) {
		*ret = src_line;
		return;
	}

	/* new object */
	terme_line_heap(&new_line, alloc_x);
	body1 = struct_terme_body(src_line);
	body2 = struct_terme_body(new_line);

	/* copy */
	memcpy(body2, body1, sizeoft(struct terme_line_body) * str->size);

	/* set array */
	str = struct_terme_line(new_line);
	str->alloc = alloc_x;
	str->size = new_x;
}

static void terme_line_resize(addr *ret, addr pos, unsigned alloc_x, unsigned new_x)
{
	struct terme_line_root *str;

	if (pos == Nil)
		terme_line_heap(&pos, alloc_x);
	else
		terme_line_resize_x(&pos, pos, alloc_x, new_x);
	str = struct_terme_line(pos);
	str->size = new_x;
	str->now = 0;
	*ret = pos;
}


/*
 *  display
 */
struct terme_display_struct {
	unsigned alloc_x, alloc_y;
	unsigned size_x, size_y;
	unsigned now_x, now_y;
};

static struct terme_display_struct *struct_terme_display(addr pos)
{
	Check(! terme_display_p(pos), "type error");
	return (struct terme_display_struct *)terme_pointer(pos);
}

void terme_display_build(addr *ret)
{
	addr pos;
	struct terme_display_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_TERME, 1, sizeoft(struct terme_display_struct));
	terme_set_type(pos, terme_type_display);
	str = struct_terme_display(pos);
	str->alloc_x = 0;
	str->alloc_y = 0;
	str->size_x = 0;
	str->size_y = 0;
	str->now_x = 0;
	str->now_y = 0;
	*ret = pos;
}

static void terme_display_resize_y(addr pos, unsigned new_y)
{
	unsigned alloc_y, src_y, y;
	addr new_array, src_array, value;
	struct terme_display_struct *str;

	/* new array */
	alloc_y = TERME_DISPLAY_SIZE(new_y, TERME_DISPLAY_ALLOC_Y);
	terme_get(pos, 0, &src_array);
	vector_heap(&new_array, alloc_y);

	/* copy */
	str = struct_terme_display(pos);
	src_y = str->size_y;
	for (y = 0; y < src_y; y++) {
		getarray(src_array, y, &value);
		setarray(new_array, y, value);
	}

	/* set array */
	terme_set(pos, 0, new_array);
	str->alloc_y = alloc_y;
	str->size_y = new_y;
}

static void terme_display_resize_x(addr pos, unsigned new_x)
{
	unsigned alloc_x, size_y, y;
	addr array, value;
	struct terme_display_struct *str;

	alloc_x = TERME_DISPLAY_SIZE(new_x, TERME_DISPLAY_ALLOC_X);
	terme_get(pos, 0, &array);

	/* copy */
	str = struct_terme_display(pos);
	size_y = str->size_y;
	for (y = 0; y < size_y; y++) {
		getarray(array, y, &value);
		terme_line_resize(&value, value, alloc_x, new_x);
		setarray(array, y, value);
	}

	/* set array */
	str->alloc_x = alloc_x;
	str->size_x = new_x;
}

static void terme_display_resize(addr pos, unsigned x, unsigned y)
{
	struct terme_display_struct *str;

	/* y */
	str = struct_terme_display(pos);
	if (str->alloc_y < y)
		terme_display_resize_y(pos, y);
	str->size_y = y;

	/* x */
	terme_display_resize_x(pos, x);
	str->size_x = x;

	/* cursor */
	str->now_x = 0;
	str->now_y = 0;
}

int terme_display_clear_(Execute ptr)
{
	unsigned x, y;
	addr pos;

	Return(terme_root_display_(ptr, &pos));
	terme_arch_size_get(&x, &y);
	terme_display_resize(pos, x, y);

	return 0;
}


/*
 *  operator
 */
static void terme_line_ignore(addr pos, unsigned x, PromptMode mode)
{
	unsigned first, last, i;
	struct terme_line_root *str;
	struct terme_line_body *body, *cursor;

	str = struct_terme_line(pos);
	if (x <= str->now)
		return;
	first = (str->now < str->size)? str->now: str->size;
	last = (x < str->size)? x: str->size;
	body = struct_terme_body(pos);

	for (i = first; i <= last; i++) {
		cursor = body + i;
		cursor->wide = 0;
		cursor->ignore = 1;
		cursor->mode = mode;
		cursor->c = 0;
	}
}

static void terme_line_write_char(addr pos, unsigned x,
		unicode c, unsigned width, PromptMode mode)
{
	int wide_p;
	struct terme_line_root *str;
	struct terme_line_body *body;

	str = struct_terme_line(pos);
	terme_line_ignore(pos, x, mode);

	/* character */
	wide_p = (width != 1);
	body = struct_terme_body(pos);
	body += str->now;
	body->wide = wide_p;
	body->ignore = 0;
	body->mode = mode;
	body->c = c;
	str->now = x + 1;

	/* eastasian width */
	if (! wide_p)
		return;
	if (str->size <= str->now)
		return;
	body++;
	body->wide = 0;
	body->ignore = 1;
	body->mode = mode;
	body->c = 0;
	str->now++;
}

int terme_display_write_char_(Execute ptr, unicode c, unsigned width, PromptMode mode)
{
	addr pos, array, line;
	struct terme_display_struct *str;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->size_y <= str->now_y) {
		str->now_x += width;
		return 0;
	}
	terme_get(pos, 0, &array);
	getarray(array, str->now_y, &line);
	terme_line_write_char(line, str->now_x, c, width, mode);
	str->now_x += width;

	return 0;
}

int terme_display_terpri_(Execute ptr)
{
	addr pos;
	struct terme_display_struct *str;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->now_y < str->size_y)
		str->now_y++;
	str->now_x = 0;

	return 0;
}

int terme_display_delete_line_right_(Execute ptr)
{
	addr pos;
	struct terme_display_struct *str;
	struct terme_line_root *root;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->size_y <= str->now_y)
		return 0;

	/* line */
	terme_get(pos, 0, &pos);
	getarray(pos, str->now_y, &pos);
	root = struct_terme_line(pos);
	if (str->now_x < root->now)
		root->now = str->now_x;

	return 0;
}

int terme_display_left_(Execute ptr, int n)
{
	unsigned value;
	addr pos;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	value = (unsigned)n;
	if (str->now_x <= value)
		str->now_x = 0;
	else
		str->now_x -= value;

	return 0;
}

int terme_display_right_(Execute ptr, int n)
{
	addr pos;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	str->now_x += (unsigned)n;

	return 0;
}

int terme_display_up_(Execute ptr, int n)
{
	addr pos;
	unsigned value;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	value = (unsigned)n;
	if (str->now_y <= value)
		str->now_y = 0;
	else
		str->now_y -= value;

	return 0;
}

int terme_display_down_(Execute ptr, int n)
{
	addr pos;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	str->now_y += (unsigned)n;
	if (str->now_y > str->size_y)
		str->now_y = str->size_y;

	return 0;
}

int terme_display_first_up_(Execute ptr, int n)
{
	unsigned value;
	addr pos;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	value = (unsigned)n;
	if (str->now_y <= value)
		str->now_y = 0;
	else
		str->now_y -= value;
	str->now_x = 0;

	return 0;
}

int terme_display_first_down_(Execute ptr, int n)
{
	addr pos;
	struct terme_display_struct *str;

	if (n <= 0)
		return 0;
	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	str->now_y += (unsigned)n;
	if (str->now_y > str->size_y)
		str->now_y = str->size_y;
	str->now_x = 0;

	return 0;
}

int terme_display_delete_line_(Execute ptr)
{
	addr pos;
	struct terme_display_struct *str;
	struct terme_line_root *root;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->size_y <= str->now_y)
		return 0;

	/* line */
	terme_get(pos, 0, &pos);
	getarray(pos, str->now_y, &pos);
	root = struct_terme_line(pos);
	root->now = 0;

	return 0;
}

int terme_display_getwidth_(Execute ptr, unsigned *ret)
{
	addr pos;
	struct terme_display_struct *str;
	struct terme_line_body *body;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->size_y <= str->now_y || str->size_x <= str->now_x)
		return Result(ret, 0);

	/* line */
	terme_get(pos, 0, &pos);
	getarray(pos, str->now_y, &pos);
	body = struct_terme_body(pos);
	body += str->now_x;
	return Result(ret, body->wide? 2: 1);
}

int terme_display_previous_(Execute ptr, int *ret)
{
	addr pos;
	struct terme_display_struct *str;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);

	/* out of range */
	if (str->size_y <= str->now_y)
		return Result(ret, -1);

	/* previous line */
	if (str->now_x == 0) {
		return Result(ret, str->now_y? 0: -1);
	}

	/* current line */
	return Result(ret, 1);
}

int terme_display_getlast_(Execute ptr, unsigned *ret)
{
	addr pos;
	struct terme_display_struct *str;
	struct terme_line_root *root;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	if (str->size_y <= str->now_y)
		return Result(ret, 0);

	/* line */
	terme_get(pos, 0, &pos);
	getarray(pos, str->now_y, &pos);
	root = struct_terme_line(pos);
	return Result(ret, root->now);
}

int terme_display_delete_page_(Execute ptr)
{
	unsigned y;
	addr pos, array, line;
	struct terme_display_struct *str;
	struct terme_line_root *root;

	Return(terme_root_display_(ptr, &pos));
	str = struct_terme_display(pos);
	terme_get(pos, 0, &array);
	for (y = 0; y < str->now_y; y++) {
		getarray(array, y, &line);
		root = struct_terme_line(line);
		root->now = 0;
	}
	str->now_x = 0;
	str->now_y = 0;

	return 0;
}


/*
 *  restore
 */
int terme_display_restore_(Execute ptr, unsigned *rx, unsigned *ry)
{
	*rx = *ry = 0;
	return 0;
}

