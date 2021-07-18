#include "eastasian_unicode.h"
#include "heap.h"
#include "strvect.h"
#include "terme_data.h"
#include "terme_object.h"
#include "typedef.h"

#define TERME_DATA_SIZE		4096

/*
 *  string
 */
struct terme_data_character {
	unsigned width : 2;
	unicode c : 21;
};

static void terme_string_build(addr *ret)
{
	addr pos;

	heap_body(&pos, LISPSYSTEM_TERME,
			sizeoft(struct terme_data_character) * TERME_DATA_SIZE);
	terme_set_type(pos, terme_type_string);
	*ret = pos;
}

static struct terme_data_character *struct_data_character(addr pos)
{
	Check(! terme_string_p(pos), "type error");
	return (struct terme_data_character *)terme_pointer(pos);
}


/*
 *  data
 */
enum terme_data_index {
	terme_data_array,
	terme_data_size
};

struct terme_data_struct {
	unsigned alloc, size, now;
};

static struct terme_data_struct *struct_terme_data(addr pos)
{
	Check(! terme_data_p(pos), "type error");
	return (struct terme_data_struct *)terme_pointer(pos);
}

void terme_data_build(addr *ret)
{
	addr pos, value;
	struct terme_data_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_TERME,
			terme_data_size,
			sizeoft(struct terme_data_struct));
	terme_set_type(pos, terme_type_data);
	terme_string_build(&value);
	terme_set(pos, terme_data_array, value);

	str = struct_terme_data(pos);
	str->alloc = TERME_DATA_SIZE;
	str->size = 0;
	str->now = 0;

	*ret = pos;
}

static void terme_data_get_body(addr pos, addr *ret)
{
	Check(! terme_data_p(pos), "type error");
	terme_get(pos, terme_data_array, ret);
}

int terme_data_clear_(Execute ptr)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	str->size = 0;
	str->now = 0;

	return 0;
}


/*
 *  insert
 */
static int terme_data_shift_right_(addr pos, unicode c, unsigned width)
{
	int i, diff, now, size, src, dst;
	addr array;
	struct terme_data_struct *str;
	struct terme_data_character *body;

	/* data */
	str = struct_terme_data(pos);
	now = str->now;
	size = str->size;
	if (size <= now)
		return 0;

	/* string */
	terme_data_get_body(pos, &array);
	body = struct_data_character(array);

	/* loop */
	diff = size - now;
	for (i = 0; i < diff; i++) {
		dst = size - i;
		src = dst - 1;
		body[dst] = body[src];
	}

	return 0;
}

static void terme_data_insert_set(addr pos, unsigned now, unicode c, unsigned width)
{
	addr array;
	struct terme_data_character *body;

	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	body += now;
	body->c = c;
	body->width = width;
}

int terme_data_insert_(Execute ptr, unicode c, unsigned *rwidth, int *ret)
{
	unsigned width;
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->alloc <= str->size) {
		*rwidth = 0;
		return Result(ret, 1);
	}
	if (str->size < str->now) {
		*rwidth = 0;
		return Result(ret, 1);
	}

	width = eastasian_width(c);
	Return(terme_data_shift_right_(pos, c, width));
	terme_data_insert_set(pos, str->now, c, width);
	str->size++;

	/* result */
	*rwidth = width;
	return Result(ret, 0);
}

int terme_data_next_(Execute ptr)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->now < str->size)
		str->now++;

	return 0;
}

int terme_data_push_(Execute ptr, unicode c, unsigned *rwidth, int *ret)
{
	int check;
	unsigned width;

	Return(terme_data_insert_(ptr, c, &width, &check));
	if (check) {
		*rwidth = 0;
		return Result(ret, 1);
	}

	*rwidth = width;
	*ret = 0;
	return terme_data_next_(ptr);
}

int terme_data_make_(Execute ptr, addr *ret, int eol)
{
	unsigned size, i;
	addr pos, array, value;
	struct terme_data_struct *str;
	struct terme_data_character *body;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	size = str->size;
	strvect_heap(&value, size + (eol? 1ULL :0ULL));
	terme_data_get_body(pos, &array);
	body = struct_data_character(array);

	for (i = 0; i < size; i++) {
		Return(strvect_setc_(value, i, body[i].c));
	}
	if (eol) {
		Return(strvect_setc_(value, i, 0x0A));
	}

	return Result(ret, value);
}

int terme_data_copy_(Execute ptr, addr value)
{
	addr pos, array;
	struct terme_data_struct *str;
	struct terme_data_character *body;
	unicode c;
	size_t size, i;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	terme_data_get_body(pos, &array);
	body = struct_data_character(array);

	strvect_length(value, &size);
	if (str->alloc < size)
		size = str->alloc;
	for (i = 0; i < size; i++) {
		strvect_getc(value, i, &c);
		body[i].c = c;
		body[i].width = eastasian_width(c);
	}
	str->size = (unsigned)size;
	str->now = 0;

	return 0;
}

int terme_data_size_(Execute ptr, unsigned *ret)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	return Result(ret, str->size);
}


/*
 *  access
 */
void terme_data_get_value(addr pos, unsigned *rnow, unsigned *rsize)
{
	struct terme_data_struct *str;

	str = struct_terme_data(pos);
	if (rnow)
		*rnow = str->now;
	if (rsize)
		*rsize = str->size;
}

int terme_data_get_character(addr pos, unsigned i, unicode *retc, unsigned *retw)
{
	addr array;
	struct terme_data_struct *str;
	struct terme_data_character *body;

	str = struct_terme_data(pos);
	if (str->size <= i) {
		if (retc)
			*retc = 0;
		if (retw)
			*retw = 0;
		return 1;
	}

	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	body += i;
	if (retc)
		*retc = body->c;
	if (retw)
		*retw = body->width;
	return 0;
}


/*
 *  operator
 */
int terme_data_left_(Execute ptr, unsigned *ret)
{
	unsigned width;
	addr pos, array;
	struct terme_data_struct *str;
	struct terme_data_character *body;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->now == 0)
		return Result(ret, 0); /* error */

	/* width */
	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	str->now--;
	width = body[str->now].width;

	/* move */
	return Result(ret, width);
}

int terme_data_right_(Execute ptr, unsigned *ret)
{
	unsigned width;
	addr pos, array;
	struct terme_data_struct *str;
	struct terme_data_character *body;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->size <= str->now)
		return Result(ret, 0); /* error */

	/* width */
	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	width = body[str->now].width;
	str->now++;

	/* move */
	return Result(ret, width);
}

int terme_data_first_(Execute ptr)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	str->now = 0;

	return 0;
}

int terme_data_last_(Execute ptr)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	str->now = str->size;

	return 0;
}

static void terme_data_shift_left_(addr pos, unsigned x, unsigned size)
{
	addr array;
	struct terme_data_character *body;

	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	for (; x < size; x++)
		body[x] = body[x + 1];
}

static void terme_data_delete_index(addr pos, unsigned index, int *ret)
{
	struct terme_data_struct *str;

	str = struct_terme_data(pos);
	if (str->size <= index) {
		*ret = 0;
		return;
	}

	/* shift */
	terme_data_shift_left_(pos, index, str->size);
	str->size--;
	*ret = 1;
}

int terme_data_delete_(Execute ptr, int *ret)
{
	int check;
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->size <= str->now)
		return Result(ret, 0);

	terme_data_delete_index(pos, str->now, &check);
	return Result(ret, 1);
}

int terme_data_backspace_(Execute ptr, int *ret)
{
	int check;
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->now == 0)
		return Result(ret, 0);

	/* backspace */
	terme_data_delete_index(pos, str->now - 1U, &check);
	if (! check)
		return Result(ret, 0);
	str->now--;

	return Result(ret, 1);
}

static int terme_data_rmleft_shift_(addr pos, unsigned index, unsigned size)
{
	unsigned i;
	addr array;
	struct terme_data_character *body;

	terme_data_get_body(pos, &array);
	body = struct_data_character(array);
	for (i = 0; i < size; i++)
		body[i] = body[i + index];

	return 0;
}

int terme_data_rmleft_(Execute ptr, int *ret)
{
	unsigned size;
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	/* do nothing */
	if (str->now == 0)
		return Result(ret, 0);

	/* all delete */
	if (str->now == str->size) {
		str->size = 0;
		str->now = 0;
		return Result(ret, 1);
	}

	/* shift */
	size = str->size - str->now;
	Return(terme_data_rmleft_shift_(pos, str->now, size));
	str->size = size;
	str->now = 0;

	return Result(ret, 1);
}

int terme_data_rmright_(Execute ptr, int *ret)
{
	addr pos;
	struct terme_data_struct *str;

	Return(terme_root_data_(ptr, &pos));
	str = struct_terme_data(pos);
	if (str->now < 0 || str->size <= str->now)
		return Result(ret, 0);

	/* shift */
	str->size = str->now;
	return Result(ret, 1);
}

