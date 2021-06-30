#include "array.h"
#include "array_access.h"
#include "bit.h"
#include "eastasian.h"
#include "eastasian_unicode.h"
#include "execute.h"
#include "strvect.h"
#include "terme_data.h"
#include "terme_value.h"
#include "typedef.h"

/*
 *  data
 */
static int terme_data_array_(Execute ptr, addr *array, addr *width)
{
	if (array) {
		Return(terme_value_data_(ptr, array));
	}
	if (width) {
		Return(terme_value_width_(ptr, width));
	}

	return 0;
}

int terme_data_init_(Execute ptr)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, NULL));
	str = ArrayInfoStruct(pos);
	str->front = 0;

	return 0;
}

static int terme_data_shift_right_(addr pos, addr width, int x, int y)
{
	int i, size, src, dst, check;
	unicode c;

	if (x == y)
		return 0;
	size = y - x;
	for (i = 0; i < size; i++) {
		dst = y - i;
		src = dst - 1;

		Return(array_get_unicode_(pos, src, &c));
		Return(array_set_character_(pos, dst, c));

		Return(bitmemory_getint_(width, src, &check));
		Return(bitmemory_setint_(width, dst, check));
	}

	return 0;
}

int terme_data_push_(Execute ptr, int index, unicode c, int *ret)
{
	int check;
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	if (str->size <= str->front)
		return Result(ret, -1);
	if (index < 0 || str->front < index)
		return Result(ret, -1);

	/* shift */
	Return(terme_data_shift_right_(pos, width, index, (int)str->front));

	/* set */
	Return(array_set_character_(pos, index, c));
	str->front++;

	/* width */
	check = (int)eastasian_width(c);
	Return(bitmemory_setint_(width, index, check == 2));

	return Result(ret, check);
}

int terme_data_get_(Execute ptr, int index, unicode *value, int *ret)
{
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, -1);

	Return(bitmemory_getint_(width, index, ret));
	return array_get_unicode_(pos, (size_t)index, value);
}

int terme_data_get_width_(Execute ptr, int index, int *ret)
{
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, -1);

	return bitmemory_getint_(width, index, ret);
}

int terme_data_size_(Execute ptr, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, NULL));
	str = ArrayInfoStruct(pos);

	return Result(ret, (int)str->front);
}

int terme_data_size_width_(Execute ptr, int *size, int *width)
{
	addr pos;
	struct array_struct *str;
	size_t value;

	Return(terme_data_array_(ptr, &pos, NULL));
	Return(eastasian_length_(pos, &value, NULL));
	str = ArrayInfoStruct(pos);
	*size = (int)str->front;
	*width = (int)value;

	return 0;
}

int terme_data_allwidth_(Execute ptr, int *ret)
{
	int size, i, all, check;
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	size = (int)str->front;
	all = 0;
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(width, i, &check));
		all += (check? 2: 1);
	}

	return Result(ret, all);
}

static int terme_data_shift_left_(addr pos, addr width, int x, int y)
{
	int check;
	unicode c;

	for (; x < y; x++) {
		Return(array_get_unicode_(pos, x + 1, &c));
		Return(array_set_character_(pos, x, c));

		Return(bitmemory_getint_(width, x + 1, &check));
		Return(bitmemory_setint_(width, x, check));
	}

	return 0;
}

int terme_data_delete_(Execute ptr, int index, int *ret)
{
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, 0);
	/* shift */
	Return(terme_data_shift_left_(pos, width, index, (int)str->front));
	str->front--;

	return Result(ret, 1);
}

static int terme_data_delete_left_shift_(addr pos, addr width, int index, int size)
{
	int i, check;
	unicode c;

	for (i = 0; i < size; i++) {
		Return(array_get_unicode_(pos, i + index, &c));
		Return(array_set_character_(pos, i, c));

		Return(bitmemory_getint_(width, i + index, &check));
		Return(bitmemory_setint_(width, i, check));
	}

	return 0;
}

int terme_data_delete_left_(Execute ptr, int index, int *ret)
{
	int size;
	addr pos, width;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, &width));
	str = ArrayInfoStruct(pos);
	/* do nothing */
	if (index == 0)
		return Result(ret, 0);

	/* all delete */
	if (index == str->front) {
		str->front = 0;
		return Result(ret, 1);
	}

	/* invalid */
	if (index < 0 || str->front < index)
		return Result(ret, 0);

	/* shift */
	size = str->front - index;
	Return(terme_data_delete_left_shift_(pos, width, index, size));
	str->front = size;

	return Result(ret, 1);
}

int terme_data_delete_right_(Execute ptr, int index, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos, NULL));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, 0);
	/* shift */
	str->front = index;

	return Result(ret, 1);
}

static int terme_data_heap_(Execute ptr, addr *ret, int eol)
{
	addr pos, make;
	unicode c;
	struct array_struct *str;
	size_t size, i;

	Return(terme_data_array_(ptr, &pos, NULL));
	str = ArrayInfoStruct(pos);
	size = str->front;
	strvect_heap(&make, size + (eol? 1: 0));

	for (i = 0; i < size; i++) {
		Return(array_get_unicode_(pos, i, &c));
		Return(strvect_setc_(make, i, c));
	}
	if (eol) {
		Return(strvect_setc_(make, i, 0x0A));
	}

	return Result(ret, make);
}

static int terme_history_set_(Execute ptr, addr value)
{
	int history;
	addr pos;

	Return(terme_value_history_(ptr, &pos));
	Return(get_history_index_terme_(ptr, &history));
	setarray(pos, history, value);

	return 0;
}

static int terme_history_next_(Execute ptr)
{
	int sizei, history;
	addr pos;
	size_t size;

	Return(terme_value_history_(ptr, &pos));
	lenarray(pos, &size);
	sizei = (int)size;

	Return(get_history_index_terme_(ptr, &history));
	history = (history + 1) % sizei;
	Return(set_history_index_terme_(ptr, history));

	return 0;
}

int terme_data_make_(Execute ptr, addr *ret)
{
	addr pos;

	/* history */
	Return(terme_data_heap_(ptr, &pos, 0));
	Return(terme_history_set_(ptr, pos));
	Return(terme_history_next_(ptr));

	/* result */
	return terme_data_heap_(ptr, ret, 1);
}

int terme_history_save_(Execute ptr)
{
	addr pos;
	Return(terme_data_heap_(ptr, &pos, 0));
	return terme_history_set_(ptr, pos);
}

static int terme_history_get_(Execute ptr, int index, addr *value, int *ret)
{
	int sizei, history;
	addr pos;
	size_t size;

	Return(terme_value_history_(ptr, &pos));
	lenarray(pos, &size);
	sizei = (int)size;
	if (index < 0 || sizei <= index) {
		*value = Nil;
		return Result(ret, 0);
	}

	Return(get_history_index_terme_(ptr, &history));
	index = (sizei + history - index) % sizei;
	getarray(pos, index, value);

	return Result(ret, 1);
}

static int terme_history_copy_(Execute ptr, addr pos)
{
	int check;
	unicode c;
	addr array, width;
	struct array_struct *str;
	size_t size, i;

	Return(terme_data_array_(ptr, &array, &width));
	str = ArrayInfoStruct(array);
	strvect_length(pos, &size);
	if (str->size < size)
		size = str->size;

	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		Return(array_set_character_(array, i, c));
		check = (int)eastasian_width(c);
		Return(bitmemory_setint_(width, i, check == 2));
	}
	str->front = size;

	return 0;
}

int terme_history_update_(Execute ptr, int index, int *ret)
{
	int check;
	addr pos;

	Return(terme_history_get_(ptr, index, &pos, &check));
	if ((! check) || pos == Nil)
		return Result(ret, 0);
	Return(terme_history_copy_(ptr, pos));

	return Result(ret, 1);
}
