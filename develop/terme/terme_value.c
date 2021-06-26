#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "constant.h"
#include "heap.h"
#include "strvect.h"
#include "symbol.h"
#include "terme_value.h"
#include "typedef.h"

#define TERME_VALUE_SIZE		4096
#define TERME_VALUE_HISTORY		5

static int terme_history_index = 0;

enum terme_value {
	terme_value_prompt,
	terme_value_data,
	terme_value_history,
	terme_value_size
};

/*
 *  build
 */
static int terme_value_array_(addr *ret)
{
	addr pos;
	struct array_struct *str;

	Return(array_heap_(&pos, 1, TERME_VALUE_SIZE));
	str = ArrayInfoStruct(pos);
	str->fillpointer = 1;
	Return(array_character_alloc_(NULL, pos));
	str->front = 0;

	return Result(ret, pos);
}

static int terme_value_heap_(addr *ret)
{
	addr pos, x;

	/* object */
	vector2_heap(&pos, terme_value_size);

	/* array */
	Return(terme_value_array_(&x));
	SetArrayA2(pos, terme_value_data, x);

	/* queue */
	vector_heap(&x, TERME_VALUE_HISTORY);
	setarray(pos, terme_value_history, x);

	return Result(ret, pos);
}

void terme_build(void)
{
	addr symbol, pos;

	GetConst(SYSTEM_TERME, &symbol);
	Error(terme_value_heap_(&pos));
	SetValueSymbol(symbol, pos);
}

static int terme_get_value_(Execute ptr, addr *ret)
{
	addr symbol;
	GetConst(SYSTEM_TERME, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}


/*
 *  prompt
 */
int terme_set_prompt_(Execute ptr, addr value)
{
	addr array;

	Return(terme_get_value_(ptr, &array));
	SetArrayA2(array, terme_value_prompt, value);

	return 0;
}

int terme_get_prompt_(Execute ptr, addr *ret)
{
	addr array;

	Return(terme_get_value_(ptr, &array));
	GetArrayA2(array, terme_value_prompt, ret);

	return 0;
}


/*
 *  data
 */
static int terme_data_array_(Execute ptr, addr *ret)
{
	addr pos;

	Return(terme_get_value_(ptr, &pos));
	GetArrayA2(pos, terme_value_data, ret);

	return 0;
}

int terme_data_init_(Execute ptr)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);
	str->front = 0;

	return 0;
}

static int terme_data_shift_right_(addr pos, size_t x, size_t y)
{
	unicode c;
	size_t i, size;

	if (x == y)
		return 0;
	size = y - x;
	for (i = 0; i < size; i++) {
		Return(array_get_unicode_(pos, y - i - 1, &c));
		Return(array_set_character_(pos, y - i, c));
	}

	return 0;
}

int terme_data_push_(Execute ptr, int index, unicode c, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);
	if (str->size <= str->front)
		return Result(ret, 0);
	if (index < 0 || str->front < index)
		return Result(ret, 0);
	/* shift */
	Return(terme_data_shift_right_(pos, (size_t)index, str->front));

	/* set */
	Return(array_set_character_(pos, index, c));
	str->front++;

	return Result(ret, 1);
}

int terme_data_get_(Execute ptr, int index, unicode *value, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, 0);

	*ret = 1;
	return array_get_unicode_(pos, (size_t)index, value);
}

int terme_data_size_(Execute ptr, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);

	return Result(ret, (int)str->front);
}

static int terme_data_shift_left_(addr pos, size_t x, size_t y)
{
	unicode c;

	for (; x < y; x++) {
		Return(array_get_unicode_(pos, x + 1, &c));
		Return(array_set_character_(pos, x, c));
	}

	return 0;
}

int terme_data_delete_(Execute ptr, int index, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
	str = ArrayInfoStruct(pos);
	if (index < 0 || str->front <= index)
		return Result(ret, 0);
	/* shift */
	Return(terme_data_shift_left_(pos, (size_t)index, str->front));
	str->front--;

	return Result(ret, 1);
}

int terme_data_delete_left_(Execute ptr, int index, int *ret)
{
	int i, size;
	unicode c;
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
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
	for (i = 0; i < size; i++) {
		Return(array_get_unicode_(pos, i + index, &c));
		Return(array_set_character_(pos, i, c));
	}
	str->front = size;

	return Result(ret, 1);
}

int terme_data_delete_right_(Execute ptr, int index, int *ret)
{
	addr pos;
	struct array_struct *str;

	Return(terme_data_array_(ptr, &pos));
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

	Return(terme_data_array_(ptr, &pos));
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
	addr pos;

	Return(terme_get_value_(ptr, &pos));
	GetArrayA2(pos, terme_value_history, &pos);
	setarray(pos, terme_history_index, value);

	return 0;
}

static int terme_history_next_(Execute ptr)
{
	int sizei;
	addr pos;
	size_t size;

	Return(terme_get_value_(ptr, &pos));
	GetArrayA2(pos, terme_value_history, &pos);
	lenarray(pos, &size);
	sizei = (int)size;
	terme_history_index = (terme_history_index + 1) % sizei;

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
	int sizei;
	addr pos;
	size_t size;

	Return(terme_get_value_(ptr, &pos));
	GetArrayA2(pos, terme_value_history, &pos);
	lenarray(pos, &size);
	sizei = (int)size;
	if (index < 0 || sizei <= index) {
		*value = Nil;
		return Result(ret, 0);
	}
	index = (sizei + terme_history_index - index) % sizei;
	getarray(pos, index, value);

	return Result(ret, 1);
}

static int terme_history_copy_(Execute ptr, addr pos)
{
	unicode c;
	addr array;
	struct array_struct *str;
	size_t size, i;

	Return(terme_data_array_(ptr, &array));
	str = ArrayInfoStruct(array);
	strvect_length(pos, &size);
	if (str->size < size)
		size = str->size;

	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		Return(array_set_character_(array, i, c));
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

