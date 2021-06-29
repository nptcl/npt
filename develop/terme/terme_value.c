#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "bit.h"
#include "constant.h"
#include "heap.h"
#include "symbol.h"
#include "terme_value.h"
#include "typedef.h"

#define TERME_VALUE_SIZE		4096
#define TERME_VALUE_HISTORY		64

enum terme_index {
	terme_index_prompt,
	terme_index_data,
	terme_index_width,
	terme_index_history,
	terme_index_size
};

struct terme_struct {
	enum prompt_mode mode;
	int history_index;
};
#define PtrTerme(x) ((struct terme_struct *)PtrBodySS(x))

static struct terme_struct *struct_terme(addr pos)
{
	CheckType(pos, LISPSYSTEM_TERME);
	return PtrTerme(pos);
}

static void get_terme(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_TERME);
	GetArraySS(pos, index, ret);
}

static void set_terme(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_TERME);
	SetArraySS(pos, index, value);
}

static void terme_heap(addr *ret)
{
	addr pos;
	struct terme_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_TERME,
			terme_index_size, sizeoft(struct terme_struct));
	str = struct_terme(pos);
	str->mode = prompt_eval;
	str->history_index = 0;
	*ret = pos;
}


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
	terme_heap(&pos);

	/* array */
	Return(terme_value_array_(&x));
	set_terme(pos, terme_index_data, x);

	/* width */
	bitmemory_heap(&x, TERME_VALUE_SIZE);
	set_terme(pos, terme_index_width, x);

	/* queue */
	vector_heap(&x, TERME_VALUE_HISTORY);
	setarray(pos, terme_index_history, x);

	return Result(ret, pos);
}

void terme_build(void)
{
	addr symbol, pos;

	GetConst(SYSTEM_TERME, &symbol);
	pos = Nil;
	Error(terme_value_heap_(&pos));
	SetValueSymbol(symbol, pos);
}

static int terme_get_value_(Execute ptr, addr *ret)
{
	addr symbol;
	GetConst(SYSTEM_TERME, &symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

/* access */
int terme_value_data_(Execute ptr, addr *ret)
{
	addr pos;

	Return(terme_get_value_(ptr, &pos));
	get_terme(pos, terme_index_data, ret);

	return 0;
}

int terme_value_width_(Execute ptr, addr *ret)
{
	addr pos;

	Return(terme_get_value_(ptr, &pos));
	get_terme(pos, terme_index_width, ret);

	return 0;
}

int terme_value_history_(Execute ptr, addr *ret)
{
	addr pos;

	Return(terme_get_value_(ptr, &pos));
	get_terme(pos, terme_index_history, ret);

	return 0;
}

int get_history_index_terme_(Execute ptr, int *ret)
{
	addr pos;
	struct terme_struct *str;

	Return(terme_get_value_(ptr, &pos));
	str = struct_terme(pos);
	return Result(ret, str->history_index);
}

int set_history_index_terme_(Execute ptr, int value)
{
	addr pos;
	struct terme_struct *str;

	Return(terme_get_value_(ptr, &pos));
	str = struct_terme(pos);
	str->history_index = value;
	return 0;
}


/*
 *  prompt
 */
int terme_set_prompt_(Execute ptr, addr value, enum prompt_mode mode)
{
	addr pos;
	struct terme_struct *str;

	Return(terme_get_value_(ptr, &pos));
	set_terme(pos, terme_index_prompt, value);
	str = struct_terme(pos);
	str->mode = mode;

	return 0;
}

int terme_get_prompt_(Execute ptr, addr *value, enum prompt_mode *mode)
{
	addr pos;
	struct terme_struct *str;

	Return(terme_get_value_(ptr, &pos));
	if (value) {
		get_terme(pos, terme_index_prompt, value);
	}
	if (mode) {
		str = struct_terme(pos);
		*mode = str->mode;
	}

	return 0;
}

