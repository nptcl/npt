#include "heap.h"
#include "terme_data.h"
#include "terme_history.h"
#include "terme_object.h"
#include "typedef.h"

#define TERME_HISTORY_SIZE	64

struct terme_history_struct {
	int now, index, size;
};

static struct terme_history_struct *struct_terme_history(addr pos)
{
	Check(! terme_history_p(pos), "type error");
	return (struct terme_history_struct *)terme_pointer(pos);
}

void terme_history_build(addr *ret)
{
	addr pos, array;
	struct terme_history_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_TERME, 1, sizeoft(struct terme_history_struct));
	terme_set_type(pos, terme_type_history);
	str = struct_terme_history(pos);
	str->now = 0;
	str->index = 0;
	str->size = TERME_HISTORY_SIZE;

	/* array */
	vector_heap(&array, str->size);
	terme_set(pos, 0, array);
	*ret = pos;
}

int terme_history_clear_(Execute ptr)
{
	addr pos;
	struct terme_history_struct *str;

	Return(terme_root_history_(ptr, &pos));
	str = struct_terme_history(pos);
	str->now = 0;

	return 0;
}


/*
 *  return
 */
int terme_history_return_(Execute ptr)
{
	addr value, pos, array;
	struct terme_history_struct *str;

	/* data */
	Return(terme_data_make_(ptr, &value, 0));

	/* object */
	Return(terme_root_history_(ptr, &pos));
	str = struct_terme_history(pos);
	terme_get(pos, 0, &array);
	setarray(array, str->index, value);

	/* index */
	str->now = 0;
	str->index = (str->index + 1) % str->size;

	return 0;
}


/*
 *  switch
 */
static int terme_history_save_(Execute ptr, addr pos)
{
	addr array, value;
	struct terme_history_struct *str;

	str = struct_terme_history(pos);
	terme_get(pos, 0, &array);
	Return(terme_data_make_(ptr, &value, 0));
	setarray(array, str->index, value);

	return 0;
}

static int terme_history_get_(Execute ptr, int now, addr *value, int *ret)
{
	addr pos, array;
	struct terme_history_struct *str;

	Return(terme_root_history_(ptr, &pos));
	str = struct_terme_history(pos);
	terme_get(pos, 0, &array);
	if (now < 0 || str->size <= now) {
		*value = Nil;
		return Result(ret, 0);
	}
	now = (str->size + str->index - now) % str->size;
	getarray(array, now, value);

	return Result(ret, 1);
}

static int terme_history_update_(Execute ptr, int now, int *ret)
{
	int check;
	addr pos;

	Return(terme_history_get_(ptr, now, &pos, &check));
	if ((! check) || pos == Nil)
		return Result(ret, 0);
	Return(terme_data_copy_(ptr, pos));

	return Result(ret, 1);
}

int terme_history_select_(Execute ptr, int diff, int *ret)
{
	int check, now;
	addr pos;
	struct terme_history_struct *str;

	Return(terme_root_history_(ptr, &pos));
	str = struct_terme_history(pos);
	if (str->now == 0) {
		Return(terme_history_save_(ptr, pos));
	}
	now = str->now + diff;
	Return(terme_history_update_(ptr, now, &check));
	if (! check)
		return Result(ret, 0);
	str->now = now;

	/* update */
	return Result(ret, 1);
}

