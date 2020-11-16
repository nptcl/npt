#include "cons.h"
#include "execute.h"
#include "execute_values.h"
#include "integer.h"
#include "object.h"
#include "question.h"
#include "stream_memory.h"
#include "stream_object.h"
#include "strtype.h"
#include "typedef.h"

/*
 *  memory-stream
 */
static int question_memory_stream_size_(Execute ptr, addr pos, addr args)
{
	size_t size;

	getsize_memory_stream(pos, &size);
	make_index_integer_heap(&pos, size);
	setresult_control(ptr, pos);

	return 0;
}

static int question_memory_stream_array_(Execute ptr, addr pos, addr args)
{
	size_t size;

	getarray_memory_stream(pos, &size);
	make_index_integer_heap(&pos, size);
	setresult_control(ptr, pos);

	return 0;
}

static int question_memory_stream_cache_(Execute ptr, addr pos, addr args)
{
	int cache;

	cache = getcache_memory_stream(pos);
	fixnum_heap(&pos, (fixnum)cache);
	setresult_control(ptr, pos);

	return 0;
}

static int question_memory_stream_(Execute ptr, addr pos, addr args)
{
	int check;
	addr type;

	Return_getcar(args, &type);
	/* size */
	Return(string_designer_equalp_char_(type, "size", &check));
	if (check)
		return question_memory_stream_size_(ptr, pos, args);

	/* array */
	Return(string_designer_equalp_char_(type, "array", &check));
	if (check)
		return question_memory_stream_array_(ptr, pos, args);

	/* cache */
	Return(string_designer_equalp_char_(type, "cache", &check));
	if (check)
		return question_memory_stream_cache_(ptr, pos, args);

	/* error */
	setresult_control(ptr, Nil);
	return 0;
}


/*
 *  question
 */
int question_values_(Execute ptr, addr pos, addr args)
{
	if (memory_stream_p(pos))
		return question_memory_stream_(ptr, pos, args);

	/* error */
	setresult_control(ptr, Nil);
	return 0;
}

