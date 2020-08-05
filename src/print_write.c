#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "bignum_equal.h"
#include "bignum_object.h"
#include "bignum_output.h"
#include "bit.h"
#include "bytespec.h"
#include "callname.h"
#include "character.h"
#include "character_name.h"
#include "clos.h"
#include "cmpl.h"
#include "condition.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "execute.h"
#include "format_float.h"
#include "function.h"
#include "hashtable.h"
#include "heap.h"
#include "hold.h"
#include "integer.h"
#include "package.h"
#include "package_object.h"
#include "pathname.h"
#include "print.h"
#include "print_dispatch.h"
#include "print_write.h"
#include "quote.h"
#include "random_state.h"
#include "ratio.h"
#include "reader_table.h"
#include "restart.h"
#include "stream.h"
#include "stream_pretty.h"
#include "stream_string.h"
#include "strtype.h"
#include "symbol.h"
#include "type.h"
#include "type_name.h"
#include "type_object.h"

static calltype_print WriteCircleTable[LISPTYPE_SIZE];
static calltype_print WriteCallTable[LISPTYPE_SIZE];

static int write_circle_call_(Execute ptr, addr stream, addr pos);
static int write_print_call_(Execute ptr, addr stream, addr pos);

/*
 *  print-check object
 */
struct print_check_struct {
	unsigned first : 1;
	size_t index;
};

static struct print_check_struct *ptr_print_check(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_CHECK);
	return (struct print_check_struct *)PtrBodySS(pos);
}

static void set_object_print_check(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_CHECK);
	SetArraySS(pos, 0, value);
}

static int get_first_print_check(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_CHECK);
	return ptr_print_check(pos)->first;
}

static void set_first_print_check(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_CHECK);
	ptr_print_check(pos)->first = 1;
}

static size_t get_index_print_check(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_CHECK);
	return ptr_print_check(pos)->index;
}

static void print_check_heap(addr *ret, addr value)
{
	addr pos;
	struct print_check_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_PRINT_CHECK, 1, sizeoft(struct print_check_struct));
	str = ptr_print_check(pos);
	str->first = 0;
	str->index = 0;
	set_object_print_check(pos, value);
	*ret = pos;
}


/*
 *  print-write object
 */
struct print_write_struct {
	size_t index, depth;
};

static struct print_write_struct *ptr_print_write(addr pos)
{
	CheckType(pos, LISPSYSTEM_PRINT_WRITE);
	return (struct print_write_struct *)PtrBodySS(pos);
}

static void set_table_print_write(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_PRINT_WRITE);
	SetArraySS(pos, 0, value);
}

static void get_table_print_write(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_PRINT_WRITE);
	GetArraySS(pos, 0, ret);
}

static void print_write_heap(addr *ret)
{
	addr pos, table;
	struct print_write_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_PRINT_WRITE, 1, sizeoft(struct print_write_struct));
	str = ptr_print_write(pos);
	str->index = 1;
	str->depth = 0;
	hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQ);
	set_table_print_write(pos, table);
	*ret = pos;
}

_g void push_write_object(Execute ptr)
{
	addr symbol, pos;

	GetConst(SYSTEM_PRINT_WRITE, &symbol);
	print_write_heap(&pos);
	pushspecial_control(ptr, symbol, pos);
}

static void print_write_object(Execute ptr, addr *ret)
{
	addr symbol, pos;

	GetConst(SYSTEM_PRINT_WRITE, &symbol);
	getspecial_local(ptr, symbol, &pos);
	if (pos == Unbound) {
		print_write_heap(&pos);
		pushspecial_control(ptr, symbol, pos);
	}
	CheckType(pos, LISPSYSTEM_PRINT_WRITE);
	*ret = pos;
}

_g void getdepth_print_write(Execute ptr, size_t *ret)
{
	addr pos;
	print_write_object(ptr, &pos);
	*ret = ptr_print_write(pos)->depth;
}

_g void setdepth_print_write(Execute ptr, size_t value)
{
	addr pos;
	print_write_object(ptr, &pos);
	ptr_print_write(pos)->depth = value;
}

static void increment_print_write(addr write, addr check)
{
	struct print_write_struct *str1;
	struct print_check_struct *str2;

	CheckType(write, LISPSYSTEM_PRINT_WRITE);
	CheckType(check, LISPSYSTEM_PRINT_CHECK);
	str1 = ptr_print_write(write);
	str2 = ptr_print_check(check);
	if (str2->index == 0)
		str2->index = str1->index++;
}

static int intern_print_write_(Execute ptr, addr pos, int *ret)
{
	int check;
	addr write, cons;

	print_write_object(ptr, &write);
	get_table_print_write(write, &cons);
	Return(internp_hashheap_(cons, pos, &cons, &check));
	if (check == 0) {
		/* make */
		print_check_heap(&pos, pos);
		SetCdr(cons, pos);
		return Result(ret, 1);
	}
	else {
		/* found */
		GetCdr(cons, &pos);
		increment_print_write(write, pos);
		return Result(ret, 0);
	}
}

static int find_print_write_(Execute ptr, addr key, addr *value, int *ret)
{
	addr pos;

	print_write_object(ptr, &pos);
	get_table_print_write(pos, &pos);
	Return(findnil_hashtable_(pos, key, value));
	CheckType(*value, LISPSYSTEM_PRINT_CHECK);

	return Result(ret, ptr_print_check(*value)->index == 0);
}

_g void write_check_all_clear(Execute ptr)
{
	addr pos, key, value;

	print_write_object(ptr, &pos);
	get_table_print_write(pos, &pos);
	/* loop */
	hash_iterator_heap(&pos, pos);
	while (next_hash_iterator(pos, &key, &value)) {
		CheckType(value, LISPSYSTEM_PRINT_CHECK);
		ptr_print_check(value)->first = 0;
	}
}


/*
 *  default
 */
static int WriteBody_error_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(print_ascii_stream_(stream, "INVALID-OBJECT"));
	Return(type_name_p_(pos, &pos, &check));
	if (check)
		return 0;
	Return(write_char_stream_(stream, ' '));
	return write_print_call_(ptr, stream, pos);
}

static int WriteCall_error(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 0, 1, WriteBody_error_);
}

static int WriteCall_system(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 1, 1, NULL);
}


/*
 *  cons
 */
static int WriteCheckCall_cons_(Execute ptr, addr pos)
{
	int lenp, levelp, check;
	addr x;
	size_t len, level, depth, i;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return 0;

	/* intern */
	Return(intern_print_write_(ptr, pos, &check));
	if (check == 0)
		return 0;

	/* list */
	CheckType(pos, LISPTYPE_CONS);
	setdepth_print_write(ptr, depth + 1);
	for (i = 0; ; i++) {
		/* *print-length* */
		if (lenp && len <= i)
			break;
		/* cons */
		GetCons(pos, &x, &pos);
		Return(write_check_call_(ptr, x));
		if (! consp(pos))
			break;
		Return(intern_print_write_(ptr, pos, &check));
		if (check == 0)
			break;
	}
	setdepth_print_write(ptr, depth);

	return 0;
}

_g int pprint_pop_circle_(Execute ptr, addr stream, addr pos, int *ret)
{
	int check;
	addr x;
	size_t index;

	Return(find_print_write_(ptr, pos, &x, &check));
	if (check)
		return Result(ret, 0);
	/* found */
	if (get_first_print_check(x) == 0)
		return fmte_("Invalid loop object.", NULL);

	Return(print_ascii_stream_(stream, ". #"));
	index = get_index_print_check(x);
	Return(output_nosign_fixnum_(stream, index, 10, 1));
	/* #3# */
	Return(write_char_stream_(stream, '#'));
	return Result(ret, 1);
}

static int WriteCircle_find_(Execute ptr, addr stream, addr pos, int *ret)
{
	int check;
	addr x;
	size_t index;

	Return(find_print_write_(ptr, pos, &x, &check));
	if (check)
		return Result(ret, 0);
	/* found */
	Return(write_char_stream_(stream, '#'));
	index = get_index_print_check(x);
	Return(output_nosign_fixnum_(stream, index, 10, 1));
	/* first, second */
	if (get_first_print_check(x) == 0) {
		/* #3= (...) */
		Return(write_char_stream_(stream, '='));
		set_first_print_check(x);
		return Result(ret, 0);
	}
	else {
		/* #3# */
		Return(write_char_stream_(stream, '#'));
		return Result(ret, 1);
	}
}

_g int pprint_check_circle_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;
	addr x, stream;
	size_t index;

	CheckType(pos, LISPTYPE_CONS);
	Return(find_print_write_(ptr, pos, &x, &check));
	if (check) {
		*value = Nil;
		return Result(ret, 0);
	}

	/* found */
	open_output_string_stream(&stream, 0);
	Return(write_char_stream_(stream, '#'));
	index = get_index_print_check(x);
	Return(output_nosign_fixnum_(stream, index, 10, 1));
	/* first, second */
	if (get_first_print_check(x) == 0) {
		/* #3= (...) */
		Return(write_char_stream_(stream, '='));
		set_first_print_check(x);
		Return(string_stream_heap_(stream, value));
		return Result(ret, 0);
	}
	else {
		/* #3# */
		Return(write_char_stream_(stream, '#'));
		Return(string_stream_heap_(stream, value));
		return Result(ret, 1);
	}
}

static int WriteCircleCall_cons_check_(Execute ptr, addr pos, int *ret)
{
	if (! consp(pos))
		return Result(ret, 0);
	else
		return find_print_write_(ptr, pos, &pos, ret);
}

static int WriteCircleCall_cons_(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp, check;
	addr x;
	size_t i, len, level, depth;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return write_char_stream_(stream, '#');

	/* table */
	Return(WriteCircle_find_(ptr, stream, pos, &check));
	if (check)
		return 0;

	/* list */
	CheckType(pos, LISPTYPE_CONS);
	setdepth_print_write(ptr, depth + 1);
	Return(write_char_stream_(stream, '('));
	for (i = 0; ; i++) {
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			break;
		}
		/* cons */
		GetCons(pos, &x, &pos);
		Return(write_circle_call_(ptr, stream, x));
		if (pos == Nil)
			break;
		Return(WriteCircleCall_cons_check_(ptr, pos, &check));
		if (check) {
			Return(write_char_stream_(stream, ' '));
		}
		else {
			Return(print_ascii_stream_(stream, " . "));
			Return(write_circle_call_(ptr, stream, pos));
			break;
		}
	}
	Return(write_char_stream_(stream, ')'));
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCall_cons_(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp;
	addr x;
	size_t len, level, depth, i;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return write_char_stream_(stream, '#');

	/* list */
	CheckType(pos, LISPTYPE_CONS);
	setdepth_print_write(ptr, depth + 1);
	Return(write_char_stream_(stream, '('));
	for (i = 0; ; i++) {
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			break;
		}
		/* cons */
		GetCons(pos, &x, &pos);
		Return(write_print_call_(ptr, stream, x));
		if (pos == Nil)
			break;
		if (consp(pos)) {
			Return(write_char_stream_(stream, ' '));
		}
		else {
			Return(print_ascii_stream_(stream, " . "));
			Return(write_print_call_(ptr, stream, pos));
			break;
		}
	}
	Return(write_char_stream_(stream, ')'));
	setdepth_print_write(ptr, depth);

	return 0;
}


/*
 *  vector
 */
static int WriteCheckCall_vector_(Execute ptr, addr pos)
{
	int lenp, levelp, check;
	addr x;
	size_t len, level, depth, size, i;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return 0;

	/* intern */
	Return(intern_print_write_(ptr, pos, &check));
	if (check == 0)
		return 0;

	/* list */
	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	setdepth_print_write(ptr, depth + 1);
	for (i = 0; i < size; i++) {
		/* *print-length* */
		if (lenp && len <= i)
			break;
		/* vector */
		getarray(pos, i, &x);
		Return(write_check_call_(ptr, x));
	}
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCircleCall_vector_(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp, check;
	addr x;
	size_t len, level, depth, size, i;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return write_char_stream_(stream, '#');

	/* table */
	Return(WriteCircle_find_(ptr, stream, pos, &check));
	if (check)
		return 0;

	/* list */
	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	setdepth_print_write(ptr, depth + 1);
	Return(print_ascii_stream_(stream, "#("));
	for (i = 0; i < size; i++) {
		if (i != 0) {
			Return(write_char_stream_(stream, ' '));
		}
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			break;
		}
		/* vector */
		getarray(pos, i, &x);
		Return(write_circle_call_(ptr, stream, x));
	}
	Return(write_char_stream_(stream, ')'));
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCall_vector_(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp;
	addr x;
	size_t len, level, depth, size, i;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return write_char_stream_(stream, '#');

	/* list */
	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	setdepth_print_write(ptr, depth + 1);
	Return(print_ascii_stream_(stream, "#("));
	for (i = 0; i < size; i++) {
		if (i != 0) {
			Return(write_char_stream_(stream, ' '));
		}
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			break;
		}
		/* vector */
		getarray(pos, i, &x);
		Return(write_print_call_(ptr, stream, x));
	}
	Return(write_char_stream_(stream, ')'));
	setdepth_print_write(ptr, depth);

	return 0;
}


/*
 *  array
 */
struct write_array_struct {
	Execute ptr;
	addr pos, stream;
	const size_t *data;
	size_t dimension, depth, index;
};

static void make_write_array(struct write_array_struct *str,
		Execute ptr, addr stream, addr pos, const size_t *data, size_t dimension)
{
	str->ptr = ptr;
	str->stream = stream;
	str->pos = pos;
	str->data = data;
	str->dimension = dimension;
	str->depth = 0;
	str->index = 0;
}

static int WriteArray_specialized_p(addr pos)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	return (str->dimension == 1)
		&& (str->type == ARRAY_TYPE_BIT || str->type == ARRAY_TYPE_CHARACTER);
}

static int WriteCheckCall_array_print_(struct write_array_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = str->ptr->local;
	push_local(local, &stack);
	Return(array_get_(local, str->pos, str->index++, &pos));
	Return(write_check_call_(str->ptr, pos));
	rollback_local(local, stack);

	return 0;
}

static int WriteCheckCall_array_call_(struct write_array_struct *str)
{
	int lenp;
	const size_t *data;
	size_t len, i, loop, depth;

	/* output */
	depth = str->depth;
	if (str->dimension <= str->depth)
		return WriteCheckCall_array_print_(str);

	/* restrict */
	data = str->data;
	loop = data[depth];
	lenp = length_print(str->ptr, &len);

	str->depth++;
	for (i = 0; i < loop; i++) {
		/* *print-length* */
		if (lenp && len <= i) {
			str->index += loop - i;
			break;
		}
		/* array */
		Return(WriteCheckCall_array_call_(str));
	}
	str->depth--;

	return 0;
}

static int WriteCheckCall_array_(Execute ptr, addr pos)
{
	int check;
	const size_t *data;
	size_t dimension, level, depth;
	struct write_array_struct str;

	/* *print-level* */
	check = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);
	if (check && level <= depth)
		return 0;

	/* intern */
	Return(intern_print_write_(ptr, pos, &check));
	if (check == 0)
		return 0;

	/* specialized */
	if (WriteArray_specialized_p(pos))
		return 0;

	/* prefix */
	dimension = ArrayInfoStruct(pos)->dimension;
	data = array_ptrsize(pos);

	/* body */
	setdepth_print_write(ptr, depth + 1);
	make_write_array(&str, ptr, Nil, pos, data, dimension);
	Return(WriteCheckCall_array_call_(&str));
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteArray_bit_(Execute ptr, addr stream, addr pos)
{
	int value;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(print_ascii_stream_(stream, "#*"));
	for (i = 0; i < size; i++) {
		Return(array_get_bit_(pos, i, &value));
		Return(write_char_stream_(stream, value? '1': '0'));
	}

	return 0;
}

static int WriteCall_string_(Execute ptr, addr stream, addr object);
static int WriteArray_specialized_(Execute ptr, addr stream, addr pos)
{
	switch (ArrayInfoStruct(pos)->type) {
		case ARRAY_TYPE_BIT:
			return WriteArray_bit_(ptr, stream, pos);

		case ARRAY_TYPE_CHARACTER:
			return WriteCall_string_(ptr, stream, pos);

		default:
			return fmte_("Invalid array type.", NULL);
	}
}

static int WriteCircleCall_array_print_(struct write_array_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = str->ptr->local;
	push_local(local, &stack);
	Return(array_get_(local, str->pos, str->index++, &pos));
	Return(write_circle_call_(str->ptr, str->stream, pos));
	rollback_local(local, stack);

	return 0;
}

static int WriteCall_array_print_(struct write_array_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = str->ptr->local;
	push_local(local, &stack);
	Return(array_get_(local, str->pos, str->index++, &pos));
	Return(write_print_call_(str->ptr, str->stream, pos));
	rollback_local(local, stack);

	return 0;
}

static int WriteCircleCall_array_call_(struct write_array_struct *str)
{
	int lenp;
	addr stream;
	const size_t *data;
	size_t len, i, loop, depth;

	/* output */
	depth = str->depth;
	if (str->dimension <= depth)
		return WriteCircleCall_array_print_(str);

	/* restrict */
	stream = str->stream;
	data = str->data;
	loop = data[depth];
	lenp = length_print(str->ptr, &len);

	Return(write_char_stream_(stream, '('));
	str->depth++;
	for (i = 0; i < loop; i++) {
		if (i != 0) {
			Return(write_char_stream_(stream, ' '));
		}
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			str->index += loop - i;
			break;
		}
		/* array */
		Return(WriteCircleCall_array_call_(str));
	}
	str->depth--;
	Return(write_char_stream_(stream, ')'));

	return 0;
}

static int WriteCall_array_call_(struct write_array_struct *str)
{
	int lenp;
	addr stream;
	const size_t *data;
	size_t len, i, loop, depth;

	/* output */
	depth = str->depth;
	if (str->dimension <= str->depth)
		return WriteCall_array_print_(str);

	/* restrict */
	stream = str->stream;
	data = str->data;
	loop = data[depth];
	lenp = length_print(str->ptr, &len);

	Return(write_char_stream_(stream, '('));
	str->depth++;
	for (i = 0; i < loop; i++) {
		if (i != 0) {
			Return(write_char_stream_(stream, ' '));
		}
		/* *print-length* */
		if (lenp && len <= i) {
			Return(print_ascii_stream_(stream, "..."));
			str->index += loop - i;
			break;
		}
		/* array */
		Return(WriteCall_array_call_(str));
	}
	str->depth--;
	Return(write_char_stream_(stream, ')'));

	return 0;
}

static int WriteCircleCall_array_(Execute ptr, addr stream, addr pos)
{
	int check;
	const size_t *data;
	size_t dimension, level, depth;
	struct write_array_struct str;

	/* *print-level* */
	check = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);
	if (check && level <= depth)
		return write_char_stream_(stream, '#');

	/* table */
	Return(WriteCircle_find_(ptr, stream, pos, &check));
	if (check)
		return 0;

	/* specialized */
	if (WriteArray_specialized_p(pos))
		return WriteArray_specialized_(ptr, stream, pos);

	/* prefix */
	Return(write_char_stream_(stream, '#'));
	dimension = ArrayInfoStruct(pos)->dimension;
	data = array_ptrsize(pos);
	if (dimension != 1) {
		Return(output_nosign_index_(stream, dimension, 10, 1));
		Return(write_char_stream_(stream, 'A'));
	}

	/* body */
	setdepth_print_write(ptr, depth + 1);
	make_write_array(&str, ptr, stream, pos, data, dimension);
	Return(WriteCircleCall_array_call_(&str));
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCall_array_(Execute ptr, addr stream, addr pos)
{
	int check;
	const size_t *data;
	size_t dimension, level, depth;
	struct write_array_struct str;

	/* *print-level* */
	check = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);
	if (check && level <= depth)
		return write_char_stream_(stream, '#');

	/* specialized */
	if (WriteArray_specialized_p(pos))
		return WriteArray_specialized_(ptr, stream, pos);

	/* prefix */
	Return(write_char_stream_(stream, '#'));
	dimension = ArrayInfoStruct(pos)->dimension;
	data = array_ptrsize(pos);
	if (dimension != 1) {
		Return(output_nosign_index_(stream, dimension, 10, 1));
		Return(write_char_stream_(stream, 'A'));
	}

	/* body */
	setdepth_print_write(ptr, depth + 1);
	make_write_array(&str, ptr, stream, pos, data, dimension);
	Return(WriteCall_array_call_(&str));
	setdepth_print_write(ptr, depth);

	return 0;
}


/*
 *  symbol
 */
static int WriteSymbol_direct_norm_(addr stream, addr pos)
{
	size_t i, size;
	unicode u;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		Return(write_char_stream_(stream, u));
	}

	return 0;
}

static int WriteSymbol_downcase_norm_(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		Return(write_char_stream_(stream, toLowerUnicode(u)));
	}

	return 0;
}

static int WriteSymbol_upcase_norm_(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		Return(write_char_stream_(stream, toUpperUnicode(u)));
	}

	return 0;
}

static int WriteSymbol_up_cap_norm_(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (isAlphanumeric(u)) {
			if (check) {
				Return(write_char_stream_(stream, u));
				check = 0;
			}
			else {
				Return(write_char_stream_(stream, toLowerUnicode(u)));
			}
		}
		else {
			Return(write_char_stream_(stream, u));
			check = 1;
		}
	}

	return 0;
}

static int WriteSymbol_down_cap_norm_(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (isAlphanumeric(u)) {
			if (check) {
				Return(write_char_stream_(stream, toUpperUnicode(u)));
				check = 0;
			}
			else {
				Return(write_char_stream_(stream, u));
			}
		}
		else {
			Return(write_char_stream_(stream, u));
			check = 1;
		}
	}

	return 0;
}

static int WriteSymbol_check_invert_(addr pos, enum PrintCase *ret)
{
	enum PrintCase check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	check = PrintCase_unread;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (isUpperCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_upcase;
			}
			else if (check != PrintCase_upcase) {
				return Result(ret, PrintCase_preserve);
			}
		}
		else if (isLowerCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_downcase;
			}
			else if (check != PrintCase_downcase) {
				return Result(ret, PrintCase_preserve);
			}
		}
	}

	return Result(ret, check);
}

static int WriteSymbol_invert_norm_(addr stream, addr pos)
{
	enum PrintCase type;

	Return(WriteSymbol_check_invert_(pos, &type));
	switch (type) {
		case PrintCase_upcase:
			return WriteSymbol_downcase_norm_(stream, pos);

		case PrintCase_downcase:
			return WriteSymbol_upcase_norm_(stream, pos);

		default:
			return WriteSymbol_direct_norm_(stream, pos);
	}
}

static int WriteSymbol_check_escape(unicode c)
{
	return (c == ' ')
		|| (c == '`') || (c == ',')
		|| (c == '(') || (c == ')')
		|| (c == '|') || (c == '\\')
		|| (c == ':') || (c == ';')
		|| (c == '\'') || (c == '"');
}

static int WriteSymbol_direct_escape_(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (u == '\\' || u == '|') {
			Return(write_char_stream_(stream, '\\'));
		}
		Return(write_char_stream_(stream, u));
	}

	return 0;
}

static int WriteSymbol_check_upcase_escape_(addr pos, int *ret)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (WriteSymbol_check_escape(u))
			return Result(ret, 1);
		if (isLowerCase(u))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int WriteSymbol_up_up_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_upcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_direct_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_downcase_escape_(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		Return(write_char_stream_(stream, toLowerUnicode(u)));
	}

	return 0;
}

static int WriteSymbol_up_down_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_upcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_downcase_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_capitalize_escape_(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (isAlphanumeric(u)) {
			if (check) {
				Return(write_char_stream_(stream, toUpperUnicode(u)));
				check = 0;
			}
			else {
				Return(write_char_stream_(stream, toLowerUnicode(u)));
			}
		}
		else {
			Return(write_char_stream_(stream, u));
			check = 1;
		}
	}

	return 0;
}

static int WriteSymbol_up_cap_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_upcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_capitalize_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_escape_(Execute ptr,
		addr stream, addr pos, int (*call)(addr, addr))
{
	int exportp, check;
	addr package, value;

	Return(getpackage_(ptr, &value));
	GetPackageSymbol(pos, &package);
	if (package == Nil) {
		/* gensym */
		if (gensym_print(ptr)) {
			Return(print_ascii_stream_(stream, "#:"));
		}
		goto final;
	}

	Return(checksymbol_package_(pos, value, &check));
	if (check) {
		/* no package name */
		goto final;
	}

	if (keywordp(pos)) {
		Return(print_ascii_stream_(stream, ":"));
		goto final;
	}

	if (package == value) {
		goto final;
	}
	Return(externalp_package_(pos, value, &check));
	if (check) {
		/* package name */
		Return(exportp_package_(pos, package, &exportp));
		Return(getname_package_(package, &package));
		Return((*call)(stream, package));
		Return(print_ascii_stream_(stream, exportp? ":": "::"));
		goto final;
	}

final:
	/* symbol name */
	GetNameSymbol(pos, &pos);
	return (*call)(stream, pos);
}

static int WriteSymbol_check_downcase_escape_(addr pos, int *ret)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (WriteSymbol_check_escape(u))
			return Result(ret, 1);
		if (isUpperCase(u))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int WriteSymbol_upcase_escape_(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		Return(write_char_stream_(stream, toUpperUnicode(u)));
	}

	return 0;
}

static int WriteSymbol_down_up_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_downcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_upcase_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_down_down_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_downcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_direct_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_down_cap_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_downcase_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_capitalize_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_check_preserve_escape_(addr pos, int *ret)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (WriteSymbol_check_escape(u))
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int WriteSymbol_preserve_output_(addr stream, addr pos)
{
	int check;

	Return(WriteSymbol_check_preserve_escape_(pos, &check));
	if (check) {
		Return(write_char_stream_(stream, '|'));
		Return(WriteSymbol_direct_escape_(stream, pos));
		Return(write_char_stream_(stream, '|'));
	}
	else {
		Return(WriteSymbol_direct_escape_(stream, pos));
	}

	return 0;
}

static int WriteSymbol_check_invert_escape_(addr pos, enum PrintCase *ret)
{
	enum PrintCase check;
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	check = PrintCase_unread;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &u));
		if (WriteSymbol_check_escape(u)) {
			return Result(ret, PrintCase_escape);
		}
		else if (isUpperCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_upcase;
			}
			else if (check != PrintCase_upcase) {
				return Result(ret, PrintCase_preserve);
			}
		}
		else if (isLowerCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_downcase;
			}
			else if (check != PrintCase_downcase) {
				return Result(ret, PrintCase_preserve);
			}
		}
	}

	return Result(ret, check);
}

static int WriteSymbol_invert_output_(addr stream, addr pos)
{
	enum PrintCase type;

	Return(WriteSymbol_check_invert_escape_(pos, &type));
	switch (type) {
		case PrintCase_upcase:
			return WriteSymbol_downcase_escape_(stream, pos);

		case PrintCase_downcase:
			return WriteSymbol_upcase_escape_(stream, pos);

		case PrintCase_escape:
			Return(write_char_stream_(stream, '|'));
			Return(WriteSymbol_direct_escape_(stream, pos));
			Return(write_char_stream_(stream, '|'));
			return 0;

		default:
			return WriteSymbol_direct_escape_(stream, pos);
	}
}

static int WriteSymbol_upcase_upcase_(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_up_up_output_);
	else
		return WriteSymbol_direct_norm_(stream, pos);
}

static int WriteSymbol_upcase_downcase_(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_up_down_output_);
	else
		return WriteSymbol_downcase_norm_(stream, pos);
}

static int WriteSymbol_upcase_capitalize_(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_up_cap_output_);
	else
		return WriteSymbol_up_cap_norm_(stream, pos);
}

static int WriteSymbol_upcase_(Execute ptr, addr stream, addr pos)
{
	enum PrintCase value;

	Return(case_print_(ptr, &value));
	switch (value) {
		case PrintCase_upcase:
			return WriteSymbol_upcase_upcase_(ptr, stream, pos);

		case PrintCase_downcase:
			return WriteSymbol_upcase_downcase_(ptr, stream, pos);

		case PrintCase_capitalize:
			return WriteSymbol_upcase_capitalize_(ptr, stream, pos);

		default:
			return fmte_("printcase error", NULL);
	}
}

static int WriteSymbol_downcase_upcase_(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_down_up_output_);
	else
		return WriteSymbol_upcase_norm_(stream, pos);
}

static int WriteSymbol_downcase_downcase_(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_down_down_output_);
	else
		return WriteSymbol_direct_norm_(stream, pos);
}

static int WriteSymbol_downcase_capitalize_(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_down_cap_output_);
	else
		return WriteSymbol_down_cap_norm_(stream, pos);
}

static int WriteSymbol_downcase_(Execute ptr, addr stream, addr pos)
{
	enum PrintCase value;

	Return(case_print_(ptr, &value));
	switch (value) {
		case PrintCase_upcase:
			return WriteSymbol_downcase_upcase_(ptr, stream, pos);

		case PrintCase_downcase:
			return WriteSymbol_downcase_downcase_(ptr, stream, pos);

		case PrintCase_capitalize:
			return WriteSymbol_downcase_capitalize_(ptr, stream, pos);

		default:
			return fmte_("printcase error", NULL);
	}
}

static int WriteSymbol_preserve_(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_preserve_output_);
	else
		return WriteSymbol_direct_norm_(stream, pos);
}

static int WriteSymbol_invert_(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		return WriteSymbol_escape_(ptr, stream, pos, WriteSymbol_invert_output_);
	else
		return WriteSymbol_invert_norm_(stream, pos);
}

static int WriteCall_symbol_(Execute ptr, addr stream, addr pos)
{
	switch (readcase_readtable(ptr)) {
		case ReadTable_upcase:
			return WriteSymbol_upcase_(ptr, stream, pos);

		case ReadTable_downcase:
			return WriteSymbol_downcase_(ptr, stream, pos);

		case ReadTable_preserve:
			return WriteSymbol_preserve_(ptr, stream, pos);

		case ReadTable_invert:
			return WriteSymbol_invert_(ptr, stream, pos);

		default:
			return fmte_("*readtable* case error", NULL);
	}
}


/*
 *  type
 */
static int WriteCall_type_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	Return(type_object_(&pos, pos));
	Return(print_ascii_stream_(stream, "#<TYPE "));
	Return(prin1_print(ptr, stream, pos));
	Return(print_ascii_stream_(stream, ">"));

	return 0;
}


/*
 *  clos, structure
 */
static int WriteCall_clos_(Execute ptr, addr stream, addr pos)
{
	addr generic;

	Check(! closp(pos), "type error");
	GetConst(COMMON_PRINT_OBJECT, &generic);
	getfunction_global(generic, &generic);

	return callclang_funcall(ptr, &pos, generic, pos, stream, NULL);
}


/*
 *  character
 */
static int WriteCall_fixnum_value_(addr stream, fixnum value, unsigned base)
{
	/* zero */
	if (value == 0)
		return write_char_stream_(stream, '0');

	/* output */
	if (value < 0) {
		Return(write_char_stream_(stream, '-'));
	}

	return output_nosign_fixnum_(stream, value, base, 1);
}

static int WriteCall_character_name_(addr stream, unicode u)
{
	if (isStandardType(u)) {
		Return(write_char_stream_(stream, u));
	}
	else {
		Return(write_char_stream_(stream, 'u'));
		Return(WriteCall_fixnum_value_(stream, (fixnum)u, 16));
	}

	return 0;
}

static int WriteCall_character_string_(addr stream, addr string)
{
	unicode c;
	size_t i, size;

	string_length(string, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(string, i, &c));
		Return(write_char_stream_(stream, c));
	}

	return 0;
}

static int WriteCall_character_(Execute ptr, addr stream, addr object)
{
	addr pos;
	unicode c;

	if (! escape_print(ptr)) {
		GetCharacter(object, &c);
		return write_char_stream_(stream, c);
	}

	Return(findtable_char_name_(&pos, object));
	if (pos != Nil) {
		/* found */
		Return(print_ascii_stream_(stream, "#\\"));
		Return(WriteCall_character_string_(stream, pos));
	}
	else {
		/* not found */
		Return(print_ascii_stream_(stream, "#\\"));
		GetCharacter(object, &c);
		Return(WriteCall_character_name_(stream, c));
	}

	return 0;
}


/*
 *  string
 */
static int WriteCall_string_(Execute ptr, addr stream, addr object)
{
	unicode c;
	size_t size, i;

	string_length(object, &size);
	if (escape_print(ptr)) {
		Return(write_char_stream_(stream, '\"'));
		for (i = 0; i < size; i++) {
			Return(string_getc_(object, i, &c));
			if (c == '\"' || c == '\\') {
				Return(write_char_stream_(stream, '\\'));
			}
			Return(write_char_stream_(stream, c));
		}
		Return(write_char_stream_(stream, '\"'));
	}
	else {
		for (i = 0; i < size; i++) {
			Return(string_getc_(object, i, &c));
			Return(write_char_stream_(stream, c));
		}
	}

	return 0;
}


/*
 *  hash-table
 */
static int WriteBody_hashtable_(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t count;

	/* #<HASH-TABLE :TEST EQL :COUNT 123 0x1234...> */
	gettest_symbol_hashtable(pos, &value);
	getcount_hashtable(pos, &count);
	/* output */
	Return(print_ascii_stream_(stream, ":TEST "));
	Return(prin1_print(ptr, stream, value));
	Return(print_ascii_stream_(stream, " :COUNT "));
	Return(output_nosign_index_(stream, count, 10, 0));

	return 0;
}

static int WriteCall_hashtable_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 1, 1, WriteBody_hashtable_);
}


/*
 *  fixnum
 */
static int WriteCall_radix_front_(addr stream, unsigned base)
{
	char buffer[8];

	Check(! isBaseChar(base), "base error");
	switch (base) {
		case 2:
			return print_ascii_stream_(stream, "#b");

		case 8:
			return print_ascii_stream_(stream, "#o");

		case 16:
			return print_ascii_stream_(stream, "#x");

		default:
			snprintf(buffer, 8, "#%ur", base);
			return print_ascii_stream_(stream, buffer);
	}
}

static int WriteCall_fixnum_(Execute ptr, addr stream, addr object)
{
	int radix;
	unsigned base;
	fixnum value;

	Return(base_print_(ptr, &base));
	radix = radix_print(ptr);
	if (radix && base != 10) {
		Return(WriteCall_radix_front_(stream, base));
	}
	GetFixnum(object, &value);
	Return(WriteCall_fixnum_value_(stream, value, base));
	if (radix && base == 10) {
		Return(write_char_stream_(stream, '.'));
	}

	return 0;
}


/*
 *  bignum
 */
static int WriteCall_bignum_value_(LocalRoot local,
		addr stream, int sign, addr object, unsigned base)
{
	/* zero */
	if (zerop_bignum(object))
		return write_char_stream_(stream, '0');

	/* output */
	if (sign) {
		Return(write_char_stream_(stream, '-'));
	}

	return output_nosign_bignum_(local, stream, object, base, 1);
}

static int WriteCall_bignum_sign_(Execute ptr, addr stream, int sign, addr object)
{
	int radix;
	unsigned base;

	Return(base_print_(ptr, &base));
	radix = radix_print(ptr);
	if (radix && base != 10) {
		Return(WriteCall_radix_front_(stream, base));
	}
	Return(WriteCall_bignum_value_(ptr->local, stream, sign, object, base));
	if (radix && base == 10) {
		Return(write_char_stream_(stream, '.'));
	}

	return 0;
}

static int WriteCall_bignum_(Execute ptr, addr stream, addr object)
{
	int sign;
	GetSignBignum(object, &sign);
	return WriteCall_bignum_sign_(ptr, stream, sign, object);
}


/*
 *  ratio
 */
static int WriteCall_ratio_(Execute ptr, addr stream, addr object)
{
	int sign;
	addr check;
	unsigned base;

	/* zero */
	if (zerop_ratio(object))
		return write_char_stream_(stream, '0');

	/* integer */
	GetDenomRatio(object, &check);
	if (equal_value_nosign_bignum(check, 1)) {
		GetSignRatio(object, &sign);
		GetNumerRatio(object, &check);
		return WriteCall_bignum_sign_(ptr, stream, sign, check);
	}

	/* ratio */
	Return(base_print_(ptr, &base));
	if (radix_print(ptr)) {
		Return(WriteCall_radix_front_(stream, base));
	}
	GetSignRatio(object, &sign);
	if (sign) {
		Return(write_char_stream_(stream, '-'));
	}

	return output_nosign_ratio_(ptr->local, stream, object, base, 1);
}


/*
 *  float
 */
static int WriteCall_single_float_(Execute ptr, addr stream, addr object)
{
	int markerp, marker, check;
	enum ReadTable_float type;
	single_float value;

	GetSingleFloat(object, &value);
	Return(float_readtable_(ptr, &type));
	markerp = (type != ReadTable_single);
	marker = markerp? 'F': 'E';
	Return(fmtfloat_princ_single_float_(stream, value, markerp, marker, &check));
	if (check)
		return fmte_("Invalid float value.", NULL);

	return 0;
}

static int WriteCall_double_float_(Execute ptr, addr stream, addr object)
{
	int markerp, marker, check;
	enum ReadTable_float type;
	double_float value;

	GetDoubleFloat(object, &value);
	Return(float_readtable_(ptr, &type));
	markerp = (type != ReadTable_double);
	marker = markerp? 'D': 'E';
	Return(fmtfloat_princ_double_float_(stream, value, markerp, marker, &check));
	if (check)
		return fmte_("Invalid float value.", NULL);

	return 0;
}

static int WriteCall_long_float_(Execute ptr, addr stream, addr object)
{
	int markerp, marker, check;
	enum ReadTable_float type;
	long_float value;

	GetLongFloat(object, &value);
	Return(float_readtable_(ptr, &type));
	markerp = (type != ReadTable_long);
	marker = markerp? 'L': 'E';
	Return(fmtfloat_princ_long_float_(stream, value, markerp, marker, &check));
	if (check)
		return fmte_("Invalid float value.", NULL);

	return 0;
}


/*
 *  complex
 */
static int WriteCall_complex_(Execute ptr, addr stream, addr object)
{
	addr real, imag;

	GetRealComplex(object, &real);
	GetImagComplex(object, &imag);
	Return(print_ascii_stream_(stream, "#C("));
	Return(write_print_call_(ptr, stream, real));
	Return(write_char_stream_(stream, ' '));
	Return(write_print_call_(ptr, stream, imag));
	Return(write_char_stream_(stream, ')'));

	return 0;
}


/*
 *  callname
 */
static int WriteBody_callname_(Execute ptr, addr stream, addr pos)
{
	name_callname_heap(pos, &pos);
	Return(print_ascii_stream_(stream, "CALLNAME "));
	return write_print_call_(ptr, stream, pos);
}

static int WriteCall_callname_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 0, 1, WriteBody_callname_);
}


/*
 *  function
 */
static int WriteBody_function_(Execute ptr, addr stream, addr pos)
{
	const char *name;
	struct function_struct *str;

	/* type */
	str = StructFunction(pos);
	if (str->compiled)
		name = "COMPILED-FUNCTION ";
	else
		name = "FUNCTION ";
	Return(print_ascii_stream_(stream, name));

	/* name */
	GetNameFunction(pos, &pos);
	if (pos == Nil) {
		Return(print_ascii_stream_(stream, "LAMBDA"));
	}
	else {
		if (RefCallNameType(pos) == CALLNAME_SYMBOL) {
			GetCallName(pos, &pos);
			Return(write_print_call_(ptr, stream, pos));
		}
		else {
			GetCallName(pos, &pos);
			Return(print_ascii_stream_(stream, "(SETF "));
			Return(write_print_call_(ptr, stream, pos));
			Return(print_ascii_stream_(stream, ")"));
		}
	}

	return 0;
}

static int WriteCall_function_(Execute ptr, addr stream, addr pos)
{
	int ident;
	addr name;

	/* #<FUNCTION NAME> */
	GetNameFunction(pos, &name);
	ident = (name == Nil);
	return print_unreadable_object_(ptr, stream, pos, 0, ident, WriteBody_function_);
}


/*
 *  index
 */
static int WriteBody_index_(Execute ptr, addr stream, addr pos)
{
	LocalRoot local;
	LocalStack stack;
	size_t size;

	GetIndex(pos, &size);
	local = ptr->local;
	push_local(local, &stack);
	Return(write_print_call_(ptr, stream, intsizea(local, size)));
	rollback_local(local, stack);

	return 0;
}

static int WriteCall_index_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 0, 0, WriteBody_index_);
}


/*
 *  package
 */
static int WriteBody_package_(Execute ptr, addr stream, addr pos)
{
	Return(getname_package_(pos, &pos));
	return print_string_stream_(stream, pos);
}

static int WriteCall_package_(Execute ptr, addr stream, addr pos)
{
	/* #<PACKAGE NAME> */
	return print_unreadable_object_(ptr, stream, pos, 1, 0, WriteBody_package_);
}


/*
 *  random-state
 */
static int WriteBody_random_state_(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_new_control(ptr, &control);
	push_escape_print(ptr, 0);
	push_readably_print(ptr, 0);
	push_radix_print(ptr, 1);
	push_base_print(ptr, 16);
	Return(push_case_print_(ptr, PrintCase_upcase));
	make_bignum_random_state_local(ptr->local, pos, &pos);
	Return(WriteCall_bignum_(ptr, stream, pos));

	return free_control_(ptr, control);
}

static int WriteCall_random_state_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 1, 0, WriteBody_random_state_);
}


/*
 *  pathname
 */
static int WriteCall_pathname_(Execute ptr, addr stream, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(name_pathname_local_(ptr, pos, &pos));
	if (escape_print(ptr)) {
		Return(print_ascii_stream_(stream, "#P"));
	}
	Return(WriteCall_string_(ptr, stream, pos));
	rollback_local(local, stack);

	return 0;
}


/*
 *  stream
 */
static int WriteBody_stream_(Execute ptr, addr stream, addr pos)
{
	struct StructStream *str;

	str = PtrStructStream(pos);
	switch (str->type) {
		case StreamType_BinaryInput:
			return print_ascii_stream_(stream, "FILE-INPUT BINARY");

		case StreamType_BinaryOutput:
			return print_ascii_stream_(stream, "FILE-OUTPUT BINARY");

		case StreamType_BinaryIO:
			return print_ascii_stream_(stream, "FILE-IO BINARY");

		case StreamType_CharacterInput:
			return print_ascii_stream_(stream, "FILE-INPUT CHARACTER");

		case StreamType_CharacterOutput:
			return print_ascii_stream_(stream, "FILE-OUTPUT CHARACTER");

		case StreamType_CharacterIO:
			return print_ascii_stream_(stream, "FILE-IO CHARACTER");

		case StreamType_BincharInput:
			return print_ascii_stream_(stream, "FILE-INPUT SYSTEM");

		case StreamType_BincharOutput:
			return print_ascii_stream_(stream, "FILE-OUTPUT SYSTEM");

		case StreamType_BincharIO:
			return print_ascii_stream_(stream, "FILE-IO SYSTEM");

		case StreamType_StringInput:
			return print_ascii_stream_(stream, "STREAM STRING-INPUT");

		case StreamType_StringOutput:
			return print_ascii_stream_(stream, "STREAM STRING-OUTPUT");

		case StreamType_Synonym:
			return print_ascii_stream_(stream, "SYNONYM-STREAM");

		case StreamType_BroadCast:
			return print_ascii_stream_(stream, "BROADCAST-STREAM");

		case StreamType_Concatenated:
			return print_ascii_stream_(stream, "CONCATENATED-STREAM");

		case StreamType_TwoWay:
			return print_ascii_stream_(stream, "TWO-WAY-STREAM");

		case StreamType_Echo:
			return print_ascii_stream_(stream, "ECHO-STREAM");

		case StreamType_Prompt:
			return print_ascii_stream_(stream, "PROMPT-STREAM");

		case StreamType_Pretty:
			return print_ascii_stream_(stream, "PRETTY-STREAM");

		default:
			return print_ascii_stream_(stream, "STREAM");
	}
}

static int WriteCall_stream_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 0, 1, WriteBody_stream_);
}


/*
 *  quote
 */
static int WriteCall_quote_(Execute ptr, addr stream, addr pos)
{
	if (quote_back_p(pos)) {
		getprint_quote(pos, &pos);
		Return(write_char_stream_(stream, '`'));
		return write_print_call_(ptr, stream, pos);
	}
	if (quote_comma_p(pos)) {
		getprint_quote(pos, &pos);
		Return(write_char_stream_(stream, ','));
		return write_print_call_(ptr, stream, pos);
	}
	if (quote_atsign_p(pos)) {
		getprint_quote(pos, &pos);
		Return(print_ascii_stream_(stream, ",@"));
		return write_print_call_(ptr, stream, pos);
	}
	if (quote_dot_p(pos)) {
		getprint_quote(pos, &pos);
		Return(print_ascii_stream_(stream, ",."));
		return write_print_call_(ptr, stream, pos);
	}
	return print_unreadable_object_(ptr, stream, pos, 1, 1, NULL);
}


/*
 *  restart
 */
static int WriteBody_restart_(Execute ptr, addr stream, addr pos)
{
	getname_restart(pos, &pos);
	return write_print_call_(ptr, stream, pos);
}

static int WriteCall_restart_(Execute ptr, addr stream, addr pos)
{
	addr restart;

	/* #<RESTART NAME #xADDRESS> */
	getreport_restart(pos, &restart);
	if (restart == Nil || escape_print(ptr))
		return print_unreadable_object_(ptr, stream, pos, 1, 1, WriteBody_restart_);
	else if (stringp(restart))
		return WriteCall_string_(ptr, stream, restart);
	else
		return callclang_funcall(ptr, &restart, restart, stream, NULL);
}


/*
 *  bitvector
 */
static int WriteCall_bitvector_(Execute ptr, addr stream, addr pos)
{
	int value;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(print_ascii_stream_(stream, "#*"));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &value));
		Return(write_char_stream_(stream, value? '1': '0'));
	}

	return 0;
}


/*
 *  byte
 */
static int WriteBody_bytespec_(Execute ptr, addr stream, addr pos)
{
	char data[256];
	struct bytespec_struct *str;

	str = ByteSpecStruct(pos);
	snprintf(data, 256, "SIZE:%zu POSITION:%zu", str->size, str->position);
	return print_ascii_stream_(stream, data);
}

static int WriteCall_bytespec_(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object_(ptr, stream, pos, 1, 0, WriteBody_bytespec_);
}


/*
 *  table
 */
_g int write_check_call_(Execute ptr, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_CONS:
			return WriteCheckCall_cons_(ptr, pos);

		case LISPTYPE_VECTOR:
			return WriteCheckCall_vector_(ptr, pos);

		case LISPTYPE_ARRAY:
			return WriteCheckCall_array_(ptr, pos);

		default:
			return 0;
	}
}

static int write_circle_call_(Execute ptr, addr stream, addr pos)
{
	int index;
	calltype_print call;

	index = (int)GetType(pos);
	call = WriteCircleTable[index];
	if (call)
		return (*call)(ptr, stream, pos);
	else
		return (WriteCallTable[index])(ptr, stream, pos);
}

static int write_print_call_(Execute ptr, addr stream, addr pos)
{
	int index = (int)GetType(pos);
	return (WriteCallTable[index])(ptr, stream, pos);
}


/*
 *  write print
 */
_g int write_default_print_(Execute ptr, addr stream, addr pos)
{
	/* normal */
	if (! circle_print(ptr))
		return write_print_call_(ptr, stream, pos);
	/* circle */
	if (discard_pretty_stream(stream))
		return 0;
	if (! push_pretty_stream_p(stream)) {
		push_write_object(ptr);
		Return(write_check_call_(ptr, pos));
	}
	return write_circle_call_(ptr, stream, pos);
}

static int write_pretty_print_(Execute ptr, addr stream, addr pos)
{
	addr dispatch;

	pprint_dispatch_print(ptr, &dispatch);
	Return(find_function_print_dispatch(ptr, pos, dispatch, &dispatch));
	if (dispatch == Nil)
		return write_default_print_(ptr, stream, pos);
	else
		return callclang_funcall(ptr, &dispatch, dispatch, stream, pos, NULL);
}

_g int write_print(Execute ptr, addr stream, addr pos)
{
	gchold_push_local(ptr->local, stream);
	if (pretty_print(ptr))
		return write_pretty_print_(ptr, stream, pos);
	else
		return write_default_print_(ptr, stream, pos);
}

_g int princ_print(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_new_control(ptr, &control);
	push_escape_print(ptr, 0);
	push_readably_print(ptr, 0);
	Return(write_print(ptr, stream, pos));

	return free_control_(ptr, control);
}

_g int prin1_print(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_new_control(ptr, &control);
	push_escape_print(ptr, 1);
	Return(write_print(ptr, stream, pos));

	return free_control_(ptr, control);
}

_g int print_print(Execute ptr, addr stream, addr pos)
{
	Return(terpri_stream_(stream));
	Return(prin1_print(ptr, stream, pos));
	return write_char_stream_(stream, ' ');
}

_g int pprint_print(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_new_control(ptr, &control);
	push_escape_print(ptr, 1);
	push_pretty_print(ptr, 1);
	Return(terpri_stream_(stream));
	Return(write_print(ptr, stream, pos));

	return free_control_(ptr, control);
}

_g int write_string_heap(Execute ptr, addr *ret, addr pos)
{
	addr control, stream;

	push_new_control(ptr, &control);
	open_output_string_stream(&stream, 0);
	Return(write_print(ptr, stream, pos));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return free_control_(ptr, control);
}

_g int write_string_local(Execute ptr, addr *ret, addr pos)
{
	addr control, stream;

	push_new_control(ptr, &control);
	open_output_string_stream(&stream, 0);
	Return(write_print(ptr, stream, pos));
	Return(string_stream_local_(ptr->local, stream, ret));
	close_output_string_stream(stream);

	return free_control_(ptr, control);
}

_g int princ_string_heap(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(princ_print(ptr, stream, pos));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}

_g int princ_string_local(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(princ_print(ptr, stream, pos));
	Return(string_stream_local_(ptr->local, stream, ret));
	close_output_string_stream(stream);

	return 0;
}

_g int prin1_string_heap(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(prin1_print(ptr, stream, pos));
	Return(string_stream_heap_(stream, ret));
	close_output_string_stream(stream);

	return 0;
}

_g int prin1_string_local(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	Return(prin1_print(ptr, stream, pos));
	Return(string_stream_local_(ptr->local, stream, ret));
	close_output_string_stream(stream);

	return 0;
}


/*
 *  initialize
 */
_g void init_print_write(void)
{
	int i;

	/* error */
	for (i = 0; i < LISPTYPE_SIZE; i++) {
		WriteCallTable[i] = WriteCall_error;
		WriteCircleTable[i] = NULL;
	}

	/* cons */
	WriteCircleTable[LISPTYPE_CONS] = WriteCircleCall_cons_;
	WriteCallTable[LISPTYPE_CONS] = WriteCall_cons_;
	/* vector */
	WriteCircleTable[LISPTYPE_VECTOR] = WriteCircleCall_vector_;
	WriteCallTable[LISPTYPE_VECTOR] = WriteCall_vector_;
	/* array */
	WriteCircleTable[LISPTYPE_ARRAY] = WriteCircleCall_array_;
	WriteCallTable[LISPTYPE_ARRAY] = WriteCall_array_;
	/* object */
	WriteCallTable[LISPTYPE_NIL] = WriteCall_symbol_;
	WriteCallTable[LISPTYPE_T] = WriteCall_symbol_;
	WriteCallTable[LISPTYPE_TYPE] = WriteCall_type_;
	WriteCallTable[LISPTYPE_CLOS] = WriteCall_clos_;
	WriteCallTable[LISPTYPE_CHARACTER] = WriteCall_character_;
	WriteCallTable[LISPTYPE_STRING] = WriteCall_string_;
	WriteCallTable[LISPTYPE_HASHTABLE] = WriteCall_hashtable_;
	WriteCallTable[LISPTYPE_READTABLE] = WriteCall_system;
	WriteCallTable[LISPTYPE_SYMBOL] = WriteCall_symbol_;
	WriteCallTable[LISPTYPE_FIXNUM] = WriteCall_fixnum_;
	WriteCallTable[LISPTYPE_BIGNUM] = WriteCall_bignum_;
	WriteCallTable[LISPTYPE_RATIO] = WriteCall_ratio_;
	WriteCallTable[LISPTYPE_SHORT_FLOAT] = WriteCall_error;
	WriteCallTable[LISPTYPE_SINGLE_FLOAT] = WriteCall_single_float_;
	WriteCallTable[LISPTYPE_DOUBLE_FLOAT] = WriteCall_double_float_;
	WriteCallTable[LISPTYPE_LONG_FLOAT] = WriteCall_long_float_;
	WriteCallTable[LISPTYPE_COMPLEX] = WriteCall_complex_;
	WriteCallTable[LISPTYPE_CONTROL] = WriteCall_system;
	WriteCallTable[LISPTYPE_CODE] = WriteCall_system;
	WriteCallTable[LISPTYPE_CALLNAME] = WriteCall_callname_;
	WriteCallTable[LISPTYPE_FUNCTION] = WriteCall_function_;
	WriteCallTable[LISPTYPE_INDEX] = WriteCall_index_;
	WriteCallTable[LISPTYPE_PACKAGE] = WriteCall_package_;
	WriteCallTable[LISPTYPE_RANDOM_STATE] = WriteCall_random_state_;
	WriteCallTable[LISPTYPE_PATHNAME] = WriteCall_pathname_;
	WriteCallTable[LISPTYPE_STREAM] = WriteCall_stream_;
	WriteCallTable[LISPTYPE_QUOTE] = WriteCall_quote_;
	WriteCallTable[LISPTYPE_RESTART] = WriteCall_restart_;
	WriteCallTable[LISPTYPE_EVAL] = WriteCall_system;
	WriteCallTable[LISPTYPE_ENVIRONMENT] = WriteCall_system;
	WriteCallTable[LISPTYPE_BITVECTOR] = WriteCall_bitvector_;
	WriteCallTable[LISPTYPE_PRINT_DISPATCH] = WriteCall_system;
	WriteCallTable[LISPTYPE_BYTESPEC] = WriteCall_bytespec_;

	WriteCallTable[LISPSYSTEM_CHARACTER2] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_CHARQUEUE] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_CHARBIT] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_SYMSTACK] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_BITTYPE] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_READLABEL] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_READINFO] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_READTYPE] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_BITCONS] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_BITBUFFER] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_HASHITERATOR] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_PACKAGEITERATOR] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_TAGINFO] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_ARRAY_DIMENSION] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_ARRAY_GENERAL] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_ARRAY_SPECIALIZED] = WriteCall_system;
}

