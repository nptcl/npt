#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "bignum.h"
#include "bit.h"
#include "bytespec.h"
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
#include "gc.h"
#include "integer.h"
#include "package.h"
#include "pathname.h"
#include "print.h"
#include "print_dispatch.h"
#include "print_write.h"
#include "quote.h"
#include "random_state.h"
#include "ratio.h"
#include "readtable.h"
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

static int write_circle_call(Execute ptr, addr stream, addr pos);
static int write_print_call(Execute ptr, addr stream, addr pos);

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

static int intern_print_write(Execute ptr, addr pos)
{
	addr write, cons;

	print_write_object(ptr, &write);
	get_table_print_write(write, &cons);
	if (intern_hashheap(cons, pos, &cons) == 0) {
		/* make */
		print_check_heap(&pos, pos);
		SetCdr(cons, pos);
		return 1;
	}
	else {
		/* found */
		GetCdr(cons, &pos);
		increment_print_write(write, pos);
		return 0;
	}
}

static int find_print_write(Execute ptr, addr key, addr *ret)
{
	addr pos;

	print_write_object(ptr, &pos);
	get_table_print_write(pos, &pos);
	findvalue_hashtable(pos, key, ret);
	CheckType(*ret, LISPSYSTEM_PRINT_CHECK);

	return ptr_print_check(*ret)->index == 0;
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
static int WriteBody_error(Execute ptr, addr stream, addr pos)
{
	print_ascii_stream(stream, "INVALID-OBJECT");
	if (type_name_p(&pos, pos))
		return 0;
	write_char_stream(stream, ' ');
	return write_print_call(ptr, stream, pos);
}

static int WriteCall_error(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object(ptr, stream, pos, 0, 1, WriteBody_error);
}

static int WriteCall_system(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object(ptr, stream, pos, 1, 1, NULL);
}


/*
 *  cons
 */
static void WriteCheckCall_cons(Execute ptr, addr pos)
{
	int lenp, levelp;
	addr x;
	size_t len, level, depth, i;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return;

	/* intern */
	if (intern_print_write(ptr, pos) == 0)
		return;

	/* list */
	CheckType(pos, LISPTYPE_CONS);
	setdepth_print_write(ptr, depth + 1);
	for (i = 0; ; i++) {
		/* *print-length* */
		if (lenp && len <= i)
			break;
		/* cons */
		GetCons(pos, &x, &pos);
		write_check_call(ptr, x);
		if (! consp(pos))
			break;
		if (intern_print_write(ptr, pos) == 0)
			break;
	}
	setdepth_print_write(ptr, depth);
}

_g int pprint_pop_circle(Execute ptr, addr stream, addr pos)
{
	addr x;
	size_t index;

	if (find_print_write(ptr, pos, &x))
		return 0;
	/* found */
	if (get_first_print_check(x) == 0)
		fmte("Invalid loop object.", NULL);

	print_ascii_stream(stream, ". #");
	index = get_index_print_check(x);
	output_nosign_fixnum(stream, index, 10, 1);
	/* #3# */
	write_char_stream(stream, '#');
	return 1;
}

static int WriteCircle_find(Execute ptr, addr stream, addr pos)
{
	addr x;
	size_t index;

	if (find_print_write(ptr, pos, &x))
		return 0;
	/* found */
	write_char_stream(stream, '#');
	index = get_index_print_check(x);
	output_nosign_fixnum(stream, index, 10, 1);
	/* first, second */
	if (get_first_print_check(x) == 0) {
		/* #3= (...) */
		write_char_stream(stream, '=');
		set_first_print_check(x);
		return 0;
	}
	else {
		/* #3# */
		write_char_stream(stream, '#');
		return 1;
	}
}

_g int pprint_check_circle(Execute ptr, addr pos, addr *ret)
{
	addr x, stream;
	size_t index;

	CheckType(pos, LISPTYPE_CONS);
	if (find_print_write(ptr, pos, &x)) {
		*ret = Nil;
		return 0;
	}

	/* found */
	open_output_string_stream(&stream, 0);
	write_char_stream(stream, '#');
	index = get_index_print_check(x);
	output_nosign_fixnum(stream, index, 10, 1);
	/* first, second */
	if (get_first_print_check(x) == 0) {
		/* #3= (...) */
		write_char_stream(stream, '=');
		set_first_print_check(x);
		string_stream_heap(stream, ret);
		return 0;
	}
	else {
		/* #3# */
		write_char_stream(stream, '#');
		string_stream_heap(stream, ret);
		return 1;
	}
}

static int WriteCircleCall_cons(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp;
	addr x;
	size_t i, len, level, depth;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth) {
		write_char_stream(stream, '#');
		return 0;
	}

	/* table */
	if (WriteCircle_find(ptr, stream, pos))
		return 0;

	/* list */
	CheckType(pos, LISPTYPE_CONS);
	setdepth_print_write(ptr, depth + 1);
	write_char_stream(stream, '(');
	for (i = 0; ; i++) {
		/* *print-length* */
		if (lenp && len <= i) {
			print_ascii_stream(stream, "...");
			break;
		}
		/* cons */
		GetCons(pos, &x, &pos);
		if (write_circle_call(ptr, stream, x))
			return 1;
		if (pos == Nil)
			break;
		if (consp(pos) && find_print_write(ptr, pos, &x)) {
			write_char_stream(stream, ' ');
		}
		else {
			print_ascii_stream(stream, " . ");
			if (write_circle_call(ptr, stream, pos))
				return 1;
			break;
		}
	}
	write_char_stream(stream, ')');
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCall_cons(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp;
	addr x;
	size_t len, level, depth, i;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth) {
		write_char_stream(stream, '#');
		return 0;
	}

	/* list */
	CheckType(pos, LISPTYPE_CONS);
	setdepth_print_write(ptr, depth + 1);
	write_char_stream(stream, '(');
	for (i = 0; ; i++) {
		/* *print-length* */
		if (lenp && len <= i) {
			print_ascii_stream(stream, "...");
			break;
		}
		/* cons */
		GetCons(pos, &x, &pos);
		if (write_print_call(ptr, stream, x))
			return 1;
		if (pos == Nil)
			break;
		if (consp(pos)) {
			write_char_stream(stream, ' ');
		}
		else {
			print_ascii_stream(stream, " . ");
			if (write_print_call(ptr, stream, pos))
				return 1;
			break;
		}
	}
	write_char_stream(stream, ')');
	setdepth_print_write(ptr, depth);

	return 0;
}


/*
 *  vector
 */
static void WriteCheckCall_vector(Execute ptr, addr pos)
{
	int lenp, levelp;
	addr x;
	size_t len, level, depth, size, i;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth)
		return;

	/* intern */
	if (intern_print_write(ptr, pos) == 0)
		return;

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
		write_check_call(ptr, x);
	}
	setdepth_print_write(ptr, depth);
}

static int WriteCircleCall_vector(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp;
	addr x;
	size_t len, level, depth, size, i;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth) {
		write_char_stream(stream, '#');
		return 0;
	}

	/* table */
	if (WriteCircle_find(ptr, stream, pos))
		return 0;

	/* list */
	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	setdepth_print_write(ptr, depth + 1);
	print_ascii_stream(stream, "#(");
	for (i = 0; i < size; i++) {
		if (i != 0)
			write_char_stream(stream, ' ');
		/* *print-length* */
		if (lenp && len <= i) {
			print_ascii_stream(stream, "...");
			break;
		}
		/* vector */
		getarray(pos, i, &x);
		if (write_circle_call(ptr, stream, x))
			return 1;
	}
	write_char_stream(stream, ')');
	setdepth_print_write(ptr, depth);

	return 0;
}

static int WriteCall_vector(Execute ptr, addr stream, addr pos)
{
	int lenp, levelp;
	addr x;
	size_t len, level, depth, size, i;

	lenp = length_print(ptr, &len);
	levelp = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);

	/* *print-level* */
	if (levelp && level <= depth) {
		write_char_stream(stream, '#');
		return 0;
	}

	/* list */
	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	setdepth_print_write(ptr, depth + 1);
	print_ascii_stream(stream, "#(");
	for (i = 0; i < size; i++) {
		if (i != 0)
			write_char_stream(stream, ' ');
		/* *print-length* */
		if (lenp && len <= i) {
			print_ascii_stream(stream, "...");
			break;
		}
		/* vector */
		getarray(pos, i, &x);
		if (write_print_call(ptr, stream, x))
			return 1;
	}
	write_char_stream(stream, ')');
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

static void WriteCheckCall_array_print(struct write_array_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = str->ptr->local;
	push_local(local, &stack);
	array_get(local, str->pos, str->index++, &pos);
	write_check_call(str->ptr, pos);
	rollback_local(local, stack);
}

static void WriteCheckCall_array_call(struct write_array_struct *str)
{
	int lenp;
	const size_t *data;
	size_t len, i, loop, depth;

	/* output */
	depth = str->depth;
	if (str->dimension <= str->depth) {
		WriteCheckCall_array_print(str);
		return;
	}

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
		WriteCheckCall_array_call(str);
	}
	str->depth--;
}

static void WriteCheckCall_array(Execute ptr, addr pos)
{
	int check;
	const size_t *data;
	size_t dimension, level, depth;
	struct write_array_struct str;

	/* *print-level* */
	check = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);
	if (check && level <= depth)
		return;

	/* intern */
	if (intern_print_write(ptr, pos) == 0)
		return;

	/* specialized */
	if (WriteArray_specialized_p(pos))
		return;

	/* prefix */
	dimension = ArrayInfoStruct(pos)->dimension;
	data = array_ptrsize(pos);

	/* body */
	setdepth_print_write(ptr, depth + 1);
	make_write_array(&str, ptr, Nil, pos, data, dimension);
	WriteCheckCall_array_call(&str);
	setdepth_print_write(ptr, depth);
}

static int WriteArray_bit(Execute ptr, addr stream, addr pos)
{
	int value;
	size_t size, i;

	array_get_rowlength(pos, &size);
	print_ascii_stream(stream, "#*");
	for (i = 0; i < size; i++) {
		(void)array_get_bit(pos, i, &value);
		write_char_stream(stream, value? '1': '0');
	}

	return 0;
}

static int WriteCall_string(Execute ptr, addr stream, addr object);
static int WriteArray_specialized(Execute ptr, addr stream, addr pos)
{
	switch (ArrayInfoStruct(pos)->type) {
		case ARRAY_TYPE_BIT:
			return WriteArray_bit(ptr, stream, pos);

		case ARRAY_TYPE_CHARACTER:
			return WriteCall_string(ptr, stream, pos);

		default:
			fmte("Invalid array type.", NULL);
			break;
	}

	return 0;
}

static int WriteCircleCall_array_print(struct write_array_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = str->ptr->local;
	push_local(local, &stack);
	array_get(local, str->pos, str->index++, &pos);
	if (write_circle_call(str->ptr, str->stream, pos))
		return 1;
	rollback_local(local, stack);

	return 0;
}

static int WriteCall_array_print(struct write_array_struct *str)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	local = str->ptr->local;
	push_local(local, &stack);
	array_get(local, str->pos, str->index++, &pos);
	if (write_print_call(str->ptr, str->stream, pos))
		return 1;
	rollback_local(local, stack);

	return 0;
}

static int WriteCircleCall_array_call(struct write_array_struct *str)
{
	int lenp;
	addr stream;
	const size_t *data;
	size_t len, i, loop, depth;

	/* output */
	depth = str->depth;
	if (str->dimension <= depth)
		return WriteCircleCall_array_print(str);

	/* restrict */
	stream = str->stream;
	data = str->data;
	loop = data[depth];
	lenp = length_print(str->ptr, &len);

	write_char_stream(stream, '(');
	str->depth++;
	for (i = 0; i < loop; i++) {
		if (i != 0)
			write_char_stream(stream, ' ');
		/* *print-length* */
		if (lenp && len <= i) {
			print_ascii_stream(stream, "...");
			str->index += loop - i;
			break;
		}
		/* array */
		if (WriteCircleCall_array_call(str))
			return 1;
	}
	str->depth--;
	write_char_stream(stream, ')');

	return 0;
}

static int WriteCall_array_call(struct write_array_struct *str)
{
	int lenp;
	addr stream;
	const size_t *data;
	size_t len, i, loop, depth;

	/* output */
	depth = str->depth;
	if (str->dimension <= str->depth)
		return WriteCall_array_print(str);

	/* restrict */
	stream = str->stream;
	data = str->data;
	loop = data[depth];
	lenp = length_print(str->ptr, &len);

	write_char_stream(stream, '(');
	str->depth++;
	for (i = 0; i < loop; i++) {
		if (i != 0)
			write_char_stream(stream, ' ');
		/* *print-length* */
		if (lenp && len <= i) {
			print_ascii_stream(stream, "...");
			str->index += loop - i;
			break;
		}
		/* array */
		if (WriteCall_array_call(str))
			return 1;
	}
	str->depth--;
	write_char_stream(stream, ')');

	return 0;
}

static int WriteCircleCall_array(Execute ptr, addr stream, addr pos)
{
	int check;
	const size_t *data;
	size_t dimension, level, depth;
	struct write_array_struct str;

	/* *print-level* */
	check = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);
	if (check && level <= depth) {
		write_char_stream(stream, '#');
		return 0;
	}

	/* table */
	if (WriteCircle_find(ptr, stream, pos))
		return 0;

	/* specialized */
	if (WriteArray_specialized_p(pos))
		return WriteArray_specialized(ptr, stream, pos);

	/* prefix */
	write_char_stream(stream, '#');
	dimension = ArrayInfoStruct(pos)->dimension;
	data = array_ptrsize(pos);
	if (dimension != 1) {
		output_nosign_index(stream, dimension, 10, 1);
		write_char_stream(stream, 'A');
	}

	/* body */
	setdepth_print_write(ptr, depth + 1);
	make_write_array(&str, ptr, stream, pos, data, dimension);
	check = WriteCircleCall_array_call(&str);
	setdepth_print_write(ptr, depth);

	return check;
}

static int WriteCall_array(Execute ptr, addr stream, addr pos)
{
	int check;
	const size_t *data;
	size_t dimension, level, depth;
	struct write_array_struct str;

	/* *print-level* */
	check = level_print(ptr, &level);
	getdepth_print_write(ptr, &depth);
	if (check && level <= depth) {
		write_char_stream(stream, '#');
		return 0;
	}

	/* specialized */
	if (WriteArray_specialized_p(pos))
		return WriteArray_specialized(ptr, stream, pos);

	/* prefix */
	write_char_stream(stream, '#');
	dimension = ArrayInfoStruct(pos)->dimension;
	data = array_ptrsize(pos);
	if (dimension != 1) {
		output_nosign_index(stream, dimension, 10, 1);
		write_char_stream(stream, 'A');
	}

	/* body */
	setdepth_print_write(ptr, depth + 1);
	make_write_array(&str, ptr, stream, pos, data, dimension);
	check = WriteCall_array_call(&str);
	setdepth_print_write(ptr, depth);

	return check;
}


/*
 *  symbol
 */
static void WriteSymbol_direct_norm(addr stream, addr pos)
{
	size_t i, size;
	unicode u;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		write_char_stream(stream, u);
	}
}

static void WriteSymbol_downcase_norm(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		write_char_stream(stream, toLowerUnicode(u));
	}
}

static void WriteSymbol_upcase_norm(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		write_char_stream(stream, toUpperUnicode(u));
	}
}

static void WriteSymbol_up_cap_norm(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (isAlphanumeric(u)) {
			if (check) {
				write_char_stream(stream, u);
				check = 0;
			}
			else {
				write_char_stream(stream, toLowerUnicode(u));
			}
		}
		else {
			write_char_stream(stream, u);
			check = 1;
		}
	}
}

static void WriteSymbol_down_cap_norm(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (isAlphanumeric(u)) {
			if (check) {
				write_char_stream(stream, toUpperUnicode(u));
				check = 0;
			}
			else {
				write_char_stream(stream, u);
			}
		}
		else {
			write_char_stream(stream, u);
			check = 1;
		}
	}
}

static enum PrintCase WriteSymbol_check_invert(addr pos)
{
	enum PrintCase check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	check = PrintCase_unread;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (isUpperCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_upcase;
			}
			else if (check != PrintCase_upcase) {
				return PrintCase_preserve;
			}
		}
		else if (isLowerCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_downcase;
			}
			else if (check != PrintCase_downcase) {
				return PrintCase_preserve;
			}
		}
	}

	return check;
}

static void WriteSymbol_invert_norm(addr stream, addr pos)
{
	switch (WriteSymbol_check_invert(pos)) {
		case PrintCase_upcase:
			WriteSymbol_downcase_norm(stream, pos);
			break;

		case PrintCase_downcase:
			WriteSymbol_upcase_norm(stream, pos);
			break;

		default:
			WriteSymbol_direct_norm(stream, pos);
			break;
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

static void WriteSymbol_direct_escape(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (u == '\\' || u == '|')
			write_char_stream(stream, '\\');
		write_char_stream(stream, u);
	}
}

static int WriteSymbol_check_upcase_escape(addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (WriteSymbol_check_escape(u))
			return 1;
		if (isLowerCase(u))
			return 1;
	}

	return 0;
}

static void WriteSymbol_up_up_output(addr stream, addr pos)
{
	if (WriteSymbol_check_upcase_escape(pos)) {
		write_char_stream(stream, '|');
		WriteSymbol_direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		WriteSymbol_direct_escape(stream, pos);
	}
}

static void WriteSymbol_downcase_escape(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		write_char_stream(stream, toLowerUnicode(u));
	}
}

static void WriteSymbol_up_down_output(addr stream, addr pos)
{
	if (WriteSymbol_check_upcase_escape(pos)) {
		write_char_stream(stream, '|');
		WriteSymbol_direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		WriteSymbol_downcase_escape(stream, pos);
	}
}

static void WriteSymbol_capitalize_escape(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (isAlphanumeric(u)) {
			if (check) {
				write_char_stream(stream, toUpperUnicode(u));
				check = 0;
			}
			else {
				write_char_stream(stream, toLowerUnicode(u));
			}
		}
		else {
			write_char_stream(stream, u);
			check = 1;
		}
	}
}

static void WriteSymbol_up_cap_output(addr stream, addr pos)
{
	if (WriteSymbol_check_upcase_escape(pos)) {
		write_char_stream(stream, '|');
		WriteSymbol_direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		WriteSymbol_capitalize_escape(stream, pos);
	}
}

static void WriteSymbol_escape(Execute ptr,
		addr stream, addr pos, void (*call)(addr, addr))
{
	int exportp;
	addr package, check;

	getpackage(ptr, &check);
	GetPackageSymbol(pos, &package);
	if (package == Nil) {
		/* gensym */
		if (gensym_print(ptr))
			print_ascii_stream(stream, "#:");
	}
	else if (checksymbol_package(pos, check)) {
		/* no package name */
	}
	else if (keywordp(pos)) {
		print_ascii_stream(stream, ":");
	}
	else if (package != check && externalp_package(pos, check)) {
		/* package name */
		exportp = exportp_package(pos, package);
		getname_package(package, &package);
		call(stream, package);
		print_ascii_stream(stream, exportp? ":": "::");
	}
	/* symbol name */
	GetNameSymbol(pos, &pos);
	call(stream, pos);
}

static int WriteSymbol_check_downcase_escape(addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (WriteSymbol_check_escape(u))
			return 1;
		if (isUpperCase(u))
			return 1;
	}

	return 0;
}

static void WriteSymbol_upcase_escape(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		write_char_stream(stream, toUpperUnicode(u));
	}
}

static void WriteSymbol_down_up_output(addr stream, addr pos)
{
	if (WriteSymbol_check_downcase_escape(pos)) {
		write_char_stream(stream, '|');
		WriteSymbol_direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		WriteSymbol_upcase_escape(stream, pos);
	}
}

static void WriteSymbol_down_down_output(addr stream, addr pos)
{
	if (WriteSymbol_check_downcase_escape(pos)) {
		write_char_stream(stream, '|');
		WriteSymbol_direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		WriteSymbol_direct_escape(stream, pos);
	}
}

static void WriteSymbol_down_cap_output(addr stream, addr pos)
{
	if (WriteSymbol_check_downcase_escape(pos)) {
		write_char_stream(stream, '|');
		WriteSymbol_direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		WriteSymbol_capitalize_escape(stream, pos);
	}
}

static int WriteSymbol_check_preserve_escape(addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (WriteSymbol_check_escape(u))
			return 1;
	}

	return 0;
}

static void WriteSymbol_preserve_output(addr stream, addr pos)
{
	if (WriteSymbol_check_preserve_escape(pos)) {
		write_char_stream(stream, '|');
		WriteSymbol_direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		WriteSymbol_direct_escape(stream, pos);
	}
}

static enum PrintCase WriteSymbol_check_invert_escape(addr pos)
{
	enum PrintCase check;
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	check = PrintCase_unread;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (WriteSymbol_check_escape(u)) {
			return PrintCase_escape;
		}
		else if (isUpperCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_upcase;
			}
			else if (check != PrintCase_upcase) {
				return PrintCase_preserve;
			}
		}
		else if (isLowerCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_downcase;
			}
			else if (check != PrintCase_downcase) {
				return PrintCase_preserve;
			}
		}
	}

	return check;
}

static void WriteSymbol_invert_output(addr stream, addr pos)
{
	switch (WriteSymbol_check_invert_escape(pos)) {
		case PrintCase_upcase:
			WriteSymbol_downcase_escape(stream, pos);
			break;

		case PrintCase_downcase:
			WriteSymbol_upcase_escape(stream, pos);
			break;

		case PrintCase_escape:
			write_char_stream(stream, '|');
			WriteSymbol_direct_escape(stream, pos);
			write_char_stream(stream, '|');
			break;

		default:
			WriteSymbol_direct_escape(stream, pos);
			break;
	}
}

static void WriteSymbol_upcase_upcase(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		WriteSymbol_escape(ptr, stream, pos, WriteSymbol_up_up_output);
	else
		WriteSymbol_direct_norm(stream, pos);
}

static void WriteSymbol_upcase_downcase(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		WriteSymbol_escape(ptr, stream, pos, WriteSymbol_up_down_output);
	else
		WriteSymbol_downcase_norm(stream, pos);
}

static void WriteSymbol_upcase_capitalize(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		WriteSymbol_escape(ptr, stream, pos, WriteSymbol_up_cap_output);
	else
		WriteSymbol_up_cap_norm(stream, pos);
}

static void WriteSymbol_upcase(Execute ptr, addr stream, addr pos)
{
	switch (case_print(ptr)) {
		case PrintCase_upcase:
			WriteSymbol_upcase_upcase(ptr, stream, pos);
			break;

		case PrintCase_downcase:
			WriteSymbol_upcase_downcase(ptr, stream, pos);
			break;

		case PrintCase_capitalize:
			WriteSymbol_upcase_capitalize(ptr, stream, pos);
			break;

		default:
			fmte("printcase error", NULL);
			break;
	}
}

static void WriteSymbol_downcase_upcase(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		WriteSymbol_escape(ptr, stream, pos, WriteSymbol_down_up_output);
	else
		WriteSymbol_upcase_norm(stream, pos);
}

static void WriteSymbol_downcase_downcase(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		WriteSymbol_escape(ptr, stream, pos, WriteSymbol_down_down_output);
	else
		WriteSymbol_direct_norm(stream, pos);
}

static void WriteSymbol_downcase_capitalize(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		WriteSymbol_escape(ptr, stream, pos, WriteSymbol_down_cap_output);
	else
		WriteSymbol_down_cap_norm(stream, pos);
}

static void WriteSymbol_downcase(Execute ptr, addr stream, addr pos)
{
	switch (case_print(ptr)) {
		case PrintCase_upcase:
			WriteSymbol_downcase_upcase(ptr, stream, pos);
			break;

		case PrintCase_downcase:
			WriteSymbol_downcase_downcase(ptr, stream, pos);
			break;

		case PrintCase_capitalize:
			WriteSymbol_downcase_capitalize(ptr, stream, pos);
			break;

		default:
			fmte("printcase error", NULL);
			break;
	}
}

static void WriteSymbol_preserve(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		WriteSymbol_escape(ptr, stream, pos, WriteSymbol_preserve_output);
	else
		WriteSymbol_direct_norm(stream, pos);
}

static void WriteSymbol_invert(Execute ptr, addr stream, addr pos)
{
	if (escape_print(ptr))
		WriteSymbol_escape(ptr, stream, pos, WriteSymbol_invert_output);
	else
		WriteSymbol_invert_norm(stream, pos);
}

static int WriteCall_symbol(Execute ptr, addr stream, addr pos)
{
	switch (readcase_readtable(ptr)) {
		case ReadTable_upcase:
			WriteSymbol_upcase(ptr, stream, pos);
			break;

		case ReadTable_downcase:
			WriteSymbol_downcase(ptr, stream, pos);
			break;

		case ReadTable_preserve:
			WriteSymbol_preserve(ptr, stream, pos);
			break;

		case ReadTable_invert:
			WriteSymbol_invert(ptr, stream, pos);
			break;

		default:
			fmte("*readtable* case error", NULL);
			break;
	}

	return 0;
}


/*
 *  type
 */
static int WriteCall_type(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	type_object(&pos, pos);
	print_ascii_stream(stream, "#<TYPE ");
	Return(prin1_print(ptr, stream, pos));
	print_ascii_stream(stream, ">");

	return 0;
}


/*
 *  clos, structure
 */
static int WriteCall_clos(Execute ptr, addr stream, addr pos)
{
	addr generic;

	Check(! closp(pos), "type error");
	GetConst(COMMON_PRINT_OBJECT, &generic);
	getfunctioncheck_local(ptr, generic, &generic);

	return callclang_funcall(ptr, &pos, generic, pos, stream, NULL);
}


/*
 *  character
 */
static void WriteCall_fixnum_value(addr stream, fixnum value, unsigned base)
{
	/* zero */
	if (value == 0) {
		write_char_stream(stream, '0');
		return;
	}

	/* output */
	if (value < 0)
		write_char_stream(stream, '-');
	output_nosign_fixnum(stream, value, base, 1);
}

static void WriteCall_character_name(addr stream, unicode u)
{
	if (isStandardType(u)) {
		write_char_stream(stream, u);
	}
	else {
		write_char_stream(stream, 'u');
		WriteCall_fixnum_value(stream, (fixnum)u, 16);
	}
}

static void WriteCall_character_string(addr stream, addr string)
{
	unicode c;
	size_t i, size;

	string_length(string, &size);
	for (i = 0; i < size; i++) {
		string_getc(string, i, &c);
		write_char_stream(stream, c);
	}
}

static int WriteCall_character(Execute ptr, addr stream, addr object)
{
	addr pos;
	unicode u;

	if (! escape_print(ptr)) {
		GetCharacter(object, &u);
		write_char_stream(stream, u);
		return 0;
	}

	if (findtable_char_name(&pos, object)) {
		/* found */
		print_ascii_stream(stream, "#\\");
		WriteCall_character_string(stream, pos);
	}
	else {
		/* not found */
		print_ascii_stream(stream, "#\\");
		GetCharacter(object, &u);
		WriteCall_character_name(stream, u);
	}

	return 0;
}


/*
 *  string
 */
static int WriteCall_string(Execute ptr, addr stream, addr object)
{
	unicode c;
	size_t size, i;

	string_length(object, &size);
	if (escape_print(ptr)) {
		write_char_stream(stream, '\"');
		for (i = 0; i < size; i++) {
			string_getc(object, i, &c);
			if (c == '\"' || c == '\\')
				write_char_stream(stream, '\\');
			write_char_stream(stream, c);
		}
		write_char_stream(stream, '\"');
	}
	else {
		for (i = 0; i < size; i++) {
			string_getc(object, i, &c);
			write_char_stream(stream, c);
		}
	}

	return 0;
}


/*
 *  hash-table
 */
static int WriteBody_hashtable(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t count;

	/* #<HASH-TABLE :TEST EQL :COUNT 123 0x1234...> */
	gettest_symbol_hashtable(pos, &value);
	getcount_hashtable(pos, &count);
	/* output */
	print_ascii_stream(stream, ":TEST ");
	if (prin1_print(ptr, stream, value)) return 1;
	print_ascii_stream(stream, " :COUNT ");
	output_nosign_index(stream, count, 10, 0);

	return 0;
}

static int WriteCall_hashtable(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object(ptr, stream, pos, 1, 1, WriteBody_hashtable);
}


/*
 *  fixnum
 */
static void WriteCall_radix_front(addr stream, unsigned base)
{
	char buffer[8];

	Check(! isBaseChar(base), "base error");
	switch (base) {
		case 2:
			print_ascii_stream(stream, "#b");
			break;

		case 8:
			print_ascii_stream(stream, "#o");
			break;

		case 16:
			print_ascii_stream(stream, "#x");
			break;

		default:
			snprintf(buffer, 8, "#%ur", base);
			print_ascii_stream(stream, buffer);
			break;
	}
}

static int WriteCall_fixnum(Execute ptr, addr stream, addr object)
{
	int radix;
	unsigned base;
	fixnum value;

	base = base_print(ptr);
	radix = radix_print(ptr);
	if (radix && base != 10)
		WriteCall_radix_front(stream, base);
	GetFixnum(object, &value);
	WriteCall_fixnum_value(stream, value, base);
	if (radix && base == 10)
		write_char_stream(stream, '.');

	return 0;
}


/*
 *  bignum
 */
static void WriteCall_bignum_value(addr stream, int sign, addr object, unsigned base)
{
	/* zero */
	if (zerop_bignum(object)) {
		write_char_stream(stream, '0');
		return;
	}

	/* output */
	if (sign)
		write_char_stream(stream, '-');
	output_nosign_bignum(Local_Thread, stream, object, base, 1);
}

static void WriteCall_bignum_sign(Execute ptr, addr stream, int sign, addr object)
{
	int radix;
	unsigned base;

	base = base_print(ptr);
	radix = radix_print(ptr);
	if (radix && base != 10)
		WriteCall_radix_front(stream, base);
	WriteCall_bignum_value(stream, sign, object, base);
	if (radix && base == 10)
		write_char_stream(stream, '.');
}

static int WriteCall_bignum(Execute ptr, addr stream, addr object)
{
	int sign;

	GetSignBignum(object, &sign);
	WriteCall_bignum_sign(ptr, stream, sign, object);

	return 0;
}


/*
 *  ratio
 */
static int WriteCall_ratio(Execute ptr, addr stream, addr object)
{
	int sign;
	addr check;
	unsigned base;

	/* zero */
	if (zerop_ratio(object)) {
		write_char_stream(stream, '0');
		return 0;
	}

	/* integer */
	GetDenomRatio(object, &check);
	if (equal_value_nosign_bignum(check, 1)) {
		GetSignRatio(object, &sign);
		GetNumerRatio(object, &check);
		WriteCall_bignum_sign(ptr, stream, sign, check);
		return 0;
	}

	/* ratio */
	base = base_print(ptr);
	if (radix_print(ptr))
		WriteCall_radix_front(stream, base);
	GetSignRatio(object, &sign);
	if (sign)
		write_char_stream(stream, '-');
	output_nosign_ratio(Local_Thread, stream, object, base, 1);

	return 0;
}


/*
 *  float
 */
static int WriteCall_single_float(Execute ptr, addr stream, addr object)
{
	int markerp, marker;
	single_float value;

	GetSingleFloat(object, &value);
	markerp = float_readtable(ptr) != ReadTable_single;
	marker = markerp? 'F': 'E';
	fmtfloat_princ_single_float(stream, value, markerp, marker);

	return 0;
}

static int WriteCall_double_float(Execute ptr, addr stream, addr object)
{
	int markerp, marker;
	double_float value;

	GetDoubleFloat(object, &value);
	markerp = float_readtable(ptr) != ReadTable_double;
	marker = markerp? 'D': 'E';
	fmtfloat_princ_double_float(stream, value, markerp, marker);

	return 0;
}

static int WriteCall_long_float(Execute ptr, addr stream, addr object)
{
	int markerp, marker;
	long_float value;

	GetLongFloat(object, &value);
	markerp = float_readtable(ptr) != ReadTable_long;
	marker = markerp? 'L': 'E';
	fmtfloat_princ_long_float(stream, value, markerp, marker);

	return 0;
}


/*
 *  complex
 */
static int WriteCall_complex(Execute ptr, addr stream, addr object)
{
	addr real, imag;

	GetRealComplex(object, &real);
	GetImagComplex(object, &imag);
	print_ascii_stream(stream, "#C(");
	if (write_print_call(ptr, stream, real))
		return 1;
	write_char_stream(stream, ' ');
	if (write_print_call(ptr, stream, imag))
		return 1;
	write_char_stream(stream, ')');

	return 0;
}


/*
 *  callname
 */
static int WriteBody_callname(Execute ptr, addr stream, addr pos)
{
	name_callname_heap(pos, &pos);
	print_ascii_stream(stream, "CALLNAME ");
	return write_print_call(ptr, stream, pos);
}

static int WriteCall_callname(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object(ptr, stream, pos, 0, 1, WriteBody_callname);
}


/*
 *  function
 */
static int WriteBody_function(Execute ptr, addr stream, addr pos)
{
	const char *name;
	struct function_struct *str;

	/* type */
	str = StructFunction(pos);
	if (str->system)
		name = "SYSTEM-FUNCTION ";
	else if (str->compiled)
		name = "COMPILED-FUNCTION ";
	else
		name = "FUNCTION ";
	print_ascii_stream(stream, name);

	/* name */
	GetNameFunction(pos, &pos);
	if (pos == Nil)
		print_ascii_stream(stream, "LAMBDA");
	else {
		if (RefCallNameType(pos) == CALLNAME_SYMBOL) {
			GetCallName(pos, &pos);
			if (write_print_call(ptr, stream, pos))
				return 1;
		}
		else {
			GetCallName(pos, &pos);
			print_ascii_stream(stream, "(SETF ");
			if (write_print_call(ptr, stream, pos))
				return 1;
			print_ascii_stream(stream, ")");
		}
	}

	return 0;
}

static int WriteCall_function(Execute ptr, addr stream, addr pos)
{
	int ident;
	addr name;

	/* #<FUNCTION NAME> */
	GetNameFunction(pos, &name);
	ident = (name == Nil);
	return print_unreadable_object(ptr, stream, pos, 0, ident, WriteBody_function);
}


/*
 *  index
 */
static int WriteBody_index(Execute ptr, addr stream, addr pos)
{
	LocalRoot local;
	LocalStack stack;
	size_t size;

	GetIndex(pos, &size);
	local = ptr->local;
	push_local(local, &stack);
	if (write_print_call(ptr, stream, intsizea(local, size)))
		return 1;
	rollback_local(local, stack);

	return 0;
}

static int WriteCall_index(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object(ptr, stream, pos, 1, 0, WriteBody_index);
}


/*
 *  package
 */
static int WriteBody_package(Execute ptr, addr stream, addr pos)
{
	getname_package(pos, &pos);
	print_string_stream(stream, pos);
	return 0;
}

static int WriteCall_package(Execute ptr, addr stream, addr pos)
{
	/* #<PACKAGE NAME> */
	return print_unreadable_object(ptr, stream, pos, 1, 0, WriteBody_package);
}


/*
 *  random-state
 */
static int WriteBody_random_state(Execute ptr, addr stream, addr pos)
{
	addr control;
	int WriteCall_bignum(Execute, addr, addr);

	push_close_control(ptr, &control);
	push_escape_print(ptr, 0);
	push_readably_print(ptr, 0);
	push_radix_print(ptr, 1);
	push_base_print(ptr, 16);
	push_case_print(ptr, PrintCase_upcase);
	make_bignum_random_state_local(ptr->local, pos, &pos);
	Return(WriteCall_bignum(ptr, stream, pos));

	return free_control_(ptr, control);
}

static int WriteCall_random_state(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object(ptr, stream, pos, 1, 0, WriteBody_random_state);
}


/*
 *  pathname
 */
static int WriteCall_pathname(Execute ptr, addr stream, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	name_pathname_local(ptr, pos, &pos);
	if (escape_print(ptr))
		print_ascii_stream(stream, "#P");
	if (WriteCall_string(ptr, stream, pos))
		return 1;
	rollback_local(local, stack);

	return 0;
}


/*
 *  stream
 */
static int WriteBody_stream(Execute ptr, addr stream, addr pos)
{
	struct StructStream *str;

	str = PtrStructStream(pos);
	switch (str->type) {
		case StreamType_BinaryInput:
			print_ascii_stream(stream, "FILE-INPUT BINARY");
			break;

		case StreamType_BinaryOutput:
			print_ascii_stream(stream, "FILE-OUTPUT BINARY");
			break;

		case StreamType_BinaryIO:
			print_ascii_stream(stream, "FILE-IO BINARY");
			break;

		case StreamType_CharacterInput:
			print_ascii_stream(stream, "FILE-INPUT CHARACTER");
			break;

		case StreamType_CharacterOutput:
			print_ascii_stream(stream, "FILE-OUTPUT CHARACTER");
			break;

		case StreamType_CharacterIO:
			print_ascii_stream(stream, "FILE-IO CHARACTER");
			break;

		case StreamType_BincharInput:
			print_ascii_stream(stream, "FILE-INPUT SYSTEM");
			break;

		case StreamType_BincharOutput:
			print_ascii_stream(stream, "FILE-OUTPUT SYSTEM");
			break;

		case StreamType_BincharIO:
			print_ascii_stream(stream, "FILE-IO SYSTEM");
			break;

		case StreamType_StringInput:
			print_ascii_stream(stream, "STREAM STRING-INPUT");
			break;

		case StreamType_StringOutput:
			print_ascii_stream(stream, "STREAM STRING-OUTPUT");
			break;

		case StreamType_Synonym:
			print_ascii_stream(stream, "SYNONYM-STREAM");
			break;

		case StreamType_BroadCast:
			print_ascii_stream(stream, "BROADCAST-STREAM");
			break;

		case StreamType_Concatenated:
			print_ascii_stream(stream, "CONCATENATED-STREAM");
			break;

		case StreamType_TwoWay:
			print_ascii_stream(stream, "TWO-WAY-STREAM");
			break;

		case StreamType_Echo:
			print_ascii_stream(stream, "ECHO-STREAM");
			break;

		case StreamType_Prompt:
			print_ascii_stream(stream, "PROMPT-STREAM");
			break;

		case StreamType_Pretty:
			print_ascii_stream(stream, "PRETTY-STREAM");
			break;

		default:
			print_ascii_stream(stream, "STREAM");
			break;
	}

	return 0;
}

static int WriteCall_stream(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object(ptr, stream, pos, 0, 1, WriteBody_stream);
}


/*
 *  quote
 */
static int WriteCall_quote(Execute ptr, addr stream, addr pos)
{
	if (quote_back_p(pos)) {
		getprint_quote(pos, &pos);
		write_char_stream(stream, '`');
		return write_print_call(ptr, stream, pos);
	}
	if (quote_comma_p(pos)) {
		getprint_quote(pos, &pos);
		write_char_stream(stream, ',');
		return write_print_call(ptr, stream, pos);
	}
	if (quote_atsign_p(pos)) {
		getprint_quote(pos, &pos);
		print_ascii_stream(stream, ",@");
		return write_print_call(ptr, stream, pos);
	}
	if (quote_dot_p(pos)) {
		getprint_quote(pos, &pos);
		print_ascii_stream(stream, ",.");
		return write_print_call(ptr, stream, pos);
	}
	return print_unreadable_object(ptr, stream, pos, 1, 1, NULL);
}


/*
 *  restart
 */
static int WriteBody_restart(Execute ptr, addr stream, addr pos)
{
	getname_restart(pos, &pos);
	return write_print_call(ptr, stream, pos);
}

static int WriteCall_restart(Execute ptr, addr stream, addr pos)
{
	addr restart;

	/* #<RESTART NAME #xADDRESS> */
	getreport_restart(pos, &restart);
	if (restart == Nil || escape_print(ptr))
		return print_unreadable_object(ptr, stream, pos, 1, 1, WriteBody_restart);
	else if (stringp(restart))
		return WriteCall_string(ptr, stream, restart);
	else
		return callclang_funcall(ptr, &restart, restart, stream, NULL);
}


/*
 *  bitvector
 */
static int WriteCall_bitvector(Execute ptr, addr stream, addr pos)
{
	int value;
	size_t size, i;

	bitmemory_length(pos, &size);
	print_ascii_stream(stream, "#*");
	for (i = 0; i < size; i++) {
		bitmemory_getint(pos, i, &value);
		write_char_stream(stream, value? '1': '0');
	}

	return 0;
}


/*
 *  byte
 */
static int WriteBody_bytespec(Execute ptr, addr stream, addr pos)
{
	char data[256];
	struct bytespec_struct *str;

	str = ByteSpecStruct(pos);
	snprintf(data, 256, "SIZE:%zu POSITION:%zu", str->size, str->position);
	print_ascii_stream(stream, data);

	return 0;
}

static int WriteCall_bytespec(Execute ptr, addr stream, addr pos)
{
	return print_unreadable_object(ptr, stream, pos, 1, 0, WriteBody_bytespec);
}


/*
 *  table
 */
_g void write_check_call(Execute ptr, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_CONS:
			WriteCheckCall_cons(ptr, pos);
			break;

		case LISPTYPE_VECTOR:
			WriteCheckCall_vector(ptr, pos);
			break;

		case LISPTYPE_ARRAY:
			WriteCheckCall_array(ptr, pos);
			break;

		default:
			break;
	}
}

static int write_circle_call(Execute ptr, addr stream, addr pos)
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

static int write_print_call(Execute ptr, addr stream, addr pos)
{
	int index = (int)GetType(pos);
	return (WriteCallTable[index])(ptr, stream, pos);
}


/*
 *  write print
 */
int write_default_print(Execute ptr, addr stream, addr pos)
{
	/* normal */
	if (! circle_print(ptr))
		return write_print_call(ptr, stream, pos);
	/* circle */
	if (discard_pretty_stream(stream))
		return 0;
	if (! push_pretty_stream_p(stream)) {
		push_write_object(ptr);
		write_check_call(ptr, pos);
	}
	return write_circle_call(ptr, stream, pos);
}

static int write_pretty_print(Execute ptr, addr stream, addr pos)
{
	addr dispatch;

	pprint_dispatch_print(ptr, &dispatch);
	if (find_function_print_dispatch(ptr, pos, dispatch, &dispatch))
		return 1;
	if (dispatch == Nil)
		return write_default_print(ptr, stream, pos);
	else
		return callclang_funcall(ptr, &dispatch, dispatch, stream, pos, NULL);
}

_g int write_print(Execute ptr, addr stream, addr pos)
{
	gchold_push_local(ptr->local, stream);
	if (pretty_print(ptr))
		return write_pretty_print(ptr, stream, pos);
	else
		return write_default_print(ptr, stream, pos);
}

_g int princ_print(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_close_control(ptr, &control);
	push_escape_print(ptr, 0);
	push_readably_print(ptr, 0);
	Return(write_print(ptr, stream, pos));

	return free_control_(ptr, control);
}

_g int prin1_print(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_close_control(ptr, &control);
	push_escape_print(ptr, 1);
	Return(write_print(ptr, stream, pos));

	return free_control_(ptr, control);
}

_g int print_print(Execute ptr, addr stream, addr pos)
{
	terpri_stream(stream);
	if (prin1_print(ptr, stream, pos))
		return 1;
	write_char_stream(stream, ' ');
	return 0;
}

_g int pprint_print(Execute ptr, addr stream, addr pos)
{
	addr control;

	push_close_control(ptr, &control);
	push_escape_print(ptr, 1);
	push_pretty_print(ptr, 1);
	terpri_stream(stream);
	Return(write_print(ptr, stream, pos));

	return free_control_(ptr, control);
}

_g int write_string_heap(Execute ptr, addr *ret, addr pos)
{
	addr control, stream;

	push_close_control(ptr, &control);
	open_output_string_stream(&stream, 0);
	Return(write_print(ptr, stream, pos));
	string_stream_heap(stream, ret);
	close_stream(stream);

	return free_control_(ptr, control);
}

_g int write_string_local(Execute ptr, addr *ret, addr pos)
{
	addr control, stream;

	push_close_control(ptr, &control);
	open_output_string_stream(&stream, 0);
	Return(write_print(ptr, stream, pos));
	string_stream_local(ptr->local, stream, ret);
	close_stream(stream);

	return free_control_(ptr, control);
}

_g int princ_string_heap(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	if (princ_print(ptr, stream, pos))
		return 1;
	string_stream_heap(stream, ret);
	close_stream(stream);

	return 0;
}

_g int princ_string_local(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	if (princ_print(ptr, stream, pos))
		return 1;
	string_stream_local(ptr->local, stream, ret);
	close_stream(stream);

	return 0;
}

_g int prin1_string_heap(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	if (prin1_print(ptr, stream, pos))
		return 1;
	string_stream_heap(stream, ret);
	close_stream(stream);

	return 0;
}

_g int prin1_string_local(Execute ptr, addr *ret, addr pos)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	if (prin1_print(ptr, stream, pos))
		return 1;
	string_stream_local(ptr->local, stream, ret);
	close_stream(stream);

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
	WriteCircleTable[LISPTYPE_CONS] = WriteCircleCall_cons;
	WriteCallTable[LISPTYPE_CONS] = WriteCall_cons;
	/* vector */
	WriteCircleTable[LISPTYPE_VECTOR] = WriteCircleCall_vector;
	WriteCallTable[LISPTYPE_VECTOR] = WriteCall_vector;
	/* array */
	WriteCircleTable[LISPTYPE_ARRAY] = WriteCircleCall_array;
	WriteCallTable[LISPTYPE_ARRAY] = WriteCall_array;
	/* object */
	WriteCallTable[LISPTYPE_NIL] = WriteCall_symbol;
	WriteCallTable[LISPTYPE_T] = WriteCall_symbol;
	WriteCallTable[LISPTYPE_TYPE] = WriteCall_type;
	WriteCallTable[LISPTYPE_CLOS] = WriteCall_clos;
	WriteCallTable[LISPTYPE_CHARACTER] = WriteCall_character;
	WriteCallTable[LISPTYPE_STRING] = WriteCall_string;
	WriteCallTable[LISPTYPE_HASHTABLE] = WriteCall_hashtable;
	WriteCallTable[LISPTYPE_READTABLE] = WriteCall_system;
	WriteCallTable[LISPTYPE_SYMBOL] = WriteCall_symbol;
	WriteCallTable[LISPTYPE_FIXNUM] = WriteCall_fixnum;
	WriteCallTable[LISPTYPE_BIGNUM] = WriteCall_bignum;
	WriteCallTable[LISPTYPE_RATIO] = WriteCall_ratio;
	WriteCallTable[LISPTYPE_SHORT_FLOAT] = WriteCall_error;
	WriteCallTable[LISPTYPE_SINGLE_FLOAT] = WriteCall_single_float;
	WriteCallTable[LISPTYPE_DOUBLE_FLOAT] = WriteCall_double_float;
	WriteCallTable[LISPTYPE_LONG_FLOAT] = WriteCall_long_float;
	WriteCallTable[LISPTYPE_COMPLEX] = WriteCall_complex;
	WriteCallTable[LISPTYPE_CONTROL] = WriteCall_system;
	WriteCallTable[LISPTYPE_CODE] = WriteCall_system;
	WriteCallTable[LISPTYPE_CALLNAME] = WriteCall_callname;
	WriteCallTable[LISPTYPE_FUNCTION] = WriteCall_function;
	WriteCallTable[LISPTYPE_INDEX] = WriteCall_index;
	WriteCallTable[LISPTYPE_PACKAGE] = WriteCall_package;
	WriteCallTable[LISPTYPE_RANDOM_STATE] = WriteCall_random_state;
	WriteCallTable[LISPTYPE_PATHNAME] = WriteCall_pathname;
	WriteCallTable[LISPTYPE_STREAM] = WriteCall_stream;
	WriteCallTable[LISPTYPE_QUOTE] = WriteCall_quote;
	WriteCallTable[LISPTYPE_RESTART] = WriteCall_restart;
	WriteCallTable[LISPTYPE_EVAL] = WriteCall_system;
	WriteCallTable[LISPTYPE_ENVIRONMENT] = WriteCall_system;
	WriteCallTable[LISPTYPE_BITVECTOR] = WriteCall_bitvector;
	WriteCallTable[LISPTYPE_PRINT_DISPATCH] = WriteCall_system;
	WriteCallTable[LISPTYPE_BYTESPEC] = WriteCall_bytespec;

	WriteCallTable[LISPSYSTEM_CHARACTER2] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_CHARQUEUE] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_CHARBIT] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_SYMSTACK] = WriteCall_system;
	WriteCallTable[LISPSYSTEM_SYMARRAY] = WriteCall_system;
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

