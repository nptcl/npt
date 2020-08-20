#include "object.c"
#include "code.h"
#include "degrade.h"

/*
 *  Header
 */
static int test_symbol(void)
{
#if defined(LISP_ARCH_32BIT)
	test((SYMBOL_INDEX_SIZE % 2) == 0, "symbol.1");
	RETURN;
#else
	return 0;
#endif
}


/*
 *  object
 */
static int test_nil(void)
{
	addr left;

	Nil = Unbound;
	nil_heap();
	test(GetType(Nil) == LISPTYPE_NIL, "nil.1");
	test(GetStatusReadOnly(Nil), "nil.2");
	test(GetStatusSystem(Nil), "nil.3");
	test(IsList(Nil), "nil.4");

	left = 0;
	GetNameSymbol(Nil, &left);
	test(GetType(left) == LISPTYPE_STRING, "nil.5");
	test(string_equal_char_debug(left, "NIL"), "nil.6");
	GetValueSymbol(Nil, &left);
	test(left == Nil, "nil.7");
	GetFunctionSymbol(Nil, &left);
	test(left == Unbound, "nil.8");

	GetCar(Nil, &left);
	test(left == Nil, "nil.9");
	GetCdr(Nil, &left);
	test(left == Nil, "nil.10");

	getarray(Nil, SYMBOL_INDEX_VALUE, &left);
	test(left == Nil, "nil.11");

	RETURN;
}

static int test_t(void)
{
	addr left;

	T = Unbound;
	t_heap();
	test(GetType(T) == LISPTYPE_T, "t.1");
	test(GetStatusReadOnly(T), "t.2");
	test(GetStatusSystem(T), "t.3");

	GetNameSymbol(T, &left);
	test(GetType(left) == LISPTYPE_STRING, "t.4");
	test(string_equal_char_debug(left, "T"), "t.5");
	GetValueSymbol(T, &left);
	test(left == T, "t.6");
	GetFunctionSymbol(T, &left);
	test(left == Unbound, "t.7");

	getarray(T, SYMBOL_INDEX_PLIST, &left);
	test(left == Nil, "t.8");
	getarray(T, SYMBOL_INDEX_INFO, &left);
	test(left == Nil, "t.9");

	RETURN;
}

static int test_cons(void)
{
	addr pos, left, right, mem1, mem2, mem3, mem4;

	consnil_heap(&mem1);
	consnil_heap(&mem2);
	consnil_heap(&mem3);
	consnil_heap(&mem4);
	consnil_heap(&pos);
	test(GetType(pos) == LISPTYPE_CONS, "cons.1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "cons.2");
	SetCons(pos, (addr)mem1, (addr)mem2);
	left = right = 0;
	GetCons(pos, &left, &right);
	test(left == (addr)mem1, "cons.3");
	test(right == (addr)mem2, "cons.4");

	consnil_local(Local_Thread, &pos);
	test(GetType(pos) == LISPTYPE_CONS, "cons.5");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "cons.6");
	test(GetStatusDynamic(pos), "cons.7");

	SetCons(pos, (addr)mem1, (addr)mem2);
	left = right = 0;
	GetCons(pos, &left, &right);
	test(left == (addr)mem1, "cons.8");
	test(right == (addr)mem2, "cons.9");

	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	setcons_force(pos, (addr)mem3, (addr)mem4);
	left = right = 0;
	GetCons(pos, &left, &right);
	test(left == (addr)mem3, "cons.10");
	test(right == (addr)mem4, "cons.11");

	RETURN;
}

static int test_carcdr(void)
{
	addr pos, left, right, mem1, mem2, mem3, mem4, mem5, mem6;

	consnil_heap(&mem1);
	consnil_heap(&mem2);
	consnil_heap(&mem3);
	consnil_heap(&mem4);
	consnil_heap(&mem5);
	consnil_heap(&mem6);
	consnil_heap(&pos);
	SetCons(pos, (addr)mem1, (addr)mem2);
	left = right = 0;
	GetCar(pos, &left);
	test(left == (addr)mem1, "carcdr.1");
	GetCdr(pos, &right);
	test(right == (addr)mem2, "carcdr2");

	SetCar(pos, (addr)mem3);
	SetCdr(pos, (addr)mem4);
	left = right = 0;
	GetCons(pos, &left, &right);
	test(left == (addr)mem3, "carcdr.3");
	test(right == (addr)mem4, "carcdr.4");

	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	setconscar_force(pos, (addr)mem5);
	setconscdr_force(pos, (addr)mem6);
	getcons_unsafe(pos, &left, &right);
	test(left == (addr)mem5, "carcdr.5");
	test(right == (addr)mem6, "carcdr.6");

	RETURN;
}

static int test_cons_nil(void)
{
	addr left, right;

	left = right = 0;
	GetCons(Nil, &left, &right);
	test(left == Nil, "cons_nil.1");
	test(right == Nil, "cons_nil.2");
	left = right = 0;
	GetCar(Nil, &left);
	GetCdr(Nil, &right);
	test(left == Nil, "cons_nil.3");
	test(right == Nil, "cons_nil.4");

	RETURN;
}

static int test_singlep(void)
{
	addr value, pos;

	make_fixnum_heap(&value, 100);
	test(! singlep(value), "singlep.1");
	list_heap(&pos, value, NULL);
	test(singlep(pos), "singlep.2");
	list_heap(&pos, value, T, NULL);
	test(! singlep(pos), "singlep.3");

	RETURN;
}

static int test_vector(void)
{
	addr pos, temp, mem1, mem2;

	make_fixnum_heap(&mem1, 123);
	make_fixnum_heap(&mem2, 4444);

	vector2_heap(&pos, 10);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector2.1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "vector2.2");
	test(GetLenArrayA2(pos) == 10, "vector2.3");
	temp = 0;
	GetArrayA2(pos, 9, &temp);
	test(temp == Nil, "vector2.4");
	SetArrayA2(pos, 3, mem1);
	GetArrayA2(pos, 3, &temp);
	test(temp == mem1, "vector2.5");

	vector2_local(Local_Thread, &pos, 22);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector2.6");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "vector2.7");
	test(GetStatusDynamic(pos), "vector2.8");
	test(GetLenArrayA2(pos) == 22, "vector2.9");
	SetArrayA2(pos, 21, mem2);
	GetArrayA2(pos, 21, &temp);
	test(temp == mem2, "vector2.10");

	vector4_heap(&pos, 10);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector4.1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY4, "vector4.2");
	test(GetLenArrayA4(pos) == 10, "vector4.3");
	temp = 0;
	GetArrayA4(pos, 9, &temp);
	test(temp == Nil, "vector4.4");
	SetArrayA4(pos, 3, mem1);
	GetArrayA4(pos, 3, &temp);
	test(temp == mem1, "vector4.5");

	vector4_local(Local_Thread, &pos, 22);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector4.6");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY4, "vector4.7");
	test(GetStatusDynamic(pos), "vector4.8");
	test(GetLenArrayA4(pos) == 22, "vector4.9");
	SetArrayA4(pos, 21, mem2);
	GetArrayA4(pos, 21, &temp);
	test(temp == mem2, "vector4.10");

#ifdef LISP_ARCH_64BIT
	vector8_heap(&pos, 11);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector8.1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY8, "vector8.2");
	test(GetLenArrayA8(pos) == 11, "vector8.3");
	temp = 0;
	GetArrayA8(pos, 10, &temp);
	test(temp == Nil, "vector8.4");
	SetArrayA8(pos, 10, mem1);
	GetArrayA8(pos, 10, &temp);
	test(temp == mem1, "vector8.5");

	vector8_local(Local_Thread, &pos, 8);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector8.6");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY8, "vector8.7");
	test(GetStatusDynamic(pos), "vector8.8");
	test(GetLenArrayA8(pos) == 8, "vector8.9");
#endif

	vector_heap(&pos, 2);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector.1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "vector.2");
	test(GetLenArrayA2(pos) == 2, "vector.3");

	vector_local(Local_Thread, &pos, 4);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector.4");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "vector.5");
	test(GetStatusDynamic(pos), "vector.6");
	test(GetLenArrayA2(pos) == 4, "vector.7");

	RETURN;
}

static int test_fixnum(void)
{
	addr pos, check;
	fixnum value;

	build_lisproot(Execute_Thread);
	build_constant();
	build_object();
	fixnum_heap(&pos, 10);
	test(GetType(pos) == LISPTYPE_FIXNUM, "fixnum.1");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "fixnum.2");
	value = 0;
	GetFixnum(pos, &value);
	test(value == 10, "fixnum.3");
	fixnum_heap(&check, 10);
	test(pos == check, "fixnum.4");
	fixnum_heap(&check, 11);
	test(pos != check, "fixnum.5");

	fixnum_local(Local_Thread, &check, 10);
	test(pos != check, "fixnum.6");
	test(fixnumequal(pos, check), "fixnum.7");
	test(fixnumcompare(pos, check) == 0, "fixnum.8");
	fixnum_local(Local_Thread, &check, 100);
	test(! fixnumequal(pos, check), "fixnum.9");
	test(fixnumcompare(pos, check) < 0, "fixnum.10");
	test(fixnumcompare(check, pos) > 0, "fixnum.11");

	RETURN;
}

static int test_index(void)
{
	addr pos;
	size_t value;

	index_heap(&pos, 100);
	test(GetType(pos) == LISPTYPE_INDEX, "index.1");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "index.2");
	value = 0;
	GetIndex(pos, &value);
	test(value == 100, "index.3");

	index_local(Local_Thread, &pos, 200);
	test(GetType(pos) == LISPTYPE_INDEX, "index.4");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "index.5");
	test(GetStatusDynamic(pos), "index.6");
	value = 0;
	GetIndex(pos, &value);
	test(value == 200, "index.7");

	SetIndex(pos, 300);
	GetIndex(pos, &value);
	test(value == 300, "index.8");
	IncIndex(pos, 111);
	GetIndex(pos, &value);
	test(value == 411, "index.9");
	DecIndex(pos, 11);
	GetIndex(pos, &value);
	test(value == 400, "index.10");

	RETURN;
}

static int test_single_float(void)
{
	addr pos;
	single_float value;

	single_float_heap(&pos, 1.23f);
	test(GetType(pos) == LISPTYPE_SINGLE_FLOAT, "single-float.1");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "single-float.2");
	test(GetStatusReadOnly(pos), "single-float.3");
	value = 0;
	GetSingleFloat(pos, &value);
	test(value == 1.23f, "single-float.4");

	single_float_local(Local_Thread, &pos, 100.0f);
	test(GetType(pos) == LISPTYPE_SINGLE_FLOAT, "single-float.5");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "single-float.6");
	test(GetStatusDynamic(pos), "single-float.7");
	test(GetStatusReadOnly(pos), "single-float.8");
	SetStatusValue(pos, LISPSTATUS_READONLY, 0);
	SetSingleFloat(pos, 222.0f);
	GetSingleFloat(pos, &value);
	test(value == 222.0f, "single-float.9");

	RETURN;
}

static int test_double_float(void)
{
	addr pos;
	double_float value;

	double_float_heap(&pos, 123.123456789);
	test(GetType(pos) == LISPTYPE_DOUBLE_FLOAT, "double_float.1");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "double_float.2");
	test(GetStatusReadOnly(pos), "double_float.3");
	value = 0;
	GetDoubleFloat(pos, &value);
	test(value == 123.123456789, "double_float.4");

	double_float_local(Local_Thread, &pos, 100.0);
	test(GetType(pos) == LISPTYPE_DOUBLE_FLOAT, "double_float.5");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "double_float.6");
	test(GetStatusDynamic(pos), "double_float.7");
	test(GetStatusReadOnly(pos), "double_float.8");
	SetStatusValue(pos, LISPSTATUS_READONLY, 0);
	SetDoubleFloat(pos, 222.0);
	GetDoubleFloat(pos, &value);
	test(value == 222.0, "double_float.9");

	RETURN;
}

static int test_long_float(void)
{
	addr pos;
	long_float value;

	long_float_heap(&pos, 123.123456789L);
	test(GetType(pos) == LISPTYPE_LONG_FLOAT, "long_float.1");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "long_float.2");
	test(GetStatusReadOnly(pos), "long_float.3");
	value = 0;
	GetLongFloat(pos, &value);
	test(value == 123.123456789L, "long_float.4");

	long_float_local(Local_Thread, &pos, 100.0L);
	test(GetType(pos) == LISPTYPE_LONG_FLOAT, "long_float.5");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "long_float.6");
	test(GetStatusDynamic(pos), "long_float.7");
	test(GetStatusReadOnly(pos), "long_float.8");
	SetStatusValue(pos, LISPSTATUS_READONLY, 0);
	SetLongFloat(pos, 222.0L);
	GetLongFloat(pos, &value);
	test(value == 222.0L, "long_float.9");

	RETURN;
}

static int test_queue_heap(void)
{
	addr pos, key, value;

	queue_heap(&pos);
	test(GetType(pos) == LISPTYPE_CONS, "queue_heap.1");
	rootqueue(pos, &key);
	tailqueue(pos, &value);
	test(key == Nil, "queue_heap.2");
	test(value == Nil, "queue_heap.3");

	strvect_char_heap(&value, "aaa");
	pushqueue_heap(pos, value);
	rootqueue(pos, &key);
	tailqueue(pos, &value);
	test(key != Nil, "queue_heap.4");
	test(value != Nil, "queue_heap.5");
	GetCar(key, &key);
	test(string_equal_char_debug(key, "aaa"), "queue_heap.6");

	strvect_char_heap(&value, "bbb");
	pushqueue_heap(pos, value);
	rootqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char_debug(key, "aaa"), "queue_heap.7");
	tailqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char_debug(key, "bbb"), "queue_heap.8");

	strvect_char_heap(&value, "ccc");
	pushqueue_heap(pos, value);
	rootqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char_debug(key, "aaa"), "queue_heap.9");
	tailqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char_debug(key, "ccc"), "queue_heap.10");

	key = 0;
	test(firstqueue(pos, &key) == 0, "queue_heap.11");
	test(string_equal_char_debug(key, "aaa"), "queue_heap.12");
	test(lastqueue(pos, &key) == 0, "queue_heap.13");
	test(string_equal_char_debug(key, "ccc"), "queue_heap.14");

	test(nthqueue(pos, 0, &key) == 0, "queue_heap.15");
	test(string_equal_char_debug(key, "aaa"), "queue_heap.16");
	test(nthqueue(pos, 1, &key) == 0, "queue_heap.17");
	test(string_equal_char_debug(key, "bbb"), "queue_heap.18");
	test(nthqueue(pos, 2, &key) == 0, "queue_heap.19");
	test(string_equal_char_debug(key, "ccc"), "queue_heap.20");
	test(nthqueue(pos, 3, &key) != 0, "queue_heap.21");

	strvect_char_heap(&value, "ddd");
	dotqueue(pos, value);
	rootqueue(pos, &value);

	GetCons(value, &key, &value);
	test(string_equal_char_debug(key, "aaa"), "queue_heap.22");
	GetCons(value, &key, &value);
	test(string_equal_char_debug(key, "bbb"), "queue_heap.23");
	GetCons(value, &key, &value);
	test(string_equal_char_debug(key, "ccc"), "queue_heap.24");
	test(string_equal_char_debug(value, "ddd"), "queue_heap.25");

	RETURN;
}

static int test_queue_local(void)
{
	addr pos, key, value;

	queue_local(Local_Thread, &pos);
	test(GetType(pos) == LISPTYPE_CONS, "queue_local.1");
	rootqueue(pos, &key);
	tailqueue(pos, &value);
	test(key == Nil, "queue_local.2");
	test(value == Nil, "queue_local.3");

	strvect_char_heap(&value, "aaa");
	pushqueue_local(Local_Thread, pos, value);
	rootqueue(pos, &key);
	tailqueue(pos, &value);
	test(key != Nil, "queue_local.4");
	test(value != Nil, "queue_local.5");
	GetCar(key, &key);
	test(string_equal_char_debug(key, "aaa"), "queue_local.6");

	strvect_char_heap(&value, "bbb");
	pushqueue_local(Local_Thread, pos, value);
	rootqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char_debug(key, "aaa"), "queue_local.7");
	tailqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char_debug(key, "bbb"), "queue_local.8");

	strvect_char_heap(&value, "ccc");
	pushqueue_local(Local_Thread, pos, value);
	rootqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char_debug(key, "aaa"), "queue_local.9");
	tailqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char_debug(key, "ccc"), "queue_local.10");

	key = 0;
	test(firstqueue(pos, &key) == 0, "queue_local.11");
	test(string_equal_char_debug(key, "aaa"), "queue_local.12");
	test(lastqueue(pos, &key) == 0, "queue_local.13");
	test(string_equal_char_debug(key, "ccc"), "queue_local.14");

	test(nthqueue(pos, 0, &key) == 0, "queue_local.15");
	test(string_equal_char_debug(key, "aaa"), "queue_local.16");
	test(nthqueue(pos, 1, &key) == 0, "queue_local.17");
	test(string_equal_char_debug(key, "bbb"), "queue_local.18");
	test(nthqueue(pos, 2, &key) == 0, "queue_local.19");
	test(string_equal_char_debug(key, "ccc"), "queue_local.20");
	test(nthqueue(pos, 3, &key) != 0, "queue_local.21");

	strvect_char_heap(&value, "ddd");
	dotqueue(pos, value);
	rootqueue(pos, &value);

	GetCons(value, &key, &value);
	test(string_equal_char_debug(key, "aaa"), "queue_local.22");
	GetCons(value, &key, &value);
	test(string_equal_char_debug(key, "bbb"), "queue_local.23");
	GetCons(value, &key, &value);
	test(string_equal_char_debug(key, "ccc"), "queue_local.24");
	test(string_equal_char_debug(value, "ddd"), "queue_local.25");

	RETURN;
}


/*
 *  object
 */
static int testcase_object(void)
{
	TestBreak(test_symbol);
	TestBreak(test_nil);
	TestBreak(test_t);
	TestBreak(test_cons);
	TestBreak(test_carcdr);
	TestBreak(test_cons_nil);
	TestBreak(test_singlep);
	TestBreak(test_vector);
	TestBreak(test_fixnum);
	TestBreak(test_index);
	TestBreak(test_single_float);
	TestBreak(test_double_float);
	TestBreak(test_long_float);
	TestBreak(test_queue_heap);
	TestBreak(test_queue_local);

	return 0;
}

int test_object(void)
{
	DegradeTitle;
	return degrade_code(NULL, testcase_object);
}

