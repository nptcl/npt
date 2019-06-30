#include "object.c"
#include "code.h"
#include "degrade.h"

/*
 *  Header
 */
static int test_sizecheck(void)
{
#if defined(LISP_ARCH_32BIT)
	test((SYMBOL_INDEX_SIZE % 2) == 0, "object_h1");
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
	addr left, right;

	Nil = Unbound;
	nil_heap();
	test(GetType(Nil) == LISPTYPE_NIL, "nil1");
	test(GetStatusReadOnly(Nil), "nil2");
	test(GetStatusSystem(Nil), "nil3");
	test(IsList(Nil), "nil6");

	left = 0;
	GetNameSymbol(Nil, &left);
	test(GetType(left) == LISPTYPE_STRING, "nil7");
	test(string_equal_char(left, "NIL"), "nil8");
	GetValueSymbol(Nil, &left);
	test(left == Nil, "nil9");
	GetFunctionSymbol(Nil, &left);
	test(left == Unbound, "nil10");

	GetCar(Nil, &left);
	test(left == Nil, "nil11");
	GetCdr(Nil, &left);
	test(left == Nil, "nil12");

	getarray(Nil, SYMBOL_INDEX_VALUE, &left);
	test(consp(left), "nil13");
	GetCons(left, &left, &right);
	test(left == Nil, "nil14");
	test(right == Nil, "nil15");

	RETURN;
}

static int test_t(void)
{
	addr left;

	T = Unbound;
	t_heap();
	test(GetType(T) == LISPTYPE_T, "t1");
	test(GetStatusReadOnly(T), "t2");
	test(GetStatusSystem(T), "t3");

	GetNameSymbol(T, &left);
	test(GetType(left) == LISPTYPE_STRING, "t5");
	test(string_equal_char(left, "T"), "t6");
	GetValueSymbol(T, &left);
	test(left == T, "t7");
	GetFunctionSymbol(T, &left);
	test(left == Unbound, "t8");

	getarray(T, SYMBOL_INDEX_PLIST, &left);
	test(left == Nil, "t9");
	getarray(T, SYMBOL_INDEX_INFO, &left);
	test(left == Nil, "t10");

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
	test(GetType(pos) == LISPTYPE_CONS, "cons1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "cons2");
	SetCons(pos, (addr)mem1, (addr)mem2);
	left = right = 0;
	GetCons(pos, &left, &right);
	test(left == (addr)mem1, "cons3");
	test(right == (addr)mem2, "cons4");

	consnil_local(Local_Thread, &pos);
	test(GetType(pos) == LISPTYPE_CONS, "cons5");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "cons6");
	test(GetStatusDynamic(pos), "cons7");

	SetCons(pos, (addr)mem1, (addr)mem2);
	left = right = 0;
	GetCons(pos, &left, &right);
	test(left == (addr)mem1, "cons8");
	test(right == (addr)mem2, "cons9");

	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	setcons_force(pos, (addr)mem3, (addr)mem4);
	left = right = 0;
	GetCons(pos, &left, &right);
	test(left == (addr)mem3, "cons10");
	test(right == (addr)mem4, "cons11");

	RETURN;
}

static int test_cons_left(void)
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
	test(left == (addr)mem1, "cons_left1");
	GetCdr(pos, &right);
	test(right == (addr)mem2, "cons_left2");

	SetCar(pos, (addr)mem3);
	SetCdr(pos, (addr)mem4);
	left = right = 0;
	GetCons(pos, &left, &right);
	test(left == (addr)mem3, "cons_left3");
	test(right == (addr)mem4, "cons_left4");

	SetStatusValue(pos, LISPSTATUS_READONLY, 1);
	setconscar_force(pos, (addr)mem5);
	setconscdr_force(pos, (addr)mem6);
	getcons_unsafe(pos, &left, &right);
	test(left == (addr)mem5, "cons_left5");
	test(right == (addr)mem6, "cons_left6");

	RETURN;
}

static int test_cons_nil(void)
{
	addr left, right;

	left = right = 0;
	GetCons(Nil, &left, &right);
	test(left == Nil, "cons_nil1");
	test(right == Nil, "cons_nil2");
	left = right = 0;
	GetCar(Nil, &left);
	GetCdr(Nil, &right);
	test(left == Nil, "cons_nil3");
	test(right == Nil, "cons_nil4");

	RETURN;
}

static int test_singlep(void)
{
	addr value, pos;

	make_fixnum_heap(&value, 100);
	test(! singlep(value), "singlep1");
	list_heap(&pos, value, NULL);
	test(singlep(pos), "singlep2");
	list_heap(&pos, value, T, NULL);
	test(! singlep(pos), "singlep3");

	RETURN;
}

static int test_list_alloc(void)
{
	addr cons, left, value;

	list_alloc(NULL, &cons, NULL);
	test(cons == Nil, "list_alloc1");
	list_alloc(NULL, &cons, T, NULL);
	test(cons != Nil, "list_alloc2");
	GetCons(cons, &left, &cons);
	test(left == T, "list_alloc3");
	test(cons == Nil, "list_alloc4");
	consnil_heap(&value);

	list_alloc(NULL, &cons, T, Nil, value, NULL);
	GetCons(cons, &left, &cons);
	test(left == T, "list_alloc5");
	GetCons(cons, &left, &cons);
	test(left == Nil, "list_alloc6");
	GetCons(cons, &left, &cons);
	test(left == value, "list_alloc7");
	test(cons == Nil, "list_alloc8");

	RETURN;
}

static int test_array(void)
{
	addr pos, temp, mem1, mem2;

	make_fixnum_heap(&mem1, 123);
	make_fixnum_heap(&mem2, 4444);

	vector2_heap(&pos, 10);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector2-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "vector2-2");
	test(GetLenArrayA2(pos) == 10, "vector2-3");
	temp = 0;
	GetArrayA2(pos, 9, &temp);
	test(temp == Nil, "vector2-5");
	SetArrayA2(pos, 3, mem1);
	GetArrayA2(pos, 3, &temp);
	test(temp == mem1, "vector2-6");

	vector2_local(Local_Thread, &pos, 22);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector2-7");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "vector2-8");
	test(GetStatusDynamic(pos), "vector2-9");
	test(GetLenArrayA2(pos) == 22, "vector2-10");
	SetArrayA2(pos, 21, mem2);
	GetArrayA2(pos, 21, &temp);
	test(temp == mem2, "vector2-11");

	vector4_heap(&pos, 10);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector4-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY4, "vector4-2");
	test(GetLenArrayA4(pos) == 10, "vector4-3");
	temp = 0;
	GetArrayA4(pos, 9, &temp);
	test(temp == Nil, "vector4-5");
	SetArrayA4(pos, 3, mem1);
	GetArrayA4(pos, 3, &temp);
	test(temp == mem1, "vector4-6");

	vector4_local(Local_Thread, &pos, 22);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector4-7");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY4, "vector4-8");
	test(GetStatusDynamic(pos), "vector4-9");
	test(GetLenArrayA4(pos) == 22, "vector4-10");
	SetArrayA4(pos, 21, mem2);
	GetArrayA4(pos, 21, &temp);
	test(temp == mem2, "vector4-11");

	vector_heap(&pos, 2);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector12");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "vector13");
	test(GetLenArrayA2(pos) == 2, "vector14");

	vector_local(Local_Thread, &pos, 4);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector16");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "vector17");
	test(GetStatusDynamic(pos), "vector18");
	test(GetLenArrayA2(pos) == 4, "vector19");

#ifdef LISP_ARCH_64BIT
	vector8_heap(&pos, 11);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector21");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY8, "vector22");
	test(GetLenArrayA8(pos) == 11, "vector23");
	temp = 0;
	GetArrayA8(pos, 10, &temp);
	test(temp == Nil, "vector15");
	SetArrayA8(pos, 10, mem1);
	GetArrayA8(pos, 10, &temp);
	test(temp == mem1, "vector26");

	vector8_local(Local_Thread, &pos, 8);
	test(GetType(pos) == LISPTYPE_VECTOR, "vector27");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY8, "vector28");
	test(GetStatusDynamic(pos), "vector29");
	test(GetLenArrayA8(pos) == 8, "vector30");
#endif

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
	test(GetType(pos) == LISPTYPE_FIXNUM, "fixnum1");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "fixnum2");
	value = 0;
	GetFixnum(pos, &value);
	test(value == 10, "fixnum4");
	fixnum_heap(&check, 10);
	test(pos == check, "fixnum5");
	fixnum_heap(&check, 11);
	test(pos != check, "fixnum6");

	fixnum_local(Local_Thread, &check, 10);
	test(pos != check, "fixnum7");
	test(fixnumequal(pos, check), "fixnum8");
	test(fixnumcompare(pos, check) == 0, "fixnum9");
	fixnum_local(Local_Thread, &check, 100);
	test(! fixnumequal(pos, check), "fixnum10");
	test(fixnumcompare(pos, check) < 0, "fixnum11");
	test(fixnumcompare(check, pos) > 0, "fixnum12");

	RETURN;
}

static int test_index(void)
{
	addr pos;
	size_t value;

	index_heap(&pos, 100);
	test(GetType(pos) == LISPTYPE_INDEX, "index1");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "index2");
	value = 0;
	GetIndex(pos, &value);
	test(value == 100, "index4");

	index_local(Local_Thread, &pos, 200);
	test(GetType(pos) == LISPTYPE_INDEX, "index5");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "index6");
	test(GetStatusDynamic(pos), "index7");
	value = 0;
	GetIndex(pos, &value);
	test(value == 200, "index9");

	SetIndex(pos, 300);
	GetIndex(pos, &value);
	test(value == 300, "index10");
	IncIndex(pos, 111);
	GetIndex(pos, &value);
	test(value == 411, "index11");
	DecIndex(pos, 11);
	GetIndex(pos, &value);
	test(value == 400, "index12");

	RETURN;
}

static int test_float(void)
{
	addr pos;
	single_float value;

	single_float_heap(&pos, 1.23f);
	test(GetType(pos) == LISPTYPE_SINGLE_FLOAT, "float1");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "float2");
	test(GetStatusReadOnly(pos), "float3");
	value = 0;
	GetSingleFloat(pos, &value);
	test(value == 1.23f, "float5");

	single_float_local(Local_Thread, &pos, 100.0f);
	test(GetType(pos) == LISPTYPE_SINGLE_FLOAT, "float6");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "float7");
	test(GetStatusDynamic(pos), "float8");
	test(GetStatusReadOnly(pos), "float9");
	SetStatusValue(pos, LISPSTATUS_READONLY, 0);
	SetSingleFloat(pos, 222.0f);
	GetSingleFloat(pos, &value);
	test(value == 222.0f, "float11");

	RETURN;
}

static int test_double(void)
{
	addr pos;
	double_float value;

	double_float_heap(&pos, 123.123456789);
	test(GetType(pos) == LISPTYPE_DOUBLE_FLOAT, "double1");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "double2");
	test(GetStatusReadOnly(pos), "double3");
	value = 0;
	GetDoubleFloat(pos, &value);
	test(value == 123.123456789, "double5");

	double_float_local(Local_Thread, &pos, 100.0);
	test(GetType(pos) == LISPTYPE_DOUBLE_FLOAT, "double6");
	test(GetStatusSize(pos) == LISPSIZE_BODY2, "double7");
	test(GetStatusDynamic(pos), "double8");
	test(GetStatusReadOnly(pos), "double9");
	SetStatusValue(pos, LISPSTATUS_READONLY, 0);
	SetDoubleFloat(pos, 222.0);
	GetDoubleFloat(pos, &value);
	test(value == 222.0, "double11");

	RETURN;
}

static int test_queue_heap(void)
{
	addr pos, key, value;

	queue_heap(&pos);
	test(GetType(pos) == LISPTYPE_CONS, "queue_heap1");
	rootqueue(pos, &key);
	tailqueue(pos, &value);
	test(key == Nil, "queue_heap2");
	test(value == Nil, "queue_heap3");

	strvect_char_heap(&value, "aaa");
	pushqueue_heap(pos, value);
	rootqueue(pos, &key);
	tailqueue(pos, &value);
	test(key != Nil, "queue_heap3");
	test(value != Nil, "queue_heap4");
	GetCar(key, &key);
	test(string_equal_char(key, "aaa"), "queue_heap5");

	strvect_char_heap(&value, "bbb");
	pushqueue_heap(pos, value);
	rootqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char(key, "aaa"), "queue_heap6");
	tailqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char(key, "bbb"), "queue_heap7");

	strvect_char_heap(&value, "ccc");
	pushqueue_heap(pos, value);
	rootqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char(key, "aaa"), "queue_heap8");
	tailqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char(key, "ccc"), "queue_heap9");

	key = 0;
	test(firstqueue(pos, &key) == 0, "queue_heap10");
	test(string_equal_char(key, "aaa"), "queue_heap11");
	test(lastqueue(pos, &key) == 0, "queue_heap12");
	test(string_equal_char(key, "ccc"), "queue_heap13");

	test(nthqueue(pos, 0, &key) == 0, "queue_heap14");
	test(string_equal_char(key, "aaa"), "queue_heap15");
	test(nthqueue(pos, 1, &key) == 0, "queue_heap16");
	test(string_equal_char(key, "bbb"), "queue_heap17");
	test(nthqueue(pos, 2, &key) == 0, "queue_heap18");
	test(string_equal_char(key, "ccc"), "queue_heap19");
	test(nthqueue(pos, 3, &key) != 0, "queue_heap20");

	strvect_char_heap(&value, "ddd");
	dotqueue(pos, value);
	rootqueue(pos, &value);

	GetCons(value, &key, &value);
	test(string_equal_char(key, "aaa"), "queue_heap21");
	GetCons(value, &key, &value);
	test(string_equal_char(key, "bbb"), "queue_heap22");
	GetCons(value, &key, &value);
	test(string_equal_char(key, "ccc"), "queue_heap23");
	test(string_equal_char(value, "ddd"), "queue_heap24");

	RETURN;
}

static int test_queue_local(void)
{
	addr pos, key, value;

	queue_local(Local_Thread, &pos);
	test(GetType(pos) == LISPTYPE_CONS, "queue_local1");
	rootqueue(pos, &key);
	tailqueue(pos, &value);
	test(key == Nil, "queue_local2");
	test(value == Nil, "queue_local3");

	strvect_char_heap(&value, "aaa");
	pushqueue_local(Local_Thread, pos, value);
	rootqueue(pos, &key);
	tailqueue(pos, &value);
	test(key != Nil, "queue_local3");
	test(value != Nil, "queue_local4");
	GetCar(key, &key);
	test(string_equal_char(key, "aaa"), "queue_local5");

	strvect_char_heap(&value, "bbb");
	pushqueue_local(Local_Thread, pos, value);
	rootqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char(key, "aaa"), "queue_local6");
	tailqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char(key, "bbb"), "queue_local7");

	strvect_char_heap(&value, "ccc");
	pushqueue_local(Local_Thread, pos, value);
	rootqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char(key, "aaa"), "queue_local8");
	tailqueue(pos, &key);
	GetCar(key, &key);
	test(string_equal_char(key, "ccc"), "queue_local9");

	key = 0;
	test(firstqueue(pos, &key) == 0, "queue_local10");
	test(string_equal_char(key, "aaa"), "queue_local11");
	test(lastqueue(pos, &key) == 0, "queue_local12");
	test(string_equal_char(key, "ccc"), "queue_local13");

	test(nthqueue(pos, 0, &key) == 0, "queue_local14");
	test(string_equal_char(key, "aaa"), "queue_local15");
	test(nthqueue(pos, 1, &key) == 0, "queue_local16");
	test(string_equal_char(key, "bbb"), "queue_local17");
	test(nthqueue(pos, 2, &key) == 0, "queue_local18");
	test(string_equal_char(key, "ccc"), "queue_local19");
	test(nthqueue(pos, 3, &key) != 0, "queue_local20");

	strvect_char_heap(&value, "ddd");
	dotqueue(pos, value);
	rootqueue(pos, &value);

	GetCons(value, &key, &value);
	test(string_equal_char(key, "aaa"), "queue_local21");
	GetCons(value, &key, &value);
	test(string_equal_char(key, "bbb"), "queue_local22");
	GetCons(value, &key, &value);
	test(string_equal_char(key, "ccc"), "queue_local23");
	test(string_equal_char(value, "ddd"), "queue_local24");

	RETURN;
}

static int testbreak_object(void)
{
	TestBreak(test_sizecheck);
	TestBreak(test_nil);
	TestBreak(test_t);
	TestBreak(test_cons);
	TestBreak(test_cons_left);
	TestBreak(test_cons_nil);
	TestBreak(test_singlep);
	TestBreak(test_list_alloc);
	TestBreak(test_array);
	TestBreak(test_fixnum);
	TestBreak(test_index);
	TestBreak(test_float);
	TestBreak(test_double);
	TestBreak(test_queue_heap);
	TestBreak(test_queue_local);

	return 0;
}

int test_object(void)
{
	lispcode code;
	int result;

	TITLE;
	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	begin_code_thread(&code);
	if (code_run_p(code)) {
		lisp_initialize = 1;
		result = testbreak_object();
	}
	end_code_thread();
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

