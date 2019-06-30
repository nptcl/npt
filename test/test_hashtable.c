#include "hashtable.c"
#include "constant.h"
#include "degrade.h"
#include "execute.h"
#include "object.h"
#include "strtype.h"


/*
 *  hashtable testcase
 */
static int test_hashtable_heap(void)
{
	addr pos, table;
	size_t limit, size;
	struct StructHashtable *ptr;

	hashtable_heap(&pos);
	test(GetType(pos) == LISPTYPE_HASHTABLE, "hashtable_heap1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "hashtable_heap2");
	GetTableHash(pos, &table);
	test(GetType(table) == LISPTYPE_VECTOR, "hashtable_heap3");
#ifdef LISP_ARCH_32BIT
	test(GetStatusSize(table) == LISPSIZE_ARRAY4, "hashtable_heap4");
#else
	test(GetStatusSize(table) == LISPSIZE_ARRAY8, "hashtable_heap4");
#endif
	LenArrayHash(table, &size);
	test(size == HASHTABLE_SIZE_DEFAULT, "hashtable_heap5");

	ptr = PtrStructHashtable(pos);
	test(ptr->test == HASHTABLE_TEST_DEFAULT, "hashtable_heap6");
	test(ptr->count == 0, "hashtable_heap7");
	test(ptr->size == HASHTABLE_SIZE_DEFAULT, "hashtable_heap8");
	test(ptr->resize_float == HASHTABLE_REHASH_SIZE_DEFAULT, "hashtable_heap9");
	test(ptr->threshold == HASHTABLE_REHASH_THRESHOLD_DEFAULT, "hashtable_heap10");
	limit = (size_t)(ptr->size * ptr->threshold);
	test(ptr->limit == limit, "hashtable_heap11");

	RETURN;
}

static int test_hashtable_size_heap(void)
{
	addr pos, table;
	enum HASHTABLE_TEST testvalue;
	size_t value, size;

	hashtable_size_heap(&pos, 100);
	test(GetType(pos) == LISPTYPE_HASHTABLE, "hashtable_size_heap1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "hashtable_size_heap2");
	GetTableHash(pos, &table);
	LenArrayHash(table, &size);
	test(size == 100, "hashtable_size_heap3");

	test(PtrStructHashtable(pos)->size == 100, "hashtable_size_heap4");
	testvalue = HASHTABLE_TEST_EQ;
	gettest_hashtable(pos, &testvalue);
	test(testvalue == HASHTABLE_TEST_DEFAULT, "hashtable_size_heap5");
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	testvalue = HASHTABLE_TEST_EQ;
	gettest_hashtable(pos, &testvalue);
	test(testvalue == HASHTABLE_TEST_EQUAL, "hashtable_size_heap6");
	value = 0;
	getcount_hashtable(pos, &value);
	test(value == 0, "hashtable_size_heap7");
	value = 0;
	getsize_hashtable(pos, &value);
	test(value == 100, "hashtable_size_heap8");

	RETURN;
}

static int check_empty(addr pos)
{
	addr table, check;
	size_t i, size;

	/* size */
	if (PtrStructHashtable(pos)->count) return 0;

	/* table */
	GetTableHash(pos, &table);
	LenArrayHash(table, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &check);
		if (check != Nil) return 0;
	}

	return 1;
}

static void make_dummy_table(addr pos)
{
	addr table;

	GetTableHash(pos, &table);
	SetArrayHash(table, 0, T);
	SetArrayHash(table, 2, T);
	PtrStructHashtable(pos)->count = 2;
}

static int test_clear_hashtable(void)
{
	addr pos;

	hashtable_size_heap(&pos, 100);
	make_dummy_table(pos);
	clear_hashtable_heap(pos);
	test(check_empty(pos), "clear_hashtable1");
	make_dummy_table(pos);
	clear_hashtable(pos);
	test(check_empty(pos), "clear_hashtable2");

	hashtable_size_local(Local_Thread, &pos, 10);
	make_dummy_table(pos);
	clear_hashtable_local(pos);
	test(check_empty(pos), "clear_hashtable3");
	make_dummy_table(pos);
	clear_hashtable(pos);
	test(check_empty(pos), "clear_hashtable4");

	RETURN;
}

static int test_hashindex(void)
{
	addr pos, pos1, pos2;
	size_t size1, size2;

	hashtable_size_heap(&pos, 10000);

	/* string (equal test) */
	strvect_char_heap(&pos1, "Hello1");
	strvect_char_heap(&pos2, "Hello1");

	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	hashindex(pos, pos1, &size1);
	hashindex(pos, pos2, &size2);
	test(size1 == size2, "hashindex1");

	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	hashindex(pos, pos1, &size1);
	hashindex(pos, pos2, &size2);
	test(size1 != size2, "hashindex2");

	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	hashindex(pos, pos1, &size1);
	hashindex(pos, pos2, &size2);
	test(size1 != size2, "hashindex3");

	/* fixnum (eql test) */
	fixnum_heap(&pos1, 112233);
	fixnum_heap(&pos2, 112233);

	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	hashindex(pos, pos1, &size1);
	hashindex(pos, pos2, &size2);
	test(size1 == size2, "hashindex4");

	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	hashindex(pos, pos1, &size1);
	hashindex(pos, pos2, &size2);
	test(size1 == size2, "hashindex5");

	fixnum_heap(&pos1, 10);
	fixnum_heap(&pos2, 10);
	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	hashindex(pos, pos1, &size1);
	hashindex(pos, pos2, &size2);
	test(size1 == size2, "hashindex6");

	RETURN;
}

static void makeunicodechar(addr *ret, const char *str)
{
	addr pos;
	size_t i, size;
	pbyte ptr = (pbyte)str;


	size = strlen(str);
	strvect_heap(&pos, size);
	for (i = 0; i < size; i++)
		strvect_setc(pos, i, (unicode)ptr[i]);
	*ret = pos;
}

static int test_gethashequal(void)
{
	addr pos, pos1, pos2;
	hashequaltype equal;

	hashtable_heap(&pos);

	/* string (equal test) */
	strvect_char_heap(&pos1, "Hello1");
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	gethashequal(pos, &equal);
	strvect_char_heap(&pos2, "Hello1");
	test(equal(pos1, pos2), "gethashequal1");
	makeunicodechar(&pos2, "Hello1");
	test(equal(pos1, pos2), "gethashequal2");

	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	gethashequal(pos, &equal);
	strvect_char_heap(&pos2, "Hello1");
	test(! equal(pos1, pos2), "gethashequal3");
	makeunicodechar(&pos2, "Hello1");
	test(! equal(pos1, pos2), "gethashequal4");

	/* fixnum */
	fixnum_heap(&pos1, 112233);
	fixnum_heap(&pos2, 112233);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	gethashequal(pos, &equal);
	test(equal(pos1, pos2), "gethashequal5");
	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	gethashequal(pos, &equal);
	test(equal(pos1, pos2), "gethashequal6");
	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	gethashequal(pos, &equal);
	test(! equal(pos1, pos2), "gethashequal7");

	RETURN;
}

static int test_insert_rehash(void)
{
	addr array, key, value;

	heap_hashtable(&array, LISPTYPE_VECTOR, 100);
	strvect_char_heap(&key, "key100");
	strvect_char_heap(&value, "value100");
	insert_rehash(NULL, array, 10, key, value);

	getarray(array, 0, &key);
	test(key == Nil, "insert_rehash1");
	getarray(array, 10, &key);
	test(key != Nil, "insert_rehash2");
	test(GetType(key) == LISPTYPE_CONS, "insert_rehash3");
	GetCons(key, &key, &value);
	test(key != Nil, "insert_rehash4");
	test(value == Nil, "insert_rehash5");
	GetCons(key, &key, &value);
	test(GetType(key) == LISPTYPE_STRING, "insert_rehash6");
	test(GetType(value) == LISPTYPE_STRING, "insert_rehash7");
	test(strvect_equal_char(key, "key100"), "insert_rehash8");
	test(! strvect_equal_char(key, "key101"), "insert_rehash9");
	test(strvect_equal_char(value, "value100"), "insert_rehash10");
	test(! strvect_equal_char(value, "value101"), "insert_rehash11");

	strvect_char_heap(&key, "key101");
	strvect_char_heap(&value, "value101");
	insert_rehash(NULL, array, 11, key, value);
	strvect_char_heap(&key, "key102");
	strvect_char_heap(&value, "value102");
	insert_rehash(NULL, array, 22, key, value);
	strvect_char_heap(&key, "key103");
	strvect_char_heap(&value, "value103");
	insert_rehash(NULL, array, 10, key, value);

	getarray(array, 10, &key);
	GetCons(key, &key, &value);
	GetCar(key, &key);
	test(! strvect_equal_char(key, "key100"), "insert_rehash12");
	test(strvect_equal_char(key, "key103"), "insert_rehash13");
	GetCons(value, &key, &value);
	test(key != Nil, "insert_rehash14");
	test(value == Nil, "insert_rehash15");
	GetCons(key, &key, &value);
	test(strvect_equal_char(key, "key100"), "insert_rehash16");
	test(strvect_equal_char(value, "value100"), "insert_rehash17");

	RETURN;
}

static int test_resize_rehash(void)
{
	int check;
	char buffer[100];
	addr table, array, key, value;
	size_t i, size, count;

	hashtable_size_heap(&table, 30);
	settest_hashtable(table, HASHTABLE_TEST_EQUAL);
	GetTableHash(table, &array);
	for (i = 0; i < 1000; i++) {
		snprintf(buffer, 100, "key%d", (int)i);
		strvect_char_heap(&key, buffer);
		snprintf(buffer, 100, "value%d", (int)i);
		strvect_char_heap(&value, buffer);
		insert_rehash(NULL, array, 10, key, value);
	}
	resize_rehash(NULL, table, 15);

	GetTableHash(table, &array);
	lenarray(array, &size);
	test(size == 15, "resize_rehash1");
	count = 0;
	check = 1;
	for (i = 0; i < size; i++) {
		getarray(array, i, &value);
		if (value == Nil) {
			check = 0;
			break;
		}
		while (value != Nil) {
			GetCons(value, &key, &value);
			count++;
		}
	}
	test(check, "resize_rehash2");
	test(count == 1000, "resize_rehash3");

	RETURN;
}

static int test_rehash_execute(void)
{
	addr table;
	size_t limit1, limit2, size1, size2;
	struct StructHashtable *ptr;

	hashtable_heap(&table);
	ptr = PtrStructHashtable(table);
	limit1 = ptr->limit;
	size1 = ptr->size;
	ptr->count = limit1;
	rehash_execute(NULL, table);
	limit2 = ptr->limit;
	size2 = ptr->size;
	test(limit1 < limit2, "rehash_execute1");
	test(size1 < size2, "rehash_execute2");

	limit1 = limit2;
	size1 = size2;
	ptr->count = limit1 - 1;
	rehash_execute(NULL, table);
	limit2 = ptr->limit;
	size2 = ptr->size;
	test(limit1 == limit2, "rehash_execute3");
	test(size1 == size2, "rehash_execute4");

	limit1 = limit2;
	size1 = size2;
	ptr->count = limit1 + 1;
	rehash_execute(NULL, table);
	limit2 = ptr->limit;
	size2 = ptr->size;
	test(limit1 < limit2, "rehash_execute5");
	test(size1 < size2, "rehash_execute6");

	RETURN;
}

static int test_findroot(void)
{
	addr table, array, root, key, value;
	hashequaltype equal;

	hashtable_size_heap(&table, 10);
	GetTableHash(table, &array);
	settest_hashtable(table, HASHTABLE_TEST_EQ);
	gethashequal(table, &equal);

	strvect_char_heap(&key, "key100");
	strvect_char_heap(&value, "value100");
	insert_rehash(NULL, array, 1, key, value);
	strvect_char_heap(&key, "key101");
	strvect_char_heap(&value, "value101");
	insert_rehash(NULL, array, 1, key, value);
	strvect_char_heap(&key, "key102");
	strvect_char_heap(&value, "value102");
	insert_rehash(NULL, array, 1, key, value);
	strvect_char_heap(&key, "key104");
	strvect_char_heap(&value, "value104");
	insert_rehash(NULL, array, 1, key, value);
	getarray(array, 1, &root);

	strvect_char_heap(&key, "aaa");
	test(findroot(equal, root, key, &value), "findroot1");
	strvect_char_heap(&key, "value100");
	test(findroot(equal, root, key, &value), "findroot2");
	strvect_char_heap(&key, "value103");
	test(findroot(equal, root, key, &value), "findroot3");

	settest_hashtable(table, HASHTABLE_TEST_EQUAL);
	gethashequal(table, &equal);
	strvect_char_heap(&key, "aaa");
	test(findroot(equal, root, key, &value), "findroot4");
	strvect_char_heap(&key, "key100");
	test(findroot(equal, root, key, &value) == 0, "findroot5");
	test(GetType(value) == LISPTYPE_CONS, "findroot6");
	GetCons(value, &key, &value);
	test(strvect_equal_char(key, "key100"), "findroot7");
	test(strvect_equal_char(value, "value100"), "findroot8");

	strvect_char_heap(&key, "key104");
	test(findroot(equal, root, key, &value) == 0, "findroot9");
	GetCons(value, &key, &value);
	test(strvect_equal_char(key, "key104"), "findroot10");
	test(strvect_equal_char(value, "value104"), "findroot11");

	RETURN;
}

static int test_appendroot(void)
{
	addr table, array, key, value, root;

	hashtable_size_heap(&table, 100);
	GetTableHash(table, &array);

	strvect_char_heap(&key, "key100");
	strvect_char_heap(&value, "value100");
	insert_rehash(NULL, array, 1, key, value);
	strvect_char_heap(&key, "key101");
	strvect_char_heap(&value, "value101");
	insert_rehash(NULL, array, 1, key, value);
	strvect_char_heap(&key, "key102");
	strvect_char_heap(&value, "value102");
	insert_rehash(NULL, array, 1, key, value);

	GetArrayHash(array, 10, &root);
	test(root == Nil, "appendroot1");
	strvect_char_heap(&key, "key200");
	appendroot(NULL, array, 10, root, key, &value);
	test(GetType(value) == LISPTYPE_CONS, "appendroot2");
	GetCdr(value, &key);
	test(key == Nil, "appendroot3");
	GetCar(value, &key);
	test(strvect_equal_char(key, "key200"), "appendroot4");

	GetArrayHash(array, 10, &root);
	test(root != Nil, "appendroot5");
	strvect_char_heap(&key, "key300");
	appendroot(NULL, array, 10, root, key, &value);
	test(GetType(value) == LISPTYPE_CONS, "appendroot6");
	GetCar(value, &key);
	test(strvect_equal_char(key, "key300"), "appendroot7");

	GetArrayHash(array, 10, &root);
	GetCons(root, &key, &value);
	GetCar(key, &key);
	test(strvect_equal_char(key, "key300"), "appendroot8");
	GetCons(value, &key, &value);
	GetCar(key, &key);
	test(strvect_equal_char(key, "key200"), "appendroot9");
	test(value == Nil, "appendroot10");

	RETURN;
}

static size_t hashcount(addr table)
{
	addr array, value;
	size_t count, size, i;

	GetTableHash(table, &array);
	lenarray(array, &size);
	count = 0;
	for (i = 0; i < size; i++) {
		getarray(array, i, &value);
		while (value != Nil) {
			GetCdr(value, &value);
			count++;
		}
	}

	return count;
}

static int test_intern_hashtable(void)
{
	addr table, key, value, cons;
	size_t i;
	char buffer[100];

	hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQUAL);
	for (i = 0; i < 1000; i++) {
		strvect_char_heap(&key, "keyaaaa");
		snprintf(buffer, 100, "value%d", (int)i);
		strvect_char_heap(&value, buffer);
		intern_hashheap(table, key, &cons);
		SetCdr(cons, value);
	}
	test(hashcount(table) == 1, "intern_hashtable1");

	hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQ);
	for (i = 0; i < 1000; i++) {
		strvect_char_heap(&key, "keyaaaa");
		snprintf(buffer, 100, "value%d", (int)i);
		strvect_char_heap(&value, buffer);
		intern_hashheap(table, key, &cons);
		SetCdr(cons, value);
	}
	test(hashcount(table) == 1000, "intern_hashtable2");

	hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQUAL);
	for (i = 0; i < 1000; i++) {
		snprintf(buffer, 100, "key%d", (int)i);
		strvect_char_heap(&key, buffer);
		snprintf(buffer, 100, "value%d", (int)i);
		strvect_char_heap(&value, buffer);
		intern_hashheap(table, key, &cons);
		SetCdr(cons, value);
	}
	test(hashcount(table) == 1000, "intern_hashtable3");

	/* findcons */
	strvect_char_heap(&key, "key4444");
	findcons_hashtable(table, key, &cons);
	test(cons == Nil, "intern_hashtable4");
	strvect_char_heap(&key, "key444");
	findcons_hashtable(table, key, &cons);
	test(cons != Nil, "intern_hashtable5");
	GetCons(cons, &key, &value);
	test(strvect_equal_char(key, "key444"), "intern_hashtable6");
	test(strvect_equal_char(value, "value444"), "intern_hashtable7");

	/* findvalue */
	strvect_char_heap(&key, "key5555");
	test(findvalue_hashtable(table, key, &value) == 1, "intern_hashtable8");
	strvect_char_heap(&key, "key555");
	test(findvalue_hashtable(table, key, &value) == 0, "intern_hashtable9");
	test(strvect_equal_char(value, "value555"), "intern_hashtable10");

	RETURN;
}

static int test_delete_hashtable(void)
{
	int error;
	addr table, key, value, cons;
	size_t i, count, check;
	char buffer[100];

	hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQUAL);
	for (i = 0; i < 1000; i++) {
		snprintf(buffer, 100, "key%d", (int)i);
		strvect_char_heap(&key, buffer);
		snprintf(buffer, 100, "value%d", (int)i);
		strvect_char_heap(&value, buffer);
		intern_hashheap(table, key, &cons);
		SetCdr(cons, value);
	}
	test(hashcount(table) == 1000, "delete_hashtable1");

	error = 1;
	for (i = 0; i < 1000; i++) {
		check = 1000 - i;
		getcount_hashtable(table, &count);
		if (check != count || check != hashcount(table)) {
			error = 0;
			break;
		}

		snprintf(buffer, 100, "key%d", (int)i);
		strvect_char_heap(&key, buffer);
		delete_hashtable(table, key);
	}
	test(error, "delete_hashtable2");

	RETURN;
}


/*
 *  main
 */
static int testbreak_hashtable(void)
{
	TestBreak(test_hashtable_heap);
	TestBreak(test_hashtable_size_heap);
	TestBreak(test_clear_hashtable);
	TestBreak(test_hashindex);
	TestBreak(test_gethashequal);
	TestBreak(test_insert_rehash);
	TestBreak(test_resize_rehash);
	TestBreak(test_rehash_execute);
	TestBreak(test_findroot);
	TestBreak(test_appendroot);
	TestBreak(test_intern_hashtable);
	TestBreak(test_delete_hashtable);

	return 0;
}

int test_hashtable(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	ptr = Execute_Thread;
	lisp_info_enable = 1;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		lisp_initialize = 1;
		result = testbreak_hashtable();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

