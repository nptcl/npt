#include "hashtable.c"
#include "constant.h"
#include "degrade.h"
#include "execute.h"
#include "object.h"
#include "strtype.h"
#include "strvect.h"

/*
 *  hashtable testcase
 */
static int test_hashtable_heap(void)
{
	addr pos, table;
	size_t limit, size;
	struct StructHashtable *ptr;

	hashtable_heap(&pos);
	test(GetType(pos) == LISPTYPE_HASHTABLE, "hashtable_heap.1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "hashtable_heap.2");
	GetTableHash(pos, &table);
	test(GetType(table) == LISPTYPE_VECTOR, "hashtable_heap.3");
#ifdef LISP_ARCH_32BIT
	test(GetStatusSize(table) == LISPSIZE_ARRAY4, "hashtable_heap.4");
#else
	test(GetStatusSize(table) == LISPSIZE_ARRAY8, "hashtable_heap.4");
#endif
	LenArrayHash(table, &size);
	test(size == HASHTABLE_SIZE_DEFAULT, "hashtable_heap.5");

	ptr = PtrStructHashtable(pos);
	test(ptr->test == HASHTABLE_TEST_DEFAULT, "hashtable_heap.6");
	test(ptr->count == 0, "hashtable_heap.7");
	test(ptr->size == HASHTABLE_SIZE_DEFAULT, "hashtable_heap.8");
	test(ptr->resize_float == HASHTABLE_REHASH_SIZE_DEFAULT, "hashtable_heap.9");
	test(ptr->threshold == HASHTABLE_REHASH_THRESHOLD_DEFAULT, "hashtable_heap.10");
	limit = (size_t)(ptr->size * ptr->threshold);
	test(ptr->limit == limit, "hashtable_heap.11");

	RETURN;
}

static int test_hashtable_size_heap(void)
{
	addr pos, table;
	enum HASHTABLE_TEST testvalue;
	size_t value, size;

	hashtable_size_heap(&pos, 100);
	test(GetType(pos) == LISPTYPE_HASHTABLE, "hashtable_size_heap.1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "hashtable_size_heap.2");
	GetTableHash(pos, &table);
	LenArrayHash(table, &size);
	test(size == 100, "hashtable_size_heap.3");

	test(PtrStructHashtable(pos)->size == 100, "hashtable_size_heap.4");
	testvalue = HASHTABLE_TEST_EQ;
	gettest_hashtable(pos, &testvalue);
	test(testvalue == HASHTABLE_TEST_DEFAULT, "hashtable_size_heap.5");
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	testvalue = HASHTABLE_TEST_EQ;
	gettest_hashtable(pos, &testvalue);
	test(testvalue == HASHTABLE_TEST_EQUAL, "hashtable_size_heap.6");
	value = 0;
	getcount_hashtable(pos, &value);
	test(value == 0, "hashtable_size_heap.7");
	value = 0;
	getsize_hashtable(pos, &value);
	test(value == 100, "hashtable_size_heap.8");

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
	test(check_empty(pos), "clear_hashtable.1");
	make_dummy_table(pos);
	clear_hashtable(pos);
	test(check_empty(pos), "clear_hashtable.2");

	hashtable_size_local(Local_Thread, &pos, 10);
	make_dummy_table(pos);
	clear_hashtable_local(pos);
	test(check_empty(pos), "clear_hashtable.3");
	make_dummy_table(pos);
	clear_hashtable(pos);
	test(check_empty(pos), "clear_hashtable.4");

	RETURN;
}

static int test_call_hashindex(void)
{
	addr pos, pos1, pos2;
	size_t size1, size2;

	hashtable_size_heap(&pos, 10000);

	/* string (equal test) */
	strvect_char_heap(&pos1, "Hello1");
	strvect_char_heap(&pos2, "Hello1");

	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	call_hashindex_(pos, pos1, &size1);
	call_hashindex_(pos, pos2, &size2);
	test(size1 == size2, "call_hashindex.1");

	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	call_hashindex_(pos, pos1, &size1);
	call_hashindex_(pos, pos2, &size2);
	test(size1 != size2, "call_hashindex.2");

	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	call_hashindex_(pos, pos1, &size1);
	call_hashindex_(pos, pos2, &size2);
	test(size1 != size2, "call_hashindex.3");

	/* fixnum (eql test) */
	fixnum_heap(&pos1, 112233);
	fixnum_heap(&pos2, 112233);

	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	call_hashindex_(pos, pos1, &size1);
	call_hashindex_(pos, pos2, &size2);
	test(size1 == size2, "call_hashindex.4");

	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	call_hashindex_(pos, pos1, &size1);
	call_hashindex_(pos, pos2, &size2);
	test(size1 == size2, "call_hashindex.5");

	fixnum_heap(&pos1, 10);
	fixnum_heap(&pos2, 10);
	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	call_hashindex_(pos, pos1, &size1);
	call_hashindex_(pos, pos2, &size2);
	test(size1 == size2, "call_hashindex.6");

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

static int test_gethashequal_call(int (*call)(addr, addr, int *), addr x, addr y)
{
	int check;
	Error((*call)(x, y, &check));
	return check;
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
	test(test_gethashequal_call(equal, pos1, pos2), "gethashequal.1");
	makeunicodechar(&pos2, "Hello1");
	test(test_gethashequal_call(equal, pos1, pos2), "gethashequal.2");

	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	gethashequal(pos, &equal);
	strvect_char_heap(&pos2, "Hello1");
	test(! test_gethashequal_call(equal, pos1, pos2), "gethashequal.3");
	makeunicodechar(&pos2, "Hello1");
	test(! test_gethashequal_call(equal, pos1, pos2), "gethashequal.4");

	/* fixnum */
	fixnum_heap(&pos1, 112233);
	fixnum_heap(&pos2, 112233);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	gethashequal(pos, &equal);
	test(test_gethashequal_call(equal, pos1, pos2), "gethashequal.5");
	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	gethashequal(pos, &equal);
	test(test_gethashequal_call(equal, pos1, pos2), "gethashequal.6");
	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	gethashequal(pos, &equal);
	test(! test_gethashequal_call(equal, pos1, pos2), "gethashequal.7");

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
	test(key == Nil, "insert_rehash.1");
	getarray(array, 10, &key);
	test(key != Nil, "insert_rehash.2");
	test(GetType(key) == LISPTYPE_CONS, "insert_rehash.3");
	GetCons(key, &key, &value);
	test(key != Nil, "insert_rehash.4");
	test(value == Nil, "insert_rehash.5");
	GetCons(key, &key, &value);
	test(GetType(key) == LISPTYPE_STRING, "insert_rehash.6");
	test(GetType(value) == LISPTYPE_STRING, "insert_rehash.7");
	test(strvect_equal_char(key, "key100"), "insert_rehash.8");
	test(! strvect_equal_char(key, "key101"), "insert_rehash.9");
	test(strvect_equal_char(value, "value100"), "insert_rehash.10");
	test(! strvect_equal_char(value, "value101"), "insert_rehash.11");

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
	test(! strvect_equal_char(key, "key100"), "insert_rehash.12");
	test(strvect_equal_char(key, "key103"), "insert_rehash.13");
	GetCons(value, &key, &value);
	test(key != Nil, "insert_rehash.14");
	test(value == Nil, "insert_rehash.15");
	GetCons(key, &key, &value);
	test(strvect_equal_char(key, "key100"), "insert_rehash.16");
	test(strvect_equal_char(value, "value100"), "insert_rehash.17");

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
	resize_rehash_(NULL, table, 15);

	GetTableHash(table, &array);
	lenarray(array, &size);
	test(size == 15, "resize_rehash.1");
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
	test(check, "resize_rehash.2");
	test(count == 1000, "resize_rehash.3");

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
	rehash_execute_(NULL, table);
	limit2 = ptr->limit;
	size2 = ptr->size;
	test(limit1 < limit2, "rehash_execute.1");
	test(size1 < size2, "rehash_execute.2");

	limit1 = limit2;
	size1 = size2;
	ptr->count = limit1 - 1;
	rehash_execute_(NULL, table);
	limit2 = ptr->limit;
	size2 = ptr->size;
	test(limit1 == limit2, "rehash_execute.3");
	test(size1 == size2, "rehash_execute.4");

	limit1 = limit2;
	size1 = size2;
	ptr->count = limit1 + 1;
	rehash_execute_(NULL, table);
	limit2 = ptr->limit;
	size2 = ptr->size;
	test(limit1 < limit2, "rehash_execute.5");
	test(size1 < size2, "rehash_execute.6");

	RETURN;
}

static int test_findroot_call(hashequaltype equal, addr root, addr key, addr *ret)
{
	int check;
	check = 0;
	Error(findroot_hashtable_(equal, root, key, ret, &check));
	return check;
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
	test(! test_findroot_call(equal, root, key, &value), "findroot.1");
	strvect_char_heap(&key, "value100");
	test(! test_findroot_call(equal, root, key, &value), "findroot.2");
	strvect_char_heap(&key, "value103");
	test(! test_findroot_call(equal, root, key, &value), "findroot.3");

	settest_hashtable(table, HASHTABLE_TEST_EQUAL);
	gethashequal(table, &equal);
	strvect_char_heap(&key, "aaa");
	test(! test_findroot_call(equal, root, key, &value), "findroot.4");
	strvect_char_heap(&key, "key100");
	test(test_findroot_call(equal, root, key, &value), "findroot.5");
	test(GetType(value) == LISPTYPE_CONS, "findroot.6");
	GetCons(value, &key, &value);
	test(strvect_equal_char(key, "key100"), "findroot.7");
	test(strvect_equal_char(value, "value100"), "findroot.8");

	strvect_char_heap(&key, "key104");
	test(test_findroot_call(equal, root, key, &value), "findroot.9");
	GetCons(value, &key, &value);
	test(strvect_equal_char(key, "key104"), "findroot.10");
	test(strvect_equal_char(value, "value104"), "findroot.11");

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
	appendroot_hashtable(NULL, array, 10, root, key, &value);
	test(GetType(value) == LISPTYPE_CONS, "appendroot.2");
	GetCdr(value, &key);
	test(key == Nil, "appendroot.3");
	GetCar(value, &key);
	test(strvect_equal_char(key, "key200"), "appendroot.4");

	GetArrayHash(array, 10, &root);
	test(root != Nil, "appendroot.5");
	strvect_char_heap(&key, "key300");
	appendroot_hashtable(NULL, array, 10, root, key, &value);
	test(GetType(value) == LISPTYPE_CONS, "appendroot.6");
	GetCar(value, &key);
	test(strvect_equal_char(key, "key300"), "appendroot.7");

	GetArrayHash(array, 10, &root);
	GetCons(root, &key, &value);
	GetCar(key, &key);
	test(strvect_equal_char(key, "key300"), "appendroot.8");
	GetCons(value, &key, &value);
	GetCar(key, &key);
	test(strvect_equal_char(key, "key200"), "appendroot.9");
	test(value == Nil, "appendroot.10");

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
		intern_hashheap_(table, key, &cons);
		SetCdr(cons, value);
	}
	test(hashcount(table) == 1, "intern_hashtable.1");

	hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQ);
	for (i = 0; i < 1000; i++) {
		strvect_char_heap(&key, "keyaaaa");
		snprintf(buffer, 100, "value%d", (int)i);
		strvect_char_heap(&value, buffer);
		intern_hashheap_(table, key, &cons);
		SetCdr(cons, value);
	}
	test(hashcount(table) == 1000, "intern_hashtable.2");

	hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQUAL);
	for (i = 0; i < 1000; i++) {
		snprintf(buffer, 100, "key%d", (int)i);
		strvect_char_heap(&key, buffer);
		snprintf(buffer, 100, "value%d", (int)i);
		strvect_char_heap(&value, buffer);
		intern_hashheap_(table, key, &cons);
		SetCdr(cons, value);
	}
	test(hashcount(table) == 1000, "intern_hashtable.3");

	/* findcons */
	strvect_char_heap(&key, "key4444");
	findcons_hashtable_(table, key, &cons);
	test(cons == Nil, "intern_hashtable.4");
	strvect_char_heap(&key, "key444");
	findcons_hashtable_(table, key, &cons);
	test(cons != Nil, "intern_hashtable.5");
	GetCons(cons, &key, &value);
	test(strvect_equal_char(key, "key444"), "intern_hashtable.6");
	test(strvect_equal_char(value, "value444"), "intern_hashtable.7");

	/* findvalue */
	strvect_char_heap(&key, "key5555");
	test(findnil_hashtable_debug(table, key, &value) == 0, "intern_hashtable.8");
	strvect_char_heap(&key, "key555");
	test(findnil_hashtable_debug(table, key, &value) == 1, "intern_hashtable.9");
	test(strvect_equal_char(value, "value555"), "intern_hashtable.10");

	RETURN;
}

static int test_delete_hashtable(void)
{
	int error, check;
	addr table, key, value, cons;
	size_t i, count, size;
	char buffer[100];

	hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQUAL);
	for (i = 0; i < 1000; i++) {
		snprintf(buffer, 100, "key%d", (int)i);
		strvect_char_heap(&key, buffer);
		snprintf(buffer, 100, "value%d", (int)i);
		strvect_char_heap(&value, buffer);
		intern_hashheap_(table, key, &cons);
		SetCdr(cons, value);
	}
	test(hashcount(table) == 1000, "delete_hashtable.1");

	error = 1;
	for (i = 0; i < 1000; i++) {
		size = 1000 - i;
		getcount_hashtable(table, &count);
		if (size != count || size != hashcount(table)) {
			error = 0;
			break;
		}

		snprintf(buffer, 100, "key%d", (int)i);
		strvect_char_heap(&key, buffer);
		delete_hashtable_(table, key, &check);
	}
	test(error, "delete_hashtable.2");

	RETURN;
}


/*
 *  hashtable
 */
static int testcase_hashtable(void)
{
	TestBreak(test_hashtable_heap);
	TestBreak(test_hashtable_size_heap);
	TestBreak(test_clear_hashtable);
	TestBreak(test_call_hashindex);
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

static void testinit_hashtable(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
}

int test_hashtable(void)
{
	TITLE;
	return degrade_code(
			testinit_hashtable,
			testcase_hashtable);
}

