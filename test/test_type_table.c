#include "type_table.c"
#include "common.h"
#include "condition.h"
#include "cons.h"
#include "degrade.h"
#include "package.h"

/*
 *  interface
 */
static int test_gettypetable(void)
{
	addr pos, check;

	type0_heap(LISPDECL_SYMBOL, &pos);
	settypetable(TypeTable_Debug1, pos);
	check = reftypetable(TypeTable_Debug1);
	test(pos == check, "gettypetable1");

	check = 0;
	gettypetable(TypeTable_Debug1, &check);
	test(pos == check, "gettypetable2");

	type0_heap(LISPDECL_LIST, &pos);
	settypetable(TypeTable_Debug2, pos);
	gettypetable(TypeTable_Debug2, &check);
	test(pos == check, "gettypetable3");

	RETURN;
}

static int test_GetTypeTable(void)
{
	addr pos, pos1, pos2, pos3, pos4;

	type0_heap(LISPDECL_SYMBOL, &pos1);
	type0_heap(LISPDECL_LIST, &pos2);
	type0_heap(LISPDECL_NIL, &pos3);
	type0_heap(LISPDECL_ATOM, &pos4);
	settypetable(TypeTable_Debug2, pos1);
	settypetable(TypeArgs_Debug2, pos2);
	settypetable(TypeValues_Debug2, pos3);
	settypetable(TypeCompiled_Debug2, pos4);

	GetTypeTable(&pos, Debug2);
	test(pos = pos1, "GetTypeTable1");
	GetTypeArgs(&pos, Debug2);
	test(pos = pos2, "GetTypeTable2");
	GetTypeValues(&pos, Debug2);
	test(pos = pos3, "GetTypeTable3");
	GetTypeCompiled(&pos, Debug2);
	test(pos = pos4, "GetTypeTable4");

	SetTypeTable(Debug3, pos4);
	SetTypeArgs(Debug3, pos3);
	SetTypeValues(Debug3, pos2);
	SetTypeCompiled(Debug3, pos1);
	test(reftypetable(TypeTable_Debug3) == pos4, "GetTypeTable5");
	test(reftypetable(TypeArgs_Debug3) == pos3, "GetTypeTable6");
	test(reftypetable(TypeValues_Debug3) == pos2, "GetTypeTable7");
	test(reftypetable(TypeCompiled_Debug3) == pos1, "GetTypeTable8");

	RETURN;
}

static int test_KeyTypeTable(void)
{
	addr pos1, pos2, check1, check2;

	type0_heap(LISPDECL_CONS, &pos1);
	settypetable(TypeArgs_Debug1, pos1);
	settypetable(TypeValues_Debug1, pos1);
	settypetable(TypeCompiled_Debug1, pos1);

	GetConst(KEYWORD_START, &pos1);
	type0_heap(LISPDECL_SYMBOL, &pos2);
	settypetable(TypeTable_Debug1, pos2);
	keytypetable(CONSTANT_KEYWORD_START, TypeTable_Debug1, &check1);
	test(consp(check1), "KeyTypeTable1");
	GetCons(check1, &check1, &check2);
	test(check1 == pos1, "KeyTypeTable2");
	test(check2 == pos2, "KeyTypeTable3");

	check1 = check2 = 0;
	KeyTypeTable(&check1, START, Debug1);
	test(consp(check1), "KeyTypeTable4");
	GetCons(check1, &check1, &check2);
	test(check1 == pos1, "KeyTypeTable5");
	test(check2 == pos2, "KeyTypeTable6");

	RETURN;
}


/*
 *  arguments
 */
static int test_typeargs_empty(void)
{
	addr pos;

	typeargs_empty(&pos);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_empty1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_empty2");
	test(lenarrayr(pos) == 4, "typeargs_empty3");
	test(refarray(pos, 0) == Nil, "typeargs_empty4");
	test(refarray(pos, 1) == Nil, "typeargs_empty5");
	test(refarray(pos, 2) == Nil, "typeargs_empty6");
	test(refarray(pos, 3) == Nil, "typeargs_empty7");

	RETURN;
}

static int test_typeargs_full(void)
{
	addr pos, v1, v2, v3, v4;

	cons_heap(&v1, Nil, T);
	cons_heap(&v2, T, Nil);
	type0_heap(LISPDECL_NIL, &v3);
	cons_heap(&v4, T, T);
	typeargs_full(&pos, v1, v2, v3, v4);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_full1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_full2");
	test(lenarrayr(pos) == 4, "typeargs_full3");
	test(refarray(pos, 0) == v1, "typeargs_full4");
	test(refarray(pos, 1) == v2, "typeargs_full5");
	test(refarray(pos, 2) == v3, "typeargs_full6");
	test(refarray(pos, 3) == v4, "typeargs_full7");

	RETURN;
}

static int test_typeargs_var1(void)
{
	addr pos, v1, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	typeargs_var1(&pos, v1);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var1-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var1-2");
	test(lenarrayr(pos) == 4, "typeargs_var1-3");
	test(refarray(pos, 1) == Nil, "typeargs_var1-5");
	test(refarray(pos, 2) == Nil, "typeargs_var1-6");
	test(refarray(pos, 3) == Nil, "typeargs_var1-7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var1-8");
	test(list == Nil, "typeargs_var1-9");

	RETURN;
}

static int test_typeargs_var2(void)
{
	addr pos, v1, v2, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	typeargs_var2(&pos, v1, v2);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var2-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var2-2");
	test(lenarrayr(pos) == 4, "typeargs_var2-3");
	test(refarray(pos, 1) == Nil, "typeargs_var2-5");
	test(refarray(pos, 2) == Nil, "typeargs_var2-6");
	test(refarray(pos, 3) == Nil, "typeargs_var2-7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var2-8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var2-9");
	test(list == Nil, "typeargs_var2-10");

	RETURN;
}

static int test_typeargs_var3(void)
{
	addr pos, v1, v2, v3, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	typeargs_var3(&pos, v1, v2, v3);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var3-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var3-2");
	test(lenarrayr(pos) == 4, "typeargs_var3-3");
	test(refarray(pos, 1) == Nil, "typeargs_var3-5");
	test(refarray(pos, 2) == Nil, "typeargs_var3-6");
	test(refarray(pos, 3) == Nil, "typeargs_var3-7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var3-8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var3-9");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var3-10");
	test(list == Nil, "typeargs_var3-11");

	RETURN;
}

static int test_typeargs_var4(void)
{
	addr pos, v1, v2, v3, v4, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	type0_heap(LISPDECL_CHARACTER, &v4);
	typeargs_var4(&pos, v1, v2, v3, v4);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var4-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var4-2");
	test(lenarrayr(pos) == 4, "typeargs_var4-3");
	test(refarray(pos, 1) == Nil, "typeargs_var4-5");
	test(refarray(pos, 2) == Nil, "typeargs_var4-6");
	test(refarray(pos, 3) == Nil, "typeargs_var4-7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var4-8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var4-9");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var4-10");
	getcons_(list, &check, &list);
	test(check == v4, "typeargs_var4-11");
	test(list == Nil, "typeargs_var4-12");

	RETURN;
}

static int test_typeargs_var5(void)
{
	addr pos, v1, v2, v3, v4, v5, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	type0_heap(LISPDECL_CHARACTER, &v4);
	type0_heap(LISPDECL_FIXNUM, &v5);
	typeargs_var5(&pos, v1, v2, v3, v4, v5);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var5-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var5-2");
	test(lenarrayr(pos) == 4, "typeargs_var5-3");
	test(refarray(pos, 1) == Nil, "typeargs_var5-5");
	test(refarray(pos, 2) == Nil, "typeargs_var5-6");
	test(refarray(pos, 3) == Nil, "typeargs_var5-7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var5-8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var5-9");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var5-10");
	getcons_(list, &check, &list);
	test(check == v4, "typeargs_var5-11");
	getcons_(list, &check, &list);
	test(check == v5, "typeargs_var5-12");
	test(list == Nil, "typeargs_var5-13");

	RETURN;
}

static int test_typeargs_var1key(void)
{
	addr pos, v1, key, list, check;

	cons_heap(&key, Nil, Nil);
	type0_heap(LISPDECL_LIST, &v1);
	typeargs_var1key(&pos, v1, key);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var1key1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var1key2");
	test(lenarrayr(pos) == 4, "typeargs_var1key3");
	test(refarray(pos, 1) == Nil, "typeargs_var1key5");
	test(refarray(pos, 2) == Nil, "typeargs_var1key6");
	test(refarray(pos, 3) == key, "typeargs_var1key6");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var1key8");
	test(list == Nil, "typeargs_var1key9");

	RETURN;
}

static int test_typeargs_var2key(void)
{
	addr pos, v1, v2, key, list, check;

	cons_heap(&key, Nil, Nil);
	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	typeargs_var2key(&pos, v1, v2, key);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var2key1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var2key2");
	test(lenarrayr(pos) == 4, "typeargs_var2key3");
	test(refarray(pos, 1) == Nil, "typeargs_var2key5");
	test(refarray(pos, 2) == Nil, "typeargs_var2key6");
	test(refarray(pos, 3) == key, "typeargs_var2key6");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var2key8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var2key9");
	test(list == Nil, "typeargs_var2key10");

	RETURN;
}

static int test_typeargs_var3key(void)
{
	addr pos, v1, v2, v3, key, list, check;

	cons_heap(&key, Nil, Nil);
	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	typeargs_var3key(&pos, v1, v2, v3, key);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var3key1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var3key2");
	test(lenarrayr(pos) == 4, "typeargs_var3key3");
	test(refarray(pos, 1) == Nil, "typeargs_var3key5");
	test(refarray(pos, 2) == Nil, "typeargs_var3key6");
	test(refarray(pos, 3) == key, "typeargs_var3key6");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var3key8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var3key9");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var3key10");
	test(list == Nil, "typeargs_var3key11");

	RETURN;
}

static int test_typeargs_var4key(void)
{
	addr pos, v1, v2, v3, v4, key, list, check;

	cons_heap(&key, Nil, Nil);
	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	type0_heap(LISPDECL_CHARACTER, &v4);
	typeargs_var4key(&pos, v1, v2, v3, v4, key);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var4key1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var4key2");
	test(lenarrayr(pos) == 4, "typeargs_var4key3");
	test(refarray(pos, 1) == Nil, "typeargs_var4key5");
	test(refarray(pos, 2) == Nil, "typeargs_var4key6");
	test(refarray(pos, 3) == key, "typeargs_var4key6");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var4key8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var4key9");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var4key10");
	getcons_(list, &check, &list);
	test(check == v4, "typeargs_var4key11");
	test(list == Nil, "typeargs_var4key12");

	RETURN;
}

static int test_typeargs_opt1(void)
{
	addr pos, v1, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	typeargs_opt1(&pos, v1);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_opt1-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_opt1-2");
	test(lenarrayr(pos) == 4, "typeargs_opt1-3");
	test(refarray(pos, 0) == Nil, "typeargs_opt1-5");
	test(refarray(pos, 2) == Nil, "typeargs_opt1-6");
	test(refarray(pos, 3) == Nil, "typeargs_opt1-7");

	getarray(pos, 1, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_opt1-8");
	test(list == Nil, "typeargs_opt1-9");

	RETURN;
}

static int test_typeargs_opt2(void)
{
	addr pos, v1, v2, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	typeargs_opt2(&pos, v1, v2);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_opt2-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_opt2-2");
	test(lenarrayr(pos) == 4, "typeargs_opt2-3");
	test(refarray(pos, 0) == Nil, "typeargs_opt2-5");
	test(refarray(pos, 2) == Nil, "typeargs_opt2-6");
	test(refarray(pos, 3) == Nil, "typeargs_opt2-7");

	getarray(pos, 1, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_opt2-8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_opt2-9");
	test(list == Nil, "typeargs_opt2-10");

	RETURN;
}

static int test_typeargs_opt3(void)
{
	addr pos, v1, v2, v3, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	typeargs_opt3(&pos, v1, v2, v3);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_opt3-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_opt3-2");
	test(lenarrayr(pos) == 4, "typeargs_opt3-3");
	test(refarray(pos, 0) == Nil, "typeargs_opt3-5");
	test(refarray(pos, 2) == Nil, "typeargs_opt3-6");
	test(refarray(pos, 3) == Nil, "typeargs_opt3-7");

	getarray(pos, 1, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_opt3-8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_opt3-9");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_opt3-10");
	test(list == Nil, "typeargs_opt3-11");

	RETURN;
}

static int test_typeargs_opt4(void)
{
	addr pos, v1, v2, v3, v4, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	type0_heap(LISPDECL_CHARACTER, &v4);
	typeargs_opt4(&pos, v1, v2, v3, v4);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_opt4-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_opt4-2");
	test(lenarrayr(pos) == 4, "typeargs_opt4-3");
	test(refarray(pos, 0) == Nil, "typeargs_opt4-5");
	test(refarray(pos, 2) == Nil, "typeargs_opt4-6");
	test(refarray(pos, 3) == Nil, "typeargs_opt4-7");

	getarray(pos, 1, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_opt4-8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_opt4-9");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_opt4-10");
	getcons_(list, &check, &list);
	test(check == v4, "typeargs_opt4-11");
	test(list == Nil, "typeargs_opt4-12");

	RETURN;
}

static int test_typeargs_opt5(void)
{
	addr pos, v1, v2, v3, v4, v5, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	type0_heap(LISPDECL_CHARACTER, &v4);
	type0_heap(LISPDECL_FIXNUM, &v5);
	typeargs_opt5(&pos, v1, v2, v3, v4, v5);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_opt5-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_opt5-2");
	test(lenarrayr(pos) == 4, "typeargs_opt5-3");
	test(refarray(pos, 0) == Nil, "typeargs_opt5-5");
	test(refarray(pos, 2) == Nil, "typeargs_opt5-6");
	test(refarray(pos, 3) == Nil, "typeargs_opt5-7");

	getarray(pos, 1, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_opt5-8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_opt5-9");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_opt5-10");
	getcons_(list, &check, &list);
	test(check == v4, "typeargs_opt5-11");
	getcons_(list, &check, &list);
	test(check == v5, "typeargs_opt5-12");
	test(list == Nil, "typeargs_opt5-13");

	RETURN;
}

static int test_typeargs_var1opt1(void)
{
	addr pos, v1, v2, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	typeargs_var1opt1(&pos, v1, v2);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var1opt1-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var1opt1-2");
	test(lenarrayr(pos) == 4, "typeargs_var1opt1-3");
	test(refarray(pos, 2) == Nil, "typeargs_var1opt1-6");
	test(refarray(pos, 3) == Nil, "typeargs_var1opt1-7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var1opt1-8");
	test(list == Nil, "typeargs_var1opt1-9");

	getarray(pos, 1, &list);
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var1opt1-10");
	test(list == Nil, "typeargs_var1opt1-11");

	RETURN;
}

static int test_typeargs_var1opt2(void)
{
	addr pos, v1, v2, v3, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	typeargs_var1opt2(&pos, v1, v2, v3);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var1opt2-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var1opt2-2");
	test(lenarrayr(pos) == 4, "typeargs_var1opt2-3");
	test(refarray(pos, 2) == Nil, "typeargs_var1opt2-6");
	test(refarray(pos, 3) == Nil, "typeargs_var1opt2-7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var1opt2-8");
	test(list == Nil, "typeargs_var1opt2-9");

	getarray(pos, 1, &list);
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var1opt2-10");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var1opt2-11");
	test(list == Nil, "typeargs_var1opt2-12");

	RETURN;
}

static int test_typeargs_var1opt2key(void)
{
	addr pos, v1, v2, v3, key, list, check;

	cons_heap(&key, T, Nil);
	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	typeargs_var1opt2key(&pos, v1, v2, v3, key);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var1opt2key1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var1opt2key2");
	test(lenarrayr(pos) == 4, "typeargs_var1opt2key3");
	test(refarray(pos, 2) == Nil, "typeargs_var1opt2key6");
	test(refarray(pos, 3) == key, "typeargs_var1opt2key7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var1opt2key8");
	test(list == Nil, "typeargs_var1opt2key9");

	getarray(pos, 1, &list);
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var1opt2key10");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var1opt2key11");
	test(list == Nil, "typeargs_var1opt2key12");

	RETURN;
}

static int test_typeargs_var2opt1(void)
{
	addr pos, v1, v2, v3, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	typeargs_var2opt1(&pos, v1, v2, v3);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var2opt1-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var2opt1-2");
	test(lenarrayr(pos) == 4, "typeargs_var2opt1-3");
	test(refarray(pos, 2) == Nil, "typeargs_var2opt1-6");
	test(refarray(pos, 3) == Nil, "typeargs_var2opt1-7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var2opt1-8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var2opt1-9");
	test(list == Nil, "typeargs_var2opt1-10");

	getarray(pos, 1, &list);
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var2opt1-11");
	test(list == Nil, "typeargs_var2opt1-12");

	RETURN;
}

static int test_typeargs_var2opt2(void)
{
	addr pos, v1, v2, v3, v4, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	type0_heap(LISPDECL_CHARACTER, &v4);
	typeargs_var2opt2(&pos, v1, v2, v3, v4);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var2opt2-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var2opt2-2");
	test(lenarrayr(pos) == 4, "typeargs_var2opt2-3");
	test(refarray(pos, 2) == Nil, "typeargs_var2opt2-6");
	test(refarray(pos, 3) == Nil, "typeargs_var2opt2-7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var2opt2-8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var2opt2-9");
	test(list == Nil, "typeargs_var2opt2-10");

	getarray(pos, 1, &list);
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var2opt2-11");
	getcons_(list, &check, &list);
	test(check == v4, "typeargs_var2opt2-12");
	test(list == Nil, "typeargs_var2opt2-13");

	RETURN;
}

static int test_typeargs_var3opt1(void)
{
	addr pos, v1, v2, v3, v4, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	type0_heap(LISPDECL_CHARACTER, &v4);
	typeargs_var3opt1(&pos, v1, v2, v3, v4);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var3opt1-1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var3opt1-2");
	test(lenarrayr(pos) == 4, "typeargs_var3opt1-3");
	test(refarray(pos, 2) == Nil, "typeargs_var3opt1-6");
	test(refarray(pos, 3) == Nil, "typeargs_var3opt1-7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var3opt1-8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var3opt1-9");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var3opt1-10");
	test(list == Nil, "typeargs_var3opt1-11");

	getarray(pos, 1, &list);
	getcons_(list, &check, &list);
	test(check == v4, "typeargs_var3opt1-12");
	test(list == Nil, "typeargs_var3opt1-13");

	RETURN;
}

static int test_typeargs_var1rest(void)
{
	addr pos, v1, rest, list, check;

	type0_heap(LISPDECL_ATOM, &rest);
	type0_heap(LISPDECL_LIST, &v1);
	typeargs_var1rest(&pos, v1, rest);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var1rest1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var1rest2");
	test(lenarrayr(pos) == 4, "typeargs_var1rest3");
	test(refarray(pos, 1) == Nil, "typeargs_var1rest5");
	test(refarray(pos, 2) == rest, "typeargs_var1rest6");
	test(refarray(pos, 3) == Nil, "typeargs_var1rest7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var1rest8");
	test(list == Nil, "typeargs_var1rest9");

	RETURN;
}

static int test_typeargs_var2rest(void)
{
	addr pos, v1, v2, rest, list, check;

	type0_heap(LISPDECL_ATOM, &rest);
	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	typeargs_var2rest(&pos, v1, v2, rest);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var2rest1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var2rest2");
	test(lenarrayr(pos) == 4, "typeargs_var2rest3");
	test(refarray(pos, 1) == Nil, "typeargs_var2rest5");
	test(refarray(pos, 2) == rest, "typeargs_var2rest6");
	test(refarray(pos, 3) == Nil, "typeargs_var2rest7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var2rest8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var2rest9");
	test(list == Nil, "typeargs_var2rest10");

	RETURN;
}

static int test_typeargs_var3rest(void)
{
	addr pos, v1, v2, v3, rest, list, check;

	type0_heap(LISPDECL_ATOM, &rest);
	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	typeargs_var3rest(&pos, v1, v2, v3, rest);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var3rest1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var3rest2");
	test(lenarrayr(pos) == 4, "typeargs_var3rest3");
	test(refarray(pos, 1) == Nil, "typeargs_var3rest5");
	test(refarray(pos, 2) == rest, "typeargs_var3rest6");
	test(refarray(pos, 3) == Nil, "typeargs_var3rest7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var3rest8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var3rest9");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var3rest10");
	test(list == Nil, "typeargs_var3rest11");

	RETURN;
}

static int test_typeargs_var4rest(void)
{
	addr pos, v1, v2, v3, v4, rest, list, check;

	type0_heap(LISPDECL_ATOM, &rest);
	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	type0_heap(LISPDECL_CHARACTER, &v4);
	typeargs_var4rest(&pos, v1, v2, v3, v4, rest);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_var4rest1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_var4rest2");
	test(lenarrayr(pos) == 4, "typeargs_var4rest3");
	test(refarray(pos, 1) == Nil, "typeargs_var4rest5");
	test(refarray(pos, 2) == rest, "typeargs_var4rest6");
	test(refarray(pos, 3) == Nil, "typeargs_var4rest7");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typeargs_var4rest8");
	getcons_(list, &check, &list);
	test(check == v2, "typeargs_var4rest9");
	getcons_(list, &check, &list);
	test(check == v3, "typeargs_var4rest10");
	getcons_(list, &check, &list);
	test(check == v4, "typeargs_var4rest11");
	test(list == Nil, "typeargs_var4rest12");

	RETURN;
}

static int test_typeargs_rest(void)
{
	addr pos, v1;

	type0_heap(LISPDECL_LIST, &v1);
	typeargs_rest(&pos, v1);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_rest1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_rest2");
	test(lenarrayr(pos) == 4, "typeargs_rest3");
	test(refarray(pos, 1) == Nil, "typeargs_rest5");
	test(refarray(pos, 2) == v1, "typeargs_rest6");
	test(refarray(pos, 3) == Nil, "typeargs_rest7");

	RETURN;
}

static int test_typeargs_key(void)
{
	addr pos, v1;

	type0_heap(LISPDECL_LIST, &v1);
	typeargs_key(&pos, v1);
	test(GetType(pos) == LISPTYPE_VECTOR, "typeargs_key1");
	test(GetStatusSize(pos) == LISPSIZE_ARRAY2, "typeargs_key2");
	test(lenarrayr(pos) == 4, "typeargs_key3");
	test(refarray(pos, 1) == Nil, "typeargs_key5");
	test(refarray(pos, 2) == Nil, "typeargs_key6");
	test(refarray(pos, 3) == v1, "typeargs_key7");

	RETURN;
}


/*
 *  values
 */
static int test_typevalues_result(void)
{
	addr pos, v1, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	typevalues_result(&pos, v1);
	test(GetType(pos) == LISPTYPE_TYPE, "typevalues_result1");
	test(RefLispDecl(pos) == LISPDECL_VALUES, "typevalues_result2");
	test(refarray(pos, 1) == Nil, "typevalues_result3");
	test(refarray(pos, 3) == Nil, "typevalues_result4");

	getarray(pos, 2, &check);
	test(RefLispDecl(check) == LISPDECL_NIL, "typevalues_result5");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typevalues_result6");
	test(list == Nil, "typevalues_result7");

	RETURN;
}

static int test_typevalues_values2(void)
{
	addr pos, v1, v2, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	typevalues_values2(&pos, v1, v2);
	test(GetType(pos) == LISPTYPE_TYPE, "typevalues_values2-1");
	test(RefLispDecl(pos) == LISPDECL_VALUES, "typevalues_values2-2");
	test(refarray(pos, 1) == Nil, "typevalues_values2-3");
	test(refarray(pos, 3) == Nil, "typevalues_values2-4");

	getarray(pos, 2, &check);
	test(RefLispDecl(check) == LISPDECL_NIL, "typevalues_values2-5");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typevalues_values2-6");
	getcons_(list, &check, &list);
	test(check == v2, "typevalues_values2-7");
	test(list == Nil, "typevalues_values2-8");

	RETURN;
}

static int test_typevalues_values3(void)
{
	addr pos, v1, v2, v3, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	typevalues_values3(&pos, v1, v2, v3);
	test(GetType(pos) == LISPTYPE_TYPE, "typevalues_values3-1");
	test(RefLispDecl(pos) == LISPDECL_VALUES, "typevalues_values3-2");
	test(refarray(pos, 1) == Nil, "typevalues_values3-3");
	test(refarray(pos, 3) == Nil, "typevalues_values3-4");

	getarray(pos, 2, &check);
	test(RefLispDecl(check) == LISPDECL_NIL, "typevalues_values3-5");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typevalues_values3-6");
	getcons_(list, &check, &list);
	test(check == v2, "typevalues_values3-7");
	getcons_(list, &check, &list);
	test(check == v3, "typevalues_values3-8");
	test(list == Nil, "typevalues_values3-9");

	RETURN;
}

static int test_typevalues_values4(void)
{
	addr pos, v1, v2, v3, v4, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	type0_heap(LISPDECL_CHARACTER, &v4);
	typevalues_values4(&pos, v1, v2, v3, v4);
	test(GetType(pos) == LISPTYPE_TYPE, "typevalues_values4-1");
	test(RefLispDecl(pos) == LISPDECL_VALUES, "typevalues_values4-2");
	test(refarray(pos, 1) == Nil, "typevalues_values4-3");
	test(refarray(pos, 3) == Nil, "typevalues_values4-4");

	getarray(pos, 2, &check);
	test(RefLispDecl(check) == LISPDECL_NIL, "typevalues_values4-5");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typevalues_values4-6");
	getcons_(list, &check, &list);
	test(check == v2, "typevalues_values4-7");
	getcons_(list, &check, &list);
	test(check == v3, "typevalues_values4-8");
	getcons_(list, &check, &list);
	test(check == v4, "typevalues_values4-9");
	test(list == Nil, "typevalues_values4-10");

	RETURN;
}

static int test_typevalues_values5(void)
{
	addr pos, v1, v2, v3, v4, v5, list, check;

	type0_heap(LISPDECL_LIST, &v1);
	type0_heap(LISPDECL_ATOM, &v2);
	type0_heap(LISPDECL_SYMBOL, &v3);
	type0_heap(LISPDECL_CHARACTER, &v4);
	type0_heap(LISPDECL_FIXNUM, &v5);
	typevalues_values5(&pos, v1, v2, v3, v4, v5);
	test(GetType(pos) == LISPTYPE_TYPE, "typevalues_values5-1");
	test(RefLispDecl(pos) == LISPDECL_VALUES, "typevalues_values5-2");
	test(refarray(pos, 1) == Nil, "typevalues_values5-3");
	test(refarray(pos, 3) == Nil, "typevalues_values5-4");

	getarray(pos, 2, &check);
	test(RefLispDecl(check) == LISPDECL_NIL, "typevalues_values5-5");

	getarray(pos, 0, &list);
	getcons_(list, &check, &list);
	test(check == v1, "typevalues_values5-6");
	getcons_(list, &check, &list);
	test(check == v2, "typevalues_values5-7");
	getcons_(list, &check, &list);
	test(check == v3, "typevalues_values5-8");
	getcons_(list, &check, &list);
	test(check == v4, "typevalues_values5-9");
	getcons_(list, &check, &list);
	test(check == v5, "typevalues_values5-10");
	test(list == Nil, "typevalues_values5-11");

	RETURN;
}

static int test_typevalues_rest(void)
{
	addr pos, v1, check;

	type0_heap(LISPDECL_LIST, &v1);
	typevalues_rest(&pos, v1);
	test(GetType(pos) == LISPTYPE_TYPE, "typevalues_rest1");
	test(RefLispDecl(pos) == LISPDECL_VALUES, "typevalues_rest2");
	test(refarray(pos, 0) == Nil, "typevalues_rest3");
	test(refarray(pos, 1) == Nil, "typevalues_rest3");
	test(refarray(pos, 3) == Nil, "typevalues_rest4");

	getarray(pos, 2, &check);
	test(check == v1, "typevalues_rest5");

	RETURN;
}


/*
 *  asterisk
 */
static int test_type1aster_alloc(void)
{
	addr pos, aster;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	GetTypeTable(&aster, Asterisk);

	type1aster_alloc(local, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type1aster_alloc1");
	test(GetStatusDynamic(pos), "type1aster_alloc2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type1aster_alloc3");
	test(lenarrayr(pos) == 1, "type1aster_alloc4");
	test(RefArrayType(pos, 0) == aster, "type1aster_alloc5");

	type1aster_alloc(NULL, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type1aster_alloc6");
	test(! GetStatusDynamic(pos), "type1aster_alloc7");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type1aster_alloc8");
	test(lenarrayr(pos) == 1, "type1aster_alloc9");
	test(RefArrayType(pos, 0) == aster, "type1aster_alloc10");

	type1aster_local(local, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type1aster_local1");
	test(GetStatusDynamic(pos), "type1aster_local2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type1aster_local3");
	test(lenarrayr(pos) == 1, "type1aster_local4");
	test(RefArrayType(pos, 0) == aster, "type1aster_local5");

	type1aster_heap(LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type1aster_heap1");
	test(! GetStatusDynamic(pos), "type1aster_heap2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type1aster_heap3");
	test(lenarrayr(pos) == 1, "type1aster_heap4");
	test(RefArrayType(pos, 0) == aster, "type1aster_heap5");

	rollback_local(local, stack);

	RETURN;
}

static int test_type2aster_alloc(void)
{
	addr pos, aster;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	GetTypeTable(&aster, Asterisk);

	type2aster_alloc(local, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2aster_alloc1");
	test(GetStatusDynamic(pos), "type2aster_alloc2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type2aster_alloc3");
	test(lenarrayr(pos) == 2, "type2aster_alloc4");
	test(RefArrayType(pos, 0) == aster, "type2aster_alloc5");
	test(RefArrayType(pos, 1) == aster, "type2aster_alloc6");

	type2aster_alloc(NULL, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2aster_alloc7");
	test(! GetStatusDynamic(pos), "type2aster_alloc8");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type2aster_alloc9");
	test(lenarrayr(pos) == 2, "type2aster_alloc10");
	test(RefArrayType(pos, 0) == aster, "type2aster_alloc11");
	test(RefArrayType(pos, 1) == aster, "type2aster_alloc12");

	type2aster_local(local, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2aster_local1");
	test(GetStatusDynamic(pos), "type2aster_local2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type2aster_local3");
	test(lenarrayr(pos) == 2, "type2aster_local4");
	test(RefArrayType(pos, 0) == aster, "type2aster_local5");
	test(RefArrayType(pos, 1) == aster, "type2aster_local6");

	type2aster_heap(LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2aster_heap1");
	test(! GetStatusDynamic(pos), "type2aster_heap2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type2aster_heap3");
	test(lenarrayr(pos) == 2, "type2aster_heap4");
	test(RefArrayType(pos, 0) == aster, "type2aster_heap5");
	test(RefArrayType(pos, 1) == aster, "type2aster_heap6");

	rollback_local(local, stack);

	RETURN;
}

static int test_type3aster_alloc(void)
{
	addr pos, aster;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	GetTypeTable(&aster, Asterisk);

	type3aster_alloc(local, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type3aster_alloc1");
	test(GetStatusDynamic(pos), "type3aster_alloc2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type3aster_alloc3");
	test(lenarrayr(pos) == 3, "type3aster_alloc4");
	test(RefArrayType(pos, 0) == aster, "type3aster_alloc5");
	test(RefArrayType(pos, 1) == aster, "type3aster_alloc6");
	test(RefArrayType(pos, 2) == aster, "type3aster_alloc7");

	type3aster_alloc(NULL, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type3aster_alloc8");
	test(! GetStatusDynamic(pos), "type3aster_alloc9");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type3aster_alloc10");
	test(lenarrayr(pos) == 3, "type3aster_alloc11");
	test(RefArrayType(pos, 0) == aster, "type3aster_alloc12");
	test(RefArrayType(pos, 1) == aster, "type3aster_alloc13");

	type3aster_local(local, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type3aster_local1");
	test(GetStatusDynamic(pos), "type3aster_local2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type3aster_local3");
	test(lenarrayr(pos) == 3, "type3aster_local4");
	test(RefArrayType(pos, 0) == aster, "type3aster_local5");
	test(RefArrayType(pos, 1) == aster, "type3aster_local6");
	test(RefArrayType(pos, 2) == aster, "type3aster_local7");

	type3aster_heap(LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type3aster_heap1");
	test(! GetStatusDynamic(pos), "type3aster_heap2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type3aster_heap3");
	test(lenarrayr(pos) == 3, "type3aster_heap4");
	test(RefArrayType(pos, 0) == aster, "type3aster_heap5");
	test(RefArrayType(pos, 1) == aster, "type3aster_heap6");
	test(RefArrayType(pos, 2) == aster, "type3aster_heap7");

	rollback_local(local, stack);

	RETURN;
}

static int test_type4aster_alloc(void)
{
	addr pos, aster;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	GetTypeTable(&aster, Asterisk);

	type4aster_alloc(local, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type4aster_alloc1");
	test(GetStatusDynamic(pos), "type4aster_alloc2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type4aster_alloc3");
	test(lenarrayr(pos) == 4, "type4aster_alloc4");
	test(RefArrayType(pos, 0) == aster, "type4aster_alloc5");
	test(RefArrayType(pos, 1) == aster, "type4aster_alloc6");
	test(RefArrayType(pos, 2) == aster, "type4aster_alloc7");
	test(RefArrayType(pos, 3) == aster, "type4aster_alloc8");

	type4aster_alloc(NULL, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type4aster_alloc9");
	test(! GetStatusDynamic(pos), "type4aster_alloc10");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type4aster_alloc11");
	test(lenarrayr(pos) == 4, "type4aster_alloc12");
	test(RefArrayType(pos, 0) == aster, "type4aster_alloc13");
	test(RefArrayType(pos, 1) == aster, "type4aster_alloc14");

	type4aster_local(local, LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type4aster_local1");
	test(GetStatusDynamic(pos), "type4aster_local2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type4aster_local3");
	test(lenarrayr(pos) == 4, "type4aster_local4");
	test(RefArrayType(pos, 0) == aster, "type4aster_local5");
	test(RefArrayType(pos, 1) == aster, "type4aster_local6");
	test(RefArrayType(pos, 2) == aster, "type4aster_local7");
	test(RefArrayType(pos, 3) == aster, "type4aster_local8");

	type4aster_heap(LISPDECL_SYMBOL, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type4aster_heap1");
	test(! GetStatusDynamic(pos), "type4aster_heap2");
	test(LowLispDecl(pos) == LISPDECL_SYMBOL, "type4aster_heap3");
	test(lenarrayr(pos) == 4, "type4aster_heap4");
	test(RefArrayType(pos, 0) == aster, "type4aster_heap5");
	test(RefArrayType(pos, 1) == aster, "type4aster_heap6");
	test(RefArrayType(pos, 2) == aster, "type4aster_heap7");
	test(RefArrayType(pos, 3) == aster, "type4aster_heap8");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  and/or
 */
static int test_type2and_alloc(void)
{
	addr x, y, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	/* alloc local */
	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, T);
	type2and_alloc(local, x, y, &pos);
	test(pos == x, "type2and_alloc1");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2and_alloc(local, x, y, &pos);
	test(pos == x, "type2and_alloc2");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2and_alloc(local, x, y, &pos);
	test(pos == y, "type2and_alloc3");

	GetTypeTable(&x, T);
	GetTypeTable(&y, Symbol);
	type2and_alloc(local, x, y, &pos);
	test(pos == y, "type2and_alloc4");

	GetTypeTable(&x, Asterisk);
	GetTypeTable(&y, Symbol);
	type2and_alloc(local, x, y, &pos);
	test(pos == y, "type2and_alloc5");

	GetTypeTable(&x, Nil);
	GetTypeTable(&y, Symbol);
	type2and_alloc(local, x, y, &pos);
	test(pos == x, "type2and_alloc6");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Integer);
	type2and_alloc(local, x, y, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2and_alloc7");
	test(GetStatusDynamic(pos), "type2and_alloc8");
	test(RefLispDecl(pos) == LISPDECL_AND, "type2and_alloc9");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "type2and_alloc10");
	test(refarray(pos, 0) == x, "type2and_alloc11");
	test(refarray(pos, 1) == y, "type2and_alloc12");

	/* alloc NULL */
	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, T);
	type2and_alloc(NULL, x, y, &pos);
	test(pos == x, "type2and_alloc13");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2and_alloc(NULL, x, y, &pos);
	test(pos == x, "type2and_alloc14");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2and_alloc(NULL, x, y, &pos);
	test(pos == y, "type2and_alloc15");

	GetTypeTable(&x, T);
	GetTypeTable(&y, Symbol);
	type2and_alloc(NULL, x, y, &pos);
	test(pos == y, "type2and_alloc16");

	GetTypeTable(&x, Asterisk);
	GetTypeTable(&y, Symbol);
	type2and_alloc(NULL, x, y, &pos);
	test(pos == y, "type2and_alloc17");

	GetTypeTable(&x, Nil);
	GetTypeTable(&y, Symbol);
	type2and_alloc(NULL, x, y, &pos);
	test(pos == x, "type2and_alloc18");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Integer);
	type2and_alloc(NULL, x, y, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2and_alloc19");
	test(! GetStatusDynamic(pos), "type2and_alloc20");
	test(RefLispDecl(pos) == LISPDECL_AND, "type2and_alloc21");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "type2and_alloc22");
	test(refarray(pos, 0) == x, "type2and_alloc23");
	test(refarray(pos, 1) == y, "type2and_alloc24");

	/* local */
	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, T);
	type2and_local(local, x, y, &pos);
	test(pos == x, "type2and_local1");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2and_local(local, x, y, &pos);
	test(pos == x, "type2and_local2");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2and_local(local, x, y, &pos);
	test(pos == y, "type2and_local3");

	GetTypeTable(&x, T);
	GetTypeTable(&y, Symbol);
	type2and_local(local, x, y, &pos);
	test(pos == y, "type2and_local4");

	GetTypeTable(&x, Asterisk);
	GetTypeTable(&y, Symbol);
	type2and_local(local, x, y, &pos);
	test(pos == y, "type2and_local5");

	GetTypeTable(&x, Nil);
	GetTypeTable(&y, Symbol);
	type2and_local(local, x, y, &pos);
	test(pos == x, "type2and_local6");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Integer);
	type2and_local(local, x, y, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2and_local7");
	test(GetStatusDynamic(pos), "type2and_local8");
	test(RefLispDecl(pos) == LISPDECL_AND, "type2and_local9");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "type2and_local10");
	test(refarray(pos, 0) == x, "type2and_local11");
	test(refarray(pos, 1) == y, "type2and_local12");

	/* heap */
	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, T);
	type2and_heap(x, y, &pos);
	test(pos == x, "type2and_heap1");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2and_heap(x, y, &pos);
	test(pos == x, "type2and_heap2");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2and_heap(x, y, &pos);
	test(pos == y, "type2and_heap3");

	GetTypeTable(&x, T);
	GetTypeTable(&y, Symbol);
	type2and_heap(x, y, &pos);
	test(pos == y, "type2and_heap4");

	GetTypeTable(&x, Asterisk);
	GetTypeTable(&y, Symbol);
	type2and_heap(x, y, &pos);
	test(pos == y, "type2and_heap5");

	GetTypeTable(&x, Nil);
	GetTypeTable(&y, Symbol);
	type2and_heap(x, y, &pos);
	test(pos == x, "type2and_heap6");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Integer);
	type2and_heap(x, y, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2and_heap7");
	test(! GetStatusDynamic(pos), "type2and_heap8");
	test(RefLispDecl(pos) == LISPDECL_AND, "type2and_heap9");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "type2and_heap10");
	test(refarray(pos, 0) == x, "type2and_heap11");
	test(refarray(pos, 1) == y, "type2and_heap12");

	rollback_local(local, stack);

	RETURN;
}

static int test_type2or_alloc(void)
{
	addr x, y, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	/* alloc local */
	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, T);
	type2or_alloc(local, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_alloc1");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2or_alloc(local, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_alloc2");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2or_alloc(local, x, y, &pos);
	test(pos == x, "type2or_alloc3");

	GetTypeTable(&x, T);
	GetTypeTable(&y, Symbol);
	type2or_alloc(local, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_alloc4");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2or_alloc(local, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_alloc5");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2or_alloc(local, x, y, &pos);
	test(pos == x, "type2or_alloc6");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Integer);
	type2or_alloc(local, x, y, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2or_alloc7");
	test(GetStatusDynamic(pos), "type2or_alloc8");
	test(RefLispDecl(pos) == LISPDECL_OR, "type2or_alloc9");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "type2or_alloc10");
	test(refarray(pos, 0) == x, "type2or_alloc11");
	test(refarray(pos, 1) == y, "type2or_alloc12");

	/* alloc NULL */
	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, T);
	type2or_alloc(NULL, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_alloc13");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2or_alloc(NULL, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_alloc14");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2or_alloc(NULL, x, y, &pos);
	test(pos == x, "type2or_alloc15");

	GetTypeTable(&x, T);
	GetTypeTable(&y, Symbol);
	type2or_alloc(NULL, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_alloc16");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2or_alloc(NULL, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_alloc17");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2or_alloc(NULL, x, y, &pos);
	test(pos == x, "type2or_alloc18");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Integer);
	type2or_alloc(NULL, x, y, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2or_alloc19");
	test(! GetStatusDynamic(pos), "type2or_alloc20");
	test(RefLispDecl(pos) == LISPDECL_OR, "type2or_alloc21");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "type2or_alloc22");
	test(refarray(pos, 0) == x, "type2or_alloc23");
	test(refarray(pos, 1) == y, "type2or_alloc24");

	/* local */
	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, T);
	type2or_local(local, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_local1");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2or_local(local, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_local2");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2or_local(local, x, y, &pos);
	test(pos == x, "type2or_local3");

	GetTypeTable(&x, T);
	GetTypeTable(&y, Symbol);
	type2or_local(local, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_local4");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2or_local(local, x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_local5");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2or_local(local, x, y, &pos);
	test(pos == x, "type2or_local6");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Integer);
	type2or_local(local, x, y, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2or_local7");
	test(GetStatusDynamic(pos), "type2or_local8");
	test(RefLispDecl(pos) == LISPDECL_OR, "type2or_local9");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "type2or_local10");
	test(refarray(pos, 0) == x, "type2or_local11");
	test(refarray(pos, 1) == y, "type2or_local12");

	/* heap */
	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, T);
	type2or_heap(x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_heap1");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2or_heap(x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_heap2");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2or_heap(x, y, &pos);
	test(pos == x, "type2or_heap3");

	GetTypeTable(&x, T);
	GetTypeTable(&y, Symbol);
	type2or_heap(x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_heap4");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Asterisk);
	type2or_heap(x, y, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type2or_heap5");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Nil);
	type2or_heap(x, y, &pos);
	test(pos == x, "type2or_heap6");

	GetTypeTable(&x, Symbol);
	GetTypeTable(&y, Integer);
	type2or_heap(x, y, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2or_heap7");
	test(! GetStatusDynamic(pos), "type2or_heap8");
	test(RefLispDecl(pos) == LISPDECL_OR, "type2or_heap9");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 2, "type2or_heap10");
	test(refarray(pos, 0) == x, "type2or_heap11");
	test(refarray(pos, 1) == y, "type2or_heap12");

	rollback_local(local, stack);

	RETURN;
}

static int test_type3or_alloc(void)
{
	addr a, b, c, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	GetTypeTable(&a, Symbol);
	GetTypeTable(&b, Cons);
	GetTypeTable(&c, Integer);

	type3or_alloc(local, a, b, c, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type3or_alloc1");
	test(GetStatusDynamic(pos), "type3or_alloc2");
	test(RefLispDecl(pos) == LISPDECL_OR, "type3or_alloc3");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 3, "type3or_alloc4");
	test(refarray(pos, 0) == a, "type3or_alloc5");
	test(refarray(pos, 1) == b, "type3or_alloc6");
	test(refarray(pos, 2) == c, "type3or_alloc7");

	type3or_alloc(NULL, a, b, c, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type3or_alloc8");
	test(! GetStatusDynamic(pos), "type3or_alloc9");
	test(RefLispDecl(pos) == LISPDECL_OR, "type3or_alloc10");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 3, "type3or_alloc11");
	test(refarray(pos, 0) == a, "type3or_alloc12");
	test(refarray(pos, 1) == b, "type3or_alloc13");
	test(refarray(pos, 2) == c, "type3or_alloc14");

	type3or_local(local, a, b, c, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type3or_local1");
	test(GetStatusDynamic(pos), "type3or_local2");
	test(RefLispDecl(pos) == LISPDECL_OR, "type3or_local3");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 3, "type3or_local4");
	test(refarray(pos, 0) == a, "type3or_local5");
	test(refarray(pos, 1) == b, "type3or_local6");
	test(refarray(pos, 2) == c, "type3or_local7");

	type3or_heap(a, b, c, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type3or_heap1");
	test(! GetStatusDynamic(pos), "type3or_heap2");
	test(RefLispDecl(pos) == LISPDECL_OR, "type3or_heap3");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 3, "type3or_heap4");
	test(refarray(pos, 0) == a, "type3or_heap5");
	test(refarray(pos, 1) == b, "type3or_heap6");
	test(refarray(pos, 2) == c, "type3or_heap7");

	rollback_local(local, stack);

	RETURN;
}

static int test_type4or_alloc(void)
{
	addr a, b, c, d, pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	GetTypeTable(&a, Symbol);
	GetTypeTable(&b, Cons);
	GetTypeTable(&c, Integer);
	GetTypeTable(&d, Package);

	type4or_alloc(local, a, b, c, d, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type4or_alloc1");
	test(GetStatusDynamic(pos), "type4or_alloc2");
	test(RefLispDecl(pos) == LISPDECL_OR, "type4or_alloc3");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 4, "type4or_alloc4");
	test(refarray(pos, 0) == a, "type4or_alloc5");
	test(refarray(pos, 1) == b, "type4or_alloc6");
	test(refarray(pos, 2) == c, "type4or_alloc7");
	test(refarray(pos, 3) == d, "type4or_alloc8");

	type4or_alloc(NULL, a, b, c, d, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type4or_alloc9");
	test(! GetStatusDynamic(pos), "type4or_alloc10");
	test(RefLispDecl(pos) == LISPDECL_OR, "type4or_alloc11");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 4, "type4or_alloc12");
	test(refarray(pos, 0) == a, "type4or_alloc13");
	test(refarray(pos, 1) == b, "type4or_alloc14");
	test(refarray(pos, 2) == c, "type4or_alloc15");
	test(refarray(pos, 3) == d, "type4or_alloc16");

	type4or_local(local, a, b, c, d, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type4or_local1");
	test(GetStatusDynamic(pos), "type4or_local2");
	test(RefLispDecl(pos) == LISPDECL_OR, "type4or_local3");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 4, "type4or_local4");
	test(refarray(pos, 0) == a, "type4or_local5");
	test(refarray(pos, 1) == b, "type4or_local6");
	test(refarray(pos, 2) == c, "type4or_local7");
	test(refarray(pos, 3) == d, "type4or_local8");

	type4or_heap(a, b, c, d, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type4or_heap1");
	test(! GetStatusDynamic(pos), "type4or_heap2");
	test(RefLispDecl(pos) == LISPDECL_OR, "type4or_heap3");
	GetArrayType(pos, 0, &pos);
	test(lenarrayr(pos) == 4, "type4or_heap4");
	test(refarray(pos, 0) == a, "type4or_heap5");
	test(refarray(pos, 1) == b, "type4or_heap6");
	test(refarray(pos, 2) == c, "type4or_heap7");
	test(refarray(pos, 3) == d, "type4or_heap8");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  range
 */
static int test_type1realf_heap(void)
{
	addr pos, value;

	fixnum_heap(&value, 11);
	type1real_heap(LISPDECL_INTEGER, value, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type1real_heap1");
	test(! GetStatusDynamic(pos), "type1real_heap2");
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "type1real_heap3");
	test(RefArrayType(pos, 0) == Nil, "type1real_heap4");
	test(RefArrayType(pos, 1) == value, "type1real_heap5");
	test(RefArrayType(pos, 2) == Nil, "type1real_heap6");
	test(RefArrayType(pos, 3) == value, "type1real_heap7");

	RETURN;
}

static int test_type4integer_heap(void)
{
	addr pos, value;

	type4integer_heap(T, 10, Nil, 20, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type4integer_heap1");
	test(! GetStatusDynamic(pos), "type4integer_heap2");
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "type4integer_heap3");
	test(RefArrayType(pos, 0) == T, "type4integer_heap4");
	test(RefArrayType(pos, 2) == Nil, "type4integer_heap5");
	GetArrayType(pos, 1, &value);
	test(RefFixnum(value) == 10, "type4integer_heap6");
	GetArrayType(pos, 3, &value);
	test(RefFixnum(value) == 20, "type4integer_heap7");

	RETURN;
}

static int test_type2integer_ab_heap(void)
{
	addr pos, value, aster;

	type2integer_ab_heap(T, 10, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2integer_ab_heap1");
	test(! GetStatusDynamic(pos), "type2integer_ab_heap2");
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "type2integer_ab_heap3");
	GetTypeTable(&aster, Asterisk);
	test(RefArrayType(pos, 0) == T, "type2integer_ab_heap4");
	GetArrayType(pos, 1, &value);
	test(RefFixnum(value) == 10, "type2integer_ab_heap5");
	test(RefArrayType(pos, 2) == aster, "type2integer_ab_heap6");
	test(RefArrayType(pos, 3) == aster, "type2integer_ab_heap7");

	RETURN;
}

static int test_type2integer_cd_heap(void)
{
	addr pos, value, aster;

	type2integer_cd_heap(T, 10, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2integer_cd_heap1");
	test(! GetStatusDynamic(pos), "type2integer_cd_heap2");
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "type2integer_cd_heap3");
	GetTypeTable(&aster, Asterisk);
	test(RefArrayType(pos, 0) == aster, "type2integer_cd_heap4");
	test(RefArrayType(pos, 1) == aster, "type2integer_cd_heap5");
	test(RefArrayType(pos, 2) == T, "type2integer_cd_heap6");
	GetArrayType(pos, 3, &value);
	test(RefFixnum(value) == 10, "type2integer_cd_heap7");

	RETURN;
}

static int test_type4float_heap(void)
{
	addr pos, value;

	type4float_heap(T, 11.1f, Nil, 22.2f, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type4float_heap1");
	test(! GetStatusDynamic(pos), "type4float_heap2");
	test(RefLispDecl(pos) == LISPDECL_FLOAT, "type4float_heap3");
	test(RefArrayType(pos, 0) == T, "type4float_heap4");
	test(RefArrayType(pos, 2) == Nil, "type4float_heap5");
	GetArrayType(pos, 1, &value);
	test(RefSingleFloat(value) == 11.1f, "type4float_heap6");
	GetArrayType(pos, 3, &value);
	test(RefSingleFloat(value) == 22.2f, "type4float_heap7");

	RETURN;
}

static int test_type2float_ab_heap(void)
{
	addr pos, value, aster;

	type2float_ab_heap(T, 11.1f, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2float_ab_heap1");
	test(! GetStatusDynamic(pos), "type2float_ab_heap2");
	test(RefLispDecl(pos) == LISPDECL_FLOAT, "type2float_ab_heap3");
	GetTypeTable(&aster, Asterisk);
	test(RefArrayType(pos, 0) == T, "type2float_ab_heap4");
	GetArrayType(pos, 1, &value);
	test(RefSingleFloat(value) == 11.1f, "type2float_ab_heap5");
	test(RefArrayType(pos, 2) == aster, "type2float_ab_heap6");
	test(RefArrayType(pos, 3) == aster, "type2float_ab_heap7");

	RETURN;
}

static int test_type2float_cd_heap(void)
{
	addr pos, value, aster;

	type2float_cd_heap(T, 11.1f, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2float_cd_heap1");
	test(! GetStatusDynamic(pos), "type2float_cd_heap2");
	test(RefLispDecl(pos) == LISPDECL_FLOAT, "type2float_cd_heap3");
	GetTypeTable(&aster, Asterisk);
	test(RefArrayType(pos, 0) == aster, "type2float_cd_heap4");
	test(RefArrayType(pos, 1) == aster, "type2float_cd_heap5");
	test(RefArrayType(pos, 2) == T, "type2float_cd_heap6");
	GetArrayType(pos, 3, &value);
	test(RefSingleFloat(value) == 11.1f, "type2float_cd_heap7");

	RETURN;
}

static int test_type4realf_heap(void)
{
	addr pos, value;

	type4realf_heap(T, 11.1f, Nil, 22.2f, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type4realf_heap1");
	test(! GetStatusDynamic(pos), "type4realf_heap2");
	test(RefLispDecl(pos) == LISPDECL_REAL, "type4realf_heap3");
	test(RefArrayType(pos, 0) == T, "type4realf_heap4");
	test(RefArrayType(pos, 2) == Nil, "type4realf_heap5");
	GetArrayType(pos, 1, &value);
	test(RefSingleFloat(value) == 11.1f, "type4realf_heap6");
	GetArrayType(pos, 3, &value);
	test(RefSingleFloat(value) == 22.2f, "type4realf_heap7");

	RETURN;
}

static int test_type2realf_ab_heap(void)
{
	addr pos, value, aster;

	type2realf_ab_heap(T, 11.1f, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2realf_ab_heap1");
	test(! GetStatusDynamic(pos), "type2realf_ab_heap2");
	test(RefLispDecl(pos) == LISPDECL_REAL, "type2realf_ab_heap3");
	GetTypeTable(&aster, Asterisk);
	test(RefArrayType(pos, 0) == T, "type2realf_ab_heap4");
	GetArrayType(pos, 1, &value);
	test(RefSingleFloat(value) == 11.1f, "type2realf_ab_heap5");
	test(RefArrayType(pos, 2) == aster, "type2realf_ab_heap6");
	test(RefArrayType(pos, 3) == aster, "type2realf_ab_heap7");

	RETURN;
}

static int test_type2realf_cd_heap(void)
{
	addr pos, value, aster;

	type2realf_cd_heap(T, 11.1f, &pos);
	test(GetType(pos) == LISPTYPE_TYPE, "type2realf_cd_heap1");
	test(! GetStatusDynamic(pos), "type2realf_cd_heap2");
	test(RefLispDecl(pos) == LISPDECL_REAL, "type2realf_cd_heap3");
	GetTypeTable(&aster, Asterisk);
	test(RefArrayType(pos, 0) == aster, "type2realf_cd_heap4");
	test(RefArrayType(pos, 1) == aster, "type2realf_cd_heap5");
	test(RefArrayType(pos, 2) == T, "type2realf_cd_heap6");
	GetArrayType(pos, 3, &value);
	test(RefSingleFloat(value) == 11.1f, "type2realf_cd_heap7");

	RETURN;
}


/*
 *  main
 */
static int testcase_type_table(void)
{
	/* interface */
	TestBreak(test_gettypetable);
	TestBreak(test_GetTypeTable);
	TestBreak(test_KeyTypeTable);
	/* arguments */
	TestBreak(test_typeargs_empty);
	TestBreak(test_typeargs_full);
	TestBreak(test_typeargs_var1);
	TestBreak(test_typeargs_var2);
	TestBreak(test_typeargs_var3);
	TestBreak(test_typeargs_var4);
	TestBreak(test_typeargs_var5);
	TestBreak(test_typeargs_var1key);
	TestBreak(test_typeargs_var2key);
	TestBreak(test_typeargs_var3key);
	TestBreak(test_typeargs_var4key);
	TestBreak(test_typeargs_opt1);
	TestBreak(test_typeargs_opt2);
	TestBreak(test_typeargs_opt3);
	TestBreak(test_typeargs_opt4);
	TestBreak(test_typeargs_opt5);
	TestBreak(test_typeargs_var1opt1);
	TestBreak(test_typeargs_var1opt2);
	TestBreak(test_typeargs_var1opt2key);
	TestBreak(test_typeargs_var2opt1);
	TestBreak(test_typeargs_var2opt2);
	TestBreak(test_typeargs_var3opt1);
	TestBreak(test_typeargs_var1rest);
	TestBreak(test_typeargs_var2rest);
	TestBreak(test_typeargs_var3rest);
	TestBreak(test_typeargs_var4rest);
	TestBreak(test_typeargs_rest);
	TestBreak(test_typeargs_key);
	/* values */
	TestBreak(test_typevalues_result);
	TestBreak(test_typevalues_values2);
	TestBreak(test_typevalues_values3);
	TestBreak(test_typevalues_values4);
	TestBreak(test_typevalues_values5);
	TestBreak(test_typevalues_rest);
	/* asterisk */
	TestBreak(test_type1aster_alloc);
	TestBreak(test_type2aster_alloc);
	TestBreak(test_type3aster_alloc);
	TestBreak(test_type4aster_alloc);
	/* and/or */
	TestBreak(test_type2and_alloc);
	TestBreak(test_type2or_alloc);
	TestBreak(test_type3or_alloc);
	TestBreak(test_type4or_alloc);
	/* range */
	TestBreak(test_type1realf_heap);
	TestBreak(test_type4integer_heap);
	TestBreak(test_type2integer_ab_heap);
	TestBreak(test_type2integer_cd_heap);
	TestBreak(test_type4float_heap);
	TestBreak(test_type2float_ab_heap);
	TestBreak(test_type2float_cd_heap);
	TestBreak(test_type4realf_heap);
	TestBreak(test_type2realf_ab_heap);
	TestBreak(test_type2realf_cd_heap);

	return 0;
}

static void testinit_type_table(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_package();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_common();
}

int test_type_table(void)
{
	DegradeTitle;
	return DegradeCode(type_table);
}

