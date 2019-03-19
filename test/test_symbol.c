#include "symbol.c"
#include "clos.h"
#include "common.h"
#include "condition.h"
#include "degrade.h"
#include "package.h"
#include "type_parse.h"
#include "type_table.h"

static int test_symbol_allocr(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	pos = symbol_allocr(NULL);
	test(GetType(pos) == LISPTYPE_SYMBOL, "symbol_allocr1");
	test(GetCheckSymbol(pos), "symbol_allocr2");
	GetValueSymbol(pos, &check);
	test(check == Unbound, "symbol_allocr3");
	GetFunctionSymbol(pos, &check);
	test(check == Unbound, "symbol_allocr4");
	GetStackSymbol_Low(pos, &check);
	test(GetStatusSize(check) == LISPSIZE_ARRAY4, "symbol_allocr5");
	GetArrayA4(check, 0, &check);
	test(GetStatusSize(check) == LISPSIZE_ARRAY2, "symbol_allocr6");

	local = Local_Thread;
	push_local(local, &stack);
	pos = symbol_localr(local);
	test(GetType(pos) == LISPTYPE_SYMBOL, "symbol_localr1");
	test(GetStatusDynamic(pos), "symbol_localr2");
	rollback_local(local, stack);

	pos = symbol_heapr();
	test(GetType(pos) == LISPTYPE_SYMBOL, "symbol_heapr1");
	test(! GetStatusDynamic(pos), "symbol_heapr2");

	RETURN;
}

static int test_symbol_alloc(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	symbol_alloc(NULL, &pos);
	test(GetType(pos) == LISPTYPE_SYMBOL, "symbol_alloc1");
	test(GetCheckSymbol(pos), "symbol_alloc2");
	GetValueSymbol(pos, &check);
	test(check == Unbound, "symbol_alloc3");
	GetFunctionSymbol(pos, &check);
	test(check == Unbound, "symbol_alloc4");
	GetStackSymbol_Low(pos, &check);
	test(GetStatusSize(check) == LISPSIZE_ARRAY4, "symbol_alloc5");
	GetArrayA4(check, 0, &check);
	test(GetStatusSize(check) == LISPSIZE_ARRAY2, "symbol_alloc6");

	local = Local_Thread;
	push_local(local, &stack);
	symbol_local(local, &pos);
	test(GetType(pos) == LISPTYPE_SYMBOL, "symbol_local1");
	test(GetStatusDynamic(pos), "symbol_local2");
	rollback_local(local, stack);

	symbol_heap(&pos);
	test(GetType(pos) == LISPTYPE_SYMBOL, "symbol_heap1");
	test(! GetStatusDynamic(pos), "symbol_heap2");

	RETURN;
}

static int test_symbolp(void)
{
	addr pos;

	symbol_heap(&pos);
	test(symbolp(pos), "symbolp1");
	test(symbolp(Nil), "symbolp2");

	fixnum_heap(&pos, 100);
	test(! symbolp(pos), "symbolp3");

	internchar_keyword("HELLO", &pos);
	test(symbolp(pos), "symbolp4");

	internchar(LISP_PACKAGE, "AAA", &pos);
	test(symbolp(pos), "symbolp5");

	test(symbolp(T), "symbolp6");

	RETURN;
}

static int test_keywordp(void)
{
	addr pos;

	internchar_keyword("HELLO", &pos);
	test(keywordp(pos), "keywordp1");

	symbol_heap(&pos);
	test(! keywordp(pos), "keywordp2");
	test(! keywordp(Nil), "keywordp3");

	fixnum_heap(&pos, 100);
	test(! keywordp(pos), "keywordp4");

	internchar(LISP_PACKAGE, "AAA", &pos);
	test(! keywordp(pos), "keywordp5");

	RETURN;
}

static int test_getname_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	strvect_char_heap(&left, "Hello");
	setarray(pos, SYMBOL_INDEX_NAME, left);
	GetNameSymbol(pos, &right);
	test(left == right, "getname_symbol1");

	strvect_char_heap(&left, "AAA");
	SetNameSymbol(pos, left);
	test(left == RefNameSymbol(pos), "setname_symbol1");

	RETURN;
}

static int test_getvalue_symbol(void)
{
	addr pos, cons, left, right;

	symbol_heap(&pos);
	getarray(pos, SYMBOL_INDEX_VALUE, &cons);
	test(cons == Nil, "getvalue_symbol1");

	strvect_char_heap(&left, "Hello");
	conscar_heap(&cons, left);
	setarray(pos, SYMBOL_INDEX_VALUE, cons);
	GetValueSymbol(pos, &right);
	test(left == right, "getvalue_symbol4");

	strvect_char_heap(&left, "AAA");
	SetValueSymbol(pos, left);
	test(left == RefValueSymbol(pos), "setvalue_symbol5");

	RETURN;
}

static int test_getfunction_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	strvect_char_heap(&left, "Hello");
	setarray(pos, SYMBOL_INDEX_FUNCTION, left);
	GetFunctionSymbol(pos, &right);
	test(left == right, "getfunction_symbol1");

	strvect_char_heap(&left, "AAA");
	SetFunctionSymbol(pos, left);
	test(left == RefFunctionSymbol(pos), "setfunction_symbol1");

	RETURN;
}

static int test_getpackage_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	find_char_package("COMMON-LISP", &left);
	setarray(pos, SYMBOL_INDEX_PACKAGE, left);
	GetPackageSymbol(pos, &right);
	test(left == right, "getpackage_symbol1");

	find_char_package("COMMON-LISP-USER", &left);
	SetPackageSymbol(pos, left);
	test(left == RefPackageSymbol(pos), "setpackage_symbol1");

	RETURN;
}

static int test_getplist_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	consnil_heap(&left);
	setarray(pos, SYMBOL_INDEX_PLIST, left);
	GetPlistSymbol(pos, &right);
	test(left == right, "getplist_symbol1");

	consnil_heap(&left);
	SetPlistSymbol(pos, left);
	test(left == RefPlistSymbol(pos), "setplist_symbol1");

	SetPlistSymbol(pos, Nil);
	test(Nil == RefPlistSymbol(pos), "setplist_symbol2");

	RETURN;
}

static int test_getinfo_constant(void)
{
	addr pos, check;

	symbol_heap(&pos);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &check);
	test(check == Nil, "getinfo_constant1");

	GetConstant(CONSTANT_SYSTEM_VALUE, &check);
	list_heap(&check, check, T, NULL);
	SetInfoSymbol_Low(pos, check);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &check);
	test(check == T, "getinfo_constant2");

	RETURN;
}

static int test_setinfo_constant(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	fixnum_heap(&left, 100);
	setinfo_constant(pos, CONSTANT_SYSTEM_VALUE, left);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &right);
	test(left == right, "setinfo_constant1");

	setinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, left);
	setinfo_constant(pos, CONSTANT_SYSTEM_VALUE, T);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &right);
	test(right == T, "setinfo_constant2");

	RETURN;
}

static int test_reminfo_constant(void)
{
	addr pos, value1, value2, value3, check;

	symbol_heap(&pos);
	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	fixnum_heap(&value3, 300);

	symbol_heap(&pos);
	setinfo_constant(pos, CONSTANT_SYSTEM_VALUE, value1);
	setinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, value2);
	setinfo_constant(pos, CONSTANT_COMMON_SYMBOL, value3);
	reminfo_constant(pos, CONSTANT_SYSTEM_VALUE);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &check);
	test(check == Nil, "reminfo_constant1");
	getinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, &check);
	test(check == value2, "reminfo_constant2");
	getinfo_constant(pos, CONSTANT_COMMON_SYMBOL, &check);
	test(check == value3, "reminfo_constant3");

	symbol_heap(&pos);
	setinfo_constant(pos, CONSTANT_SYSTEM_VALUE, value1);
	setinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, value2);
	setinfo_constant(pos, CONSTANT_COMMON_SYMBOL, value3);
	reminfo_constant(pos, CONSTANT_SYSTEM_FUNCTION);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &check);
	test(check == value1, "reminfo_constant4");
	getinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, &check);
	test(check == Nil, "reminfo_constant5");
	getinfo_constant(pos, CONSTANT_COMMON_SYMBOL, &check);
	test(check == value3, "reminfo_constant6");

	symbol_heap(&pos);
	setinfo_constant(pos, CONSTANT_SYSTEM_VALUE, value1);
	setinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, value2);
	setinfo_constant(pos, CONSTANT_COMMON_SYMBOL, value3);
	reminfo_constant(pos, CONSTANT_COMMON_SYMBOL);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &check);
	test(check == value1, "reminfo_constant7");
	getinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, &check);
	test(check == value2, "reminfo_constant8");
	getinfo_constant(pos, CONSTANT_COMMON_SYMBOL, &check);
	test(check == Nil, "reminfo_constant9");

	RETURN;
}

static int test_gettype_value_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	GetTypeTable(&left, Null);
	setinfo_constant(pos, CONSTANT_SYSTEM_VALUE, left);
	gettype_value_symbol(pos, &right);
	test(left == right, "gettype_value_symbol1");

	GetTypeTable(&left, Package);
	settype_value_symbol(pos, left);
	gettype_value_symbol(pos, &right);
	test(left == right, "gettype_value_symbol2");

	remtype_value_symbol(pos);
	gettype_value_symbol(pos, &right);
	test(right == Nil, "gettype_value_symbol3");

	RETURN;
}

static int test_gettype_function_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	GetTypeTable(&left, Null);
	setinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, left);
	gettype_function_symbol(pos, &right);
	test(left == right, "gettype_function_symbol1");

	GetTypeTable(&left, Package);
	settype_function_symbol(pos, left);
	gettype_function_symbol(pos, &right);
	test(left == right, "gettype_function_symbol2");

	remtype_function_symbol(pos);
	gettype_function_symbol(pos, &right);
	test(right == Nil, "gettype_function_symbol3");

	RETURN;
}

static int test_gettype_setf_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	GetTypeTable(&left, Null);
	setinfo_constant(pos, CONSTANT_SYSTEM_SETF, left);
	gettype_setf_symbol(pos, &right);
	test(left == right, "gettype_setf_symbol1");

	GetTypeTable(&left, Package);
	settype_setf_symbol(pos, left);
	gettype_setf_symbol(pos, &right);
	test(left == right, "gettype_setf_symbol2");

	remtype_setf_symbol(pos);
	gettype_setf_symbol(pos, &right);
	test(right == Nil, "gettype_setf_symbol3");

	RETURN;
}

static int test_inlinep_function_symbol(void)
{
	addr pos, value;

	symbol_heap(&pos);
	test(! inlinep_function_symbol(pos), "inlinep_function_symbol1");

	GetConstant(CONSTANT_COMMON_INLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
	test(inlinep_function_symbol(pos), "inlinep_function_symbol2");

	GetConstant(CONSTANT_COMMON_NOTINLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
	test(! inlinep_function_symbol(pos), "inlinep_function_symbol3");

	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, Nil);
	test(! inlinep_function_symbol(pos), "inlinep_function_symbol4");

	setinline_function_symbol(pos);
	test(inlinep_function_symbol(pos), "inlinep_function_symbol5");

	reminline_function_symbol(pos);
	test(! inlinep_function_symbol(pos), "inlinep_function_symbol6");

	RETURN;
}

static int test_notinlinep_function_symbol(void)
{
	addr pos, value;

	symbol_heap(&pos);
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol1");

	GetConstant(CONSTANT_COMMON_NOTINLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
	test(notinlinep_function_symbol(pos), "notinlinep_function_symbol2");

	GetConstant(CONSTANT_COMMON_INLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol3");

	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, Nil);
	test(! inlinep_function_symbol(pos), "notinlinep_function_symbol4");
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol5");

	setinline_function_symbol(pos);
	test(inlinep_function_symbol(pos), "notinlinep_function_symbol6");
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol7");

	setnotinline_function_symbol(pos);
	test(! inlinep_function_symbol(pos), "notinlinep_function_symbol8");
	test(notinlinep_function_symbol(pos), "notinlinep_function_symbol9");

	setinline_function_symbol(pos);
	test(inlinep_function_symbol(pos), "notinlinep_function_symbol10");
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol11");

	reminline_function_symbol(pos);
	test(! inlinep_function_symbol(pos), "notinlinep_function_symbol12");
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol13");

	RETURN;
}

static int test_inlinep_setf_symbol(void)
{
	addr pos, value;

	symbol_heap(&pos);
	test(! inlinep_setf_symbol(pos), "inlinep_setf_symbol1");

	GetConstant(CONSTANT_COMMON_INLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, value);
	test(inlinep_setf_symbol(pos), "inlinep_setf_symbol2");

	GetConstant(CONSTANT_COMMON_NOTINLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, value);
	test(! inlinep_setf_symbol(pos), "inlinep_setf_symbol3");

	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, Nil);
	test(! inlinep_setf_symbol(pos), "inlinep_setf_symbol4");

	setinline_setf_symbol(pos);
	test(inlinep_setf_symbol(pos), "inlinep_setf_symbol5");

	reminline_setf_symbol(pos);
	test(! inlinep_setf_symbol(pos), "inlinep_setf_symbol6");

	RETURN;
}

static int test_notinlinep_setf_symbol(void)
{
	addr pos, value;

	symbol_heap(&pos);
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol1");

	GetConstant(CONSTANT_COMMON_NOTINLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, value);
	test(notinlinep_setf_symbol(pos), "notinlinep_setf_symbol2");

	GetConstant(CONSTANT_COMMON_INLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, value);
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol3");

	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, Nil);
	test(! inlinep_setf_symbol(pos), "notinlinep_setf_symbol4");
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol5");

	setinline_setf_symbol(pos);
	test(inlinep_setf_symbol(pos), "notinlinep_setf_symbol6");
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol7");

	setnotinline_setf_symbol(pos);
	test(! inlinep_setf_symbol(pos), "notinlinep_setf_symbol8");
	test(notinlinep_setf_symbol(pos), "notinlinep_setf_symbol9");

	setinline_setf_symbol(pos);
	test(inlinep_setf_symbol(pos), "notinlinep_setf_symbol10");
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol11");

	reminline_setf_symbol(pos);
	test(! inlinep_setf_symbol(pos), "notinlinep_setf_symbol12");
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol13");

	RETURN;
}

static int test_getsetf_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	getsetf_symbol(pos, &left);
	test(left == Unbound, "getsetf_symbol1");

	consnil_heap(&left);
	setinfo_constant(pos, CONSTANT_COMMON_SETF, left);
	getsetf_symbol(pos, &right);
	test(left == right, "getsetf_symbol2");

	conscar_heap(&left, left);
	setsetf_symbol(pos, left);
	test(left == refsetf_symbol(pos), "getsetf_symbol3");

	remsetf_symbol(pos);
	test(refsetf_symbol(pos) == Unbound, "getsetf_symbol4");

	RETURN;
}

static int test_getscope_symbol(void)
{
	addr pos, check;

	symbol_heap(&pos);
	getscope_symbol(pos, &check);
	test(check == Nil, "getscope_symbol1");
	test(lexicalp_symbol(pos), "getscope_symbol2");
	test(! specialp_symbol(pos), "getscope_symbol3");

	setinfo_constant(pos, CONSTANT_COMMON_SPECIAL, T);
	getscope_symbol(pos, &check);
	test(check == T, "getscope_symbol4");
	test(! lexicalp_symbol(pos), "getscope_symbol5");
	test(specialp_symbol(pos), "getscope_symbol6");

	symbol_heap(&pos);
	setscope_symbol(pos, T);
	test(refscope_symbol(pos) == T, "getscope_symbol7");
	test(! lexicalp_symbol(pos), "getscope_symbol8");
	test(specialp_symbol(pos), "getscope_symbol9");

	symbol_heap(&pos);
	setspecial_symbol(pos);
	test(! lexicalp_symbol(pos), "getscope_symbol10");
	test(specialp_symbol(pos), "getscope_symbol11");

	setlexical_symbol(pos);
	test(lexicalp_symbol(pos), "getscope_symbol12");
	test(! specialp_symbol(pos), "getscope_symbol13");

	RETURN;
}


/*
 *  symstack
 */
static int test_realloc_symbol(void)
{
	addr pos, temp;
	fixnum i, fix;

	heap_array4(&pos, LISPTYPE_VECTOR, 10);
	for (i = 0; i < 10; i++) {
		fixnum_heap(&temp, i*10);
		SetArrayA4(pos, i, temp);
	}
	realloc_symbol(&pos, 10, 20);
	i = GetLenArrayA4(pos);
	test(i == 20, "realloc_symbol1");
	for (i = 0; i < 10; i++) {
		GetArrayA4(pos, i, &temp);
		GetFixnum(temp, &fix);
		test(fix == (i * 10), "realloc_symbol2");
	}
	GetArrayA4(pos, i, &temp);
	test(temp == Nil, "realloc_symbol3");

	RETURN;
}

static int test_symstack(void)
{
	addr symbol, array, value;
	size_t size, size2;

	symbol_heap(&symbol);
	GetStackSymbol_Low(symbol, &array);
	size = GetLenArrayA4(array);
	symstack(0, symbol, &value);
	test(value != Nil, "symstack1");

	GetStackSymbol_Low(symbol, &array);
	size2 = GetLenArrayA4(array);
	test(size == size2, "symstack2");

	symstack(size + 1, symbol, &value);
	test(value != Nil, "symstack3");
#ifndef LISP_THREAD_DISABLE
	GetStackSymbol_Low(symbol, &array);
	size2 = GetLenArrayA4(array);
	test((size * 2) == size2, "symstack4");
#endif

	RETURN;
}

static int test_pushsymlocal(void)
{
	addr pos, root, temp, check, right;
	Execute ptr;
	size_t index;

	ptr = Execute_Thread;
	index = Index_Thread;
	unsafe_push_local(ptr->local);
	symbol_heap(&pos);
	GetStackSymbol_Low(pos, &root);
	GetArrayA4(root, index, &root);
	GetArrayA2(root, SYMBOL_STACK_LEXICAL, &check);
	test(check == Nil, "pushsymlocal1");

	consnil_heap(&temp);
	pushsymlocal(ptr, pos, temp, SYMBOL_STACK_LEXICAL);
	GetArrayA2(root, SYMBOL_STACK_LEXICAL, &check);
	test(check != Nil, "pushsymlocal2");
	GetCons(check, &check, &right);
	test(check == temp, "pushsymlocal3");
	test(right == Nil, "pushsymlocal4");

	strvect_char_heap(&temp, "Hello");
	pushlexical_unsafe(ptr, pos, temp);
	GetArrayA2(root, SYMBOL_STACK_LEXICAL, &check);
	GetCons(check, &check, &right);
	test(consp(check), "pushsymlocal5");
	GetCar(check, &check);
	test(string_equal_char(check, "Hello"), "pushsymlocal6");
	GetCons(right, &check, &right);
	test(check != Nil, "pushsymlocal7");
	test(right == Nil, "pushsymlocal8");

	pushspecial_unsafe(ptr, pos, temp);
	pushfunction_unsafe(ptr, pos, temp);
	GetStackSymbol_Low(pos, &root);
	GetArrayA4(root, index, &root);
	GetArrayA2(root, SYMBOL_STACK_SPECIAL, &check);
	GetCons(check, &check, &right);
	test(check == temp, "pushsymlocal9");
	test(right == Nil, "pushsymlocal10");

	GetStackSymbol_Low(pos, &root);
	GetArrayA4(root, index, &root);
	GetArrayA2(root, SYMBOL_STACK_FUNCTION, &check);
	GetCons(check, &check, &right);
	test(check == temp, "pushsymlocal11");
	test(right == Nil, "pushsymlocal12");
	unsafe_pop_local(ptr->local);

	RETURN;
}

static int test_popsymlocal(void)
{
	addr pos, temp, root, left, right;
	Execute ptr;

	ptr = Execute_Thread;
	unsafe_push_local(ptr->local);

	symbol_heap(&pos);
	strvect_char_heap(&temp, "Hello");
	pushlexical_unsafe(ptr, pos, temp);
	strvect_char_heap(&temp, "aaa");
	pushlexical_unsafe(ptr, pos, temp);
	strvect_char_heap(&temp, "bbb");
	pushlexical_unsafe(ptr, pos, temp);

	GetStackSymbol_Low(pos, &root);
	GetArrayA4(root, 0, &root);
	GetArrayA2(root, SYMBOL_STACK_LEXICAL, &right);
	GetCons(right, &left, &right);
	test(consp(left), "popsymlocal1");
	GetCar(left, &left);
	test(string_equal_char(left, "bbb"), "popsymlocal2");
	GetCons(right, &left, &right);
	GetCar(left, &left);
	test(string_equal_char(left, "aaa"), "popsymlocal3");
	GetCons(right, &left, &right);
	GetCar(left, &left);
	test(string_equal_char(left, "Hello"), "popsymlocal4");
	test(right == Nil, "popsymlocal5");

	popsymlocal(ptr, pos, SYMBOL_STACK_LEXICAL);
	GetStackSymbol_Low(pos, &root);
	GetArrayA4(root, 0, &root);
	GetArrayA2(root, SYMBOL_STACK_LEXICAL, &right);
	GetCons(right, &left, &right);
	test(consp(left), "popsymlocal6");
	GetCar(left, &left);
	test(string_equal_char(left, "aaa"), "popsymlocal7");
	GetCons(right, &left, &right);
	GetCar(left, &left);
	test(string_equal_char(left, "Hello"), "popsymlocal8");
	test(right == Nil, "popsymlocal9");

	poplexical_unsafe(ptr, pos);
	poplexical_unsafe(ptr, pos);
	GetStackSymbol_Low(pos, &root);
	GetArrayA4(root, 0, &root);
	GetArrayA2(root, SYMBOL_STACK_LEXICAL, &right);
	test(right == Nil, "popsymlocal10");
	unsafe_pop_local(ptr->local);

	RETURN;
}

static int test_clearsymlocal(void)
{
	addr pos, root, temp;
	Execute ptr;

	ptr = Execute_Thread;
	unsafe_push_local(ptr->local);

	symbol_heap(&pos);
	strvect_char_heap(&temp, "hello");
	pushspecial_unsafe(ptr, pos, temp);
	strvect_char_heap(&temp, "aaa");
	pushspecial_unsafe(ptr, pos, temp);
	strvect_char_heap(&temp, "bbb");
	pushspecial_unsafe(ptr, pos, temp);

	clearlexical_local(ptr, pos);
	GetStackSymbol_Low(pos, &root);
	GetArrayA4(root, 0, &root);
	GetArrayA2(root, SYMBOL_STACK_SPECIAL, &root);
	test(root != Nil, "clearsymlocal1");

	clearspecial_local(ptr, pos);
	GetStackSymbol_Low(pos, &root);
	GetArrayA4(root, 0, &root);
	GetArrayA2(root, SYMBOL_STACK_SPECIAL, &root);
	test(root == Nil, "clearsymlocal2");
	unsafe_pop_local(ptr->local);

	RETURN;
}

static int test_getsymlocal(void)
{
	addr pos, base, temp, check;
	Execute ptr;

	ptr = Execute_Thread;
	unsafe_push_local(ptr->local);

	symbol_heap(&pos);
	getlexical_local(ptr, pos, &check);
	test(check == Unbound, "getsymlocal1");
	getspecial_local(ptr, pos, &check);
	test(check == Unbound, "getsymlocal2");
	getfunction_local(ptr, pos, &check);
	test(check == Unbound, "getsymlocal3");

	strvect_char_heap(&temp, "aaa");
	pushlexical_unsafe(ptr, pos, temp);
	getlexical_local(ptr, pos, &check);
	test(check == temp, "getsymlocal4");
	getspecial_local(ptr, pos, &check);
	test(check == Unbound, "getsymlocal5");
	getfunction_local(ptr, pos, &check);
	test(check == Unbound, "getsymlocal6");

	strvect_char_heap(&base, "base");
	SetValueSymbol(pos, base);
	getlexical_local(ptr, pos, &check);
	test(check == temp, "getsymlocal7");
	getspecial_local(ptr, pos, &check);
	test(check == base, "getsymlocal8");
	getfunction_local(ptr, pos, &check);
	test(check == Unbound, "getsymlocal9");

	strvect_char_heap(&temp, "bbb");
	pushlexical_unsafe(ptr, pos, temp);
	strvect_char_heap(&temp, "ccc");
	pushspecial_unsafe(ptr, pos, temp);
	strvect_char_heap(&temp, "ddd");
	pushspecial_unsafe(ptr, pos, temp);
	strvect_char_heap(&temp, "eee");
	pushspecial_unsafe(ptr, pos, temp);

	getlexical_local(ptr, pos, &check);
	test(string_equal_char(check, "bbb"), "getsymlocal10");
	getspecial_local(ptr, pos, &check);
	test(string_equal_char(check, "eee"), "getsymlocal11");
	getfunction_local(ptr, pos, &check);
	test(check == Unbound, "getsymlocal12");
	GetValueSymbol(pos, &check);
	test(string_equal_char(check, "base"), "getsymlocal13");
	poplexical_unsafe(ptr, pos);
	getlexical_local(ptr, pos, &check);
	test(string_equal_char(check, "aaa"), "getsymlocal14");

	strvect_char_heap(&temp, "function");
	pushfunction_unsafe(ptr, pos, temp);
	getfunction_local(ptr, pos, &check);
	test(string_equal_char(check, "function"), "getsymlocal15");
	clearlexical_local(ptr, pos);
	clearspecial_local(ptr, pos);
	clearfunction_local(ptr, pos);

	getlexical_local(ptr, pos, &check);
	test(check == base, "getsymlocal16");
	getspecial_local(ptr, pos, &check);
	test(check == base, "getsymlocal17");
	getfunction_local(ptr, pos, &check);
	test(check == Unbound, "getsymlocal18");
	unsafe_pop_local(ptr->local);

	RETURN;
}

static int test_setsymlocal(void)
{
	addr pos, temp, check;
	Execute ptr;

	ptr = Execute_Thread;
	unsafe_push_local(ptr->local);

	symbol_heap(&pos);
	strvect_char_heap(&temp, "Hello");
	setlexical_local(ptr, pos, temp);

	GetValueSymbol(pos, &check);
	test(string_equal_char(check, "Hello"), "setsymlocal1");
	strvect_char_heap(&temp, "aaaa");
	setspecial_local(ptr, pos, temp);
	GetValueSymbol(pos, &check);
	test(string_equal_char(check, "aaaa"), "setsymlocal2");

	strvect_char_heap(&temp, "bbb");
	pushspecial_unsafe(ptr, pos, temp);
	strvect_char_heap(&temp, "ccc");
	pushspecial_unsafe(ptr, pos, temp);
	strvect_char_heap(&temp, "ddd");
	pushspecial_unsafe(ptr, pos, temp);

	strvect_char_heap(&temp, "eee");
	setlexical_local(ptr, pos, temp);

	GetValueSymbol(pos, &check);
	test(string_equal_char(check, "aaaa"), "setsymlocal3");
	getlexical_local(ptr, pos, &check);
	test(string_equal_char(check, "eee"), "setsymlocal4");
	getspecial_local(ptr, pos, &check);
	test(string_equal_char(check, "eee"), "setsymlocal5");
	unsafe_push_local(ptr->local);

	RETURN;
}


static int testbreak_symbol(void)
{
	TestBreak(test_symbol_allocr);
	TestBreak(test_symbol_alloc);
	TestBreak(test_symbolp);
	TestBreak(test_keywordp);
	TestBreak(test_getname_symbol);
	TestBreak(test_getvalue_symbol);
	TestBreak(test_getfunction_symbol);
	TestBreak(test_getpackage_symbol);
	TestBreak(test_getplist_symbol);
	TestBreak(test_getinfo_constant);
	TestBreak(test_setinfo_constant);
	TestBreak(test_reminfo_constant);
	TestBreak(test_gettype_value_symbol);
	TestBreak(test_gettype_function_symbol);
	TestBreak(test_gettype_setf_symbol);
	TestBreak(test_inlinep_function_symbol);
	TestBreak(test_notinlinep_function_symbol);
	TestBreak(test_inlinep_setf_symbol);
	TestBreak(test_notinlinep_setf_symbol);
	TestBreak(test_getsetf_symbol);
	TestBreak(test_getscope_symbol);
	/* symstack */
	TestBreak(test_realloc_symbol);
	TestBreak(test_symstack);
	TestBreak(test_pushsymlocal);
	TestBreak(test_popsymlocal);
	TestBreak(test_clearsymlocal);
	TestBreak(test_getsymlocal);
	TestBreak(test_setsymlocal);

	return 0;
}

int test_symbol(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;
	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		lisp_init = 1;
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_package();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_common();
		result = testbreak_symbol();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

