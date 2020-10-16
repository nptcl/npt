#include "symbol.c"
#include "clos.h"
#include "character.h"
#include "common.h"
#include "condition.h"
#include "degrade.h"
#include "package.h"
#include "package_intern.h"
#include "stream.h"
#include "type_parse.h"
#include "type_table.h"

static int test_symbol_heap(void)
{
	addr pos, check;

	symbol_heap(&pos);
	test(GetType(pos) == LISPTYPE_SYMBOL, "symbol_heap.1");
	GetValueSymbol(pos, &check);
	test(check == Unbound, "symbol_heap.2");
	GetFunctionSymbol(pos, &check);
	test(check == Unbound, "symbol_heap.3");
	GetSpecialSymbol_Low(pos, &check);
	test(GetStatusSize(check) == LISPSIZE_ARRAY4, "symbol_heap.4");
	GetArrayA4(check, 0, &check);
	test(check == NULL, "symbol_heap.5");

	symbol_heap(&pos);
	test(GetType(pos) == LISPTYPE_SYMBOL, "symbol_heap.6");
	test(! GetStatusDynamic(pos), "symbol_heap.7");

	RETURN;
}

static int test_symbolp(void)
{
	addr pos;

	symbol_heap(&pos);
	test(symbolp(pos), "symbolp.1");
	test(symbolp(Nil), "symbolp.2");

	fixnum_heap(&pos, 100);
	test(! symbolp(pos), "symbolp.3");

	internchar_keyword_debug("HELLO", &pos);
	test(symbolp(pos), "symbolp.4");

	internchar_debug(LISP_COMMON_USER, "AAA", &pos);
	test(symbolp(pos), "symbolp.5");

	test(symbolp(T), "symbolp.6");

	RETURN;
}

static int test_keywordp(void)
{
	addr pos;

	internchar_keyword_debug("HELLO", &pos);
	test(keywordp(pos), "keywordp.1");

	symbol_heap(&pos);
	test(! keywordp(pos), "keywordp.2");
	test(! keywordp(Nil), "keywordp.3");

	fixnum_heap(&pos, 100);
	test(! keywordp(pos), "keywordp.4");

	internchar_debug(LISP_COMMON_USER, "AAA", &pos);
	test(! keywordp(pos), "keywordp.5");

	RETURN;
}

static int test_getname_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	strvect_char_heap(&left, "Hello");
	setarray(pos, SYMBOL_INDEX_NAME, left);
	GetNameSymbol(pos, &right);
	test(left == right, "getname_symbol.1");

	strvect_char_heap(&left, "AAA");
	SetNameSymbol(pos, left);
	GetNameSymbol(pos, &right);
	test(left == right, "setname_symbol.1");

	RETURN;
}

static int test_getvalue_symbol(void)
{
	addr pos, cons, left, right;

	symbol_heap(&pos);
	getarray(pos, SYMBOL_INDEX_VALUE, &cons);
	test(cons == Unbound, "getvalue_symbol.1");

	strvect_char_heap(&left, "Hello");
	setarray(pos, SYMBOL_INDEX_VALUE, left);
	GetValueSymbol(pos, &right);
	test(left == right, "getvalue_symbol.2");

	strvect_char_heap(&left, "AAA");
	SetValueSymbol(pos, left);
	GetValueSymbol(pos, &right);
	test(left == right, "setvalue_symbol.1");

	RETURN;
}

static int test_getfunction_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	strvect_char_heap(&left, "Hello");
	setarray(pos, SYMBOL_INDEX_FUNCTION, left);
	GetFunctionSymbol(pos, &right);
	test(left == right, "getfunction_symbol.1");

	strvect_char_heap(&left, "AAA");
	SetFunctionSymbol(pos, left);
	GetFunctionSymbol(pos, &right);
	test(left == right, "setfunction_symbol.1");

	RETURN;
}

static int test_getpackage_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	find_char_package_("COMMON-LISP", &left);
	setarray(pos, SYMBOL_INDEX_PACKAGE, left);
	GetPackageSymbol(pos, &right);
	test(left == right, "getpackage_symbol.1");

	find_char_package_("COMMON-LISP-USER", &left);
	SetPackageSymbol(pos, left);
	GetPackageSymbol(pos, &right);
	test(left == right, "setpackage_symbol.1");

	RETURN;
}

static int test_getplist_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	consnil_heap(&left);
	setarray(pos, SYMBOL_INDEX_PLIST, left);
	GetPlistSymbol(pos, &right);
	test(left == right, "getplist_symbol.1");

	consnil_heap(&left);
	SetPlistSymbol(pos, left);
	GetPlistSymbol(pos, &right);
	test(left == right, "setplist_symbol.1");

	SetPlistSymbol(pos, Nil);
	GetPlistSymbol(pos, &right);
	test(Nil == right, "setplist_symbol.2");

	RETURN;
}

static int test_getinfo_constant(void)
{
	addr pos, check;

	symbol_heap(&pos);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &check);
	test(check == Nil, "getinfo_constant.1");

	GetConstant(CONSTANT_SYSTEM_VALUE, &check);
	list_heap(&check, check, T, NULL);
	SetInfoSymbol_Low(pos, check);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &check);
	test(check == T, "getinfo_constant.2");

	RETURN;
}

static int test_setinfo_constant(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	fixnum_heap(&left, 100);
	setinfo_constant(pos, CONSTANT_SYSTEM_VALUE, left);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &right);
	test(left == right, "setinfo_constant.1");

	setinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, left);
	setinfo_constant(pos, CONSTANT_SYSTEM_VALUE, T);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &right);
	test(right == T, "setinfo_constant.2");

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
	reminfo_constant_(pos, CONSTANT_SYSTEM_VALUE);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &check);
	test(check == Nil, "reminfo_constant.1");
	getinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, &check);
	test(check == value2, "reminfo_constant.2");
	getinfo_constant(pos, CONSTANT_COMMON_SYMBOL, &check);
	test(check == value3, "reminfo_constant.3");

	symbol_heap(&pos);
	setinfo_constant(pos, CONSTANT_SYSTEM_VALUE, value1);
	setinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, value2);
	setinfo_constant(pos, CONSTANT_COMMON_SYMBOL, value3);
	reminfo_constant_(pos, CONSTANT_SYSTEM_FUNCTION);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &check);
	test(check == value1, "reminfo_constant.4");
	getinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, &check);
	test(check == Nil, "reminfo_constant.5");
	getinfo_constant(pos, CONSTANT_COMMON_SYMBOL, &check);
	test(check == value3, "reminfo_constant.6");

	symbol_heap(&pos);
	setinfo_constant(pos, CONSTANT_SYSTEM_VALUE, value1);
	setinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, value2);
	setinfo_constant(pos, CONSTANT_COMMON_SYMBOL, value3);
	reminfo_constant_(pos, CONSTANT_COMMON_SYMBOL);
	getinfo_constant(pos, CONSTANT_SYSTEM_VALUE, &check);
	test(check == value1, "reminfo_constant.7");
	getinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, &check);
	test(check == value2, "reminfo_constant.8");
	getinfo_constant(pos, CONSTANT_COMMON_SYMBOL, &check);
	test(check == Nil, "reminfo_constant.9");

	RETURN;
}

static int test_gettype_value_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	GetTypeTable(&left, Null);
	setinfo_constant(pos, CONSTANT_SYSTEM_VALUE, left);
	gettype_value_symbol(pos, &right);
	test(left == right, "gettype_value_symbol.1");

	GetTypeTable(&left, Package);
	settype_value_symbol(pos, left);
	gettype_value_symbol(pos, &right);
	test(left == right, "gettype_value_symbol.2");

	remtype_value_symbol(pos);
	gettype_value_symbol(pos, &right);
	test(right == Nil, "gettype_value_symbol.3");

	RETURN;
}

static int test_gettype_function_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	GetTypeTable(&left, Null);
	setinfo_constant(pos, CONSTANT_SYSTEM_FUNCTION, left);
	gettype_function_symbol(pos, &right);
	test(left == right, "gettype_function_symbol.1");

	GetTypeTable(&left, Package);
	settype_function_symbol(pos, left);
	gettype_function_symbol(pos, &right);
	test(left == right, "gettype_function_symbol.2");

	remtype_function_symbol_(pos);
	gettype_function_symbol(pos, &right);
	test(right == Nil, "gettype_function_symbol.3");

	RETURN;
}

static int test_gettype_setf_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	GetTypeTable(&left, Null);
	setinfo_constant(pos, CONSTANT_SYSTEM_SETF, left);
	gettype_setf_symbol(pos, &right);
	test(left == right, "gettype_setf_symbol.1");

	GetTypeTable(&left, Package);
	settype_setf_symbol(pos, left);
	gettype_setf_symbol(pos, &right);
	test(left == right, "gettype_setf_symbol.2");

	remtype_setf_symbol_(pos);
	gettype_setf_symbol(pos, &right);
	test(right == Nil, "gettype_setf_symbol.3");

	RETURN;
}

static int test_inlinep_function_symbol(void)
{
	addr pos, value;

	symbol_heap(&pos);
	test(! inlinep_function_symbol(pos), "inlinep_function_symbol.1");

	GetConstant(CONSTANT_COMMON_INLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
	test(inlinep_function_symbol(pos), "inlinep_function_symbol.2");

	GetConstant(CONSTANT_COMMON_NOTINLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
	test(! inlinep_function_symbol(pos), "inlinep_function_symbol.3");

	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, Nil);
	test(! inlinep_function_symbol(pos), "inlinep_function_symbol.4");

	setinline_function_symbol(pos);
	test(inlinep_function_symbol(pos), "inlinep_function_symbol.5");

	reminline_function_symbol(pos);
	test(! inlinep_function_symbol(pos), "inlinep_function_symbol.6");

	RETURN;
}

static int test_notinlinep_function_symbol(void)
{
	addr pos, value;

	symbol_heap(&pos);
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol.1");

	GetConstant(CONSTANT_COMMON_NOTINLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
	test(notinlinep_function_symbol(pos), "notinlinep_function_symbol.2");

	GetConstant(CONSTANT_COMMON_INLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, value);
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol.3");

	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_FUNCTION, Nil);
	test(! inlinep_function_symbol(pos), "notinlinep_function_symbol.4");
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol.5");

	setinline_function_symbol(pos);
	test(inlinep_function_symbol(pos), "notinlinep_function_symbol.6");
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol.7");

	setnotinline_function_symbol(pos);
	test(! inlinep_function_symbol(pos), "notinlinep_function_symbol.8");
	test(notinlinep_function_symbol(pos), "notinlinep_function_symbol.9");

	setinline_function_symbol(pos);
	test(inlinep_function_symbol(pos), "notinlinep_function_symbol.10");
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol.11");

	reminline_function_symbol(pos);
	test(! inlinep_function_symbol(pos), "notinlinep_function_symbol.12");
	test(! notinlinep_function_symbol(pos), "notinlinep_function_symbol.13");

	RETURN;
}

static int test_inlinep_setf_symbol(void)
{
	addr pos, value;

	symbol_heap(&pos);
	test(! inlinep_setf_symbol(pos), "inlinep_setf_symbol.1");

	GetConstant(CONSTANT_COMMON_INLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, value);
	test(inlinep_setf_symbol(pos), "inlinep_setf_symbol.2");

	GetConstant(CONSTANT_COMMON_NOTINLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, value);
	test(! inlinep_setf_symbol(pos), "inlinep_setf_symbol.3");

	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, Nil);
	test(! inlinep_setf_symbol(pos), "inlinep_setf_symbol.4");

	setinline_setf_symbol(pos);
	test(inlinep_setf_symbol(pos), "inlinep_setf_symbol.5");

	reminline_setf_symbol(pos);
	test(! inlinep_setf_symbol(pos), "inlinep_setf_symbol.6");

	RETURN;
}

static int test_notinlinep_setf_symbol(void)
{
	addr pos, value;

	symbol_heap(&pos);
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol.1");

	GetConstant(CONSTANT_COMMON_NOTINLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, value);
	test(notinlinep_setf_symbol(pos), "notinlinep_setf_symbol.2");

	GetConstant(CONSTANT_COMMON_INLINE, &value);
	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, value);
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol.3");

	setinfo_constant(pos, CONSTANT_SYSTEM_INLINE_SETF, Nil);
	test(! inlinep_setf_symbol(pos), "notinlinep_setf_symbol.4");
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol.5");

	setinline_setf_symbol(pos);
	test(inlinep_setf_symbol(pos), "notinlinep_setf_symbol.6");
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol.7");

	setnotinline_setf_symbol(pos);
	test(! inlinep_setf_symbol(pos), "notinlinep_setf_symbol.8");
	test(notinlinep_setf_symbol(pos), "notinlinep_setf_symbol.9");

	setinline_setf_symbol(pos);
	test(inlinep_setf_symbol(pos), "notinlinep_setf_symbol.10");
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol.11");

	reminline_setf_symbol(pos);
	test(! inlinep_setf_symbol(pos), "notinlinep_setf_symbol.12");
	test(! notinlinep_setf_symbol(pos), "notinlinep_setf_symbol.13");

	RETURN;
}

static int test_getsetf_symbol(void)
{
	addr pos, left, right;

	symbol_heap(&pos);
	getsetf_symbol(pos, &left);
	test(left == Unbound, "getsetf_symbol.1");

	consnil_heap(&left);
	setinfo_constant(pos, CONSTANT_COMMON_SETF, left);
	getsetf_symbol(pos, &right);
	test(left == right, "getsetf_symbol.2");

	conscar_heap(&left, left);
	setsetf_symbol(pos, left);
	getsetf_symbol(pos, &right);
	test(left == right, "getsetf_symbol.3");

	remsetf_symbol(pos);
	getsetf_symbol(pos, &right);
	test(right == Unbound, "getsetf_symbol.4");

	RETURN;
}

static int test_getscope_symbol(void)
{
	addr pos, check;

	symbol_heap(&pos);
	getscope_symbol(pos, &check);
	test(check == Nil, "getscope_symbol.1");
	test(lexicalp_symbol(pos), "getscope_symbol.2");
	test(! specialp_symbol(pos), "getscope_symbol.3");

	setinfo_constant(pos, CONSTANT_COMMON_SPECIAL, T);
	getscope_symbol(pos, &check);
	test(check == T, "getscope_symbol.4");
	test(! lexicalp_symbol(pos), "getscope_symbol.5");
	test(specialp_symbol(pos), "getscope_symbol.6");

	symbol_heap(&pos);
	setscope_symbol(pos, T);
	getscope_symbol(pos, &check);
	test(check == T, "getscope_symbol.7");
	test(! lexicalp_symbol(pos), "getscope_symbol.8");
	test(specialp_symbol(pos), "getscope_symbol.9");

	symbol_heap(&pos);
	setspecial_symbol(pos);
	test(! lexicalp_symbol(pos), "getscope_symbol.10");
	test(specialp_symbol(pos), "getscope_symbol.11");

	setlexical_symbol(pos);
	test(lexicalp_symbol(pos), "getscope_symbol.12");
	test(! specialp_symbol(pos), "getscope_symbol.13");

	RETURN;
}


/*
 *  symstack
 */
static int test_realloc_symbol(void)
{
	int check;
	addr pos, temp;
	fixnum i, fix;

	heap_array4(&pos, LISPTYPE_VECTOR, 10);
	for (i = 0; i < 10; i++) {
		fixnum_heap(&temp, i*10);
		SetArrayA4(pos, i, temp);
	}
	realloc_symbol(&pos, 10, 20);
	i = GetLenArrayA4(pos);
	test(i == 20, "realloc_symbol.1");
	check = 1;
	for (i = 0; i < 10; i++) {
		GetArrayA4(pos, i, &temp);
		GetFixnum(temp, &fix);
		if (fix != (i * 10)) {
			check = 0;
			break;
		}
	}
	test(check, "realloc_symbol.2");
	GetArrayA4(pos, i, &temp);
	test(temp == NULL, "realloc_symbol.3");

	RETURN;
}

static int test_symstack(void)
{
	addr symbol, array, value;
	size_t size, size2;

	symbol_heap(&symbol);
	GetSpecialSymbol_Low(symbol, &array);
	size = GetLenArrayA4(array);
	symstack(0, symbol, &value);
	test(value != Nil, "symstack.1");

	GetSpecialSymbol_Low(symbol, &array);
	size2 = GetLenArrayA4(array);
	test(size == size2, "symstack.2");

	symstack(size + 1, symbol, &value);
	test(value != Nil, "symstack.3");
#ifndef LISP_THREAD_DISABLE
	GetSpecialSymbol_Low(symbol, &array);
	size2 = GetLenArrayA4(array);
	test((size * 2) == size2, "symstack.4");
#endif

	RETURN;
}


/*
 *  symbol
 */
static int testcase_symbol(void)
{
	TestBreak(test_symbol_heap);
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

	return 0;
}

static void testinit_symbol(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_character();
	build_package();
	build_stream();
	build_symbol();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_common();
}

int test_symbol(void)
{
	DegradeTitle;
	return DegradeCode(symbol);
}

