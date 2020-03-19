#include "eval_scope.c"
#include "array.h"
#include "array_make.h"
#include "bignum.h"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "constant.h"
#include "copy.h"
#include "degrade.h"
#include "eval_declare.h"
#include "lambda.h"
#include "ratio.h"
#include "readtable.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  memory
 */
static int test_eval_scope_heap(void)
{
	addr pos;
	size_t size;

	init_eval_stack(Execute_Thread);
	eval_scope_heap(Execute_Thread, &pos, 0);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_scope_heap1");
	test(RefEvalType(pos) == EVAL_TYPE_SCOPE, "eval_scope_heap2");

	eval_scope_heap(Execute_Thread, &pos, 5);
	lenarray(pos, &size);
	test(5 <= size, "eval_scope_heap3");

	RETURN;
}

static void test_parse_type(addr *ret, addr type)
{
	if (parse_type(Execute_Thread, ret, type, Nil))
		_fmte("system error", NULL);
}

static int test_eval_scope_size(void)
{
	addr pos, type, value;

	readstring(&type, "integer");
	test_parse_type(&type, type);
	fixnum_heap(&value, 10);
	eval_scope_size(Execute_Thread, &pos, 5, EVAL_PARSE_T, type, value);
	test(RefEvalScopeType(pos) == EVAL_PARSE_T, "eval_scope_size1");
	test(RefEvalScopeThe(pos) == type, "eval_scope_size2");
	test(RefEvalScopeValue(pos) == value, "eval_scope_size3");

	RETURN;
}

static int test_make_eval_scope(void)
{
	addr pos, check;

	GetTypeTable(&pos, T);
	make_eval_scope(Execute_Thread, &pos, EVAL_PARSE_LET, pos, T);
	test(GetType(pos) == LISPTYPE_EVAL, "make_eval_scope1");
	test(RefEvalType(pos) == EVAL_TYPE_SCOPE, "make_eval_scope2");
	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_T, "make_eval_scope3");
	GetEvalScopeValue(pos, &check);
	test(check == T, "make_eval_scope4");

	RETURN;
}

static int test_StructEvalScope(void)
{
	enum EVAL_PARSE ptype;
	addr pos, type, value, check;
	struct eval_scope *str;

	readstring(&type, "integer");
	test_parse_type(&type, type);
	fixnum_heap(&value, 10);
	eval_scope_size(Execute_Thread, &pos, 5, EVAL_PARSE_T, type, value);
	str = StructEvalScope(pos);
	test(str->type == EVAL_PARSE_T, "StructEvalScope1");

	test(RefEvalScopeType(pos) == EVAL_PARSE_T, "RefEvalScopeType1");
	SetEvalScopeType(pos, EVAL_PARSE_NIL);
	GetEvalScopeType(pos, &ptype);
	test(ptype == EVAL_PARSE_NIL, "RefEvalScopeType2");

	test(RefEvalScopeThe(pos) == type, "RefEvalScopeThe1");
	SetEvalScopeThe(pos, T);
	GetEvalScopeThe(pos, &check);
	test(check == T, "RefEvalScopeThe2");

	test(RefEvalScopeValue(pos) == value, "RefEvalScopeValue1");
	fixnum_heap(&value, 20);
	SetEvalScopeValue(pos, value);
	GetEvalScopeValue(pos, &check);
	test(value == check, "RefEvalScopeValue2");

	fixnum_heap(&value, 30);
	SetEvalScopeIndex(pos, 4, value);
	test(RefEvalScopeIndex(pos, 4) == value, "RefEvalScopeIndex1");
	GetEvalScopeIndex(pos, 4, &check);
	test(value == check, "RefEvalScopeIndex2");

	RETURN;
}


/*
 *  scope constant
 */
static void eval_parse_execute(addr *ret, addr value)
{
	eval_parse(Execute_Thread, ret, value);
}

static void parse_eval_string(addr *ret, const char *str)
{
	readstring(ret, str);
	eval_parse_execute(ret, *ret);
}

static int test_scope_nil(void)
{
	addr eval, check;

	parse_eval_string(&eval, "nil");
	scope_nil(Execute_Thread, &eval, eval);
	test(eval_scope_p(eval), "scope_nil1");
	test(RefEvalScopeType(eval) == EVAL_PARSE_NIL, "scope_nil2");
	GetEvalScopeThe(eval, &check);
	test(RefLispDecl(check) == LISPDECL_NULL, "scope_nil3");
	GetEvalScopeValue(eval, &check);
	test(check == Nil, "scope_nil4");

	RETURN;
}

static int test_scope_t(void)
{
	addr eval, check;

	parse_eval_string(&eval, "t");
	scope_t(Execute_Thread, &eval, eval);
	test(eval_scope_p(eval), "scope_t1");
	test(RefEvalScopeType(eval) == EVAL_PARSE_T, "scope_t2");
	GetEvalScopeThe(eval, &check);
	test(RefLispDecl(check) == LISPDECL_BOOLEAN, "scope_t3");
	GetEvalScopeValue(eval, &check);
	test(check == T, "scope_t4");

	RETURN;
}

static int test_scope_integer(void)
{
	addr eval, type, check;

	parse_eval_string(&eval, "100");
	scope_integer(Execute_Thread, &eval, eval);
	test(eval_scope_p(eval), "scope_integer1");
	test(RefEvalScopeType(eval) == EVAL_PARSE_INTEGER, "scope_integer2");
	GetEvalScopeThe(eval, &type);
	test(RefLispDecl(type) == LISPDECL_INTEGER, "scope_integer3");
	GetArrayType(type, 0, &check);
	test(check == Nil, "scope_integer4");
	GetArrayType(type, 1, &check);
	test(RefFixnum(check) == 100, "scope_integer5");
	GetArrayType(type, 2, &check);
	test(check == Nil, "scope_integer6");
	GetArrayType(type, 3, &check);
	test(RefFixnum(check) == 100, "scope_integer7");
	GetEvalScopeValue(eval, &check);
	test(RefFixnum(check) == 100, "scope_integer8");

	RETURN;
}

static int test_scope_rational(void)
{
	addr eval, type, check;

	parse_eval_string(&eval, "100");
	scope_rational(Execute_Thread, &eval, eval);
	test(eval_scope_p(eval), "scope_rational1");
	test(RefEvalScopeType(eval) == EVAL_PARSE_RATIONAL, "scope_rational2");
	GetEvalScopeThe(eval, &type);
	test(RefLispDecl(type) == LISPDECL_RATIONAL, "scope_rational3");
	GetArrayType(type, 0, &check);
	test(check == Nil, "scope_rational4");
	GetArrayType(type, 1, &check);
	test(RefFixnum(check) == 100, "scope_rational5");
	GetArrayType(type, 2, &check);
	test(check == Nil, "scope_rational6");
	GetArrayType(type, 3, &check);
	test(RefFixnum(check) == 100, "scope_rational7");
	GetEvalScopeValue(eval, &check);
	test(RefFixnum(check) == 100, "scope_rational8");

	RETURN;
}

static int test_scope_character(void)
{
	addr eval, check;

	parse_eval_string(&eval, "#\\a");
	scope_character(Execute_Thread, &eval, eval);
	test(eval_scope_p(eval), "scope_character1");
	test(RefEvalScopeType(eval) == EVAL_PARSE_CHARACTER, "scope_character2");
	GetEvalScopeThe(eval, &check);
	test(RefLispDecl(check) == LISPDECL_STANDARD_CHAR, "scope_character3");
	GetEvalScopeValue(eval, &check);
	test(RefCharacter(check) == 'a', "scope_character4");

	RETURN;
}

static int test_scope_array(void)
{
	addr eval, check;

	GetTypeTable(&eval, T);
	array_make_array(&eval, fixnumh(1), eval, Unbound, Unbound,
			Nil, Nil, Nil, fixnumh(0));
	eval_parse_execute(&eval, eval);
	scope_array(Execute_Thread, &eval, eval);
	test(eval_scope_p(eval), "scope_array1");
	test(RefEvalScopeType(eval) == EVAL_PARSE_ARRAY, "scope_array2");
	GetEvalScopeThe(eval, &check);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_ARRAY, "scope_array3");
	GetEvalScopeValue(eval, &check);
	test(GetType(check) == LISPTYPE_ARRAY, "scope_array4");

	RETURN;
}

static int test_scope_vector(void)
{
	addr eval, check;

	parse_eval_string(&eval, "#(10 20 30)");
	scope_vector(Execute_Thread, &eval, eval);
	test(eval_scope_p(eval), "scope_vector1");
	test(RefEvalScopeType(eval) == EVAL_PARSE_VECTOR, "scope_vector2");
	GetEvalScopeThe(eval, &check);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_VECTOR, "scope_vector3");
	GetEvalScopeValue(eval, &check);
	test(GetType(check) == LISPTYPE_VECTOR, "scope_vector4");

	RETURN;
}

static int test_scope_string(void)
{
	addr eval, check;

	strvect_char_heap(&eval, "Hello");
	eval_parse_execute(&eval, eval);
	scope_string(Execute_Thread, &eval, eval);
	test(eval_scope_p(eval), "scope_string1");
	test(RefEvalScopeType(eval) == EVAL_PARSE_STRING, "scope_string2");
	GetEvalScopeThe(eval, &check);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_BASE_STRING, "scope_string3");
	GetEvalScopeValue(eval, &check);
	test(GetType(check) == LISPTYPE_STRING, "scope_string4");

	strarray_char_heap(&eval, "Hello");
	strarray_setc(eval, 3, 0xF0000000);
	eval_parse_execute(&eval, eval);
	scope_string(Execute_Thread, &eval, eval);
	test(eval_scope_p(eval), "scope_string5");
	test(RefEvalScopeType(eval) == EVAL_PARSE_STRING, "scope_string6");
	GetEvalScopeThe(eval, &check);
	test(RefLispDecl(check) == LISPDECL_SIMPLE_STRING, "scope_string7");
	GetEvalScopeValue(eval, &check);
	test(GetType(check) == LISPTYPE_ARRAY, "scope_string8");

	RETURN;
}

static int test_scope_float(void)
{
	addr eval, check;

	parse_eval_string(&eval, "10.2d0");
	scope_float(Execute_Thread, &eval, eval);
	test(eval_scope_p(eval), "scope_float1");
	test(RefEvalScopeType(eval) == EVAL_PARSE_FLOAT, "scope_float2");
	GetEvalScopeThe(eval, &check);
	test(RefLispDecl(check) == LISPDECL_DOUBLE_FLOAT, "scope_float3");
	GetEvalScopeValue(eval, &check);
	test(GetType(check) == LISPTYPE_DOUBLE_FLOAT, "scope_float4");

	RETURN;
}

static int test_scope_quote(void)
{
	addr eval, check, symbol;

	readstring(&symbol, "hello");
	eval_parse_execute(&eval, symbol);
	scope_quote(Execute_Thread, &eval, eval);
	test(eval_scope_p(eval), "scope_quote1");
	test(RefEvalScopeType(eval) == EVAL_PARSE_QUOTE, "scope_quote2");
	GetEvalScopeThe(eval, &check);
	test(RefLispDecl(check) == LISPDECL_SYMBOL, "scope_quote3");
	GetEvalScopeValue(eval, &check);
	test(check == symbol, "scope_quote4");

	RETURN;
}

static int test_scope_allcons(void)
{
	addr pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	scope_allcons(ptr, &pos, &check, Nil);
	test(pos == Nil, "scope_allcons1");
	test(RefLispDecl(check) == LISPDECL_NULL, "scope_allcons2");

	parse_eval_string(&pos, "(progn \"Hello\" #\\a 30)");
	GetEvalParse(pos, 0, &pos);
	scope_allcons(ptr, &pos, &check, pos);
	test(length_list_unsafe(pos) == 3, "scope_allcons3");
	test(RefLispDecl(check) == LISPDECL_INTEGER, "scope_allcons4");

	GetCons(pos, &check, &pos);
	test(RefEvalScopeType(check) == EVAL_PARSE_STRING, "scope_allcons5");
	GetCons(pos, &check, &pos);
	test(RefEvalScopeType(check) == EVAL_PARSE_CHARACTER, "scope_allcons6");
	GetCons(pos, &check, &pos);
	test(RefEvalScopeType(check) == EVAL_PARSE_INTEGER, "scope_allcons7");

	RETURN;
}

static int test_scope_progn(void)
{
	addr pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	parse_eval_string(&pos, "(progn)");
	scope_progn(ptr, &pos, pos);
	GetEvalScopeThe(pos, &check);
	test(GetType(check) == LISPTYPE_TYPE, "scope_progn1");
	test(RefLispDecl(check) == LISPDECL_NULL, "scope_progn2");
	GetEvalScopeValue(pos, &pos);
	test(pos == Nil, "scope_progn3");

	parse_eval_string(&pos, "(progn #\\a \"Hello\" 30)");
	scope_progn(ptr, &pos, pos);
	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "scope_progn4");
	GetEvalScopeValue(pos, &check);
	test(length_list_unsafe(check) == 3, "scope_progn5");

	RETURN;
}

static int test_scope_declaim(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	parse_eval_string(&pos, "(declaim (special aa bb))");
	scope_declaim(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_declaim1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_DECLAIM, "scope_declaim2");
	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_NULL, "scope_declaim3");
	GetEvalScopeValue(pos, &check);
	test(eval_declare_p(check), "scope_declaim4");

	getglobal_eval(ptr, &pos);
	GetEvalStackTable(pos, &check);
	getplist_constant(check, CONSTANT_SYSTEM_TYPE_SCOPE, &check);
	readstring(&pos, "aa");
	test(find_list_eq_unsafe(pos, check), "scope_declaim5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  apply_declare
 */
static void setplistplist_value(LocalRoot local, addr stack, addr pos, addr value)
{
	addr key, table;

	GetConstant(CONSTANT_SYSTEM_TABLE_VALUE, &key);
	GetEvalStackTable(stack, &table);
	if (setplistplist_local(local, table, key, pos, value, &table))
		SetEvalStackTable(stack, table);
}

static void setplistplist_function(LocalRoot local, addr stack, addr pos, addr value)
{
	addr key, table;

	GetConstant(CONSTANT_SYSTEM_TABLE_FUNCTION, &key);
	GetEvalStackTable(stack, &table);
	if (GetType(pos) != LISPTYPE_CALLNAME)
		parse_callname_local(local, &pos, pos);
	if (setplistplist_callname_local(local, table, key, pos, value, &table))
		SetEvalStackTable(stack, table);
}

static int test_find_tablevalue(void)
{
	addr control, stack, pos, value, check;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "aa");
	readstring(&value, "bb");
	setplistplist_value(local, stack, pos, value);

	readstring(&pos, "cc");
	readstring(&value, "dd");
	setplistplist_value(local, stack, pos, value);

	readstring(&pos, "aa");
	test(find_tablevalue(stack, pos, &check), "find_tablevalue1");
	readstring(&value, "bb");
	test(check == value, "find_tablevalue2");
	test(find_tablevalue(stack, pos, NULL), "find_tablevalue3");
	readstring(&pos, "bb");
	test(! find_tablevalue(stack, pos, NULL), "find_tablevalue4");
	readstring(&pos, "cc");
	test(find_tablevalue(stack, pos, &check), "find_tablevalue5");
	readstring(&value, "dd");
	test(check == value, "find_tablevalue6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_find_tablefunction(void)
{
	addr control, stack, pos, value, check;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "aa");
	readstring(&value, "bb");
	setplistplist_function(local, stack, pos, value);

	readstring(&pos, "cc");
	readstring(&value, "dd");
	setplistplist_function(local, stack, pos, value);

	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	test(find_tablefunction(stack, pos, &check), "find_tablefunction1");
	readstring(&value, "bb");
	test(check == value, "find_tablefunction2");
	test(find_tablefunction(stack, pos, NULL), "find_tablefunction3");
	readstring(&pos, "bb");
	parse_callname_local(local, &pos, pos);
	test(! find_tablefunction(stack, pos, NULL), "find_tablefunction4");
	readstring(&pos, "cc");
	parse_callname_local(local, &pos, pos);
	test(find_tablefunction(stack, pos, &check), "find_tablefunction5");
	readstring(&value, "dd");
	test(check == value, "find_tablefunction6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_check_value_declare(void)
{
	addr control, stack;
	addr key1, val1, key2, val2, key3, val3, list, key, value, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&key1, "aa");
	readstring(&val1, "bb");
	readstring(&key2, "cc");
	readstring(&val2, "dd");
	readstring(&key3, "ee");
	readstring(&val3, "ff");
	setplistplist_value(local, stack, key1, val1);
	setplistplist_value(local, stack, key3, val3);
	list_local(local, &list, key1, val1, key2, val2, key3, val3, NULL);
	pos = Nil;
	check_value_declare(ptr, stack, list, &pos);
	nreverse_list_unsafe(&list, pos);

	test(length_list_unsafe(list) == 1, "check_value_declare1");
	GetCar(list, &key);
	GetCons(key, &key, &value);
	test(eval_tablevalue_p(key), "check_value_declare2");
	getname_tablevalue(key, &key);
	readstring(&pos, "cc");
	test(key == pos, "check_value_declare3");
	readstring(&pos, "dd");
	test(value == pos, "check_value_declare4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_check_function_declare(void)
{
	addr control, stack;
	addr key1, val1, key2, val2, key3, val3, list, key, value, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&key1, "aa");
	readstring(&val1, "bb");
	readstring(&key2, "cc");
	readstring(&val2, "dd");
	readstring(&key3, "ee");
	readstring(&val3, "ff");
	parse_callname_local(local, &key1, key1);
	parse_callname_local(local, &key2, key2);
	parse_callname_local(local, &key3, key3);
	setplistplist_function(local, stack, key1, val1);
	setplistplist_function(local, stack, key3, val3);
	list_local(local, &list, key1, val1, key2, val2, key3, val3, NULL);
	pos = Nil;
	check_function_declare(ptr, stack, list, &pos);
	nreverse_list_unsafe(&list, pos);

	test(length_list_unsafe(list) == 1, "check_function_declare1");
	GetCar(list, &key);
	GetCons(key, &key, &value);
	test(eval_tablefunction_p(key), "check_function_declare2");
	getname_tablefunction(key, &key);
	readstring(&pos, "cc");
	parse_callname_local(local, &pos, pos);
	test(equal_callname(key, pos), "check_function_declare3");
	readstring(&pos, "dd");
	test(value == pos, "check_function_declare4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_check_declare_stack(void)
{
	addr control, stack, decl, pos, value, list, check, check2;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&decl, "((type integer aa bb) (ftype function cc dd ee))");
	parse_declare_heap(Execute_Thread, Nil, decl, &decl);

	readstring(&pos, "aa");
	readstring(&value, "hello1");
	setplistplist_value(local, stack, pos, value);
	readstring(&pos, "dd");
	readstring(&value, "hello2");
	setplistplist_function(local, stack, pos, value);

	check_declare_stack(ptr, stack, decl, &list);
	test(length_list_unsafe(list) == 3, "check_declare_stack1");

	GetCons(list, &pos, &list);
	GetCons(pos, &pos, &value);
	getname_tablevalue(pos, &pos);
	readstring(&check, "bb");
	test(pos == check, "check_declare_stack2");
	test(RefLispDecl(value) == LISPDECL_INTEGER, "check_declare_stack3");

	GetCons(list, &pos, &list);
	GetCons(pos, &pos, &value);
	getname_tablefunction(pos, &pos);
	readstring(&check, "cc");
	parse_callname_local(local, &check, check);
	readstring(&check2, "ee");
	parse_callname_local(local, &check2, check2);
	test(equal_callname(pos, check) || equal_callname(pos, check2),
			"check_declare_stack4");
	test(RefLispDecl(value) == LISPDECL_FUNCTION, "check_declare_stack5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_apply_declare(void)
{
	addr control, stack, decl, pos, value, list, check;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&decl, "((type integer aa bb) (ftype function cc dd ee))");
	parse_declare_heap(Execute_Thread, Nil, decl, &decl);

	readstring(&pos, "aa");
	readstring(&value, "hello1");
	setplistplist_value(local, stack, pos, value);
	readstring(&pos, "dd");
	readstring(&value, "hello2");
	setplistplist_function(local, stack, pos, value);

	apply_declare(ptr, stack, decl, &list);
	test(length_list_unsafe(list) == 3, "apply_declare1");

	GetConstant(CONSTANT_SYSTEM_TYPE_VALUE, &pos);
	GetEvalStackTable(stack, &list);
	readstring(&check, "aa");
	test(getplistplist(list, pos, check, &check) == 0, "apply_declare2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  let
 */
static int test_check_scope_variable(void)
{
	addr symbol;

	readstring(&symbol, "aaa");
	check_scope_variable(symbol);
	test(1, "check_scope_variable1");

	RETURN;
}

static int test_let_init(void)
{
	addr control, pos, var, init, check;
	Execute ptr;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(let ((a 10) (b (progn 20)) (c)) :hello)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &pos); /* let-args */
	str.args = pos;
	let_init(ptr, &str);
	pos = str.args;

	test(length_list_unsafe(pos) == 3, "let_init1");
	GetCons(pos, &var, &pos);
	GetCons(var, &var, &init);
	readstring(&check, "a");
	test(var == check, "let_init2");
	test(RefEvalScopeType(init) == EVAL_PARSE_INTEGER, "let_init3");

	GetCons(pos, &var, &pos);
	GetCons(var, &var, &init);
	readstring(&check, "b");
	test(var == check, "let_init4");
	test(RefEvalScopeType(init) == EVAL_PARSE_PROGN, "let_init5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_make_tablevalue_stack(void)
{
	int result;
	addr control, stack, value, symbol, check;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "aa");
	make_tablevalue_stack(local, &value, stack, symbol);
	test(eval_tablevalue_p(value), "make_tablevalue_stack1");

	result = find_tablevalue(stack, symbol, &check);
	test(result, "make_tablevalue_stack2");
	test(check == value, "make_tablevalue_stack3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_let_maketable(void)
{
	addr control, pos;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(let ((a 10) (b (progn 20)) (c)) :hello)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &pos); /* let-args */
	str.args = pos;
	let_init(ptr, &str);
	let_maketable(local, &str);

	readstring(&pos, "a");
	test(find_tablevalue(str.stack, pos, &pos), "let_maketable1");
	test(eval_tablevalue_p(pos), "let_maketable2");
	gettype_tablevalue(pos, &pos);
	test(type_asterisk_p(pos), "let_maketable3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_specialp_stack_tablevalue(void)
{
	int result, specialp;
	addr control, stack, symbol, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "aa");
	specialp = 0;
	result = specialp_stack_tablevalue(stack, symbol, &specialp);
	test(result == 0, "specialp_stack_tablevalue1");

	readstring(&pos, "((special bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = specialp_stack_tablevalue(stack, symbol, &specialp);
	test(result == 0, "specialp_stack_tablevalue2");

	readstring(&pos, "((special aa))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = specialp_stack_tablevalue(stack, symbol, &specialp);
	test(result, "specialp_stack_tablevalue3");
	test(specialp, "specialp_stack_tablevalue4");

	readstring(&symbol, "cc");
	make_tablevalue_stack(local, &pos, stack, symbol);
	result = specialp_stack_tablevalue(stack, symbol, &specialp);
	test(result, "specialp_stack_tablevalue5");
	test(! specialp, "specialp_stack_tablevalue6");

	setspecialp_tablevalue(pos, 1);
	result = specialp_stack_tablevalue(stack, symbol, &specialp);
	test(result, "specialp_stack_tablevalue7");
	test(specialp, "specialp_stack_tablevalue8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_specialp_tablevalue(void)
{
	int specialp;
	addr control, stack, symbol, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "specialp_tablevalue-temp-symbol");
	specialp = specialp_tablevalue(ptr, stack, symbol);
	test(! specialp, "specialp_tablevalue1");

	setspecial_symbol(symbol);
	specialp = specialp_tablevalue(ptr, stack, symbol);
	test(specialp, "specialp_tablevalue2");

	readstring(&symbol, "aa");
	readstring(&pos, "((special aa))");
	parse_declaim_heap(Execute_Thread, Nil, pos, &pos);
	apply_declaim_stack(ptr, pos);
	specialp = specialp_tablevalue(ptr, stack, symbol);
	test(specialp, "specialp_tablevalue3");

	readstring(&symbol, "bb");
	readstring(&pos, "((special bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	specialp = specialp_tablevalue(ptr, stack, symbol);
	test(specialp, "specialp_tablevalue4");

	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	specialp = specialp_tablevalue(ptr, stack, symbol);
	test(specialp, "specialp_tablevalue5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_dynamic_stack_tablevalue(void)
{
	int result, dynamic;
	addr control, stack, symbol, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "aa");
	dynamic = 999;
	result = dynamic_stack_tablevalue(stack, symbol, &dynamic);
	test(result == 0, "dynamic_stack_tablevalue1");

	readstring(&pos, "((dynamic-extent bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = dynamic_stack_tablevalue(stack, symbol, &dynamic);
	test(result == 0, "dynamic_stack_tablevalue2");

	readstring(&pos, "((dynamic-extent aa))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = dynamic_stack_tablevalue(stack, symbol, &dynamic);
	test(result, "dynamic_stack_tablevalue3");
	test(dynamic, "dynamic_stack_tablevalue4");

	readstring(&symbol, "cc");
	make_tablevalue_stack(local, &pos, stack, symbol);
	result = dynamic_stack_tablevalue(stack, symbol, &dynamic);
	test(result, "dynamic_stack_tablevalue5");
	test(! dynamic, "dynamic_stack_tablevalue6");

	setdynamic_tablevalue(pos, 1);
	result = dynamic_stack_tablevalue(stack, symbol, &dynamic);
	test(result, "dynamic_stack_tablevalue7");
	test(dynamic, "dynamic_stack_tablevalue8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_dynamic_tablevalue(void)
{
	int dynamic;
	addr control, stack, symbol, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "aa");
	dynamic = dynamic_tablevalue(stack, symbol);
	test(! dynamic, "dynamic_tablevalue1");

	readstring(&symbol, "bb");
	readstring(&pos, "((dynamic-extent bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	dynamic = dynamic_tablevalue(stack, symbol);
	test(dynamic, "dynamic_tablevalue2");

	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	dynamic = dynamic_tablevalue(stack, symbol);
	test(dynamic, "dynamic_tablevalue3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_ignore_stack_tablevalue(void)
{
	enum IgnoreType ignore;
	int result;
	addr control, stack, symbol, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "aa");
	result = ignore_stack_tablevalue(stack, symbol, &ignore);
	test(! result, "ignore_stack_tablevalue1");

	readstring(&pos, "((ignorable bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = ignore_stack_tablevalue(stack, symbol, &ignore);
	test(! result, "ignore_stack_tablevalue2");

	readstring(&pos, "((ignore aa))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = ignore_stack_tablevalue(stack, symbol, &ignore);
	test(result, "ignore_stack_tablevalue3");
	test(ignore == IgnoreType_Ignore, "ignore_stack_tablevalue4");

	readstring(&symbol, "bb");
	result = ignore_stack_tablevalue(stack, symbol, &ignore);
	test(result, "ignore_stack_tablevalue5");
	test(ignore == IgnoreType_Ignorable, "ignore_stack_tablevalue6");

	readstring(&symbol, "cc");
	make_tablevalue_stack(local, &pos, stack, symbol);
	result = ignore_stack_tablevalue(stack, symbol, &ignore);
	test(result, "ignore_stack_tablevalue7");
	test(ignore == IgnoreType_None, "ignore_stack_tablevalue8");

	setignore_tablevalue(pos, IgnoreType_Ignore);
	result = ignore_stack_tablevalue(stack, symbol, &ignore);
	test(result, "ignore_stack_tablevalue9");
	test(ignore == IgnoreType_Ignore, "ignore_stack_tablevalue10");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_ignore_tablevalue(void)
{
	enum IgnoreType ignore;
	addr control, stack, symbol, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "aa");
	ignore = ignore_tablevalue(stack, symbol);
	test(ignore == IgnoreType_None, "ignore_tablevalue1");

	readstring(&symbol, "bb");
	readstring(&pos, "((ignore bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	ignore = ignore_tablevalue(stack, symbol);
	test(ignore == IgnoreType_Ignore, "ignore_tablevalue2");

	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	ignore = ignore_tablevalue(stack, symbol);
	test(ignore == IgnoreType_Ignore, "ignore_tablevalue3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_type_free_tablevalue(void)
{
	addr control, stack, pos, value;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "((type integer aa bb cc) (string dd ee))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);

	readstring(&pos, "bb");
	test(type_free_tablevalue(stack, pos, &value), "type_free_tablevalue1");
	test(RefLispDecl(value) == LISPDECL_INTEGER, "type_free_tablevalue2");
	readstring(&pos, "zz");
	test(! type_free_tablevalue(stack, pos, &value), "type_free_tablevalue3");
	readstring(&pos, "ee");
	test(type_free_tablevalue(stack, pos, &value), "type_free_tablevalue4");
	test(RefLispDecl(value) == LISPDECL_STRING, "type_free_tablevalue5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_type_boundary_tablevalue(void)
{
	addr control, stack, pos, value;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);

	readstring(&pos, "aa");
	make_tablevalue_stack(local, &pos, stack, pos);
	readstring(&value, "integer");
	test_parse_type(&value, value);
	settype_tablevalue(pos, value);

	readstring(&pos, "bb");
	make_tablevalue_stack(local, &pos, stack, pos);
	readstring(&value, "string");
	test_parse_type(&value, value);
	settype_tablevalue(pos, value);

	readstring(&pos, "bb");
	test(type_boundary_tablevalue(stack, pos, &value), "type_boundary_tablevalue1");
	test(RefLispDecl(value) == LISPDECL_STRING, "type_boundary_tablevalue2");
	readstring(&pos, "zz");
	test(! type_boundary_tablevalue(stack, pos, &value), "type_boundary_tablevalue3");
	readstring(&pos, "aa");
	test(type_boundary_tablevalue(stack, pos, &value), "type_boundary_tablevalue4");
	test(RefLispDecl(value) == LISPDECL_INTEGER, "type_boundary_tablevalue5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_type_tablevalue_local(void)
{
	addr control, stack, pos, value;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "((type integer aa))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);

	readstring(&pos, "aa");
	type_tablevalue(ptr, local, stack, pos, 0, &pos);
	test(length_list_unsafe(pos) == 1, "type_tablevalue_local1");
	GetCar(pos, &value);
	test(RefLispDecl(value) == LISPDECL_INTEGER, "type_tablevalue_local2");

	stack = newstack_nil(ptr);
	readstring(&pos, "((type string aa))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);

	readstring(&pos, "aa");
	type_tablevalue(ptr, local, stack, pos, 0, &pos);
	test(length_list_unsafe(pos) == 2, "type_tablevalue_local3");
	GetCons(pos, &value, &pos);
	test(RefLispDecl(value) == LISPDECL_STRING, "type_tablevalue_local4");
	GetCar(pos, &value);
	test(RefLispDecl(value) == LISPDECL_INTEGER, "type_tablevalue_local5");

	readstring(&pos, "aa");
	make_tablevalue_stack(local, &pos, stack, pos);
	readstring(&value, "null");
	test_parse_type(&value, value);
	settype_tablevalue(pos, value);

	readstring(&pos, "aa");
	type_tablevalue(ptr, local, stack, pos, 0, &pos);
	test(length_list_unsafe(pos) == 2, "type_tablevalue_local6");
	GetCons(pos, &value, &pos);
	test(RefLispDecl(value) == LISPDECL_STRING, "type_tablevalue_local7");
	GetCar(pos, &value);
	test(RefLispDecl(value) == LISPDECL_NULL, "type_tablevalue_local8");

	readstring(&pos, "aa");
	type_tablevalue(ptr, local, stack, pos, 1, &pos);
	test(length_list_unsafe(pos) == 3, "type_tablevalue_local9");
	GetCons(pos, &value, &pos);
	test(RefLispDecl(value) == LISPDECL_STRING, "type_tablevalue_local10");
	GetCons(pos, &value, &pos);
	test(RefLispDecl(value) == LISPDECL_NULL, "type_tablevalue_local11");
	GetCar(pos, &value);
	test(RefLispDecl(value) == LISPDECL_INTEGER, "type_tablevalue_local12");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_type_tablevalue_global(void)
{
	addr control, geval, stack, pos, value, type;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "type-tablevalue-global-temp-symbol");
	type_tablevalue(ptr, local, stack, pos, 0, &value);
	test(value == Nil, "type_tablevalue_global1");

	readstring(&value, "null");
	test_parse_type(&value, value);
	settype_value_symbol(pos, value);
	type_tablevalue(ptr, local, stack, pos, 0, &value);
	test(singlep(value), "type_tablevalue_global2");
	GetCar(value, &value);
	test(RefLispDecl(value) == LISPDECL_NULL, "type_tablevalue_global3");

	getglobal_eval(ptr, &geval);
	make_tablevalue_stack(NULL, &value, geval, pos);
	readstring(&type, "integer");
	test_parse_type(&type, type);
	settype_tablevalue(value, type);

	type_tablevalue(ptr, local, stack, pos, 0, &value);
	test(singlep(value), "type_tablevalue_global4");
	GetCar(value, &value);
	test(RefLispDecl(value) == LISPDECL_INTEGER, "type_tablevalue_global5");

	readstring(&value, "((type string type-tablevalue-global-temp-symbol))");
	parse_declaim_heap(Execute_Thread, Nil, value, &value);
	apply_declaim_stack(ptr, value);
	type_tablevalue(ptr, local, stack, pos, 0, &value);
	test(singlep(value), "type_tablevalue_global6");
	GetCar(value, &value);
	test(RefLispDecl(value) == LISPDECL_STRING, "type_tablevalue_global7");

	make_tablevalue_stack(local, &value, stack, pos);
	readstring(&type, "null");
	test_parse_type(&type, type);
	settype_tablevalue(value, type);

	type_tablevalue(ptr, local, stack, pos, 0, &type);
	test(length_list_unsafe(type) == 1, "type_tablevalue_global8");
	GetCons(type, &value, &type);
	test(RefLispDecl(value) == LISPDECL_NULL, "type_tablevalue_global9");

	type_tablevalue(ptr, local, stack, pos, 1, &type);
	test(length_list_unsafe(type) == 2, "type_tablevalue_global10");
	GetCons(type, &value, &type);
	test(RefLispDecl(value) == LISPDECL_NULL, "type_tablevalue_global11");
	GetCons(type, &value, &type);
	test(RefLispDecl(value) == LISPDECL_STRING, "type_tablevalue_global12");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_type_and_array(void)
{
	addr pos, cons, check, pos1, pos2, pos3, pos4, pos5;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	test(type_and_array(local, Nil, &pos), "type_and_array1");

	GetTypeTable(&pos, Atom);
	list_local(local, &cons, pos, NULL);
	test(! type_and_array(local, cons, &check), "type_and_array2");
	test(check == pos, "type_and_array3");

	GetTypeTable(&pos1, Atom);
	GetTypeTable(&pos2, Null);
	GetTypeTable(&pos3, Asterisk);
	GetTypeTable(&pos4, T);
	GetTypeTable(&pos5, Fixnum);
	list_local(local, &cons, pos1, pos2, pos3, pos4, pos5, NULL);
	test(! type_and_array(local, cons, &check), "type_and_array4");
	test(RefLispDecl(check) == LISPDECL_AND, "type_and_array5");
	GetArrayType(check, 0, &check);
	test(lenarrayr(check) == 3, "type_and_array6");
	GetArrayA4(check, 0, &pos);
	test(RefLispDecl(pos) == LISPDECL_ATOM, "type_and_array7");
	GetArrayA4(check, 1, &pos);
	test(RefLispDecl(pos) == LISPDECL_NULL, "type_and_array8");
	GetArrayA4(check, 2, &pos);
	test(RefLispDecl(pos) == LISPDECL_FIXNUM, "type_and_array9");

	rollback_local(local, stack);

	RETURN;
}

static int test_push_tablevalue_alloc(void)
{
	addr control, stack, symbol, value, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "aa");
	push_tablevalue_alloc(ptr, local, stack, symbol, &value);
	test(! getspecialp_tablevalue(value), "push_tablevalue_alloc1");

	readstring(&symbol, "bb");
	readstring(&pos, "((special bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	push_tablevalue_alloc(ptr, local, stack, symbol, &value);
	test(getspecialp_tablevalue(value), "push_tablevalue_alloc2");

	getglobal_eval(ptr, &stack);
	readstring(&symbol, "bb");
	readstring(&pos, "((special bb))");
	parse_declaim_heap(Execute_Thread, Nil, pos, &pos);
	apply_declaim_stack(ptr, pos);
	push_tablevalue_alloc(ptr, NULL, stack, symbol, &value);
	test(getspecialp_tablevalue(value), "push_tablevalue_alloc3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_checktype_p(void)
{
	int check, warning;
	addr form, symbol;

	readstring(&form, "integer");
	test_parse_type(&form, form);
	readstring(&symbol, "real");
	test_parse_type(&symbol, symbol);
	warning = checktype_p(form, symbol, &check);
	test(! check, "checktype_p1");
	test(! warning, "checktype_p2");

	readstring(&form, "real");
	test_parse_type(&form, form);
	readstring(&symbol, "integer");
	test_parse_type(&symbol, symbol);
	warning = checktype_p(form, symbol, &check);
	test(check, "checktype_p3");
	test(! warning, "checktype_p4");

	readstring(&form, "string");
	test_parse_type(&form, form);
	readstring(&symbol, "integer");
	test_parse_type(&symbol, symbol);
	warning = checktype_p(form, symbol, &check);
	test(check, "checktype_p5");
	test(warning, "checktype_p6");

	RETURN;
}

static int test_checktype_value(void)
{
	addr control, stack, symbol, value, type, init;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "aa");
	push_tablevalue_alloc(ptr, local, stack, symbol, &value);
	readstring(&type, "integer");
	test_parse_type(&type, type);
	settype_tablevalue(value, type);

	readstring(&type, "fixnum");
	test_parse_type(&type, type);
	make_eval_scope(Execute_Thread, &init, EVAL_PARSE_EMPTY, type, Nil);
	checktype_value(value, init);
	test(getcheck_tablevalue(value) == 0, "checktype_value1");

	readstring(&type, "real");
	test_parse_type(&type, type);
	make_eval_scope(Execute_Thread, &init, EVAL_PARSE_EMPTY, type, Nil);
	checktype_value(value, init);
	test(getcheck_tablevalue(value), "checktype_value2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_let_applytable(void)
{
	addr control, pos, value, type;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(let ((aa 10) (bb 20)) (declare (special bb)) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args); /* let-args */
	GetEvalParse(pos, 1, &str.decl); /* let-decl */
	let_init(ptr, &str);
	apply_declare_stack(local, str.stack, str.decl);

	let_applytable(ptr, &str);
	readstring(&pos, "aa");
	test(find_tablevalue(str.stack, pos, &value), "let_applytable1");
	test(! getspecialp_tablevalue(value), "let_applytable2");
	readstring(&pos, "bb");
	test(find_tablevalue(str.stack, pos, &value), "let_applytable3");
	test(getspecialp_tablevalue(value), "let_applytable4");

	readstring(&pos, "aa");
	test(find_tablevalue(str.stack, pos, &value), "let_applytable5");
	readstring(&type, "fixnum");
	test_parse_type(&type, type);
	settype_tablevalue(value, type);

	readstring(&pos, "bb");
	test(find_tablevalue(str.stack, pos, &value), "let_applytable6");
	readstring(&type, "(satisfies hello)");
	test_parse_type(&type, type);
	settype_tablevalue(value, type);

	let_applytable(ptr, &str);
	readstring(&pos, "aa");
	test(find_tablevalue(str.stack, pos, &value), "let_applytable7");
	test(! getspecialp_tablevalue(value), "let_applytable8");
	test(! getcheck_tablevalue(value), "let_applytable9");
	readstring(&pos, "bb");
	test(find_tablevalue(str.stack, pos, &value), "let_applytable10");
	test(getspecialp_tablevalue(value), "let_applytable11");
	test(getcheck_tablevalue(value), "let_applytable12");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_ignore_checkvalue(void)
{
	addr control, pos, value;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(let ((aa 10) (bb 20)) (declare (special bb)) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args); /* let-args */
	GetEvalParse(pos, 1, &str.decl); /* let-decl */
	let_init(ptr, &str);
	apply_declare_stack(local, str.stack, str.decl);
	let_applytable(ptr, &str);

	readstring(&pos, "aa");
	find_tablevalue(str.stack, pos, &value);
	setreference_tablevalue(value, 1);
	readstring(&pos, "bb");
	find_tablevalue(str.stack, pos, &value);
	setreference_tablevalue(value, 1);
	ignore_checkvalue(str.stack);
	test(1, "ignore_checkvalue1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_tablevalue_update(void)
{
	addr control, stack, key, pos, value;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "aa");
	push_tablevalue_local(ptr, stack, pos, &value);
	GetConstant(CONSTANT_SYSTEM_TABLE_VALUE, &key);
	GetEvalStackTable(stack, &stack);
	test(getplist(stack, key, &stack) == 0, "tablevalue_update1");
	tablevalue_update(stack, &value, pos);
	test(eval_tablevalue_p(value), "tablevalue_update2");
	test(! GetStatusDynamic(value), "tablevalue_update3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_let_update(void)
{
	int check;
	addr control, pos, cons;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(let ((aa 10) (bb 20)) (declare (special bb)) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args); /* let-args */
	GetEvalParse(pos, 1, &str.decl); /* let-decl */
	let_init(ptr, &str);
	apply_declare_stack(local, str.stack, str.decl);
	let_applytable(ptr, &str);
	let_update(ptr, &str);

	check = 1;
	for (cons = str.args; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		GetCar(pos, &pos);
		if (GetStatusDynamic(pos)) {
			check = 0;
			break;
		}
	}
	test(check, "let_update1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_let_execute(void)
{
	addr control, pos;
	Execute ptr;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos,
			"(let ((aa 10) (bb 20)) "
			"  (declare (special bb) (ignorable aa bb)) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args); /* let-args */
	GetEvalParse(pos, 1, &str.decl); /* let-decl */
	GetEvalParse(pos, 2, &str.cons); /* let-decl */
	let_execute(ptr, &str);
	test(1, "let_execute1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_let(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos,
			"(let ((aa 10) (bb 20)) "
			"  (declare (special bb) (ignorable aa bb)) :aa)");
	eval_parse_execute(&pos, pos);
	scope_let(ptr, &pos, pos);
	test(RefEvalScopeType(pos) == EVAL_PARSE_LET, "scope_let1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_ifdeclvalue(void)
{
	int invalidp;
	addr control, stack, decl, symbol, value, check, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&decl, "()");
	parse_declare_heap(Execute_Thread, Nil, decl, &decl);
	readstring(&symbol, "aa");
	ifdeclvalue(ptr, stack, symbol, decl, &value);
	test(eval_tablevalue_p(value), "ifdeclvalue1");
	test(! getspecialp_tablevalue(value), "ifdeclvalue2");
	gettype_tablevalue(value, &check);
	test(type_asterisk_p(check), "ifdeclvalue3");

	readstring(&decl, "((special bb) (null bb))");
	parse_declare_heap(Execute_Thread, Nil, decl, &decl);
	readstring(&symbol, "bb");
	ifdeclvalue(ptr, stack, symbol, decl, &value);
	test(eval_tablevalue_p(value), "ifdeclvalue4");
	test(getspecialp_tablevalue(value), "ifdeclvalue5");
	gettype_tablevalue(value, &check);

	readstring(&pos, "null");
	test_parse_type(&pos, pos);
	test(subtypep_clang(check, pos, &invalidp), "ifdeclvalue6");

	ifdeclvalue(ptr, stack, symbol, decl, NULL);

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_leta_checktype(void)
{
	addr control, pos, value, type;
	Execute ptr;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(let ((aa 10) (bb 20)) (declare (special bb)) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args); /* let-args */
	GetEvalParse(pos, 1, &str.decl); /* let-decl */

	leta_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);

	readstring(&pos, "aa");
	test(find_tablevalue(str.stack, pos, &value), "leta_checktype1");
	readstring(&type, "fixnum");
	test_parse_type(&type, type);
	settype_tablevalue(value, type);

	readstring(&pos, "bb");
	test(find_tablevalue(str.stack, pos, &value), "leta_checktype2");
	readstring(&type, "(satisfies hello)");
	test_parse_type(&type, type);
	settype_tablevalue(value, type);

	leta_checktype(ptr, &str);
	readstring(&pos, "aa");
	test(find_tablevalue(str.stack, pos, &value), "leta_checktype3");
	test(! getspecialp_tablevalue(value), "leta_checktype4");
	test(! getcheck_tablevalue(value), "leta_checktype5");
	readstring(&pos, "bb");
	test(find_tablevalue(str.stack, pos, &value), "leta_checktype6");
	test(getspecialp_tablevalue(value), "leta_checktype7");
	test(getcheck_tablevalue(value), "leta_checktype8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_leta_execute(void)
{
	addr control, pos;
	Execute ptr;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos,
			"(let* ((aa 10) (bb 20)) "
			"  (declare (special bb) (ignorable aa bb)) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args); /* let-args */
	GetEvalParse(pos, 1, &str.decl); /* let-decl */
	GetEvalParse(pos, 2, &str.cons); /* let-decl */
	leta_execute(ptr, &str);
	test(1, "leta_execute1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_leta(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos,
			"(let* ((aa 10) (bb 20)) "
			"  (declare (special bb) (ignorable aa bb)) :aa)");
	eval_parse_execute(&pos, pos);
	scope_leta(ptr, &pos, pos);
	test(RefEvalScopeType(pos) == EVAL_PARSE_LETA, "scope_leta1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_let_special(void)
{
	addr control, pos, symbol, value;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(let (aa) (declare (special aa)) :hello)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args);
	GetEvalParse(pos, 1, &str.decl);
	let_init(ptr, &str);
	let_maketable(local, &str);
	apply_declare_stack(local, str.stack, str.decl);
	let_applytable(ptr, &str);

	readstring(&symbol, "aa");
	find_tablevalue(str.stack, symbol, &value);
	test(getspecialp_tablevalue(value), "let_special1");

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(let (aa) :hello)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args);
	GetEvalParse(pos, 1, &str.decl);
	let_init(ptr, &str);
	let_maketable(local, &str);
	apply_declare_stack(local, str.stack, str.decl);
	let_applytable(ptr, &str);

	readstring(&symbol, "aa");
	find_tablevalue(str.stack, symbol, &value);
	test(! getspecialp_tablevalue(value), "let_special2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  symbol
 */
static int test_symbol_global_tablevalue(void)
{
	int specialp, result;
	addr control, symbol, pos, value, stack;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&symbol, "aa");
	specialp = symbol_global_tablevalue(ptr, symbol, &pos);
	test(! specialp, "symbol_global_tablevalue1");
	getglobal_eval(ptr, &stack);
	result = find_tablevalue(stack, symbol, &value);
	test(result, "symbol_global_tablevalue2");
	test(! getspecialp_tablevalue(value), "symbol_global_tablevalue3");
	specialp = symbol_global_tablevalue(ptr, symbol, &pos);
	test(! specialp, "symbol_global_tablevalue4");

	readstring(&pos, "((special bb))");
	parse_declaim_heap(Execute_Thread, Nil, pos, &pos);
	apply_declaim_stack(ptr, pos);
	readstring(&symbol, "bb");
	specialp = symbol_global_tablevalue(ptr, symbol, &pos);
	test(specialp, "symbol_global_tablevalue5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_push_closure_value(void)
{
	int result;
	addr control, stack, symbol, value, check, table, key;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "aa");
	readstring(&value, "bb");
	push_closure_value(stack, symbol, value);
	GetConstant(CONSTANT_SYSTEM_CLOSURE_VALUE, &key);
	GetEvalStackTable(stack, &table);
	result = getplistplist(table, key, symbol, &check);
	test(result == 0, "push_closure_value1");
	test(check == value, "push_closure_value2");

	readstring(&check, "cc");
	push_closure_value(stack, symbol, check);
	GetEvalStackTable(stack, &table);
	result = getplistplist(table, key, symbol, &check);
	test(result == 0, "push_closure_value3");
	test(check == value, "push_closure_value4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_symbol_tablevalue(void)
{
	int specialp;
	addr control, stack, symbol, pos, key, table, value;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "((special bb))");
	parse_declaim_heap(Execute_Thread, Nil, pos, &pos);
	apply_declaim_stack(ptr, pos);
	readstring(&symbol, "bb");
	specialp = symbol_tablevalue(ptr, Nil, symbol, &pos);
	test(specialp, "symbol_tablevalue1");

	readstring(&symbol, "cc");
	readstring(&pos, "((special cc))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	push_tablevalue_local(ptr, stack, symbol, &pos);
	specialp = symbol_tablevalue(ptr, stack, symbol, &pos);
	test(specialp, "symbol_tablevalue2");

	readstring(&symbol, "dd");
	specialp = symbol_tablevalue(ptr, stack, symbol, &pos);
	test(! specialp, "symbol_tablevalue3");

	readstring(&pos, "((integer ee))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	readstring(&symbol, "ee");
	make_tablevalue_stack(local, &value, stack, symbol);
	apply_declare_value_stack(local, stack, symbol, pos);
	push_tablevalue_local(ptr, stack, symbol, &value);

	stack = newstack_lambda(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);

	readstring(&symbol, "cc");
	specialp = symbol_tablevalue(ptr, stack, symbol, &pos);
	test(specialp, "symbol_tablevalue4");

	readstring(&symbol, "dd");
	specialp = symbol_tablevalue(ptr, stack, symbol, &pos);
	test(! specialp, "symbol_tablevalue5");

	GetConstant(CONSTANT_SYSTEM_CLOSURE_VALUE, &key);
	GetEvalStackTable(stack, &table);
	specialp = getplistplist(table, key, symbol, &pos);
	test(! specialp , "symbol_tablevalue6");

	readstring(&symbol, "ee");
	specialp = symbol_tablevalue(ptr, stack, symbol, &pos);
	test(! specialp, "symbol_tablevalue7");

	GetConstant(CONSTANT_SYSTEM_CLOSURE_VALUE, &key);
	GetEvalStackTable(stack, &table);
	specialp = getplistplist(table, key, symbol, &pos);

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_find_symbol_scope(void)
{
	int specialp;
	addr control, symbol, value, stack;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&symbol, "aa");
	specialp = find_symbol_scope(ptr, symbol, &value);
	test(! specialp, "find_symbol_scope1");
	test(eval_tablevalue_p(value), "find_symbol_scope2");
	gettype_tablevalue(value, &value);
	test(RefLispDecl(value) == LISPDECL_ASTERISK, "find_symbol_scope3");

	getstack_eval(ptr, &stack);
	symbol_tablevalue(ptr, stack, symbol, &value);
	test(getreference_tablevalue(value), "find_symbol_scope4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_make_scope_symbol(void)
{
	addr control, symbol, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&symbol, "aa");
	make_scope_symbol(ptr, symbol, &pos);
	test(RefEvalScopeType(pos) == EVAL_PARSE_SYMBOL, "make_scope_symbol1");
	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_ASTERISK, "make_scope_symbol2");
	GetEvalScopeValue(pos, &check);
	test(check == symbol, "make_scope_symbol3");
	GetEvalScopeIndex(pos, 0, &check);
	test(eval_tablevalue_p(check), "make_scope_symbol4");
	test(! getspecialp_tablevalue(check), "make_scope_symbol5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_symbol_macrolet_global_p(void)
{
	addr control, stack, symbol, value, v1, v2;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&symbol, "aa");
	symbol_global_tablevalue(ptr, symbol, &value);
	setspecialp_tablevalue(value, 1);
	test(! symbol_macrolet_global_p(ptr, symbol, &value),
			"symbol_macrolet_global_p1");

	symbol = readr("bb");
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	getglobal_eval(ptr, &stack);
	push_symbol_macrolet(stack, symbol, v1, v2);
	test(symbol_macrolet_global_p(ptr, symbol, &value),
			"symbol_macrolet_global_p2");
	test(consp(value), "symbol_macrolet_global_p3");

	symbol = readr("test-symbol-macrolet-global-p");
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	cons_heap(&v1, v1, v2);
	setsymbol_macro_symbol(symbol, v1, Nil);
	test(symbol_macrolet_global_p(ptr, symbol, &value),
			"symbol_macrolet_global_p4");
	test(consp(value), "symbol_macrolet_global_p5");

	setspecial_symbol(symbol);
	test(! symbol_macrolet_global_p(ptr, symbol, &value),
			"symbol_macrolet_global_p6");
	test(consp(value), "symbol_macrolet_global_p7");

	remsymbol_macro_symbol(symbol);

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_symbol_macrolet_p(void)
{
	addr control, stack, symbol, value, v1, v2, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	stack = newstack_nil(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);

	symbol = readr("bb");
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	push_symbol_macrolet(stack, symbol, v1, v2);
	test(symbol_macrolet_p(ptr, symbol, &check), "symbol_macrolet_p1");
	test(consp(check), "symbol_macrolet_p2");

	symbol_tablevalue(ptr, stack, symbol, &value);
	GetEvalStackTable(stack, &v1);
	GetConst(SYSTEM_TABLE_VALUE, &v2);
	if (setplistplist_heap(v1, v2, symbol, value, &v1))
		SetEvalStackTable(stack, v1);
	test(! symbol_macrolet_p(ptr, symbol, &check), "symbol_macrolet_p3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_symbol_replace_function(Execute ptr,
		addr call, addr args, addr env)
{
	addr pos, check;

	GetConst(SYSTEM_SYMBOL_MACRO_EXPANDER, &check);
	GetFunctionSymbol(check, &check);
	if (check != call) {
		setresult_control(ptr, fixnumh(30));
		return 0;
	}

	eval_parse_execute(&pos, fixnumh(args == T? 10: 20));
	setresult_control(ptr, pos);
	return 0;
}

static int test_scope_symbol_replace(void)
{
	addr control, call, hook, pos;
	Execute ptr;

	ptr = Execute_Thread;
	init_eval_stack(ptr);
	push_close_control(ptr, &control);

	compiled_heap(&call, Nil);
	SetPointer(p_debug1, var3, test_scope_symbol_replace_function);
	setcompiled_var3(call, p_debug1);
	GetConst(SPECIAL_MACROEXPAND_HOOK, &hook);
	pushspecial_control(ptr, hook, call);
	cons_heap(&pos, T, Nil); /* (form . env) */
	scope_symbol_replace(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_symbol_replace1");
	GetEvalScopeValue(pos, &pos);
	test(RefFixnum(pos) == 10, "scope_symbol_replace2");

	free_control(ptr, control);
	free_eval_stack(ptr);

	RETURN;
}

static int test_scope_symbol(void)
{
	addr control, symbol, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&symbol, "aa");
	eval_parse_execute(&pos, symbol);
	scope_symbol(ptr, &pos, pos);
	test(RefEvalScopeType(pos) == EVAL_PARSE_SYMBOL, "scope_symbol1");

	readstring(&symbol, ":aa");
	eval_parse_execute(&pos, symbol);
	scope_symbol(ptr, &pos, pos);
	test(RefEvalScopeType(pos) == EVAL_PARSE_SYMBOL, "scope_symbol2");
	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_KEYWORD, "scope_symbol3");
	GetEvalScopeValue(pos, &check);
	test(check == symbol, "scope_symbol4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_setq_cons(void)
{
	addr control, pos, value, cons, type, symbol, check, stack;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	scope_setq_cons(ptr, Nil, &cons, &type);
	test(cons == Nil, "scope_setq_cons1");
	test(RefLispDecl(type) == LISPDECL_NULL, "scope_setq_cons2");

	readstring(&cons, "(setq aa 100 bb 200)");
	eval_parse_execute(&cons, cons);
	GetEvalParse(cons, 0, &cons);
	scope_setq_cons(ptr, cons, &cons, &type);
	test(length_list_unsafe(cons) == 2, "scope_setq_cons3");
	test(RefLispDecl(type) == LISPDECL_INTEGER, "scope_setq_cons4");
	GetCons(cons, &pos, &cons);
	GetCons(pos, &pos, &value);
	test(eval_tablevalue_p(pos), "scope_setq_cons5");
	getname_tablevalue(pos, &symbol);
	readstring(&check, "aa");
	test(symbol == check, "scope_setq_cons6");
	test(! getcheck_tablevalue(pos), "scope_setq_cons7");
	GetEvalScopeValue(value, &value);
	test(RefFixnum(value) == 100, "scope_setq_cons8");

	GetCons(cons, &pos, &cons);
	GetCons(pos, &pos, &value);
	test(eval_tablevalue_p(pos), "scope_setq_cons9");
	getname_tablevalue(pos, &symbol);
	readstring(&check, "bb");
	test(symbol == check, "scope_setq_cons10");
	test(! getcheck_tablevalue(pos), "scope_setq_cons11");
	GetEvalScopeValue(value, &value);
	test(RefFixnum(value) == 200, "scope_setq_cons12");

	stack = newstack_nil(ptr);
	readstring(&check, "cc");
	readstring(&pos, "((type (satisfies hello) cc))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	push_tablevalue_local(ptr, stack, check, &pos);

	readstring(&cons, "(setq cc 300)");
	eval_parse_execute(&cons, cons);
	GetEvalParse(cons, 0, &cons);
	scope_setq_cons(ptr, cons, &cons, &type);
	test(length_list_unsafe(cons) == 1, "scope_setq_cons13");
	test(RefLispDecl(type) == LISPDECL_INTEGER, "scope_setq_cons14");

	GetCons(cons, &pos, &cons);
	GetCons(pos, &pos, &value);
	test(eval_tablevalue_p(pos), "scope_setq_cons15");
	getname_tablevalue(pos, &symbol);
	readstring(&check, "cc");
	test(symbol == check, "scope_setq_cons16");
	test(getcheck_tablevalue(pos), "scope_setq_cons17");
	GetEvalScopeValue(value, &value);
	test(RefFixnum(value) == 300, "scope_setq_cons18");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_setq(void)
{
	addr control, cons, type;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&cons, "(setq aa 100 bb 200)");
	eval_parse_execute(&cons, cons);
	scope_setq(ptr, &cons, cons);
	test(RefEvalScopeType(cons) == EVAL_PARSE_SETQ, "scope_setq1");
	GetEvalScopeThe(cons, &type);
	test(RefLispDecl(type) == LISPDECL_INTEGER, "scope_setq2");
	GetEvalScopeValue(cons, &cons);
	test(length_list_unsafe(cons) == 2, "scope_setq3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  function
 */
static int test_globalp_stack_tablefunction(void)
{
	int result;
	addr control, stack, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "((ftype function bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);

	readstring(&pos, "cc");
	parse_callname_local(local, &pos, pos);
	make_tablefunction_stack(local, &pos, stack, pos);

	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	result = globalp_stack_tablefunction(stack, pos);
	test(result == 0, "globalp_stack_tablefunction1");

	readstring(&pos, "bb");
	parse_callname_local(local, &pos, pos);
	result = globalp_stack_tablefunction(stack, pos);
	test(result, "globalp_stack_tablefunction2");

	readstring(&pos, "cc");
	parse_callname_local(local, &pos, pos);
	result = globalp_stack_tablefunction(stack, pos);
	test(result, "globalp_stack_tablefunction3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_globalp_tablefunction(void)
{
	int result;
	addr control, stack, symbol, call, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "aa");
	parse_callname_local(local, &call, symbol);
	result = globalp_tablefunction(ptr, stack, call);
	test(result, "globalp_tablefunction1");

	readstring(&pos, "((ftype function aa))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = globalp_tablefunction(ptr, stack, call);
	test(! result, "globalp_tablefunction2");

	SetFunctionSymbol(symbol, T);
	result = globalp_tablefunction(ptr, stack, call);
	test(! result, "globalp_tablefunction3");
	SetFunctionSymbol(symbol, Unbound);
	result = globalp_tablefunction(ptr, stack, call);
	test(! result, "globalp_tablefunction4");

	readstring(&symbol, "bb");
	parse_callname_local(local, &call, symbol);
	readstring(&pos, "((ftype function aa))");
	parse_declaim_heap(Execute_Thread, Nil, pos, &pos);
	apply_declaim_stack(ptr, pos);
	result = globalp_tablefunction(ptr, stack, call);
	test(result, "globalp_tablefunction5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_dynamic_stack_tablefunction(void)
{
	int result, dynamic;
	addr control, stack, call, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	dynamic = 999;
	result = dynamic_stack_tablefunction(stack, call, &dynamic);
	test(result == 0, "dynamic_stack_tablefunction1");

	readstring(&pos, "((dynamic-extent #'bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = dynamic_stack_tablefunction(stack, call, &dynamic);
	test(result == 0, "dynamic_stack_tablefunction2");

	readstring(&pos, "((dynamic-extent #'aa))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = dynamic_stack_tablefunction(stack, call, &dynamic);
	test(result, "dynamic_stack_tablefunction3");
	test(dynamic, "dynamic_stack_tablefunction4");

	readstring(&call, "cc");
	parse_callname_local(local, &call, call);
	make_tablefunction_stack(local, &pos, stack, call);
	result = dynamic_stack_tablefunction(stack, call, &dynamic);
	test(result, "dynamic_stack_tablefunction5");
	test(! dynamic, "dynamic_stack_tablefunction6");

	setdynamic_tablefunction(pos, 1);
	result = dynamic_stack_tablefunction(stack, call, &dynamic);
	test(result, "dynamic_stack_tablefunction7");
	test(dynamic, "dynamic_stack_tablefunction8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_dynamic_tablefunction(void)
{
	int dynamic;
	addr control, stack, call, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	dynamic = dynamic_tablefunction(stack, call);
	test(! dynamic, "dynamic_tablefunction1");

	readstring(&call, "bb");
	parse_callname_local(local, &call, call);
	readstring(&pos, "((dynamic-extent #'bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	dynamic = dynamic_tablefunction(stack, call);
	test(dynamic, "dynamic_tablefunction2");

	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	dynamic = dynamic_tablefunction(stack, call);
	test(dynamic, "dynamic_tablefunction3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_ignore_stack_tablefunction(void)
{
	enum IgnoreType ignore;
	int result;
	addr control, stack, call, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	result = ignore_stack_tablefunction(stack, call, &ignore);
	test(! result, "ignore_stack_tablefunction1");

	readstring(&pos, "((ignorable #'bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = ignore_stack_tablefunction(stack, call, &ignore);
	test(! result, "ignore_stack_tablefunction2");

	readstring(&pos, "((ignore #'aa))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = ignore_stack_tablefunction(stack, call, &ignore);
	test(result, "ignore_stack_tablefunction3");
	test(ignore == IgnoreType_Ignore, "ignore_stack_tablefunction4");

	readstring(&call, "bb");
	parse_callname_local(local, &call, call);
	result = ignore_stack_tablefunction(stack, call, &ignore);
	test(result, "ignore_stack_tablefunction5");
	test(ignore == IgnoreType_Ignorable, "ignore_stack_tablefunction6");

	readstring(&call, "cc");
	parse_callname_local(local, &call, call);
	make_tablefunction_stack(local, &pos, stack, call);
	result = ignore_stack_tablefunction(stack, call, &ignore);
	test(result, "ignore_stack_tablefunction7");
	test(ignore == IgnoreType_None, "ignore_stack_tablefunction8");

	setignore_tablefunction(pos, IgnoreType_Ignore);
	result = ignore_stack_tablefunction(stack, call, &ignore);
	test(result, "ignore_stack_tablefunction9");
	test(ignore == IgnoreType_Ignore, "ignore_stack_tablefunction10");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_ignore_tablefunction(void)
{
	enum IgnoreType ignore;
	addr control, stack, call, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	ignore = ignore_tablefunction(stack, call);
	test(ignore == IgnoreType_None, "ignore_tablefunction1");

	readstring(&call, "bb");
	parse_callname_local(local, &call, call);
	readstring(&pos, "((ignore #'bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	ignore = ignore_tablefunction(stack, call);
	test(ignore == IgnoreType_Ignore, "ignore_tablefunction2");

	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	ignore = ignore_tablefunction(stack, call);
	test(ignore == IgnoreType_Ignore, "ignore_tablefunction3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_inline_stack_tablefunction(void)
{
	enum InlineType Inline;
	int result;
	addr control, stack, call, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	result = inline_stack_tablefunction(stack, call, &Inline);
	test(! result, "inline_stack_tablefunction1");

	readstring(&pos, "((notinline bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = inline_stack_tablefunction(stack, call, &Inline);
	test(! result, "inline_stack_tablefunction2");

	readstring(&pos, "((inline aa))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	result = inline_stack_tablefunction(stack, call, &Inline);
	test(result, "inline_stack_tablefunction3");
	test(Inline == InlineType_Inline, "inline_stack_tablefunction4");

	readstring(&call, "bb");
	parse_callname_local(local, &call, call);
	result = inline_stack_tablefunction(stack, call, &Inline);
	test(result, "inline_stack_tablefunction5");
	test(Inline == InlineType_NotInline, "inline_stack_tablefunction6");

	readstring(&call, "cc");
	parse_callname_local(local, &call, call);
	make_tablefunction_stack(local, &pos, stack, call);
	result = inline_stack_tablefunction(stack, call, &Inline);
	test(result, "inline_stack_tablefunction7");
	test(Inline == InlineType_None, "inline_stack_tablefunction8");

	setinline_tablefunction(pos, InlineType_Inline);
	result = inline_stack_tablefunction(stack, call, &Inline);
	test(result, "inline_stack_tablefunction9");
	test(Inline == InlineType_Inline, "inline_stack_tablefunction10");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_inline_tablefunction(void)
{
	enum InlineType Inline;
	addr control, stack, call, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	Inline = inline_tablefunction(ptr, stack, call);
	test(Inline == InlineType_None, "inline_tablefunction1");

	readstring(&call, "bb");
	parse_callname_local(local, &call, call);
	readstring(&pos, "((inline bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	Inline = inline_tablefunction(ptr, stack, call);
	test(Inline == InlineType_Inline, "inline_tablefunction2");

	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	Inline = inline_tablefunction(ptr, stack, call);
	test(Inline == InlineType_Inline, "inline_tablefunction3");

	readstring(&call, "cc");
	parse_callname_local(local, &call, call);
	readstring(&pos, "((notinline cc))");
	parse_declaim_heap(Execute_Thread, Nil, pos, &pos);
	apply_declaim_stack(ptr, pos);
	Inline = inline_tablefunction(ptr, stack, call);
	test(Inline == InlineType_NotInline, "inline_tablefunction4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_gettype_global_callname(void)
{
	addr symbol, call, pos;

	readstring(&symbol, "gettype-global-callname-temp-symbol");
	parse_callname_heap(&call, symbol);
	gettype_global_callname(NULL, call, &pos);
	test(pos == Nil, "gettype_global_callname1");

	GetTypeTable(&pos, Null);
	settype_function_symbol(symbol, pos);
	GetTypeTable(&pos, Atom);
	settype_setf_symbol(symbol, pos);

	gettype_global_callname(NULL, call, &pos);
	test(RefLispDecl(pos) == LISPDECL_NULL, "gettype_global_callname2");

	readstring(&call, "(setf gettype-global-callname-temp-symbol)");
	parse_callname_heap(&call, call);
	gettype_global_callname(NULL, call, &pos);
	test(RefLispDecl(pos) == LISPDECL_ATOM, "gettype_global_callname3");

	remtype_function_symbol(symbol);

	RETURN;
}

static int test_type_free_tablefunction(void)
{
	addr control, stack, pos, value;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos,
			"((ftype (function * integer) aa bb cc) "
			" (ftype (function * string) dd ee))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);

	readstring(&pos, "bb");
	parse_callname_local(local, &pos, pos);
	test(type_free_tablefunction(stack, pos, &value), "type_free_tablefunction1");
	test(RefLispDecl(value) == LISPDECL_FUNCTION, "type_free_tablefunction2");
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_INTEGER, "type_free_tablefunction3");
	readstring(&pos, "zz");
	parse_callname_local(local, &pos, pos);
	test(! type_free_tablefunction(stack, pos, &value), "type_free_tablefunction4");
	readstring(&pos, "ee");
	parse_callname_local(local, &pos, pos);
	test(type_free_tablefunction(stack, pos, &value), "type_free_tablefunction5");
	test(RefLispDecl(value) == LISPDECL_FUNCTION, "type_free_tablefunction6");
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_STRING, "type_free_tablefunction7");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_type_boundary_tablefunction(void)
{
	addr control, stack, pos, value;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);

	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	make_tablefunction_stack(local, &pos, stack, pos);
	readstring(&value, "(function * integer)");
	test_parse_type(&value, value);
	settype_tablefunction(pos, value);

	readstring(&pos, "bb");
	parse_callname_local(local, &pos, pos);
	make_tablefunction_stack(local, &pos, stack, pos);
	readstring(&value, "(function * string)");
	test_parse_type(&value, value);
	settype_tablefunction(pos, value);

	readstring(&pos, "bb");
	parse_callname_local(local, &pos, pos);
	test(type_boundary_tablefunction(stack, pos, &value),
			"type_boundary_tablefunction1");
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_STRING, "type_boundary_tablefunction2");
	readstring(&pos, "zz");
	parse_callname_local(local, &pos, pos);
	test(! type_boundary_tablefunction(stack, pos, &value),
			"type_boundary_tablefunction3");
	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	test(type_boundary_tablefunction(stack, pos, &value),
			"type_boundary_tablefunction4");
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_INTEGER, "type_boundary_tablefunction5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_type_tablefunction_local(void)
{
	addr control, stack, pos, value;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "((ftype (function * integer) aa))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);

	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	type_tablefunction(ptr, local, stack, pos, &pos);
	test(length_list_unsafe(pos) == 1, "type_tablefunction_local1");
	GetCar(pos, &value);
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_INTEGER, "type_tablefunction_local2");

	stack = newstack_nil(ptr);
	readstring(&pos, "((ftype (function * string) aa))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);

	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	type_tablefunction(ptr, local, stack, pos, &pos);
	test(length_list_unsafe(pos) == 2, "type_tablefunction_local3");
	GetCons(pos, &value, &pos);
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_STRING, "type_tablefunction_local4");
	GetCar(pos, &value);
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_INTEGER, "type_tablefunction_local5");

	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	make_tablefunction_stack(local, &pos, stack, pos);
	readstring(&value, "(function * null)");
	test_parse_type(&value, value);
	settype_tablefunction(pos, value);

	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	type_tablefunction(ptr, local, stack, pos, &pos);
	test(length_list_unsafe(pos) == 2, "type_tablefunction_local6");
	GetCons(pos, &value, &pos);
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_STRING, "type_tablefunction_local7");
	GetCar(pos, &value);
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_NULL, "type_tablefunction_local8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_type_tablefunction_global(void)
{
	addr control, geval, stack, symbol, pos, value, type;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&symbol, "type-tablefunction-global-temp-symbol");
	parse_callname_local(local, &pos, symbol);
	type_tablefunction(ptr, local, stack, pos, &value);
	test(value == Nil, "type_tablefunction_global1");

	readstring(&value, "(function * null)");
	test_parse_type(&value, value);
	settype_function_symbol(symbol, value);
	type_tablefunction(ptr, local, stack, pos, &value);
	test(singlep(value), "type_tablefunction_global2");
	GetCar(value, &value);
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_NULL, "type_tablefunction_global3");

	getglobal_eval(ptr, &geval);
	make_tablefunction_stack(NULL, &value, geval, pos);
	readstring(&type, "(function * integer)");
	test_parse_type(&type, type);
	settype_tablefunction(value, type);

	type_tablefunction(ptr, local, stack, pos, &value);
	test(singlep(value), "type_tablefunction_global4");
	GetCar(value, &value);
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_INTEGER, "type_tablefunction_global5");

	readstring(&value,
			"((ftype (function * string) "
			"  type-tablefunction-global-temp-symbol))");
	parse_declaim_heap(Execute_Thread, Nil, value, &value);
	apply_declaim_stack(ptr, value);
	type_tablefunction(ptr, local, stack, pos, &value);
	test(singlep(value), "type_tablefunction_global6");
	GetCar(value, &value);
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_STRING, "type_tablefunction_global7");

	make_tablefunction_stack(local, &value, stack, pos);
	readstring(&type, "(function * null)");
	test_parse_type(&type, type);
	settype_tablefunction(value, type);

	type_tablefunction(ptr, local, stack, pos, &type);
	test(length_list_unsafe(type) == 1, "type_tablefunction_global8");
	GetCons(type, &value, &type);
	GetArrayType(value, 1, &value);
	test(RefLispDecl(value) == LISPDECL_NULL, "type_tablefunction_global9");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_make_tablefunction_stack(void)
{
	int result;
	addr control, stack, value, call, check;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	make_tablefunction_stack(local, &value, stack, call);
	test(eval_tablefunction_p(value), "make_tablefunction_stack1");

	result = find_tablefunction(stack, call, &check);
	test(result, "make_tablefunction_stack2");
	test(check == value, "make_tablefunction_stack3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_push_tablefunction_alloc(void)
{
	addr control, stack, call, value, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);
	stack = newstack_nil(ptr);
	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	push_tablefunction_alloc(ptr, local, stack, call, &value);
	test(getglobalp_tablefunction(value), "push_tablefunction_alloc1");
	push_tablefunction_alloc(ptr, local, stack, call, &value);
	test(! getglobalp_tablefunction(value), "push_tablefunction_alloc2");

	readstring(&call, "bb");
	parse_callname_local(local, &call, call);
	readstring(&pos, "((dynamic-extent #'bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	apply_declare_stack(local, stack, pos);
	push_tablefunction_alloc(ptr, local, stack, call, &value);
	test(getdynamic_tablefunction(value), "push_tablefunction_alloc3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_callname_global_tablefunction(void)
{
	addr control, call, pos1, pos2;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	callname_global_tablefunction(ptr, &pos1, call);
	test(getglobalp_tablefunction(pos1), "callname_global_tablefunction1");
	callname_global_tablefunction(ptr, &pos2, call);
	test(pos1 == pos2, "callname_global_tablefunction2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_push_closure_function(void)
{
	int result;
	addr control, stack, call, value, check, table, key;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	readstring(&value, "bb");
	push_closure_function(stack, call, value);
	GetConstant(CONSTANT_SYSTEM_CLOSURE_FUNCTION, &key);
	GetEvalStackTable(stack, &table);
	result = getplistplist_callname(table, key, call, &check);
	test(result == 0, "push_closure_function1");
	test(check == value, "push_closure_function2");

	readstring(&check, "cc");
	parse_callname_local(local, &call, call);
	push_closure_function(stack, call, check);
	GetEvalStackTable(stack, &table);
	result = getplistplist_callname(table, key, call, &check);
	test(result == 0, "push_closure_function3");
	test(check == value, "push_closure_function4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_callname_tablefunction(void)
{
	int check;
	addr control, stack, call, pos, key, table, value, front;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	check = callname_tablefunction(ptr, Nil, call, &pos);
	test(check, "callname_tablefunction1");
	test(eval_tablefunction_p(pos), "callname_tablefunction2");

	check = callname_tablefunction(ptr, stack, call, &pos);
	test(check, "callname_tablefunction3");

	make_tablefunction_stack(local, &value, stack, call);
	check = callname_tablefunction(ptr, stack, call, &pos);
	test(! check, "callname_tablefunction4");

	stack = newstack_nil(ptr);
	check = callname_tablefunction(ptr, stack, call, &pos);
	test(! check, "callname_tablefunction5");

	stack = newstack_lambda(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	front = newstack_nil(ptr);
	check = callname_tablefunction(ptr, front, call, &pos);
	test(! check, "callname_tablefunction6");

	GetConstant(CONSTANT_SYSTEM_CLOSURE_FUNCTION, &key);
	GetEvalStackTable(stack, &table);
	check = getplistplist_callname(table, key, call, &pos);
	test(check == 0, "callname_tablefunction7");

	readstring(&call, "ee");
	parse_callname_local(local, &call, call);
	check = callname_tablefunction(ptr, front, call, &pos);
	test(check, "callname_tablefunction8");

	GetConstant(CONSTANT_SYSTEM_CLOSURE_FUNCTION, &key);
	GetEvalStackTable(stack, &table);
	check = getplistplist_callname(table, key, call, &pos);
	test(check, "callname_tablefunction9");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_function(void)
{
	addr control, pos, check, call;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	parse_eval_string(&pos, "(function hello)");
	scope_function(ptr, &pos, pos);
	test(RefEvalScopeType(pos) == EVAL_PARSE_FUNCTION, "scope_function1");
	GetEvalScopeValue(pos, &check);
	test(eval_tablefunction_p(check), "scope_function2");
	test(getreference_tablefunction(check), "scope_function3");
	getname_tablefunction(check, &check);
	readstring(&call, "hello");
	parse_callname_heap(&call, call);
	test(equal_callname(check, call), "scope_function4");
	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_FUNCTION, "scope_function5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  lambda
 */
static int test_lambda_init_var(void)
{
	addr control, stack, args, decl, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	eval_declare_local(local, &decl);
	parse_eval_string(&args, "(lambda (aa bb))");
	GetEvalParse(args, 0, &args); /* args */
	getnth(args, 0, &args); /* var */
	lambda_init_var(ptr, stack, args, decl);
	readstring(&pos, "aa");
	test(find_tablevalue(stack, pos, &pos), "lambda_init_var1");
	readstring(&pos, "bb");
	test(find_tablevalue(stack, pos, &pos), "lambda_init_var2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_init_opt(void)
{
	addr control, stack, args, decl, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	eval_declare_local(local, &decl);
	parse_eval_string(&args, "(lambda (&optional (aa 10 bb) cc))");
	GetEvalParse(args, 0, &args); /* args */
	getnth(args, 1, &args); /* optional */
	lambda_init_opt(ptr, stack, args, decl, &args);
	test(length_list_unsafe(args) == 2, "lambda_init_opt1");
	readstring(&pos, "aa");
	test(find_tablevalue(stack, pos, &pos), "lambda_init_opt2");
	readstring(&pos, "bb");
	test(find_tablevalue(stack, pos, &pos), "lambda_init_opt3");
	readstring(&pos, "cc");
	test(find_tablevalue(stack, pos, &pos), "lambda_init_opt4");

	GetCons(args, &pos, &args);
	getnth(pos, 1, &pos);
	test(eval_scope_p(pos), "lambda_init_opt5");
	GetCons(args, &pos, &args);
	getnth(pos, 2, &pos);
	test(pos == Nil, "lambda_init_opt6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_init_key(void)
{
	addr control, stack, args, decl, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	eval_declare_local(local, &decl);
	parse_eval_string(&args, "(lambda (&key ((name aa) 10 bb) cc))");
	GetEvalParse(args, 0, &args); /* args */
	getnth(args, 3, &args); /* key */
	lambda_init_key(ptr, stack, args, decl, &args);
	test(length_list_unsafe(args) == 2, "lambda_init_key1");
	readstring(&pos, "aa");
	test(find_tablevalue(stack, pos, &pos), "lambda_init_key2");
	readstring(&pos, "bb");
	test(find_tablevalue(stack, pos, &pos), "lambda_init_key3");
	readstring(&pos, "cc");
	test(find_tablevalue(stack, pos, &pos), "lambda_init_key4");

	GetCons(args, &pos, &args);
	getnth(pos, 1, &pos);
	test(symbolp(pos), "lambda_init_key5");
	GetCons(args, &pos, &args);
	getnth(pos, 3, &pos);
	test(pos == Nil, "lambda_init_key6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_init_aux(void)
{
	addr control, stack, args, decl, pos;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	eval_declare_local(local, &decl);
	parse_eval_string(&args, "(lambda (&aux (aa 10) cc))");
	GetEvalParse(args, 0, &args); /* args */
	getnth(args, 5, &args); /* aux */
	lambda_init_aux(ptr, stack, args, decl, &args);
	test(length_list_unsafe(args) == 2, "lambda_init_aux1");
	readstring(&pos, "aa");
	test(find_tablevalue(stack, pos, &pos), "lambda_init_aux2");
	readstring(&pos, "bb");
	test(! find_tablevalue(stack, pos, &pos), "lambda_init_aux3");
	readstring(&pos, "cc");
	test(find_tablevalue(stack, pos, &pos), "lambda_init_aux4");

	GetCar(args, &args);
	getnth(args, 0, &pos);
	test(symbolp(pos), "lambda_init_aux5");
	getnth(args, 1, &pos);
	test(eval_scope_p(pos), "lambda_init_aux6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_init(void)
{
	addr control, pos;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	eval_declare_local(local, &str.decl);
	parse_eval_string(&pos,
			"(lambda (aa &optional bb &rest cc &key dd &allow-other-keys &aux ee))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	readstring(&pos, "aa");
	test(find_tablevalue(str.stack, pos, &pos), "lambda_init1");
	readstring(&pos, "bb");
	test(find_tablevalue(str.stack, pos, &pos), "lambda_init2");
	readstring(&pos, "cc");
	test(find_tablevalue(str.stack, pos, &pos), "lambda_init3");
	readstring(&pos, "dd");
	test(find_tablevalue(str.stack, pos, &pos), "lambda_init4");
	readstring(&pos, "ee");
	test(find_tablevalue(str.stack, pos, &pos), "lambda_init5");

	getnth(str.args, 1, &pos); /* optional */
	getnth(pos, 0, &pos);
	getnth(pos, 1, &pos); /* init */
	test(eval_scope_p(pos), "lambda_init6");

	parse_eval_string(&pos, "(lambda ())");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	test(1, "lambda_init6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_tablevalue_var(void)
{
	addr control, pos, args;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	eval_declare_local(local, &str.decl);
	parse_eval_string(&pos, "(lambda (aa bb))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	getnth(str.args, 0, &args); /* var */
	lambda_tablevalue_var(local, str.stack, args, &args);
	test(length_list_unsafe(args) == 2, "lambda_tablevalue_var1");
	GetCons(args, &pos, &args);
	test(eval_tablevalue_p(pos), "lambda_tablevalue_var2");
	GetCons(args, &pos, &args);
	test(eval_tablevalue_p(pos), "lambda_tablevalue_var3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_tablevalue_opt(void)
{
	addr control, pos, args, check;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	eval_declare_local(local, &str.decl);
	parse_eval_string(&pos, "(lambda (&optional aa (bb 100 cc)))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	getnth(str.args, 1, &args); /* optional */
	lambda_tablevalue_opt(local, str.stack, args, &args);
	test(length_list_unsafe(args) == 2, "lambda_tablevalue_opt1");
	GetCons(args, &pos, &args);
	test(length_list_unsafe(pos) == 3, "lambda_tablevalue_opt2");
	getnth(pos, 0, &check);
	test(eval_tablevalue_p(check), "lambda_tablevalue_opt3");
	getnth(pos, 2, &check);
	test(check == Nil, "lambda_tablevalue_opt4");
	GetCons(args, &pos, &args);
	getnth(pos, 0, &check);
	test(eval_tablevalue_p(check), "lambda_tablevalue_opt5");
	getnth(pos, 2, &check);
	test(eval_tablevalue_p(check), "lambda_tablevalue_opt6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_tablevalue_key(void)
{
	addr control, pos, args, check;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	eval_declare_local(local, &str.decl);
	parse_eval_string(&pos, "(lambda (&key aa (bb 100 cc)))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	getnth(str.args, 3, &args); /* key */
	lambda_tablevalue_key(local, str.stack, args, &args);
	test(length_list_unsafe(args) == 2, "lambda_tablevalue_key1");
	GetCons(args, &pos, &args);
	test(length_list_unsafe(pos) == 4, "lambda_tablevalue_key2");
	getnth(pos, 0, &check);
	test(eval_tablevalue_p(check), "lambda_tablevalue_key3");
	getnth(pos, 3, &check);
	test(check == Nil, "lambda_tablevalue_key4");
	GetCons(args, &pos, &args);
	getnth(pos, 0, &check);
	test(eval_tablevalue_p(check), "lambda_tablevalue_key5");
	getnth(pos, 3, &check);
	test(eval_tablevalue_p(check), "lambda_tablevalue_key6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_tablevalue_aux(void)
{
	addr control, pos, args, check;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	eval_declare_local(local, &str.decl);
	parse_eval_string(&pos, "(lambda (&aux aa (bb 100)))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	getnth(str.args, 5, &args); /* aux */
	lambda_tablevalue_aux(local, str.stack, args, &args);
	test(length_list_unsafe(args) == 2, "lambda_tablevalue_aux1");
	GetCons(args, &pos, &args);
	test(length_list_unsafe(pos) == 2, "lambda_tablevalue_aux2");
	getnth(pos, 0, &check);
	test(eval_tablevalue_p(check), "lambda_tablevalue_aux3");
	GetCons(args, &pos, &args);
	getnth(pos, 0, &check);
	test(eval_tablevalue_p(check), "lambda_tablevalue_aux4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_tablevalue(void)
{
	addr control, pos;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	eval_declare_local(local, &str.decl);
	parse_eval_string(&pos,
			"(lambda (aa &optional bb &rest cc &key dd &allow-other-keys &aux ee))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	lambda_tablevalue(local, &str);
	getnth(str.args, 2, &pos); /* aux */
	test(eval_tablevalue_p(pos), "lambda_tablevalue1");
	getnth(str.args, 4, &pos); /* allow-other-keys */
	test(pos == T, "lambda_tablevalue2");

	parse_eval_string(&pos, "(lambda ())");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	lambda_tablevalue(local, &str);
	test(1, "lambda_tablevalue3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_type_ordinary_var(void)
{
	int invalid;
	addr control, pos, args, check;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "((type integer aa) (type string bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &str.decl);

	parse_eval_string(&pos, "(lambda (aa bb))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	getnth(str.args, 0, &args); /* var */
	type_ordinary_var(local, args, &args);
	test(length_list_unsafe(args) == 2, "type_ordinary_var1");
	GetCons(args, &pos, &args);
	readstring(&check, "integer");
	test_parse_type(&check, check);
	test(subtypep_clang(pos, check, &invalid), "type_ordinary_var2");
	test(subtypep_clang(check, pos, &invalid), "type_ordinary_var3");
	GetCons(args, &pos, &args);
	readstring(&check, "string");
	test_parse_type(&check, check);
	test(subtypep_clang(pos, check, &invalid), "type_ordinary_var4");
	test(subtypep_clang(check, pos, &invalid), "type_ordinary_var5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_type_ordinary_opt(void)
{
	int invalid;
	addr control, pos, args, check;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "((type integer aa) (type string bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &str.decl);

	parse_eval_string(&pos, "(lambda (&optional (aa 10) (bb \"Hello\")) :hello)");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	getnth(str.args, 1, &args); /* optional */
	type_ordinary_opt(local, args, &args);
	test(length_list_unsafe(args) == 2, "type_ordinary_opt1");
	GetCons(args, &pos, &args);
	readstring(&check, "integer");
	test_parse_type(&check, check);
	test(subtypep_clang(pos, check, &invalid), "type_ordinary_opt2");
	test(subtypep_clang(check, pos, &invalid), "type_ordinary_opt3");
	GetCons(args, &pos, &args);
	readstring(&check, "string");
	test_parse_type(&check, check);
	test(subtypep_clang(pos, check, &invalid), "type_ordinary_opt4");
	test(subtypep_clang(check, pos, &invalid), "type_ordinary_opt5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_type_ordinary_rest(void)
{
	addr pos;

	pos = Nil;
	type_ordinary_rest(NULL, pos, &pos);
	test(pos == Nil, "type_ordinary_rest1");

	pos = T;
	type_ordinary_rest(NULL, pos, &pos);
	test(RefLispDecl(pos) == LISPDECL_T, "type_ordinary_rest2");

	RETURN;
}

static int test_type_ordinary_key(void)
{
	int invalid;
	addr control, pos, args, check, key, value;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "((type integer aa) (type string bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &str.decl);

	parse_eval_string(&pos, "(lambda (&key ((hello aa) 10) (bb \"Hello\")))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	getnth(str.args, 3, &args); /* optional */
	type_ordinary_key(local, args, Nil, &args);
	test(length_list_unsafe(args) == 2, "type_ordinary_key1");
	GetCons(args, &pos, &args);
	GetCons(pos, &key, &value);
	readstring(&check, "hello");
	test(key == check, "type_ordinary_key2");
	readstring(&check, "integer");
	test_parse_type(&check, check);
	test(subtypep_clang(value, check, &invalid), "type_ordinary_key3");
	test(subtypep_clang(check, value, &invalid), "type_ordinary_key4");
	GetCons(args, &pos, &args);
	GetCons(pos, &key, &value);
	readstring(&check, ":bb");
	test(key == check, "type_ordinary_key5");
	readstring(&check, "string");
	test_parse_type(&check, check);
	test(subtypep_clang(value, check, &invalid), "type_ordinary_key6");
	test(subtypep_clang(check, value, &invalid), "type_ordinary_key7");

	type_ordinary_key(local, Nil, T, &check);
	test(check == T, "type_ordinary_key8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_make_type_ordinary(void)
{
	addr control, pos;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "((type integer aa) (type string bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &str.decl);

	parse_eval_string(&pos, "(lambda ())");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	make_type_ordinary(local, str.args, &pos);
	test(lenarrayr(pos) == 4, "make_type_ordinary1");
	test(refarray(pos, 0) == Nil, "make_type_ordinary2");
	test(refarray(pos, 1) == Nil, "make_type_ordinary3");
	test(refarray(pos, 2) == Nil, "make_type_ordinary4");
	test(refarray(pos, 3) == Nil, "make_type_ordinary5");

	newstack_nil(ptr);
	parse_eval_string(&pos,
			"(lambda (aa bb &optional cc (dd 10) &rest ee "
			" &key ff &allow-other-keys &aux gg))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	make_type_ordinary(local, str.args, &pos);
	test(refarray(pos, 0) != Nil, "make_type_ordinary6");
	test(refarray(pos, 1) != Nil, "make_type_ordinary7");
	test(refarray(pos, 2) != Nil, "make_type_ordinary8");
	test(refarray(pos, 3) != Nil, "make_type_ordinary9");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_type_incomplete(void)
{
	addr control, pos, check;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "((type integer aa) (type string bb))");
	parse_declare_heap(Execute_Thread, Nil, pos, &str.decl);

	parse_eval_string(&pos, "(lambda ())");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);

	lambda_type_incomplete(local, str.args, &pos);
	test(RefLispDecl(pos) == LISPDECL_FUNCTION, "lambda_type_incomplete1");
	GetArrayType(pos, 0, &check);
	test(lenarrayr(check) == 4, "lambda_type_incomplete2");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "lambda_type_incomplete3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_declare(void)
{
	addr control, pos, check;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.call = Nil;
	str.table = Nil;
	str.the = Nil;
	str.stack = newstack_nil(ptr);
	eval_declare_alloc(local, &str.decl);
	parse_eval_string(&pos, "(lambda (aa))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	lambda_declare(local, &str);
	test(str.table == Nil, "lambda_declare1");
	test(RefLispDecl(str.the) == LISPDECL_FUNCTION, "lambda_declare2");

	readstring(&pos, "hello");
	parse_callname_local(local, &str.call, pos);
	str.table = Nil;
	str.the = Nil;
	str.globalp = 1;
	str.stack = newstack_nil(ptr);
	eval_declare_alloc(local, &str.decl);
	parse_eval_string(&pos, "(lambda (aa))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	lambda_declare(local, &str);
	test(str.table != Nil, "lambda_declare3");

	test(! getglobalp_tablefunction(str.table), "lambda_declare4");
	gettype_tablefunction(str.table, &pos);
	test(RefLispDecl(pos) == LISPDECL_FUNCTION, "lambda_declare5");
	test(RefLispDecl(str.the) == LISPDECL_FUNCTION, "lambda_declare6");
	GetArrayType(pos, 0, &check);
	test(! type_asterisk_p(check), "lambda_declare7");
	GetArrayType(pos, 1, &check);
	test(type_asterisk_p(check), "lambda_declare8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_progn(void)
{
	addr control, pos, check;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "hello");
	parse_callname_local(local, &str.call, pos);
	str.table = Nil;
	str.the = Nil;
	str.globalp = 1;
	str.stack = newstack_nil(ptr);
	parse_eval_string(&pos, "(lambda (aa) 100)");
	GetEvalParse(pos, 0, &str.args); /* args */
	GetEvalParse(pos, 1, &str.decl); /* decl */
	GetEvalParse(pos, 3, &str.cons); /* cons */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	lambda_declare(local, &str);
	lambda_progn(ptr, &str);

	pos = str.the;
	test(RefLispDecl(pos) == LISPDECL_FUNCTION, "lambda_progn1");
	GetArrayType(pos, 0, &check);
	test(! type_asterisk_p(check), "lambda_progn2");
	GetArrayType(pos, 1, &check);
	test(! type_asterisk_p(check), "lambda_progn3");

	gettype_tablefunction(str.table, &pos);
	GetArrayType(pos, 1, &check);
	test(! type_asterisk_p(check), "lambda_progn4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_update_var(void)
{
	addr control, pos;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.call = Nil;
	str.table = Nil;
	str.stack = newstack_nil(ptr);
	eval_declare_alloc(local, &str.decl);
	parse_eval_string(&pos, "(lambda (aa bb))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	lambda_declare(local, &str);
	getnth(str.args, 0, &pos); /* var */
	lambda_update_var(&pos, pos);
	test(! GetStatusDynamic(pos), "lambda_update_var1");
	test(length_list_unsafe(pos) == 2, "lambda_update_var2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_update_opt(void)
{
	addr control, pos;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.call = Nil;
	str.table = Nil;
	str.stack = newstack_nil(ptr);
	eval_declare_alloc(local, &str.decl);
	parse_eval_string(&pos, "(lambda (&optional aa (bb 10 cc)))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	lambda_declare(local, &str);
	getnth(str.args, 1, &pos); /* optional */
	lambda_update_opt(&pos, pos);
	test(! GetStatusDynamic(pos), "lambda_update_opt1");
	test(length_list_unsafe(pos) == 2, "lambda_update_opt2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_update_key(void)
{
	addr control, pos;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.call = Nil;
	str.table = Nil;
	str.stack = newstack_nil(ptr);
	eval_declare_alloc(local, &str.decl);
	parse_eval_string(&pos, "(lambda (&key aa ((hello bb) 10 cc)))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	lambda_declare(local, &str);
	getnth(str.args, 3, &pos); /* key */
	lambda_update_key(&pos, pos);
	test(! GetStatusDynamic(pos), "lambda_update_key1");
	test(length_list_unsafe(pos) == 2, "lambda_update_key2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_update_aux(void)
{
	addr control, pos;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.call = Nil;
	str.table = Nil;
	str.stack = newstack_nil(ptr);
	eval_declare_alloc(local, &str.decl);
	parse_eval_string(&pos, "(lambda (&aux aa (bb 10)))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	lambda_declare(local, &str);
	getnth(str.args, 5, &pos); /* aux */
	lambda_update_aux(&pos, pos);
	test(! GetStatusDynamic(pos), "lambda_update_aux1");
	test(length_list_unsafe(pos) == 2, "lambda_update_aux2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_update(void)
{
	addr control, pos;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "hello");
	parse_callname_local(local, &str.call, pos);
	str.table = Nil;
	str.globalp = 1;
	str.stack = newstack_nil(ptr);
	eval_declare_alloc(local, &str.decl);
	parse_eval_string(&pos, "(lambda ())");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	lambda_declare(local, &str);
	lambda_update(&str);
	test(str.table != Nil, "lambda_update1");
	test(! GetStatusDynamic(str.table), "lambda_update2");

	str.call = Nil;
	str.table = Nil;
	str.globalp = 1;
	str.stack = newstack_nil(ptr);
	eval_declare_alloc(local, &str.decl);
	parse_eval_string(&pos, "(lambda (aa bb cc &optional dd "
			"&rest ee &key ff &allow-other-keys &aux gg hh))");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	lambda_declare(local, &str);
	lambda_update(&str);
	test(str.table == Nil, "lambda_update3");
	test(! GetStatusDynamic(str.args), "lambda_update4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_closure(void)
{
	addr control, pos, stack, table, current, check;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "aa");
	push_tablevalue_local(ptr, stack, pos, &table);

	str.call = Nil;
	str.table = Nil;
	str.globalp = 1;
	str.clos = Nil;
	str.stack = newstack_lambda(ptr);
	eval_declare_alloc(local, &str.decl);
	parse_eval_string(&pos, "(lambda ())");
	GetEvalParse(pos, 0, &str.args); /* args */
	lambda_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	lambda_tablevalue(local, &str);
	lambda_declare(local, &str);
	lambda_update(&str);

	current = newstack_nil(ptr);
	readstring(&pos, "aa");
	test(! find_symbol_scope(ptr, pos, &pos), "lambda_closure1");
	freestack_eval(ptr, current);

	lambda_closure(ptr, &str);
	test(str.clos != Nil, "lambda_closure2");
	GetArrayA2(str.clos, 0, &pos);
	GetCons(pos, &check, &pos);
	test(eval_tablevalue_p(check), "lambda_closure3");
	test(pos == Nil, "lambda_closure4");
	getname_tablevalue(check, &check);
	readstring(&pos, "aa");
	test(check == pos, "lambda_closure5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_lambda_execute(void)
{
	addr control, pos;
	Execute ptr;
	struct lambda_struct str;

	init_lambda_struct(&str, EVAL_PARSE_LAMBDA, 0);
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_lambda(ptr);
	parse_eval_string(&pos, "(lambda (aa bb) (declare (integer aa)) aa bb :hello)");
	GetEvalParse(pos, 0, &str.args); /* args */
	GetEvalParse(pos, 1, &str.decl); /* decl */
	GetEvalParse(pos, 3, &str.cons); /* cons */
	lambda_execute(ptr, &str, &pos);
	test(eval_scope_p(pos), "lambda_execute1");

	str.stack = newstack_lambda(ptr);
	parse_eval_string(&pos, "(lambda (aa bb) (declare (integer aa)) aa bb :hello)");
	GetEvalParse(pos, 0, &str.args); /* args */
	GetEvalParse(pos, 1, &str.decl); /* decl */
	GetEvalParse(pos, 3, &str.cons); /* cons */
	lambda_object(ptr, &str, &pos);
	test(eval_scope_p(pos), "lambda_object1");

	parse_eval_string(&pos, "(lambda (aa bb) (declare (integer aa)) aa bb :hello)");
	scope_lambda(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_lambda1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_LAMBDA, "scope_lambda2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  defun
 */
static int test_defun_update(void)
{
	addr control, stack, call, pos, check;
	Execute ptr;
	LocalRoot local;
	struct lambda_struct str;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_lambda(ptr);
	parse_eval_string(&pos, "(defun name (aa bb) (declare (integer aa)) aa bb 10)");
	init_lambda_struct(&str, EVAL_PARSE_DEFUN, 1);
	GetEvalParse(pos, 0, &str.call);
	GetEvalParse(pos, 1, &str.args);
	GetEvalParse(pos, 2, &str.decl);
	GetEvalParse(pos, 3, &str.doc);
	GetEvalParse(pos, 4, &str.cons);
	lambda_object(ptr, &str, &pos);
	defun_update(ptr, &str);

	getglobal_eval(ptr, &stack);
	readstring(&call, "name");
	parse_callname_local(local, &call, call);
	test(find_tablefunction(stack, call, &pos), "defun_update1");
	getname_tablefunction(pos, &check);
	test(equal_callname(call, check), "defun_update2");
	gettype_tablefunction(pos, &check);
	test(RefLispDecl(check) == LISPDECL_FUNCTION, "defun_update3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_defun_the(void)
{
	addr pos, eval, type, car, cdr, check;
	struct lambda_struct str;

	init_eval_stack(Execute_Thread);
	init_lambda_struct(&str, EVAL_PARSE_DEFUN, 1);
	readstring(&pos, "hello");
	parse_callname_heap(&pos, pos);
	str.call = pos;
	eval_scope_size(Execute_Thread, &eval, EvalLambda_Size, EVAL_PARSE_EMPTY, Nil, Nil);
	defun_the(eval, &str);
	GetEvalScopeThe(eval, &type);
	test(RefLispDecl(type) == LISPDECL_SYMBOL, "defun_the1");

	init_lambda_struct(&str, EVAL_PARSE_DEFUN, 1);
	readstring(&pos, "(setf hello)");
	parse_callname_heap(&pos, pos);
	str.call = pos;
	eval_scope_size(Execute_Thread, &eval, EvalLambda_Size, EVAL_PARSE_EMPTY, Nil, Nil);
	defun_the(eval, &str);
	GetEvalScopeThe(eval, &type);
	/* (cons (eql setf) (cons (eql hello) null)) */
	test(RefLispDecl(type) == LISPDECL_CONS, "defun_the2");
	GetArrayType(type, 0, &car);
	GetArrayType(type, 1, &cdr);
	test(RefLispDecl(car) == LISPDECL_EQL, "defun_the3");
	GetArrayType(car, 0, &car);
	GetConstant(CONSTANT_COMMON_SETF, &check);
	test(car == check, "defun_the4");
	test(RefLispDecl(cdr) == LISPDECL_CONS, "defun_the5");
	GetArrayType(cdr, 0, &car);
	GetArrayType(cdr, 1, &cdr);
	test(RefLispDecl(car) == LISPDECL_EQL, "defun_the6");
	GetArrayType(car, 0, &car);
	readstring(&check, "hello");
	test(car == check, "defun_the7");
	test(RefLispDecl(cdr) == LISPDECL_NULL, "defun_the8");

	RETURN;
}

static int test_scope_defun(void)
{
	addr control, pos;
	Execute ptr;
	struct lambda_struct str;

	init_lambda_struct(&str, EVAL_PARSE_LAMBDA, 0);
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	parse_eval_string(&pos, "(defun name (aa) (declare (integer aa)) aa :hello)");
	scope_defun(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_defun1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_DEFUN, "scope_defun2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  macro-lambda
 */
static int test_macro_init_var(void)
{
	addr args, control, stack, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&args, "(aa bb)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	test(length_list_unsafe(args) == 8, "macro_init_var1");
	GetCar(args, &args);
	test(length_list_unsafe(args) == 2, "macro_init_var2");
	GetCons(args, &pos, &args);
	test(pos == readr("aa"), "macro_init_var3");
	GetCons(args, &pos, &args);
	test(pos == readr("bb"), "macro_init_var4");

	readstring(&args, "(aa (bb cc) dd)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	test(length_list_unsafe(args) == 8, "macro_init_var5");
	GetCar(args, &args);
	test(length_list_unsafe(args) == 3, "macro_init_var6");
	GetCons(args, &pos, &args);
	test(pos == readr("aa"), "macro_init_var7");
	GetCons(args, &pos, &args);
	GetCar(pos, &pos);
	test(length_list_unsafe(pos) == 2, "macro_init_var8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_macro_init_rest(void)
{
	addr args, control, stack, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&args, "(&rest aa)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	lista_bind(args, &pos, &pos, &pos, &args, NULL);
	test(consp(pos), "macro_init_rest1");
	GetCar(pos, &pos);
	test(pos == readr("aa"), "macro_init_rest2");

	readstring(&args, "(&body bb)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	lista_bind(args, &pos, &pos, &pos, &args, NULL);
	test(consp(pos), "macro_init_rest3");
	GetCar(pos, &pos);
	test(pos == readr("bb"), "macro_init_rest4");

	readstring(&args, "(cc . dd)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	lista_bind(args, &pos, &pos, &pos, &args, NULL);
	test(consp(pos), "macro_init_rest5");
	GetCar(pos, &pos);
	test(pos == readr("dd"), "macro_init_rest6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_macro_init_args(void)
{
	addr args, control, stack;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&args, "(&whole whole &environment env aa (&rest bb))");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	test(length_list_unsafe(args) == 8, "macro_init_args1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_macro_tablevalue_var(void)
{
	addr args, control, stack, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&args, "(aa bb)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	macro_tablevalue_args(ptr, stack, args, &args);
	test(length_list_unsafe(args) == 8, "macro_tablevalue_var1");
	GetCar(args, &args);
	test(length_list_unsafe(args) == 2, "macro_tablevalue_var2");
	GetCons(args, &pos, &args);
	test(eval_tablevalue_p(pos), "macro_tablevalue_var3");
	getname_tablevalue(pos, &pos);
	test(pos == readr("aa"), "macro_tablevalue_var4");
	GetCons(args, &pos, &args);
	test(eval_tablevalue_p(pos), "macro_tablevalue_var5");
	getname_tablevalue(pos, &pos);
	test(pos == readr("bb"), "macro_tablevalue_var6");

	readstring(&args, "(aa (bb cc) dd)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	macro_tablevalue_args(ptr, stack, args, &args);
	test(length_list_unsafe(args) == 8, "macro_tablevalue_var7");
	GetCar(args, &args);
	test(length_list_unsafe(args) == 3, "macro_tablevalue_var8");
	GetCons(args, &pos, &args);
	test(eval_tablevalue_p(pos), "macro_tablevalue_var9");
	getname_tablevalue(pos, &pos);
	test(pos == readr("aa"), "macro_tablevalue_var10");
	GetCons(args, &pos, &args);
	GetCar(pos, &pos);
	test(length_list_unsafe(pos) == 2, "macro_tablevalue_var11");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_macro_tablevalue_rest(void)
{
	addr args, control, stack, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&args, "(&rest aa)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	macro_tablevalue_args(ptr, stack, args, &args);
	lista_bind(args, &pos, &pos, &pos, &args, NULL);
	test(consp(pos), "macro_tablevalue_rest1");
	GetCar(pos, &pos);
	test(eval_tablevalue_p(pos), "macro_tablevalue_rest2");
	getname_tablevalue(pos, &pos);
	test(pos == readr("aa"), "macro_tablevalue_rest3");

	readstring(&args, "(&body bb)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	macro_tablevalue_args(ptr, stack, args, &args);
	lista_bind(args, &pos, &pos, &pos, &args, NULL);
	test(consp(pos), "macro_tablevalue_rest4");
	GetCar(pos, &pos);
	test(eval_tablevalue_p(pos), "macro_tablevalue_rest5");
	getname_tablevalue(pos, &pos);
	test(pos == readr("bb"), "macro_tablevalue_rest6");

	readstring(&args, "(cc . dd)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	macro_tablevalue_args(ptr, stack, args, &args);
	lista_bind(args, &pos, &pos, &pos, &args, NULL);
	test(consp(pos), "macro_tablevalue_rest7");
	GetCar(pos, &pos);
	test(eval_tablevalue_p(pos), "macro_tablevalue_rest8");
	getname_tablevalue(pos, &pos);
	test(pos == readr("dd"), "macro_tablevalue_rest9");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_macro_tablevalue_args(void)
{
	addr args, control, stack, pos, whole, env;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&args, "(&whole aa &environment bb cc (&rest dd))");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	macro_tablevalue_args(ptr, stack, args, &args);
	test(length_list_unsafe(args) == 8, "macro_tablevalue_args1");
	list_bind(args, &pos, &pos, &pos, &pos, &pos, &pos, &whole, &env, NULL);
	test(eval_tablevalue_p(whole), "macro_tablevalue_args2");
	getname_tablevalue(whole, &whole);
	test(whole == readr("aa"), "macro_tablevalue_args3");
	test(eval_tablevalue_p(env), "macro_tablevalue_args4");
	getname_tablevalue(env, &env);
	test(env == readr("bb"), "macro_tablevalue_args5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_macro_update_var(void)
{
	addr args, control, stack, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&args, "(aa (bb cc) dd)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	macro_tablevalue_args(ptr, stack, args, &args);
	macro_update_args(&args, args);
	test(length_list_unsafe(args) == 8, "macro_update_var1");
	GetCar(args, &args);
	test(length_list_unsafe(args) == 3, "macro_update_var2");
	GetCons(args, &pos, &args);
	test(eval_tablevalue_p(pos), "macro_update_var3");
	getname_tablevalue(pos, &pos);
	test(pos == readr("aa"), "macro_update_var4");
	GetCons(args, &pos, &args);
	GetCar(pos, &pos);
	test(length_list_unsafe(pos) == 2, "macro_update_var5");
	test(! GetStatusDynamic(pos), "macro_update_var6");
	GetCar(pos, &pos);
	test(eval_tablevalue_p(pos), "macro_update_var7");
	test(! GetStatusDynamic(pos), "macro_update_var8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_macro_update_rest(void)
{
	addr args, control, stack, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&args, "(&rest aa)");
	stack = newstack_nil(ptr);
	lambda_macro(ptr->local, &args, args, Nil);
	parse_macro_lambda_list(ptr, &args, args);
	macro_init_args(ptr, stack, args, Nil, &args);
	macro_tablevalue_args(ptr, stack, args, &args);
	macro_update_args(&args, args);
	lista_bind(args, &pos, &pos, &pos, &args, NULL);
	test(consp(pos), "macro_update_rest1");
	test(! GetStatusDynamic(pos), "macro_update_rest2");
	GetCar(pos, &pos);
	test(eval_tablevalue_p(pos), "macro_update_rest3");
	test(! GetStatusDynamic(pos), "macro_update_rest4");
	getname_tablevalue(pos, &pos);
	test(pos == readr("aa"), "macro_update_rest5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_defmacro(void)
{
	addr control, pos, name, lambda;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	parse_eval_string(&pos, "(defmacro aaa () :hello)");
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &lambda);
	test(symbolp(name), "scope_defmacro1");
	test(name == readr("aaa"), "scope_defmacro2");
	test(macro_function_p(lambda), "scope_defmacro3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_push_symbol_macrolet(void)
{
	addr control, stack, key, table, symbol, v1, v2, left, right;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);
	stack = newstack_nil(ptr);

	symbol = readr("aaa");
	fixnum_heap(&v1, 10);
	fixnum_heap(&v2, 20);
	push_symbol_macrolet(stack, symbol, v1, v2);

	GetConst(SYSTEM_SYMBOL_MACROLET, &key);
	GetEvalStackTable(stack, &table);
	test(getplistplist(table, key, symbol, &right) == 0, "push_symbol_macrolet1");
	test(consp(right), "push_symbol_macrolet2");
	GetCons(right, &left, &right);
	test(left == v1, "push_symbol_macrolet3");
	test(right == v2, "push_symbol_macrolet4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_define_symbol_macro(void)
{
	addr control, pos, name, form;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);
	newstack_nil(ptr);

	parse_eval_string(&pos, "(define-symbol-macro aaa ''hello)");
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &form);
	test(symbolp(name), "scope_define_symbol_macro1");
	test(name == readr("aaa"), "scope_define_symbol_macro2");
	test(eval_parse_p(form), "scope_define_symbol_macro3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_apply_symbol_macrolet(void)
{
	addr control, stack, pos, key, table, left, right;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);
	stack = newstack_nil(ptr);

	parse_eval_string(&pos, "(symbol-macrolet "
			"((aaa ''hello1) (bbb ''hello2)) :hello)");
	GetEvalParse(pos, 0, &pos);
	apply_symbol_macrolet(stack, pos);

	GetConst(SYSTEM_SYMBOL_MACROLET, &key);
	GetEvalStackTable(stack, &table);
	pos = readr("aaa");
	test(getplistplist(table, key, pos, &right) == 0, "apply_symbol_macrolet1");
	test(consp(right), "apply_symbol_macrolet2");
	GetCons(right, &left, &right);
	test(eval_parse_p(left), "apply_symbol_macrolet3");
	test(GetType(right) == LISPTYPE_ENVIRONMENT, "apply_symbol_macrolet4");

	pos = readr("bbb");
	test(getplistplist(table, key, pos, &right) == 0, "apply_symbol_macrolet5");
	test(consp(right), "apply_symbol_macrolet6");
	GetCons(right, &left, &right);
	test(eval_parse_p(left), "apply_symbol_macrolet7");
	test(GetType(right) == LISPTYPE_ENVIRONMENT, "apply_symbol_macrolet8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_symbol_macrolet(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);
	newstack_nil(ptr);

	parse_eval_string(&pos, "(symbol-macrolet "
			"((aaa ''hello1) (bbb ''hello2)) :hello)");
	scope_symbol_macrolet(ptr, &pos, pos);
	test(RefEvalScopeType(pos) == EVAL_PARSE_LOCALLY, "scope_symbol_macrolet1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  flet
 */
static int test_flet_call(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	parse_eval_string(&pos, "(flet ((aa () :hello)) :cons)");
	GetEvalParse(pos, 0, &pos); /* args */
	GetCar(pos, &pos); /* aa */
	flet_call(ptr, &pos, pos);
	test(RefEvalScopeType(pos) == EVAL_PARSE_EMPTY, "flet_call1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_flet_init(void)
{
	addr control, pos, args, eval, check;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);
	memset(&str, 0, sizeoft(struct let_struct));

	parse_eval_string(&pos, "(flet ((aa () :hello) (bb (c) c 10)) :cons)");
	GetEvalParse(pos, 0, &str.args);
	flet_init(ptr, &str);
	args = str.args;
	test(length_list_unsafe(args) == 2, "flet_init1");
	GetCons(args, &pos, &args);
	GetCons(pos, &pos, &eval);
	readstring(&check, "aa");
	parse_callname_local(local, &check, check);
	test(equal_callname(pos, check), "flet_init2");
	test(eval_scope_p(eval), "flet_init3");
	GetCons(args, &pos, &args);
	GetCons(pos, &pos, &eval);
	readstring(&check, "bb");
	parse_callname_local(local, &check, check);
	test(equal_callname(pos, check), "flet_init4");
	test(eval_scope_p(eval), "flet_init5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_flet_maketable(void)
{
	addr control, pos;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(flet ((a () 10) (b () (progn 20)) (c ())) :hello)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &pos); /* let-args */
	str.args = pos;
	flet_init(ptr, &str);
	flet_maketable(local, &str);

	readstring(&pos, "a");
	parse_callname_local(local, &pos, pos);
	test(find_tablefunction(str.stack, pos, &pos), "flet_maketable1");
	test(eval_tablefunction_p(pos), "flet_maketable2");
	gettype_tablefunction(pos, &pos);
	test(type_function_aster_p(pos), "flet_maketable3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_checktype_function(void)
{
	addr control, stack, call, table, type, init;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&call, "aa");
	parse_callname_local(local, &call, call);
	push_tablefunction_alloc(ptr, local, stack, call, &table);
	readstring(&type, "(function * *)");
	test_parse_type(&type, type);
	settype_tablefunction(table, type);

	readstring(&type, "(function * *)");
	test_parse_type(&type, type);
	make_eval_scope(Execute_Thread, &init, EVAL_PARSE_EMPTY, type, Nil);
	checktype_function(table, init);
	test(1, "checktype_function1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_flet_applytable(void)
{
	addr control, pos, table, type;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(flet ((aa () 10) (bb () 20)) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args); /* let-args */
	GetEvalParse(pos, 1, &str.decl); /* let-decl */
	flet_init(ptr, &str);
	flet_maketable(local, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	flet_applytable(ptr, &str);

	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	test(find_tablefunction(str.stack, pos, &table), "flet_applytable1");
	test(! getglobalp_tablefunction(table), "flet_applytable2");
	readstring(&pos, "bb");
	parse_callname_local(local, &pos, pos);
	test(find_tablefunction(str.stack, pos, &table), "flet_applytable3");
	test(! getglobalp_tablefunction(table), "flet_applytable4");

	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	test(find_tablefunction(str.stack, pos, &table), "flet_applytable5");
	readstring(&type, "(function * *)");
	test_parse_type(&type, type);
	settype_tablefunction(table, type);

	flet_applytable(ptr, &str);
	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	test(find_tablefunction(str.stack, pos, &table), "flet_applytable7");
	test(! getglobalp_tablefunction(table), "flet_applytable8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_ignore_checkfunction(void)
{
	addr control, pos, table;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(flet ((aa () 10) (bb () 20)) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args); /* let-args */
	GetEvalParse(pos, 1, &str.decl); /* let-decl */
	flet_init(ptr, &str);
	flet_maketable(local, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	flet_applytable(ptr, &str);

	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	find_tablefunction(str.stack, pos, &table);
	setreference_tablefunction(table, 1);
	readstring(&pos, "bb");
	parse_callname_local(local, &pos, pos);
	find_tablefunction(str.stack, pos, &table);
	setreference_tablefunction(table, 1);
	ignore_checkfunction(str.stack);
	test(1, "ignore_checkfunction1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_flet_update(void)
{
	int check;
	addr control, pos, cons;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(flet ((aa () 10) (bb () 20)) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args); /* let-args */
	GetEvalParse(pos, 1, &str.decl); /* let-decl */
	flet_init(ptr, &str);
	flet_maketable(local, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);
	flet_applytable(ptr, &str);
	flet_update(&str);

	check = 1;
	for (cons = str.args; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		GetCar(pos, &pos);
		if (GetStatusDynamic(pos)) {
			check = 0;
			break;
		}
	}
	test(check, "flet_update1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_flet(void)
{
	addr control, pos;
	Execute ptr;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(flet ((aa () 10) (bb () 20)) (aa) (bb) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args);
	GetEvalParse(pos, 1, &str.decl);
	GetEvalParse(pos, 2, &str.cons);
	flet_execute(ptr, &str);
	test(RefLispDecl(str.the) == LISPDECL_KEYWORD, "flet_execute1");

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(flet ((aa () 10) (bb () 20)) (aa) (bb) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args);
	GetEvalParse(pos, 1, &str.decl);
	GetEvalParse(pos, 2, &str.cons);
	flet_object(ptr, &str);
	test(RefLispDecl(str.the) == LISPDECL_KEYWORD, "flet_object1");

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(flet ((aa () 10) (bb () 20)) (aa) (bb) :aa)");
	eval_parse_execute(&pos, pos);
	scope_flet(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_flet1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  labels
 */
static int test_ifdeclcall(void)
{
	addr control, call, pos;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos,
			"(labels ((aa () 10) (bb () 20)) "
			"  (declare (inline aa bb cc)) "
			"  (aa) (bb) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args);
	GetEvalParse(pos, 1, &str.decl);

	readstring(&call, "bb");
	parse_callname_local(local, &call, call);
	ifdeclcall(ptr, str.stack, call, str.decl, &pos);
	test(eval_tablefunction_p(pos), "ifdeclcall1");
	test(find_tablefunction(str.stack, call, NULL), "ifdeclcall2");
	test(getinline_tablefunction(pos) == InlineType_Inline, "ifdeclcall3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_labels_init(void)
{
	addr control, pos, args, eval, check, call;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos,
			"(labels ((aa () 10) (bb () 20)) "
			"  (declare (inline aa bb cc)) "
			"  (aa) (bb) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args);
	GetEvalParse(pos, 1, &str.decl);
	labels_init(ptr, &str);
	args = str.args;
	test(length_list_unsafe(args) == 2, "labels_init1");
	GetCons(args, &pos, &args);
	GetCons(pos, &pos, &eval);
	readstring(&check, "aa");
	parse_callname_local(local, &check, check);
	test(equal_callname(pos, check), "labels_init2");
	test(eval_scope_p(eval), "labels_init3");
	GetCons(args, &pos, &args);
	GetCons(pos, &pos, &eval);
	readstring(&check, "bb");
	parse_callname_local(local, &check, check);
	test(equal_callname(pos, check), "labels_init4");
	test(eval_scope_p(eval), "labels_init5");

	readstring(&call, "bb");
	parse_callname_local(local, &call, call);
	test(find_tablefunction(str.stack, call, &pos), "labels_init6");
	test(getinline_tablefunction(pos) == InlineType_Inline, "labels_init7");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_labels_checktype(void)
{
	addr control, pos, table, type;
	Execute ptr;
	LocalRoot local;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(labels ((aa () 10) (bb () 20)) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args);
	GetEvalParse(pos, 1, &str.decl);

	labels_init(ptr, &str);
	apply_declare(ptr, str.stack, str.decl, &str.free);

	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	test(find_tablefunction(str.stack, pos, &table), "labels_checktype1");
	readstring(&type, "(function * *)");
	test_parse_type(&type, type);
	settype_tablefunction(table, type);

	labels_checktype(&str);
	readstring(&pos, "aa");
	parse_callname_local(local, &pos, pos);
	test(find_tablefunction(str.stack, pos, &table), "labels_checktype2");
	test(! getglobalp_tablefunction(table), "labels_checktype3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_labels(void)
{
	addr control, pos;
	Execute ptr;
	struct let_struct str;

	memset(&str, 0, sizeoft(str));
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(labels ((aa () 10) (bb () 20)) (aa) (bb) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args);
	GetEvalParse(pos, 1, &str.decl);
	GetEvalParse(pos, 2, &str.cons);
	labels_execute(ptr, &str);
	test(RefLispDecl(str.the) == LISPDECL_KEYWORD, "labels_execute1");

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(labels ((aa () 10) (bb () 20)) (aa) (bb) :aa)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &str.args);
	GetEvalParse(pos, 1, &str.decl);
	GetEvalParse(pos, 2, &str.cons);
	labels_object(ptr, &str);
	test(RefLispDecl(str.the) == LISPDECL_KEYWORD, "labels_object1");

	str.stack = newstack_nil(ptr);
	readstring(&pos, "(labels ((aa () 10) (bb () 20)) (aa) (bb) :aa)");
	eval_parse_execute(&pos, pos);
	scope_labels(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_labels1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  call
 */
static int test_call_first(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(hello 10 20 30)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &pos);
	call_first(ptr, &pos, pos);
	test(eval_scope_p(pos), "call_first1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_FUNCTION, "call_first2");

	readstring(&pos, "((lambda ()) 10 20 30)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &pos);
	call_first(ptr, &pos, pos);
	test(eval_scope_p(pos), "call_first3");
	test(RefEvalScopeType(pos) == EVAL_PARSE_LAMBDA, "call_first4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_check_tablecall(void)
{
	addr control, pos, type, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "100");
	eval_parse_execute(&pos, pos);
	readstring(&type, "integer");
	test_parse_type(&type, type);
	check_tablecall(ptr, pos, type, &pos);
	test(eval_tablecall_p(pos), "check_tablecall1");
	gettype_tablecall(pos, &check);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "check_tablecall2");
	getvalue_tablecall(pos, &check);
	test(eval_scope_p(check), "check_tablecall3");
	test(! getcheck_tablecall(pos), "check_tablecall4");

	readstring(&pos, "100");
	eval_parse_execute(&pos, pos);
	readstring(&type, "(satisfies hello)");
	test_parse_type(&type, type);
	check_tablecall(ptr, pos, type, &pos);
	test(getcheck_tablecall(pos), "check_tablecall5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_callargs_var(void)
{
	addr control, pos, args, root, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(function (integer character))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call 10 #\\a)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	root = Nil;
	test(callargs_var(ptr, pos, &args, &root) == 0, "callargs_var1");
	test(length_list_unsafe(root) == 2, "callargs_var2");
	GetCons(root, &pos, &root);
	test(eval_tablecall_p(pos), "callargs_var3");
	gettype_tablecall(pos, &check);
	test(RefLispDecl(check) == LISPDECL_CHARACTER, "callargs_var4");
	getvalue_tablecall(pos, &check);
	test(RefEvalScopeType(check) == EVAL_PARSE_CHARACTER, "callargs_var5");
	test(! getcheck_tablecall(pos), "callargs_var6");

	readstring(&pos, "(function (integer character real))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call 10 #\\a)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	root = Nil;
	test(callargs_var(ptr, pos, &args, &root), "callargs_var7");

	readstring(&pos, "(function ((satisfies hello)))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call 10)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	root = Nil;
	test(callargs_var(ptr, pos, &args, &root) == 0, "callargs_var8");
	test(length_list_unsafe(root) == 1, "callargs_var9");
	GetCons(root, &pos, &root);
	test(getcheck_tablecall(pos), "callargs_var10");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_callargs_opt(void)
{
	addr control, pos, args, root, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(function (&optional integer character))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call 10 #\\a)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	root = Nil;
	callargs_opt(ptr, pos, &args, &root);
	test(length_list_unsafe(root) == 2, "callargs_opt1");
	GetCons(root, &pos, &root);
	test(eval_tablecall_p(pos), "callargs_opt2");
	gettype_tablecall(pos, &check);
	test(RefLispDecl(check) == LISPDECL_CHARACTER, "callargs_opt3");
	getvalue_tablecall(pos, &check);
	test(RefEvalScopeType(check) == EVAL_PARSE_CHARACTER, "callargs_opt4");
	test(! getcheck_tablecall(pos), "callargs_opt5");

	readstring(&pos, "(function (&optional (satisfies hello)))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call 10)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	root = Nil;
	callargs_opt(ptr, pos, &args, &root);
	test(length_list_unsafe(root) == 1, "callargs_opt6");
	GetCons(root, &pos, &root);
	test(getcheck_tablecall(pos), "callargs_opt7");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_callargs_keyvalue(void)
{
	addr cons, key, type, check;

	readstring(&key, "hello");
	GetTypeTable(&type, Atom);
	cons_heap(&cons, key, type);
	callargs_keyvalue(0, cons, &check);
	test(RefLispDecl(check) == LISPDECL_EQL, "callargs_keyvalue1");
	GetArrayType(check, 0, &check);
	test(check == key, "callargs_keyvalue2");

	callargs_keyvalue(1, cons, &check);
	test(RefLispDecl(check) == LISPDECL_ATOM, "callargs_keyvalue3");

	RETURN;
}

static int test_callargs_key(void)
{
	addr cons, key, type, check;

	callargs_key(0, T, &check);
	test(RefLispDecl(check) == LISPDECL_SYMBOL, "callargs_key1");
	callargs_key(1, T, &check);
	test(RefLispDecl(check) == LISPDECL_T, "callargs_key2");

	readstring(&key, "hello");
	GetTypeTable(&type, Atom);
	cons_heap(&cons, key, type);
	list_heap(&cons, cons, NULL);
	callargs_key(0, cons, &check);
	test(RefLispDecl(check) == LISPDECL_EQL, "callargs_key3");
	callargs_key(1, cons, &check);
	test(RefLispDecl(check) == LISPDECL_ATOM, "callargs_key4");

	cons_heap(&cons, key, type);
	list_heap(&cons, cons, cons, NULL);
	callargs_key(0, cons, &check);
	test(RefLispDecl(check) == LISPDECL_OR, "callargs_key5");
	callargs_key(1, cons, &check);
	test(RefLispDecl(check) == LISPDECL_OR, "callargs_key6");

	RETURN;
}

static int test_callargs_restkey(void)
{
	int result;
	addr control, pos, args, root, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(function ())");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call 10 #\\a)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	root = Nil;
	callargs_restkey(ptr, pos, &args, &root, &result);
	test(result, "callargs_restkey1");

	readstring(&args, "(call)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	root = Nil;
	callargs_restkey(ptr, pos, &args, &root, &result);
	test(! result, "callargs_restkey2");
	test(root == Nil, "callargs_restkey3");

	readstring(&pos, "(function (&rest integer))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call 10)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	root = Nil;
	callargs_restkey(ptr, pos, &args, &root, &result);
	test(! result, "callargs_restkey4");
	test(singlep(root), "callargs_restkey5");
	GetCar(root, &root);
	gettype_tablecall(root, &root);
	test(RefLispDecl(root) == LISPDECL_INTEGER, "callargs_restkey6");

	readstring(&pos, "(function (&key (aaa integer) (bbb real)))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call aaa 10)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	root = Nil;
	callargs_restkey(ptr, pos, &args, &root, &result);
	test(! result, "callargs_restkey7");
	test(length_list_unsafe(root) == 2, "callargs_restkey8");
	GetCar(root, &check);
	gettype_tablecall(check, &check);
	test(RefLispDecl(check) == LISPDECL_OR, "callargs_restkey9");

	readstring(&pos, "(function (&rest fixnum &key (aaa integer) (bbb real)))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call aaa 10 bbb 20)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	root = Nil;
	callargs_restkey(ptr, pos, &args, &root, &result);
	test(! result, "callargs_restkey10");
	test(length_list_unsafe(root) == 4, "callargs_restkey11");
	GetCar(root, &check);
	gettype_tablecall(check, &check);
	test(RefLispDecl(check) == LISPDECL_AND, "callargs_restkey12");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_callargs_check(void)
{
	addr control, pos, args, root, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(function ())");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	callargs_check(ptr, pos, args, &root);
	test(root == Nil, "callargs_check1");

	readstring(&pos, "(function (integer))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call 10)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	callargs_check(ptr, pos, args, &root);
	test(length_list_unsafe(root) == 1, "callargs_check2");

	readstring(&pos, "(function (integer &optional real))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call 10 20)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	callargs_check(ptr, pos, args, &root);
	test(length_list_unsafe(root) == 2, "callargs_check3");
	GetCar(root, &check);
	getvalue_tablecall(check, &check);
	GetEvalScopeValue(check, &check);
	test(RefFixnum(check) == 10, "callargs_check4");

	readstring(&pos, "(function (integer &rest real))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call 10 20 30 40 50)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	callargs_check(ptr, pos, args, &root);
	test(length_list_unsafe(root) == 5, "callargs_check5");

	readstring(&pos, "(function (&rest t &key (hello integer)))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	callargs_check(ptr, pos, args, &root);
	test(length_list_unsafe(root) == 0, "callargs_check6");

	readstring(&pos, "(function (&rest t &key (hello integer)))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call hello 20)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	callargs_check(ptr, pos, args, &root);
	test(length_list_unsafe(root) == 2, "callargs_check7");

	readstring(&pos, "(function (&key (hello integer)))");
	test_parse_type(&pos, pos);
	GetArrayType(pos, 0, &pos); /* args */
	readstring(&args, "(call hello 100)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	callargs_check(ptr, pos, args, &root);
	test(length_list_unsafe(root) == 2, "callargs_check8");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_asterisk_function_argument(void)
{
	addr pos;

	GetTypeTable(&pos, Asterisk);
	test(asterisk_function_argument(pos, &pos), "asterisk_function_argument1");

	readstring(&pos, "function");
	test_parse_type(&pos, pos);
	test(asterisk_function_argument(pos, &pos), "asterisk_function_argument2");

	readstring(&pos, "(function * integer)");
	test_parse_type(&pos, pos);
	test(asterisk_function_argument(pos, &pos), "asterisk_function_argument3");

	readstring(&pos, "(function ())");
	test_parse_type(&pos, pos);
	test(! asterisk_function_argument(pos, &pos), "asterisk_function_argument4");
	test(GetType(pos) == LISPTYPE_VECTOR, "asterisk_function_argument5");

	RETURN;
}

static int test_callargs_nocheck(void)
{
	addr control, args, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&args, "(call 10 20 30 40)");
	eval_parse_execute(&args, args);
	GetEvalParse(args, 1, &args); /* args */
	callargs_nocheck(ptr, args, &args);
	test(length_list_unsafe(args) == 4, "callargs_nocheck1");
	GetCar(args, &check);
	test(eval_tablecall_p(check), "callargs_nocheck2");
	gettype_tablecall(check, &check);
	test(type_asterisk_p(check), "callargs_nocheck3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_call_args(void)
{
	addr control, pos, first, args, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(call 10 20 30 40)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &first);
	GetEvalParse(pos, 1, &args);
	call_first(ptr, &first, first);
	readstring(&pos, "(function * *)");
	test_parse_type(&pos, pos);
	SetEvalScopeThe(first, pos);

	call_args(ptr, &args, first, args);
	test(length_list_unsafe(args) == 4, "call_args1");
	GetCar(args, &check);
	test(eval_tablecall_p(check), "call_args2");
	gettype_tablecall(check, &check);
	test(type_asterisk_p(check), "call_args3");

	readstring(&pos, "(call 10 20 30 40)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &first);
	GetEvalParse(pos, 1, &args);
	call_first(ptr, &first, first);
	readstring(&pos, "(function (&rest integer) *)");
	test_parse_type(&pos, pos);
	SetEvalScopeThe(first, pos);

	call_args(ptr, &args, first, args);
	test(length_list_unsafe(args) == 4, "call_args4");
	GetCar(args, &check);
	test(eval_tablecall_p(check), "call_args5");
	gettype_tablecall(check, &check);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "call_args6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_call_result(void)
{
	addr control, pos, first, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(call 10 20 30 40)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &first);
	call_first(ptr, &first, first);

	GetTypeTable(&pos, Asterisk);
	SetEvalScopeThe(first, pos);
	call_result(&check, first);
	test(type_asterisk_p(check), "call_result1");

	readstring(&pos, "(function * *)");
	test_parse_type(&pos, pos);
	SetEvalScopeThe(first, pos);
	call_result(&check, first);
	test(type_asterisk_p(check), "call_result2");

	readstring(&pos, "(function * integer)");
	test_parse_type(&pos, pos);
	SetEvalScopeThe(first, pos);
	call_result(&check, first);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "call_result3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_call(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(call 10 20 30 40)");
	eval_parse_execute(&pos, pos);
	scope_call(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_call1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  syntax operator
 */
static int test_values_args(void)
{
	addr control, pos, cons, type;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(values 10 #\\A)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &pos);
	values_args(ptr, pos, &cons, &type);
	test(length_list_unsafe(cons) == 2, "values_args1");
	GetCar(cons, &pos);
	GetEvalScopeValue(pos, &pos);
	test(RefFixnum(pos) == 10, "values_args2");

	test(RefLispDecl(type) == LISPDECL_VALUES, "values_args3");
	GetArrayType(type, 0, &pos); /* var */
	test(length_list_unsafe(pos) == 2, "values_args4");
	GetCar(pos, &pos);
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "values_args5");

	GetArrayType(type, 2, &pos); /* rest */
	test(RefLispDecl(pos) == LISPDECL_NIL, "values_args6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_values(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(values 10 #\\A)");
	eval_parse_execute(&pos, pos);
	scope_values(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_values1");
	GetEvalScopeValue(pos, &check);
	test(length_list_unsafe(check) == 2, "scope_values2");
	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_VALUES, "scope_values3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_the_check(void)
{
	addr control, pos, eval, type, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(the integer 100)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &type);
	GetEvalParse(pos, 1, &eval);
	scope_eval(ptr, &eval, eval);
	the_check(eval, type, &check);
	test(check == Nil, "the_check1");

	readstring(&pos, "(the (satisfies hello) 100)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &type);
	GetEvalParse(pos, 1, &eval);
	scope_eval(ptr, &eval, eval);
	the_check(eval, type, &check);
	test(check == T, "the_check2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_the(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(the (satisfies hello) 100)");
	eval_parse_execute(&pos, pos);
	scope_the(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_the1");
	GetEvalScopeValue(pos, &check);
	test(eval_scope_p(check), "scope_the2");
	GetEvalScopeIndex(pos, 0, &check);
	test(check == T, "scope_the3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_locally_execute(void)
{
	addr control, pos, decl, cons, free, type;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(locally (declare (type integer aaa)) 100 :hello)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &decl);
	GetEvalParse(pos, 1, &cons);
	locally_execute(ptr, decl, cons, &cons, &type, &free);
	test(length_list_unsafe(cons) == 2, "locally_execute1");
	GetCar(cons, &cons);
	GetEvalScopeValue(cons, &cons);
	test(RefFixnum(cons) == 100, "locally_execute2");
	test(RefLispDecl(type) == LISPDECL_KEYWORD, "locally_execute3");
	test(length_list_unsafe(free) == 1, "locally_execute4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_locally(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(locally (declare (type integer aaa)) 100 :hello)");
	eval_parse_execute(&pos, pos);
	scope_locally(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_locally1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_LOCALLY, "scope_locally2");
	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_KEYWORD, "scope_locally3");
	GetEvalScopeIndex(pos, 0, &check);
	test(eval_declare_p(check), "scope_locally4");
	GetEvalScopeIndex(pos, 1, &check);
	test(length_list_unsafe(check) == 2, "scope_locally5");
	GetEvalScopeIndex(pos, 2, &check);
	test(length_list_unsafe(check) == 1, "scope_locally6");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_if(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(if 10 20 30)");
	eval_parse_execute(&pos, pos);
	scope_if(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_if1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_IF, "scope_if2");

	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_OR, "scope_if3");
	GetArrayType(check, 0, &check);
	test(lenarrayr(check) == 2, "scope_if4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_unwind_protect(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(unwind-protect 10 20 :hello)");
	eval_parse_execute(&pos, pos);
	scope_unwind_protect(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_unwind_protect1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_UNWIND_PROTECT, "scope_unwind_protect2");

	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "scope_unwind_protect3");
	GetEvalScopeIndex(pos, 0, &check);
	test(eval_scope_p(check), "scope_unwind_protect4");
	GetEvalScopeIndex(pos, 1, &check);
	test(length_list_unsafe(check) == 2, "scope_unwind_protect5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  tagbody
 */
static int test_push_tabletagbody(void)
{
	addr control, stack, table, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "hello");
	push_tabletagbody(stack, pos);
	readstring(&pos, "hello");
	push_tabletagbody(stack, pos);
	make_fixnum_heap(&pos, 10);
	push_tabletagbody(stack, pos);
	make_fixnum_heap(&pos, 10);
	push_tabletagbody(stack, pos);
	readstring(&pos, "aaa");
	push_tabletagbody(stack, pos);

	GetConstant(CONSTANT_SYSTEM_TABLE_TAGBODY, &pos);
	GetEvalStackTable(stack, &table);
	getplist(table, pos, &table);
	test(length_list_unsafe(table) == 6, "push_tabletagbody1");
	readstring(&pos, "10");
	test(getplist_eql(table, pos, &pos) == 0, "push_tabletagbody2");
	test(eval_tabletagbody_p(pos), "push_tabletagbody3");
	gettag_tabletagbody(pos, &pos);
	test(RefFixnum(pos) == 10, "push_tabletagbody4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_tagbody_push(void)
{
	addr control, stack, table, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "(tagbody 10 20 hello 40 (call))");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &pos); /* tag */
	tagbody_push(stack, pos);

	GetConstant(CONSTANT_SYSTEM_TABLE_TAGBODY, &pos);
	GetEvalStackTable(stack, &table);
	getplist(table, pos, &table);
	test(length_list_unsafe(table) == 8, "tagbody_push1");
	readstring(&pos, "10");
	test(getplist_eql(table, pos, &pos) == 0, "tagbody_push2");
	test(eval_tabletagbody_p(pos), "tagbody_push3");
	gettag_tabletagbody(pos, &pos);
	test(RefFixnum(pos) == 10, "tagbody_push4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_tagbody_allcons(void)
{
	addr control, stack, pos, tag, body;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "(tagbody 10 20 hello 40 (call))");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &tag); /* tag */
	GetEvalParse(pos, 1, &body); /* body */
	tagbody_push(stack, tag);
	tagbody_allcons(ptr, body, &body);

	test(length_list_unsafe(body) == 5, "tagbody_allcons1");
	GetCar(body, &body);
	test(eval_scope_p(body), "tagbody_allcons2");
	test(RefEvalScopeType(body) == EVAL_PARSE_TAG, "tagbody_allcons3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_tagbody_check(void)
{
	addr control, stack, pos, tag, body;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "(tagbody 10 20 (go 10) (call) (go 20))");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &tag); /* tag */
	GetEvalParse(pos, 1, &body); /* body */
	tagbody_push(stack, tag);
	tagbody_allcons(ptr, body, &body);
	tagbody_check(stack);
	test(1, "tagbody_check1");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_tagbody(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(tagbody 10 20 (go 10) (call) (go 20))");
	eval_parse_execute(&pos, pos);
	scope_tagbody(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_tagbody1");
	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_NULL, "scope_tagbody2");
	GetEvalScopeIndex(pos, 0, &check);
	test(length_list_unsafe(check) == 2, "scope_tagbody3");
	GetEvalScopeIndex(pos, 1, &check);
	test(length_list_unsafe(check) == 5, "scope_tagbody4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_find_tabletagbody(void)
{
	addr control, stack, pos, tag, body, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "(tagbody 10 20 (go 10) (call) (go 20))");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &tag); /* tag */
	GetEvalParse(pos, 1, &body); /* body */
	tagbody_push(stack, tag);

	readstring(&pos, "20");
	test(find_tabletagbody(stack, pos, &check), "find_tabletagbody1");
	test(find_tabletagbody(stack, pos, NULL), "find_tabletagbody2");
	test(eval_tabletagbody_p(check), "find_tabletagbody3");
	readstring(&pos, "hello");
	test(! find_tabletagbody(stack, pos, &check), "find_tabletagbody4");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_push_closure_tagbody(void)
{
	addr control, stack, pos, tag, body, check, key, table;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "(tagbody 10 20 (go 10) (call) (go 20))");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &tag); /* tag */
	GetEvalParse(pos, 1, &body); /* body */
	tagbody_push(stack, tag);

	readstring(&pos, "20");
	test(find_tabletagbody(stack, pos, &check), "push_closure_tagbody1");
	push_closure_tagbody(stack, pos, check);
	readstring(&pos, "10");
	test(find_tabletagbody(stack, pos, &check), "push_closure_tagbody2");
	push_closure_tagbody(stack, pos, check);

	GetConstant(CONSTANT_SYSTEM_CLOSURE_TAGBODY, &key);
	GetEvalStackTable(stack, &table);
	getplist(table, key, &pos);
	test(length_list_unsafe(pos) == 4, "push_closure_tagbody3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_go_tabletagbody(void)
{
	addr control, tagbody, stack, pos, tag, body, check, key, table;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_tagbody(ptr);
	readstring(&pos, "(tagbody 10 20 (go 10) (call) (go 20))");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &tag); /* tag */
	GetEvalParse(pos, 1, &body); /* body */
	tagbody_push(stack, tag);
	readstring(&pos, "20");
	test(find_tabletagbody(stack, pos, &check), "go_tabletagbody1");

	stack = newstack_nil(ptr);
	tagbody = newstack_lambda(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);

	readstring(&pos, "20");
	test(go_tabletagbody(stack, pos, &pos), "go_tabletagbody2");
	test(pos == check, "go_tabletagbody3");

	GetConstant(CONSTANT_SYSTEM_CLOSURE_TAGBODY, &key);
	GetEvalStackTable(tagbody, &table);
	getplist(table, key, &pos);
	test(length_list_unsafe(pos) == 2, "go_tabletagbody4");

	readstring(&pos, "hello");
	test(! go_tabletagbody(stack, pos, &pos), "go_tabletagbody5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_go_execute(void)
{
	addr control, stack, pos, tag, body;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_tagbody(ptr);
	readstring(&pos, "(tagbody 10 20 (go 10) (call) (go 20))");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &tag); /* tag */
	GetEvalParse(pos, 1, &body); /* body */
	tagbody_push(stack, tag);

	readstring(&pos, "20");
	go_execute(ptr, &pos, pos);
	test(eval_tabletagbody_p(pos), "go_execute1");
	test(getreference_tabletagbody(pos), "go_execute2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_go(void)
{
	addr control, stack, pos, tag, body;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_tagbody(ptr);
	readstring(&pos, "(tagbody 10 20 (go 10) (call) (go 20))");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &tag); /* tag */
	GetEvalParse(pos, 1, &body); /* body */
	tagbody_push(stack, tag);

	readstring(&pos, "(go 20)");
	eval_parse_execute(&pos, pos);
	scope_go(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_go1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_GO, "scope_go2");
	GetEvalScopeThe(pos, &pos);
	test(RefLispDecl(pos) == LISPDECL_NIL, "scope_go3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  block
 */
static int test_push_tableblock(void)
{
	addr control, stack, pos, key, table, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_tagbody(ptr);
	readstring(&pos, "aaa");
	push_tableblock(stack, pos);

	GetConstant(CONSTANT_SYSTEM_TABLE_BLOCK, &key);
	GetEvalStackTable(stack, &table);
	test(getplist(table, key, &check) == 0, "push_tableblock1");
	test(check == pos, "push_tableblock2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_block_execute(void)
{
	addr control, pos, name, cons;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	newstack_tagbody(ptr);
	readstring(&pos, "(block name 10 20 :hello)");
	eval_parse_execute(&pos, pos);
	GetEvalParse(pos, 0, &name);
	GetEvalParse(pos, 1, &cons);
	block_execute(ptr, name, cons, &cons, &pos);
	test(length_list_unsafe(cons) == 3, "block_execute1");
	test(RefLispDecl(pos) == LISPDECL_KEYWORD, "block_execute2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_block(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	newstack_tagbody(ptr);
	readstring(&pos, "(block name 10 20 :hello)");
	eval_parse_execute(&pos, pos);
	scope_block(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_block1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_BLOCK, "scope_block2");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_push_closure_block(void)
{
	addr control, stack, pos, key, table;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_tagbody(ptr);
	readstring(&pos, "name");
	push_closure_block(stack, pos);
	readstring(&pos, "aaa");
	push_closure_block(stack, pos);
	readstring(&pos, "name");
	push_closure_block(stack, pos);
	readstring(&pos, "bbb");
	push_closure_block(stack, pos);

	GetConstant(CONSTANT_SYSTEM_CLOSURE_BLOCK, &key);
	GetEvalStackTable(stack, &table);
	test(getplist(table, key, &table) == 0, "push_closure_block1");
	test(length_list_unsafe(table) == 3, "push_closure_block2");
	readstring(&pos, "name");
	test(find_list_eq_unsafe(pos, table), "push_closure_block3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_find_tableblock(void)
{
	addr control, stack, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_tagbody(ptr);
	readstring(&pos, "name");
	test(! find_tableblock(stack, pos), "find_tableblock1");

	readstring(&pos, "name");
	push_tableblock(stack, pos);

	readstring(&pos, "name");
	test(find_tableblock(stack, pos), "find_tableblock2");
	readstring(&pos, "bbb");
	test(! find_tableblock(stack, pos), "find_tableblock3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_name_tableblock(void)
{
	addr control, stack, pos, key, table, lambda;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "name");
	test(! name_tableblock(Nil, pos), "name_tableblock1");

	stack = newstack_nil(ptr);
	readstring(&pos, "name");
	push_tableblock(stack, pos);

	readstring(&pos, "aaa");
	test(! name_tableblock(stack, pos), "name_tableblock2");
	readstring(&pos, "name");
	test(name_tableblock(stack, pos), "name_tableblock3");

	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	readstring(&pos, "aaa");
	test(! name_tableblock(stack, pos), "name_tableblock4");
	readstring(&pos, "name");
	test(name_tableblock(stack, pos), "name_tableblock5");

	lambda = newstack_lambda(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);
	stack = newstack_nil(ptr);

	readstring(&pos, "aaa");
	test(! name_tableblock(stack, pos), "name_tableblock6");
	readstring(&pos, "name");
	test(name_tableblock(stack, pos), "name_tableblock7");

	GetConstant(CONSTANT_SYSTEM_CLOSURE_BLOCK, &key);
	GetEvalStackTable(lambda, &table);
	test(getplist(table, key, &table) == 0, "name_tableblock8");
	test(singlep(table), "name_tableblock9");
	GetCar(table, &table);
	test(table == pos, "name_tableblock10");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_return_from(void)
{
	addr control, stack, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&pos, "name");
	push_tableblock(stack, pos);

	readstring(&pos, "(return-from name 100)");
	eval_parse_execute(&pos, pos);
	scope_return_from(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_return_from1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_RETURN_FROM, "scope_return_from2");
	GetEvalScopeThe(pos, &pos);
	test(RefLispDecl(pos) == LISPDECL_NIL, "scope_return_from3");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  catch
 */
static int test_scope_catch(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(catch 'hello 20 30)");
	eval_parse_execute(&pos, pos);
	scope_catch(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_catch1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_CATCH, "scope_catch2");

	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_ASTERISK, "scope_catch3");
	GetEvalScopeIndex(pos, 0, &check);
	test(RefEvalScopeType(check) == EVAL_PARSE_QUOTE, "scope_catch4");
	GetEvalScopeIndex(pos, 1, &check);
	test(lenarrayr(check) == 2, "scope_catch5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}

static int test_scope_throw(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(throw 'name 100)");
	eval_parse_execute(&pos, pos);
	scope_throw(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_throw1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_THROW, "scope_throw2");
	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_NIL, "scope_throw3");
	GetEvalScopeIndex(pos, 0, &check);
	test(RefEvalScopeType(check) == EVAL_PARSE_QUOTE, "scope_throw4");
	GetEvalScopeIndex(pos, 1, &check);
	test(RefEvalScopeType(check) == EVAL_PARSE_INTEGER, "scope_throw5");

	free_eval_stack(ptr);
	free_control(ptr, control);

	RETURN;
}


/*
 *  eval-when
 */
static int test_eval_when_check(void)
{
	addr control, symbol, value;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	symbol_toplevel_eval(&symbol);
	pushspecial_control(ptr, symbol, T);
	symbol_evalwhen_eval(&symbol);
	pushspecial_control(ptr, symbol, Nil);

	symbol_evalwhen_eval(&symbol);
	GetConst(COMMON_EVAL, &value);
	setspecial_local(ptr, symbol, value);
	test(! eval_when_check(ptr, T, T, Nil), "eval_when_check1");
	test(eval_when_check(ptr, Nil, Nil, T), "eval_when_check2");
	test(eval_when_check(ptr, T, Nil, T), "eval_when_check3");

	GetConst(COMMON_LOAD, &value);
	setspecial_local(ptr, symbol, value);
	test(! eval_when_check(ptr, T, Nil, T), "eval_when_check4");
	test(eval_when_check(ptr, Nil, T, Nil), "eval_when_check5");

	GetConst(COMMON_COMPILE, &value);
	setspecial_local(ptr, symbol, value);
	test(! eval_when_check(ptr, Nil, T, T), "eval_when_check6");
	test(eval_when_check(ptr, T, Nil, Nil), "eval_when_check7");

	symbol_toplevel_eval(&symbol);
	setspecial_local(ptr, symbol, Nil);
	symbol_evalwhen_eval(&symbol);
	GetConst(COMMON_EVAL, &value);
	setspecial_local(ptr, symbol, value);
	test(! eval_when_check(ptr, T, T, Nil), "eval_when_check8");
	test(eval_when_check(ptr, Nil, Nil, T), "eval_when_check9");
	test(eval_when_check(ptr, T, Nil, T), "eval_when_check10");

	GetConst(COMMON_LOAD, &value);
	setspecial_local(ptr, symbol, value);
	test(! eval_when_check(ptr, T, Nil, T), "eval_when_check11");
	test(! eval_when_check(ptr, Nil, T, Nil), "eval_when_check12");

	GetConst(COMMON_COMPILE, &value);
	setspecial_local(ptr, symbol, value);
	test(! eval_when_check(ptr, Nil, T, T), "eval_when_check13");
	test(! eval_when_check(ptr, T, Nil, Nil), "eval_when_check14");

	free_control(ptr, control);

	RETURN;
}

static int test_scope_eval_when(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(eval-when nil 100)");
	eval_parse_execute(&pos, pos);
	scope_eval_when(ptr, &pos, pos);
	test(RefEvalScopeType(pos) == EVAL_PARSE_NIL, "scope_eval_when1");

	readstring(&pos, "(eval-when (:execute) 100)");
	eval_parse_execute(&pos, pos);
	scope_eval_when(ptr, &pos, pos);
	test(RefEvalScopeType(pos) == EVAL_PARSE_PROGN, "scope_eval_when2");

	free_control(ptr, control);

	RETURN;
}


/*
 *  multiple-value-bind
 */
static int test_mvbind_maketable(void)
{
	return 0;
}


/*
 *  multiple-value-call
 */
static int test_function_result_type(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	readstring(&pos, "(progn 100)");
	eval_parse_execute(&pos, pos);
	eval_scope(ptr, &pos, pos);
	test(function_result_type(pos, &pos), "function_result_type1");

	readstring(&pos, "(lambda () 100)");
	eval_parse_execute(&pos, pos);
	eval_scope(ptr, &pos, pos);
	test(! function_result_type(pos, &pos), "function_result_type2");
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "function_result_type3");

	RETURN;
}

static int test_scope_multiple_value_call(void)
{
	addr control, pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	init_eval_stack(ptr);

	readstring(&pos, "(multiple-value-call (lambda () 100) (values 10 20 30) 40)");
	eval_parse_execute(&pos, pos);
	scope_multiple_value_call(ptr, &pos, pos);
	test(eval_scope_p(pos), "scope_multiple_value_call1");
	test(RefEvalScopeType(pos) == EVAL_PARSE_MULTIPLE_VALUE_CALL,
			"scope_multiple_value_call2");
	GetEvalScopeThe(pos, &check);
	test(RefLispDecl(check) == LISPDECL_INTEGER, "scope_multiple_value_call3");
	GetEvalScopeIndex(pos, 0, &check);
	test(RefEvalScopeType(check) == EVAL_PARSE_LAMBDA, "scope_multiple_value_call4");
	GetEvalScopeIndex(pos, 1, &check);
	test(lenarrayr(check) == 2, "scope_multiple_value_call5");

	free_control(ptr, control);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_eval_scope(void)
{
	/* memory */
	TestBreak(test_eval_scope_heap);
	TestBreak(test_eval_scope_size);
	TestBreak(test_make_eval_scope);
	TestBreak(test_StructEvalScope);
	/* scope constant */
	TestBreak(test_scope_nil);
	TestBreak(test_scope_t);
	TestBreak(test_scope_integer);
	TestBreak(test_scope_rational);
	TestBreak(test_scope_character);
	TestBreak(test_scope_array);
	TestBreak(test_scope_vector);
	TestBreak(test_scope_string);
	TestBreak(test_scope_float);
	TestBreak(test_scope_quote);
	TestBreak(test_scope_allcons);
	TestBreak(test_scope_progn);
	TestBreak(test_scope_declaim);
	/* apply_declare */
	TestBreak(test_find_tablevalue);
	TestBreak(test_find_tablefunction);
	TestBreak(test_check_value_declare);
	TestBreak(test_check_function_declare);
	TestBreak(test_check_declare_stack);
	TestBreak(test_apply_declare);
	/* let */
	TestBreak(test_check_scope_variable);
	TestBreak(test_let_init);
	TestBreak(test_make_tablevalue_stack);
	TestBreak(test_let_maketable);
	TestBreak(test_specialp_stack_tablevalue);
	TestBreak(test_specialp_tablevalue);
	TestBreak(test_dynamic_stack_tablevalue);
	TestBreak(test_dynamic_tablevalue);
	TestBreak(test_ignore_stack_tablevalue);
	TestBreak(test_ignore_tablevalue);
	TestBreak(test_type_free_tablevalue);
	TestBreak(test_type_boundary_tablevalue);
	TestBreak(test_type_tablevalue_local);
	TestBreak(test_type_tablevalue_global);
	TestBreak(test_type_and_array);
	TestBreak(test_push_tablevalue_alloc);
	TestBreak(test_checktype_p);
	TestBreak(test_checktype_value);
	TestBreak(test_let_applytable);
	TestBreak(test_ignore_checkvalue);
	TestBreak(test_tablevalue_update);
	TestBreak(test_let_update);
	TestBreak(test_let_execute);
	TestBreak(test_scope_let);
	TestBreak(test_ifdeclvalue);
	TestBreak(test_leta_checktype);
	TestBreak(test_leta_execute);
	TestBreak(test_scope_leta);
	TestBreak(test_let_special);
	/* symbol */
	TestBreak(test_symbol_global_tablevalue);
	TestBreak(test_push_closure_value);
	TestBreak(test_symbol_tablevalue);
	TestBreak(test_find_symbol_scope);
	TestBreak(test_make_scope_symbol);
	TestBreak(test_symbol_macrolet_global_p);
	TestBreak(test_symbol_macrolet_p);
	TestBreak(test_scope_symbol_replace);
	TestBreak(test_scope_symbol);
	TestBreak(test_scope_setq_cons);
	TestBreak(test_scope_setq);
	/* function */
	TestBreak(test_globalp_stack_tablefunction);
	TestBreak(test_globalp_tablefunction);
	TestBreak(test_dynamic_stack_tablefunction);
	TestBreak(test_dynamic_tablefunction);
	TestBreak(test_ignore_stack_tablefunction);
	TestBreak(test_ignore_tablefunction);
	TestBreak(test_inline_stack_tablefunction);
	TestBreak(test_inline_tablefunction);
	TestBreak(test_gettype_global_callname);
	TestBreak(test_type_free_tablefunction);
	TestBreak(test_type_boundary_tablefunction);
	TestBreak(test_type_tablefunction_local);
	TestBreak(test_type_tablefunction_global);
	TestBreak(test_make_tablefunction_stack);
	TestBreak(test_push_tablefunction_alloc);
	TestBreak(test_callname_global_tablefunction);
	TestBreak(test_push_closure_function);
	TestBreak(test_callname_tablefunction);
	TestBreak(test_scope_function);
	/* lambda */
	TestBreak(test_lambda_init_var);
	TestBreak(test_lambda_init_opt);
	TestBreak(test_lambda_init_key);
	TestBreak(test_lambda_init_aux);
	TestBreak(test_lambda_init);
	TestBreak(test_lambda_tablevalue_var);
	TestBreak(test_lambda_tablevalue_opt);
	TestBreak(test_lambda_tablevalue_key);
	TestBreak(test_lambda_tablevalue_aux);
	TestBreak(test_lambda_tablevalue);
	TestBreak(test_type_ordinary_var);
	TestBreak(test_type_ordinary_opt);
	TestBreak(test_type_ordinary_rest);
	TestBreak(test_type_ordinary_key);
	TestBreak(test_make_type_ordinary);
	TestBreak(test_lambda_type_incomplete);
	TestBreak(test_lambda_declare);
	TestBreak(test_lambda_progn);
	TestBreak(test_lambda_update_var);
	TestBreak(test_lambda_update_opt);
	TestBreak(test_lambda_update_key);
	TestBreak(test_lambda_update_aux);
	TestBreak(test_lambda_update);
	TestBreak(test_lambda_closure);
	TestBreak(test_lambda_execute);
	/* defun */
	TestBreak(test_defun_update);
	TestBreak(test_defun_the);
	TestBreak(test_scope_defun);
	/* macro */
	TestBreak(test_macro_init_var);
	TestBreak(test_macro_init_rest);
	TestBreak(test_macro_init_args);
	TestBreak(test_macro_tablevalue_var);
	TestBreak(test_macro_tablevalue_rest);
	TestBreak(test_macro_tablevalue_args);
	TestBreak(test_macro_update_var);
	TestBreak(test_macro_update_rest);
	TestBreak(test_scope_defmacro);
	TestBreak(test_push_symbol_macrolet);
	TestBreak(test_scope_define_symbol_macro);
	TestBreak(test_apply_symbol_macrolet);
	TestBreak(test_scope_symbol_macrolet);
	/* flet */
	TestBreak(test_flet_call);
	TestBreak(test_flet_init);
	TestBreak(test_flet_maketable);
	TestBreak(test_checktype_function);
	TestBreak(test_flet_applytable);
	TestBreak(test_ignore_checkfunction);
	TestBreak(test_flet_update);
	TestBreak(test_scope_flet);
	/* labels */
	TestBreak(test_ifdeclcall);
	TestBreak(test_labels_init);
	TestBreak(test_labels_checktype);
	TestBreak(test_scope_labels);
	/* call */
	TestBreak(test_call_first);
	TestBreak(test_check_tablecall);
	TestBreak(test_callargs_var);
	TestBreak(test_callargs_opt);
	TestBreak(test_callargs_keyvalue);
	TestBreak(test_callargs_key);
	TestBreak(test_callargs_restkey);
	TestBreak(test_callargs_check);
	TestBreak(test_asterisk_function_argument);
	TestBreak(test_callargs_nocheck);
	TestBreak(test_call_args);
	TestBreak(test_call_result);
	TestBreak(test_scope_call);
	/* syntax operator */
	TestBreak(test_values_args);
	TestBreak(test_scope_values);
	TestBreak(test_the_check);
	TestBreak(test_scope_the);
	TestBreak(test_locally_execute);
	TestBreak(test_scope_locally);
	TestBreak(test_scope_if);
	TestBreak(test_scope_unwind_protect);
	/* tagbody */
	TestBreak(test_push_tabletagbody);
	TestBreak(test_tagbody_push);
	TestBreak(test_tagbody_allcons);
	TestBreak(test_tagbody_check);
	TestBreak(test_scope_tagbody);
	TestBreak(test_find_tabletagbody);
	TestBreak(test_push_closure_tagbody);
	TestBreak(test_go_tabletagbody);
	TestBreak(test_go_execute);
	TestBreak(test_scope_go);
	/* block */
	TestBreak(test_push_tableblock);
	TestBreak(test_block_execute);
	TestBreak(test_scope_block);
	TestBreak(test_push_closure_block);
	TestBreak(test_find_tableblock);
	TestBreak(test_name_tableblock);
	TestBreak(test_scope_return_from);
	/* catch */
	TestBreak(test_scope_catch);
	TestBreak(test_scope_throw);
	/* eval-when */
	TestBreak(test_eval_when_check);
	TestBreak(test_scope_eval_when);
	/* multiple-value-bind */
	TestBreak(test_mvbind_maketable);
	/* multiple-value-call */
	TestBreak(test_function_result_type);
	TestBreak(test_scope_multiple_value_call);

	return 0;
}

static void test_build_eval_scope(void)
{
	addr pos, when;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &pos);
	GetConstant(CONSTANT_COMMON_EVAL, &when);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);
}

int test_eval_scope(void)
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
		build_syscall();
		build_common();
		build_readtable();
		build_pathname();
		build_eval_declare();
		build_code();
		test_build_eval_scope();
		lisp_initialize = 1;
		result = testbreak_eval_scope();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

