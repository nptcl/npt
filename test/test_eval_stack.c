#include "eval_stack.c"
#include "array.h"
#include "bignum.h"
#include "callname.h"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "condition.h"
#include "constant.h"
#include "copy.h"
#include "degrade.h"
#include "ratio.h"
#include "reader.h"
#include "package.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  memory
 */
static int test_eval_stack_alloc(void)
{
	int i, check;
	addr pos;
	LocalRoot local;
	struct eval_stack *str;

	local = Local_Thread;
	eval_stack_alloc(local, &pos, EVAL_STACK_MODE_NIL);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_stack_alloc1");
	test(RefEvalType(pos) == EVAL_TYPE_STACK, "eval_stack_alloc2");
	test(RefEvalStackType(pos) == EVAL_STACK_MODE_NIL, "eval_stack_alloc3");
	test(lenarrayr(pos) == EVAL_STACK_SIZE, "eval_stack_alloc4");

	str = StructEvalStack(pos);
	check = 1;
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		if (str->optimize[i] != -1)
			check = 0;
	}
	test(check, "eval_stack_alloc5");
	test(str->stack, "eval_stack_alloc6");

	/* local */
	eval_stack_local(local, &pos, EVAL_STACK_MODE_NIL);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_stack_alloc7");
	rollback_local(local, str->stack);

	/* heap */
	eval_stack_heap(&pos, EVAL_STACK_MODE_NIL);
	test(GetType(pos) == LISPTYPE_EVAL, "eval_stack_alloc8");

	RETURN;
}


/*
 *  eval-stack
 */
static int test_getstack_symbol(void)
{
	addr symbol;

	getstack_symbol(&symbol);
	test(GetType(symbol) == LISPTYPE_SYMBOL, "getstack_symbol1");
	test(specialp_symbol(symbol), "getstack_symbol2");

	RETURN;
}

static int test_getglobal_symbol(void)
{
	addr symbol;

	getglobal_symbol(&symbol);
	test(GetType(symbol) == LISPTYPE_SYMBOL, "getglobal_symbol1");
	test(specialp_symbol(symbol), "getglobal_symbol2");

	RETURN;
}

static int test_getstack_eval(void)
{
	Execute ptr;
	addr control, symbol, check;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	fixnum_heap(&check, 100);
	getstack_symbol(&symbol);
	pushspecial_control(ptr, symbol, check);

	getstack_eval(ptr, &symbol);
	test(check == symbol, "getstack_eval1");
	free_control_(ptr, control);

	RETURN;
}

static int test_getglobal_eval(void)
{
	Execute ptr;
	addr control, symbol, check;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	fixnum_heap(&check, 100);
	getglobal_symbol(&symbol);
	pushspecial_control(ptr, symbol, check);

	getglobal_eval(ptr, &symbol);
	test(check == symbol, "getglobal_eval1");
	free_control_(ptr, control);

	RETURN;
}

static void temp_eval_stack(Execute ptr)
{
	addr symbol;
	getstack_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
}

static int test_newstack_eval(void)
{
	addr control, pos1, pos2, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	temp_eval_stack(ptr);
	pos1 = newstack_eval(ptr, EVAL_STACK_MODE_LAMBDA);
	getstack_eval(ptr, &check);
	test(pos1 == check, "newstack_eval1");
	pos2 = newstack_nil(ptr);
	getstack_eval(ptr, &check);
	test(pos2 == check, "newstack_eval2");
	GetEvalStackNext(pos2, &check);
	test(check == pos1, "newstack_eval3");

	getstack_symbol(&check);
	setspecial_local(ptr, check, Nil);
	free_control_(ptr, control);

	RETURN;
}

static int test_closestack_unsafe(void)
{
	addr control, pos0, pos1, pos2, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	temp_eval_stack(ptr);
	pos0 = newstack_nil(ptr);
	pos1 = newstack_lambda(ptr);
	pos2 = newstack_nil(ptr);

	getstack_eval(ptr, &check);
	test(check == pos2, "closestack_unsafe1");
	GetEvalStackNext(check, &check);
	test(check == pos1, "closestack_unsafe2");

	closestack_unsafe(ptr);
	getstack_eval(ptr, &check);
	test(check == pos1, "closestack_unsafe3");

	closestack_unsafe(ptr);
	getstack_eval(ptr, &check);
	test(check == pos0, "closestack_unsafe4");

	getstack_symbol(&check);
	setspecial_local(ptr, check, Nil);
	free_control_(ptr, control);

	RETURN;
}

static int test_freestack_eval(void)
{
	addr control, pos0, pos1, pos2, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	temp_eval_stack(ptr);
	pos0 = newstack_lambda(ptr);
	pos1 = newstack_lambda(ptr);
	pos2 = newstack_nil(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	freestack_eval(ptr, pos2);

	getstack_eval(ptr, &check);
	test(check == pos1, "freestack_eval1");
	GetEvalStackNext(check, &check);
	test(check == pos0, "freestack_eval2");

	getstack_symbol(&check);
	setspecial_local(ptr, check, Nil);
	free_control_(ptr, control);

	RETURN;
}

static int test_begin_eval_stack(void)
{
	addr control, symbol1, symbol2, value1, value2, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	getglobal_symbol(&symbol1);
	getstack_symbol(&symbol2);
	getspecial_local(ptr, symbol1, &value1);
	getspecial_local(ptr, symbol2, &value2);
	begin_eval_stack(ptr);

	getspecial_local(ptr, symbol1, &check);
	test(check != value1,"begin_eval_stack1");
	test(! GetStatusDynamic(check), "begin_eval_stack2");
	getspecial_local(ptr, symbol2, &check);
	test(check != value2,"begin_eval_stack3");
	test(GetStatusDynamic(check), "begin_eval_stack4");

	getstack_symbol(&check);
	setspecial_local(ptr, check, Nil);
	getglobal_symbol(&check);
	setspecial_local(ptr, check, Nil);
	free_control_(ptr, control);

	RETURN;
}

static int test_free_eval_stack(void)
{
	addr control, control2, symbol1, symbol2, value1, value2, check;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	push_new_control(ptr, &control2);

	getglobal_symbol(&symbol1);
	getstack_symbol(&symbol2);
	getspecial_local(ptr, symbol1, &value1);
	getspecial_local(ptr, symbol2, &value2);
	begin_eval_stack(ptr);
	free_eval_stack(ptr);

	newstack_lambda(ptr);
	newstack_lambda(ptr);
	newstack_nil(ptr);
	newstack_nil(ptr);
	free_control_(ptr, control2);

	getspecial_local(ptr, symbol1, &check);
	test(check == Unbound,"free_eval_stack1");
	getspecial_local(ptr, symbol2, &check);
	test(check == Unbound,"free_eval_stack2");
	free_control_(ptr, control);

	RETURN;
}


/*
 *  declaim
 */
static int test_apply_pushnew_stack(void)
{
	int check;
	addr pos, sym1, sym2, list, table, sym3;

	eval_stack_heap(&pos, EVAL_STACK_MODE_NIL);
	readstring(&sym1, "aaa");
	readstring(&sym2, "bbb");
	list_heap(&list, sym1, sym2, NULL);
	apply_pushnew_stack(NULL, pos, list, CONSTANT_SYSTEM_DECLARATION);
	GetEvalStackTable(pos, &table);
	check = getplist_constant(table, CONSTANT_SYSTEM_DECLARATION, &table);
	test(check == 0, "apply_pushnew_stack");
	test(length_list_unsafe(table) == 2, "apply_pushnew_stack");
	test(find_list_eq_unsafe(sym1, table), "apply_pushnew_stack");
	test(find_list_eq_unsafe(sym2, table), "apply_pushnew_stack");

	readstring(&sym3, "ccc");
	list_heap(&list, sym1, sym2, sym3, NULL);
	apply_pushnew_stack(NULL, pos, list, CONSTANT_SYSTEM_DECLARATION);
	GetEvalStackTable(pos, &table);
	check = getplist_constant(table, CONSTANT_SYSTEM_DECLARATION, &table);
	test(check == 0, "apply_pushnew_stack");
	test(length_list_unsafe(table) == 3, "apply_pushnew_stack");
	test(find_list_eq_unsafe(sym1, table), "apply_pushnew_stack");
	test(find_list_eq_unsafe(sym2, table), "apply_pushnew_stack");
	test(find_list_eq_unsafe(sym3, table), "apply_pushnew_stack");

	RETURN;
}

static int test_apply_pushnew_callname_stack(void)
{
	int check;
	addr pos, sym1, sym2, list, table, sym3;

	eval_stack_heap(&pos, EVAL_STACK_MODE_NIL);
	readstring(&sym1, "aaa");
	readstring(&sym2, "bbb");
	parse_callname_alloc(NULL, &sym1, sym1);
	parse_callname_alloc(NULL, &sym2, sym2);
	list_heap(&list, sym1, sym2, NULL);
	apply_pushnew_callname_stack(NULL, pos, list, CONSTANT_SYSTEM_DECLARATION);
	GetEvalStackTable(pos, &table);
	check = getplist_constant(table, CONSTANT_SYSTEM_DECLARATION, &table);
	test(check == 0, "apply_pushnew_callname_stack");
	test(length_list_unsafe(table) == 2, "apply_pushnew_callname_stack");
	test(find_list_callname_unsafe(sym1, table), "apply_pushnew_callname_stack");
	test(find_list_callname_unsafe(sym2, table), "apply_pushnew_callname_stack");

	readstring(&sym3, "ccc");
	parse_callname_alloc(NULL, &sym3, sym3);
	list_heap(&list, sym1, sym2, sym3, NULL);
	apply_pushnew_callname_stack(NULL, pos, list, CONSTANT_SYSTEM_DECLARATION);
	GetEvalStackTable(pos, &table);
	check = getplist_constant(table, CONSTANT_SYSTEM_DECLARATION, &table);
	test(check == 0, "apply_pushnew_callname_stack");
	test(length_list_unsafe(table) == 3, "apply_pushnew_callname_stack");
	test(find_list_callname_unsafe(sym1, table), "apply_pushnew_callname_stack");
	test(find_list_callname_unsafe(sym2, table), "apply_pushnew_callname_stack");
	test(find_list_callname_unsafe(sym3, table), "apply_pushnew_callname_stack");

	RETURN;
}

static int test_apply_plist_stack(void)
{
	int check;
	addr pos, sym1, sym2, sym3, sym4, sym5, sym6, list, table, sym;

	eval_stack_heap(&pos, EVAL_STACK_MODE_NIL);
	readstring(&sym1, "aaa");
	readstring(&sym2, "bbb");
	readstring(&sym3, "ccc");
	readstring(&sym4, "ddd");
	list_heap(&list, sym1, sym2, sym3, sym4, NULL);
	apply_plist_stack(NULL, pos, list, CONSTANT_SYSTEM_INLINE);
	GetEvalStackTable(pos, &table);
	check = getplist_constant(table, CONSTANT_SYSTEM_INLINE, &table);
	test(check == 0, "apply_plist_stack");
	test(length_list_unsafe(table) == 4, "apply_plist_stack");
	test(getplist(table, sym1, &sym) == 0, "apply_plist_stack");
	test(sym == sym2, "apply_plist_stack");
	test(getplist(table, sym3, &sym) == 0, "apply_plist_stack");
	test(sym == sym4, "apply_plist_stack");

	readstring(&sym5, "eee");
	readstring(&sym6, "fff");
	list_heap(&list, sym1, sym5, sym5, sym6, NULL);
	apply_plist_stack(NULL, pos, list, CONSTANT_SYSTEM_INLINE);
	GetEvalStackTable(pos, &table);
	check = getplist_constant(table, CONSTANT_SYSTEM_INLINE, &table);
	test(length_list_unsafe(table) == 6, "apply_plist_stack");
	test(getplist(table, sym1, &sym) == 0, "apply_plist_stack");
	test(sym == sym5, "apply_plist_stack");
	test(getplist(table, sym5, &sym) == 0, "apply_plist_stack");
	test(sym == sym6, "apply_plist_stack");

	RETURN;
}

static int test_apply_plist_callname_stack(void)
{
	int check;
	addr pos, sym1, sym2, sym3, sym4, sym5, sym6, list, table, sym;

	eval_stack_heap(&pos, EVAL_STACK_MODE_NIL);
	readstring(&sym1, "aaa");
	readstring(&sym2, "bbb");
	readstring(&sym3, "ccc");
	readstring(&sym4, "ddd");
	parse_callname_alloc(NULL, &sym1, sym1);
	parse_callname_alloc(NULL, &sym2, sym2);
	parse_callname_alloc(NULL, &sym3, sym3);
	parse_callname_alloc(NULL, &sym4, sym4);
	list_heap(&list, sym1, sym2, sym3, sym4, NULL);
	apply_plist_callname_stack(NULL, pos, list, CONSTANT_SYSTEM_INLINE);
	GetEvalStackTable(pos, &table);
	check = getplist_constant(table, CONSTANT_SYSTEM_INLINE, &table);
	test(check == 0, "apply_plist_callname_stack");
	test(length_list_unsafe(table) == 4, "apply_plist_callname_stack");
	test(getplist_callname(table, sym1, &sym) == 0, "apply_plist_callname_stack");
	test(sym == sym2, "apply_plist_callname_stack");
	test(getplist_callname(table, sym3, &sym) == 0, "apply_plist_callname_stack");
	test(sym == sym4, "apply_plist_callname_stack");

	readstring(&sym5, "eee");
	readstring(&sym6, "fff");
	parse_callname_alloc(NULL, &sym5, sym5);
	parse_callname_alloc(NULL, &sym6, sym6);
	list_heap(&list, sym1, sym5, sym5, sym6, NULL);
	apply_plist_callname_stack(NULL, pos, list, CONSTANT_SYSTEM_INLINE);
	GetEvalStackTable(pos, &table);
	check = getplist_constant(table, CONSTANT_SYSTEM_INLINE, &table);
	test(length_list_unsafe(table) == 6, "apply_plist_callname_stack");
	test(getplist_callname(table, sym1, &sym) == 0, "apply_plist_callname_stack");
	test(sym == sym5, "apply_plist_callname_stack");
	test(getplist_callname(table, sym5, &sym) == 0, "apply_plist_callname_stack");
	test(sym == sym6, "apply_plist_callname_stack");

	RETURN;
}

static int test_apply_optimize_stack(void)
{
	addr pos, decl;
	const OptimizeType *optimize;

	eval_stack_heap(&pos, EVAL_STACK_MODE_NIL);
	readstring(&decl, "((optimize (speed 0) (debug 1) safety))");
	parse_declaim_heap(Execute_Thread, Nil, decl, &decl);
	apply_optimize_stack(pos, decl);

	optimize = StructEvalStack(pos)->optimize;
	test(optimize[EVAL_OPTIMIZE_SPEED] == 0, "apply_optimize_stack");
	test(optimize[EVAL_OPTIMIZE_DEBUG] == 1, "apply_optimize_stack");
	test(optimize[EVAL_OPTIMIZE_SAFETY] == 3, "apply_optimize_stack");
	test(optimize[EVAL_OPTIMIZE_SPACE] == -1, "apply_optimize_stack");

	RETURN;
}

static int test_apply_declaim_stack(void)
{
	int check;
	addr control, pos, table, list, key, value, constant;
	Execute ptr;
	const OptimizeType *optimize;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	begin_eval_stack(ptr);

	readstring(&pos,
			"((declaration hello) (type integer qq ww) (ftype function ee) "
			" (special aa bb cc) (inline aa bb cc) (notinline dd) "
			" (optimize speed compilation-speed))");
	parse_declaim_heap(Execute_Thread, Nil, pos, &pos);
	apply_declaim_stack(ptr, pos);
	getglobal_eval(ptr, &pos);
	GetEvalStackTable(pos, &table);

	/* inline */
	check = getplist_constant(table, CONSTANT_SYSTEM_INLINE, &list);
	test(check == 0, "apply_declaim_stack");
	test(length_list_unsafe(list) == 8, "apply_declaim_stack");
	GetConstant(CONSTANT_COMMON_INLINE, &constant);
	readstring(&key, "aa");
	parse_callname_alloc(NULL, &key, key);
	check = getplist_callname(list, key, &value);
	test(check == 0, "apply_declaim_stack");
	test(value == constant, "apply_declaim_stack");

	/* notinline */
	GetConstant(CONSTANT_COMMON_NOTINLINE, &constant);
	readstring(&key, "dd");
	parse_callname_alloc(NULL, &key, key);
	check = getplist_callname(list, key, &value);
	test(check == 0, "apply_declaim_stack");
	test(value == constant, "apply_declaim_stack");

	/* special */
	check = getplist_constant(table, CONSTANT_SYSTEM_TYPE_SCOPE, &list);
	test(check == 0, "apply_declaim_stack");
	test(length_list_unsafe(list) == 3, "apply_declaim_stack");
	readstring(&key, "aa");
	test(find_list_eq_unsafe(key, list), "apply_declaim_stack");

	/* type */
	check = getplist_constant(table, CONSTANT_SYSTEM_TYPE_VALUE, &list);
	test(check == 0, "apply_declaim_stack");
	test(length_list_unsafe(list) == 4, "apply_declaim_stack");
	readstring(&key, "qq");
	check = getplist(list, key, &value);
	test(check == 0, "apply_declaim_stack");
	test(RefLispDecl(value) == LISPDECL_INTEGER, "apply_declaim_stack");

	/* ftype */
	check = getplist_constant(table, CONSTANT_SYSTEM_TYPE_FUNCTION, &list);
	test(check == 0, "apply_declaim_stack");
	test(length_list_unsafe(list) == 2, "apply_declaim_stack");
	readstring(&key, "ee");
	parse_callname_alloc(NULL, &key, key);
	check = getplist_callname(list, key, &value);
	test(check == 0, "apply_declaim_stack");
	test(RefLispDecl(value) == LISPDECL_FUNCTION, "apply_declaim_stack");

	/* optimize */
	optimize = StructEvalStack(pos)->optimize;
	test(optimize[EVAL_OPTIMIZE_SPEED] == 3, "apply_declaim_stack");
	test(optimize[EVAL_OPTIMIZE_COMPILATION] == 3, "apply_declaim_stack");

	/* declaration */
	check = getplist_constant(table, CONSTANT_SYSTEM_DECLARATION, &list);
	test(check == 0, "apply_declaim_stack");
	test(length_list_unsafe(list) == 1, "apply_declaim_stack");
	readstring(&key, "hello");
	test(find_list_eq_unsafe(key, list), "apply_declaim_stack");

	free_eval_stack(ptr);
	free_control_(ptr, control);

	RETURN;
}


/*
 *  declare
 */
static int test_apply_declare_stack(void)
{
	int check;
	addr control, pos, stack, table, list, key, value, constant;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_new_control(ptr, &control);
	begin_eval_stack(ptr);

	readstring(&pos, "((special aa bb cc) "
			" (dynamic-extent aa bb #'cc #'dd) "
			" (ignore ee #'ff) (ignorable gg hh #'ii #'jj))");
	parse_declare_heap(Execute_Thread, Nil, pos, &pos);
	stack = newstack_nil(ptr);
	apply_declare_stack(local, stack, pos);
	GetEvalStackTable(stack, &table);

	/* special */
	check = getplist_constant(table, CONSTANT_SYSTEM_TYPE_SCOPE, &list);
	test(check == 0, "apply_declare_stack1");
	test(length_list_unsafe(list) == 3, "apply_declare_stack2");
	readstring(&key, "aa");
	test(find_list_eq_unsafe(key, list), "apply_declare_stack3");

	/* dynamic-extent value */
	check = getplist_constant(table, CONSTANT_SYSTEM_DYNAMIC_VALUE, &list);
	test(check == 0, "apply_declare_stack4");
	test(length_list_unsafe(list) == 2, "apply_declare_stack5");
	readstring(&key, "aa");
	test(find_list_eq_unsafe(key, list), "apply_declare_stack6");

	/* dynamic-extent function */
	check = getplist_constant(table, CONSTANT_SYSTEM_DYNAMIC_FUNCTION, &list);
	test(check == 0, "apply_declare_stack7");
	test(length_list_unsafe(list) == 2, "apply_declare_stack8");
	readstring(&key, "cc");
	parse_callname_alloc(NULL, &key, key);
	test(find_list_callname_unsafe(key, list), "apply_declare_stack9");

	/* ignore, ignorable value */
	check = getplist_constant(table, CONSTANT_SYSTEM_IGNORE_VALUE, &list);
	test(check == 0, "apply_declare_stack10");
	test(length_list_unsafe(list) == 6, "apply_declare_stack11");
	GetConstant(CONSTANT_COMMON_IGNORE, &constant);
	readstring(&key, "ee");
	check = getplist(list, key, &value);
	test(check == 0, "apply_declare_stack12");
	test(value == constant, "apply_declare_stack13");

	GetConstant(CONSTANT_COMMON_IGNORABLE, &constant);
	readstring(&key, "gg");
	check = getplist(list, key, &value);
	test(check == 0, "apply_declare_stack14");
	test(value == constant, "apply_declare_stack15");

	/* ignore, ignorable function */
	check = getplist_constant(table, CONSTANT_SYSTEM_IGNORE_FUNCTION, &list);
	test(check == 0, "apply_declare_stack16");
	test(length_list_unsafe(list) == 6, "apply_declare_stack17");
	GetConstant(CONSTANT_COMMON_IGNORE, &constant);
	readstring(&key, "ff");
	parse_callname_alloc(NULL, &key, key);
	check = getplist_callname(list, key, &value);
	test(check == 0, "apply_declare_stack18");
	test(value == constant, "apply_declare_stack19");

	GetConstant(CONSTANT_COMMON_IGNORABLE, &constant);
	readstring(&key, "ii");
	parse_callname_alloc(NULL, &key, key);
	check = getplist_callname(list, key, &value);
	test(check == 0, "apply_declare_stack20");
	test(value == constant, "apply_declare_stack21");

	free_eval_stack(ptr);
	free_control_(ptr, control);

	RETURN;
}

static int test_apply_pushsymbol_stack(void)
{
	int check;
	addr control, stack, symbol, cons, list, var;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_new_control(ptr, &control);
	begin_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&cons, "(aa bb cc)");
	readstring(&symbol, "bb");
	apply_pushsymbol_stack(local, stack, symbol, cons, CONSTANT_SYSTEM_TYPE_SCOPE);
	GetEvalStackTable(stack, &list);
	check = getplist_constant(list, CONSTANT_SYSTEM_TYPE_SCOPE, &list);
	test(check == 0, "apply_pushsymbol_stack");
	test(length_list_unsafe(list) == 1, "apply_pushsymbol_stack");
	GetCar(list, &list);
	test(list == symbol, "apply_pushsymbol_stack");

	readstring(&var, "hello");
	apply_pushsymbol_stack(local, stack, var, cons, CONSTANT_SYSTEM_TYPE_SCOPE);
	GetEvalStackTable(stack, &list);
	check = getplist_constant(list, CONSTANT_SYSTEM_TYPE_SCOPE, &list);
	test(check == 0, "apply_pushsymbol_stack");
	test(length_list_unsafe(list) == 1, "apply_pushsymbol_stack");
	GetCar(list, &list);
	test(list == symbol, "apply_pushsymbol_stack");

	free_eval_stack(ptr);
	free_control_(ptr, control);

	RETURN;
}

static int test_apply_plistsymbol_stack(void)
{
	int check;
	addr control, stack, symbol, cons, list, var;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_new_control(ptr, &control);
	begin_eval_stack(ptr);

	stack = newstack_nil(ptr);
	readstring(&cons, "(aa bb cc dd)");
	readstring(&symbol, "cc");
	apply_plistsymbol_stack(local, stack, symbol, cons, CONSTANT_SYSTEM_TYPE_SCOPE);
	GetEvalStackTable(stack, &list);
	check = getplist_constant(list, CONSTANT_SYSTEM_TYPE_SCOPE, &list);
	test(check == 0, "apply_plistsymbol_stack");
	test(length_list_unsafe(list) == 2, "apply_plistsymbol_stack");
	test(getplist(list, symbol, &list) == 0, "apply_plistsymbol_stack");
	readstring(&symbol, "dd");
	test(list == symbol, "apply_plistsymbol_stack");

	readstring(&var, "hello");
	apply_plistsymbol_stack(local, stack, var, cons, CONSTANT_SYSTEM_TYPE_SCOPE);
	GetEvalStackTable(stack, &list);
	check = getplist_constant(list, CONSTANT_SYSTEM_TYPE_SCOPE, &list);
	test(check == 0, "apply_plistsymbol_stack");
	test(length_list_unsafe(list) == 2, "apply_plistsymbol_stack");
	readstring(&symbol, "cc");
	test(getplist(list, symbol, &list) == 0, "apply_plistsymbol_stack");
	readstring(&symbol, "dd");
	test(list == symbol, "apply_plistsymbol_stack");

	free_eval_stack(ptr);
	free_control_(ptr, control);

	RETURN;
}

static int test_apply_declare_value_stack(void)
{
	addr control, decl, stack, symbol, pos, list, value, constant;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_new_control(ptr, &control);
	begin_eval_stack(ptr);

	readstring(&decl, "((special aa bb cc) (integer aa bb) "
			" (dynamic-extent aa bb #'cc #'dd) "
			" (ignore aa #'bb) (ignorable cc hh #'ii #'jj))");
	parse_declare_heap(Execute_Thread, Nil, decl, &decl);
	stack = newstack_nil(ptr);
	readstring(&symbol, "zz");
	apply_declare_value_stack(local, stack, symbol, decl);
	GetEvalStackTable(stack, &list);
	getplist_constant(list, CONSTANT_SYSTEM_TYPE_SCOPE, &pos);
	test(pos == Nil, "apply_declare_value_stack1");
	getplist_constant(list, CONSTANT_SYSTEM_TYPE_VALUE, &pos);
	test(pos == Nil, "apply_declare_value_stack2");
	getplist_constant(list, CONSTANT_SYSTEM_DYNAMIC_VALUE, &pos);
	test(pos == Nil, "apply_declare_value_stack3");
	getplist_constant(list, CONSTANT_SYSTEM_IGNORE_VALUE, &pos);
	test(pos == Nil, "apply_declare_value_stack4");

	readstring(&symbol, "aa");
	apply_declare_value_stack(local, stack, symbol, decl);
	GetEvalStackTable(stack, &list);
	getplist_constant(list, CONSTANT_SYSTEM_TYPE_SCOPE, &pos);
	test(length_list_unsafe(pos) == 1, "apply_declare_value_stack5");
	test(find_list_eq_unsafe(symbol, pos), "apply_declare_value_stack6");
	getplist_constant(list, CONSTANT_SYSTEM_TYPE_VALUE, &pos);
	test(length_list_unsafe(pos) == 2, "apply_declare_value_stack7");
	test(getplist(pos, symbol, &value) == 0, "apply_declare_value_stack8");
	test(RefLispDecl(value) == LISPDECL_INTEGER, "apply_declare_value_stack9");
	getplist_constant(list, CONSTANT_SYSTEM_DYNAMIC_VALUE, &pos);
	test(length_list_unsafe(pos) == 1, "apply_declare_value_stack10");
	test(find_list_eq_unsafe(symbol, pos), "apply_declare_value_stack11");
	getplist_constant(list, CONSTANT_SYSTEM_IGNORE_VALUE, &pos);
	test(length_list_unsafe(pos) == 2, "apply_declare_value_stack12");
	test(getplist(pos, symbol, &value) == 0, "apply_declare_value_stack13");
	GetConstant(CONSTANT_COMMON_IGNORE, &constant);
	test(constant == value, "apply_declare_value_stack14");

	free_eval_stack(ptr);
	free_control_(ptr, control);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_eval_stack(void)
{
	/* memory */
	TestBreak(test_eval_stack_alloc);
	/* eval-stack */
	TestBreak(test_getstack_symbol);
	TestBreak(test_getglobal_symbol);
	TestBreak(test_getstack_eval);
	TestBreak(test_getglobal_eval);
	TestBreak(test_newstack_eval);
	TestBreak(test_closestack_unsafe);
	TestBreak(test_freestack_eval);
	TestBreak(test_begin_eval_stack);
	TestBreak(test_free_eval_stack);
	/* declaim */
	TestBreak(test_apply_pushnew_stack);
	TestBreak(test_apply_pushnew_callname_stack);
	TestBreak(test_apply_plist_stack);
	TestBreak(test_apply_plist_callname_stack);
	TestBreak(test_apply_optimize_stack);
	TestBreak(test_apply_declaim_stack);
	/* declare */
	TestBreak(test_apply_declare_stack);
	TestBreak(test_apply_pushsymbol_stack);
	TestBreak(test_apply_plistsymbol_stack);
	TestBreak(test_apply_declare_value_stack);

	return 0;
}

int test_eval_stack(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_setjmp(ptr, &code);
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
		build_reader();
		build_pathname();
		build_declare();
		build_code();
		lisp_initialize = 1;
		result = testbreak_eval_stack();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

