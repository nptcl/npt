#include "condition.h"
#include "cons.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "equal.h"
#include "eval_execute.h"
#include "format.h"
#include "function.h"
#include "hashtable.h"
#include "integer.h"
#include "hold.h"
#include "package_symbol.h"
#include "print_write.h"
#include "rt.h"
#include "stream.h"
#include "symbol.h"
#include "type_parse.h"
#include "type_table.h"

/* (defpackage rt ...)
 *   (import lisp-system::infobit 'rt)
 *   (export lisp-system::infobit 'rt)
 *   (import lisp-system::infoprint 'rt)
 *   (export lisp-system::infoprint 'rt)
 */
static void import_export_symbol_rt(constindex index)
{
	addr symbol, package;

	GetConstant(index, &symbol);
	GetConst(PACKAGE_RT, &package);
	Error(import_package_(package, symbol));
	Error(export_package_(package, symbol));
}

static void defpackage_rt(void)
{
	import_export_symbol_rt(CONSTANT_SYSTEM_INFOBIT);
	import_export_symbol_rt(CONSTANT_SYSTEM_INFOPRINT);
}


/* (defvar lisp-rt::*entries* [queue])
 *   *entries*  (root . tail)
 */
static void defvar_entries(void)
{
	addr symbol, pos;

	GetConst(RT_ENTRIES, &symbol);
	setspecial_symbol(symbol);
	queue_heap(&pos);
	SetValueSymbol(symbol, pos);
}


/* (defvar lisp-rt::*entries-table* (make-hash-table :test #'eq)) */
static void defvar_entries_table(void)
{
	addr symbol, pos;

	GetConst(RT_ENTRIES_TABLE, &symbol);
	setspecial_symbol(symbol);
	hashtable_heap(&pos);
	SetValueSymbol(symbol, pos);
}


/* (defvar lisp-rt::*entries-warning* [queue])
 *   *entries*  (root . tail)
 */
static void defvar_entries_warning(void)
{
	addr symbol, pos;

	GetConst(RT_ENTRIES_WARNING, &symbol);
	setspecial_symbol(symbol);
	queue_heap(&pos);
	SetValueSymbol(symbol, pos);
}


/* (defun push-entries (name expr values) ...) -> nil
 *   name    symbol
 *   expr    t
 *   values  list
 */
static int rt_push_entries_(Execute ptr, constindex index, addr name)
{
	addr queue;

	GetConstant(index, &queue);
	Return(getspecialcheck_local_(ptr, queue, &queue));
	Check(! consp(queue), "*entries* error");
	pushqueue_heap(queue, name);

	return 0;
}

static int function_push_entries(Execute ptr, addr name, addr expr, addr values)
{
	addr table, pos, list;

	/* check *entries-table* */
	GetConst(RT_ENTRIES_TABLE, &table);
	Return(getspecialcheck_local_(ptr, table, &table));
	Return(find_hashtable_(table, name, &pos));
	if (pos != Unbound) {
		Return(fmtw_("The deftest ~S is already exist.", name, NULL));
		Return(rt_push_entries_(ptr, CONSTANT_RT_ENTRIES_WARNING, name));
	}
	else  {
		/* push *entries* */
		Return(rt_push_entries_(ptr, CONSTANT_RT_ENTRIES, name));
	}

	/* intern *entries-table* */
	cons_heap(&list, expr, values);
	Return(intern_hashheap_(table, name, &pos));
	SetCdr(pos, list);

	/* result */
	setresult_control(ptr, Nil);
	return 0;
}

static void type_push_entries(addr *ret)
{
	addr arg, values, name, expr;

	GetTypeTable(&name, Symbol);
	GetTypeTable(&expr, T);
	GetTypeTable(&values, List);
	typeargs_var3(&arg, name, expr, values);
	GetTypeTable(&values, Null);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_push_entries(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(RT_PUSH_ENTRIES, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_push_entries);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_push_entries(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rem-all-tests () -> nil
 *   (export 'rem-all-tests 'lisp-rt)
 */
static void export_symbol_rt(addr symbol)
{
	addr package;
	GetPackageSymbol(symbol, &package);
	Error(export_package_(package, symbol));
}

static int rm_all_tests_clear_(Execute ptr, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	clearqueue(pos);

	return 0;
}

static int rem_all_tests_(Execute ptr)
{
	addr symbol, pos;

	/* (setq *entries* (list nil)) */
	Return(rm_all_tests_clear_(ptr, CONSTANT_RT_ENTRIES));
	/* (setq *entries-warning* (list nil)) */
	Return(rm_all_tests_clear_(ptr, CONSTANT_RT_ENTRIES_WARNING));
	/* (clrhash *entries-table*) */
	GetConst(RT_ENTRIES_TABLE, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &pos));
	clear_hashtable(pos);

	return 0;
}

static int function_rem_all_tests(Execute ptr)
{
	Return(rem_all_tests_(ptr));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_rem_all_tests(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeTable(&values, Null);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_rem_all_tests(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(RT_REM_ALL_TESTS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_rem_all_tests);
	SetFunctionSymbol(symbol, pos);
	export_symbol_rt(symbol);
	/* type */
	type_rem_all_tests(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro deftest (name expr &rest values) ...)
 *   name    symbol
 *   expr    t
 *   values  &rest t
 *   (export 'deftest 'lisp-rt)
 */
static int function_deftest(Execute ptr, addr form, addr env)
{
	addr args, name, expr, quote, push;

	/* arguments */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &name, &args);
	if (! symbolp(name))
		return fmte_("The deftest name ~S must be a symbol.", name, NULL);
	if (! consp(args))
		goto error;
	GetCons(args, &expr, &args);
	/* `(push-entries ',name ',expr ',value) */
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&name, quote, name, NULL);
	list_heap(&expr, quote, expr, NULL);
	list_heap(&args, quote, args, NULL);
	GetConst(RT_PUSH_ENTRIES, &push);
	list_heap(&push, push, name, expr, args, NULL);
	setresult_control(ptr, push);
	return 0;

error:
	return fmte_("The deftest ~S "
			"must be a (deftest name expr . values) form.", form, NULL);
}

static void defmacro_deftest(void)
{
	addr symbol, pos, type;

	GetConst(RT_DEFTEST, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_deftest);
	setmacro_symbol(symbol, pos);
	export_symbol_rt(symbol);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro deftest-error (name expr &optional (error error))
 *   `(deftest ,name
 *      (handler-case
 *        ,expr
 *        (,error () 'lisp-rt::error))
 *      ,lisp-rt::error))
 *   name    symbol
 *   expr    t
 *   error   symbol
 *   (export 'deftest-error 'lisp-rt)
 *
 */
static int function_deftest_error(Execute ptr, addr form, addr env)
{
	addr args, name, expr, error, symbol, rterror, deftest, handler_case, quote;

	/* args */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &name, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &expr, &args);
	if (args == Nil) {
		GetConst(COMMON_ERROR, &error);
		goto make_deftest;
	}
	if (! consp(args))
		goto error;
	GetCons(args, &error, &args);
	if (args != Nil)
		goto error;
	goto make_deftest;

	/* make body */
make_deftest:
	GetConst(RT_ERROR, &rterror);
	GetConst(RT_DEFTEST, &deftest);
	GetConst(COMMON_HANDLER_CASE, &handler_case);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&symbol, quote, rterror, NULL);
	list_heap(&error, error, Nil, symbol, NULL);
	list_heap(&handler_case, handler_case, expr, error, NULL);
	list_heap(&form, deftest, name, handler_case, rterror, NULL);
	setresult_control(ptr, form);
	return 0;

error:
	return fmte_("Invalid deftest-error form ~S.", form, NULL);
}

static void defmacro_deftest_error(void)
{
	addr symbol, pos, type;

	GetConst(RT_DEFTEST_ERROR, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_deftest_error);
	setmacro_symbol(symbol, pos);
	export_symbol_rt(symbol);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun do-tests (&optional delete) ...) -> boolean
 *   (export 'do-tests 'lisp-rt)
 */
static int do_test_equal_(Execute ptr, addr expr, addr values, addr *rvalues, int *ret)
{
	int check1, check2;
	addr result, pos1, pos2;

	/* (eval expr) */
	Return(eval_execute_partial(ptr, expr));
	getvalues_list_control_local(ptr, &result);
	*rvalues = result;

	/* values check */
	for (;;) {
		check1 = (values == Nil);
		check2 = (result == Nil);
		if (check1 && check2)
			break;
		if (check1 || check2)
			return Result(ret, 0);
		GetCons(values, &pos1, &values);
		GetCons(result, &pos2, &result);
		Return(equalrt_function_(pos1, pos2, &check1));
		if (! check1)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int do_test_output_loop_(Execute ptr, addr io, const char *str, addr list)
{
	/* format_stream(ptr, io, "  *** Expect:~{ ~S~}~%", values, NULL); */
	addr pos;

	Return(print_ascii_stream_(io, str));
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(write_char_stream_(io, ' '));
		Return(prin1_print(ptr, io, pos));
	}

	return terpri_stream_(io);
}

static int do_test_output_(Execute ptr,
		addr io, addr name, fixnum index,
		addr values, addr result, int check)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	fixnum_heap(&pos, index);
	if (check) {
		Return(format_stream(ptr, io, "~&[RT] ~6@A: ~A~%", pos, name, NULL));
	}
	else {
		Return(format_stream(ptr, io, "~&[ERROR] ~6@A: ~A~%", pos, name, NULL));
		Return(do_test_output_loop_(ptr, io, "  *** Expect:", values));
		Return(do_test_output_loop_(ptr, io, "  *** Actial:", result));
	}
	rollback_local(local, stack);

	return 0;
}

static int do_test_output_unhandling_(Execute ptr,
		addr io, addr name, addr values, fixnum index)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	fixnum_heap(&pos, index);
	Return(format_stream(ptr, io, "~&[ERROR] ~6@A: ~A~%", pos, name, NULL));
	Return(do_test_output_loop_(ptr, io, "  *** Expect:", values));
	Return(format_stream(ptr, io,
				"  *** Actual: #<System error, unhandling signal>~%", NULL));
	rollback_local(local, stack);

	return 0;
}

static int do_test_execute_(Execute ptr,
		addr io, addr name, fixnum index,
		addr expr, addr values, int *ret)
{
	int check;
	addr result;

	Return(do_test_equal_(ptr, expr, values, &result, &check));
	Return(do_test_output_(ptr, io, name, index, values, result, check));

	return Result(ret, check);
}

static int do_test_(Execute ptr,
		addr io, addr name, addr table, fixnum index, int *ret)
{
	int check;
	addr control, expr, values;
	codejump jump;

	/* table */
	Return(find_hashtable_(table, name, &expr));
	if (expr == Unbound)
		return fmte_("The deftest ~S is not exist.", name, NULL);
	GetCons(expr, &expr, &values);

	/* test */
	check = 0;
	push_new_control(ptr, &control);
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump)) {
		if (do_test_execute_(ptr, io, name, index, expr, values, &check))
			exitexecute(ptr, LISPCODE_ERROR);
	}
	end_switch(&jump);
	if (codejump_error_p(&jump)) {
		Return(do_test_output_unhandling_(ptr, io, name, values, index));
		check = 0; /* error */
	}
	if (free_control_(ptr, control)) {
		Return(do_test_output_unhandling_(ptr, io, name, values, index));
		check = 0; /* error */
	}

	return Result(ret, check);
}

static int function_do_tests_getindex(Execute ptr, fixnum *ret)
{
	addr symbol, value;

	GetConst(RT_INDEX, &symbol);
	getspecial_local(ptr, symbol, &value);
	if (value == Unbound)
		*ret = 0;
	else
		GetFixnum(value, ret);

	return 0;
}

static int function_do_tests_setindex(Execute ptr, fixnum index)
{
	addr symbol, value;

	GetConst(RT_INDEX, &symbol);
	fixnum_heap(&value, index);
	setspecial_local(ptr, symbol, value);

	return 0;
}

static int function_do_tests_variables_(Execute ptr,
		addr *rio, addr *rlist, addr *rtable)
{
	addr io, list, table;

	Return(debug_io_stream_(ptr, &io));
	GetConst(RT_ENTRIES, &list);
	Return(getspecialcheck_local_(ptr, list, &list));
	rootqueue(list, &list);
	GetConst(RT_ENTRIES_TABLE, &table);
	Return(getspecialcheck_local_(ptr, table, &table));

	*rio = io;
	*rlist = list;
	*rtable = table;

	return 0;
}

static int function_do_tests_output2_(Execute ptr, addr io, fixnum count2)
{
	addr root2;

	if (count2 == 0)
		return 0;
	make_index_integer_heap(&root2, count2);
	gchold_push_local(ptr->local, root2);
	Return(format_stream(ptr, io, "~%", NULL));
	Return(format_stream(ptr, io, "*************~%", NULL));
	Return(format_stream(ptr, io, "*** ERROR ***~%", NULL));
	Return(format_stream(ptr, io, "*************~2%", NULL));
	Return(format_stream(ptr, io, "ERROR = ~A~%", root2, NULL));

	return 0;
}

static int function_do_tests_duplicated_(Execute ptr, addr io)
{
	addr pos;

	/* *entries-warning* */
	GetConst(RT_ENTRIES_WARNING, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	rootqueue(pos, &pos);
	if (pos != Nil) {
		Return(format_stream(ptr, io,
					"~&[DUPLICATED] These testcases is ignored.~%", NULL));
		Return(format_stream(ptr, io,
					"  *** Testcase: ~A~2%", pos, NULL));
	}

	return 0;
}

static int function_do_tests_execute_(Execute ptr, fixnum *value)
{
	int check;
	addr list, table, name, root1, root2, io;
	fixnum index, count1, count2;
	LocalRoot local;
	LocalStack stack;

	/* initialize */
	Return(function_do_tests_variables_(ptr, &io, &list, &table));
	root1 = root2 = Nil;
	count1 = count2 = 0;
	local = ptr->local;
	push_local(local, &stack);

	/* loop */
	for (index = *value; list != Nil; ) {
		GetCons(list, &name, &list);
		Return(do_test_(ptr, io, name, table, ++index, &check));
		if (check) {
			/* ok */
			cons_local(local, &root1, name, root1);
			count1++;
		}
		else {
			/* error */
			cons_local(local, &root2, name, root2);
			count2++;
		}
	}

	/* output */
	Return(function_do_tests_output2_(ptr, io, count2));
	Return(function_do_tests_duplicated_(ptr, io));

	/* result */
	rollback_local(local, stack);
	setbool_control(ptr, count2 == 0);
	return Result(value, index);
}

static int function_do_tests(Execute ptr, addr rest)
{
	fixnum index;

	/* argument */
	if (GetKeyArgs(rest, KEYWORD_TEST, &rest))
		rest = Nil;

	/* do-tests */
	function_do_tests_getindex(ptr, &index);
	Return(function_do_tests_execute_(ptr, &index));
	function_do_tests_setindex(ptr, index);

	/* rem-all-tests */
	if (rest != Nil) {
		Return(rem_all_tests_(ptr));
	}

	return 0;
}

static void type_do_tests(addr *ret)
{
	addr arg, values;

	KeyTypeTable(&arg, TEST, T);
	list_heap(&arg, arg, NULL);
	typeargs_key(&arg, arg);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_do_tests(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(RT_DO_TESTS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_do_tests);
	SetFunctionSymbol(symbol, pos);
	export_symbol_rt(symbol);
	/* type */
	type_do_tests(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equalrt (a b) ...) -> boolean */
static int function_equalrt(Execute ptr, addr a, addr b)
{
	int check;

	Return(equalrt_function_(a, b, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_equalrt(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(RT_EQUALRT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_equalrt);
	SetFunctionSymbol(symbol, pos);
	export_symbol_rt(symbol);
	/* type */
	GetTypeCompiled(&type, Eq);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  build
 */
_g void init_rt(void)
{
	SetPointerCall(defun, var3, push_entries);
	SetPointerCall(defun, empty, rem_all_tests);
	SetPointerCall(defmacro, macro, deftest);
	SetPointerCall(defmacro, macro, deftest_error);
	SetPointerCall(defun, dynamic, do_tests);
	SetPointerCall(defun, var2, equalrt);
}

_g void build_rt(void)
{
	defpackage_rt();
	defvar_entries();
	defvar_entries_table();
	defvar_entries_warning();
	defun_push_entries();
	defun_rem_all_tests();
	defmacro_deftest();
	defmacro_deftest_error();
	defun_do_tests();
	defun_equalrt();
}

