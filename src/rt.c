#include "condition.h"
#include "cons.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "equal.h"
#include "eval.h"
#include "format.h"
#include "function.h"
#include "hashtable.h"
#include "integer.h"
#include "hold.h"
#include "package.h"
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
_g void import_export_symbol_rt(constindex index)
{
	addr symbol, package;

	GetConstant(index, &symbol);
	GetConst(PACKAGE_RT, &package);
	import_package(package, symbol);
	export_package(package, symbol);
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
static void rt_push_entries(Execute ptr, constindex index, addr name)
{
	addr queue;

	GetConstant(index, &queue);
	getspecialcheck_local(ptr, queue, &queue);
	Check(! consp(queue), "*entries* error");
	pushqueue_heap(queue, name);
}

static int function_push_entries(Execute ptr, addr name, addr expr, addr values)
{
	addr table, pos, list;

	/* check *entries-table* */
	GetConst(RT_ENTRIES_TABLE, &table);
	getspecialcheck_local(ptr, table, &table);
	if (findvalue_hashtable(table, name, &pos)) {
		fmtw("The deftest ~S is already exist.", name, NULL);
		rt_push_entries(ptr, CONSTANT_RT_ENTRIES_WARNING, name);
	}
	else  {
		/* push *entries* */
		rt_push_entries(ptr, CONSTANT_RT_ENTRIES, name);
	}

	/* intern *entries-table* */
	cons_heap(&list, expr, values);
	intern_hashheap(table, name, &pos);
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
	compiled_heap(&pos, symbol);
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
static void export_symbol(addr symbol)
{
	addr package;

	GetPackageSymbol(symbol, &package);
	export_package(package, symbol);
}

static void rm_all_tests_clear(Execute ptr, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	clearqueue(pos);
}

static void rem_all_tests(Execute ptr)
{
	addr symbol, pos;

	/* (setq *entries* (list nil)) */
	rm_all_tests_clear(ptr, CONSTANT_RT_ENTRIES);
	/* (setq *entries-warning* (list nil)) */
	rm_all_tests_clear(ptr, CONSTANT_RT_ENTRIES_WARNING);
	/* (clrhash *entries-table*) */
	GetConst(RT_ENTRIES_TABLE, &symbol);
	getspecialcheck_local(ptr, symbol, &pos);
	clear_hashtable(pos);
}

static int function_rem_all_tests(Execute ptr)
{
	rem_all_tests(ptr);
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
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_rem_all_tests);
	SetFunctionSymbol(symbol, pos);
	export_symbol(symbol);
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
	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &name, &args);
	if (! symbolp(name))
		fmte("The deftest name ~S must be a symbol.", name, NULL);
	if (! consp(args)) goto error;
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
	fmte("The deftest ~S must be a (deftest name expr . values) form.", form, NULL);
	return 0;
}

static void defmacro_deftest(void)
{
	addr symbol, pos, type;

	GetConst(RT_DEFTEST, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_deftest);
	setmacro_symbol(symbol, pos);
	export_symbol(symbol);
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
	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &name, &args);
	if (! consp(args)) goto error;
	GetCons(args, &expr, &args);
	if (args == Nil) {
		GetConst(COMMON_ERROR, &error);
		goto make_deftest;
	}
	if (! consp(args)) goto error;
	GetCons(args, &error, &args);
	if (args != Nil) goto error;
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
	fmte("Invalid deftest-error form ~S.", form, NULL);
	return 0;
}

static void defmacro_deftest_error(void)
{
	addr symbol, pos, type;

	GetConst(RT_DEFTEST_ERROR, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_deftest_error);
	setmacro_symbol(symbol, pos);
	export_symbol(symbol);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun do-tests (&optional delete) ...) -> boolean
 *   (export 'do-tests 'lisp-rt)
 */
static int do_test_equal(Execute ptr, addr expr, addr values, addr *ret)
{
	int check1, check2;
	addr result, pos1, pos2;

	/* (eval expr) */
	push_toplevel_eval(ptr, Nil);
	push_evalwhen_eval(ptr);
	if (eval_execute(ptr, expr))
		Abort("Invalid signals.");
	getvalues_list_control_local(ptr, &result);
	*ret = result;

	/* values check */
	for (;;) {
		check1 = (values == Nil);
		check2 = (result == Nil);
		if (check1 && check2) break;
		if (check1 || check2) return 0;
		GetCons(values, &pos1, &values);
		GetCons(result, &pos2, &result);
		if (! equalrt_function(pos1, pos2)) return 0;
	}

	return 1;
}

static void do_test_output_loop(Execute ptr, addr io, const char *str, addr list)
{
	/* format_stream(ptr, io, "  *** Expect:~{ ~S~}~%", values, NULL); */
	addr pos;

	print_ascii_stream(io, str);
	while (list != Nil) {
		getcons(list, &pos, &list);
		write_char_stream(io, ' ');
		prin1_print(ptr, io, pos);
	}
	terpri_stream(io);
}

static void do_test_output(Execute ptr, addr io,
		int check, addr name, addr values, addr result, fixnum index)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	fixnum_heap(&pos, index);
	if (check) {
		format_stream(ptr, io, "~&[RT] ~6@A: ~A~%", pos, name, NULL);
	}
	else {
		format_stream(ptr, io, "~&[ERROR] ~6@A: ~A~%", pos, name, NULL);
		do_test_output_loop(ptr, io, "  *** Expect:", values);
		do_test_output_loop(ptr, io, "  *** Actial:", result);
	}

	rollback_local(local, stack);
}

static void do_test_output_unhandling(Execute ptr,
		addr io, addr name, addr values, fixnum index)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	fixnum_heap(&pos, index);
	format_stream(ptr, io, "~&[ERROR] ~6@A: ~A~%", pos, name, NULL);
	do_test_output_loop(ptr, io, "  *** Expect:", values);
	format_stream(ptr, io, "  *** Actual: #<System error, unhandling signal>~%", NULL);
	rollback_local(local, stack);
}

static int do_test(Execute ptr, addr io, addr name, addr table, fixnum index)
{
	int check;
	addr control, expr, values, result;
	codejump jump;

	if (! findvalue_hashtable(table, name, &expr))
		fmte("The deftest ~S is not exist.", name, NULL);
	GetCons(expr, &expr, &values);

	push_new_control(ptr, &control);
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump)) {
		check = do_test_equal(ptr, expr, values, &result);
		do_test_output(ptr, io, check, name, values, result, index);
	}
	end_switch(&jump);
	if (codejump_error_p(&jump)) {
		do_test_output_unhandling(ptr, io, name, values, index);
		check = 0;
	}
	if (free_control_(ptr, control)) {
		do_test_output_unhandling(ptr, io, name, values, index);
		check = 0;
	}

	return check;
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

static int function_do_tests_execute(Execute ptr, fixnum *value)
{
	addr list, table, name, root1, root2, io;
	fixnum index, count1, count2;
	LocalRoot local;
	LocalStack stack;

	debug_io_stream(ptr, &io);
	GetConst(RT_ENTRIES, &list);
	getspecialcheck_local(ptr, list, &list);
	GetConst(RT_ENTRIES_TABLE, &table);
	getspecialcheck_local(ptr, table, &table);
	root1 = root2 = Nil;
	count1 = count2 = 0;
	rootqueue(list, &list);

	local = ptr->local;
	push_local(local, &stack);
	for (index = *value; list != Nil; ) {
		GetCons(list, &name, &list);
		if (do_test(ptr, io, name, table, ++index)) {
			cons_local(local, &root1, name, root1);
			count1++;
		}
		else {
			cons_local(local, &root2, name, root2);
			count2++;
		}
	}
	if (count2) {
		make_index_integer_heap(&root2, count2);
		gchold_push_local(local, root2);
		format_stream(ptr, io, "~%", NULL);
		format_stream(ptr, io, "*************~%", NULL);
		format_stream(ptr, io, "*** ERROR ***~%", NULL);
		format_stream(ptr, io, "*************~2%", NULL);
		format_stream(ptr, io, "ERROR = ~A~%", root2, NULL);
	}

	/* *entries-warning* */
	GetConst(RT_ENTRIES_WARNING, &list);
	getspecialcheck_local(ptr, list, &list);
	rootqueue(list, &list);
	if (list != Nil) {
		format_stream(ptr, io, "~&[DUPCATED] These testcases is ignored.~%", NULL);
		format_stream(ptr, io, "  *** Testcase: ~A~2%", list, NULL);
	}

	rollback_local(local, stack);
	*value = index;

	setbool_control(ptr, count2 == 0);
	return 0;
}

static int function_do_tests(Execute ptr, addr rest)
{
	fixnum index;

	/* argument */
	if (getkeyargs(rest, KEYWORD_TEST, &rest))
		rest = Nil;

	/* do-tests */
	function_do_tests_getindex(ptr, &index);
	function_do_tests_execute(ptr, &index);
	function_do_tests_setindex(ptr, index);

	/* rem-all-tests */
	if (rest != Nil)
		rem_all_tests(ptr);

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
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_do_tests);
	SetFunctionSymbol(symbol, pos);
	export_symbol(symbol);
	/* type */
	type_do_tests(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equalrt (a b) ...) -> boolean */
static int function_equalrt(Execute ptr, addr a, addr b)
{
	setbool_control(ptr, equalrt_function(a, b));
	return 0;
}

static void defun_equalrt(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(RT_EQUALRT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_equalrt);
	SetFunctionSymbol(symbol, pos);
	export_symbol(symbol);
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

