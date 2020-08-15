#include "code_make.c"

#if 0
#include "character.h"
#include "clos.h"
#include "code.h"
#include "code_object.h"
#include "common.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "degrade.h"
#include "eval_execute.h"
#include "package.h"
#include "parse.h"
#include "pathname.h"
#include "reader.h"
#include "scope.h"
#include "stream.h"
#include "syscall.h"
#include "type.h"

/*
 *  code
 */
static void eval_scope_eval(Execute ptr, addr *ret, addr pos)
{
	addr control;
	codejump jump;

	push_new_control(ptr, &control);
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump)) {
		init_eval_when(ptr);
		eval_scope(ptr, ret, pos);
	}
	end_switch(&jump);
	free_control_(ptr, control);
	throw_switch(&jump);
}

static void codechar_call(addr *ret, const char *str,
		void (*call)(LocalRoot, addr, addr))
{
	addr pos, code;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);

	code_queue_local(local, &code);
	code_queue_push_simple(local, code);
	readstring_debug(&pos, str);
	eval_parse(ptr, &pos, pos);
	eval_scope_eval(ptr, &pos, pos);
	call(local, code, pos);
	code_queue_pop(local, code, ret);

	rollback_local(local, stack);
}
static void codechar_set(addr *ret, const char *str)
{
	codechar_call(ret, str, code_make_execute_set);
}
static void codechar_push(addr *ret, const char *str)
{
	codechar_call(ret, str, code_make_execute_push);
}
static void codechar_rem(addr *ret, const char *str)
{
	codechar_call(ret, str, code_make_execute_rem);
}

static int localhold_runcode_control(Execute ptr, addr pos)
{
	gchold_push_special(ptr, pos);
	return runcode_control(ptr, pos);
}

static int test_code_nil(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	codechar_set(&pos, "nil");
	setvalues_control(ptr, T, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_nil1");

	codechar_push(&pos, "nil");
	setargs_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getargs_control(ptr, 0, &pos);
	test(pos == Nil, "code_nil2");

	codechar_rem(&pos, "nil");
	setvalues_control(ptr, T, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_nil3");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_t(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	codechar_set(&pos, "t");
	setvalues_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_t1");

	codechar_push(&pos, "t");
	setargs_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getargs_control(ptr, 0, &pos);
	test(pos == T, "code_t2");

	codechar_rem(&pos, "t");
	setvalues_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_t3");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_value(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	codechar_set(&pos, "100");
	setvalues_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 100, "code_value1");

	codechar_push(&pos, "200");
	setargs_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getargs_control(ptr, 0, &pos);
	test(RefFixnum(pos) == 200, "code_value2");

	codechar_rem(&pos, "300");
	setvalues_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_value3");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_declaim_special(void)
{
	addr control, pos, symbol;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	codechar_set(&pos, "(declaim (special code-declaim-special-test))");
	readstring_debug(&symbol, "code-declaim-special-test");
	setvalues_control(ptr, T, T, T, NULL);
	setlexical_symbol(symbol);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_declaim_special1");
	test(specialp_symbol(symbol), "code_declaim_special2");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_declaim_type_value(void)
{
	addr control, pos, symbol;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	codechar_set(&pos, "(declaim (type integer code-declaim-type-value-test))");
	readstring_debug(&symbol, "code-declaim-type-value-test");
	localhold_runcode_control(ptr, pos);
	gettype_value_symbol(symbol, &pos);
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "code_declaim_type_value1");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_declaim_type_function(void)
{
	addr control, pos, symbol;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	codechar_set(&pos, "(declaim (ftype (function * integer) "
			"code-declaim-type-function-test))");
	readstring_debug(&symbol, "code-declaim-type-function-test");
	localhold_runcode_control(ptr, pos);
	gettype_function_symbol(symbol, &pos);
	test(RefLispDecl(pos) == LISPDECL_FUNCTION, "code_declaim_type_function1");
	GetArrayType(pos, 1, &pos);
	test(RefLispDecl(pos) == LISPDECL_INTEGER, "code_declaim_type_function2");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_declaim_inline(void)
{
	addr control, pos, symbol;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	codechar_set(&pos, "(declaim "
			"(inline code-declaim-inline-test) "
			"(notinline code-declaim-notinline-test))");
	localhold_runcode_control(ptr, pos);

	readstring_debug(&symbol, "code-declaim-inline-test");
	test(inlinep_function_symbol(symbol), "code_declaim_inline1");
	readstring_debug(&symbol, "code-declaim-notinline-test");
	test(notinlinep_function_symbol(symbol), "code_declaim_inline2");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_declaim_optimize(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	codechar_set(&pos, "(declaim (optimize debug))");
	localhold_runcode_control(ptr, pos);

	getroot_declare(&pos);
	test(get_optimize_debug_declare(pos) == 3, "code_declaim_optimize1");

	codechar_set(&pos,
			"(declaim (optimize (compilation-speed 0) "
			"(safety 2) (speed 3) (space 0)))");
	localhold_runcode_control(ptr, pos);

	getroot_declare(&pos);
	test(get_optimize_compilation_declare(pos) == 0, "code_declaim_optimize2");
	test(get_optimize_safety_declare(pos) == 2, "code_declaim_optimize3");
	test(get_optimize_speed_declare(pos) == 3, "code_declaim_optimize4");
	test(get_optimize_space_declare(pos) == 0, "code_declaim_optimize5");
	test(get_optimize_debug_declare(pos) == 3, "code_declaim_optimize6");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_declaim_declaration(void)
{
	addr control, pos, symbol;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	readstring_debug(&symbol, "code-declaim-declaration-test");
	getroot_declare(&pos);
	getall_declaration_declare(pos, &pos);
	test(! find_list_eq_unsafe(symbol, pos), "code_declaim_declaration1");

	codechar_set(&pos, "(declaim (declaration code-declaim-declaration-test))");
	localhold_runcode_control(ptr, pos);

	getroot_declare(&pos);
	getall_declaration_declare(pos, &pos);
	test(find_list_eq_unsafe(symbol, pos), "code_declaim_declaration2");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_symbol(void)
{
#if 0
	addr control, pos, check, symbol;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);
	codechar_set(&pos, ":hello");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	readstring_debug(&check, ":hello");
	test(pos == check, "code_symbol1");

	readstring_debug(&symbol, "code-symbol-test");
	pushlexical_control(ptr, symbol, fixnumh(10));
	pushspecial_control(ptr, symbol, fixnumh(20));

	/* set */
	setlexical_symbol(symbol);
	codechar_set(&pos, "code-symbol-test");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_symbol2");

	setspecial_symbol(symbol);
	codechar_set(&pos, "code-symbol-test");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_symbol3");

	setlexical_symbol(symbol);
	codechar_set(&pos,
			"(locally (declare (integer code-symbol-test)) "
			"  code-symbol-test)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_symbol4");

	setspecial_symbol(symbol);
	codechar_set(&pos,
			"(locally (declare (integer code-symbol-test)) "
			"  code-symbol-test)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_symbol5");

	/* push */
	setlexical_symbol(symbol);
	codechar_push(&pos, "code-symbol-test");
	setvalues_control(ptr, T, T, T, NULL);
	setargs_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_symbol6");
	getargs_control(ptr, 0, &pos);
	test(RefFixnum(pos) == 10, "code_symbol7");

	setspecial_symbol(symbol);
	codechar_push(&pos, "code-symbol-test");
	setvalues_control(ptr, T, T, T, NULL);
	setargs_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_symbol8");
	getargs_control(ptr, 0, &pos);
	test(RefFixnum(pos) == 20, "code_symbol9");

	setlexical_symbol(symbol);
	codechar_push(&pos,
			"(locally (declare (integer code-symbol-test)) "
			"  code-symbol-test)");
	setvalues_control(ptr, T, T, T, NULL);
	setargs_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_symbol10");
	getargs_control(ptr, 0, &pos);
	test(RefFixnum(pos) == 10, "code_symbol11");

	setspecial_symbol(symbol);
	codechar_push(&pos,
			"(locally (declare (integer code-symbol-test)) "
			"  code-symbol-test)");
	setvalues_control(ptr, T, T, T, NULL);
	setargs_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_symbol12");
	getargs_control(ptr, 0, &pos);
	test(RefFixnum(pos) == 20, "code_symbol13");

	/* remove */
	setlexical_symbol(symbol);
	codechar_rem(&pos, "code-symbol-test");
	setvalues_control(ptr, T, T, T, NULL);
	setargs_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_symbol14");
	getargs_list_control_unsafe(ptr, 0, &pos);
	test(pos == Nil, "code_symbol15");

	setspecial_symbol(symbol);
	codechar_rem(&pos, "code-symbol-test");
	setvalues_control(ptr, T, T, T, NULL);
	setargs_nil_control(ptr);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_symbol16");
	getargs_list_control_unsafe(ptr, 0, &pos);
	test(pos == Nil, "code_symbol17");

	free_control_(ptr, control);

	RETURN;
#endif
	return 0;
}

static int test_code_progn(void)
{
	addr control, pos, check;
	Execute ptr;
	size_t count;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(progn)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_progn1");

	codechar_set(&pos, "(progn 10 20 30 40)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &check);
	test(RefFixnum(check) == 40, "code_progn2");
	getarray_code(pos, &check);
	count = lenarrayr(check);

	codechar_set(&pos, "(progn 10 20 'hello 'aaa :hello 30 40)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &check);
	test(RefFixnum(check) == 40, "code_progn3");
	getarray_code(pos, &check);
	test(count == lenarrayr(check), "code_progn4");

	codechar_push(&pos, "(progn 10 20 30 40)");
	setargs_nil_control(ptr);
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &check);
	test(check == T, "code_progn5");
	getargs_control(ptr, 0, &check);
	test(RefFixnum(check) == 40, "code_progn6");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_let(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(let nil)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_let1");

	codechar_set(&pos, "(let nil 10 20 30)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "code_let2");

	codechar_set(&pos, "(let (a) a)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_let3");

	codechar_set(&pos, "(let ((a 10) (b 20)) b a)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_let4");

	codechar_set(&pos, "(let ((a 10) (b 20)) a b)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_let5");

	codechar_set(&pos, "(let ((a 10)) (let ((a 20) (b a)) a b))");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_let6");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_leta(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(let* nil)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_leta1");

	codechar_set(&pos, "(let* nil 10 20 30)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "code_leta2");

	codechar_set(&pos, "(let* (a) a)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_leta3");

	codechar_set(&pos, "(let* ((a 10) (b 20)) b a)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_leta4");

	codechar_set(&pos, "(let* ((a 10) (b 20)) a b)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_leta5");

	codechar_set(&pos, "(let* ((a 10)) a (let* ((a 20) (b a)) b))");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_leta6");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_setq(void)
{
#if 0
	addr control, pos, symbol;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	readstring_debug(&symbol, "code-setq-test");

	setlexical_symbol(symbol);
	codechar_set(&pos, "(setq code-setq-test 10)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_setq1");
	getlexical_local(ptr, symbol, &pos);
	test(RefFixnum(pos) == 10, "code_setq2");

	setspecial_symbol(symbol);
	codechar_set(&pos, "(setq code-setq-test 20)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_setq3");
	getspecial_local(ptr, symbol, &pos);
	test(RefFixnum(pos) == 20, "code_setq4");

	getlexical_local(ptr, symbol, &pos);
	test(RefFixnum(pos) == 20, "code_setq5");
	getspecial_local(ptr, symbol, &pos);
	test(RefFixnum(pos) == 20, "code_setq6");
	GetValueSymbol(symbol, &pos);
	test(RefFixnum(pos) == 20, "code_setq7");

	setlexical_symbol(symbol);
	codechar_set(&pos, "(let (a code-setq-test) (setq a 30 code-setq-test 40))");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 40, "code_setq8");

	setlexical_symbol(symbol);
	codechar_set(&pos,
			"(let (a code-setq-test)"
			"  (setq a 30 code-setq-test 40) code-setq-test)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 40, "code_setq9");

	free_control_(ptr, control);

	RETURN;
#endif
	return 0;
}

static int test_code_function(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(function car)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(functionp(pos), "code_function1");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_call(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(+)");
	setvalues_control(ptr, T, T, T, NULL);
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 0, "code_call1");

	codechar_set(&pos, "(+ 10 20 30 40 50)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 150, "code_call2");

	codechar_set(&pos, "((lambda () (+ 10 20 30 40 50)))");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 150, "code_call3");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_lambda(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(lambda () (+ 10 20 30 40 50))");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(functionp(pos), "code_lambda1");

	codechar_set(&pos, "(lambda ())");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(functionp(pos), "code_lambda2");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_defun(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(defun aaa () (+ 10 20 30 40 50))");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(symbolp(pos), "code_defun1");

	codechar_set(&pos, "(aaa)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 150, "code_defun2");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_flet(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(flet () 10 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "code_flet1");

	codechar_set(&pos, "(flet ((aaa () 10)) (aaa))");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_flet2");

	codechar_set(&pos,
			"(flet ((aa () 10)) (flet ((aa () 20) (bb () (aa))) (aa) (bb)))");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_flet3");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_labels(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(labels () 10 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "code_labels1");

	codechar_set(&pos, "(labels ((aaa () 10)) (aaa))");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_labels2");

	codechar_set(&pos,
			"(labels ((aa () 10)) (aa) (labels ((aa () 20) (bb () (aa))) (bb)))");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_labels3");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_values(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(values)");
	localhold_runcode_control(ptr, pos);
	test(lengthvalues_control(ptr) == 0, "code_values1");

	codechar_set(&pos, "(values 10)");
	localhold_runcode_control(ptr, pos);
	test(lengthvalues_control(ptr) == 1, "code_values2");
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_values3");

	codechar_set(&pos, "(values 20 30)");
	localhold_runcode_control(ptr, pos);
	test(lengthvalues_control(ptr) == 2, "code_values4");
	getvalues_control(ptr, 0, &pos);
	test(RefFixnum(pos) == 20, "code_values5");
	getvalues_control(ptr, 1, &pos);
	test(RefFixnum(pos) == 30, "code_values6");

	codechar_set(&pos, "(values 40 50 60 70)");
	localhold_runcode_control(ptr, pos);
	test(lengthvalues_control(ptr) == 4, "code_values7");
	getvalues_control(ptr, 0, &pos);
	test(RefFixnum(pos) == 40, "code_values8");
	getvalues_control(ptr, 1, &pos);
	test(RefFixnum(pos) == 50, "code_values9");
	getvalues_control(ptr, 2, &pos);
	test(RefFixnum(pos) == 60, "code_values10");
	getvalues_control(ptr, 3, &pos);
	test(RefFixnum(pos) == 70, "code_values11");

	setargs_nil_control(ptr);
	codechar_push(&pos, "(values)");
	localhold_runcode_control(ptr, pos);
	getargs_list_control_unsafe(ptr, 0, &pos);
	test(singlep(pos), "code_values12");
	GetCar(pos, &pos);
	test(pos == Nil, "code_values13");

	setargs_nil_control(ptr);
	codechar_push(&pos, "(values 10)");
	localhold_runcode_control(ptr, pos);
	getargs_list_control_unsafe(ptr, 0, &pos);
	test(singlep(pos), "code_values14");
	GetCar(pos, &pos);
	test(RefFixnum(pos) == 10, "code_values15");

	setargs_nil_control(ptr);
	codechar_push(&pos, "(values 10 20 30 40 50)");
	localhold_runcode_control(ptr, pos);
	getargs_list_control_unsafe(ptr, 0, &pos);
	test(singlep(pos), "code_values16");
	GetCar(pos, &pos);
	test(RefFixnum(pos) == 10, "code_values17");

	RETURN;
}

static int test_code_the(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(the integer 10)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_the1");

	RETURN;
}

static int test_code_locally(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(locally (declare (special aaa)) 10 20)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_locally1");

	RETURN;
}

static int test_code_if(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(if 10 20)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_if1");

	codechar_set(&pos, "(if nil 20)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_if2");

	codechar_set(&pos, "(if 10 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_if3");

	codechar_set(&pos, "(if nil 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "code_if4");

	codechar_set(&pos, "(if (not 10) 20)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_if5");

	codechar_set(&pos, "(if (not nil) 20)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_if6");

	codechar_set(&pos, "(if (not 10) 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "code_if7");

	codechar_set(&pos, "(if (not nil) 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_if8");

	codechar_set(&pos, "(if (null 10) 20)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_if9");

	codechar_set(&pos, "(if (null nil) 20)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_if10");

	codechar_set(&pos, "(if (null 10) 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "code_if11");

	codechar_set(&pos, "(if (null nil) 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_if12");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_unwind_protect(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(unwind-protect 10)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_unwind_protect1");

	codechar_set(&pos, "(unwind-protect 10 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_unwind_protect2");

	codechar_set(&pos, "(let (a) (unwind-protect 10 (setq a 20) 30) a)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 20, "code_unwind_protect3");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_tagbody(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(tagbody)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_tagbody1");

	codechar_set(&pos, "(tagbody 10 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_tagbody2");

	codechar_set(&pos, "(let (a) (tagbody 10 20 (setq a 100)))");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_tagbody3");

	codechar_set(&pos, "(let (a) (tagbody (go hello) (setq a 100) hello) a)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_tagbody4");

	codechar_set(&pos,
			"(let (a)"
			"  (tagbody"
			"    (tagbody (go 10))"
			"    (go 20)"
			"    10"
			"    (setq a 100)"
			"    20)"
			"  a)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 100, "code_tagbody5");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_block(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(block hello)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_block1");

	codechar_set(&pos, "(block hello 10 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "code_block2");

	codechar_set(&pos, "(block hello 10 (return-from hello 40) 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 40, "code_block3");

	codechar_set(&pos,
			"(block hello"
			"  (block aaa"
			"    (return-from hello 10)"
			"    20)"
			"  30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_block4");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_catch(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(catch 'hello)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_catch1");

	codechar_set(&pos, "(catch 'hello 10 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "code_catch2");

	codechar_set(&pos, "(catch 'hello 10 (throw 'hello 40) 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 40, "code_catch3");

	codechar_set(&pos,
			"(catch 'hello"
			"  (catch 'aaa"
			"    (throw 'hello 10)"
			"    20)"
			"  30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 10, "code_catch4");

	free_control_(ptr, control);

	RETURN;
}

static int test_multiple_value_call1(Execute ptr, addr rest)
{
	addr pos1, pos2, pos3;

	if (! consp(rest)) goto result_nil;
	getcons_(rest, &pos1, &rest);
	if (! consp(rest)) goto result_nil;
	getcons_(rest, &pos2, &rest);
	if (! consp(rest)) goto result_nil;
	getcons_(rest, &pos3, &rest);
	if (rest != Nil) goto result_nil;
	rest = (RefFixnum(pos1) == 10
			&& RefFixnum(pos2) == 20
			&& RefFixnum(pos3) == 30)? T: Nil;
	setresult_control(ptr, rest);
	return 0;

result_nil:
	setresult_control(ptr, Nil);
	return 0;
}

static int test_code_multiple_value_call(void)
{
	addr control, pos, call;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	readstring_debug(&pos, LISP_PACKAGE "::test-multiple-value-call1");
	compiled_system(&call, pos);
	SetPointer(p_debug1, dynamic, test_multiple_value_call1);
	setcompiled_dynamic(call, p_debug1);
	SetFunctionSymbol(pos, call);

	codechar_set(&pos, "(multiple-value-call #'"
			LISP_PACKAGE "::test-multiple-value-call1 "
			"10 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_multiple_value_call1");

	codechar_set(&pos, "(multiple-value-call #'"
			LISP_PACKAGE "::test-multiple-value-call1 "
			"(values 10 20) 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_multiple_value_call2");

	codechar_set(&pos, "(multiple-value-call #'"
			LISP_PACKAGE "::test-multiple-value-call1 "
			"10 (values 20 30))");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_multiple_value_call3");

	codechar_set(&pos, "(multiple-value-call #'"
			LISP_PACKAGE "::test-multiple-value-call1 "
			"(values 10 20 30))");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "code_multiple_value_call4");

	codechar_set(&pos, "(multiple-value-call #'"
			LISP_PACKAGE "::test-multiple-value-call1 "
			"10 20 30 40)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(pos == Nil, "code_multiple_value_call5");

	codechar_set(&pos, "(multiple-value-call #'+)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 0, "code_multiple_value_call6");

	codechar_set(&pos, "(multiple-value-call #'+ (values))");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 0, "code_multiple_value_call7");

	free_control_(ptr, control);

	RETURN;
}


/*
 *  call
 */
static int test_code_make_specialize_symbol_p(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	readstring_debug(&pos, "(hello 10 20 30)");
	eval_parse(ptr, &pos, pos);
	eval_scope_eval(ptr, &pos, pos);
	GetEvalScopeValue(pos, &pos); /* lexical */
	GetEvalScopeIndex(pos, 0, &pos); /* call */
	test(! code_make_specialize_symbol_p(pos, CONSTANT_SYSTEM_HANDLER),
			"code_make_specialize_symbol_p1");

	readstring_debug(&pos, "(" LISP_SYSTEM "::handler 10 20 30)");
	eval_parse(ptr, &pos, pos);
	eval_scope_eval(ptr, &pos, pos);
	GetEvalScopeValue(pos, &pos); /* lexical */
	GetEvalScopeIndex(pos, 0, &pos); /* call */
	test(code_make_specialize_symbol_p(pos, CONSTANT_SYSTEM_HANDLER),
			"code_make_specialize_symbol_p2");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_handler(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(" LISP_SYSTEM "::handler 10 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "call_handler1");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_restart(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(" LISP_SYSTEM "::restart 10 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "call_restart1");

	free_control_(ptr, control);

	RETURN;
}

static int test_code_push_return(void)
{
	addr control, pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &control);

	codechar_set(&pos, "(" LISP_SYSTEM "::push-return 10 20 30)");
	localhold_runcode_control(ptr, pos);
	getresult_control(ptr, &pos);
	test(RefFixnum(pos) == 30, "call_push_return1");

	free_control_(ptr, control);

	RETURN;
}


/*
 *  Main
 */
static int testbreak_code_make(void)
{
	TestBreak(test_code_nil);
	TestBreak(test_code_t);
	TestBreak(test_code_value);
	TestBreak(test_code_declaim_special);
	TestBreak(test_code_declaim_type_value);
	TestBreak(test_code_declaim_type_function);
	TestBreak(test_code_declaim_inline);
	TestBreak(test_code_declaim_optimize);
	TestBreak(test_code_declaim_declaration);
	TestBreak(test_code_symbol);
	TestBreak(test_code_progn);
	TestBreak(test_code_let);
	TestBreak(test_code_leta);
	TestBreak(test_code_setq);
	TestBreak(test_code_function);
	TestBreak(test_code_call);
	TestBreak(test_code_lambda);
	TestBreak(test_code_defun);
	TestBreak(test_code_flet);
	TestBreak(test_code_labels);
	TestBreak(test_code_values);
	TestBreak(test_code_the);
	TestBreak(test_code_locally);
	TestBreak(test_code_if);
	TestBreak(test_code_unwind_protect);
	TestBreak(test_code_tagbody);
	TestBreak(test_code_block);
	TestBreak(test_code_catch);
	TestBreak(test_code_multiple_value_call);
	/* call */
	TestBreak(test_code_make_specialize_symbol_p);
	TestBreak(test_code_handler);
	TestBreak(test_code_restart);
	TestBreak(test_code_push_return);

	return 0;
}

static void test_build_eval_scope(void)
{
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
}
#endif

int test_code_make(void)
{
#if 0
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
		test_build_eval_scope();
		lisp_initialize = 1;
		result = testbreak_code_make();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
#endif
	return 0;
}

