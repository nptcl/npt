#include "code_queue.c"
#include "clos.h"
#include "character.h"
#include "condition.h"
#include "control_object.h"
#include "common.h"
#include "declare.h"
#include "degrade.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"

#if 0
/*
 *  code_queue-stack
 */
static int test_alloc_code_stack(void)
{
	addr pos;

	alloc_code_stack(NULL, &pos);
	test(GetType(pos) == LISPSYSTEM_EVALSTACK, "alloc_code_stack1");

	RETURN;
}

static int test_code_stack_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_stack_local(local, &pos);
	test(GetType(pos) == LISPSYSTEM_EVALSTACK, "code_stack_local1");
	test(StructCodeStack(pos)->stack != NULL, "code_stack_local2");

	rollback_local(local, stack);

	RETURN;
}

static int test_free_code_stack(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_stack_local(local, &pos);
	free_code_stack(local, pos);
	test(stack == local->stack, "free_code_stack1");

	rollback_local(local, stack);

	RETURN;
}

static int test_push_code_stack(void)
{
	addr pos, cons, check;
	LocalRoot local;
	struct code_stack *str;

	local = Local_Thread;
	code_stack_local(local, &pos);
	str = StructCodeStack(pos);
	push_code_stack(local, pos, fixnumh(10));
	test(str->size == 1, "push_code_stack1");
	GetCodeStack(pos, CodeStack_Root, &cons);
	test(length_list_unsafe(cons) == 1, "push_code_stack2");
	GetCar(cons, &check);
	test(RefFixnum(check) == 10, "push_code_stack3");

	push_code_stack(local, pos, fixnumh(20));
	test(str->size == 2, "push_code_stack4");
	GetCodeStack(pos, CodeStack_Root, &cons);
	test(length_list_unsafe(cons) == 2, "push_code_stack5");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 20, "push_code_stack6");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 10, "push_code_stack7");

	free_code_stack(local, pos);

	RETURN;
}

static int test_finish_code_stack(void)
{
	addr pos, cons, check;
	LocalRoot local;
	struct code_stack *str;

	local = Local_Thread;
	code_stack_local(local, &pos);
	str = StructCodeStack(pos);
	push_code_stack(local, pos, fixnumh(10));
	push_code_stack(local, pos, fixnumh(20));
	finish_code_stack(local, pos);
	test(str->size == 2, "finish_code_stack1");
	test(str->finish, "finish_code_stack2");
	GetCodeStack(pos, CodeStack_Result, &cons);
	test(length_list_unsafe(cons) == 2, "finish_code_stack3");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 10, "finish_code_stack4");
	GetCons(cons, &check, &cons);
	test(RefFixnum(check) == 20, "finish_code_stack5");
	GetCodeStack(pos, CodeStack_Root, &cons);
	test(cons == Nil, "finish_code_stack6");

	free_code_stack(local, pos);

	RETURN;
}


/*
 *  code_queue
 */
static int test_alloc_code_queue(void)
{
	addr pos;

	alloc_code_queue(NULL, &pos);
	test(eval_code_p(pos), "alloc_code_queue1");

	RETURN;
}

static int test_code_queue_local(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct code_queue *str;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);
	test(eval_code_p(pos), "code_queue_local1");
	str = StructCodeQueue(pos);
	test(str->mode == CodeQueue_ModeSet, "code_queue_local2");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_mode(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct code_queue *str;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);
	str = StructCodeQueue(pos);
	test(code_queue_mode(pos) == CodeQueue_ModeSet, "code_queue_mode1");
	str->mode = CodeQueue_ModePush;
	test(code_queue_mode(pos) == CodeQueue_ModePush, "code_queue_mode2");

	str->mode = CodeQueue_ModeSet;
	test(code_queue_setp(pos), "code_queue_setp1");
	str->mode = CodeQueue_ModePush;
	test(code_queue_pushp(pos), "code_queue_pushp1");
	str->mode = CodeQueue_ModeRemove;
	test(code_queue_remp(pos), "code_queue_remp1");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_save(void)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	struct code_queue *str;
	modeswitch mode;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);
	str = StructCodeQueue(pos);
	str->mode = CodeQueue_ModeSet;
	code_queue_save(pos, &mode);
	str->mode = CodeQueue_ModePush;
	code_queue_rollback(pos, &mode);
	test(str->mode == CodeQueue_ModeSet, "code_queue_save1");

	str->mode = CodeQueue_ModeRemove;
	code_queue_setmode(pos, &mode);
	test(str->mode == CodeQueue_ModeSet, "code_queue_setmode1");
	code_queue_rollback(pos, &mode);
	test(str->mode == CodeQueue_ModeRemove, "code_queue_setmode2");

	str->mode = CodeQueue_ModeSet;
	code_queue_remmode(pos, &mode);
	test(str->mode == CodeQueue_ModeRemove, "remove_modeswitch1");
	code_queue_rollback(pos, &mode);
	test(str->mode == CodeQueue_ModeSet, "remove_modeswitch2");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_add(void)
{
	addr pos, value;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	fixnum_heap(&value, 10);
	code_queue_add(local, pos, value);
	fixnum_heap(&value, 20);
	code_queue_add(local, pos, value);
	GetCodeQueue(pos, CodeQueue_Code, &pos);
	GetCodeStack(pos, CodeStack_Root, &pos);
	test(length_list_unsafe(pos) == 2, "code_queue_add1");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_add2(void)
{
	addr pos, value;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	code_queue_add2(local, pos, fixnumh(10), fixnumh(20));
	GetCodeQueue(pos, CodeQueue_Code, &pos);
	GetCodeStack(pos, CodeStack_Root, &pos);
	test(length_list_unsafe(pos) == 1, "code_queue_add2.1");
	GetCar(pos, &pos);
	test(consp(pos), "code_queue_add2.2");
	GetCons(pos, &pos, &value);
	test(RefFixnum(pos) == 10, "code_queue_add2.3");
	test(RefFixnum(value) == 20, "code_queue_add2.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_push(void)
{
	addr pos, value;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	code_queue_push(local, pos,
			fixnumh(10), fixnumh(20), fixnumh(30), NULL);
	GetCodeQueue(pos, CodeQueue_Code, &pos);
	GetCodeStack(pos, CodeStack_Root, &pos);
	test(length_list_unsafe(pos) == 1, "code_queue_push1");
	GetCar(pos, &pos);
	test(length_list_unsafe(pos) == 3, "code_queue_push2");
	test(consp(pos), "code_queue_push3");
	GetCons(pos, &value, &pos);
	test(RefFixnum(value) == 10, "code_queue_push4");
	GetCons(pos, &value, &pos);
	test(RefFixnum(value) == 20, "code_queue_push5");
	GetCons(pos, &value, &pos);
	test(RefFixnum(value) == 30, "code_queue_push6");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_single(void)
{
	addr pos, value;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	GetCodeQueue(pos, CodeQueue_Code, &pos);
	GetCodeStack(pos, CodeStack_Root, &pos);
	GetCar(pos, &pos);
	test(length_list_unsafe(pos) == 1, "code_queue_single1");
	GetCons(pos, &value, &pos);
	GetConst(CODE_NOP, &pos);
	test(value == pos, "code_queue_single2");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_cons(void)
{
	addr pos, value, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	code_queue_cons(local, pos, CONSTANT_CODE_NOP, fixnumh(10));
	GetCodeQueue(pos, CodeQueue_Code, &pos);
	GetCodeStack(pos, CodeStack_Root, &pos);
	GetCar(pos, &pos);
	test(consp(pos), "code_queue_cons1");
	GetCons(pos, &value, &pos);
	GetConst(CODE_NOP, &check);
	test(value == check, "code_queue_cons2");
	test(RefFixnum(pos) == 10, "code_queue_cons3");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_double(void)
{
	addr pos, value, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	code_queue_double(local, pos, CONSTANT_CODE_NOP, fixnumh(10), fixnumh(20));
	GetCodeQueue(pos, CodeQueue_Code, &pos);
	GetCodeStack(pos, CodeStack_Root, &pos);
	GetCar(pos, &pos);
	test(length_list_unsafe(pos) == 3, "code_queue_double1");
	GetCons(pos, &value, &pos);
	GetConst(CODE_NOP, &check);
	test(value == check, "code_queue_double2");
	GetCons(pos, &value, &pos);
	test(RefFixnum(value) == 10, "code_queue_double3");
	GetCons(pos, &value, &pos);
	test(RefFixnum(value) == 20, "code_queue_double4");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_ifpush(void)
{
	addr pos, value, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	StructCodeQueue(pos)->mode = CodeQueue_ModeSet;
	code_queue_ifpush(local, pos);
	GetCodeQueue(pos, CodeQueue_Code, &check);
	GetCodeStack(check, CodeStack_Root, &check);
	test(check == Nil, "code_queue_ifpush1");

	StructCodeQueue(pos)->mode = CodeQueue_ModePush;
	code_queue_ifpush(local, pos);
	GetCodeQueue(pos, CodeQueue_Code, &check);
	GetCodeStack(check, CodeStack_Root, &check);
	test(singlep(check), "code_queue_ifpush2");
	GetCar(check, &check);
	GetCar(check, &check);
	internchar(LISP_CODE, "PUSH-RESULT", &value);
	test(check == value, "code_queue_ifpush3");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  stack
 */
static int test_code_queue_push_struct(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;
	struct code_queue *str;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);
	str = StructCodeQueue(pos);

	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	test(str->size == 0, "code_queue_push_struct1");
	GetCodeQueue(pos, CodeQueue_Code, &check);
	GetCodeStack(check, CodeStack_Root, &check);
	test(length_list_unsafe(check) == 3, "code_queue_push_struct2");

	code_queue_push_struct(local, pos);
	test(str->size == 1, "code_queue_push_struct3");
	GetCodeQueue(pos, CodeQueue_Code, &check);
	GetCodeStack(check, CodeStack_Root, &check);
	test(length_list_unsafe(check) == 0, "code_queue_push_struct4");

	GetCodeQueue(pos, CodeQueue_Stack, &check);
	GetCar(check, &check);
	GetCodeStack(check, CodeStack_Root, &check);
	test(length_list_unsafe(check) == 3, "code_queue_push_struct5");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_pop_goto_p(void)
{
	addr cons, key;

	GetConst(CODE_GOTO, &key);
	list_heap(&cons, key, T, NULL);
	test(code_queue_pop_goto_p(cons), "code_queue_pop_goto_p1");
	test(! code_queue_pop_goto_p(T), "code_queue_pop_goto_p2");
	list_heap(&cons, T, T, NULL);
	test(! code_queue_pop_goto_p(cons), "code_queue_pop_goto_p3");

	RETURN;
}

static int test_code_queue_pop_tag_p(void)
{
	addr key, cons;

	GetConst(CODE_TAG, &key);
	cons_heap(&cons, key, T);
	test(code_queue_pop_tag_p(cons, &cons), "code_queue_pop_tag_p1");
	test(cons == T, "code_queue_pop_tag_p2");
	test(! code_queue_pop_tag_p(T, NULL), "code_queue_pop_tag_p3");
	GetConst(CODE_GOTO, &key);
	list_heap(&cons, key, T, NULL);
	test(! code_queue_pop_tag_p(cons, NULL), "code_queue_pop_tag_p4");

	RETURN;
}

static int test_code_queue_pop_label_p(void)
{
	addr pos;

	index_heap(&pos, 10U);
	test(code_queue_pop_label_p(pos), "code_queue_pop_label_p1");
	test(! code_queue_pop_label_p(T), "code_queue_pop_label_p2");

	RETURN;
}

static void test_push_testdata(LocalRoot local, addr pos)
{
	code_queue_add(local, pos, index_heapr(10));
	code_queue_cons(local, pos, CONSTANT_CODE_GOTO, index_heapr(40));
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_add(local, pos, index_heapr(20));
	code_queue_add(local, pos, index_heapr(30));
	code_queue_cons(local, pos, CONSTANT_CODE_GOTO, index_heapr(10));
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_cons(local, pos, CONSTANT_CODE_TAG, fixnumh(111));
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_cons(local, pos, CONSTANT_CODE_TAG, fixnumh(222));
	code_queue_cons(local, pos, CONSTANT_CODE_GOTO, index_heapr(50));
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_add(local, pos, index_heapr(40));
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_add(local, pos, index_heapr(50));
}

static int test_code_queue_pop_label(void)
{
	addr pos, cons, label, tag, check;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	test_push_testdata(local, pos);
	GetCodeQueue(pos, CodeQueue_Code, &check);
	GetCodeStack(check, CodeStack_Root, &cons);
	nreverse_list_unsafe(&cons, cons);

	code_queue_pop_label(local, cons, &label, &tag, &size);
	test(length_list_unsafe(label) == 5, "code_queue_pop_label1");
	test(length_list_unsafe(tag) == 2, "code_queue_pop_label2");
	test(size == 10, "code_queue_pop_label3");

	GetCons(label, &pos, &label);
	GetCons(pos, &pos, &check);
	test(RefIndex(pos) == 50, "code_queue_pop_label4");
	test(RefIndex(check) == 10, "code_queue_pop_label5");
	GetCons(label, &pos, &label);
	GetCons(pos, &pos, &check);
	test(RefIndex(pos) == 40, "code_queue_pop_label6");
	test(RefIndex(check) == 9, "code_queue_pop_label7");
	GetCons(label, &pos, &label);
	GetCons(pos, &pos, &check);
	test(RefIndex(pos) == 30, "code_queue_pop_label8");
	test(RefIndex(check) == 2, "code_queue_pop_label9");
	GetCons(label, &pos, &label);
	GetCons(pos, &pos, &check);
	test(RefIndex(pos) == 20, "code_queue_pop_label10");
	test(RefIndex(check) == 2, "code_queue_pop_label11");
	GetCons(label, &pos, &label);
	GetCons(pos, &pos, &check);
	test(RefIndex(pos) == 10, "code_queue_pop_label12");
	test(RefIndex(check) == 0, "code_queue_pop_label13");

	GetCons(tag, &pos, &tag);
	GetCons(pos, &pos, &check);
	test(RefFixnum(pos) == 222, "code_queue_pop_label14");
	test(RefIndex(check) == 7, "code_queue_pop_label15");
	GetCons(tag, &pos, &tag);
	GetCons(pos, &pos, &check);
	test(RefFixnum(pos) == 111, "code_queue_pop_label16");
	test(RefIndex(check) == 5, "code_queue_pop_label17");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_pop_find(void)
{
	addr pos, label, tag, check;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	test_push_testdata(local, pos);
	GetCodeQueue(pos, CodeQueue_Code, &check);
	GetCodeStack(check, CodeStack_Root, &check);
	nreverse_list_unsafe(&check, check);
	code_queue_pop_label(local, check, &label, &tag, &size);

	index_heap(&check, 10);
	size = code_queue_pop_find(label, check);
	test(size == 0, "code_queue_pop_find1");

	index_heap(&check, 20);
	size = code_queue_pop_find(label, check);
	test(size == 2, "code_queue_pop_find2");

	index_heap(&check, 40);
	size = code_queue_pop_find(label, check);
	test(size == 9, "code_queue_pop_find3");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_pop_replace(void)
{
	addr pos, check, label, tag, left, right;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	test_push_testdata(local, pos);
	GetCodeQueue(pos, CodeQueue_Code, &check);
	GetCodeStack(check, CodeStack_Root, &check);
	nreverse_list_unsafe(&check, check);
	code_queue_pop_label(local, check, &label, &tag, &size);

	GetConst(CODE_GOTO, &pos);
	index_heap(&check, 40);
	cons_heap(&check, pos, check);

	code_queue_pop_replace(label, check, &right);
	GetCons(right, &left, &right);
	test(left == pos, "code_queue_pop_replace1");
	test(RefIndex(right) == 9, "code_queue_pop_replace2");

	RETURN;
}

static int test_code_queue_pop_make(void)
{
	addr pos, check, array, left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	test_push_testdata(local, pos);

	GetCodeQueue(pos, CodeQueue_Code, &check);
	GetCodeStack(check, CodeStack_Root, &check);
	nreverse_list_unsafe(&check, check);

	code_queue_pop_make(local, check, &array);
	test(lenarrayr(array) == 10, "code_queue_pop_make1");

	GetArrayA4(array, 0, &right);
	GetCons(right, &left, &right);
	GetConst(CODE_GOTO, &check);
	test(left == check, "code_queue_pop_make2");
	test(RefIndex(right) == 9 ,"code_queue_pop_make3");

	GetArrayA4(array, 2, &right);
	GetCons(right, &left, &right);
	GetConst(CODE_GOTO, &check);
	test(left == check, "code_queue_pop_make4");
	test(RefIndex(right) == 0 ,"code_queue_pop_make5");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_pop_code(void)
{
	addr pos, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_single(local, pos, CONSTANT_CODE_NOP);

	GetCodeQueue(pos, CodeQueue_Code, &check);
	finish_code_stack(local, check);
	code_queue_pop_code(local, check, &check);
	test(GetType(check) == LISPTYPE_CODE, "code_queue_pop_code1");
	getargs_code(check, &check);
	test(lenarrayr(check) == 4, "code_queue_pop_code2");

	rollback_local(local, stack);

	RETURN;
}

static int test_code_queue_pop(void)
{
	addr pos, code, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	code_queue_local(local, &pos);

	code_queue_push_struct(local, pos);
	code_queue_push_struct(local, pos);
	GetCodeQueue(pos, CodeQueue_Code, &check);
	code_queue_push_struct(local, pos);
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_single(local, pos, CONSTANT_CODE_NOP);
	code_queue_single(local, pos, CONSTANT_CODE_NOP);

	code_queue_pop(local, pos, &code);
	test(GetType(code) == LISPTYPE_CODE, "code_queue_pop1");
	getargs_code(code, &code);
	test(lenarrayr(code) == 4, "code_queue_pop2");

	GetCodeQueue(pos, CodeQueue_Code, &pos);
	test(pos == check, "code_queue_pop3");

	rollback_local(local, stack);

	RETURN;
}
#endif


/*
 *  Main
 */
static int testbreak_code_queue(void)
{
#if 0
	/* code_queue-stack */
	TestBreak(test_alloc_code_stack);
	TestBreak(test_code_stack_local);
	TestBreak(test_free_code_stack);
	TestBreak(test_push_code_stack);
	TestBreak(test_finish_code_stack);
	/* code_queue */
	TestBreak(test_alloc_code_queue);
	TestBreak(test_code_queue_local);
	TestBreak(test_code_queue_mode);
	TestBreak(test_code_queue_save);
	TestBreak(test_code_queue_add);
	TestBreak(test_code_queue_add2);
	TestBreak(test_code_queue_push);
	TestBreak(test_code_queue_single);
	TestBreak(test_code_queue_cons);
	TestBreak(test_code_queue_double);
	TestBreak(test_code_queue_ifpush);
	/* stack */
	TestBreak(test_code_queue_push_struct);
	TestBreak(test_code_queue_pop_goto_p);
	TestBreak(test_code_queue_pop_tag_p);
	TestBreak(test_code_queue_pop_label_p);
	TestBreak(test_code_queue_pop_label);
	TestBreak(test_code_queue_pop_find);
	TestBreak(test_code_queue_pop_replace);
	TestBreak(test_code_queue_pop_make);
	TestBreak(test_code_queue_pop_code);
	TestBreak(test_code_queue_pop);
#endif

	return 0;
}

static void test_build_eval_scope(void)
{
	addr pos, when;
	Execute ptr;

	ptr = Execute_Thread;
	push_new_control(ptr, &pos);
	GetConstant(CONSTANT_COMMON_EVAL, &when);
	push_toplevel_eval(ptr, T);
	push_evalwhen_eval(ptr);
}

int test_code_queue(void)
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
		build_reader();
		build_pathname();
		build_declare();
		build_code();
		test_build_eval_scope();
		lisp_initialize = 1;
		result = testbreak_code_queue();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

