#include "code_make.h"
#include "code_object.h"
#include "code_queue.h"
#include "cons.h"
#include "cons_list.h"
#include "eval.h"
#include "eval_table.h"
#include "memory.h"

/*
 *  code_stack
 */
struct code_stack {
	unsigned finish : 1;
	unsigned p_control : 1;
	unsigned p_args : 1;
	LocalStack stack;
	size_t size;
};

enum CodeStack_Mode {
	CodeStack_Root,
	CodeStack_Result,
	CodeStack_Size
};

#define RefCodeStack		RefArraySS
#define GetCodeStack		GetArraySS
#define SetCodeStack		SetArraySS
#define PtrCodeStack(x)		PtrBodySSa(x, CodeStack_Size)
#define StructCodeStack(p)	((struct code_stack *)PtrCodeStack(p))

static void alloc_code_stack(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret, LISPSYSTEM_EVALSTACK,
			CodeStack_Size,
			sizeoft(struct code_stack));
}

static void code_stack_local(LocalRoot local, addr *ret)
{
	addr pos;
	struct code_stack *ptr;
	LocalStack stack;

	Check(local == NULL, "local error");
	push_local(local, &stack);
	alloc_code_stack(local, &pos);
	ptr = StructCodeStack(pos);
	clearpoint(ptr);
	ptr->stack = stack;
	*ret = pos;
}

static void free_code_stack(LocalRoot local, addr pos)
{
	rollback_local(local, StructCodeStack(pos)->stack);
}

static void push_code_stack(LocalRoot local, addr pos, addr value)
{
	addr root;
	struct code_stack *ptr;

	ptr = StructCodeStack(pos);
	Check(ptr->finish, "finish error");
	GetCodeStack(pos, CodeStack_Root, &root);
	cons_local(local, &root, value, root);
	SetCodeStack(pos, CodeStack_Root, root);
	ptr->size++;
}

static void finish_code_stack(LocalRoot local, addr pos)
{
	addr root;
	struct code_stack *ptr;

	ptr = StructCodeStack(pos);
	Check(ptr->finish, "finish error");
	GetCodeStack(pos, CodeStack_Root, &root);
	nreverse_list_unsafe(&root, root);
	SetCodeStack(pos, CodeStack_Root, Nil);
	SetCodeStack(pos, CodeStack_Result, root);
	ptr->finish = 1;
}


/*
 *  code_queue
 */
static void alloc_code_queue(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_CODE,
			CodeQueue_Size,
			sizeoft(struct code_queue));
}

_g void code_queue_local(LocalRoot local, addr *ret)
{
	addr pos, stack;
	struct code_queue *ptr;

	Check(local == NULL, "local error");
	alloc_code_queue(local, &pos);
	/* array */
	code_stack_local(local, &stack);
	SetCodeQueue(pos, CodeQueue_Code, stack);
	/* body */
	ptr = StructCodeQueue(pos);
	clearpoint(ptr);
	ptr->mode = CodeQueue_ModeSet;
	/* result */
	*ret = pos;
}

_g enum CodeQueue_Mode code_queue_mode(addr code)
{
	CheckTypeCodeQueue(code);
	return StructCodeQueue(code)->mode;
}

_g int code_queue_setp(addr code)
{
	return code_queue_mode(code) == CodeQueue_ModeSet;
}

_g int code_queue_pushp(addr code)
{
	return code_queue_mode(code) == CodeQueue_ModePush;
}

_g int code_queue_remp(addr code)
{
	return code_queue_mode(code) == CodeQueue_ModeRemove;
}

static void code_queue_save(addr code, modeswitch *mode)
{
	CheckTypeCodeQueue(code);
	mode->mode = code_queue_mode(code);
}

_g void code_queue_rollback(addr code, modeswitch *mode)
{
	CheckTypeCodeQueue(code);
	StructCodeQueue(code)->mode = mode->mode;
}

static void code_queue_savevalue(addr code, modeswitch *mode, enum CodeQueue_Mode value)
{
	code_queue_save(code, mode);
	StructCodeQueue(code)->mode = value;
}

_g void code_queue_setmode(addr code, modeswitch *mode)
{
	code_queue_savevalue(code, mode, CodeQueue_ModeSet);
}
_g void code_queue_pushmode(addr code, modeswitch *mode)
{
	code_queue_savevalue(code, mode, CodeQueue_ModePush);
}
_g void code_queue_remmode(addr code, modeswitch *mode)
{
	code_queue_savevalue(code, mode, CodeQueue_ModeRemove);
}

static void code_queue_add(LocalRoot local, addr code, addr value)
{
	addr stack;

	CheckTypeCodeQueue(code);
	Check(GetStatusDynamic(value), "dynamic error");
	GetCodeQueue(code, CodeQueue_Code, &stack);
	Check(stack == Nil, "stack error");
	push_code_stack(local, stack, value);
}

_g void code_queue_add2(LocalRoot local, addr code, addr left, addr right)
{
	cons_heap(&right, left, right);
	code_queue_add(local, code, right);
}

_g void code_queue_push(LocalRoot local, addr code, addr pos, ...)
{
	addr list;
	va_list args;

	va_start(args, pos);
	list_alloc_stdarg(NULL, &list, args);
	va_end(args);
	cons_heap(&list, pos, list);
	code_queue_add(local, code, list);
}

_g void code_queue_list(LocalRoot local, addr code, constindex index, ...)
{
	addr pos, list;
	va_list args;

	GetConstant(index, &pos);
	va_start(args, index);
	list_alloc_stdarg(NULL, &list, args);
	va_end(args);
	cons_heap(&list, pos, list);
	code_queue_add(local, code, list);
}

_g void code_queue_single(LocalRoot local, addr code, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	conscar_heap(&pos, pos);
	code_queue_add(local, code, pos);
}

_g void code_queue_cons(LocalRoot local, addr code, constindex index, addr right)
{
	addr pos;
	GetConstant(index, &pos);
	code_queue_add2(local, code, pos, right);
}

_g void code_queue_double(LocalRoot local,
		addr code, constindex index, addr left, addr right)
{
	addr first;
	GetConstant(index, &first);
	code_queue_push(local, code, first, left, right, NULL);
}

_g void code_queue_ifpush(LocalRoot local, addr code)
{
	if (code_queue_pushp(code))
		CodeQueue_single(local, code, PUSH_RESULT);
}


/*
 *  stack
 */
static struct code_stack *code_queue_push_struct(LocalRoot local, addr code)
{
	addr stack, one, pos;
	struct code_queue *ptr;

	CheckTypeCodeQueue(code);
	/* new stack */
	code_stack_local(local, &one);
	/* push */
	GetCodeQueue(code, CodeQueue_Code, &pos);
	GetCodeQueue(code, CodeQueue_Stack, &stack);
	cons_local(local, &stack, pos, stack);
	SetCodeQueue(code, CodeQueue_Stack, stack);
	SetCodeQueue(code, CodeQueue_Code, one);
	ptr = StructCodeQueue(code);
	ptr->size++;
	/* result */
	return StructCodeStack(one);
}

_g void code_queue_push_simple(LocalRoot local, addr code)
{
	(void)code_queue_push_struct(local, code);
}

_g void code_queue_push_new(LocalRoot local, addr code)
{
	struct code_stack *str;

	str = code_queue_push_struct(local, code);
	str->p_control = 1;
}

_g void code_queue_push_args(LocalRoot local, addr code)
{
	struct code_stack *str;

	str = code_queue_push_struct(local, code);
	str->p_control = 1;
	str->p_args = 1;
}

static int code_queue_pop_goto_p(addr pos)
{
	addr check;

	if (GetType(pos) != LISPTYPE_CONS)
		return 0;
	GetCar(pos, &pos);
	GetConst(CODE_GOTO, &check);
	if (pos == check)
		return 1;

	return 0;
}

static int code_queue_pop_tag_p(addr pos, addr *ret)
{
	addr check, left;

	if (GetType(pos) != LISPTYPE_CONS)
		return 0;
	GetCar(pos, &left);
	GetConst(CODE_TAG, &check);
	if (left != check)
		return 0;
	GetCdr(pos, ret);

	return 1;
}

static int code_queue_pop_label_p(addr pos)
{
	return GetType(pos) == LISPTYPE_INDEX;
}

static void code_queue_pop_label(LocalRoot local,
		addr body, addr *rlabel, size_t *rsize)
{
	addr label, pos, index;
	size_t size;

	label = Nil;
	size = 0;
	while (body != Nil) {
		GetCons(body, &pos, &body);
		/* label */
		if (code_queue_pop_label_p(pos)) {
			index_local(local, &index, size);
			cons_local(local, &pos, pos, index);
			cons_local(local, &label, pos, label);
			continue;
		}
		/* tag */
		if (code_queue_pop_tag_p(pos, &pos)) {
			setjump_tabletagbody(pos, size);
			continue;
		}
		else {
			size++;
		}
	}
	*rlabel = label;
	*rsize = size;
}

static size_t code_queue_pop_find(addr label, addr right)
{
	addr left;
	size_t index;

	GetIndex(right, &index);
	while (label != Nil) {
		GetCons(label, &left, &label);
		GetCons(left, &left, &right);
		if (RefIndex(left) == index)
			return RefIndex(right);
	}
	Abort("code_queue_pop_find error");

	return 0;
}

static void code_queue_pop_replace(addr label, addr pos, addr *ret)
{
	addr cdr;
	size_t value;

	GetCons(pos, &pos, &cdr);
	value = code_queue_pop_find(label, cdr);
	index_heap(&cdr, value);
	cons_heap(ret, pos, cdr);
}

static void code_queue_pop_goto(LocalRoot local,
		addr list, addr label, addr array, size_t size)
{
	addr pos;
	size_t i;

	/* code */
	for (i = 0; list != Nil; ) {
		GetCons(list, &pos, &list);
		if (code_queue_pop_label_p(pos))
			continue;
		if (code_queue_pop_tag_p(pos, &pos))
			continue;
		if (code_queue_pop_goto_p(pos))
			code_queue_pop_replace(label, pos, &pos);
		SetArrayA4(array, i++, pos);
	}
}

static void code_queue_pop_make(LocalRoot local, addr cons, addr *ret)
{
	addr label, array;
	size_t size;

	code_queue_pop_label(local, cons, &label, &size);
	vector4_heap(&array, size);
	code_queue_pop_goto(local, cons, label, array, size);
	*ret = array;
}

static void code_queue_pop_code(LocalRoot local, addr stack, addr *ret)
{
	addr pos;
	struct code_stack str;
	struct code_struct *code;

	/* free stack */
	str = *(StructCodeStack(stack));
	Check(! str.finish, "finish error");
	GetCodeStack(stack, CodeStack_Result, &pos);
	code_queue_pop_make(local, pos, &pos);
	free_code_stack(local, stack);

	/* make code */
	code_heap(&pos, pos);
	code = StructCode(pos);
	code->p_control = str.p_control;
	code->p_args = str.p_args;
	*ret = pos;
}

_g void code_queue_pop(LocalRoot local, addr code, addr *ret)
{
	addr pos, left, right;
	struct code_queue *ptr;

	CheckTypeCodeQueue(code);
	/* close stack */
	GetCodeQueue(code, CodeQueue_Code, &pos);
	finish_code_stack(local, pos);
	/* pop stack */
	GetCodeQueue(code, CodeQueue_Stack, &right);
	GetCons(right, &left, &right);
	SetCodeQueue(code, CodeQueue_Stack, right);
	SetCodeQueue(code, CodeQueue_Code, left);
	ptr = StructCodeQueue(code);
	ptr->size--;
	/* push operator */
	code_queue_pop_code(local, pos, ret);
}


/*
 *  code
 */
_g void code_make_execute_set(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	code_queue_setmode(code, &mode);
	code_make_execute(local, code, scope);
	code_queue_rollback(code, &mode);
}
_g void code_make_execute_push(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	code_queue_pushmode(code, &mode);
	code_make_execute(local, code, scope);
	code_queue_rollback(code, &mode);
}
_g void code_make_execute_rem(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	code_queue_remmode(code, &mode);
	code_make_execute(local, code, scope);
	code_queue_rollback(code, &mode);
}

_g void code_make_execute_simple(LocalRoot local, addr code, addr pos)
{
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, EXECUTE_SIMPLE_PUSH, pos);
	else
		CodeQueue_cons(local, code, EXECUTE_SIMPLE_SET, pos);
}

_g void code_make_execute_normal(LocalRoot local, addr code, addr pos)
{
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, EXECUTE_NORMAL_PUSH, pos);
	else
		CodeQueue_cons(local, code, EXECUTE_NORMAL_SET, pos);
}

_g void code_make_execute_control(LocalRoot local, addr code, addr pos)
{
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, EXECUTE_CONTROL_PUSH, pos);
	else
		CodeQueue_cons(local, code, EXECUTE_CONTROL_SET, pos);
}

_g void code_make_execute_switch(LocalRoot local, addr code, addr pos)
{
	if (code_queue_pushp(code))
		CodeQueue_cons(local, code, EXECUTE_SWITCH_PUSH, pos);
	else
		CodeQueue_cons(local, code, EXECUTE_SWITCH_SET, pos);
}

_g void code_make_single(LocalRoot local, addr code,
		constindex set, constindex push)
{
	CheckTypeCodeQueue(code);
	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			code_queue_single(local, code, set);
			break;

		case CodeQueue_ModePush:
			code_queue_single(local, code, push);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}

_g void code_make_object(LocalRoot local, addr code, addr value)
{
	CheckTypeCodeQueue(code);
	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(local, code, SET, value);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(local, code, PUSH, value);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}

