#include "code_object.h"
#include "cons.h"
#include "cons_list.h"
#include "eval_object.h"
#include "eval_table.h"
#include "make.h"
#include "make_queue.h"
#include "make_typedef.h"
#include "memory.h"

/*
 *  code_stack
 */
struct code_stack {
	unsigned finish : 1;
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
	nreverse(&root, root);
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

void code_queue_local(LocalRoot local, addr *ret)
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

enum CodeQueue_Mode code_queue_mode(CodeMake ptr)
{
	addr code;

	code = ptr->code;
	CheckTypeCodeQueue(code);
	return StructCodeQueue(code)->mode;
}

int code_queue_setp(CodeMake ptr)
{
	return code_queue_mode(ptr) == CodeQueue_ModeSet;
}

int code_queue_pushp(CodeMake ptr)
{
	return code_queue_mode(ptr) == CodeQueue_ModePush;
}

int code_queue_remp(CodeMake ptr)
{
	return code_queue_mode(ptr) == CodeQueue_ModeRemove;
}

static void code_queue_save(CodeMake ptr, modeswitch *mode)
{
	CheckTypeCodeQueue(ptr->code);
	mode->mode = code_queue_mode(ptr);
}

void code_queue_rollback(CodeMake ptr, modeswitch *mode)
{
	addr code;

	code = ptr->code;
	CheckTypeCodeQueue(code);
	StructCodeQueue(code)->mode = mode->mode;
}

static void code_queue_savevalue(CodeMake ptr,
		modeswitch *mode, enum CodeQueue_Mode value)
{
	addr code;

	code = ptr->code;
	code_queue_save(ptr, mode);
	StructCodeQueue(code)->mode = value;
}

void code_queue_setmode(CodeMake ptr, modeswitch *mode)
{
	code_queue_savevalue(ptr, mode, CodeQueue_ModeSet);
}
void code_queue_pushmode(CodeMake ptr, modeswitch *mode)
{
	code_queue_savevalue(ptr, mode, CodeQueue_ModePush);
}
void code_queue_remmode(CodeMake ptr, modeswitch *mode)
{
	code_queue_savevalue(ptr, mode, CodeQueue_ModeRemove);
}

static void code_queue_add(CodeMake ptr, addr value)
{
	addr code, stack;
	LocalRoot local;

	local = ptr->local;
	code = ptr->code;

	CheckTypeCodeQueue(code);
	Check(GetStatusDynamic(value), "dynamic error");
	GetCodeQueue(code, CodeQueue_Code, &stack);
	Check(stack == Nil, "stack error");
	push_code_stack(local, stack, value);
}

void code_queue_add2(CodeMake ptr, addr x, addr y)
{
	cons_heap(&y, x, y);
	code_queue_add(ptr, y);
}

void code_queue_push(CodeMake ptr, addr pos, ...)
{
	addr list;
	va_list args;

	va_start(args, pos);
	list_stdarg_alloc(NULL, &list, args);
	va_end(args);
	cons_heap(&list, pos, list);
	code_queue_add(ptr, list);
}

void code_queue_list(CodeMake ptr, constindex index, ...)
{
	addr pos, list;
	va_list args;

	GetConstant(index, &pos);
	va_start(args, index);
	list_stdarg_alloc(NULL, &list, args);
	va_end(args);
	cons_heap(&list, pos, list);
	code_queue_add(ptr, list);
}

void code_queue_single(CodeMake ptr, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	conscar_heap(&pos, pos);
	code_queue_add(ptr, pos);
}

void code_queue_cons(CodeMake ptr, constindex x, addr y)
{
	addr pos;
	GetConstant(x, &pos);
	code_queue_add2(ptr, pos, y);
}

void code_queue_double(CodeMake ptr, constindex x, addr y, addr z)
{
	addr pos;
	GetConstant(x, &pos);
	code_queue_push(ptr, pos, y, z, NULL);
}

void code_queue_index(CodeMake ptr, constindex x, size_t y)
{
	addr pos1, pos2;

	GetConstant(x, &pos1);
	index_heap(&pos2, y);
	code_queue_add2(ptr, pos1, pos2);
}

void code_queue_ifpush(CodeMake ptr)
{
	if (code_queue_pushp(ptr))
		CodeQueue_single(ptr, PUSH_RESULT);
}


/*
 *  stack
 */
static struct code_stack *code_queue_push_struct(CodeMake ptr)
{
	addr code, stack, one, pos;
	struct code_queue *queue;
	LocalRoot local;

	local = ptr->local;
	code = ptr->code;
	CheckTypeCodeQueue(ptr->code);
	/* new stack */
	code_stack_local(local, &one);
	/* push */
	GetCodeQueue(code, CodeQueue_Code, &pos);
	GetCodeQueue(code, CodeQueue_Stack, &stack);
	cons_local(local, &stack, pos, stack);
	SetCodeQueue(code, CodeQueue_Stack, stack);
	SetCodeQueue(code, CodeQueue_Code, one);
	queue = StructCodeQueue(code);
	queue->size++;
	/* result */
	return StructCodeStack(one);
}

void code_queue_push_code(CodeMake ptr)
{
	(void)code_queue_push_struct(ptr);
}

#define CodeQueueGoto(pos, x) { \
	addr __check; \
	GetConst(x, &__check); \
	if (pos == __check) { \
		return 1; \
	} \
}
static int code_queue_pop_goto_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_CONS)
		return 0;
	GetCar(pos, &pos);
	CodeQueueGoto(pos, CODE_ESCAPE);
	CodeQueueGoto(pos, CODE_ESCAPE_NOT);
	CodeQueueGoto(pos, CODE_REVERT_GOTO);
	CodeQueueGoto(pos, CODE_GOTO);
	CodeQueueGoto(pos, CODE_IF_UNBOUND);
	CodeQueueGoto(pos, CODE_IF_NIL);
	CodeQueueGoto(pos, CODE_IF_T);

	return 0;
}

static int code_queue_pop_lambda_cache_p(addr pos)
{
	if (GetType(pos) != LISPTYPE_CONS)
		return 0;
	GetCar(pos, &pos);
	CodeQueueGoto(pos, CODE_LAMBDA_CACHE);

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

static void code_queue_pop_lambda_cache_replace(addr label, addr list, addr *ret)
{
	addr pos, cdr, tail;
	size_t value;

	Lista_bind(list, &pos, &cdr, &tail, NULL);
	value = code_queue_pop_find(label, cdr);
	index_heap(&cdr, value);
	lista_heap(ret, pos, cdr, tail, NULL);
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
		else if (code_queue_pop_lambda_cache_p(pos))
			code_queue_pop_lambda_cache_replace(label, pos, &pos);
		SetArrayA4(array, i++, pos);
	}
}

static void code_queue_pop_tag(addr list, addr *ret)
{
	addr root, pos, name, jump, lexical;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		/* (name jump lexical) */
		CheckTableTagBody(pos);
		getname_tabletagbody(pos, &name);
		index_heap(&jump, getjump_tabletagbody(pos));
		index_heap(&lexical, getlexical_tabletagbody(pos));
		list_heap(&pos, name, jump, lexical, NULL);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);
}

static void code_queue_pop_block(addr pos, addr *ret)
{
	addr name, lexical;

	/* (name lexical) */
	CheckTableBlock(pos);
	getname_tableblock(pos, &name);
	index_heap(&lexical, getlexical_tableblock(pos));
	list_heap(ret, name, lexical, NULL);
}

static void code_queue_pop_info(addr array, size_t size)
{
	addr cons, car, cdr, key1, key2;
	size_t i;

	GetConst(CODE_TAGINFO, &key1);
	GetConst(CODE_BLOCKINFO, &key2);
	for (i = 0; i < size; i++) {
		GetArrayA4(array, i, &cons);
		GetCons(cons, &car, &cdr);
		if (car == key1) {
			code_queue_pop_tag(cdr, &cdr);
			SetCdr(cons, cdr);
			continue;
		}
		if (car == key2) {
			code_queue_pop_block(cdr, &cdr);
			SetCdr(cons, cdr);
			continue;
		}
	}
}

static int code_queue_pop_delete_p(addr list)
{
	addr x, y;
	size_t check1, check2;

	/* goto */
	if (! consp_getcons(list, &x, &list))
		return 0;
	if (! code_queue_pop_goto_p(x))
		return 0;
	GetCdr(x, &x);
	GetIndex(x, &check1);

	/* tag */
	if (! consp_getcar(list, &y))
		return 0;
	if (! indexp(y))
		return 0;
	GetIndex(y, &check2);

	return check1 == check2;
}

static void code_queue_pop_delete(addr list, addr *ret)
{
	addr list1, list2;

	*ret = list;
	list2 = Nil;
	while (list != Nil) {
		GetCdr(list, &list1);
		if (code_queue_pop_delete_p(list)) {
			if (list2 == Nil)
				*ret = list1;
			else
				SetCdr(list2, list1);
		}
		else {
			list2 = list;
		}
		list = list1;
	}
}

static void code_queue_pop_make(LocalRoot local, addr cons, addr *ret)
{
	addr label, array;
	size_t size;

	code_queue_pop_delete(cons, &cons);
	code_queue_pop_label(local, cons, &label, &size);
	vector4_heap(&array, size);
	code_queue_pop_goto(local, cons, label, array, size);
	code_queue_pop_info(array, size);
	*ret = array;
}

static void code_queue_pop_code(LocalRoot local, addr stack, addr *ret)
{
	addr pos;

	/* free stack */
	Check(StructCodeStack(stack)->finish == 0, "finish error");
	GetCodeStack(stack, CodeStack_Result, &pos);
	code_queue_pop_make(local, pos, &pos);
	free_code_stack(local, stack);

	/* make code */
	code_heap(&pos, pos);
	*ret = pos;
}

void code_queue_pop(CodeMake ptr, addr *ret)
{
	addr code, pos, left, right;
	struct code_queue *queue;
	LocalRoot local;

	local = ptr->local;
	code = ptr->code;

	CheckTypeCodeQueue(ptr->code);
	/* close stack */
	GetCodeQueue(code, CodeQueue_Code, &pos);
	finish_code_stack(local, pos);
	/* pop stack */
	GetCodeQueue(code, CodeQueue_Stack, &right);
	GetCons(right, &left, &right);
	SetCodeQueue(code, CodeQueue_Stack, right);
	SetCodeQueue(code, CodeQueue_Code, left);
	queue = StructCodeQueue(code);
	queue->size--;
	/* push operator */
	code_queue_pop_code(local, pos, ret);
}


/*
 *  code
 */
int code_make_execute_set_(CodeMake ptr, addr scope)
{
	modeswitch mode;

	code_queue_setmode(ptr, &mode);
	Return(code_make_execute_(ptr, scope));
	code_queue_rollback(ptr, &mode);

	return 0;
}

int code_make_execute_push_(CodeMake ptr, addr scope)
{
	modeswitch mode;

	code_queue_pushmode(ptr, &mode);
	Return(code_make_execute_(ptr, scope));
	code_queue_rollback(ptr, &mode);

	return 0;
}

int code_make_execute_rem_(CodeMake ptr, addr scope)
{
	modeswitch mode;

	code_queue_remmode(ptr, &mode);
	Return(code_make_execute_(ptr, scope));
	code_queue_rollback(ptr, &mode);

	return 0;
}

void code_make_single(CodeMake ptr, constindex set, constindex push)
{
	CheckTypeCodeQueue(ptr->code);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			code_queue_single(ptr, set);
			break;

		case CodeQueue_ModePush:
			code_queue_single(ptr, push);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}

void code_make_object(CodeMake ptr, addr value)
{
	CheckTypeCodeQueue(ptr->code);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(ptr, SET, value);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(ptr, PUSH, value);
			break;

		case CodeQueue_ModeRemove:
		default:
			break;
	}
}


/*
 *  label
 */
void code_queue_make_label(CodeMake ptr, addr *ret)
{
	struct code_queue *str = StructCodeQueue(ptr->code);
	index_heap(ret, str->label++);
}

void code_queue_push_label(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	code_queue_add(ptr, label);
}

void code_queue_if_unbound(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, IF_UNBOUND, label);
}

void code_queue_if_nil(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, IF_NIL, label);
}

void code_queue_if_t(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, IF_T, label);
}

void code_queue_goto(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, GOTO, label);
}


/*
 *  begin / end
 */
void code_escape_clear(CodeMake ptr)
{
	ptr->escape = 0;
}

void code_escape_wake(CodeMake ptr)
{
	ptr->escape = 1;
}

int code_escape_get(CodeMake ptr)
{
	return ptr->escape;
}

#ifdef LISP_DEBUG
static fixnum code_make_begin_index = 0;
#endif

void code_make_begin(CodeMake ptr, fixnum *ret)
{
#ifdef LISP_DEBUG
	addr pos;

	fixnum_heap(&pos, code_make_begin_index);
	CodeQueue_cons(ptr, BEGIN, pos);
	*ret = code_make_begin_index;
	code_make_begin_index++;
#else
	CodeQueue_single(ptr, BEGIN);
	*ret = 0;
#endif
}

void code_make_begin_call(CodeMake ptr, fixnum *ret)
{
#ifdef LISP_DEBUG
	addr pos;

	fixnum_heap(&pos, code_make_begin_index);
	CodeQueue_cons(ptr, BEGIN_CALL, pos);
	*ret = code_make_begin_index;
	code_make_begin_index++;
#else
	CodeQueue_single(ptr, BEGIN_CALL);
	*ret = 0;
#endif
}

void code_make_end(CodeMake ptr, fixnum value)
{
#ifdef LISP_DEBUG
	addr pos;
	fixnum_heap(&pos, value);
	CodeQueue_cons(ptr, END, pos);
#else
	CodeQueue_single(ptr, END);
#endif
}

void code_jump_escape(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, ESCAPE, label);
}

void code_jump_escape_not(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, ESCAPE_NOT, label);
}

void code_jump_escape_wake(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, ESCAPE, label);
	code_escape_wake(ptr);
}

void code_jump_escape_not_wake(CodeMake ptr, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	CodeQueue_cons(ptr, ESCAPE_NOT, label);
	code_escape_wake(ptr);
}

