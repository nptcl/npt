#include "code.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control.h"
#include "define.h"
#include "eval.h"
#include "eval_code.h"
#include "eval_declare.h"
#include "eval_scope.h"
#include "eval_parse.h"
#include "eval_table.h"
#include "execute.h"
#include "function.h"
#include "heap.h"
#include "local.h"
#include "object.h"
#include "optimize_common.h"
#include "package.h"
#include "sequence.h"
#include "symbol.h"
#include "type_value.h"

/*
 *  evalcode
 */
enum EvalCode_Index {
	EvalCode_Code,
	EvalCode_Stack,
	EvalCode_Size
};

enum EvalCodeStack_Mode {
	EvalCodeStack_Root,
	EvalCodeStack_Result,
	EvalCodeStack_Size
};

struct evalcode {
	enum EvalCode_Mode mode;
	size_t size, label;
};

struct evalcode_stack {
	unsigned finish : 1;
	enum CodeType type;
	LocalStack stack;
	size_t size;
};

struct evalcode_switch {
	enum EvalCode_Mode mode;
};
typedef struct evalcode_switch modeswitch;

#define RefEvalCode				RefEval
#define GetEvalCode				GetEval
#define SetEvalCode				SetEval
#define PtrEvalCode(p)			PtrEvalBody(p, EvalCode_Size)
#define StructEvalCode(p)		((struct evalcode *)PtrEvalCode(p))
#define CheckTypeEvalCode(p)	Check(! eval_code_p(p), "type error")

#define RefEvalCodeStack		RefArraySS
#define GetEvalCodeStack		GetArraySS
#define SetEvalCodeStack		SetArraySS
#define PtrEvalCodeStack(x)		PtrBodySSa(x, EvalCodeStack_Size)
#define StructEvalCodeStack(p)	((struct evalcode_stack *)PtrEvalCodeStack(p))

#define ConstantCode(x,y,z) ((x)? CONSTANT_CODE_##y: CONSTANT_CODE_##z)
#define GetConstantCode(x,y,z,w) GetConstant(ConstantCode(x,y,z),(w))


/*
 *  evalcode-stack
 */
static void alloc_evalcode_stack(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret, LISPSYSTEM_EVALSTACK,
			EvalCodeStack_Size, sizeoft(struct evalcode_stack));
}

static void evalcode_stack_local(LocalRoot local, addr *ret, enum CodeType type)
{
	addr pos;
	struct evalcode_stack *ptr;
	LocalStack stack;

	Check(local == NULL, "local error");
	push_local(local, &stack);
	alloc_evalcode_stack(local, &pos);
	ptr = StructEvalCodeStack(pos);
	memset(ptr, 0, sizeoft(struct evalcode_stack));
	ptr->type = type;
	ptr->stack = stack;
	*ret = pos;
}

static void free_evalcode_stack(LocalRoot local, addr pos)
{
	rollback_local(local, StructEvalCodeStack(pos)->stack);
}

static void push_evalcode_stack(LocalRoot local, addr pos, addr value)
{
	addr root;
	struct evalcode_stack *ptr;

	ptr = StructEvalCodeStack(pos);
	Check(ptr->finish, "finish error");
	GetEvalCodeStack(pos, EvalCodeStack_Root, &root);
	cons_local(local, &root, value, root);
	SetEvalCodeStack(pos, EvalCodeStack_Root, root);
	ptr->size++;
}

static void finish_evalcode_stack(LocalRoot local, addr pos)
{
	addr root;
	struct evalcode_stack *ptr;

	ptr = StructEvalCodeStack(pos);
	Check(ptr->finish, "finish error");
	GetEvalCodeStack(pos, EvalCodeStack_Root, &root);
	nreverse_list_unsafe(&root, root);
	SetEvalCodeStack(pos, EvalCodeStack_Root, Nil);
	SetEvalCodeStack(pos, EvalCodeStack_Result, root);
	ptr->finish = 1;
}


/*
 *  evalcode
 */
static void alloc_evalcode(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_CODE, EvalCode_Size, sizeoft(struct evalcode));
}

static void evalcode_local(LocalRoot local, addr *ret)
{
	addr pos, stack;
	struct evalcode *ptr;

	Check(local == NULL, "local error");
	alloc_evalcode(local, &pos);
	/* array */
	evalcode_stack_local(local, &stack, CodeType_Default);
	SetEvalCode(pos, EvalCode_Code, stack);
	/* body */
	ptr = StructEvalCode(pos);
	memset(ptr, 0, sizeoft(struct evalcode));
	ptr->mode = EvalCode_ModeSet;
	/* result */
	*ret = pos;
}

_g enum EvalCode_Mode evalcode_mode(addr code)
{
	CheckTypeEvalCode(code);
	return StructEvalCode(code)->mode;
}

#ifdef LISP_DEGRADE
static int evalcode_setp(addr code)
{
	return evalcode_mode(code) == EvalCode_ModeSet;
}
#endif

static int evalcode_pushp(addr code)
{
	return evalcode_mode(code) == EvalCode_ModePush;
}

static int evalcode_remp(addr code)
{
	return evalcode_mode(code) == EvalCode_ModeRemove;
}

static void evalcode_save(addr code, modeswitch *mode)
{
	CheckTypeEvalCode(code);
	mode->mode = evalcode_mode(code);
}

static void evalcode_rollback(addr code, modeswitch *mode)
{
	CheckTypeEvalCode(code);
	StructEvalCode(code)->mode = mode->mode;
}

static void evalcode_savevalue(addr code, modeswitch *mode, enum EvalCode_Mode value)
{
	evalcode_save(code, mode);
	StructEvalCode(code)->mode = value;
}
static void evalcode_setmode(addr code, modeswitch *mode)
{
	evalcode_savevalue(code, mode, EvalCode_ModeSet);
}
static void evalcode_pushmode(addr code, modeswitch *mode)
{
	evalcode_savevalue(code, mode, EvalCode_ModePush);
}
static void evalcode_remmode(addr code, modeswitch *mode)
{
	evalcode_savevalue(code, mode, EvalCode_ModeRemove);
}

static void pushdata(LocalRoot local, addr code, addr value)
{
	addr stack;

	CheckTypeEvalCode(code);
	Check(GetStatusDynamic(value), "dynamic error");
	GetEvalCode(code, EvalCode_Code, &stack);
	Check(stack == Nil, "stack error");
	push_evalcode_stack(local, stack, value);
}

static void pushleftright(LocalRoot local, addr code, addr left, addr right)
{
	cons_heap(&right, left, right);
	pushdata(local, code, right);
}

static void pushlist_eval(LocalRoot local, addr code, addr pos, ...)
{
	addr cons;
	va_list args;

	va_start(args, pos);
	list_alloc_stdarg(NULL, &cons, args);
	va_end(args);
	cons_heap(&pos, pos, cons);
	pushdata(local, code, pos);
}

_g void evalcode_single(LocalRoot local, addr code, constindex index)
{
	addr pos;

	GetConstant(index, &pos);
	conscar_heap(&pos, pos);
	pushdata(local, code, pos);
}

_g void evalcode_carcdr(LocalRoot local, addr code, constindex index, addr right)
{
	addr pos;
	GetConstant(index, &pos);
	pushleftright(local, code, pos, right);
}

_g void evalcode_double(LocalRoot local,
		addr code, constindex index, addr left, addr right)
{
	addr first;
	GetConstant(index, &first);
	pushlist_eval(local, code, first, left, right, NULL);
}

_g void evalcode_push3(LocalRoot local,
		addr code, constindex index, addr left, addr right, addr third)
{
	addr first;
	GetConstant(index, &first);
	pushlist_eval(local, code, first, left, right, third, NULL);
}

static void code_push6(LocalRoot local,
		addr code, constindex index,
		addr a, addr b, addr c, addr d, addr e, addr f)
{
	addr first;
	GetConstant(index, &first);
	pushlist_eval(local, code, first, a, b, c, d, e, f, NULL);
}

#ifdef LISP_DEGRADE
static void code_list(LocalRoot local, addr code, constindex index, ...)
{
	addr pos, cons;
	va_list args;

	GetConstant(index, &pos);
	va_start(args, index);
	list_alloc_stdarg(NULL, &cons, args);
	va_end(args);
	cons_heap(&pos, pos, cons);
	pushdata(local, code, pos);
}
#endif

static void if_push_result(LocalRoot local, addr code)
{
	if (evalcode_pushp(code))
		EvalCode_single(local, code, PUSH_RESULT);
}


/*
 *  stack
 */
static void pushstack_type(LocalRoot local, addr code, enum CodeType type)
{
	addr stack, one, pos;
	struct evalcode *ptr;

	CheckTypeEvalCode(code);
	/* new stack */
	evalcode_stack_local(local, &one, type);
	/* push */
	GetEvalCode(code, EvalCode_Code, &pos);
	GetEvalCode(code, EvalCode_Stack, &stack);
	cons_local(local, &stack, pos, stack);
	SetEvalCode(code, EvalCode_Stack, stack);
	SetEvalCode(code, EvalCode_Code, one);
	ptr = StructEvalCode(code);
	ptr->size++;
}

static void pushstack(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_Default);
}
static void pushstack_return(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_Return);
}
static void pushstack_argument(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_Argument);
}
static void pushstack_push(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_Push);
}
static void pushstack_remove(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_Remove);
}
static void pushstack_close(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_Close);
}
static void pushstack_protect(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_Protect);
}
static void pushstack_tagbody(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_TagBody);
}
static void pushstack_block(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_Block);
}
static void pushstack_catch(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_Catch);
}
static void pushstack_condition(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_Condition);
}
static void pushstack_restart(LocalRoot local, addr code)
{
	pushstack_type(local, code, CodeType_Restart);
}

static void pushstack_mode(LocalRoot local, addr code)
{
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			pushstack_return(local, code);
			break;

		case EvalCode_ModePush:
			pushstack_push(local, code);
			break;

		case EvalCode_ModeRemove:
		default:
			pushstack_remove(local, code);
			break;
	}
}

static int stack_goto_p(addr pos)
{
	addr check;

	if (GetType(pos) != LISPTYPE_CONS) return 0;
	GetCar(pos, &pos);
	GetConst(CODE_GOTO, &check);
	if (pos == check) return 1;
	GetConst(CODE_IF_NIL, &check);
	if (pos == check) return 1;
	GetConst(CODE_IF_T, &check);
	if (pos == check) return 1;

	return 0;
}

static int stack_tag_p(addr pos, addr *ret)
{
	addr check, left;

	if (GetType(pos) != LISPTYPE_CONS) return 0;
	GetCar(pos, &left);
	GetConst(CODE_TAG, &check);
	if (left != check) return 0;
	GetCdr(pos, ret);

	return 1;
}

static int stack_label_p(addr pos)
{
	return GetType(pos) == LISPTYPE_INDEX;
}

static void stack_labelcons(LocalRoot local,
		addr right, addr *retlabel, addr *rettag, size_t *retsize)
{
	addr label, tag, left, index;
	size_t size;

	label = tag = Nil;
	size = 0;
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (stack_label_p(left)) {
			/* local */
			index_local(local, &index, size);
			cons_local(local, &left, left, index);
			cons_local(local, &label, left, label);
		}
		else if (stack_tag_p(left, &left)) {
			/* heap */
			index_heap(&index, size);
			cons_heap(&left, left, index);
			cons_heap(&tag, left, tag);
		}
		else {
			size++;
		}
	}
	*retlabel = label;
	*rettag = tag;
	*retsize = size;
}

static size_t stack_findlabel(addr label, addr right)
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
	Abort("stack_findlabel error");

	return 0;
}

static void stack_replace_goto(addr label, addr pos, addr *ret)
{
	addr cdr;
	size_t value;

	GetCons(pos, &pos, &cdr);
	value = stack_findlabel(label, cdr);
	index_heap(&cdr, value);
	cons_heap(ret, pos, cdr);
}

static void stack_makecode(LocalRoot local, addr cons, addr *retvect, addr *rettag)
{
	addr pos, label, tag, array;
	size_t size, i;

	stack_labelcons(local, cons, &label, &tag, &size);
	vector4_heap(&array, size);
	for (i = 0; cons != Nil; ) {
		GetCons(cons, &pos, &cons);
		if (stack_label_p(pos) || stack_tag_p(pos, &pos)) {
			continue;
		}
		else if (stack_goto_p(pos)) {
			stack_replace_goto(label, pos, &pos);
		}
		SetArrayA4(array, i++, pos);
	}
	*retvect = array;
	*rettag = tag;
}

static void popstack_code(LocalRoot local, addr stack, addr tag, addr *ret)
{
	addr pos;
	enum CodeType type;
	struct evalcode_stack *ptr;

	/* free stack */
	ptr = StructEvalCodeStack(stack);
	Check(! ptr->finish, "finish error");
	type = ptr->type;
	GetEvalCodeStack(stack, EvalCodeStack_Result, &pos);
	stack_makecode(local, pos, &pos, &tag);
	free_evalcode_stack(local, stack);

	/* make code */
	code_heap(&pos, pos);
	settype_code(pos, type);
	if (tag != Nil)
		setinfo_code(pos, tag);
	*ret = pos;
}

static void popstack_args(LocalRoot local, addr code, addr tag, addr *ret)
{
	addr pos, left, right;
	struct evalcode *ptr;

	CheckTypeEvalCode(code);
	/* close stack */
	evalcode_single(local, code, CONSTANT_CODE_END);
	GetEvalCode(code, EvalCode_Code, &pos);
	finish_evalcode_stack(local, pos);
	/* pop stack */
	GetEvalCode(code, EvalCode_Stack, &right);
	GetCons(right, &left, &right);
	SetEvalCode(code, EvalCode_Stack, right);
	SetEvalCode(code, EvalCode_Code, left);
	ptr = StructEvalCode(code);
	ptr->size--;
	/* push operator */
	popstack_code(local, pos, tag, ret);
}
static void popstack(LocalRoot local, addr code, addr *ret)
{
	popstack_args(local, code, Nil, ret);
}
static void popstack_tagbody(LocalRoot local, addr code, addr tag, addr *ret)
{
	popstack_args(local, code, tag, ret);
}

static void popstack_block(LocalRoot local, addr code, addr name, addr *ret)
{
	addr pos;

	popstack(local, code, &pos);
	setinfo_code(pos, name);
	*ret = pos;
}


/*
 *  label
 */
static void make_label(LocalRoot local, addr code, addr *ret)
{
	struct evalcode *ptr;

	ptr = StructEvalCode(code);
	index_heap(ret, ptr->label++);
}

static void push_label(LocalRoot local, addr code, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	pushdata(local, code, label);
}

static void code_if_nil(LocalRoot local, addr code, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	EvalCode_carcdr(local, code, IF_NIL, label);
}

static void code_if_t(LocalRoot local, addr code, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	EvalCode_carcdr(local, code, IF_T, label);
}

static void code_goto(LocalRoot local, addr code, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	EvalCode_carcdr(local, code, GOTO, label);
}


/*
 *  code
 */
_g void eval_code_execute_set(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	evalcode_setmode(code, &mode);
	eval_code_execute(local, code, scope);
	evalcode_rollback(code, &mode);
}
_g void eval_code_execute_push(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	evalcode_pushmode(code, &mode);
	eval_code_execute(local, code, scope);
	evalcode_rollback(code, &mode);
}
_g void eval_code_execute_rem(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	evalcode_remmode(code, &mode);
	eval_code_execute(local, code, scope);
	evalcode_rollback(code, &mode);
}

_g void evalcode_single_sp(LocalRoot local, addr code,
		constindex set, constindex push)
{
	CheckTypeEvalCode(code);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			evalcode_single(local, code, set);
			break;

		case EvalCode_ModePush:
			evalcode_single(local, code, push);
			break;

		case EvalCode_ModeRemove:
		default:
			break;
	}
}

_g void evalcode_carcdr_sp(LocalRoot local, addr code,
		constindex set, constindex push, addr value)
{
	CheckTypeEvalCode(code);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			evalcode_carcdr(local, code, set, value);
			break;

		case EvalCode_ModePush:
			evalcode_carcdr(local, code, push, value);
			break;

		case EvalCode_ModeRemove:
		default:
			break;
	}
}

_g void eval_code_object(LocalRoot local, addr code, addr value)
{
	CheckTypeEvalCode(code);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			EvalCode_carcdr(local, code, SET, value);
			break;

		case EvalCode_ModePush:
			EvalCode_carcdr(local, code, PUSH, value);
			break;

		case EvalCode_ModeRemove:
		default:
			break;
	}
}

static void eval_code_nil(LocalRoot local, addr code, addr ignore)
{
	evalcode_single_sp(local, code, CONSTANT_CODE_NIL_SET, CONSTANT_CODE_NIL_PUSH);
}

static void eval_code_t(LocalRoot local, addr code, addr ignore)
{
	evalcode_single_sp(local, code, CONSTANT_CODE_T_SET, CONSTANT_CODE_T_PUSH);
}

static void eval_code_value(LocalRoot local, addr code, addr scope)
{
	CheckTypeEvalCode(code);
	GetEvalScopeValue(scope, &scope);
	eval_code_object(local, code, scope);
}

static void code_declaim_special(LocalRoot local, addr code, addr cons)
{
	addr pos;

	getall_special_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		EvalCode_carcdr(local, code, DECLAIM_SPECIAL, pos);
	}
}

static void code_declaim_type_value(LocalRoot local, addr code, addr cons)
{
	addr key, value;

	getall_type_value_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		EvalCode_double(local, code, DECLAIM_TYPE_VALUE, key, value);
	}
}

static void code_declaim_type_function(LocalRoot local, addr code, addr cons)
{
	addr key, value;

	getall_type_function_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		EvalCode_double(local, code, DECLAIM_TYPE_FUNCTION, key, value);
	}
}

static void code_declaim_inline(LocalRoot local, addr code, addr cons)
{
	addr key, value, check1, check2;

	getall_inline_declare(cons, &cons);
	GetConst(COMMON_INLINE, &check1);
	GetConst(COMMON_NOTINLINE, &check2);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		if (value == check1)
			EvalCode_carcdr(local, code, DECLAIM_INLINE, key);
		if (value == check2)
			EvalCode_carcdr(local, code, DECLAIM_NOTINLINE, key);
	}
}

static void code_declaim_optimize(LocalRoot local, addr code, addr declare)
{
	addr pos;
	OptimizeType optimize;

	/* compilation-speed */
	optimize = get_optimize_compilation_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		EvalCode_carcdr(local, code, DECLAIM_COMPILATION, pos);
	}
	/* debug */
	optimize = get_optimize_debug_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		EvalCode_carcdr(local, code, DECLAIM_DEBUG, pos);
	}
	/* safety */
	optimize = get_optimize_safety_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		EvalCode_carcdr(local, code, DECLAIM_SAFETY, pos);
	}
	/* space */
	optimize = get_optimize_space_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		EvalCode_carcdr(local, code, DECLAIM_SPACE, pos);
	}
	/* speed */
	optimize = get_optimize_speed_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		EvalCode_carcdr(local, code, DECLAIM_SPEED, pos);
	}
}

static void code_declaim_declaration(LocalRoot local, addr code, addr cons)
{
	addr pos;

	getall_declaration_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		EvalCode_carcdr(local, code, DECLAIM_DECLARATION, pos);
	}
}

static void eval_code_declaim(LocalRoot local, addr code, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	code_declaim_special(local, code, scope);
	code_declaim_type_value(local, code, scope);
	code_declaim_type_function(local, code, scope);
	code_declaim_inline(local, code, scope);
	code_declaim_optimize(local, code, scope);
	code_declaim_declaration(local, code, scope);
	eval_code_nil(local, code, NULL);
}

static void code_symbol_set(LocalRoot local, addr code, addr symbol, addr table)
{
	int check, specialp;
	addr first, type;

	check = getcheck_tablevalue(table);
	specialp = getspecialp_tablevalue(table);
	if (check) {
		GetConstantCode(specialp, SPECIAL_SET_TYPE, LEXICAL_SET_TYPE, &first);
		gettype_tablevalue(table, &type);
		pushlist_eval(local, code, first, symbol, type, NULL);
	}
	else {
		GetConstantCode(specialp, SPECIAL_SET, LEXICAL_SET, &first);
		pushleftright(local, code, first, symbol);
	}
}

static void code_symbol_push(LocalRoot local, addr code, addr symbol, addr table)
{
	int check, specialp;
	addr first, type;

	check = getcheck_tablevalue(table);
	specialp = getspecialp_tablevalue(table);
	if (check) {
		GetConstantCode(specialp, SPECIAL_PUSH_TYPE, LEXICAL_PUSH_TYPE, &first);
		gettype_tablevalue(table, &type);
		pushlist_eval(local, code, first, symbol, type, NULL);
	}
	else {
		GetConstantCode(specialp, SPECIAL_PUSH, LEXICAL_PUSH, &first);
		pushleftright(local, code, first, symbol);
	}
}

static void code_symbol_remove(LocalRoot local, addr code, addr symbol, addr table)
{
	int check, specialp;
	addr first, type;

	check = getcheck_tablevalue(table);
	specialp = getspecialp_tablevalue(table);
	if (check) {
		GetConstantCode(specialp, SPECIAL_TYPE, LEXICAL_TYPE, &first);
		gettype_tablevalue(table, &type);
		pushlist_eval(local, code, first, symbol, type, NULL);
	}
	else {
		GetConstantCode(specialp, SPECIAL_REMOVE, LEXICAL_REMOVE, &first);
		pushleftright(local, code, first, symbol);
	}
}

static void eval_code_symbol(LocalRoot local, addr code, addr scope)
{
	addr symbol, table;

	/* keyword */
	GetEvalScopeValue(scope, &symbol);
	if (keywordp(symbol)) {
		eval_code_value(local, code, scope);
		return;
	}

	/* symbol */
	GetEvalScopeIndex(scope, 0, &table);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			code_symbol_set(local, code, symbol, table);
			break;

		case EvalCode_ModePush:
			code_symbol_push(local, code, symbol, table);
			break;

		case EvalCode_ModeRemove:
		default:
			code_symbol_remove(local, code, symbol, table);
			break;
	}
}

static void code_allcons(LocalRoot local, addr code, addr cons)
{
	addr pos;
	modeswitch mode;

	/* nil */
	if (cons == Nil) {
		eval_code_nil(local, code, NULL);
		return;
	}

	/* butlast */
	evalcode_remmode(code, &mode);
	for (;;) {
		GetCons(cons, &pos, &cons);
		if (cons == Nil) break;
		eval_code_execute(local, code, pos);
	}
	evalcode_rollback(code, &mode);

	/* last */
	eval_code_execute(local, code, pos);
}

static void code_allcons_set(LocalRoot local, addr code, addr cons)
{
	modeswitch mode;

	evalcode_setmode(code, &mode);
	code_allcons(local, code, cons);
	evalcode_rollback(code, &mode);
}

static void code_allcons_rem(LocalRoot local, addr code, addr cons)
{
	addr pos;

	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		eval_code_execute_rem(local, code, pos);
	}
}

static void eval_code_progn(LocalRoot local, addr code, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	code_allcons(local, code, scope);
}

static void code_check_value(LocalRoot local, addr code, addr pos, addr type)
{
	int specialp;
	addr first;

	specialp = getspecialp_tablevalue(pos);
	getname_tablevalue(pos, &pos);
	GetConstantCode(specialp, SPECIAL_TYPE, LEXICAL_TYPE, &first);
	pushlist_eval(local, code, first, pos, type, NULL);
}

static void code_check_function(LocalRoot local, addr code, addr pos, addr type)
{
	int globalp, symbolp;
	addr first;

	globalp = getglobalp_tablefunction(pos);
	getname_tablefunction(pos, &pos);
	symbolp = (RefCallNameType(pos) == CALLNAME_SYMBOL);
	if (globalp)
		GetConstantCode(symbolp, FUNCTION_GLOBAL_TYPE, SETF_GLOBAL_TYPE, &first);
	else
		GetConstantCode(symbolp, FUNCTION_LOCAL_TYPE, SETF_LOCAL_TYPE, &first);
	GetCallName(pos, &pos);
	pushlist_eval(local, code, first, pos, type, NULL);
}

static void code_free_declare(LocalRoot local, addr code, addr cons)
{
	addr pos, type;

	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		GetCons(pos, &pos, &type);
		if (eval_tablevalue_p(pos)) {
			code_check_value(local, code, pos, type);
		}
		else if (eval_tablefunction_p(pos)) {
			code_check_function(local, code, pos, type);
		}
		else {
			Abort("type error");
		}
	}
}

static void code_let_args(LocalRoot local, addr code, addr args)
{
	int specialp, check;
	addr cons, name, type, pos, first, index;
	size_t i, size;

	/* local allocate */
	size = length_list_unsafe(args);
	if (size == 0) return;
	index_heap(&pos, size);
	EvalCode_carcdr(local, code, LOCAL_ALLOC, pos);

	/* init -> local storage */
	cons = args;
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		GetCdr(pos, &pos);
		eval_code_execute_set(local, code, pos);
		index_heap(&index, i);
		EvalCode_carcdr(local, code, LOCAL_RESULT, index);
	}

	/* local storage -> var */
	cons = args;
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		GetCar(pos, &pos);
		index_heap(&index, i);
		specialp = getspecialp_tablevalue(pos);
		check = getcheck_tablevalue(pos);
		getname_tablevalue(pos, &name);

		if (check) {
			GetConstantCode(specialp, LET_SPECIAL_TYPE, LET_LEXICAL_TYPE, &first);
			gettype_tablevalue(pos, &type);
			pushlist_eval(local, code, first, name, index, type, NULL);
		}
		else {
			GetConstantCode(specialp, LET_SPECIAL, LET_LEXICAL, &first);
			pushlist_eval(local, code, first, name, index, NULL);
		}
	}
}

static void eval_code_let(LocalRoot local, addr code, addr scope)
{
	addr args, cons, free;

	/* make code */
	pushstack_mode(local, code);
	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &cons);
	GetEvalScopeIndex(scope, 3, &free);
	code_free_declare(local, code, free);
	code_let_args(local, code, args);
	code_allcons_set(local, code, cons);
	popstack(local, code, &cons);
	/* execute */
	EvalCode_carcdr(local, code, EXECUTE, cons);
}

static void code_leta_args(LocalRoot local, addr code, addr args)
{
	int specialp, check;
	addr pos, init, first, name, type;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCons(pos, &pos, &init);
		eval_code_execute_set(local, code, init);
		specialp = getspecialp_tablevalue(pos);
		check = getcheck_tablevalue(pos);
		getname_tablevalue(pos, &name);

		if (check) {
			GetConstantCode(specialp, LETA_SPECIAL_TYPE, LETA_LEXICAL_TYPE, &first);
			gettype_tablevalue(pos, &type);
			pushlist_eval(local, code, first, name, type, NULL);
		}
		else {
			GetConstantCode(specialp, LETA_SPECIAL, LETA_LEXICAL, &first);
			pushleftright(local, code, first, name);
		}
	}
}

static void eval_code_leta(LocalRoot local, addr code, addr scope)
{
	addr args, cons, free;

	/* make code */
	pushstack_mode(local, code);
	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &cons);
	GetEvalScopeIndex(scope, 3, &free);
	code_free_declare(local, code, free);
	code_leta_args(local, code, args);
	code_allcons_set(local, code, cons);
	popstack(local, code, &cons);
	/* execute */
	EvalCode_carcdr(local, code, EXECUTE, cons);
}

static void code_setq_execute(LocalRoot local, addr code, addr pos, addr form)
{
	int specialp, check;
	addr first, name, type;

	eval_code_execute_set(local, code, form);
	specialp = getspecialp_tablevalue(pos);
	check = getcheck_tablevalue(pos);
	getname_tablevalue(pos, &name);

	if (check) {
		GetConstantCode(specialp, SETQ_SPECIAL_TYPE, SETQ_LEXICAL_TYPE, &first);
		gettype_tablevalue(pos, &type);
		pushlist_eval(local, code, first, name, type, NULL);
	}
	else {
		GetConstantCode(specialp, SETQ_SPECIAL, SETQ_LEXICAL, &first);
		pushleftright(local, code, first, name);
	}
}

static void eval_code_setq(LocalRoot local, addr code, addr scope)
{
	addr cons, pos, form;

	/* nil */
	GetEvalScopeValue(scope, &cons);
	if (cons == Nil) {
		eval_code_nil(local, code, NULL);
		return;
	}

	/* setq */
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		GetCons(pos, &pos, &form);
		code_setq_execute(local, code, pos, form);
	}

	/* result */
	if_push_result(local, code);
}

static void code_function_object(LocalRoot local, addr code, addr pos)
{
	CheckTypeEvalCode(code);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			EvalCode_carcdr(local, code, SET, pos);
			break;

		case EvalCode_ModePush:
			EvalCode_carcdr(local, code, PUSH, pos);
			break;

		case EvalCode_ModeRemove:
		default:
			break;
	}
}

static void code_function_callname(LocalRoot local, addr code, addr pos)
{
	constindex index;
	int globalp, symbolp;

	globalp = getglobalp_tablefunction(pos);
	getname_tablefunction(pos, &pos);
	symbolp = (RefCallNameType(pos) == CALLNAME_SYMBOL);
	GetCallName(pos, &pos);

	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			index = globalp?
				ConstantCode(symbolp, FUNCTION_GLOBAL_SET, SETF_GLOBAL_SET):
				ConstantCode(symbolp, FUNCTION_LOCAL_SET, SETF_LOCAL_SET);
			break;

		case EvalCode_ModePush:
			index = globalp?
				ConstantCode(symbolp, FUNCTION_GLOBAL_PUSH, SETF_GLOBAL_PUSH):
				ConstantCode(symbolp, FUNCTION_LOCAL_PUSH, SETF_LOCAL_PUSH);
			break;

		case EvalCode_ModeRemove:
		default:
			return;
	}
	evalcode_carcdr(local, code, index, pos);
}

static void eval_code_function(LocalRoot local, addr code, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	if (functionp(scope))
		code_function_object(local, code, scope);
	else
		code_function_callname(local, code, scope);
}

static void ordinary_bind_opt(LocalRoot local, addr code, addr args, addr *ret)
{
	addr root, list, var, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		pushstack(local, code);
		eval_code_execute_set(local, code, init);
		popstack(local, code, &init);
		list_heap(&var, var, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void ordinary_bind_key(LocalRoot local, addr code, addr args, addr *ret)
{
	addr root, list, var, name, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &name, &init, &svar, NULL);
		pushstack(local, code);
		eval_code_execute_set(local, code, init);
		popstack(local, code, &init);
		list_heap(&var, var, name, init, svar, NULL);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void ordinary_bind_aux(LocalRoot local, addr code, addr args, addr *ret)
{
	addr root, list, var, init;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, NULL);
		pushstack(local, code);
		eval_code_execute_set(local, code, init);
		popstack(local, code, &init);
		list_heap(&var, var, init, NULL);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void ordinary_bind(LocalRoot local, addr code, addr args)
{
	addr var, opt, rest, key, allow, aux;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, NULL);
	ordinary_bind_opt(local, code, opt, &opt);
	ordinary_bind_key(local, code, key, &key);
	ordinary_bind_aux(local, code, aux, &aux);
	list_heap(&args, var, opt, rest, key, allow, aux, NULL);
	EvalCode_carcdr(local, code, LAMBDA_BIND, args);
}

static void code_lambda_code(LocalRoot local, addr code, addr scope, addr *ret)
{
	addr args, free, cons;
	modeswitch mode;

	/* set mode */
	evalcode_setmode(code, &mode);
	/* make code */
	GetEvalScopeIndex(scope, EvalLambda_Args, &args);
	GetEvalScopeIndex(scope, EvalLambda_Cons, &cons);
	GetEvalScopeIndex(scope, EvalLambda_Free, &free);
	pushstack_argument(local, code);
	code_free_declare(local, code, free);
	ordinary_bind(local, code, args);
	code_allcons_set(local, code, cons);
	popstack(local, code, ret);
	/* rollback */
	evalcode_rollback(code, &mode);
}

static void code_lambda_clos(LocalRoot local, addr code, addr clos)
{
	addr left, right;

	if (clos == Nil) return;
	/* value */
	GetArrayA2(clos, EvalClosure_Value, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		getname_tablevalue(left, &left);
		EvalCode_carcdr(local, code, LAMBDA_VALUE, left);
	}

	/* function */
	GetArrayA2(clos, EvalClosure_Function, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		getname_tablefunction(left, &left);
		EvalCode_carcdr(local, code, LAMBDA_FUNCTION, left);
	}

	/* tagbody */
	GetArrayA2(clos, EvalClosure_TagBody, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		gettag_tabletagbody(left, &left);
		EvalCode_carcdr(local, code, LAMBDA_TAGBODY, left);
	}

	/* block */
	GetArrayA2(clos, EvalClosure_Block, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		EvalCode_carcdr(local, code, LAMBDA_BLOCK, left);
	}
}

static void code_lambda_operator(LocalRoot local,
		addr code, addr name, addr scope, addr pos)
{
	addr doc, table, the, form, defun;

	GetEvalScopeIndex(scope, EvalLambda_Table, &table);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	GetEvalScopeIndex(scope, EvalLambda_The, &the);
	GetEvalScopeIndex(scope, EvalLambda_Form, &form);
	GetEvalScopeIndex(scope, EvalLambda_Defun, &defun);
	if ((name != Nil) && getreference_tablefunction(table)) {
		code_push6(local, code, CONSTANT_CODE_LAMBDA_SELF,
				name, pos, the, doc, form, defun);
	}
	else {
		code_push6(local, code, CONSTANT_CODE_LAMBDA,
				name, pos, the, doc, form, defun);
	}
}

static void code_lambda_function(LocalRoot local, addr code, addr name, addr scope)
{
	addr pos, clos;

	Check(evalcode_remp(code), "modeswitch error");
	/* function */
	code_lambda_code(local, code, scope, &pos);
	code_lambda_operator(local, code, name, scope, pos);
	/* closure */
	GetEvalScopeIndex(scope, EvalLambda_Clos, &clos);
	code_lambda_clos(local, code, clos);
	if_push_result(local, code);
}

static void eval_code_lambda(LocalRoot local, addr code, addr scope)
{
	if (! evalcode_remp(code))
		code_lambda_function(local, code, Nil, scope);
}

static void code_lambda_set(LocalRoot local, addr code, addr scope)
{
	addr call;
	modeswitch mode;

	evalcode_setmode(code, &mode);
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	code_lambda_function(local, code, call, scope);
	evalcode_rollback(code, &mode);
}

static void eval_code_defun(LocalRoot local, addr code, addr scope)
{
	code_lambda_set(local, code, scope);
	EvalCode_single(local, code, DEFUN);
	if_push_result(local, code);
}

static void code_macro_bind_args(LocalRoot, addr, addr, addr *);
static void code_macro_bind_var(LocalRoot local, addr code, addr args, addr *ret)
{
	addr root, var;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &var, &args);
		if (consp(var))
			code_macro_bind_args(local, code, var, &var);
		cons_heap(&root, var, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void code_macro_bind_args(LocalRoot local, addr code, addr args, addr *ret)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(args, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	code_macro_bind_var(local, code, var, &var);
	ordinary_bind_opt(local, code, opt, &opt);
	ordinary_bind_key(local, code, key, &key);
	ordinary_bind_aux(local, code, aux, &aux);
	list_heap(ret, var, opt, rest, key, allow, aux, whole, env, NULL);
}

static void code_macro_bind(LocalRoot local, addr code, addr args)
{
	code_macro_bind_args(local, code, args, &args);
	EvalCode_carcdr(local, code, MACRO_BIND, args);
}

static void code_macro_code(LocalRoot local, addr code, addr scope, addr *ret)
{
	addr args, free, cons;
	modeswitch mode;

	/* set mode */
	evalcode_setmode(code, &mode);
	/* make code */
	GetEvalScopeIndex(scope, EvalLambda_Args, &args);
	GetEvalScopeIndex(scope, EvalLambda_Cons, &cons);
	GetEvalScopeIndex(scope, EvalLambda_Free, &free);
	pushstack_argument(local, code);
	code_free_declare(local, code, free);
	code_macro_bind(local, code, args);
	code_allcons_set(local, code, cons);
	popstack(local, code, ret);
	/* rollback */
	evalcode_rollback(code, &mode);
}

static void code_macro_function(LocalRoot local, addr code, addr scope)
{
	addr pos, clos, doc;

	/* function */
	code_macro_code(local, code, scope, &pos);
	/* closure */
	GetEvalScopeIndex(scope, EvalLambda_Clos, &clos);
	code_lambda_clos(local, code, clos);
	/* make code */
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	evalcode_double(local, code, CONSTANT_CODE_MACRO_LAMBDA, pos, doc);
	if_push_result(local, code);
}

static void eval_code_macro_lambda(LocalRoot local, addr code, addr scope)
{
	code_macro_function(local, code, scope);
}

static void eval_code_defmacro(LocalRoot local, addr code, addr scope)
{
	addr name, lambda;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &lambda);
	EvalCode_double(local, code, DEFMACRO, name, lambda);
	if_push_result(local, code);
}

static void eval_code_deftype(LocalRoot local, addr code, addr scope)
{
	addr call, doc;

	code_macro_function(local, code, scope);
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	EvalCode_double(local, code, DEFTYPE, call, doc);
	if_push_result(local, code);
}

static void eval_code_define_compiler_macro(LocalRoot local, addr code, addr scope)
{
	addr call, doc;

	code_macro_function(local, code, scope);
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	EvalCode_double(local, code, DEFINE_COMPILER_MACRO, call, doc);
	if_push_result(local, code);
}

static void code_dbind_args(LocalRoot local, addr code, addr args)
{
	code_macro_bind_args(local, code, args, &args);
	EvalCode_carcdr(local, code, DESTRUCTURING_BIND, args);
}

static void code_dbind_code(LocalRoot local, addr code, addr scope, addr *ret)
{
	addr args, free, cons;
	modeswitch mode;

	/* set mode */
	evalcode_setmode(code, &mode);
	/* make code */
	GetEvalScopeIndex(scope, EvalLambda_Args, &args);
	GetEvalScopeIndex(scope, EvalLambda_Cons, &cons);
	GetEvalScopeIndex(scope, EvalLambda_Free, &free);
	pushstack_argument(local, code);
	code_free_declare(local, code, free);
	code_dbind_args(local, code, args);
	code_allcons_set(local, code, cons);
	popstack(local, code, ret);
	/* rollback */
	evalcode_rollback(code, &mode);
}

static void eval_code_destructuring_bind(LocalRoot local, addr code, addr scope)
{
	addr expr, args, pos;
	modeswitch mode;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &args);

	pushstack_return(local, code);

	evalcode_setmode(code, &mode);
	eval_code_execute(local, code, expr);
	EvalCode_single(local, code, PUSH_RESULT);
	code_dbind_code(local, code, args, &args);
	EvalCode_carcdr(local, code, EXECUTE, args);
	evalcode_rollback(code, &mode);
	if_push_result(local, code);

	popstack(local, code, &pos);
	EvalCode_carcdr(local, code, EXECUTE, pos);
	if_push_result(local, code);
}

static void eval_code_define_symbol_macro(LocalRoot local, addr code, addr scope)
{
	addr symbol, lambda, body;

	GetEvalScopeIndex(scope, 0, &symbol);
	GetEvalScopeIndex(scope, 1, &lambda);
	GetEvalScopeIndex(scope, 2, &body);
	EvalCode_push3(local, code, DEFINE_SYMBOL_MACRO, symbol, lambda, body);
	if_push_result(local, code);
}

static void code_flet_args(LocalRoot local, addr code, addr args)
{
	addr pos, cons, index;
	size_t size, i;

	/* local allocate */
	size = length_list_unsafe(args);
	if (size == 0) return;
	index_heap(&pos, size);
	EvalCode_carcdr(local, code, LOCAL_ALLOC, pos);

	/* init -> local storage */
	cons = args;
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		GetCdr(pos, &pos);
		code_lambda_set(local, code, pos);
		index_heap(&index, i);
		EvalCode_carcdr(local, code, LOCAL_RESULT, index);
	}

	/* local storage -> var */
	cons = args;
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		GetCar(pos, &pos);
		getname_tablefunction(pos, &pos);
		index_heap(&index, i);
		EvalCode_double(local, code, FLET, pos, index);
	}
}

static void eval_code_flet(LocalRoot local, addr code, addr scope)
{
	addr args, cons, free;

	/* make code */
	pushstack_mode(local, code);
	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &cons);
	GetEvalScopeIndex(scope, 3, &free);
	code_free_declare(local, code, free);
	code_flet_args(local, code, args);
	code_allcons_set(local, code, cons);
	popstack(local, code, &cons);
	/* execute */
	EvalCode_carcdr(local, code, EXECUTE, cons);
}

static void code_lables_args(LocalRoot local, addr code, addr args)
{
	addr pos, call;
	size_t i;

	for (i = 0; args != Nil; i++) {
		GetCons(args, &pos, &args);
		GetCons(pos, &call, &pos);
		getname_tablefunction(call, &call);
		code_lambda_set(local, code, pos);
		EvalCode_carcdr(local, code, LABELS, call);
	}
}

static void eval_code_labels(LocalRoot local, addr code, addr scope)
{
	addr args, cons, free;

	/* make code */
	pushstack_mode(local, code);
	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 2, &cons);
	GetEvalScopeIndex(scope, 3, &free);
	code_free_declare(local, code, free);
	code_lables_args(local, code, args);
	code_allcons_set(local, code, cons);
	popstack(local, code, &cons);
	/* execute */
	EvalCode_carcdr(local, code, EXECUTE, cons);
}

static void code_values_set(LocalRoot local, addr code, addr cons)
{
	addr pos;

	/* nil */
	if (cons == Nil) {
		EvalCode_single(local, code, VALUES_NIL);
		return;
	}

	/* list */
	pushstack_return(local, code);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		eval_code_execute_push(local, code, pos);
	}
	EvalCode_single(local, code, VALUES_SET);
	popstack(local, code, &pos);
	EvalCode_carcdr(local, code, EXECUTE, pos);
}

static void code_values_push(LocalRoot local, addr code, addr cons)
{
	addr pos;

	/* nil */
	if (cons == Nil) {
		eval_code_nil(local, code, NULL);
		return;
	}

	/* list */
	GetCons(cons, &pos, &cons);
	eval_code_execute_push(local, code, pos);
	code_allcons_rem(local, code, cons);
}

static void eval_code_values(LocalRoot local, addr code, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			code_values_set(local, code, scope);
			break;

		case EvalCode_ModePush:
			code_values_push(local, code, scope);
			break;

		case EvalCode_ModeRemove:
		default:
			code_allcons(local, code, scope);
			break;
	}
}

static void eval_code_the(LocalRoot local, addr code, addr scope)
{
	addr form, check;

	GetEvalScopeValue(scope, &form);
	GetEvalScopeIndex(scope, 0, &check);
	if (check == Nil) {
		eval_code_execute(local, code, form);
	}
	else {
		eval_code_execute_set(local, code, form);
		GetEvalScopeThe(scope, &form);
		EvalCode_carcdr(local, code, THE, form);
		if_push_result(local, code);
	}
}

static void eval_code_locally(LocalRoot local, addr code, addr scope)
{
	addr cons, free;

	GetEvalScopeIndex(scope, 1, &cons);
	GetEvalScopeIndex(scope, 2, &free);
	code_free_declare(local, code, free);
	code_allcons(local, code, cons);
}

static int code_if_not_p(addr scope)
{
	addr call, check1, check2;

	if (RefEvalScopeType(scope) != EVAL_PARSE_CALL) return 0;
	/* args */
	GetEvalScopeIndex(scope, 1, &call);
	if (! singlep(call)) return 0;
	/* (not x) or (null x) */
	GetEvalScopeIndex(scope, 0, &call);
	if (RefEvalScopeType(scope) != EVAL_PARSE_FUNCTION) return 0;
	GetEvalScopeValue(call, &call);
	getname_tablefunction(call, &call);
	if (! symbol_callname_p(call)) return 0;
	GetCallName(call, &call);
	GetConst(COMMON_NOT, &check1);
	GetConst(COMMON_NULL, &check2);

	return call == check1 || call == check2;
}

static void code_if_true(LocalRoot local, addr code, addr then, addr last)
{
	int check;
	addr label_else, label_end;

	check = (RefEvalScopeType(last) == EVAL_PARSE_NIL);
	/* (if expr then else) */
	make_label(local, code, &label_else);
	make_label(local, code, &label_end);
	code_if_nil(local, code, label_else);
	eval_code_execute(local, code, then);
	code_goto(local, code, label_end);
	push_label(local, code, label_else);
	if (check)
		eval_code_nil(local, code, NULL);
	else
		eval_code_execute(local, code, last);
	push_label(local, code, label_end);
}

static void code_if_false(LocalRoot local, addr code, addr then, addr last)
{
	int check;
	addr label_else, label_end;

	check = (RefEvalScopeType(last) == EVAL_PARSE_NIL);
	/* (if (not expr) then else) */
	make_label(local, code, &label_else);
	make_label(local, code, &label_end);
	code_if_t(local, code, label_else);
	eval_code_execute(local, code, then);
	code_goto(local, code, label_end);
	push_label(local, code, label_else);
	if (check)
		eval_code_nil(local, code, NULL);
	else
		eval_code_execute(local, code, last);
	push_label(local, code, label_end);
}

static void eval_code_if(LocalRoot local, addr code, addr scope)
{
	addr expr, then, last;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &then);
	GetEvalScopeIndex(scope, 2, &last);

	eval_code_execute_set(local, code, expr);
	if (code_if_not_p(expr))
		code_if_false(local, code, then, last);
	else
		code_if_true(local, code, then, last);
}

static void eval_code_unwind_protect(LocalRoot local, addr code, addr scope)
{
	addr form, cons, protect, cleanup;

	GetEvalScopeIndex(scope, 0, &form);
	GetEvalScopeIndex(scope, 1, &cons);
	/* protect */
	pushstack_protect(local, code);
	eval_code_execute_set(local, code, form);
	popstack(local, code, &protect);
	/* cleanup */
	pushstack_close(local, code);
	code_allcons_rem(local, code, cons);
	popstack(local, code, &cleanup);
	/* set code */
	setinfo_code(protect, cleanup);
	EvalCode_carcdr(local, code, EXECUTE, protect);
	if_push_result(local, code);
}

static void code_tagbody_cons(LocalRoot local, addr code, addr cons)
{
	addr pos;
	modeswitch mode;

	evalcode_remmode(code, &mode);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		if (RefEvalScopeType(pos) == EVAL_PARSE_TAG) {
			GetEvalScopeValue(pos, &pos);
			EvalCode_carcdr(local, code, TAG, pos);
		}
		else {
			eval_code_execute(local, code, pos);
		}
	}
	evalcode_rollback(code, &mode);
}

static void eval_code_tagbody(LocalRoot local, addr code, addr scope)
{
	addr tag, cons;

	GetEvalScopeIndex(scope, 0, &tag);
	GetEvalScopeIndex(scope, 1, &cons);
	/* body */
	pushstack_tagbody(local, code);
	code_tagbody_cons(local, code, cons);
	popstack_tagbody(local, code, tag, &cons);
	/* execute */
	EvalCode_carcdr(local, code, EXECUTE, cons);
	if_push_result(local, code);
}

static void eval_code_go(LocalRoot local, addr code, addr scope)
{
	addr pos;

	GetEvalScopeValue(scope, &pos);
	gettag_tabletagbody(pos, &pos);
	EvalCode_carcdr(local, code, GO, pos);
}

static void eval_code_block(LocalRoot local, addr code, addr scope)
{
	addr name, cons;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);
	pushstack_block(local, code);
	code_allcons_set(local, code, cons);
	popstack_block(local, code, name, &cons);
	EvalCode_carcdr(local, code, EXECUTE, cons);
	if_push_result(local, code);
}

static void eval_code_return_from(LocalRoot local, addr code, addr scope)
{
	addr name, form;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &form);
	eval_code_execute_set(local, code, form);
	EvalCode_carcdr(local, code, RETURN_FROM, name);
}

static void eval_code_catch(LocalRoot local, addr code, addr scope)
{
	addr name, cons;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);
	pushstack_catch(local, code);
	eval_code_execute_set(local, code, name);
	EvalCode_single(local, code, CATCH);
	code_allcons_set(local, code, cons);
	popstack(local, code, &cons);
	EvalCode_carcdr(local, code, EXECUTE, cons);
	if_push_result(local, code);
}

static void eval_code_throw(LocalRoot local, addr code, addr scope)
{
	addr name, form;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &form);
	pushstack_close(local, code);
	eval_code_execute_push(local, code, name);
	eval_code_execute_set(local, code, form);
	EvalCode_single(local, code, THROW);
	popstack(local, code, &form);
	EvalCode_carcdr(local, code, EXECUTE, form);
}

static void eval_code_multiple_value_bind(LocalRoot local, addr code, addr scope)
{
	addr args, expr, cons, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 1, &expr);
	GetEvalScopeIndex(scope, 4, &cons);
	GetEvalScopeIndex(scope, 5, &free);

	pushstack_mode(local, code);
	eval_code_execute_set(local, code, expr);
	code_free_declare(local, code, free);
	EvalCode_carcdr(local, code, MULTIPLE_VALUE_BIND, args);
	code_allcons_set(local, code, cons);
	popstack(local, code, &cons);
	EvalCode_carcdr(local, code, EXECUTE, cons);
}

static void eval_code_multiple_value_call(LocalRoot local, addr code, addr scope)
{
	addr call, args, pos;

	GetEvalScopeIndex(scope, 0, &call);
	GetEvalScopeIndex(scope, 1, &args);
	pushstack_return(local, code);
	/* call */
	eval_code_execute_push(local, code, call);
	/* args */
	while (args != Nil) {
		GetCons(args, &pos, &args);
		eval_code_execute_set(local, code, pos);
		EvalCode_single(local, code, PUSH_VALUES);
	}
	/* call */
	EvalCode_single(local, code, FUNCALL);
	popstack(local, code, &pos);
	EvalCode_carcdr(local, code, EXECUTE, pos);
	if_push_result(local, code);
}

static void eval_code_multiple_value_prog1(LocalRoot local, addr code, addr scope)
{
	addr first, cons, pos;

	GetEvalScopeIndex(scope, 0, &first);
	GetEvalScopeIndex(scope, 1, &cons);
	pushstack_return(local, code);
	/* first */
	eval_code_execute_set(local, code, first);
	/* cons */
	pushstack_close(local, code);
	code_allcons_rem(local, code, cons);
	popstack(local, code, &pos);
	EvalCode_carcdr(local, code, EXECUTE, pos);
	/* result */
	popstack(local, code, &pos);
	EvalCode_carcdr(local, code, EXECUTE, pos);
	if_push_result(local, code);
}

static void eval_code_nth_value(LocalRoot local, addr code, addr scope)
{
	addr nth, expr, pos;

	GetEvalScopeIndex(scope, 0, &nth);
	GetEvalScopeIndex(scope, 1, &expr);
	pushstack_return(local, code);
	eval_code_execute_push(local, code, nth);
	eval_code_execute_set(local, code, expr);
	EvalCode_single(local, code, NTH_VALUE);
	popstack(local, code, &pos);
	EvalCode_carcdr(local, code, EXECUTE, pos);
	if_push_result(local, code);
}

static void eval_code_progv(LocalRoot local, addr code, addr scope)
{
	addr symbols, values, body, pos;

	GetEvalScopeIndex(scope, 0, &symbols);
	GetEvalScopeIndex(scope, 1, &values);
	GetEvalScopeIndex(scope, 2, &body);
	pushstack_return(local, code);
	eval_code_execute_push(local, code, symbols);
	eval_code_execute_push(local, code, values);
	EvalCode_single(local, code, PROGV);
	code_allcons_set(local, code, body);
	popstack(local, code, &pos);
	EvalCode_carcdr(local, code, EXECUTE, pos);
	if_push_result(local, code);
}


/*
 *  call
 */
static int eval_code_specialize_common_p(addr call)
{
	addr common;

	if (RefEvalScopeType(call) != EVAL_PARSE_FUNCTION)
		return 0;
	GetEvalScopeValue(call, &call);
	getname_tablefunction(call, &call);
	if (RefCallNameType(call) != CALLNAME_SYMBOL)
		return 0;
	GetCallName(call, &call);
	GetPackageSymbol(call, &call);
	GetConst(PACKAGE_COMMON_LISP, &common);

	return call == common;
}

static int eval_code_specialize_symbol_p(addr call, constindex index)
{
	addr left, right;

	if (RefEvalScopeType(call) != EVAL_PARSE_FUNCTION) return 0;
	GetConstant(index, &left);
	GetEvalScopeValue(call, &call);
	getname_tablefunction(call, &call);
	GetCallName(call, &right);
	return RefCallNameType(call) == CALLNAME_SYMBOL && left == right;
}

static void eval_code_specialize_allcons(LocalRoot local, addr code, addr args)
{
	addr pos;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &pos);
		eval_code_execute_set(local, code, pos);
	}
}

static int eval_code_specialize_handler(LocalRoot local, addr code, addr scope)
{
	addr args;

	GetEvalScopeIndex(scope, 1, &args);
	pushstack_condition(local, code);
	eval_code_specialize_allcons(local, code, args);
	popstack(local, code, &args);
	EvalCode_carcdr(local, code, EXECUTE, args);
	if_push_result(local, code);

	return 1;
}

static int eval_code_specialize_restart(LocalRoot local, addr code, addr scope)
{
	addr args;

	GetEvalScopeIndex(scope, 1, &args);
	pushstack_restart(local, code);
	eval_code_specialize_allcons(local, code, args);
	popstack(local, code, &args);
	EvalCode_carcdr(local, code, EXECUTE, args);
	if_push_result(local, code);

	return 1;
}

static int eval_code_specialize_push_return(LocalRoot local, addr code, addr scope)
{
	addr args;

	GetEvalScopeIndex(scope, 1, &args);
	pushstack_return(local, code);
	eval_code_specialize_allcons(local, code, args);
	popstack(local, code, &args);
	EvalCode_carcdr(local, code, EXECUTE, args);
	if_push_result(local, code);

	return 1;
}

static int eval_code_specialize_type(
		LocalRoot local, addr code, addr scope, constindex type)
{
	addr args, pos;

	GetEvalScopeIndex(scope, 1, &args);
	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &pos);
		eval_code_execute_push(local, code, pos);
	}
	evalcode_single(local, code, type);

	return 1;
}

static int eval_code_specialize_handler_bind(LocalRoot local, addr code, addr scope)
{
	return eval_code_specialize_type(local, code, scope, CONSTANT_CODE_HANDLER_BIND);
}

static int eval_code_specialize_handler_case(LocalRoot local, addr code, addr scope)
{
	return eval_code_specialize_type(local, code, scope, CONSTANT_CODE_HANDLER_CASE);
}

static int eval_code_specialize_restart_bind(LocalRoot local, addr code, addr scope)
{
	return eval_code_specialize_type(local, code, scope, CONSTANT_CODE_RESTART_BIND);
}

static int eval_code_specialize_restart_case(LocalRoot local, addr code, addr scope)
{
	return eval_code_specialize_type(local, code, scope, CONSTANT_CODE_RESTART_CASE);
}

static int eval_code_specialize(LocalRoot local, addr code, addr scope)
{
	addr call;

	GetEvalScopeIndex(scope, 0, &call);

	/* common-lisp */
	if (eval_code_specialize_common_p(call))
		return optimize_common(local, code, scope);

	/* lisp-system::handler */
	if (eval_code_specialize_symbol_p(call, CONSTANT_SYSTEM_HANDLER))
		return eval_code_specialize_handler(local, code, scope);

	/* lisp-system::restart */
	if (eval_code_specialize_symbol_p(call, CONSTANT_SYSTEM_RESTART))
		return eval_code_specialize_restart(local, code, scope);

	/* lisp-system::push-return */
	if (eval_code_specialize_symbol_p(call, CONSTANT_SYSTEM_PUSH_RETURN))
		return eval_code_specialize_push_return(local, code, scope);

	/* lisp-system::handler-bind */
	if (eval_code_specialize_symbol_p(call, CONSTANT_SYSTEM_HANDLER_BIND))
		return eval_code_specialize_handler_bind(local, code, scope);

	/* lisp-system::handler-case */
	if (eval_code_specialize_symbol_p(call, CONSTANT_SYSTEM_HANDLER_CASE))
		return eval_code_specialize_handler_case(local, code, scope);

	/* lisp-system::restart-bind */
	if (eval_code_specialize_symbol_p(call, CONSTANT_SYSTEM_RESTART_BIND))
		return eval_code_specialize_restart_bind(local, code, scope);

	/* lisp-system::restart-case */
	if (eval_code_specialize_symbol_p(call, CONSTANT_SYSTEM_RESTART_CASE))
		return eval_code_specialize_restart_case(local, code, scope);

	/* lisp-system::optimize-check */
	if (eval_code_specialize_symbol_p(call, CONSTANT_SYSTEM_OPTIMIZE_CHECK))
		return optimize_check_code(local, code, scope);

	return 0;
}

static void eval_code_call(LocalRoot local, addr code, addr scope)
{
	addr call, args, pos, value;

	if (eval_code_specialize(local, code, scope))
		return;
	GetEvalScopeIndex(scope, 0, &call);
	GetEvalScopeIndex(scope, 1, &args);
	pushstack_return(local, code);
	/* args */
	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &value);
		eval_code_execute_push(local, code, value);
		if (getcheck_tablecall(pos)) {
			gettype_tablecall(pos, &value);
			EvalCode_carcdr(local, code, CALL_TYPE, value);
		}
	}
	/* function */
	eval_code_execute_set(local, code, call);
	/* call */
	EvalCode_single(local, code, CALL);
	popstack(local, code, &pos);
	EvalCode_carcdr(local, code, EXECUTE, pos);
	if_push_result(local, code);
}


/*
 *  code
 */
typedef void (*eval_code_calltype)(LocalRoot, addr, addr);
static eval_code_calltype EvalCodeTable[EVAL_PARSE_SIZE];

_g void eval_code_execute(LocalRoot local, addr code, addr scope)
{
	enum EVAL_PARSE type;
	eval_code_calltype call;

	Check(! eval_scope_p(scope), "type error");
	GetEvalScopeType(scope, &type);
	call = EvalCodeTable[type];
	if (call == NULL) {
		fmte("Invalid scope type.", NULL);
		return;
	}
	(*call)(local, code, scope);
}

_g void eval_code(LocalRoot local, addr *ret, addr scope)
{
	addr code;
	LocalStack stack;

	push_local(local, &stack);
	evalcode_local(local, &code);
	eval_code_execute_set(local, code, scope);
	popstack(local, code, ret);
	rollback_local(local, stack);
}

_g void init_eval_code(void)
{
	EvalCodeTable[EVAL_PARSE_NIL] = eval_code_nil;
	EvalCodeTable[EVAL_PARSE_T] = eval_code_t;
	EvalCodeTable[EVAL_PARSE_CLOS] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_INTEGER] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_RATIONAL] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_COMPLEX] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_CHARACTER] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_ARRAY] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_VECTOR] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_BITVECTOR] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_STRING] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_SYMBOL] = eval_code_symbol;
	EvalCodeTable[EVAL_PARSE_FLOAT] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_DECLAIM] = eval_code_declaim;
	EvalCodeTable[EVAL_PARSE_PATHNAME] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_ENVIRONMENT] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_PROGN] = eval_code_progn;
	EvalCodeTable[EVAL_PARSE_LET] = eval_code_let;
	EvalCodeTable[EVAL_PARSE_LETA] = eval_code_leta;
	EvalCodeTable[EVAL_PARSE_SETQ] = eval_code_setq;
	EvalCodeTable[EVAL_PARSE_DEFUN] = eval_code_defun;
	EvalCodeTable[EVAL_PARSE_DEFMACRO] = eval_code_defmacro;
	EvalCodeTable[EVAL_PARSE_MACRO_LAMBDA] = eval_code_macro_lambda;
	EvalCodeTable[EVAL_PARSE_DEFTYPE] = eval_code_deftype;
	EvalCodeTable[EVAL_PARSE_DEFINE_COMPILER_MACRO] = eval_code_define_compiler_macro;
	EvalCodeTable[EVAL_PARSE_DESTRUCTURING_BIND] = eval_code_destructuring_bind;
	EvalCodeTable[EVAL_PARSE_DEFINE_SYMBOL_MACRO] = eval_code_define_symbol_macro;
	EvalCodeTable[EVAL_PARSE_QUOTE] = eval_code_value;
	EvalCodeTable[EVAL_PARSE_FUNCTION] = eval_code_function;
	EvalCodeTable[EVAL_PARSE_LAMBDA] = eval_code_lambda;
	EvalCodeTable[EVAL_PARSE_IF] = eval_code_if;
	EvalCodeTable[EVAL_PARSE_UNWIND_PROTECT] = eval_code_unwind_protect;
	EvalCodeTable[EVAL_PARSE_TAGBODY] = eval_code_tagbody;
	EvalCodeTable[EVAL_PARSE_GO] = eval_code_go;
	EvalCodeTable[EVAL_PARSE_BLOCK] = eval_code_block;
	EvalCodeTable[EVAL_PARSE_RETURN_FROM] = eval_code_return_from;
	EvalCodeTable[EVAL_PARSE_CATCH] = eval_code_catch;
	EvalCodeTable[EVAL_PARSE_THROW] = eval_code_throw;
	EvalCodeTable[EVAL_PARSE_FLET] = eval_code_flet;
	EvalCodeTable[EVAL_PARSE_LABELS] = eval_code_labels;
	EvalCodeTable[EVAL_PARSE_THE] = eval_code_the;
	EvalCodeTable[EVAL_PARSE_VALUES] = eval_code_values;
	EvalCodeTable[EVAL_PARSE_LOCALLY] = eval_code_locally;
	EvalCodeTable[EVAL_PARSE_CALL] = eval_code_call;
	EvalCodeTable[EVAL_PARSE_MULTIPLE_VALUE_BIND] = eval_code_multiple_value_bind;
	EvalCodeTable[EVAL_PARSE_MULTIPLE_VALUE_CALL] = eval_code_multiple_value_call;
	EvalCodeTable[EVAL_PARSE_MULTIPLE_VALUE_PROG1] = eval_code_multiple_value_prog1;
	EvalCodeTable[EVAL_PARSE_NTH_VALUE] = eval_code_nth_value;
	EvalCodeTable[EVAL_PARSE_PROGV] = eval_code_progv;
}

