#include "code.h"
#include "condition.h"
#include "cons.h"
#include "control.h"
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
#include "package.h"
#include "sequence.h"
#include "symbol.h"
#include "type_value.h"

static void code_execute(LocalRoot local, addr code, addr scope);

/*
 *  evalcode
 */
enum EvalCode_Index {
	EvalCode_Code,
	EvalCode_Stack,
	EvalCode_Result,
	EvalCode_Argument,
	EvalCode_Size
};

enum EvalCode_Mode {
	EvalCode_ModeSet,
	EvalCode_ModePush,
	EvalCode_ModeRemove,
	EvalCode_ModeSize
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
	alloc_smallsize(local, ret, LISPTYPE_SYSTEM,
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

static enum EvalCode_Mode evalcode_mode(addr code)
{
	CheckTypeEvalCode(code);
	return StructEvalCode(code)->mode;
}

#if LISP_DEGRADE
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

static void save_modeswitch(addr code, modeswitch *mode)
{
	CheckTypeEvalCode(code);
	mode->mode = evalcode_mode(code);
}

static void rollback_modeswitch(addr code, modeswitch *mode)
{
	CheckTypeEvalCode(code);
	StructEvalCode(code)->mode = mode->mode;
}

static void savevalue_modeswitch(addr code, modeswitch *mode, enum EvalCode_Mode value)
{
	save_modeswitch(code, mode);
	StructEvalCode(code)->mode = value;
}
static void set_modeswitch(addr code, modeswitch *mode)
{
	savevalue_modeswitch(code, mode, EvalCode_ModeSet);
}
static void push_modeswitch(addr code, modeswitch *mode)
{
	savevalue_modeswitch(code, mode, EvalCode_ModePush);
}
static void rem_modeswitch(addr code, modeswitch *mode)
{
	savevalue_modeswitch(code, mode, EvalCode_ModeRemove);
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

static void pushlist(LocalRoot local, addr code, addr pos, ...)
{
	addr cons;
	va_list args;

	va_start(args, pos);
	list_alloc_stdarg(NULL, &cons, args);
	va_end(args);
	cons_heap(&pos, pos, cons);
	pushdata(local, code, pos);
}

static void code_single(LocalRoot local, addr code, enum CONSTANT_INDEX index)
{
	addr pos;

	GetConstant(index, &pos);
	conscar_heap(&pos, pos);
	pushdata(local, code, pos);
}

static void code_leftright(LocalRoot local,
		addr code, enum CONSTANT_INDEX index, addr right)
{
	addr pos;
	GetConstant(index, &pos);
	pushleftright(local, code, pos, right);
}

static void code_double(LocalRoot local,
		addr code, enum CONSTANT_INDEX index, addr left, addr right)
{
	addr first;
	GetConstant(index, &first);
	pushlist(local, code, first, left, right, NULL);
}

static void code_push3(LocalRoot local,
		addr code, enum CONSTANT_INDEX index, addr left, addr right, addr third)
{
	addr first;
	GetConstant(index, &first);
	pushlist(local, code, first, left, right, third, NULL);
}

static void code_push5(LocalRoot local,
		addr code, enum CONSTANT_INDEX index,
		addr a, addr b, addr c, addr d, addr e)
{
	addr first;
	GetConstant(index, &first);
	pushlist(local, code, first, a, b, c, d, e, NULL);
}

#ifdef LISP_DEGRADE
static void code_list(LocalRoot local, addr code, enum CONSTANT_INDEX index, ...)
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

#define Code_single(a,b,c) code_single(a,b,CONSTANT_CODE_##c)
#define Code_leftright(a,b,c,d) code_leftright(a,b,CONSTANT_CODE_##c,d)
#define Code_double(a,b,c,d,e) code_double(a,b,CONSTANT_CODE_##c,d,e)
#define Code_push3(a,b,c,d,e,f) code_push3(a,b,CONSTANT_CODE_##c,d,e,f)

static void if_push_result(LocalRoot local, addr code)
{
	if (evalcode_pushp(code))
		Code_single(local, code, PUSH_RESULT);
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
	code_single(local, code, CONSTANT_CODE_END);
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
	Code_leftright(local, code, IF_NIL, label);
}

static void code_if_t(LocalRoot local, addr code, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	Code_leftright(local, code, IF_T, label);
}

static void code_goto(LocalRoot local, addr code, addr label)
{
	CheckType(label, LISPTYPE_INDEX);
	Code_leftright(local, code, GOTO, label);
}


/*
 *  code
 */
static void code_execute_set(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	set_modeswitch(code, &mode);
	code_execute(local, code, scope);
	rollback_modeswitch(code, &mode);
}
static void code_execute_push(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	push_modeswitch(code, &mode);
	code_execute(local, code, scope);
	rollback_modeswitch(code, &mode);
}
static void code_execute_rem(LocalRoot local, addr code, addr scope)
{
	modeswitch mode;

	rem_modeswitch(code, &mode);
	code_execute(local, code, scope);
	rollback_modeswitch(code, &mode);
}

static void code_nil(LocalRoot local, addr code)
{
	CheckTypeEvalCode(code);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			Code_single(local, code, NIL_SET);
			break;

		case EvalCode_ModePush:
			Code_single(local, code, NIL_PUSH);
			break;

		case EvalCode_ModeRemove:
		default:
			break;
	}
}

static void code_t(LocalRoot local, addr code)
{
	CheckTypeEvalCode(code);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			Code_single(local, code, T_SET);
			break;

		case EvalCode_ModePush:
			Code_single(local, code, T_PUSH);
			break;

		case EvalCode_ModeRemove:
		default:
			break;
	}
}

static void code_value(LocalRoot local, addr code, addr scope)
{
	CheckTypeEvalCode(code);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			GetEvalScopeValue(scope, &scope);
			Code_leftright(local, code, SET, scope);
			break;

		case EvalCode_ModePush:
			GetEvalScopeValue(scope, &scope);
			Code_leftright(local, code, PUSH, scope);
			break;

		case EvalCode_ModeRemove:
		default:
			break;
	}
}

static void code_declaim_special(LocalRoot local, addr code, addr cons)
{
	addr pos;

	getall_special_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		Code_leftright(local, code, DECLAIM_SPECIAL, pos);
	}
}

static void code_declaim_type_value(LocalRoot local, addr code, addr cons)
{
	addr key, value;

	getall_type_value_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		Code_double(local, code, DECLAIM_TYPE_VALUE, key, value);
	}
}

static void code_declaim_type_function(LocalRoot local, addr code, addr cons)
{
	addr key, value;

	getall_type_function_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &key, &cons);
		GetCons(cons, &value, &cons);
		Code_double(local, code, DECLAIM_TYPE_FUNCTION, key, value);
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
			Code_leftright(local, code, DECLAIM_INLINE, key);
		if (value == check2)
			Code_leftright(local, code, DECLAIM_NOTINLINE, key);
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
		Code_leftright(local, code, DECLAIM_COMPILATION, pos);
	}
	/* debug */
	optimize = get_optimize_debug_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		Code_leftright(local, code, DECLAIM_DEBUG, pos);
	}
	/* safety */
	optimize = get_optimize_safety_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		Code_leftright(local, code, DECLAIM_SAFETY, pos);
	}
	/* space */
	optimize = get_optimize_space_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		Code_leftright(local, code, DECLAIM_SPACE, pos);
	}
	/* speed */
	optimize = get_optimize_speed_declare(declare);
	if (0 <= optimize) {
		fixnum_heap(&pos, (fixnum)optimize);
		Code_leftright(local, code, DECLAIM_SPEED, pos);
	}
}

static void code_declaim_declaration(LocalRoot local, addr code, addr cons)
{
	addr pos;

	getall_declaration_declare(cons, &cons);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		Code_leftright(local, code, DECLAIM_DECLARATION, pos);
	}
}

static void code_declaim(LocalRoot local, addr code, addr scope)
{
	GetEvalScopeValue(scope, &scope);
	code_declaim_special(local, code, scope);
	code_declaim_type_value(local, code, scope);
	code_declaim_type_function(local, code, scope);
	code_declaim_inline(local, code, scope);
	code_declaim_optimize(local, code, scope);
	code_declaim_declaration(local, code, scope);
	code_nil(local, code);
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
		pushlist(local, code, first, symbol, type, NULL);
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
		pushlist(local, code, first, symbol, type, NULL);
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
		pushlist(local, code, first, symbol, type, NULL);
	}
}

static void code_symbol(LocalRoot local, addr code, addr scope)
{
	addr symbol, table;

	/* keyword */
	GetEvalScopeValue(scope, &symbol);
	if (keywordp(symbol)) {
		code_value(local, code, scope);
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
		code_nil(local, code);
		return;
	}

	/* butlast */
	rem_modeswitch(code, &mode);
	for (;;) {
		GetCons(cons, &pos, &cons);
		if (cons == Nil) break;
		code_execute(local, code, pos);
	}
	rollback_modeswitch(code, &mode);

	/* last */
	code_execute(local, code, pos);
}

static void code_allcons_set(LocalRoot local, addr code, addr cons)
{
	modeswitch mode;

	set_modeswitch(code, &mode);
	code_allcons(local, code, cons);
	rollback_modeswitch(code, &mode);
}

static void code_allcons_rem(LocalRoot local, addr code, addr cons)
{
	addr pos;

	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		code_execute_rem(local, code, pos);
	}
}

static void code_progn(LocalRoot local, addr code, addr scope)
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
	pushlist(local, code, first, pos, type, NULL);
}

static void code_check_function(LocalRoot local, addr code, addr pos, addr type)
{
	int globalp, symbolp;
	addr first;

	globalp = getglobalp_tablefunction(pos);
	GetCallName(pos, &pos);
	symbolp = (RefCallNameType(pos) == CALLNAME_SYMBOL);
	if (globalp)
		GetConstantCode(symbolp, FUNCTION_GLOBAL_TYPE, SETF_GLOBAL_TYPE, &first);
	else
		GetConstantCode(symbolp, FUNCTION_LOCAL_TYPE, SETF_LOCAL_TYPE, &first);
	pushlist(local, code, first, pos, type, NULL);
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
	Code_leftright(local, code, LOCAL_ALLOC, pos);

	/* init -> local storage */
	cons = args;
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		GetCdr(pos, &pos);
		code_execute_set(local, code, pos);
		index_heap(&index, i);
		Code_leftright(local, code, LOCAL_RESULT, index);
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
			pushlist(local, code, first, name, index, type, NULL);
		}
		else {
			GetConstantCode(specialp, LET_SPECIAL, LET_LEXICAL, &first);
			pushlist(local, code, first, name, index, NULL);
		}
	}
}

static void code_let(LocalRoot local, addr code, addr scope)
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
	Code_leftright(local, code, EXECUTE, cons);
}

static void code_leta_args(LocalRoot local, addr code, addr args)
{
	int specialp, check;
	addr pos, init, first, name, type;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCons(pos, &pos, &init);
		code_execute_set(local, code, init);
		specialp = getspecialp_tablevalue(pos);
		check = getcheck_tablevalue(pos);
		getname_tablevalue(pos, &name);

		if (check) {
			GetConstantCode(specialp, LETA_SPECIAL_TYPE, LETA_LEXICAL_TYPE, &first);
			gettype_tablevalue(pos, &type);
			pushlist(local, code, first, name, type, NULL);
		}
		else {
			GetConstantCode(specialp, LETA_SPECIAL, LETA_LEXICAL, &first);
			pushleftright(local, code, first, name);
		}
	}
}

static void code_leta(LocalRoot local, addr code, addr scope)
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
	Code_leftright(local, code, EXECUTE, cons);
}

static void code_setq_execute(LocalRoot local, addr code, addr pos, addr form)
{
	int specialp, check;
	addr first, name, type;

	code_execute_set(local, code, form);
	specialp = getspecialp_tablevalue(pos);
	check = getcheck_tablevalue(pos);
	getname_tablevalue(pos, &name);

	if (check) {
		GetConstantCode(specialp, SETQ_SPECIAL_TYPE, SETQ_LEXICAL_TYPE, &first);
		gettype_tablevalue(pos, &type);
		pushlist(local, code, first, name, type, NULL);
	}
	else {
		GetConstantCode(specialp, SETQ_SPECIAL, SETQ_LEXICAL, &first);
		pushleftright(local, code, first, name);
	}
}

static void code_setq(LocalRoot local, addr code, addr scope)
{
	addr cons, pos, form;

	/* nil */
	GetEvalScopeValue(scope, &cons);
	if (cons == Nil) {
		code_nil(local, code);
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

static void code_function(LocalRoot local, addr code, addr scope)
{
	enum CONSTANT_INDEX index;
	int globalp, symbolp;
	addr pos;

	GetEvalScopeValue(scope, &pos);
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
	code_leftright(local, code, index, pos);
}

static void ordinary_bind_opt(LocalRoot local, addr code, addr args, addr *ret)
{
	addr root, list, var, init, svar;

	for (root = Nil; args != Nil; ) {
		GetCons(args, &list, &args);
		List_bind(list, &var, &init, &svar, NULL);
		pushstack(local, code);
		code_execute_set(local, code, init);
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
		code_execute_set(local, code, init);
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
		code_execute_set(local, code, init);
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
	Code_leftright(local, code, LAMBDA_BIND, args);
}

static void code_lambda_code(LocalRoot local, addr code, addr scope, addr *ret)
{
	addr args, free, cons;
	modeswitch mode;

	/* set mode */
	set_modeswitch(code, &mode);
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
	rollback_modeswitch(code, &mode);
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
		Code_leftright(local, code, LAMBDA_VALUE, left);
	}

	/* function */
	GetArrayA2(clos, EvalClosure_Function, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		getname_tablefunction(left, &left);
		Code_leftright(local, code, LAMBDA_FUNCTION, left);
	}

	/* tagbody */
	GetArrayA2(clos, EvalClosure_TagBody, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		gettag_tabletagbody(left, &left);
		Code_leftright(local, code, LAMBDA_TAGBODY, left);
	}

	/* block */
	GetArrayA2(clos, EvalClosure_Block, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		Code_leftright(local, code, LAMBDA_BLOCK, left);
	}
}

static void code_lambda_operator(LocalRoot local,
		addr code, addr name, addr scope, addr pos)
{
	addr doc, table, form, the;

	GetEvalScopeIndex(scope, EvalLambda_Table, &table);
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	GetEvalScopeIndex(scope, EvalLambda_Form, &form);
	GetEvalScopeIndex(scope, EvalLambda_The, &the);
	if ((name != Nil) && getreference_tablefunction(table)) {
		code_push5(local, code, CONSTANT_CODE_LAMBDA_SELF, name, pos, the, doc, form);
	}
	else {
		code_push5(local, code, CONSTANT_CODE_LAMBDA, name, pos, the, doc, form);
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

static void code_lambda(LocalRoot local, addr code, addr scope)
{
	if (! evalcode_remp(code))
		code_lambda_function(local, code, Nil, scope);
}

static void code_lambda_set(LocalRoot local, addr code, addr scope)
{
	addr call;
	modeswitch mode;

	set_modeswitch(code, &mode);
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	code_lambda_function(local, code, call, scope);
	rollback_modeswitch(code, &mode);
}

static void code_defun(LocalRoot local, addr code, addr scope)
{
	code_lambda_set(local, code, scope);
	Code_single(local, code, DEFUN);
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
	Code_leftright(local, code, MACRO_BIND, args);
}

static void code_macro_code(LocalRoot local, addr code, addr scope, addr *ret)
{
	addr args, free, cons;
	modeswitch mode;

	/* set mode */
	set_modeswitch(code, &mode);
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
	rollback_modeswitch(code, &mode);
}

static void code_macro_function(LocalRoot local, addr code, addr name, addr scope)
{
	addr pos, clos, doc;

	Check(evalcode_remp(code), "modeswitch error");
	/* function */
	code_macro_code(local, code, scope, &pos);
	/* closure */
	GetEvalScopeIndex(scope, EvalLambda_Clos, &clos);
	code_lambda_clos(local, code, clos);
	/* make code */
	GetEvalScopeIndex(scope, EvalLambda_Doc, &doc);
	code_double(local, code, CONSTANT_CODE_MACRO_LAMBDA, pos, doc);
	if_push_result(local, code);
}

static void code_macro_lambda(LocalRoot local, addr code, addr scope)
{
	addr call;
	GetEvalScopeIndex(scope, EvalLambda_Call, &call);
	code_macro_function(local, code, call, scope);
}

static void code_defmacro(LocalRoot local, addr code, addr scope)
{
	addr name, lambda;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &lambda);
	Code_double(local, code, DEFMACRO, name, lambda);
	if_push_result(local, code);
}

static void code_dbind_args(LocalRoot local, addr code, addr args)
{
	code_macro_bind_args(local, code, args, &args);
	Code_leftright(local, code, DESTRUCTURING_BIND, args);
}

static void code_dbind_code(LocalRoot local, addr code, addr scope, addr *ret)
{
	addr args, free, cons;
	modeswitch mode;

	/* set mode */
	set_modeswitch(code, &mode);
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
	rollback_modeswitch(code, &mode);
}

static void code_destructuring_bind(LocalRoot local, addr code, addr scope)
{
	addr expr, args;
	modeswitch mode;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &args);

	set_modeswitch(code, &mode);
	code_execute(local, code, expr);
	Code_single(local, code, PUSH_RESULT);
	code_dbind_code(local, code, args, &args);
	Code_leftright(local, code, EXECUTE, args);
	rollback_modeswitch(code, &mode);
	if_push_result(local, code);
}

static void code_define_symbol_macro(LocalRoot local, addr code, addr scope)
{
	addr symbol, lambda, body;

	GetEvalScopeIndex(scope, 0, &symbol);
	GetEvalScopeIndex(scope, 1, &lambda);
	GetEvalScopeIndex(scope, 2, &body);
	Code_push3(local, code, DEFINE_SYMBOL_MACRO, symbol, lambda, body);
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
	Code_leftright(local, code, LOCAL_ALLOC, pos);

	/* init -> local storage */
	cons = args;
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		GetCdr(pos, &pos);
		code_lambda_set(local, code, pos);
		index_heap(&index, i);
		Code_leftright(local, code, LOCAL_RESULT, index);
	}

	/* local storage -> var */
	cons = args;
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &pos, &cons);
		GetCar(pos, &pos);
		getname_tablefunction(pos, &pos);
		index_heap(&index, i);
		Code_double(local, code, FLET, pos, index);
	}
}

static void code_flet(LocalRoot local, addr code, addr scope)
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
	Code_leftright(local, code, EXECUTE, cons);
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
		Code_leftright(local, code, LABELS, call);
	}
}

static void code_labels(LocalRoot local, addr code, addr scope)
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
	Code_leftright(local, code, EXECUTE, cons);
}

static int call_specialize(LocalRoot local, addr code, addr scope);
static void code_call(LocalRoot local, addr code, addr scope)
{
	addr call, args, pos, value;

	if (call_specialize(local, code, scope)) return;
	GetEvalScopeIndex(scope, 0, &call);
	GetEvalScopeIndex(scope, 1, &args);
	pushstack_return(local, code);
	/* args */
	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &value);
		code_execute_push(local, code, value);
		if (getcheck_tablecall(pos)) {
			gettype_tablecall(pos, &value);
			Code_leftright(local, code, CALL_TYPE, value);
		}
	}
	/* function */
	code_execute_set(local, code, call);
	/* call */
	Code_single(local, code, CALL);
	popstack(local, code, &pos);
	Code_leftright(local, code, EXECUTE, pos);
	if_push_result(local, code);
}

static void code_values_set(LocalRoot local, addr code, addr cons)
{
	addr pos;

	/* nil */
	if (cons == Nil) {
		Code_single(local, code, VALUES_NIL);
		return;
	}

	/* list */
	pushstack_return(local, code);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		code_execute_push(local, code, pos);
	}
	Code_single(local, code, VALUES_SET);
	popstack(local, code, &pos);
	Code_leftright(local, code, EXECUTE, pos);
}

static void code_values_push(LocalRoot local, addr code, addr cons)
{
	addr pos;

	/* nil */
	if (cons == Nil) {
		code_nil(local, code);
		return;
	}

	/* list */
	GetCons(cons, &pos, &cons);
	code_execute_push(local, code, pos);
	code_allcons_rem(local, code, cons);
}

static void code_values(LocalRoot local, addr code, addr scope)
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

static void code_the(LocalRoot local, addr code, addr scope)
{
	addr form, check;

	GetEvalScopeValue(scope, &form);
	GetEvalScopeIndex(scope, 0, &check);
	if (check == Nil) {
		code_execute(local, code, form);
	}
	else {
		code_execute_set(local, code, form);
		GetEvalScopeThe(scope, &form);
		Code_leftright(local, code, THE, form);
		if_push_result(local, code);
	}
}

static void code_locally(LocalRoot local, addr code, addr scope)
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
	if (check) {
		/* (if expr then) */
		make_label(local, code, &label_end);
		code_if_nil(local, code, label_end);
		code_execute(local, code, then);
		push_label(local, code, label_end);
	}
	else {
		/* (if expr then else) */
		make_label(local, code, &label_else);
		make_label(local, code, &label_end);
		code_if_nil(local, code, label_else);
		code_execute(local, code, then);
		code_goto(local, code, label_end);
		push_label(local, code, label_else);
		code_execute(local, code, last);
		push_label(local, code, label_end);
	}
}

static void code_if_false(LocalRoot local, addr code, addr then, addr last)
{
	int check;
	addr label_else, label_end;

	check = (RefEvalScopeType(last) == EVAL_PARSE_NIL);
	if (check) {
		/* (if (not expr) then) */
		make_label(local, code, &label_end);
		code_if_t(local, code, label_end);
		code_execute(local, code, then);
		push_label(local, code, label_end);
	}
	else {
		/* (if (not expr) then else) */
		make_label(local, code, &label_else);
		make_label(local, code, &label_end);
		code_if_t(local, code, label_else);
		code_execute(local, code, then);
		code_goto(local, code, label_end);
		push_label(local, code, label_else);
		code_execute(local, code, last);
		push_label(local, code, label_end);
	}
}

static void code_if(LocalRoot local, addr code, addr scope)
{
	addr expr, then, last;

	GetEvalScopeIndex(scope, 0, &expr);
	GetEvalScopeIndex(scope, 1, &then);
	GetEvalScopeIndex(scope, 2, &last);

	code_execute_set(local, code, expr);
	if (code_if_not_p(expr))
		code_if_false(local, code, then, last);
	else
		code_if_true(local, code, then, last);
}

static void code_unwind_protect(LocalRoot local, addr code, addr scope)
{
	addr form, cons, protect, cleanup;

	GetEvalScopeIndex(scope, 0, &form);
	GetEvalScopeIndex(scope, 1, &cons);
	/* protect */
	pushstack_protect(local, code);
	code_execute(local, code, form);
	popstack(local, code, &protect);
	/* cleanup */
	pushstack_close(local, code);
	code_allcons_rem(local, code, cons);
	popstack(local, code, &cleanup);
	/* set code */
	setinfo_code(protect, cleanup);
	Code_leftright(local, code, EXECUTE, protect);
}

static void code_tagbody_cons(LocalRoot local, addr code, addr cons)
{
	addr pos;
	modeswitch mode;

	rem_modeswitch(code, &mode);
	while (cons != Nil) {
		GetCons(cons, &pos, &cons);
		if (RefEvalScopeType(pos) == EVAL_PARSE_TAG) {
			GetEvalScopeValue(pos, &pos);
			Code_leftright(local, code, TAG, pos);
		}
		else {
			code_execute(local, code, pos);
		}
	}
	rollback_modeswitch(code, &mode);
}

static void code_tagbody(LocalRoot local, addr code, addr scope)
{
	addr tag, cons;

	GetEvalScopeIndex(scope, 0, &tag);
	GetEvalScopeIndex(scope, 1, &cons);
	/* body */
	pushstack_tagbody(local, code);
	code_tagbody_cons(local, code, cons);
	popstack_tagbody(local, code, tag, &cons);
	/* execute */
	Code_leftright(local, code, EXECUTE, cons);
}

static void code_go(LocalRoot local, addr code, addr scope)
{
	addr pos;

	GetEvalScopeValue(scope, &pos);
	gettag_tabletagbody(pos, &pos);
	Code_leftright(local, code, GO, pos);
}

static void code_block(LocalRoot local, addr code, addr scope)
{
	addr name, cons;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);
	pushstack_block(local, code);
	code_allcons(local, code, cons);
	popstack_block(local, code, name, &cons);
	Code_leftright(local, code, EXECUTE, cons);
}

static void code_return_from(LocalRoot local, addr code, addr scope)
{
	addr name, form;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &form);
	code_execute_set(local, code, form);
	Code_leftright(local, code, RETURN_FROM, name);
}

static void code_catch(LocalRoot local, addr code, addr scope)
{
	addr name, cons;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &cons);
	pushstack_catch(local, code);
	code_execute_set(local, code, name);
	Code_single(local, code, CATCH);
	code_allcons(local, code, cons);
	popstack(local, code, &cons);
	Code_leftright(local, code, EXECUTE, cons);
}

static void code_throw(LocalRoot local, addr code, addr scope)
{
	addr name, form;

	GetEvalScopeIndex(scope, 0, &name);
	GetEvalScopeIndex(scope, 1, &form);
	pushstack_close(local, code);
	code_execute_push(local, code, name);
	code_execute_set(local, code, form);
	Code_single(local, code, THROW);
	popstack(local, code, &form);
	Code_leftright(local, code, EXECUTE, form);
}

static void code_multiple_value_bind(LocalRoot local, addr code, addr scope)
{
	addr args, expr, cons, free;

	GetEvalScopeIndex(scope, 0, &args);
	GetEvalScopeIndex(scope, 1, &expr);
	GetEvalScopeIndex(scope, 4, &cons);
	GetEvalScopeIndex(scope, 5, &free);

	pushstack_mode(local, code);
	code_execute_set(local, code, expr);
	code_free_declare(local, code, free);
	Code_leftright(local, code, MULTIPLE_VALUE_BIND, args);
	code_allcons_set(local, code, cons);
	popstack(local, code, &cons);
	Code_leftright(local, code, EXECUTE, cons);
}

static void code_multiple_value_call(LocalRoot local, addr code, addr scope)
{
	addr call, args, pos;

	GetEvalScopeIndex(scope, 0, &call);
	GetEvalScopeIndex(scope, 1, &args);
	pushstack_return(local, code);
	/* call */
	code_execute_push(local, code, call);
	/* args */
	while (args != Nil) {
		GetCons(args, &pos, &args);
		code_execute_set(local, code, pos);
		Code_single(local, code, PUSH_VALUES);
	}
	/* call */
	Code_single(local, code, FUNCALL);
	popstack(local, code, &pos);
	Code_leftright(local, code, EXECUTE, pos);
	if_push_result(local, code);
}

static void code_multiple_value_prog1(LocalRoot local, addr code, addr scope)
{
	addr first, cons, pos;

	GetEvalScopeIndex(scope, 0, &first);
	GetEvalScopeIndex(scope, 1, &cons);
	pushstack_return(local, code);
	/* first */
	code_execute_set(local, code, first);
	/* cons */
	pushstack_close(local, code);
	code_allcons_rem(local, code, cons);
	popstack(local, code, &pos);
	Code_leftright(local, code, EXECUTE, pos);
	/* result */
	popstack(local, code, &pos);
	Code_leftright(local, code, EXECUTE, pos);
	if_push_result(local, code);
}

static void code_nth_value(LocalRoot local, addr code, addr scope)
{
	addr nth, expr, pos;

	GetEvalScopeIndex(scope, 0, &nth);
	GetEvalScopeIndex(scope, 1, &expr);
	pushstack_return(local, code);
	code_execute_push(local, code, nth);
	code_execute_set(local, code, expr);
	Code_single(local, code, NTH_VALUE);
	popstack(local, code, &pos);
	Code_leftright(local, code, EXECUTE, pos);
	if_push_result(local, code);
}

static void code_progv(LocalRoot local, addr code, addr scope)
{
	addr symbols, values, body, pos;

	GetEvalScopeIndex(scope, 0, &symbols);
	GetEvalScopeIndex(scope, 1, &values);
	GetEvalScopeIndex(scope, 2, &body);
	pushstack_return(local, code);
	code_execute_push(local, code, symbols);
	code_execute_push(local, code, values);
	Code_single(local, code, PROGV);
	code_allcons_set(local, code, body);
	popstack(local, code, &pos);
	Code_leftright(local, code, EXECUTE, pos);
	if_push_result(local, code);
}


/*
 *  code
 */
static void code_execute(LocalRoot local, addr code, addr scope)
{
	Check(! eval_scope_p(scope), "type error");
	switch (RefEvalScopeType(scope)) {
		case EVAL_PARSE_NIL:
			code_nil(local, code);
			break;

		case EVAL_PARSE_T:
			code_t(local, code);
			break;

		case EVAL_PARSE_INTEGER:
		case EVAL_PARSE_RATIONAL:
		case EVAL_PARSE_COMPLEX:
		case EVAL_PARSE_CHARACTER:
		case EVAL_PARSE_ARRAY:
		case EVAL_PARSE_VECTOR:
		case EVAL_PARSE_BITVECTOR:
		case EVAL_PARSE_STRING:
		case EVAL_PARSE_FLOAT:
		case EVAL_PARSE_QUOTE:
		case EVAL_PARSE_PATHNAME:
			code_value(local, code, scope);
			break;

		case EVAL_PARSE_DECLAIM:
			code_declaim(local, code, scope);
			break;

		case EVAL_PARSE_PROGN:
			code_progn(local, code, scope);
			break;

		case EVAL_PARSE_LET:
			code_let(local, code, scope);
			break;

		case EVAL_PARSE_LETA:
			code_leta(local, code, scope);
			break;

		case EVAL_PARSE_SYMBOL:
			code_symbol(local, code, scope);
			break;

		case EVAL_PARSE_SETQ:
			code_setq(local, code, scope);
			break;

		case EVAL_PARSE_FUNCTION:
			code_function(local, code, scope);
			break;

		case EVAL_PARSE_LAMBDA:
			code_lambda(local, code, scope);
			break;

		case EVAL_PARSE_DEFUN:
			code_defun(local, code, scope);
			break;

		case EVAL_PARSE_MACRO_LAMBDA:
			code_macro_lambda(local, code, scope);
			break;

		case EVAL_PARSE_DEFMACRO:
			code_defmacro(local, code, scope);
			break;

		case EVAL_PARSE_DESTRUCTURING_BIND:
			code_destructuring_bind(local, code, scope);
			break;

		case EVAL_PARSE_DEFINE_SYMBOL_MACRO:
			code_define_symbol_macro(local, code, scope);
			break;

		case EVAL_PARSE_FLET:
			code_flet(local, code, scope);
			break;

		case EVAL_PARSE_LABELS:
			code_labels(local, code, scope);
			break;

		case EVAL_PARSE_CALL:
			code_call(local, code, scope);
			break;

		case EVAL_PARSE_VALUES:
			code_values(local, code, scope);
			break;

		case EVAL_PARSE_THE:
			code_the(local, code, scope);
			break;

		case EVAL_PARSE_LOCALLY:
			code_locally(local, code, scope);
			break;

		case EVAL_PARSE_IF:
			code_if(local, code, scope);
			break;

		case EVAL_PARSE_UNWIND_PROTECT:
			code_unwind_protect(local, code, scope);
			break;

		case EVAL_PARSE_TAGBODY:
			code_tagbody(local, code, scope);
			break;

		case EVAL_PARSE_GO:
			code_go(local, code, scope);
			break;

		case EVAL_PARSE_BLOCK:
			code_block(local, code, scope);
			break;

		case EVAL_PARSE_RETURN_FROM:
			code_return_from(local, code, scope);
			break;

		case EVAL_PARSE_CATCH:
			code_catch(local, code, scope);
			break;

		case EVAL_PARSE_THROW:
			code_throw(local, code, scope);
			break;

		case EVAL_PARSE_MULTIPLE_VALUE_BIND:
			code_multiple_value_bind(local, code, scope);
			break;

		case EVAL_PARSE_MULTIPLE_VALUE_CALL:
			code_multiple_value_call(local, code, scope);
			break;

		case EVAL_PARSE_MULTIPLE_VALUE_PROG1:
			code_multiple_value_prog1(local, code, scope);
			break;

		case EVAL_PARSE_NTH_VALUE:
			code_nth_value(local, code, scope);
			break;

		case EVAL_PARSE_PROGV:
			code_progv(local, code, scope);
			break;

		case EVAL_PARSE_EVAL_WHEN:
		case EVAL_PARSE_EMPTY:
		case EVAL_PARSE_TAG:
		case EVAL_PARSE_SIZE:
		default:
			fmte("Invalid scope type.", NULL);
			break;
	}
}

static void eval_code_execute(LocalRoot local, addr *ret, addr scope)
{
	addr code;

	evalcode_local(local, &code);
	code_execute_set(local, code, scope);
	popstack(local, code, ret);
}

void eval_code(LocalRoot local, addr *ret, addr scope)
{
	LocalStack stack;

	push_local(local, &stack);
	eval_code_execute(local, ret, scope);
	rollback_local(local, stack);
}


/*
 *  call specialize
 */
static int callconst_symbol_p(addr call, constindex index)
{
	addr left, right;

	if (RefEvalScopeType(call) != EVAL_PARSE_FUNCTION) return 0;
	GetConstant(index, &left);
	GetEvalScopeValue(call, &call);
	getname_tablefunction(call, &call);
	GetCallName(call, &right);
	return RefCallNameType(call) == CALLNAME_SYMBOL && left == right;
}

static void call_allcons(LocalRoot local, addr code, addr args)
{
	addr pos;

	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &pos);
		code_execute_set(local, code, pos);
	}
}

static int call_handler(LocalRoot local, addr code, addr scope)
{
	addr args;

	GetEvalScopeIndex(scope, 1, &args);
	pushstack_condition(local, code);
	call_allcons(local, code, args);
	popstack(local, code, &args);
	Code_leftright(local, code, EXECUTE, args);

	return 1;
}

static int call_restart(LocalRoot local, addr code, addr scope)
{
	addr args;

	GetEvalScopeIndex(scope, 1, &args);
	pushstack_restart(local, code);
	call_allcons(local, code, args);
	popstack(local, code, &args);
	Code_leftright(local, code, EXECUTE, args);

	return 1;
}

static int call_push_return(LocalRoot local, addr code, addr scope)
{
	addr args;

	GetEvalScopeIndex(scope, 1, &args);
	pushstack_return(local, code);
	call_allcons(local, code, args);
	popstack(local, code, &args);
	Code_leftright(local, code, EXECUTE, args);

	return 1;
}

static int call_function_type(LocalRoot local, addr code, addr scope, constindex type)
{
	addr args, pos;

	GetEvalScopeIndex(scope, 1, &args);
	while (args != Nil) {
		GetCons(args, &pos, &args);
		getvalue_tablecall(pos, &pos);
		code_execute_push(local, code, pos);
	}
	code_single(local, code, type);

	return 1;
}

static int call_handler_bind(LocalRoot local, addr code, addr scope)
{
	return call_function_type(local, code, scope, CONSTANT_CODE_HANDLER_BIND);
}

static int call_handler_case(LocalRoot local, addr code, addr scope)
{
	return call_function_type(local, code, scope, CONSTANT_CODE_HANDLER_CASE);
}

static int call_restart_bind(LocalRoot local, addr code, addr scope)
{
	return call_function_type(local, code, scope, CONSTANT_CODE_RESTART_BIND);
}

static int call_restart_case(LocalRoot local, addr code, addr scope)
{
	return call_function_type(local, code, scope, CONSTANT_CODE_RESTART_CASE);
}

static int call_specialize(LocalRoot local, addr code, addr scope)
{
	addr call;

	GetEvalScopeIndex(scope, 0, &call);

	/* lisp-system::handler */
	if (callconst_symbol_p(call, CONSTANT_SYSTEM_HANDLER)) {
		return call_handler(local, code, scope);
	}

	/* lisp-system::restart */
	if (callconst_symbol_p(call, CONSTANT_SYSTEM_RESTART)) {
		return call_restart(local, code, scope);
	}

	/* lisp-system::push-return */
	if (callconst_symbol_p(call, CONSTANT_SYSTEM_PUSH_RETURN)) {
		return call_push_return(local, code, scope);
	}

	/* lisp-system::handler-bind */
	if (callconst_symbol_p(call, CONSTANT_SYSTEM_HANDLER_BIND)) {
		return call_handler_bind(local, code, scope);
	}

	/* lisp-system::handler-case */
	if (callconst_symbol_p(call, CONSTANT_SYSTEM_HANDLER_CASE)) {
		return call_handler_case(local, code, scope);
	}

	/* lisp-system::restart-bind */
	if (callconst_symbol_p(call, CONSTANT_SYSTEM_RESTART_BIND)) {
		return call_restart_bind(local, code, scope);
	}

	/* lisp-system::restart-case */
	if (callconst_symbol_p(call, CONSTANT_SYSTEM_RESTART_CASE)) {
		return call_restart_case(local, code, scope);
	}

	return 0;
}

