#include "callname.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_object.h"
#include "copy.h"
#include "declare.h"
#include "eval_object.h"
#include "eval_stack.h"
#include "eval_table.h"
#include "function.h"
#include "object.h"
#include "parse.h"
#include "sequence.h"
#include "symbol.h"
#include "type.h"
#include "type_parse.h"
#include "type_subtypep.h"
#include "type_value.h"

/*
 *  memory
 */
_g void eval_stack_alloc(LocalRoot local, addr *ret, enum EVAL_STACK_MODE type)
{
	int i;
	addr pos;
	LocalStack stack;
	struct eval_stack *str;

	/* local-stack */
	if (local)
		push_local(local, &stack);
	else
		stack = NULL;

	/* memory */
	Check(0xFF < sizeof(struct eval_stack), "struct size error");
	eval_alloc(local, &pos, EVAL_TYPE_STACK, EVAL_STACK_SIZE,
			sizeoft(struct eval_stack));
	SetEvalStackType(pos, type);

	/* struct */
	str = StructEvalStack(pos);
	str->stack = stack;
	str->globalp = 0;
	str->lexical = 0;
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		str->optimize[i] = -1;
	*ret = pos;
}
_g void eval_stack_local(LocalRoot local, addr *ret, enum EVAL_STACK_MODE type)
{
	Check(local == NULL, "local error");
	eval_stack_alloc(local, ret, type);
}
_g void eval_stack_heap(addr *ret, enum EVAL_STACK_MODE type)
{
	/* for global stack */
	eval_stack_alloc(NULL, ret, type);
}


/*
 *  eval-stack
 */
_g struct eval_stack *structevalstack(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return StructEvalStack_Low(pos);
}
_g enum EVAL_STACK_MODE refevalstacktype(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return RefEvalStackType_Low(pos);
}
_g void getevalstacktype(addr pos, enum EVAL_STACK_MODE *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackType_Low(pos, ret);
}
_g void setevalstacktype(addr pos, enum EVAL_STACK_MODE value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackType_Low(pos, value);
}
_g void getevalstacknext(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackNext_Low(pos, ret);
}
_g void setevalstacknext(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackNext_Low(pos, value);
}
_g void getevalstacktable(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackTable_Low(pos, ret);
}
_g void setevalstacktable(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackTable_Low(pos, value);
}
_g void getevalstackscope(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackScope_Low(pos, ret);
}
_g void setevalstackscope(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackScope_Low(pos, value);
}
_g void getevalstacklexical(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackLexical_Low(pos, ret);
}
_g void setevalstacklexical(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackLexical_Low(pos, value);
}


/*
 *  eval-stack
 */
_g int eval_stack_lambda_lexical_p(addr stack)
{
	enum EVAL_STACK_MODE type;
	GetEvalStackType(stack, &type);
	return (type == EVAL_STACK_MODE_LAMBDA) || (type == EVAL_STACK_MODE_LEXICAL);
}

static void getstack_symbol(addr *ret)
{
	GetConst(SYSTEM_EVAL_SCOPE, ret);
}
static void getglobal_symbol(addr *ret)
{
	GetConst(SYSTEM_EVAL_SCOPE_GLOBAL, ret);
}

_g void getstack_eval(Execute ptr, addr *ret)
{
	addr symbol;
	getstack_symbol(&symbol);
	getspecialcheck_local(ptr, symbol, ret);
}
_g void getglobal_eval(Execute ptr, addr *ret)
{
	addr symbol;
	getglobal_symbol(&symbol);
	getspecialcheck_local(ptr, symbol, ret);
}

_g addr newstack_eval(Execute ptr, enum EVAL_STACK_MODE type)
{
	addr stack, symbol, next;

	getstack_symbol(&symbol);
	eval_stack_local(ptr->local, &stack, type);
	getspecialcheck_local(ptr, symbol, &next);
	SetEvalStackNext(stack, next);
	setspecial_local(ptr, symbol, stack);

	return stack;
}

static void closestack_unsafe(Execute ptr)
{
	addr symbol, eval;
	LocalStack stack;

	/* replace eval-stack */
	getstack_symbol(&symbol);
	getspecialcheck_local(ptr, symbol, &eval);
	if (eval == Nil)
		fmte("scope-stack is nil.", NULL);
	stack = StructEvalStack(eval)->stack;
	GetEvalStackNext(eval, &eval);
	setspecial_local(ptr, symbol, eval);

	/* free eval-stack */
	rollback_local(ptr->local, stack);
}

_g void freestack_eval(Execute ptr, addr scope)
{
	addr symbol, pos;

	getstack_symbol(&symbol);
#ifdef LISP_DEBUG
	getspecialcheck_local(ptr, symbol, &pos);
	for (;;) {
		Check(pos == Nil, "stack error [check].");
		Check(pos == Unbound, "unbound error.");
		if (pos == scope) break;
		GetEvalStackNext(pos, &pos);
	}
#endif
	for (;;) {
		getspecialcheck_local(ptr, symbol, &pos);
		Check(pos == Nil, "stack error");
		closestack_unsafe(ptr);
		if (pos == scope) break;
	}
}

_g void begin_eval_stack(Execute ptr)
{
	addr symbol, stack;

	/* Global stack must be a heap object. Don't use local function.  */
	eval_stack_heap(&stack, EVAL_STACK_MODE_NIL);
	StructEvalStack(stack)->globalp = 1;
	getglobal_symbol(&symbol);
	pushspecial_control(ptr, symbol, stack);
	getstack_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
	/* new stack */
	newstack_nil(ptr);
}

static int function_free_eval_stack(Execute ptr)
{
	addr symbol;

	getglobal_symbol(&symbol);
	setspecial_local(ptr, symbol, Nil);
	getstack_symbol(&symbol);
	setspecial_local(ptr, symbol, Nil);

	return 0;
}

_g void free_eval_stack(Execute ptr)
{
	setprotect_control(ptr, p_defun_free_eval_stack, Nil);
}

_g int globalp_stack_eval(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return StructEvalStack(pos)->globalp;
}

_g size_t increment_stack_eval(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return (StructEvalStack(pos)->lexical)++;
}

_g size_t getlexical_stack_eval(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return StructEvalStack(pos)->lexical;
}

_g void getlexical_index_heap(addr stack, addr *ret)
{
	size_t size;

	size = getlexical_stack_eval(stack);
	if (size == 0)
		*ret = Nil;
	else
		index_heap(ret, size);
}

_g void init_eval_stack(void)
{
	SetPointerCall(defun, empty, free_eval_stack);
}


/*
 *  declaim
 */
static void apply_pushnew_stack(LocalRoot local,
		addr stack, addr cons, constindex index)
{
	addr key, table, symbol;

	GetConstant(index, &key);
	GetEvalStackTable(stack, &table);
	while (cons != Nil) {
		GetCons(cons, &symbol, &cons);
		if (pushnewplist_alloc(local, table, key, symbol, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_pushnew_callname_stack(LocalRoot local,
		addr stack, addr cons, constindex index)
{
	addr key, table, callname;

	GetConstant(index, &key);
	GetEvalStackTable(stack, &table);
	while (cons != Nil) {
		GetCons(cons, &callname, &cons);
		copylocal_object(local, &callname, callname);
		if (pushnewplist_callname_alloc(local, table, key, callname, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_plist_stack(LocalRoot local,
		addr stack, addr cons, constindex index)
{
	addr key, table, symbol, value;

	GetConstant(index, &key);
	GetEvalStackTable(stack, &table);
	while (cons != Nil) {
		GetCons(cons, &symbol, &cons);
		Check(cons == Nil, "plist error");
		GetCons(cons, &value, &cons);
		copylocal_object(local, &value, value);
		if (setpplist_alloc(local, table, key, symbol, value, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_plist_callname_stack(LocalRoot local,
		addr stack, addr cons, constindex index)
{
	addr key, table, callname, value;

	GetConstant(index, &key);
	GetEvalStackTable(stack, &table);
	while (cons != Nil) {
		GetCons(cons, &callname, &cons);
		Check(cons == Nil, "plist error");
		GetCons(cons, &value, &cons);
		copylocal_object(local, &callname, callname);
		copylocal_object(local, &value, value);
		if (setpplist_callname_alloc(local, table, key, callname, value, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_optimize_stack(addr stack, addr declare)
{
	int i;
	OptimizeType *left, value;
	const OptimizeType *right;

	left = StructEvalStack(stack)->optimize;
	right = getall_optimize_declare(declare);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		value = right[i];
		if (0 <= value)
			left[i] = value;
	}
}

static void apply_declare_switch(LocalRoot local,
		addr stack, addr declare, int declaimp)
{
	addr pos;

	/* inline, notinline */
	getall_inline_declare(declare, &pos);
	apply_plist_callname_stack(local, stack, pos, CONSTANT_SYSTEM_INLINE);
	/* special */
	getall_special_declare(declare, &pos);
	apply_pushnew_stack(local, stack, pos, CONSTANT_SYSTEM_TYPE_SCOPE);
	/* type */
	getall_type_value_declare(declare, &pos);
	apply_plist_stack(local, stack, pos, CONSTANT_SYSTEM_TYPE_VALUE);
	/* ftype */
	getall_type_function_declare(declare, &pos);
	apply_plist_callname_stack(local, stack, pos, CONSTANT_SYSTEM_TYPE_FUNCTION);
	/* optimize */
	apply_optimize_stack(stack, declare);

	if (declaimp) {
		/* declaration */
		getall_declaration_declare(declare, &pos);
		apply_pushnew_stack(local, stack, pos, CONSTANT_SYSTEM_DECLARATION);
		/* dynamic-extent, ignore and ignorable are declaration only. */
	}
	else {
		/* dynamic-extent value */
		getall_dynamic_value_declare(declare, &pos);
		apply_pushnew_stack(local, stack, pos, CONSTANT_SYSTEM_DYNAMIC_VALUE);
		/* dynamic-extent function */
		getall_dynamic_function_declare(declare, &pos);
		apply_pushnew_callname_stack(local,
				stack, pos, CONSTANT_SYSTEM_DYNAMIC_FUNCTION);
		/* ignore, ignorable value */
		getall_ignore_value_declare(declare, &pos);
		apply_plist_stack(local, stack, pos, CONSTANT_SYSTEM_IGNORE_VALUE);
		/* ignore, ignorable function */
		getall_ignore_function_declare(declare, &pos);
		apply_plist_callname_stack(local, stack, pos, CONSTANT_SYSTEM_IGNORE_FUNCTION);
		/* declaration is proclamation only. */
	}
}

_g void apply_declaim_stack(Execute ptr, addr declare)
{
	addr stack;

	if (declare == Nil) return;
	getglobal_eval(ptr, &stack);
	apply_declare_switch(NULL, stack, declare, 1);
}


/*
 *  declare
 */
_g void apply_declare_stack(LocalRoot local, addr stack, addr declare)
{
	if (declare == Nil) return;
	apply_declare_switch(local, stack, declare, 0);
}

/* for let* arguments */
static void apply_pushsymbol_stack(LocalRoot local,
		addr stack, addr symbol, addr cons, constindex index)
{
	addr key, table;

	if (find_list_eq_unsafe(symbol, cons)) {
		GetConstant(index, &key);
		GetEvalStackTable(stack, &table);
		if (pushnewplist_alloc(local, table, key, symbol, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_plistsymbol_stack(LocalRoot local,
		addr stack, addr symbol, addr cons, constindex index)
{
	addr key, table;

	if (getplist(cons, symbol, &cons) == 0) {
		copylocal_object(local, &cons, cons);
		GetConstant(index, &key);
		GetEvalStackTable(stack, &table);
		if (setpplist_alloc(local, table, key, symbol, cons, &table))
			SetEvalStackTable(stack, table);
	}
}

_g void apply_declare_value_stack(LocalRoot local, addr stack, addr symbol, addr declare)
{
	addr pos;

	if (declare == Nil) return;
	/* inline, notinline */
	/* special */
	getall_special_declare(declare, &pos);
	apply_pushsymbol_stack(local, stack, symbol, pos, CONSTANT_SYSTEM_TYPE_SCOPE);
	/* type */
	getall_type_value_declare(declare, &pos);
	apply_plistsymbol_stack(local, stack, symbol, pos, CONSTANT_SYSTEM_TYPE_VALUE);
	/* ftype */
	/* optimize */
	/* dynamic-extent value */
	getall_dynamic_value_declare(declare, &pos);
	apply_pushsymbol_stack(local, stack, symbol, pos, CONSTANT_SYSTEM_DYNAMIC_VALUE);
	/* dynamic-extent function */
	/* ignore, ignorable value */
	getall_ignore_value_declare(declare, &pos);
	apply_plistsymbol_stack(local, stack, symbol, pos, CONSTANT_SYSTEM_IGNORE_VALUE);
	/* ignore, ignorable function */
	/* declaration is proclamation only. */
}

/* for labels arguments */
static void apply_pushcall_stack(LocalRoot local,
		addr stack, addr call, addr cons, constindex index)
{
	addr key, table;

	if (find_list_callname_unsafe(call, cons)) {
		copylocal_object(local, &call, call);
		copylocal_object(local, &cons, cons);
		GetConstant(index, &key);
		GetEvalStackTable(stack, &table);
		if (pushnewplist_callname_alloc(local, table, key, call, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_plistcall_stack(LocalRoot local,
		addr stack, addr call, addr cons, constindex index)
{
	addr key, table;

	if (getplist_callname(cons, call, &cons) == 0) {
		copylocal_object(local, &call, call);
		copylocal_object(local, &cons, cons);
		GetConstant(index, &key);
		GetEvalStackTable(stack, &table);
		if (setpplist_callname_alloc(local, table, key, call, cons, &table))
			SetEvalStackTable(stack, table);
	}
}

_g void apply_declare_function_stack(LocalRoot local, addr stack, addr call, addr declare)
{
	addr pos;

	if (declare == Nil) return;
	/* inline, notinline */
	getall_inline_declare(declare, &pos);
	apply_plistcall_stack(local, stack, call, pos, CONSTANT_SYSTEM_INLINE);
	/* special */
	/* type */
	/* ftype */
	getall_type_function_declare(declare, &pos);
	apply_plistcall_stack(local, stack, call, pos, CONSTANT_SYSTEM_TYPE_FUNCTION);
	/* optimize */
	/* dynamic-extent value */
	/* dynamic-extent function */
	getall_dynamic_function_declare(declare, &pos);
	apply_pushcall_stack(local, stack, call, pos, CONSTANT_SYSTEM_DYNAMIC_FUNCTION);
	/* ignore, ignorable value */
	/* ignore, ignorable function */
	getall_ignore_function_declare(declare, &pos);
	apply_plistcall_stack(local, stack, call, pos, CONSTANT_SYSTEM_IGNORE_FUNCTION);
	/* declaration is proclamation only. */
}


/*
 *  table scope
 */
_g int getvalue_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! symbolp(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return getvalue_evaltable(stack, pos, ret);
}

_g void setvalue_scope_evalstack(addr stack, addr pos)
{
	addr list;
#ifdef LISP_DEBUG
	addr name;
#endif

	Check(! eval_stack_p(stack), "type error");
	CheckTableValue(pos);
#ifdef LISP_DEBUG
	getname_tablevalue(pos, &name);
	Check(getvalue_scope_evalstack(stack, name, NULL), "duplicate error");
#endif
	evaltable_value_heap(&pos, pos);
	GetEvalStackScope(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackScope(stack, list);
}

_g int getfunction_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! callnamep(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return getfunction_evaltable(stack, pos, ret);
}

_g void setfunction_scope_evalstack(addr stack, addr pos)
{
	addr list;
#ifdef LISP_DEBUG
	addr name;
#endif

	Check(! eval_stack_p(stack), "type error");
	CheckTableFunction(pos);
#ifdef LISP_DEBUG
	getname_tablefunction(pos, &name);
	Check(getfunction_scope_evalstack(stack, name, NULL), "duplicate error");
#endif
	evaltable_function_heap(&pos, pos);
	GetEvalStackScope(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackScope(stack, list);
}

_g int gettagbody_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! tagbody_tag_p(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return gettagbody_evaltable(stack, pos, ret);
}

_g void settagbody_scope_evalstack(addr stack, addr pos)
{
	addr list;
#ifdef LISP_DEBUG
	addr name;
#endif

	Check(! eval_stack_p(stack), "type error");
	CheckTableTagBody(pos);
#ifdef LISP_DEBUG
	getname_tabletagbody(pos, &name);
	Check(gettagbody_scope_evalstack(stack, name, NULL), "duplicate error");
#endif
	evaltable_tagbody_heap(&pos, pos);
	GetEvalStackScope(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackScope(stack, list);
}

_g int getblock_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! symbolp(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return getblock_evaltable(stack, pos, ret);
}

_g void setblock_scope_evalstack(addr stack, addr pos)
{
	addr list;
#ifdef LISP_DEBUG
	addr name;
#endif

	Check(! eval_stack_p(stack), "type error");
	CheckTableBlock(pos);
#ifdef LISP_DEBUG
	getname_tableblock(pos, &name);
	Check(getblock_scope_evalstack(stack, name, NULL), "duplicate error");
#endif
	evaltable_block_heap(&pos, pos);
	GetEvalStackScope(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackScope(stack, list);
}


/*
 *  table lexical
 */
_g void setvalue_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableValue(pos);
	evaltable_value_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

_g void setfunction_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableFunction(pos);
	evaltable_function_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

_g void settagbody_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableTagBody(pos);
	evaltable_tagbody_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

_g void setblock_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableBlock(pos);
	evaltable_block_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

