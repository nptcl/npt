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
#include "eval_value.h"
#include "function.h"
#include "object.h"
#include "parse.h"
#include "sequence.h"
#include "subtypep.h"
#include "symbol.h"
#include "type.h"
#include "type_parse.h"
#include "type_value.h"

/*
 *  memory
 */
void eval_stack_alloc(LocalRoot local, addr *ret, enum EVAL_STACK_MODE type)
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
void eval_stack_local(LocalRoot local, addr *ret, enum EVAL_STACK_MODE type)
{
	Check(local == NULL, "local error");
	eval_stack_alloc(local, ret, type);
}
void eval_stack_heap(addr *ret, enum EVAL_STACK_MODE type)
{
	/* for global stack */
	eval_stack_alloc(NULL, ret, type);
}


/*
 *  eval-stack
 */
struct eval_stack *structevalstack(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return StructEvalStack_Low(pos);
}
enum EVAL_STACK_MODE refevalstacktype(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return RefEvalStackType_Low(pos);
}
void getevalstacktype(addr pos, enum EVAL_STACK_MODE *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackType_Low(pos, ret);
}
void setevalstacktype(addr pos, enum EVAL_STACK_MODE value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackType_Low(pos, value);
}
void getevalstacknext(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackNext_Low(pos, ret);
}
void setevalstacknext(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackNext_Low(pos, value);
}
void getevalstacktable(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackTable_Low(pos, ret);
}
void setevalstacktable(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackTable_Low(pos, value);
}
void getevalstackscope(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackScope_Low(pos, ret);
}
void setevalstackscope(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackScope_Low(pos, value);
}
void getevalstacklexical(addr pos, addr *ret)
{
	Check(! eval_stack_p(pos), "type error");
	GetEvalStackLexical_Low(pos, ret);
}
void setevalstacklexical(addr pos, addr value)
{
	Check(! eval_stack_p(pos), "type error");
	SetEvalStackLexical_Low(pos, value);
}


/*
 *  eval-stack
 */
int eval_stack_lambda_lexical_p(addr stack)
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

int getstack_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	getstack_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}
int getglobal_eval_(Execute ptr, addr *ret)
{
	addr symbol;
	getglobal_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

int newstack_eval_(Execute ptr, enum EVAL_STACK_MODE type, addr *ret)
{
	addr stack, symbol, next;

	getstack_symbol(&symbol);
	eval_stack_local(ptr->local, &stack, type);
	Return(getspecialcheck_local_(ptr, symbol, &next));
	SetEvalStackNext(stack, next);
	setspecial_local(ptr, symbol, stack);
	if (ret)
		*ret = stack;

	return 0;
}

static int closestack_unsafe_(Execute ptr)
{
	addr symbol, eval;
	LocalStack stack;

	/* replace eval-stack */
	getstack_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &eval));
	Check(eval == Nil, "scope-stack is nil.");
	stack = StructEvalStack(eval)->stack;
	GetEvalStackNext(eval, &eval);
	setspecial_local(ptr, symbol, eval);

	/* free eval-stack */
	rollback_local(ptr->local, stack);

	return 0;
}

int freestack_eval_(Execute ptr, addr scope)
{
	addr symbol, pos;

	getstack_symbol(&symbol);
#ifdef LISP_DEBUG
	Return(getspecialcheck_local_(ptr, symbol, &pos));
	for (;;) {
		Check(pos == Nil, "stack error [check].");
		Check(pos == Unbound, "unbound error.");
		if (pos == scope)
			break;
		GetEvalStackNext(pos, &pos);
	}
#endif
	for (;;) {
		Return(getspecialcheck_local_(ptr, symbol, &pos));
		Check(pos == Nil, "stack error");
		Return(closestack_unsafe_(ptr));
		if (pos == scope)
			break;
	}

	return 0;
}

static void eval_stack_global_optimize(OptimizeType *optimize, addr list)
{
	int i;
	addr pos, root;
	OptimizeType value;

	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		root = list;
		while (root != Nil) {
			GetCons(root, &pos, &root);
			GetEvalDeclareOptimize(pos, i, &value);
			if (0 <= value) {
				optimize[i] = value;
				break;
			}
		}
	}
}

static void eval_stack_global_heap(Execute ptr, addr *ret)
{
	addr pos, list;
	struct eval_stack *str;

	/* object */
	eval_stack_heap(&pos, EVAL_STACK_MODE_NIL);
	str = StructEvalStack(pos);
	str->globalp = 1;

	/* parse-declare */
	get_nocheck_parse_declare(ptr, &list);
	if (list != Unbound)
		eval_stack_global_optimize(str->optimize, list);

	*ret = pos;
}

int begin_eval_stack_(Execute ptr)
{
	addr symbol, stack;

	/* Global stack must be a heap object. Don't use local function.  */
	eval_stack_global_heap(ptr, &stack);
	getglobal_symbol(&symbol);
	pushspecial_control(ptr, symbol, stack);
	getstack_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
	/* new stack */
	return newstack_nil_(ptr, NULL);
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

void free_eval_stack(Execute ptr)
{
	setprotect_control(ptr, p_defun_free_eval_stack, Nil);
}

int globalp_stack_eval(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return StructEvalStack(pos)->globalp;
}

size_t increment_stack_eval(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return (StructEvalStack(pos)->lexical)++;
}

size_t getlexical_stack_eval(addr pos)
{
	Check(! eval_stack_p(pos), "type error");
	return StructEvalStack(pos)->lexical;
}

void getlexical_index_heap(addr stack, addr *ret)
{
	size_t size;

	size = getlexical_stack_eval(stack);
	if (size == 0)
		*ret = Nil;
	else
		index_heap(ret, size);
}

void init_eval_stack(void)
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

	/* special */
	getall_special_declare(declare, &pos);
	apply_pushnew_stack(local, stack, pos, CONSTANT_SYSTEM_TYPE_SPECIAL);
	/* inline, notinline */
	getall_inline_declare(declare, &pos);
	apply_plist_callname_stack(local, stack, pos, CONSTANT_SYSTEM_INLINE);
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

int apply_declaim_stack_(Execute ptr, addr declare)
{
	addr stack;

	if (declare == Nil)
		return 0;
	Return(getglobal_eval_(ptr, &stack));
	apply_declare_switch(NULL, stack, declare, 1);

	return 0;
}


/*
 *  declare
 */
void apply_declare_stack(LocalRoot local, addr stack, addr declare)
{
	if (declare == Nil)
		return;
	apply_declare_switch(local, stack, declare, 0);
}

/* for let* arguments */
static int apply_declare_global_p_(Execute ptr, addr symbol, addr list, int *ret)
{
	if (find_list_eq_unsafe(symbol, list))
		return Result(ret, 1); /* special */

	/* global */
	Return(getglobal_eval_(ptr, &list));
	GetEvalStackTable(list, &list);
	if (! GetPlistConst(list, SYSTEM_TYPE_SPECIAL, &list)) {
		if (find_list_eq_unsafe(symbol, list))
			return Result(ret, 1); /* special */
	}

	/* symbol */
	if (specialp_symbol(symbol))
		return Result(ret, 1); /* special */

	/* lexical */
	return Result(ret, 0); /* lexical */
}

static int apply_declare_symbol_stack_(Execute ptr, addr stack, addr symbol, addr list)
{
	int check;
	constindex index;
	addr key, table;

	Return(apply_declare_global_p_(ptr, symbol, list, &check));
	index = check?
		CONSTANT_SYSTEM_TYPE_SPECIAL:
		CONSTANT_SYSTEM_TYPE_LEXICAL;
	GetConstant(index, &key);
	GetEvalStackTable(stack, &table);
	if (pushnewplist_alloc(ptr->local, table, key, symbol, &table))
		SetEvalStackTable(stack, table);

	return 0;
}

static void apply_declare_dynamic_stack(LocalRoot local,
		addr stack, addr symbol, addr list)
{
	addr key, table;

	if (find_list_eq_unsafe(symbol, list)) {
		GetConst(SYSTEM_DYNAMIC_VALUE , &key);
		GetEvalStackTable(stack, &table);
		if (pushnewplist_alloc(local, table, key, symbol, &table))
			SetEvalStackTable(stack, table);
	}
}

static void apply_plistsymbol_stack(LocalRoot local,
		addr stack, addr symbol, addr list, constindex index)
{
	addr key, table;

	if (getplist(list, symbol, &list) == 0) {
		copylocal_object(local, &list, list);
		GetConstant(index, &key);
		GetEvalStackTable(stack, &table);
		if (setpplist_alloc(local, table, key, symbol, list, &table))
			SetEvalStackTable(stack, table);
	}
}

int apply_declare_value_stack_(Execute ptr, addr stack, addr symbol, addr declare)
{
	addr list;
	LocalRoot local;

	local = ptr->local;
	/* special */
	getall_special_declare(declare, &list);
	Return(apply_declare_symbol_stack_(ptr, stack, symbol, list));
	/* inline, notinline */
	if (declare == Nil)
		return 0;
	/* type */
	getall_type_value_declare(declare, &list);
	apply_plistsymbol_stack(local, stack, symbol, list, CONSTANT_SYSTEM_TYPE_VALUE);
	/* ftype */
	/* optimize */
	/* dynamic-extent value */
	getall_dynamic_value_declare(declare, &list);
	apply_declare_dynamic_stack(local, stack, symbol, list);
	/* dynamic-extent function */
	/* ignore, ignorable value */
	getall_ignore_value_declare(declare, &list);
	apply_plistsymbol_stack(local, stack, symbol, list, CONSTANT_SYSTEM_IGNORE_VALUE);
	/* ignore, ignorable function */
	/* declaration is proclamation only. */

	return 0;
}

int apply_declare_let_stack_(Execute ptr, addr stack, addr symbol, addr declare)
{
	getall_special_declare(declare, &declare);
	return apply_declare_symbol_stack_(ptr, stack, symbol, declare);
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

void apply_declare_function_stack(LocalRoot local, addr stack, addr call, addr declare)
{
	addr list;

	if (declare == Nil)
		return;
	/* inline, notinline */
	getall_inline_declare(declare, &list);
	apply_plistcall_stack(local, stack, call, list, CONSTANT_SYSTEM_INLINE);
	/* special */
	/* type */
	/* ftype */
	getall_type_function_declare(declare, &list);
	apply_plistcall_stack(local, stack, call, list, CONSTANT_SYSTEM_TYPE_FUNCTION);
	/* optimize */
	/* dynamic-extent value */
	/* dynamic-extent function */
	getall_dynamic_function_declare(declare, &list);
	apply_pushcall_stack(local, stack, call, list, CONSTANT_SYSTEM_DYNAMIC_FUNCTION);
	/* ignore, ignorable value */
	/* ignore, ignorable function */
	getall_ignore_function_declare(declare, &list);
	apply_plistcall_stack(local, stack, call, list, CONSTANT_SYSTEM_IGNORE_FUNCTION);
	/* declaration is proclamation only. */
}


/*
 *  table scope
 */
int getvalue_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! symbolp(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return getvalue_evaltable(stack, pos, ret);
}

void setvalue_scope_evalstack(addr stack, addr pos)
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

int getfunction_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! callnamep(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return getfunction_evaltable(stack, pos, ret);
}

void setfunction_scope_evalstack(addr stack, addr pos)
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

int gettagbody_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! tagbody_tag_p(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return gettagbody_evaltable(stack, pos, ret);
}

void settagbody_scope_evalstack(addr stack, addr pos)
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

int getblock_scope_evalstack(addr stack, addr pos, addr *ret)
{
	Check(! eval_stack_p(stack), "type error");
	Check(! symbolp(pos), "name error");
	GetEvalStackScope(stack, &stack);
	return getblock_evaltable(stack, pos, ret);
}

void setblock_scope_evalstack(addr stack, addr pos)
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
void setvalue_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableValue(pos);
	evaltable_value_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

void setfunction_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableFunction(pos);
	evaltable_function_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

void settagbody_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableTagBody(pos);
	evaltable_tagbody_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

void setblock_lexical_evalstack(addr stack, addr pos)
{
	addr list;

	Check(! eval_stack_p(stack), "type error");
	CheckTableBlock(pos);
	evaltable_block_heap(&pos, pos);
	GetEvalStackLexical(stack, &list);
	cons_heap(&list, pos, list);
	SetEvalStackLexical(stack, list);
}

int find_plistlist_evalstack(addr stack, addr key, addr symbol)
{
	Check(! eval_stack_p(stack), "type error");

	GetEvalStackTable(stack, &stack);
	return getplist(stack, key, &stack) == 0
		&& find_list_eq_unsafe(symbol, stack);
}

int find_special_evalstack(addr stack, addr symbol)
{
	addr key;

	Check(! eval_stack_p(stack), "type error");
	Check(! symbolp(symbol), "type error");
	GetConst(SYSTEM_TYPE_SPECIAL, &key);
	return find_plistlist_evalstack(stack, key, symbol);
}

int find_global_special_evalstack(addr stack, addr symbol, addr *ret)
{
	addr list, key, x, y;

	Check(! eval_stack_p(stack), "type error");
	Check(! symbolp(symbol), "type error");

	GetConst(COMMON_SPECIAL, &key);
	GetEvalStackTable(stack, &list);
	if (getplist(list, key, ret))
		goto not_found;

	while (list != Nil) {
		GetCons(list, &x, &list);
		getname_tablevalue(x, &y);
		if (y == symbol) {
			*ret = x;
			return 1;
		}
	}

not_found:
	*ret = Nil;
	return 0;
}

void push_global_special_evalstack(addr stack, addr value)
{
	addr list, key;

	Check(! eval_stack_p(stack), "type error");
	CheckTableValue(value);

	GetConst(COMMON_SPECIAL, &key);
	GetEvalStackTable(stack, &list);
	if (setplist_heap(list, key, value, &list))
		SetEvalStackTable(stack, list);
}

