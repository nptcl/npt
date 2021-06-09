#include "build.h"
#include "callname.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "declare.h"
#include "eval_object.h"
#include "function.h"
#include "hold.h"
#include "object.h"
#include "parse.h"
#include "scope.h"
#include "sequence.h"
#include "strtype.h"
#include "symbol.h"
#include "type_copy.h"
#include "type_parse.h"
#include "type_symbol.h"
#include "type_table.h"


/*
 *  declaration object
 */
static void make_eval_declare(LocalRoot local, addr *ret)
{
	eval_alloc(local, ret, EVAL_TYPE_DECLARE, EVAL_DECLARE_SIZE,
			sizeoft(OptimizeType) * EVAL_OPTIMIZE_SIZE);
}
static void eval_declare_alloc_optimize(LocalRoot local, addr *ret, OptimizeType value)
{
	int i;
	addr pos;

	make_eval_declare(local, &pos);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		SetEvalDeclareOptimize(pos, i, value);
	*ret = pos;
}
void eval_declare_alloc(LocalRoot local, addr *ret)
{
	eval_declare_alloc_optimize(local, ret, -1);
}
void eval_declare_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	eval_declare_alloc(local, ret);
}
void eval_declare_heap(addr *ret)
{
	eval_declare_alloc(NULL, ret);
}

int empty_declare(addr pos)
{
	int i;
	addr check;
	OptimizeType value;

	Check(! eval_declare_p(pos), "type error");
	/* array */
	for (i = 0; i < EVAL_DECLARE_SIZE; i++) {
		GetEvalDeclare(pos, i, &check);
		if (check != Nil)
			return 0;
	}

	/* optimize */
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		GetEvalDeclareOptimize(pos, i, &value);
		if (0 <= value)
			return 0;
	}

	return 1;
}
int empty_nil_declare(addr pos)
{
	return pos == Nil || (eval_declare_p(pos) && empty_declare(pos));
}

void apply_array_declare(OptimizeType *array, addr pos)
{
	int i;
	OptimizeType value;

	if (pos == Nil)
		return;
	Check(! eval_declare_p(pos), "type error");
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		GetEvalDeclareOptimize(pos, i, &value);
		if (0 <= value)
			array[i] = value;
	}
}

addr refevaldeclare(addr pos, size_t index)
{
	Check(! eval_declare_p(pos), "type error");
	return RefEvalDeclare_Low(pos, index);
}
void getevaldeclare(addr pos, size_t index, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare_Low(pos, index, ret);
}
void setevaldeclare(addr pos, size_t index, addr value)
{
	Check(! eval_declare_p(pos), "type error");
	SetEvalDeclare_Low(pos, index, value);
}
OptimizeType refevaldeclareoptimize(addr pos, size_t index)
{
	Check(! eval_declare_p(pos), "type error");
	return RefEvalDeclareOptimize_Low(pos, index);
}
void getevaldeclareoptimize(addr pos, size_t index, OptimizeType *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclareOptimize_Low(pos, index, ret);
}
void setevaldeclareoptimize(addr pos, size_t index, OptimizeType value)
{
	Check(! eval_declare_p(pos), "type error");
	SetEvalDeclareOptimize_Low(pos, index, value);
}


/*
 *  access
 */
static void set_type_declare_heap(addr pos, addr symbol, addr type)
{
	addr list;

	Check(! eval_declare_p(pos), "type error");
	CheckSymbol(symbol);
	CheckType(type, LISPTYPE_TYPE);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, &list);
	if (setplist_heap(list, symbol, type, &list))
		SetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, list);
}
static void push_type_declare_heap(addr pos, addr symbol, addr type)
{
	addr list, temp;

	Check(! eval_declare_p(pos), "type error");
	CheckSymbol(symbol);
	CheckType(type, LISPTYPE_TYPE);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, &list);

	if (getplist(list, symbol, &temp)) {
		/* don't exist */
		set_type_declare_heap(pos, symbol, type);
	}
	else {
		/* already exists */
		type2and_heap(temp, type, &type);
		set_type_declare_heap(pos, symbol, type);
	}
}

static void set_ftype_declare_heap(addr pos, addr callname, addr type)
{
	addr list;

	Check(! eval_declare_p(pos), "type error");
	CheckType(callname, LISPTYPE_CALLNAME);
	CheckType(type, LISPTYPE_TYPE);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, &list);
	if (setplist_callname_heap(list, callname, type, &list))
		SetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, list);
}
static void push_ftype_declare_heap(addr pos, addr callname, addr type)
{
	addr list, temp;

	Check(! eval_declare_p(pos), "type error");
	CheckType(callname, LISPTYPE_CALLNAME);
	CheckType(type, LISPTYPE_TYPE);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, &list);

	if (getplist_callname(list, callname, &temp)) {
		/* don't exist */
		set_ftype_declare_heap(pos, callname, type);
	}
	else {
		/* already exists */
		type2and_heap(temp, type, &type);
		set_ftype_declare_heap(pos, callname, type);
	}
}

static void plist_constant_declare_heap(addr pos, addr symbol,
		constindex constant, enum EVAL_DECLARE declare)
{
	addr list, value;

	Check(! eval_declare_p(pos), "type error");
	CheckSymbol(symbol);
	GetConstant(constant, &value);
	GetEvalDeclare(pos, declare, &list);
	if (setplist_heap(list, symbol, value, &list))
		SetEvalDeclare(pos, declare, list);
}

static void plist_callname_declare_heap(addr pos, addr callname,
		constindex constant, enum EVAL_DECLARE declare)
{
	addr list, value;

	Check(! eval_declare_p(pos), "type error");
	CheckType(callname, LISPTYPE_CALLNAME);
	GetConstant(constant, &value);
	GetEvalDeclare(pos, declare, &list);
	if (setplist_callname_heap(list, callname, value, &list))
		SetEvalDeclare(pos, declare, list);
}

static void push_inline_declare_heap(addr pos, addr callname)
{
	plist_callname_declare_heap(pos, callname,
			CONSTANT_COMMON_INLINE,
			EVAL_DECLARE_INLINE);
}

static void push_notinline_declare_heap(addr pos, addr callname)
{
	plist_callname_declare_heap(pos, callname,
			CONSTANT_COMMON_NOTINLINE,
			EVAL_DECLARE_INLINE);
}

static void push_ignore_value_declare_heap(addr pos, addr symbol)
{
	plist_constant_declare_heap(pos, symbol,
			CONSTANT_COMMON_IGNORE,
			EVAL_DECLARE_IGNORE_VALUE);
}

static void push_ignorable_value_declare_heap(addr pos, addr symbol)
{
	plist_constant_declare_heap(pos, symbol,
			CONSTANT_COMMON_IGNORABLE,
			EVAL_DECLARE_IGNORE_VALUE);
}

static void push_ignore_function_declare_heap(addr pos, addr callname)
{
	plist_callname_declare_heap(pos, callname,
			CONSTANT_COMMON_IGNORE,
			EVAL_DECLARE_IGNORE_FUNCTION);
}

static void push_ignorable_function_declare_heap(addr pos, addr callname)
{
	plist_callname_declare_heap(pos, callname,
			CONSTANT_COMMON_IGNORABLE,
			EVAL_DECLARE_IGNORE_FUNCTION);
}

static void push_constant_declare_heap(addr pos, addr symbol,
		enum EVAL_DECLARE declare)
{
	addr list;

	Check(! eval_declare_p(pos), "type error");
	CheckSymbol(symbol);
	GetEvalDeclare(pos, declare, &list);
	if (pushnew_heap(list, symbol, &list))
		SetEvalDeclare(pos, declare, list);
}
static void push_special_declare_heap(addr pos, addr symbol)
{
	push_constant_declare_heap(pos, symbol, EVAL_DECLARE_SPECIAL);
}
static void push_dynamic_value_declare_heap(addr pos, addr symbol)
{
	push_constant_declare_heap(pos, symbol, EVAL_DECLARE_DYNAMIC_VALUE);
}

static void push_callname_declare_heap(addr pos, addr callname,
		enum EVAL_DECLARE declare)
{
	addr list;

	Check(! eval_declare_p(pos), "type error");
	CheckType(callname, LISPTYPE_CALLNAME);
	GetEvalDeclare(pos, declare, &list);
	if (pushnewlist_callname_heap(list, callname, &list))
		SetEvalDeclare(pos, declare, list);
}
static void push_dynamic_function_declare_heap(addr pos, addr callname)
{
	push_callname_declare_heap(pos, callname, EVAL_DECLARE_DYNAMIC_FUNCTION);
}

static void push_declaration_declare_heap(addr pos, addr symbol)
{
	push_constant_declare_heap(pos, symbol, EVAL_DECLARE_DECLARATION);
}

static int check_constant_declare(addr pos, addr symbol, enum EVAL_DECLARE declare)
{
	Check(! eval_declare_p(pos), "type error");
	if (! symbolp(symbol))
		return 0;
	GetEvalDeclare(pos, declare, &pos);
	return find_list_eq_unsafe(symbol, pos);
}
static int check_declaration_declare(addr pos, addr symbol)
{
	return check_constant_declare(pos, symbol, EVAL_DECLARE_DECLARATION);
}

OptimizeType get_optimize_declare(addr pos, enum EVAL_OPTIMIZE index)
{
	Check(! eval_declare_p(pos), "type error");
	return RefEvalDeclareOptimize(pos, index);
}

OptimizeType get_optimize_compilation_declare(addr pos)
{
	return get_optimize_declare(pos, EVAL_OPTIMIZE_COMPILATION);
}

OptimizeType get_optimize_debug_declare(addr pos)
{
	return get_optimize_declare(pos, EVAL_OPTIMIZE_DEBUG);
}

OptimizeType get_optimize_safety_declare(addr pos)
{
	return get_optimize_declare(pos, EVAL_OPTIMIZE_SAFETY);
}

OptimizeType get_optimize_space_declare(addr pos)
{
	return get_optimize_declare(pos, EVAL_OPTIMIZE_SPACE);
}

OptimizeType get_optimize_speed_declare(addr pos)
{
	return get_optimize_declare(pos, EVAL_OPTIMIZE_SPEED);
}


/*
 *  getall
 */
void getall_declaration_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_DECLARATION, ret);
}

void getall_inline_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_INLINE, ret);
}

void getall_special_declare(addr pos, addr *ret)
{
	if (pos == Nil) {
		*ret = Nil;
		return;
	}

	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, ret);
}

void getall_type_value_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, ret);
}

void getall_type_function_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, ret);
}

const OptimizeType *getall_optimize_declare(addr pos)
{
	Check(! eval_declare_p(pos), "type error");
	return PtrEvalDeclare_Low(pos);
}

void getall_dynamic_value_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_DYNAMIC_VALUE, ret);
}

void getall_dynamic_function_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_DYNAMIC_FUNCTION, ret);
}

void getall_ignore_value_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_IGNORE_VALUE, ret);
}

void getall_ignore_function_declare(addr pos, addr *ret)
{
	Check(! eval_declare_p(pos), "type error");
	GetEvalDeclare(pos, EVAL_DECLARE_IGNORE_FUNCTION, ret);
}


/*
 *  build_declare
 */
void getroot_declare(addr *ret)
{
	*ret = LispRoot(DECLARE);
	Check(! eval_declare_p(*ret), "type error");
}

void setroot_declare(addr pos)
{
	Check(! eval_declare_p(pos), "type error");
	SetLispRoot(DECLARE, pos);
}

void build_declare(void)
{
	addr pos;
	eval_declare_alloc_optimize(NULL, &pos, 1);
	setroot_declare(pos);
}

void push_declaration_declaim(addr symbol)
{
	addr eval;
	getroot_declare(&eval);
	push_declaration_declare_heap(eval, symbol);
}

void copy_optimize_declare(OptimizeType *array)
{
	int i;
	addr pos;
	OptimizeType value;

	getroot_declare(&pos);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		GetEvalDeclareOptimize(pos, i, &value);
		array[i] = value;
	}
}

static void apply_optimize_declaim(enum EVAL_OPTIMIZE index, OptimizeType value)
{
	addr pos;
	getroot_declare(&pos);
	SetEvalDeclareOptimize(pos, index, value);
}
void apply_compilation_speed_declaim(OptimizeType value)
{
	apply_optimize_declaim(EVAL_OPTIMIZE_COMPILATION, value);
}
void apply_debug_declaim(OptimizeType value)
{
	apply_optimize_declaim(EVAL_OPTIMIZE_DEBUG, value);
}
void apply_safety_declaim(OptimizeType value)
{
	apply_optimize_declaim(EVAL_OPTIMIZE_SAFETY, value);
}
void apply_space_declaim(OptimizeType value)
{
	apply_optimize_declaim(EVAL_OPTIMIZE_SPACE, value);
}
void apply_speed_declaim(OptimizeType value)
{
	apply_optimize_declaim(EVAL_OPTIMIZE_SPEED, value);
}


/*
 *  parse-declaration
 */
static int check_callname_heap_(addr *ret, addr symbol)
{
	addr check;

	Return(parse_callname_error_(&symbol, symbol));
	GetCallName(symbol, &check);
	Return(check_variable_(check));

	return Result(ret, symbol);
}

static int decl_type_(Execute ptr, addr env, addr eval, addr cons)
{
	addr type, symbol;

	Return_getcons(cons, &type, &cons);
	Return(parse_type(ptr, &type, type, env));
	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_variable_(symbol));
		push_type_declare_heap(eval, symbol, type);
	}

	return 0;
}

static int decl_ftype_(Execute ptr, addr env, addr eval, addr form)
{
	addr cons, type, symbol;

	if (! consp_getcons(form, &type, &cons))
		return fmte_("Invalid ftype form, ~S.", form, NULL);
	Return(parse_type(ptr, &type, type, env));
	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_callname_heap_(&symbol, symbol));
		push_ftype_declare_heap(eval, symbol, type);
	}

	return 0;
}

static int decl_special_(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_variable_(symbol));
		push_special_declare_heap(eval, symbol);
	}

	return 0;
}

static int decl_inline_(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_callname_heap_(&symbol, symbol));
		push_inline_declare_heap(eval, symbol);
	}

	return 0;
}

static int decl_notinline_(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_callname_heap_(&symbol, symbol));
		push_notinline_declare_heap(eval, symbol);
	}

	return 0;
}

static int decl_declaration_(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_variable_(symbol));
		push_declaration_declare_heap(eval, symbol);
	}

	return 0;
}

static int function_callname_p_(addr *value, addr pos, int *ret)
{
	addr type, symbol, check;

	if (GetType(pos) != LISPTYPE_CONS)
		return Result(ret, 0);
	GetCons(pos, &type, &pos);
	if (GetType(pos) != LISPTYPE_CONS)
		return Result(ret, 0);
	GetCons(pos, &symbol, &pos);
	if (pos != Nil)
		return Result(ret, 0);
	GetConst(COMMON_FUNCTION, &check);
	if (check != type)
		return Result(ret, 0);
	Return(check_callname_heap_(value, symbol));

	return Result(ret, 1);
}

static int decl_ignore_(addr eval, addr cons)
{
	int check;
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(function_callname_p_(&symbol, symbol, &check));
		if (check) {
			push_ignore_function_declare_heap(eval, symbol);
		}
		else {
			Return(check_variable_(symbol));
			push_ignore_value_declare_heap(eval, symbol);
		}
	}

	return 0;
}

static int decl_ignorable_(addr eval, addr cons)
{
	int check;
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(function_callname_p_(&symbol, symbol, &check));
		if (check) {
			push_ignorable_function_declare_heap(eval, symbol);
		}
		else {
			Return(check_variable_(symbol));
			push_ignorable_value_declare_heap(eval, symbol);
		}
	}

	return 0;
}

static int decl_dynamic_extent_(addr eval, addr cons)
{
	int check;
	addr symbol;

	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(function_callname_p_(&symbol, symbol, &check));
		if (check) {
			push_dynamic_function_declare_heap(eval, symbol);
		}
		else {
			Return(check_variable_(symbol));
			push_dynamic_value_declare_heap(eval, symbol);
		}
	}

	return 0;
}

static int check_optimize_symbol_(addr symbol, enum EVAL_OPTIMIZE *ret, int *ignore)
{
	addr check;

	*ignore = 0;
	GetConst(COMMON_SAFETY, &check);
	if (check == symbol)
		return Result(ret, EVAL_OPTIMIZE_SAFETY);

	GetConst(COMMON_SPACE, &check);
	if (check == symbol)
		return Result(ret, EVAL_OPTIMIZE_SPACE);

	GetConst(COMMON_SPEED, &check);
	if (check == symbol)
		return Result(ret, EVAL_OPTIMIZE_SPEED);

	GetConst(COMMON_COMPILATION_SPEED, &check);
	if (check == symbol)
		return Result(ret, EVAL_OPTIMIZE_COMPILATION);

	GetConst(COMMON_DEBUG, &check);
	if (check == symbol)
		return Result(ret, EVAL_OPTIMIZE_DEBUG);

	*ignore = 1;
	*ret = EVAL_OPTIMIZE_SIZE;
	return fmtw_("Invalid optimize symbol ~S.", symbol, NULL);
}

static int decl_optimize_symbol_(addr eval, addr symbol)
{
	int ignore;
	enum EVAL_OPTIMIZE index;

	Return(check_optimize_symbol_(symbol, &index, &ignore));
	if (ignore)
		return 0;
	SetEvalDeclareOptimize(eval, (int)index, 3);

	return 0;
}

static int decl_optimize_cons_(addr eval, addr cons)
{
	int ignore;
	enum EVAL_OPTIMIZE index;
	addr symbol, value;
	fixnum check;

	Return_getcons(cons, &symbol, &cons);
	if (GetType(symbol) != LISPTYPE_SYMBOL)
		return fmte_("The optimize type ~S must be a symbol.", symbol, NULL);
	Return(check_optimize_symbol_(symbol, &index, &ignore));
	if (ignore)
		return 0;

	/* (speed) -> (speed 3) */
	if (cons == Nil) {
		SetEvalDeclareOptimize(eval, (int)index, 3);
		return 0;
	}

	/* (speed x) */
	Return_getcons(cons, &value, &cons);
	if (cons != Nil)
		return fmte_("Invalid optimize argument ~S.", cons, NULL);
	if (GetType(value) != LISPTYPE_FIXNUM)
		return fmte_("The optimize value ~S must be a fixnum.", value, NULL);
	GetFixnum(value, &check);
	if (check < 0 || 3 < check)
		return fmte_("The optimize value ~S must be between 0 and 3.", value, NULL);
	SetEvalDeclareOptimize(eval, (int)index, (int)check);

	return 0;
}

static int decl_optimize_(addr eval, addr cons)
{
	addr one;

	while (cons != Nil) {
		Return_getcons(cons, &one, &cons);
		switch (GetType(one)) {
			case LISPTYPE_SYMBOL:
				Return(decl_optimize_symbol_(eval, one));
				break;

			case LISPTYPE_CONS:
				Return(decl_optimize_cons_(eval, one));
				break;

			default:
				return fmte_("Invalid optimize argument ~S.", one, NULL);
		}
	}

	return 0;
}

static int declaration_p(addr eval, addr symbol)
{
	if (check_declaration_declare(eval, symbol))
		return 1;
	getroot_declare(&eval);
	return check_declaration_declare(eval, symbol);
}

static int decl_otherwise_(Execute ptr, addr env, addr eval, addr type, addr cons)
{
	addr symbol;

	/* declaration */
	if (declaration_p(eval, type)) {
		/* do nothing */
		return 0;
	}

	/* (type ...) */
	Return(parse_type(ptr, &type, type, env));
	while (cons != Nil) {
		Return_getcons(cons, &symbol, &cons);
		Return(check_variable_(symbol));
		push_type_declare_heap(eval, symbol, type);
	}

	return 0;
}

static int push_declaim_(Execute ptr, addr env, addr eval, addr symbol, addr cons)
{
	addr check;

	/* type */
	GetConst(COMMON_TYPE, &check);
	if (check == symbol)
		return decl_type_(ptr, env, eval, cons);

	/* ftype */
	GetConst(COMMON_FTYPE, &check);
	if (check == symbol)
		return decl_ftype_(ptr, env, eval, cons);

	/* special */
	GetConst(COMMON_SPECIAL, &check);
	if (check == symbol)
		return decl_special_(eval, cons);

	/* inline */
	GetConst(COMMON_INLINE, &check);
	if (check == symbol)
		return decl_inline_(eval, cons);

	/* notinline */
	GetConst(COMMON_NOTINLINE, &check);
	if (check == symbol)
		return decl_notinline_(eval, cons);

	/* declaration */
	GetConst(COMMON_DECLARATION, &check);
	if (check == symbol)
		return decl_declaration_(eval, cons);

	/* optimize */
	GetConst(COMMON_OPTIMIZE, &check);
	if (check == symbol)
		return decl_optimize_(eval, cons);

	/* error/otherwise */
	GetConst(COMMON_DYNAMIC_EXTENT, &check);
	if (check == symbol)
		return fmte_("dynamic-extent don't allow in the declaim/proclaim form.", NULL);

	GetConst(COMMON_IGNORE, &check);
	if (check == symbol)
		return fmte_("ignore don't allow in the declaim/proclaim form.", NULL);

	GetConst(COMMON_IGNORABLE, &check);
	if (check == symbol)
		return fmte_("ignorable don't allow in the declaim/proclaim form.", NULL);

	return decl_otherwise_(ptr, env, eval, symbol, cons);
}

static int push_declare_(Execute ptr, addr env, addr eval, addr symbol, addr cons)
{
	addr check;

	/* type */
	GetConst(COMMON_TYPE, &check);
	if (check == symbol)
		return decl_type_(ptr, env, eval, cons);

	/* ftype */
	GetConst(COMMON_FTYPE, &check);
	if (check == symbol)
		return decl_ftype_(ptr, env, eval, cons);

	/* special */
	GetConst(COMMON_SPECIAL, &check);
	if (check == symbol)
		return decl_special_(eval, cons);

	/* inline */
	GetConst(COMMON_INLINE, &check);
	if (check == symbol)
		return decl_inline_(eval, cons);

	/* notinline */
	GetConst(COMMON_NOTINLINE, &check);
	if (check == symbol)
		return decl_notinline_(eval, cons);

	/* ignore */
	GetConst(COMMON_IGNORE, &check);
	if (check == symbol)
		return decl_ignore_(eval, cons);

	/* ignorable */
	GetConst(COMMON_IGNORABLE, &check);
	if (check == symbol)
		return decl_ignorable_(eval, cons);

	/* dynamic-extent */
	GetConst(COMMON_DYNAMIC_EXTENT, &check);
	if (check == symbol)
		return decl_dynamic_extent_(eval, cons);

	/* optimize */
	GetConst(COMMON_OPTIMIZE, &check);
	if (check == symbol)
		return decl_optimize_(eval, cons);

	/* error/otherwise */
	GetConst(COMMON_DECLARATION, &check);
	if (check == symbol)
		return fmte_("declaration don't allow in the declare form.", NULL);

	return decl_otherwise_(ptr, env, eval, symbol, cons);
}

static int parse_declare_form_(Execute ptr, addr env, addr decl, addr *ret,
		int (*call_)(Execute, addr ,addr, addr, addr))
{
	addr eval, car, tail;
	LocalHold hold;

	eval_declare_heap(&eval);
	hold = LocalHold_local(ptr);
	localhold_pushva(hold, eval, decl, NULL);
	while (decl != Nil) {
		Return_getcons(decl, &car, &decl);
		Return_getcons(car, &car, &tail);
		Return((*call_)(ptr, env, eval, car, tail));
	}
	localhold_end(hold);

	return Result(ret, eval);
}

int parse_declaim_heap_(Execute ptr, addr env, addr decl, addr *ret)
{
	return parse_declare_form_(ptr, env, decl, ret, push_declaim_);
}

int parse_declare_heap_(Execute ptr, addr env, addr decl, addr *ret)
{
	return parse_declare_form_(ptr, env, decl, ret, push_declare_);
}

int parse_optimize_heap_(addr decl, addr *value, int *ret)
{
	addr eval, pos, car, tail, optimize;

	eval_declare_heap(&eval);
	GetConst(COMMON_OPTIMIZE, &optimize);
	while (decl != Nil) {
		Return_getcons(decl, &pos, &decl);
		Return_getcons(pos, &car, &tail);
		if (car != optimize)
			return Result(ret, 1);
		Return(decl_optimize_(eval, tail));
	}
	*value = eval;
	return Result(ret, 0);
}


/*
 *  declare_body_documentation
 */
static int declare_split_(addr cons, addr *retdecl, addr *retbody)
{
	addr decl, car, cdr, declare;

	GetConst(COMMON_DECLARE, &declare);
	for (decl = Nil; cons != Nil; ) {
		Return_getcar(cons, &cdr);
		if (GetType(cdr) != LISPTYPE_CONS)
			break;
		Return_getcons(cdr, &car, &cdr);
		if (car != declare)
			break;
		while (cdr != Nil) {
			Return_getcons(cdr, &car, &cdr);
			cons_heap(&decl, car, decl);
		}
		Return_getcdr(cons, &cons);
	}
	nreverse(retdecl, decl);
	*retbody = cons;
	return 0;
}

int declare_body_form_(addr list, addr *retdecl, addr *retbody)
{
	addr declare, decl, car, cdr;

	GetConst(COMMON_DECLARE, &declare);
	for (decl = Nil; list != Nil; ) {
		Return_getcar(list, &cdr);
		if (! consp(cdr))
			break;
		Return_getcar(cdr, &car);
		if (car != declare)
			break;
		cons_heap(&decl, cdr, decl);
		Return_getcdr(list, &list);
	}
	nreverse(retdecl, decl);
	*retbody = list;
	return 0;
}

int declare_body_(Execute ptr, addr env, addr cons, addr *retdecl, addr *retbody)
{
	addr decl;

	Return(declare_split_(cons, &decl, retbody));
	if (decl != Nil)
		return parse_declare_heap_(ptr, env, decl, retdecl);
	*retdecl = Nil;
	return 0;
}

int declare_body_documentation_(Execute ptr, addr env,
		addr cons, addr *rdoc, addr *rdecl, addr *rbody)
{
	addr car, cdr;

	/* nil */
	if (cons == Nil) {
		*rdoc = *rdecl = *rbody = Nil;
		return 0;
	}

	/* (body) */
	Return_getcons(cons, &car, &cdr);
	if (cdr == Nil) {
		*rdoc = Nil;
		return declare_body_(ptr, env, cons, rdecl, rbody);
	}

	/* (doc . body) */
	if (stringp(car)) {
		*rdoc = car;
		return declare_body_(ptr, env, cdr, rdecl, rbody);
	}

	/* (decl . nil) */
	Return(declare_body_(ptr, env, cons, rdecl, &cdr));
	if (cdr == Nil) {
		*rdoc = *rbody = Nil;
		return 0;
	}

	/* ([decl] doc . body) */
	Return_getcons(cdr, &car, &cons);
	if (stringp(car)) {
		if (cons == Nil) {
			*rdoc = Nil;
			*rbody = cdr;
		}
		else {
			*rdoc = car;
			*rbody = cons;
		}
		return 0;
	}

	/* ([decl] . body) */
	*rdoc = Nil;
	*rbody = cdr;
	return 0;
}

int split_decl_body_doc_(addr list, addr *rdoc, addr *rdecl, addr *rbody)
{
	addr car, cdr;

	/* nil */
	if (list == Nil) {
		*rdoc = *rdecl = *rbody = Nil;
		return 0;
	}

	/* (body) */
	Return_getcons(list, &car, &cdr);
	if (cdr == Nil) {
		*rdoc = Nil;
		return declare_split_(list, rdecl, rbody);
	}

	/* (doc . body) */
	if (stringp(car)) {
		*rdoc = car;
		return declare_split_(cdr, rdecl, rbody);
	}

	/* (decl . nil) */
	Return(declare_split_(list, rdecl, &cdr));
	if (cdr == Nil) {
		*rdoc = *rbody = Nil;
		return 0;
	}

	/* ([decl] doc . body) */
	Return_getcons(cdr, &car, &list);
	if (stringp(car)) {
		if (list == Nil) {
			*rdoc = Nil;
			*rbody = cdr;
		}
		else {
			*rdoc = car;
			*rbody = list;
		}
		return 0;
	}

	/* ([decl] . body) */
	*rdoc = Nil;
	*rbody = cdr;
	return 0;
}


/*
 *  copy eval-declare
 */
static void copy_declare_type_v(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, key, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &key, &list);
		GetCons(list, &value, &list);
		type_copy_alloc(local, &value, value);
		cons_alloc(local, &root, key, root);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

static void copy_declare_type_f(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, key, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &key, &list);
		GetCons(list, &value, &list);
		copy_callname_alloc(local, &key, key);
		type_copy_alloc(local, &value, value);
		cons_alloc(local, &root, key, root);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

static void copy_declare_push_v(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &value, &list);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

static void copy_declare_push_f(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &value, &list);
		copy_callname_alloc(local, &value, value);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

static void copy_declare_plist_v(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, key, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &key, &list);
		GetCons(list, &value, &list);
		cons_alloc(local, &root, key, root);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

static void copy_declare_plist_f(LocalRoot local,
		enum EVAL_DECLARE type, addr pos, addr eval)
{
	addr list, key, value, root;

	GetEvalDeclare(pos, type, &list);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &key, &list);
		GetCons(list, &value, &list);
		copy_callname_alloc(local, &key, key);
		cons_alloc(local, &root, key, root);
		cons_alloc(local, &root, value, root);
	}
	nreverse(&root, root);
	SetEvalDeclare(eval, type, root);
}

void copy_eval_declare_alloc(LocalRoot local, addr *ret, addr pos)
{
	int i;
	addr eval;
	OptimizeType value;

	make_eval_declare(local, &eval);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		GetEvalDeclareOptimize(pos, i, &value);
		SetEvalDeclareOptimize(eval, i, value);
	}
	copy_declare_type_v(local, EVAL_DECLARE_TYPE_VALUE, pos, eval);
	copy_declare_type_f(local, EVAL_DECLARE_TYPE_FUNCTION, pos, eval);
	copy_declare_push_v(local, EVAL_DECLARE_SPECIAL, pos, eval);
	copy_declare_plist_f(local, EVAL_DECLARE_INLINE, pos, eval);
	copy_declare_plist_v(local, EVAL_DECLARE_IGNORE_VALUE, pos, eval);
	copy_declare_plist_f(local, EVAL_DECLARE_IGNORE_FUNCTION, pos, eval);
	copy_declare_push_v(local, EVAL_DECLARE_DYNAMIC_VALUE, pos, eval);
	copy_declare_push_f(local, EVAL_DECLARE_DYNAMIC_FUNCTION, pos, eval);
	copy_declare_push_v(local, EVAL_DECLARE_DECLARATION, pos, eval);
	*ret = eval;
}

void copy_eval_declare_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	copy_eval_declare_alloc(local, ret, pos);
}

void copy_eval_declare_heap(addr *ret, addr pos)
{
	copy_eval_declare_alloc(NULL, ret, pos);
}


/*
 *  debug
 */
static void set_index_optimize_declare(addr pos,
		OptimizeType value, enum EVAL_OPTIMIZE index)
{
	Check(! eval_declare_p(pos), "type error");
	Check(! (0 <= value && value <= 3), "range error");
	SetEvalDeclareOptimize(pos, index, value);
}
void set_optimize_compilation_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_COMPILATION);
}
void set_optimize_debug_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_DEBUG);
}
void set_optimize_safety_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_SAFETY);
}
void set_optimize_space_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_SPACE);
}
void set_optimize_speed_declare(addr pos, OptimizeType value)
{
	set_index_optimize_declare(pos, value, EVAL_OPTIMIZE_SPEED);
}


/*
 *  proclaim
 */
static int parse_proclaim_heap_(Execute ptr, addr env, addr car, addr *ret)
{
	addr eval, tail;
	LocalHold hold;

	eval_declare_heap(&eval);
	hold = LocalHold_local_push(ptr, eval);
	Return_getcons(car, &car, &tail);
	Return(push_declaim_(ptr, env, eval, car, tail));
	localhold_end(hold);

	return Result(ret, eval);
}

static int apply_type_value_proclaim_(addr pos)
{
	addr key, value;

	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, &pos);
	while (pos != Nil) {
		GetCons(pos, &key, &pos);
		GetCons(pos, &value, &pos);
		CheckSymbol(key);
		CheckType(value, LISPTYPE_TYPE);
		Return(settype_value_symbol_(key, value));
	}

	return 0;
}

static int apply_type_function_proclaim_(addr pos)
{
	addr key, value, symbol;

	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_FUNCTION, &pos);
	while (pos != Nil) {
		GetCons(pos, &key, &pos);
		GetCons(pos, &value, &pos);
		CheckType(key, LISPTYPE_CALLNAME);
		CheckType(value, LISPTYPE_TYPE);

		GetCallName(key, &symbol);
		if (symbolp_callname(key)) {
			Return(settype_function_symbol_(symbol, value));
		}
		else {
			Return(settype_setf_symbol_(symbol, value));
		}
	}

	return 0;
}

static int apply_special_proclaim_(addr pos)
{
	addr symbol;

	GetEvalDeclare(pos, EVAL_DECLARE_SPECIAL, &pos);
	while (pos != Nil) {
		GetCons(pos, &symbol, &pos);
		CheckSymbol(symbol);
		Return(setspecial_symbol_(symbol));
	}

	return 0;
}

static void apply_inline_value_proclaim(addr key, addr value)
{
	addr check, symbol;

	GetCallName(key, &symbol);
	/* inline */
	GetConst(COMMON_INLINE, &check);
	if (check == value) {
		if (symbolp_callname(key))
			setinline_function_symbol(symbol);
		else
			setinline_setf_symbol(symbol);
	}
	/* notinline */
	GetConst(COMMON_NOTINLINE, &check);
	if (check == value) {
		if (symbolp_callname(key))
			setnotinline_function_symbol(symbol);
		else
			setnotinline_setf_symbol(symbol);
	}
}

static void apply_inline_proclaim(addr pos)
{
	addr key, value;

	GetEvalDeclare(pos, EVAL_DECLARE_INLINE, &pos);
	while (pos != Nil) {
		GetCons(pos, &key, &pos);
		GetCons(pos, &value, &pos);
		CheckType(key, LISPTYPE_CALLNAME);
		apply_inline_value_proclaim(key, value);
	}
}

static void apply_optimize_proclaim(addr pos)
{
	int i;
	OptimizeType value;

	for (i = 0; i < (int)EVAL_OPTIMIZE_SIZE; i++) {
		GetEvalDeclareOptimize(pos, i, &value);
		if (0 <= value)
			apply_optimize_declaim((enum EVAL_OPTIMIZE)i, value);
	}
}

static void apply_declaration_proclaim(addr pos)
{
	addr symbol;

	GetEvalDeclare(pos, EVAL_DECLARE_DECLARATION, &pos);
	while (pos != Nil) {
		GetCons(pos, &symbol, &pos);
		CheckSymbol(symbol);
		push_declaration_declaim(symbol);
	}
}

int proclaim_common(Execute ptr, addr var)
{
	Return(parse_proclaim_heap_(ptr, Nil, var, &var));
	Return(apply_type_value_proclaim_(var));
	Return(apply_type_function_proclaim_(var));
	Return(apply_special_proclaim_(var));
	apply_inline_proclaim(var);
	apply_optimize_proclaim(var);
	apply_declaration_proclaim(var);

	return 0;
}

