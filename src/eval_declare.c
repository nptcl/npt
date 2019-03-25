#include "build.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "eval.h"
#include "eval_declare.h"
#include "eval_scope.h"
#include "function.h"
#include "object.h"
#include "sequence.h"
#include "strtype.h"
#include "symbol.h"
#include "type_copy.h"
#include "type_parse.h"
#include "type_symbol.h"
#include "type_table.h"

enum EVAL_DECLARE {
	EVAL_DECLARE_TYPE_VALUE,
	EVAL_DECLARE_TYPE_FUNCTION,
	EVAL_DECLARE_SPECIAL,
	EVAL_DECLARE_INLINE,
	EVAL_DECLARE_IGNORE_VALUE,
	EVAL_DECLARE_IGNORE_FUNCTION,
	EVAL_DECLARE_DYNAMIC_VALUE,
	EVAL_DECLARE_DYNAMIC_FUNCTION,
	EVAL_DECLARE_DECLARATION,
	EVAL_DECLARE_SIZE
};

#define RefEvalDeclare_Low      RefEval
#define GetEvalDeclare_Low      GetEval
#define SetEvalDeclare_Low      SetEval

#define PtrEvalDeclare_Low(p)	((OptimizeType *)PtrEvalBody(p, EVAL_DECLARE_SIZE))
#define RefEvalDeclareOptimize_Low(p,i)		(PtrEvalDeclare_Low(p)[i])
#define GetEvalDeclareOptimize_Low(p,i,v)	(*(v) = PtrEvalDeclare_Low(p)[i])
#define SetEvalDeclareOptimize_Low(p,i,v)	(PtrEvalDeclare_Low(p)[i] = (v))

#ifdef LISP_DECL
#define RefEvalDeclare          refevaldeclare
#define GetEvalDeclare          getevaldeclare
#define SetEvalDeclare          setevaldeclare
#define RefEvalDeclareOptimize	refevaldeclareoptimize
#define GetEvalDeclareOptimize	getevaldeclareoptimize
#define SetEvalDeclareOptimize	setevaldeclareoptimize
#else
#define RefEvalDeclare          RefEvalDeclare_Low
#define GetEvalDeclare          GetEvalDeclare_Low
#define SetEvalDeclare          SetEvalDeclare_Low
#define RefEvalDeclareOptimize	RefEvalDeclareOptimize_Low
#define GetEvalDeclareOptimize	GetEvalDeclareOptimize_Low
#define SetEvalDeclareOptimize	SetEvalDeclareOptimize_Low
#endif

#define DEFAULT_OPTIMIZE 1


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
		if (check != Nil) return 0;
	}

	/* optimize */
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++) {
		GetEvalDeclareOptimize(pos, i, &value);
		if (0 <= value) return 0;
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

	if (pos == Nil) return;
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
	CheckType(symbol, LISPTYPE_SYMBOL);
	CheckType(type, LISPTYPE_TYPE);
	GetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, &list);
	if (setplist_heap(list, symbol, type, &list))
		SetEvalDeclare(pos, EVAL_DECLARE_TYPE_VALUE, list);
}
static void push_type_declare_heap(addr pos, addr symbol, addr type)
{
	addr list, temp;

	Check(! eval_declare_p(pos), "type error");
	CheckType(symbol, LISPTYPE_SYMBOL);
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
		enum CONSTANT_INDEX constant,
		enum EVAL_DECLARE declare)
{
	addr list, value;

	Check(! eval_declare_p(pos), "type error");
	CheckType(symbol, LISPTYPE_SYMBOL);
	GetConstant(constant, &value);
	GetEvalDeclare(pos, declare, &list);
	if (setplist_heap(list, symbol, value, &list))
		SetEvalDeclare(pos, declare, list);
}

static void plist_callname_declare_heap(addr pos, addr callname,
		enum CONSTANT_INDEX constant,
		enum EVAL_DECLARE declare)
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
	CheckType(symbol, LISPTYPE_SYMBOL);
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

void push_declaration_declare_heap(addr pos, addr symbol)
{
	push_constant_declare_heap(pos, symbol, EVAL_DECLARE_DECLARATION);
}

static int check_constant_declare(addr pos, addr symbol, enum EVAL_DECLARE declare)
{
	Check(! eval_declare_p(pos), "type error");
	CheckType(symbol, LISPTYPE_SYMBOL);
	GetEvalDeclare(pos, declare, &pos);
	return find_list_eq_unsafe(symbol, pos);
}
static int check_declaration_declare(addr pos, addr symbol)
{
	return check_constant_declare(pos, symbol, EVAL_DECLARE_DECLARATION);
}

static OptimizeType get_index_optimize_declare(addr pos, enum EVAL_OPTIMIZE index)
{
	Check(! eval_declare_p(pos), "type error");
	return RefEvalDeclareOptimize(pos, index);
}

OptimizeType get_optimize_compilation_declare(addr pos)
{
	return get_index_optimize_declare(pos, EVAL_OPTIMIZE_COMPILATION);
}

OptimizeType get_optimize_debug_declare(addr pos)
{
	return get_index_optimize_declare(pos, EVAL_OPTIMIZE_DEBUG);
}

OptimizeType get_optimize_safety_declare(addr pos)
{
	return get_index_optimize_declare(pos, EVAL_OPTIMIZE_SAFETY);
}

OptimizeType get_optimize_space_declare(addr pos)
{
	return get_index_optimize_declare(pos, EVAL_OPTIMIZE_SPACE);
}

OptimizeType get_optimize_speed_declare(addr pos)
{
	return get_index_optimize_declare(pos, EVAL_OPTIMIZE_SPEED);
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
 *  build_eval_declare
 */
void getroot_declare(addr *ret)
{
	*ret = Root(LISPINDEX_DECLARE);
	Check(! eval_declare_p(*ret), "type error");
}

void setroot_declare(addr pos)
{
	Check(! eval_declare_p(pos), "type error");
	Root(LISPINDEX_DECLARE) = pos;
}

void build_eval_declare(void)
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
static void check_callname_heap(addr *ret, addr symbol)
{
	addr check;

	if (parse_callname_heap(&symbol, symbol))
		fmte("Invalid function name ~S.", symbol, NULL);
	GetCallName(symbol, &check);
	check_variable(check);
	*ret = symbol;
}

static int decl_type(Execute ptr, addr env, addr eval, addr cons)
{
	addr type, symbol;

	getcons(cons, &type, &cons);
	if (parse_type(ptr, &type, type, env))
		return 1;
	while (cons != Nil) {
		getcons(cons, &symbol, &cons);
		check_variable(symbol);
		push_type_declare_heap(eval, symbol, type);
	}

	return 0;
}

static int decl_ftype(Execute ptr, addr env, addr eval, addr cons)
{
	addr type, symbol;

	getcons(cons, &type, &cons);
	if (parse_type(ptr, &type, type, env))
		return 1;
	while (cons != Nil) {
		getcons(cons, &symbol, &cons);
		check_callname_heap(&symbol, symbol);
		push_ftype_declare_heap(eval, symbol, type);
	}

	return 0;
}

static void decl_special(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		getcons(cons, &symbol, &cons);
		check_variable(symbol);
		push_special_declare_heap(eval, symbol);
	}
}

static void decl_inline(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		getcons(cons, &symbol, &cons);
		check_callname_heap(&symbol, symbol);
		push_inline_declare_heap(eval, symbol);
	}
}

static void decl_notinline(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		getcons(cons, &symbol, &cons);
		check_callname_heap(&symbol, symbol);
		push_notinline_declare_heap(eval, symbol);
	}
}

static void decl_declaration(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		getcons(cons, &symbol, &cons);
		check_variable(symbol);
		push_declaration_declare_heap(eval, symbol);
	}
}

static int function_callname_p(addr *ret, addr pos)
{
	addr type, symbol, check;

	if (GetType(pos) != LISPTYPE_CONS) return 0;
	GetCons(pos, &type, &pos);
	if (GetType(pos) != LISPTYPE_CONS) return 0;
	GetCons(pos, &symbol, &pos);
	if (pos != Nil) return 0;
	GetConst(COMMON_FUNCTION, &check);
	if (check != type) return 0;
	check_callname_heap(ret, symbol);

	return 1;
}

static void decl_ignore(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		getcons(cons, &symbol, &cons);
		if (function_callname_p(&symbol, symbol)) {
			push_ignore_function_declare_heap(eval, symbol);
		}
		else {
			check_variable(symbol);
			push_ignore_value_declare_heap(eval, symbol);
		}
	}
}

static void decl_ignorable(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		getcons(cons, &symbol, &cons);
		if (function_callname_p(&symbol, symbol)) {
			push_ignorable_function_declare_heap(eval, symbol);
		}
		else {
			check_variable(symbol);
			push_ignorable_value_declare_heap(eval, symbol);
		}
	}
}

static void decl_dynamic_extent(addr eval, addr cons)
{
	addr symbol;

	while (cons != Nil) {
		getcons(cons, &symbol, &cons);
		if (function_callname_p(&symbol, symbol)) {
			push_dynamic_function_declare_heap(eval, symbol);
		}
		else {
			check_variable(symbol);
			push_dynamic_value_declare_heap(eval, symbol);
		}
	}
}

static enum EVAL_OPTIMIZE check_optimize_symbol(addr symbol)
{
	addr check;

	GetConst(COMMON_SAFETY, &check);
	if (check == symbol)
		return EVAL_OPTIMIZE_SAFETY;

	GetConst(COMMON_SPACE, &check);
	if (check == symbol)
		return EVAL_OPTIMIZE_SPACE;

	GetConst(COMMON_SPEED, &check);
	if (check == symbol)
		return EVAL_OPTIMIZE_SPEED;

	GetConst(COMMON_COMPILATION_SPEED, &check);
	if (check == symbol)
		return EVAL_OPTIMIZE_COMPILATION;

	GetConst(COMMON_DEBUG, &check);
	if (check == symbol)
		return EVAL_OPTIMIZE_DEBUG;

	fmte("Invalid optimize symbol ~S.", symbol, NULL);
	return EVAL_OPTIMIZE_SIZE;
}

static void decl_optimize_symbol(addr eval, addr symbol)
{
	int index;
	index = (int)check_optimize_symbol(symbol);
	SetEvalDeclareOptimize(eval, index, 3);
}

static void decl_optimize_cons(addr eval, addr cons)
{
	int index;
	addr symbol, value;
	fixnum check;

	getcons(cons, &symbol, &cons);
	if (GetType(symbol) != LISPTYPE_SYMBOL)
		fmte("The optimize type ~S must be a symbol.", symbol, NULL);
	index = (int)check_optimize_symbol(symbol);

	/* (speed) -> (speed 3) */
	if (cons == Nil) {
		SetEvalDeclareOptimize(eval, index, 3);
		return;
	}

	/* (speed x) */
	getcons(cons, &value, &cons);
	if (cons != Nil)
		fmte("Invalid optimize argument ~S.", cons, NULL);
	if (GetType(value) != LISPTYPE_FIXNUM)
		fmte("The optimize value ~S must be a fixnum.", value, NULL);
	GetFixnum(value, &check);
	if (check < 0 || 3 < check)
		fmte("The optimize value ~S must be between 0 and 3.", value, NULL);
	SetEvalDeclareOptimize(eval, index, (int)check);
}

static void decl_optimize(addr eval, addr cons)
{
	addr one;

	while (cons != Nil) {
		getcons(cons, &one, &cons);
		switch (GetType(one)) {
			case LISPTYPE_SYMBOL:
				decl_optimize_symbol(eval, one);
				break;

			case LISPTYPE_CONS:
				decl_optimize_cons(eval, one);
				break;

			default:
				fmte("Invalid optimize argument ~S.", one, NULL);
				break;
		}
	}
}

static int declaration_p(addr eval, addr symbol)
{
	if (check_declaration_declare(eval, symbol)) return 1;
	getroot_declare(&eval);
	return check_declaration_declare(eval, symbol);
}

static int decl_otherwise(Execute ptr, addr env, addr eval, addr type, addr cons)
{
	addr symbol;

	/* declaration */
	if (declaration_p(eval, type)) {
		/* do nothing */
		return 0;
	}

	/* type */
	if (type_symbol_p(type)) {
		if (parse_type(ptr, &type, type, env))
			return 1;
		while (cons != Nil) {
			getcons(cons, &symbol, &cons);
			check_variable(symbol);
			push_type_declare_heap(eval, symbol, type);
		}
		return 0;
	}

	/* Not implementation */
	fmtw("Declaration ~S is not implemented.", type, NULL);
	return 0;
}

static int push_declaim(Execute ptr, addr env, addr eval, addr symbol, addr cons)
{
	addr check;

	GetConst(COMMON_TYPE, &check);
	if (check == symbol)
		return decl_type(ptr, env, eval, cons);
	GetConst(COMMON_FTYPE, &check);
	if (check == symbol)
		return decl_ftype(ptr, env, eval, cons);
	GetConst(COMMON_SPECIAL, &check);
	if (check == symbol) {
		decl_special(eval, cons);
		return 0;
	}
	GetConst(COMMON_INLINE, &check);
	if (check == symbol) {
		decl_inline(eval, cons);
		return 0;
	}
	GetConst(COMMON_NOTINLINE, &check);
	if (check == symbol) {
		decl_notinline(eval, cons);
		return 0;
	}
	GetConst(COMMON_DECLARATION, &check);
	if (check == symbol) {
		decl_declaration(eval, cons);
		return 0;
	}
	GetConst(COMMON_OPTIMIZE, &check);
	if (check == symbol) {
		decl_optimize(eval, cons);
		return 0;
	}

	/* error/otherwise */
	GetConst(COMMON_DYNAMIC_EXTENT, &check);
	if (check == symbol) {
		fmte("dynamic-extent don't allow in the declaim/proclaim form.", NULL);
		return 0;
	}
	GetConst(COMMON_IGNORE, &check);
	if (check == symbol) {
		fmte("ignore don't allow in the declaim/proclaim form.", NULL);
		return 0;
	}
	GetConst(COMMON_IGNORABLE, &check);
	if (check == symbol) {
		fmte("ignorable don't allow in the declaim/proclaim form.", NULL);
		return 0;
	}

	return decl_otherwise(ptr, env, eval, symbol, cons);
}

static int push_declare(Execute ptr, addr env, addr eval, addr symbol, addr cons)
{
	addr check;

	GetConst(COMMON_TYPE, &check);
	if (check == symbol)
		return decl_type(ptr, env, eval, cons);
	GetConst(COMMON_FTYPE, &check);
	if (check == symbol)
		return decl_ftype(ptr, env, eval, cons);
	GetConst(COMMON_SPECIAL, &check);
	if (check == symbol) {
		decl_special(eval, cons);
		return 0;
	}
	GetConst(COMMON_INLINE, &check);
	if (check == symbol) {
		decl_inline(eval, cons);
		return 0;
	}
	GetConst(COMMON_NOTINLINE, &check);
	if (check == symbol) {
		decl_notinline(eval, cons);
		return 0;
	}
	GetConst(COMMON_IGNORE, &check);
	if (check == symbol) {
		decl_ignore(eval, cons);
		return 0;
	}
	GetConst(COMMON_IGNORABLE, &check);
	if (check == symbol) {
		decl_ignorable(eval, cons);
		return 0;
	}
	GetConst(COMMON_DYNAMIC_EXTENT, &check);
	if (check == symbol) {
		decl_dynamic_extent(eval, cons);
		return 0;
	}
	GetConst(COMMON_OPTIMIZE, &check);
	if (check == symbol) {
		decl_optimize(eval, cons);
		return 0;
	}

	/* error/otherwise */
	GetConst(COMMON_DECLARATION, &check);
	if (check == symbol) {
		fmte("declaration don't allow in the declare form.", NULL);
		return 0;
	}
	return decl_otherwise(ptr, env, eval, symbol, cons);
}

static int parse_declare_form(Execute ptr, addr env, addr decl, addr *ret,
		int (*call)(Execute, addr ,addr, addr, addr))
{
	addr eval, car, tail;

	eval_declare_heap(&eval);
	while (decl != Nil) {
		getcons(decl, &car, &decl);
		getcons(car, &car, &tail);
		if (! IsSymbol(car))
			TypeError(car, SYMBOL);
		if (call(ptr, env, eval, car, tail))
			return 1;
	}
	*ret = eval;

	return 0;
}

int parse_declaim_heap(Execute ptr, addr env, addr decl, addr *ret)
{
	return parse_declare_form(ptr, env, decl, ret, push_declaim);
}

int parse_declare_heap(Execute ptr, addr env, addr decl, addr *ret)
{
	return parse_declare_form(ptr, env, decl, ret, push_declare);
}


/*
 *  declare_body_documentation
 */
static void declare_split(addr cons, addr *retdecl, addr *retbody)
{
	addr decl, car, cdr, declare;

	GetConst(COMMON_DECLARE, &declare);
	for (decl = Nil; cons != Nil; ) {
		getcar(cons, &cdr);
		if (GetType(cdr) != LISPTYPE_CONS) break;
		getcons(cdr, &car, &cdr);
		if (car != declare) break;
		while (cdr != Nil) {
			getcons(cdr, &car, &cdr);
			cons_heap(&decl, car, decl);
		}
		getcdr(cons, &cons);
	}
	nreverse_list_unsafe(retdecl, decl);
	*retbody = cons;
}

void declare_body_form(addr list, addr *retdecl, addr *retbody)
{
	addr declare, decl, car, cdr;

	GetConst(COMMON_DECLARE, &declare);
	for (decl = Nil; list != Nil; ) {
		getcar(list, &cdr);
		if (! consp(cdr)) break;
		getcar(cdr, &car);
		if (car != declare) break;
		cons_heap(&decl, cdr, decl);
		getcdr(list, &list);
	}
	nreverse_list_unsafe(retdecl, decl);
	*retbody = list;
}

int declare_body(Execute ptr, addr env, addr cons, addr *retdecl, addr *retbody)
{
	addr decl;
	declare_split(cons, &decl, retbody);
	if (decl != Nil) {
		return parse_declare_heap(ptr, env, decl, retdecl);
	}
	else {
		*retdecl = Nil;
		return 0;
	}
}

int declare_body_documentation(Execute ptr, addr env,
		addr cons, addr *rdoc, addr *rdecl, addr *rbody)
{
	addr car, cdr;

	/* nil */
	if (cons == Nil) {
		*rdoc = *rdecl = *rbody = Nil;
		return 0;
	}

	/* (body) */
	getcons(cons, &car, &cdr);
	if (cdr == Nil) {
		*rdoc = Nil;
		return declare_body(ptr, env, cons, rdecl, rbody);
	}

	/* (doc . body) */
	if (stringp(car)) {
		*rdoc = car;
		return declare_body(ptr, env, cdr, rdecl, rbody);
	}

	/* (decl . nil) */
	if (declare_body(ptr, env, cons, rdecl, &cdr)) {
		return 1;
	}
	if (cdr == Nil) {
		*rdoc = *rbody = Nil;
		return 0;
	}

	/* ([decl] doc . body) */
	getcons(cdr, &car, &cons);
	if (stringp(car)) {
		*rdoc = car;
		*rbody = cons;
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
	nreverse_list_unsafe(&root, root);
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
	nreverse_list_unsafe(&root, root);
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
	nreverse_list_unsafe(&root, root);
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
	nreverse_list_unsafe(&root, root);
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
	nreverse_list_unsafe(&root, root);
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
	nreverse_list_unsafe(&root, root);
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

