#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "eval_code.h"
#include "eval_declare.h"
#include "eval_scope.h"
#include "eval_table.h"
#include "function.h"
#include "optimize.h"
#include "optimize_common.h"
#include "strtype.h"
#include "type_typep.h"

static OptimizeType get_optimize_scope(addr scope, enum EVAL_OPTIMIZE index)
{
	OptimizeType value;
	addr root;

	value = StructEvalScope(scope)->optimize[index];
	if (0 <= value)
		return value;
	getroot_declare(&root);
	return get_optimize_declare(root, index);
}

static int optimize_common_p(addr scope)
{
	OptimizeType value = get_optimize_scope(scope, EVAL_OPTIMIZE_SPEED);
	return 1 <= value;
}


/*
 *  car
 */
_g void optcode_car0_set(Execute ptr, addr pos)
{
	getresult_control(ptr, &pos);
	GetCar(pos, &pos);
	setresult_control(ptr, pos);
}

_g void optcode_car0_push(Execute ptr, addr pos)
{
	getresult_control(ptr, &pos);
	GetCar(pos, &pos);
	pushargs_control(ptr, pos);
}

_g void optcode_car1_set(Execute ptr, addr type)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return0(typep_clang(ptr, pos, type, &check));
	if (! check)
		type_error(pos, type);
	GetCar(pos, &pos);
	setresult_control(ptr, pos);
}

_g void optcode_car1_push(Execute ptr, addr type)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return0(typep_clang(ptr, pos, type, &check));
	if (! check)
		type_error(pos, type);
	GetCar(pos, &pos);
	pushargs_control(ptr, pos);
}

static int optimize_common_car0(LocalRoot local, addr code, addr pos)
{
	/* no check */
	getvalue_tablecall(pos, &pos);
	eval_code_execute_set(local, code, pos);
	EvalCode_single_sp(local, code, CAR0_SET, CAR0_PUSH);

	return 1;
}

static int optimize_common_car1(LocalRoot local, addr code, addr pos)
{
	addr type;

	/* type check */
	gettype_tablecall(pos, &type);
	getvalue_tablecall(pos, &pos);
	eval_code_execute_set(local, code, pos);
	EvalCode_carcdr_sp(local, code, CAR1_SET, CAR1_PUSH, type);

	return 1;
}

static int optimize_common_car(LocalRoot local, addr code, addr scope)
{
	addr args, pos;

	if (! optimize_common_p(scope))
		return 0;
	/* first argument */
	GetEvalScopeIndex(scope, 1, &args);
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (args != Nil)
		return 0;
	Check(! eval_tablecall_p(pos), "type error");
	if (! getcheck_tablecall(pos))
		return optimize_common_car0(local, code, pos);
	else
		return optimize_common_car1(local, code, pos);
}


/*
 *  optimize-common
 */
static void optimize_common_symbol(addr scope, addr *ret)
{
	addr call;

	GetEvalScopeIndex(scope, 0, &call); /* first */
	Check(RefEvalScopeType(call) != EVAL_PARSE_FUNCTION, "type error");
	GetEvalScopeValue(call, &call);
	getname_tablefunction(call, &call);
	Check(RefCallNameType(call) != CALLNAME_SYMBOL, "callname error");
	GetCallName(call, ret);
}

_g int optimize_common(LocalRoot local, addr code, addr scope)
{
	addr symbol, check;

	optimize_common_symbol(scope, &symbol);
	/* car */
	GetConst(COMMON_CAR, &check);
	if (symbol == check)
		return optimize_common_car(local, code, scope);

	return 0;
}


/*
 *  optimize-check code
 */
static int optimize_check_code1(LocalRoot local, addr code, addr scope, addr pos)
{
	Check(! eval_tablecall_p(pos), "type error");
	getvalue_tablecall(pos, &pos);
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeValue(pos, &pos);
	if (! string_designer_equal_char(pos, "SCOPE"))
		return 0;

	/* fixnum */
	fixnum_heap(&pos, (fixnum)optimize_common_p(scope));
	eval_code_object(local, code, pos);
	return 1;
}

static int optimize_check_code2(
		LocalRoot local, addr code, addr scope, addr pos, addr args)
{
	addr list, symbol, value;

	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (args != Nil)
		return 0;
	Check(! eval_tablecall_p(pos), "type error");
	getvalue_tablecall(pos, &pos);
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeValue(pos, &pos);
	if (! string_designer_equal_char(pos, "LIST"))
		return 0;

	list = Nil;
	/* compilation-speed */
	GetConst(COMMON_COMPILATION_SPEED, &symbol);
	fixnum_heap(&value, (fixnum)get_optimize_scope(scope, EVAL_OPTIMIZE_COMPILATION));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* debug */
	GetConst(COMMON_DEBUG, &symbol);
	fixnum_heap(&value, (fixnum)get_optimize_scope(scope, EVAL_OPTIMIZE_DEBUG));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* safety */
	GetConst(COMMON_SAFETY, &symbol);
	fixnum_heap(&value, (fixnum)get_optimize_scope(scope, EVAL_OPTIMIZE_SAFETY));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* space */
	GetConst(COMMON_SPACE, &symbol);
	fixnum_heap(&value, (fixnum)get_optimize_scope(scope, EVAL_OPTIMIZE_SPACE));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* speed */
	GetConst(COMMON_SPEED, &symbol);
	fixnum_heap(&value, (fixnum)get_optimize_scope(scope, EVAL_OPTIMIZE_SPEED));
	cons_heap(&value, symbol, value);
	cons_heap(&list, value, list);
	/* object */
	eval_code_object(local, code, list);

	return 1;
}

_g int optimize_check_code(LocalRoot local, addr code, addr scope)
{
	addr args, pos;

	/* (lisp-system:optimize-check scope) */
	GetEvalScopeIndex(scope, 1, &args);
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (args == Nil)
		return optimize_check_code1(local, code, scope, pos);
	else
		return optimize_check_code2(local, code, scope, pos, args);
}


/*
 *  initialize
 */
#define SetPointer_common(x) SetPointer_code(p_##x, x)
_g void init_optimize_common(void)
{
	SetPointer_common(optcode_car0_set);
	SetPointer_common(optcode_car0_push);
	SetPointer_common(optcode_car1_set);
	SetPointer_common(optcode_car1_push);
}

_g void build_optimize_common(void)
{
	defcode_constant(CONSTANT_CODE_CAR0_SET, p_optcode_car0_set);
	defcode_constant(CONSTANT_CODE_CAR0_PUSH, p_optcode_car0_push);
	defcode_constant(CONSTANT_CODE_CAR1_SET, p_optcode_car1_set);
	defcode_constant(CONSTANT_CODE_CAR1_PUSH, p_optcode_car1_push);
}

