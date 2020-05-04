#include "code_function.h"
#include "code_make.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control_operator.h"
#include "declare.h"
#include "eval_table.h"
#include "function.h"
#include "optimize.h"
#include "optimize_common.h"
#include "scope_object.h"
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
 *  result-type
 */
static int optcode_result_type(Execute ptr, addr type)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang(ptr, pos, type, &check));
	if (! check)
		type_error(pos, type);
	
	return 0;
}


/*
 *  car
 */
static int optcode_car0_set(Execute ptr, addr pos)
{
	getresult_control(ptr, &pos);
	GetCar(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_car0_push(Execute ptr, addr pos)
{
	getresult_control(ptr, &pos);
	GetCar(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optcode_car1_set(Execute ptr, addr type)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang(ptr, pos, type, &check));
	if (! check)
		type_error(pos, type);
	GetCar(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_car1_push(Execute ptr, addr type)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang(ptr, pos, type, &check));
	if (! check)
		type_error(pos, type);
	GetCar(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optimize_common_car0(LocalRoot local, addr code, addr pos)
{
	/* no check */
	getvalue_tablecall(pos, &pos);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			eval_code_execute_set(local, code, pos);
			EvalCode_single(local, code, CAR0_SET);
			break;

		case EvalCode_ModePush:
			eval_code_execute_set(local, code, pos);
			EvalCode_single(local, code, CAR0_PUSH);
			break;

		case EvalCode_ModeRemove:
		default:
			eval_code_execute_rem(local, code, pos);
			break;
	}

	return 1;
}

static int optimize_common_car1(LocalRoot local, addr code, addr pos)
{
	addr type;

	/* type check */
	gettype_tablecall(pos, &type);
	getvalue_tablecall(pos, &pos);
	eval_code_execute_set(local, code, pos);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			EvalCode_carcdr(local, code, CAR1_SET, type);
			break;

		case EvalCode_ModePush:
			EvalCode_carcdr(local, code, CAR1_PUSH, type);
			break;

		case EvalCode_ModeRemove:
		default:
			EvalCode_carcdr(local, code, RESULT_TYPE, type);
			break;
	}

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
 *  cdr
 */
static int optcode_cdr0_set(Execute ptr, addr pos)
{
	getresult_control(ptr, &pos);
	GetCdr(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_cdr0_push(Execute ptr, addr pos)
{
	getresult_control(ptr, &pos);
	GetCdr(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optcode_cdr1_set(Execute ptr, addr type)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang(ptr, pos, type, &check));
	if (! check)
		type_error(pos, type);
	GetCdr(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_cdr1_push(Execute ptr, addr type)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang(ptr, pos, type, &check));
	if (! check)
		type_error(pos, type);
	GetCdr(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optimize_common_cdr0(LocalRoot local, addr code, addr pos)
{
	/* no check */
	getvalue_tablecall(pos, &pos);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			eval_code_execute_set(local, code, pos);
			EvalCode_single(local, code, CDR0_SET);
			break;

		case EvalCode_ModePush:
			eval_code_execute_set(local, code, pos);
			EvalCode_single(local, code, CDR0_PUSH);
			break;

		case EvalCode_ModeRemove:
		default:
			eval_code_execute_rem(local, code, pos);
			break;
	}

	return 1;
}

static int optimize_common_cdr1(LocalRoot local, addr code, addr pos)
{
	addr type;

	/* type check */
	gettype_tablecall(pos, &type);
	getvalue_tablecall(pos, &pos);
	eval_code_execute_set(local, code, pos);
	switch (evalcode_mode(code)) {
		case EvalCode_ModeSet:
			EvalCode_carcdr(local, code, CDR1_SET, type);
			break;

		case EvalCode_ModePush:
			EvalCode_carcdr(local, code, CDR1_PUSH, type);
			break;

		case EvalCode_ModeRemove:
		default:
			EvalCode_carcdr(local, code, RESULT_TYPE, type);
			break;
	}

	return 1;
}

static int optimize_common_cdr(LocalRoot local, addr code, addr scope)
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
		return optimize_common_cdr0(local, code, pos);
	else
		return optimize_common_cdr1(local, code, pos);
}


/*
 *  cons
 */
static int optcode_cons(Execute ptr, addr list)
{
	addr car, cdr;

	getargs_list_control_unsafe(ptr, 0, &list);
	GetCons(list, &car, &list);
	GetCar(list, &cdr);
	cons_heap(&list, car, cdr);
	setresult_control(ptr, list);

	return 0;
}

static int optimize_common_cons0(LocalRoot local, addr code, addr car, addr cdr)
{
	addr pos;

	getvalue_tablecall(car, &car);
	getvalue_tablecall(cdr, &cdr);
	/* return begin */
	evalcode_push_return(local, code);
	eval_code_execute_push(local, code, car);
	eval_code_execute_push(local, code, cdr);
	/* cons */
	EvalCode_single(local, code, CONS);
	/* return end */
	evalcode_pop(local, code, &pos);
	EvalCode_carcdr(local, code, EXECUTE, pos);
	evalcode_ifpush(local, code);

	return 1;
}

static int optimize_common_cons_rem(LocalRoot local, addr code, addr car, addr cdr)
{
	getvalue_tablecall(car, &car);
	getvalue_tablecall(cdr, &cdr);
	eval_code_execute(local, code, car);
	eval_code_execute(local, code, cdr);

	return 1;
}

static int optimize_common_cons(LocalRoot local, addr code, addr scope)
{
	addr args, car, cdr;

	if (! optimize_common_p(scope))
		return 0;
	/* first argument */
	GetEvalScopeIndex(scope, 1, &args);
	if (! consp_getcons(args, &car, &args))
		return 0;
	if (args == Nil)
		return 0;
	if (! consp_getcons(args, &cdr, &args))
		return 0;
	if (args != Nil)
		return 0;
	Check(! eval_tablecall_p(car), "type error");
	Check(! eval_tablecall_p(cdr), "type error");
	if (! evalcode_remp(code))
		return optimize_common_cons0(local, code, car, cdr);
	else
		return optimize_common_cons_rem(local, code, car, cdr);
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
	/* cdr */
	GetConst(COMMON_CDR, &check);
	if (symbol == check)
		return optimize_common_cdr(local, code, scope);
	/* cons */
	GetConst(COMMON_CONS, &check);
	if (symbol == check)
		return optimize_common_cons(local, code, scope);

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
	SetPointer_common(optcode_result_type);
	SetPointer_common(optcode_car0_set);
	SetPointer_common(optcode_car0_push);
	SetPointer_common(optcode_car1_set);
	SetPointer_common(optcode_car1_push);
	SetPointer_common(optcode_cdr0_set);
	SetPointer_common(optcode_cdr0_push);
	SetPointer_common(optcode_cdr1_set);
	SetPointer_common(optcode_cdr1_push);
	SetPointer_common(optcode_cons);
}

_g void build_optimize_common(void)
{
	defcode_constant(CONSTANT_CODE_RESULT_TYPE, p_optcode_result_type);
	defcode_constant(CONSTANT_CODE_CAR0_SET, p_optcode_car0_set);
	defcode_constant(CONSTANT_CODE_CAR0_PUSH, p_optcode_car0_push);
	defcode_constant(CONSTANT_CODE_CAR1_SET, p_optcode_car1_set);
	defcode_constant(CONSTANT_CODE_CAR1_PUSH, p_optcode_car1_push);
	defcode_constant(CONSTANT_CODE_CDR0_SET, p_optcode_cdr0_set);
	defcode_constant(CONSTANT_CODE_CDR0_PUSH, p_optcode_cdr0_push);
	defcode_constant(CONSTANT_CODE_CDR1_SET, p_optcode_cdr1_set);
	defcode_constant(CONSTANT_CODE_CDR1_PUSH, p_optcode_cdr1_push);
	defcode_constant(CONSTANT_CODE_CONS, p_optcode_cons);
}

