#include "callname.h"
#include "code_init.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control_operator.h"
#include "declare.h"
#include "eval_object.h"
#include "eval_table.h"
#include "function.h"
#include "make.h"
#include "make_queue.h"
#include "optimize.h"
#include "optimize_common.h"
#include "scope_object.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
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
	OptimizeType value;
	value = get_optimize_scope(scope, EVAL_OPTIMIZE_SPEED);
	return 1 <= value;
}


/*
 *  result-type
 */
static int optcode_result_type_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang_(ptr, pos, x.pos, &check));
	if (! check)
		return call_type_error_(ptr, pos, x.pos);

	return 0;
}


/*
 *  car
 */
static int optcode_car0_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	GetCar(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_car0_push_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	GetCar(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optcode_car1_set_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang_(ptr, pos, x.pos, &check));
	if (! check)
		return call_type_error_(ptr, pos, x.pos);
	GetCar(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_car1_push_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang_(ptr, pos, x.pos, &check));
	if (! check)
		return call_type_error_(ptr, pos, x.pos);
	GetCar(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optimize_common_car0_(CodeMake ptr, addr pos, int *ret)
{
	/* no check */
	getvalue_tablecall(pos, &pos);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			Return(code_make_execute_set_(ptr, pos));
			CodeQueue_single(ptr, OPTCODE_CAR0_SET);
			break;

		case CodeQueue_ModePush:
			Return(code_make_execute_set_(ptr, pos)); /* set */
			CodeQueue_single(ptr, OPTCODE_CAR0_PUSH);
			break;

		case CodeQueue_ModeRemove:
		default:
			Return(code_make_execute_rem_(ptr, pos));
			break;
	}

	return Result(ret, 1);
}

static int optimize_common_car1_(CodeMake ptr, addr pos, int *ret)
{
	addr type;

	/* type check */
	gettype_tablecall(pos, &type);
	getvalue_tablecall(pos, &pos);
	Return(code_make_execute_set_(ptr, pos));
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(ptr, OPTCODE_CAR1_SET, type);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(ptr, OPTCODE_CAR1_PUSH, type);
			break;

		case CodeQueue_ModeRemove:
		default:
			CodeQueue_cons(ptr, OPTCODE_RESULT_TYPE, type);
			break;
	}

	return Result(ret, 1);
}

static int optimize_common_car_(CodeMake ptr, addr scope, int *ret)
{
	addr args, pos;

	if (! optimize_common_p(scope))
		return Result(ret, 0);
	/* first argument */
	GetEvalScopeIndex(scope, 1, &args);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (args != Nil)
		return Result(ret, 0);
	Check(! eval_tablecall_p(pos), "type error");
	if (! getcheck_tablecall(pos))
		return optimize_common_car0_(ptr, pos, ret);
	else
		return optimize_common_car1_(ptr, pos, ret);
}


/*
 *  cdr
 */
static int optcode_cdr0_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	GetCdr(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_cdr0_push_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	GetCdr(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optcode_cdr1_set_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang_(ptr, pos, x.pos, &check));
	if (! check)
		return call_type_error_(ptr, pos, x.pos);
	GetCdr(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int optcode_cdr1_push_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;

	getresult_control(ptr, &pos);
	Return(typep_clang_(ptr, pos, x.pos, &check));
	if (! check)
		return call_type_error_(ptr, pos, x.pos);
	GetCdr(pos, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int optimize_common_cdr0_(CodeMake ptr, addr pos, int *ret)
{
	/* no check */
	getvalue_tablecall(pos, &pos);
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			Return(code_make_execute_set_(ptr, pos));
			CodeQueue_single(ptr, OPTCODE_CDR0_SET);
			break;

		case CodeQueue_ModePush:
			Return(code_make_execute_set_(ptr, pos));
			CodeQueue_single(ptr, OPTCODE_CDR0_PUSH);
			break;

		case CodeQueue_ModeRemove:
		default:
			Return(code_make_execute_rem_(ptr, pos));
			break;
	}

	return Result(ret, 1);
}

static int optimize_common_cdr1_(CodeMake ptr, addr pos, int *ret)
{
	addr type;

	/* type check */
	gettype_tablecall(pos, &type);
	getvalue_tablecall(pos, &pos);
	Return(code_make_execute_set_(ptr, pos));
	switch (code_queue_mode(ptr)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(ptr, OPTCODE_CDR1_SET, type);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(ptr, OPTCODE_CDR1_PUSH, type);
			break;

		case CodeQueue_ModeRemove:
		default:
			CodeQueue_cons(ptr, OPTCODE_RESULT_TYPE, type);
			break;
	}

	return Result(ret, 1);
}

static int optimize_common_cdr_(CodeMake ptr, addr scope, int *ret)
{
	addr args, pos;

	if (! optimize_common_p(scope))
		return Result(ret, 0);
	/* first argument */
	GetEvalScopeIndex(scope, 1, &args);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (args != Nil)
		return Result(ret, 0);
	Check(! eval_tablecall_p(pos), "type error");
	if (! getcheck_tablecall(pos))
		return optimize_common_cdr0_(ptr, pos, ret);
	else
		return optimize_common_cdr1_(ptr, pos, ret);
}


/*
 *  cons
 */
static int optcode_cons_code(Execute ptr, CodeValue x)
{
	addr list, car, cdr;

	getargs_list_control_unsafe(ptr, 0, &list);
	GetCons(list, &car, &list);
	GetCar(list, &cdr);
	cons_heap(&list, car, cdr);
	setresult_control(ptr, list);

	return 0;
}

static int optimize_common_cons0_(CodeMake ptr, addr car, addr cdr, int *ret)
{
	addr pos;

	getvalue_tablecall(car, &car);
	getvalue_tablecall(cdr, &cdr);
	/* return begin */
	code_queue_push_new(ptr);
	Return(code_make_execute_push_(ptr, car));
	Return(code_make_execute_push_(ptr, cdr));
	/* cons */
	CodeQueue_single(ptr, OPTCODE_CONS);
	/* return end */
	code_queue_pop(ptr, &pos);
	code_make_execute_control(ptr, pos);

	return Result(ret, 1);
}

static int optimize_common_cons_rem_(CodeMake ptr, addr car, addr cdr, int *ret)
{
	getvalue_tablecall(car, &car);
	getvalue_tablecall(cdr, &cdr);
	Return(code_make_execute_(ptr, car));
	Return(code_make_execute_(ptr, cdr));

	return Result(ret, 1);
}

static int optimize_common_cons_(CodeMake ptr, addr scope, int *ret)
{
	addr args, car, cdr;

	if (! optimize_common_p(scope))
		return Result(ret, 0);
	/* first argument */
	GetEvalScopeIndex(scope, 1, &args);
	if (! consp_getcons(args, &car, &args))
		return Result(ret, 0);
	if (args == Nil)
		return Result(ret, 0);
	if (! consp_getcons(args, &cdr, &args))
		return Result(ret, 0);
	if (args != Nil)
		return Result(ret, 0);
	Check(! eval_tablecall_p(car), "type error");
	Check(! eval_tablecall_p(cdr), "type error");
	if (! code_queue_remp(ptr))
		return optimize_common_cons0_(ptr, car, cdr, ret);
	else
		return optimize_common_cons_rem_(ptr, car, cdr, ret);
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

int optimize_common_(CodeMake ptr, addr scope, int *ret)
{
	addr symbol, check;

	optimize_common_symbol(scope, &symbol);
	/* car */
	GetConst(COMMON_CAR, &check);
	if (symbol == check)
		return optimize_common_car_(ptr, scope, ret);
	/* cdr */
	GetConst(COMMON_CDR, &check);
	if (symbol == check)
		return optimize_common_cdr_(ptr, scope, ret);
	/* cons */
	GetConst(COMMON_CONS, &check);
	if (symbol == check)
		return optimize_common_cons_(ptr, scope, ret);

	return Result(ret, 0);
}


/*
 *  optimize-check code
 */
static int optimize_check_code1_(CodeMake ptr, addr scope, addr pos, int *ret)
{
	Check(! eval_tablecall_p(pos), "type error");
	getvalue_tablecall(pos, &pos);
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeValue(pos, &pos);
	if (! strvect_designer_equalp_char(pos, "SCOPE"))
		return Result(ret, 0);

	/* fixnum */
	fixnum_heap(&pos, (fixnum)optimize_common_p(scope));
	code_make_object(ptr, pos);

	return Result(ret, 1);
}

static int optimize_check_code2_(CodeMake ptr,
		addr scope, addr pos, addr args, int *ret)
{
	addr list, symbol, value;

	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (args != Nil)
		return Result(ret, 0);
	Check(! eval_tablecall_p(pos), "type error");
	getvalue_tablecall(pos, &pos);
	Check(! eval_scope_p(pos), "type error");
	GetEvalScopeValue(pos, &pos);
	if (! strvect_designer_equalp_char(pos, "LIST"))
		return Result(ret, 0);

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
	code_make_object(ptr, list);

	return Result(ret, 1);
}

int optimize_check_code_(CodeMake ptr, addr scope, int *ret)
{
	addr args, pos;

	/* (lisp-system:optimize-check scope) */
	GetEvalScopeIndex(scope, 1, &args);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (args == Nil)
		return optimize_check_code1_(ptr, scope, pos, ret);
	else
		return optimize_check_code2_(ptr, scope, pos, args, ret);
}


/*
 *  initialize
 */
#define defcode(x,y) defcode_constant(CONSTANT_CODE_##x, p_##y)
#define initcode(x,y) { \
	SetPointer_code(p_##x, x); \
	CodeValueArray[p_##x] = (byte)CodeValueType_##y; \
}

void init_optimize_common(void)
{
	initcode(optcode_result_type_code,  Addr);
	initcode(optcode_car0_set_code,     Null);
	initcode(optcode_car0_push_code,    Null);
	initcode(optcode_car1_set_code,     Addr);
	initcode(optcode_car1_push_code,    Addr);
	initcode(optcode_cdr0_set_code,     Null);
	initcode(optcode_cdr0_push_code,    Null);
	initcode(optcode_cdr1_set_code,     Addr);
	initcode(optcode_cdr1_push_code,    Addr);
	initcode(optcode_cons_code,         Null);
}

void build_optimize_common(void)
{
	defcode(OPTCODE_RESULT_TYPE,  optcode_result_type_code);
	defcode(OPTCODE_CAR0_SET,     optcode_car0_set_code);
	defcode(OPTCODE_CAR0_PUSH,    optcode_car0_push_code);
	defcode(OPTCODE_CAR1_SET,     optcode_car1_set_code);
	defcode(OPTCODE_CAR1_PUSH,    optcode_car1_push_code);
	defcode(OPTCODE_CDR0_SET,     optcode_cdr0_set_code);
	defcode(OPTCODE_CDR0_PUSH,    optcode_cdr0_push_code);
	defcode(OPTCODE_CDR1_SET,     optcode_cdr1_set_code);
	defcode(OPTCODE_CDR1_PUSH,    optcode_cdr1_push_code);
	defcode(OPTCODE_CONS,         optcode_cons_code);
}

#undef defcode
#undef initcode

