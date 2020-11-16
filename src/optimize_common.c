#include "callname.h"
#include "code_init.h"
#include "code_make.h"
#include "code_queue.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control_operator.h"
#include "declare.h"
#include "eval_object.h"
#include "eval_table.h"
#include "function.h"
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
	OptimizeType value = get_optimize_scope(scope, EVAL_OPTIMIZE_SPEED);
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

static int optimize_common_car0(LocalRoot local, addr code, addr pos)
{
	/* no check */
	getvalue_tablecall(pos, &pos);
	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			code_make_execute_set(local, code, pos);
			CodeQueue_single(local, code, OPTCODE_CAR0_SET);
			break;

		case CodeQueue_ModePush:
			code_make_execute_set(local, code, pos);
			CodeQueue_single(local, code, OPTCODE_CAR0_PUSH);
			break;

		case CodeQueue_ModeRemove:
		default:
			code_make_execute_rem(local, code, pos);
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
	code_make_execute_set(local, code, pos);
	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(local, code, OPTCODE_CAR1_SET, type);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(local, code, OPTCODE_CAR1_PUSH, type);
			break;

		case CodeQueue_ModeRemove:
		default:
			CodeQueue_cons(local, code, OPTCODE_RESULT_TYPE, type);
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

static int optimize_common_cdr0(LocalRoot local, addr code, addr pos)
{
	/* no check */
	getvalue_tablecall(pos, &pos);
	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			code_make_execute_set(local, code, pos);
			CodeQueue_single(local, code, OPTCODE_CDR0_SET);
			break;

		case CodeQueue_ModePush:
			code_make_execute_set(local, code, pos);
			CodeQueue_single(local, code, OPTCODE_CDR0_PUSH);
			break;

		case CodeQueue_ModeRemove:
		default:
			code_make_execute_rem(local, code, pos);
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
	code_make_execute_set(local, code, pos);
	switch (code_queue_mode(code)) {
		case CodeQueue_ModeSet:
			CodeQueue_cons(local, code, OPTCODE_CDR1_SET, type);
			break;

		case CodeQueue_ModePush:
			CodeQueue_cons(local, code, OPTCODE_CDR1_PUSH, type);
			break;

		case CodeQueue_ModeRemove:
		default:
			CodeQueue_cons(local, code, OPTCODE_RESULT_TYPE, type);
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

static int optimize_common_cons0(LocalRoot local, addr code, addr car, addr cdr)
{
	addr pos;

	getvalue_tablecall(car, &car);
	getvalue_tablecall(cdr, &cdr);
	/* return begin */
	code_queue_push_new(local, code);
	code_make_execute_push(local, code, car);
	code_make_execute_push(local, code, cdr);
	/* cons */
	CodeQueue_single(local, code, OPTCODE_CONS);
	/* return end */
	code_queue_pop(local, code, &pos);
	code_make_execute_control(local, code, pos);

	return 1;
}

static int optimize_common_cons_rem(LocalRoot local, addr code, addr car, addr cdr)
{
	getvalue_tablecall(car, &car);
	getvalue_tablecall(cdr, &cdr);
	code_make_execute(local, code, car);
	code_make_execute(local, code, cdr);

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
	if (! code_queue_remp(code))
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

int optimize_common(LocalRoot local, addr code, addr scope)
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
	if (! strvect_designer_equalp_char(pos, "SCOPE"))
		return 0;

	/* fixnum */
	fixnum_heap(&pos, (fixnum)optimize_common_p(scope));
	code_make_object(local, code, pos);
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
	if (! strvect_designer_equalp_char(pos, "LIST"))
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
	code_make_object(local, code, list);

	return 1;
}

int optimize_check_code(LocalRoot local, addr code, addr scope)
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

