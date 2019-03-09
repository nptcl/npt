/*
 *  ANSI COMMON LISP: 12. Numbers
 */
#include <float.h>
#include "cmpl.h"
#include "common_header.h"
#include "cons.h"
#include "integer.h"
#include "math_exp.h"
#include "math_power.h"
#include "number.h"
#include "number_gcd.h"
#include "number_random.h"
#include "package.h"
#include "random_state.h"
#include "rational.h"
#include "real_ceiling.h"
#include "real_common.h"
#include "real_division.h"
#include "real_float.h"
#include "real_floor.h"
#include "real_round.h"
#include "real_truncate.h"
#include "setf.h"
#include "strtype.h"
#include "type_parse.h"

/* (defun = (first &rest numbers) ...) -> boolean */
static void function_number_equal(Execute ptr, addr left, addr rest)
{
	addr right;
	LocalRoot local;

	local = ptr->local;
	for (; rest != Nil; left = right) {
		getcons(rest, &right, &rest);
		if (! equal_number(local, left, right)) {
			setresult_control(ptr, Nil);
			return;
		}
	}
	setresult_control(ptr, T);
}

static void defun_number_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_EQUAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_number_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Number_Equal);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun /= (first &rest numbers) ...) -> boolean */
static void function_number_not_equal(Execute ptr, addr left, addr rest)
{
	addr right, list;
	LocalRoot local;

	local = ptr->local;
	while (rest != Nil) {
		for (list = rest; list != Nil; ) {
			getcons(list, &right, &list);
			if (equal_number(local, left, right)) {
				setresult_control(ptr, Nil);
				return;
			}
		}
		getcons(rest, &left, &rest);
	}
	setresult_control(ptr, T);
}

static void defun_number_not_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_NOT_EQUAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_number_not_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Number_Equal);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun < (first &rest numbers) ...) -> boolean */
static void function_number_less(Execute ptr, addr left, addr rest)
{
	addr right;
	LocalRoot local;

	local = ptr->local;
	for (; rest != Nil; left = right) {
		getcons(rest, &right, &rest);
		if (! less_number(local, left, right)) {
			setresult_control(ptr, Nil);
			return;
		}
	}
	setresult_control(ptr, T);
}

static void defun_number_less(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_LESS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_number_less);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Number_Compare);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun > (first &rest numbers) ...) -> boolean */
static void function_number_greater(Execute ptr, addr left, addr rest)
{
	addr right;
	LocalRoot local;

	local = ptr->local;
	for (; rest != Nil; left = right) {
		getcons(rest, &right, &rest);
		if (! greater_number(local, left, right)) {
			setresult_control(ptr, Nil);
			return;
		}
	}
	setresult_control(ptr, T);
}

static void defun_number_greater(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_GREATER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_number_greater);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Number_Compare);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun <= (first &rest numbers) ...) -> boolean */
static void function_number_less_equal(Execute ptr, addr left, addr rest)
{
	addr right;
	LocalRoot local;

	local = ptr->local;
	for (; rest != Nil; left = right) {
		getcons(rest, &right, &rest);
		if (! less_equal_number(local, left, right)) {
			setresult_control(ptr, Nil);
			return;
		}
	}
	setresult_control(ptr, T);
}

static void defun_number_less_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_LESS_EQUAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_number_less_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Number_Compare);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun >= (first &rest numbers) ...) -> boolean */
static void function_number_greater_equal(Execute ptr, addr left, addr rest)
{
	addr right;
	LocalRoot local;

	local = ptr->local;
	for (; rest != Nil; left = right) {
		getcons(rest, &right, &rest);
		if (! greater_equal_number(local, left, right)) {
			setresult_control(ptr, Nil);
			return;
		}
	}
	setresult_control(ptr, T);
}

static void defun_number_greater_equal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBER_GREATER_EQUAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_number_greater_equal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Number_Compare);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun max (real &rest real) ...) -> real */
static void function_max(Execute ptr, addr left, addr rest)
{
	addr right;
	LocalRoot local;

	local = ptr->local;
	while (rest != Nil) {
		getcons(rest, &right, &rest);
		if (less_number(local, left, right)) {
			left = right;
		}
	}
	setresult_control(ptr, left);
}

static void defun_max(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAX, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_max);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Max);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun min (real &rest real) ...) -> real */
static void function_min(Execute ptr, addr left, addr rest)
{
	addr right;
	LocalRoot local;

	local = ptr->local;
	while (rest != Nil) {
		getcons(rest, &right, &rest);
		if (greater_number(local, left, right)) {
			left = right;
		}
	}
	setresult_control(ptr, left);
}

static void defun_min(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MIN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_min);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Max);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun minusp (real) ...) -> boolean */
static void function_minusp(Execute ptr, addr var)
{
	setbool_control(ptr, minusp_real(var));
}

static void defun_minusp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MINUSP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_minusp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Minusp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun plusp (real) ...) -> boolean */
static void function_plusp(Execute ptr, addr var)
{
	setbool_control(ptr, plusp_real(var));
}

static void defun_plusp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PLUSP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_plusp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Minusp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun zerop (real) ...) -> boolean */
static void function_zerop(Execute ptr, addr var)
{
	setbool_control(ptr, zerop_number(var));
}

static void defun_zerop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ZEROP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_zerop);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Zerop);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun floor (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   integer
 *   remainder  real
 */
static void function_floor(Execute ptr, addr var, addr div)
{
	if (div == Unbound)
		floor1_common(ptr->local, &var, &div, var);
	else
		floor_common(ptr->local, &var, &div, var, div);
	setvalues_va_control(ptr, var, div, NULL);
}

static void defun_floor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FLOOR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_floor);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Floor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ffloor (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   real
 *   remainder  real
 */
static void function_ffloor(Execute ptr, addr var, addr div)
{
	if (div == Unbound)
		ffloor1_common(ptr->local, &var, &div, var);
	else
		ffloor_common(ptr->local, &var, &div, var, div);
	setvalues_va_control(ptr, var, div, NULL);
}

static void defun_ffloor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FFLOOR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_ffloor);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Ffloor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ceiling (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   integer
 *   remainder  real
 */
static void function_ceiling(Execute ptr, addr var, addr div)
{
	if (div == Unbound)
		ceiling1_common(ptr->local, &var, &div, var);
	else
		ceiling_common(ptr->local, &var, &div, var, div);
	setvalues_va_control(ptr, var, div, NULL);
}

static void defun_ceiling(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CEILING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_ceiling);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Floor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fceiling (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   real
 *   remainder  real
 */
static void function_fceiling(Execute ptr, addr var, addr div)
{
	if (div == Unbound)
		fceiling1_common(ptr->local, &var, &div, var);
	else
		fceiling_common(ptr->local, &var, &div, var, div);
	setvalues_va_control(ptr, var, div, NULL);
}

static void defun_fceiling(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FCEILING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_fceiling);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Ffloor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun truncate (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   integer
 *   remainder  real
 */
static void function_truncate(Execute ptr, addr var, addr div)
{
	if (div == Unbound)
		truncate1_common(ptr->local, &var, &div, var);
	else
		truncate_common(ptr->local, &var, &div, var, div);
	setvalues_va_control(ptr, var, div, NULL);
}

static void defun_truncate(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TRUNCATE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_truncate);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Floor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ftruncate (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   real
 *   remainder  real
 */
static void function_ftruncate(Execute ptr, addr var, addr div)
{
	if (div == Unbound)
		ftruncate1_common(ptr->local, &var, &div, var);
	else
		ftruncate_common(ptr->local, &var, &div, var, div);
	setvalues_va_control(ptr, var, div, NULL);
}

static void defun_ftruncate(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FTRUNCATE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_ftruncate);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Ffloor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun round (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   integer
 *   remainder  real
 */
static void function_round(Execute ptr, addr var, addr div)
{
	if (div == Unbound)
		round1_common(ptr->local, &var, &div, var);
	else
		round_common(ptr->local, &var, &div, var, div);
	setvalues_va_control(ptr, var, div, NULL);
}

static void defun_round(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ROUND, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_round);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Floor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fround (number &optional divisor) ...) -> quotient, remainder
 *   number     real
 *   divisor    real  ;; default 1
 *   quotient   real
 *   remainder  real
 */
static void function_fround(Execute ptr, addr var, addr div)
{
	if (div == Unbound)
		fround1_common(ptr->local, &var, &div, var);
	else
		fround_common(ptr->local, &var, &div, var, div);
	setvalues_va_control(ptr, var, div, NULL);
}

static void defun_fround(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FROUND, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_fround);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Ffloor);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun cis (real) ...) -> number */
static void function_cis(Execute ptr, addr var)
{
	cis_common(var, &var);
	setresult_control(ptr, var);
}

static void type_cis(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Real);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Number);
	type_compiled_heap(arg, values, ret);
}

static void defun_cis(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CIS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_cis);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cis(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sin (number) ...) -> number */
static void function_sin(Execute ptr, addr var)
{
	sin_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_sin(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_sin);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun cos (number) ...) -> number */
static void function_cos(Execute ptr, addr var)
{
	cos_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_cos(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_cos);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun tan (number) ...) -> number */
static void function_tan(Execute ptr, addr var)
{
	tan_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_tan(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TAN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_tan);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sinh (number) ...) -> number */
static void function_sinh(Execute ptr, addr var)
{
	sinh_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_sinh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SINH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_sinh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun cosh (number) ...) -> number */
static void function_cosh(Execute ptr, addr var)
{
	cosh_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_cosh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COSH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_cosh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun tanh (number) ...) -> number */
static void function_tanh(Execute ptr, addr var)
{
	tanh_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_tanh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TANH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_tanh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun asin (number) ...) -> number */
static void function_asin(Execute ptr, addr var)
{
	asin_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_asin(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASIN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_asin);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun acos (number) ...) -> number */
static void function_acos(Execute ptr, addr var)
{
	acos_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_acos(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ACOS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_acos);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun atan (number &optional real) ...) -> number */
static void function_atan(Execute ptr, addr var, addr opt)
{
	if (opt == Unbound)
		atan_common(var, &var);
	else
		atan2_common(var, opt, &var);
	setresult_control(ptr, var);
}

static void type_atan(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Number);
	GetCallType(&values, Real);
	var1opt1_argtype(&arg, arg, values);
	GetCallType(&values, Values_Number);
	type_compiled_heap(arg, values, ret);
}

static void defun_atan(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ATAN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_atan);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_atan(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun asinh (number) ...) -> number */
static void function_asinh(Execute ptr, addr var)
{
	asinh_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_asinh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASINH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_asinh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun acosh (number) ...) -> number */
static void function_acosh(Execute ptr, addr var)
{
	acosh_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_acosh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ACOSH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_acosh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun atanh (number) ...) -> number */
static void function_atanh(Execute ptr, addr var)
{
	atanh_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_atanh(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ATANH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_atanh);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun exp (number) ...) -> number */
static void function_exp(Execute ptr, addr var)
{
	exp_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_exp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EXP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_exp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun expt (number number) ...) -> number */
static void function_expt(Execute ptr, addr base, addr power)
{
	expt_common(ptr->local, &base, base, power);
	setresult_control(ptr, base);
}

static void type_expt(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Number);
	var2_argtype(&arg, arg, arg);
	GetCallType(&values, Values_Number);
	type_compiled_heap(arg, values, ret);
}

static void defun_expt(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EXPT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_expt);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_expt(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun + (&rest number) ...) -> number */
static void function_plus(Execute ptr, addr rest)
{
	addr left, right;
	LocalRoot local;

	/* nil */
	if (rest == Nil) {
		fixnum_heap(&rest, 0);
		setresult_control(ptr, rest);
		return;
	}

	/* list */
	local = ptr->local;
	getcons(rest, &left, &rest);
	while (rest != Nil) {
		getcons(rest, &right, &rest);
		plus_number_heap(local, left, right, &left);
	}
	setresult_control(ptr, left);
}

static void defun_plus(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PLUS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, function_plus);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Plus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun - (&rest number) ...) -> number */
static void function_minus(Execute ptr, addr left, addr rest)
{
	addr right;
	LocalRoot local;

	/* nil */
	if (rest == Nil) {
		sign_reverse_number_common(left, &left);
		setresult_control(ptr, left);
		return;
	}

	/* list */
	local = ptr->local;
	while (rest != Nil) {
		getcons(rest, &right, &rest);
		minus_number_heap(local, left, right, &left);
	}
	setresult_control(ptr, left);
}

static void defun_minus(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MINUS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_minus);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Minus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun * (&rest number) ...) -> number */
static void function_asterisk(Execute ptr, addr rest)
{
	addr left, right;
	LocalRoot local;

	/* nil */
	if (rest == Nil) {
		fixnum_heap(&rest, 1);
		setresult_control(ptr, rest);
		return;
	}

	/* list */
	local = ptr->local;
	getcons(rest, &left, &rest);
	while (rest != Nil) {
		getcons(rest, &right, &rest);
		multi_number_heap(local, left, right, &left);
	}
	setresult_control(ptr, left);
}

static void defun_asterisk(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASTERISK, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, function_asterisk);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Plus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun / (number &rest number) ...) -> number */
static void function_slash(Execute ptr, addr left, addr rest)
{
	addr right;
	LocalRoot local;

	/* nil */
	local = ptr->local;
	if (rest == Nil) {
		inverse_number_heap(local, left, &left);
		setresult_control(ptr, left);
		return;
	}

	/* list */
	while (rest != Nil) {
		getcons(rest, &right, &rest);
		div_number_heap(local, left, right, &left);
	}
	setresult_control(ptr, left);
}

static void defun_slash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SLASH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_slash);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Minus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun 1+ (number) ...) -> number */
static void function_oneplus(Execute ptr, addr var)
{
	oneplus_number_common(ptr->local, var, &var);
	setresult_control(ptr, var);
}

static void defun_oneplus(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ONE_PLUS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_oneplus);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_OnePlus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun 1- (number) ...) -> number */
static void function_oneminus(Execute ptr, addr var)
{
	oneminus_number_common(ptr->local, var, &var);
	setresult_control(ptr, var);
}

static void defun_oneminus(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ONE_MINUS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_oneminus);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_OnePlus);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun abs (number) ...) value
 *    value  (real 0 *)
 */
static void function_abs(Execute ptr, addr var)
{
	abs_number_common(var, &var);
	setresult_control(ptr, var);
}

static void type_abs(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Number);
	var1_argtype(&arg, arg);
	fixnum_heap(&values, 0);
	type_object4(NULL, LISPDECL_REAL, Nil, values,
			type_asterisk_heapr(),
			type_asterisk_heapr(),
			&values);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_abs(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ABS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_abs);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_abs(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun evenp (integer) ...) -> boolean */
static void function_evenp(Execute ptr, addr var)
{
	setbool_control(ptr, evenp_integer(var));
}

static void defun_evenp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EVENP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_evenp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Evenp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun oddp (integer) ...) -> boolean */
static void function_oddp(Execute ptr, addr var)
{
	setbool_control(ptr, ! evenp_integer(var));
}

static void defun_oddp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ODDP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_oddp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Evenp);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun gcd (&rest integer) ...) -> (integer 0 *) */
static void function_gcd(Execute ptr, addr args)
{
	gcd_number(ptr->local, args, &args);
	setresult_control(ptr, args);
}

static void defun_gcd(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GCD, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, function_gcd);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Gcd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun lcm (&rest integer) ...) -> (integer 0 *) */
static void function_lcm(Execute ptr, addr args)
{
	lcm_number(ptr->local, args, &args);
	setresult_control(ptr, args);
}

static void defun_lcm(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LCM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, function_lcm);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Gcd);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro incf (place &optional number) ...) -> number) */
static void expand_incf(Execute ptr, addr *ret, addr place, addr value, addr env)
{
	addr a, b, g, w, r;
	addr c, d, ig, args, leta, declare, ignorable, setq, plus;

	get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r);
	if (! singlep(g)) {
		fmte("INCF place ~S don't allow a multiple store value.", place, NULL);
		return;
	}

	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        g)
	 *   (declare (ignorable a1 a2))
	 *   (setq g (+ r value))  ;; (setq g (1+ r))
	 *   w g)
	 */
	args = Nil;
	for (ig = a; a != Nil; ) {
		getcons(a, &c, &a);
		getcons(b, &d, &b);
		list_heap(&c, c, d, NULL);
		cons_heap(&args, c, args);
	}
	GetCar(g, &g);
	cons_heap(&args, g, args);
	nreverse_list_unsafe(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, ig);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* setq */
	GetConst(COMMON_SETQ, &setq);
	if (value == Unbound) {
		GetConst(COMMON_ONE_PLUS, &plus);
		list_heap(&plus, plus, r, NULL);
	}
	else {
		GetConst(COMMON_PLUS, &plus);
		list_heap(&plus, plus, r, value, NULL);
	}
	list_heap(&setq, setq, g, plus, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, setq, w, g, NULL);
}

static void function_incf(Execute ptr, addr form, addr env)
{
	addr args, place, value;

	getcdr(form, &form);
	if (form == Nil) goto error;
	getcons(form, &place, &args);
	if (args == Nil) {
		value = Unbound;
	}
	else {
		getcons(args, &value, &args);
		if (args != Nil) goto error;
	}
	expand_incf(ptr, &value, place, value, env);
	setresult_control(ptr, value);
	return;

error:
	fmte("INCF ~S must be (place &optional value) form.", form, NULL);
}

static void defmacro_incf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_INCF, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_incf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_MacroFunction);
	settype_function(pos, type);
}


/* (defmacro decf (place &optional number) ...) -> number) */
static void expand_decf(Execute ptr, addr *ret, addr place, addr value, addr env)
{
	addr a, b, g, w, r;
	addr c, d, ig, args, leta, declare, ignorable, setq, minus;

	get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r);
	if (! singlep(g)) {
		fmte("DECF place ~S don't allow a multiple store value.", place, NULL);
		return;
	}

	/* (let* ((a1 b1)
	 *        (a2 b2)
	 *        g)
	 *   (declare (ignorable a1 a2))
	 *   (setq g (- r value))  ;; (setq g (1- r))
	 *   w g)
	 */
	args = Nil;
	for (ig = a; a != Nil; ) {
		getcons(a, &c, &a);
		getcons(b, &d, &b);
		list_heap(&c, c, d, NULL);
		cons_heap(&args, c, args);
	}
	GetCar(g, &g);
	cons_heap(&args, g, args);
	nreverse_list_unsafe(&args, args);
	/* declare */
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, ig);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, ignorable, NULL);
	/* setq */
	GetConst(COMMON_SETQ, &setq);
	if (value == Unbound) {
		GetConst(COMMON_ONE_MINUS, &minus);
		list_heap(&minus, minus, r, NULL);
	}
	else {
		GetConst(COMMON_MINUS, &minus);
		list_heap(&minus, minus, r, value, NULL);
	}
	list_heap(&setq, setq, g, minus, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	list_heap(ret, leta, args, declare, setq, w, g, NULL);
}

static void function_decf(Execute ptr, addr form, addr env)
{
	addr args, place, value;

	getcdr(form, &form);
	if (form == Nil) goto error;
	getcons(form, &place, &args);
	if (args == Nil) {
		value = Unbound;
	}
	else {
		getcons(args, &value, &args);
		if (args != Nil) goto error;
	}
	expand_decf(ptr, &value, place, value, env);
	setresult_control(ptr, value);
	return;

error:
	fmte("DECF ~S must be (place &optional value) form.", form, NULL);
}

static void defmacro_decf(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DECF, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, function_decf);
	SetMacroCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_MacroFunction);
	settype_function(pos, type);
}


/* (defun log (number &optional number) ...) -> number */
static void function_log(Execute ptr, addr value, addr base)
{
	if (base == Unbound)
		log_natural_common(value, &value);
	else
		log_base_common(value, base, &value);
	setresult_control(ptr, value);
}

static void type_log(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Number);
	var1opt1_argtype(&arg, arg, arg);
	GetCallType(&values, Values_Number);
	type_compiled_heap(arg, values, ret);
}

static void defun_log(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOG, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_log);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_log(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun mod (real divisor) ...) -> modules
 *   real     real
 *   divisor  real
 *   modules  real
 */
static void function_mod(Execute ptr, addr num, addr div)
{
	mod_number_common(ptr->local, num, div, &num);
	setresult_control(ptr, num);
}

static void defun_mod(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MOD, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_mod);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Mod);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rem (real divisor) ...) -> modules
 *   real     real
 *   divisor  real
 *   modules  real
 */
static void function_rem(Execute ptr, addr num, addr div)
{
	rem_number_common(ptr->local, num, div, &num);
	setresult_control(ptr, num);
}

static void defun_rem(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_rem);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Mod);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun signum (number) ...) -> number */
static void function_signum(Execute ptr, addr var)
{
	signum_number_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_signum(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIGNUM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_signum);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sqrt (number) ...) -> number */
static void function_sqrt(Execute ptr, addr var)
{
	sqrt_number_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_sqrt(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SQRT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_sqrt);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-random-state (&optional state) ...) -> random-state
 *   state  (or random-state null (eql t))
 */
static void function_make_random_state(Execute ptr, addr opt)
{
	if (opt == Unbound) opt = Nil;
	make_random_state_heap(ptr, &opt, opt);
	setresult_control(ptr, opt);
}

static void type_make_random_state(addr *ret)
{
	addr arg, values, type1, type2, type3;

	vector4_heap(&arg, 3);
	GetCallType(&type1, RandomState);
	GetCallType(&type2, Null);
	GetCallType(&type3, EqlT);
	type_or3(NULL, type1, type2, type3, &arg);
	opt1_argtype(&arg, arg);
	result_valuestype(&values, type1);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_random_state(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_RANDOM_STATE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, function_make_random_state);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_random_state(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun random (limit &optional random-state) ...) -> value
 *   limit  (or (integer (0) *) (float (0) *))
 *   value  (or (integer 0 *) (float 0 *))
 */
static void function_random(Execute ptr, addr limit, addr state)
{
	if (state == Unbound) {
		/* symbol-value *random-state* */
		GetConst(SPECIAL_RANDOM_STATE, &state);
		getspecialcheck_local(ptr, state, &state);
	}
	random_number_common(ptr->local, limit, state, &limit);
	setresult_control(ptr, limit);
}

static void type_random(addr *ret)
{
	addr arg, values, type1, type2;

	GetCallType(&arg, RandomState);
	type_intrange_left(T, 0, &type1);
	type_floatrange_left(T, 0.0f, &type2);
	type_or(NULL, type1, type2, &type1);
	var1opt1_argtype(&arg, type1, arg);
	type_intrange_left(Nil, 0, &type1);
	type_floatrange_left(Nil, 0.0f, &type2);
	type_or(NULL, type1, type2, &values);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_random(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RANDOM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_random);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_random(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun random-state-p (object) ...) -> boolean */
static void function_random_state_p(Execute ptr, addr pos)
{
	setbool_control(ptr, GetType(pos) == LISPTYPE_RANDOM_STATE);
}

static void defun_random_state_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RANDOM_STATE_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_random_state_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar *random-state* random-state) */
static void defvar_random_state(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_RANDOM_STATE, &symbol);
	make_random_state_heap(Execute_Thread, &value, T);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);

	/* type */
	GetCallType(&type, RandomState);
	settype_value_symbol(symbol, type);
}


/* (defun numberp (object) ...) -> boolean */
static void function_numberp(Execute ptr, addr pos)
{
	setbool_control(ptr, numberp(pos));
}

static void defun_numberp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMBERP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_numberp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun complex (real &optional imag) ...) -> result
 *   real    real
 *   imag    real
 *   result  (or rational complex)
 */
static void function_complex(Execute ptr, addr real, addr imag)
{
	complex_heap(&real, real, imag);
	setresult_control(ptr, real);
}

static void type_complex_common(addr *ret)
{
	addr arg, values, type;

	GetCallType(&arg, Real);
	var1opt1_argtype(&arg, arg, arg);
	GetCallType(&type, Rational);
	GetCallType(&values, Complex);
	type_or(NULL, type, values, &values);
	type_compiled_heap(arg, values, ret);
}

static void defun_complex(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPLEX, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_complex);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_complex_common(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun complexp (object) ...) -> boolean */
static void function_complexp(Execute ptr, addr pos)
{
	setbool_control(ptr, GetType(pos) == LISPTYPE_COMPLEX);
}

static void defun_complexp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPLEXP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_complexp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun conjugate (number) ...) -> number */
static void function_conjugate(Execute ptr, addr var)
{
	addr real, imag;

	if (complexp(var)) {
		GetRealComplex(var, &real);
		GetImagComplex(var, &imag);
		sign_reverse_real_common(imag, &imag);
		complex_heap(&var, real, imag);
	}
	else if (! realp(var)) {
		TypeError(var, NUMBER);
		var = Nil;
	}
	setresult_control(ptr, var);
}

static void defun_conjugate(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONJUGATE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_conjugate);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun phase (number) ...) -> number */
static void function_phase(Execute ptr, addr var)
{
	phase_common(var, &var);
	setresult_control(ptr, var);
}

static void defun_phase(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PHASE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_phase);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Sin);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun realpart (number) ...) -> real */
static void function_realpart(Execute ptr, addr var)
{
	if (complexp(var)) {
		GetRealComplex(var, &var);
	}
	real_throw_heap(var, &var);
	setresult_control(ptr, var);
}

static void defun_realpart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REALPART, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_realpart);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_RealPart);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun imagpart (number) ...) -> real */
static void function_imagpart(Execute ptr, addr var)
{
	if (complexp(var)) {
		GetImagComplex(var, &var);
		real_throw_heap(var, &var);
	}
	else {
		fixnum_heap(&var, 0);
	}
	setresult_control(ptr, var);
}

static void defun_imagpart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_IMAGPART, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_imagpart);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_RealPart);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun complexp (object) ...) -> boolean */
static void function_upgraded_complex_part_type(Execute ptr, addr type, addr env)
{
	fmte("TODO", NULL);
}

static void defun_upgraded_complex_part_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UPGRADED_COMPLEX_PART_TYPE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_upgraded_complex_part_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun realp (object) ...) -> boolean */
static void function_realp(Execute ptr, addr pos)
{
	setbool_control(ptr, realp(pos));
}

static void defun_realp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REALP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_realp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun numerator (rational) ...) -> integer */
static void function_numerator(Execute ptr, addr var)
{
	numerator_common(var, &var);
	setresult_control(ptr, var);
}

static void type_numerator(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Rational);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Integer);
	type_compiled_heap(arg, values, ret);
}

static void defun_numerator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_NUMERATOR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_numerator);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_numerator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun denominator (rational) ...) -> positive-integer */
static void function_denominator(Execute ptr, addr var)
{
	denominator_common(var, &var);
	setresult_control(ptr, var);
}

static void type_denominator(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Rational);
	var1_argtype(&arg, arg);
	type_intrange_left(T, 0, &values);
	result_valuestype(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_denominator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DENOMINATOR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_denominator);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_denominator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rationalp (object) ...) -> boolean */
static void function_rationalp(Execute ptr, addr pos)
{
	setbool_control(ptr, rationalp(pos));
}

static void defun_rationalp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RATIONALP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_rationalp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ash (integer count) ...) -> shifted
 *   integer  integer
 *   count    integer
 *   shifted  integer
 */
static void function_ash(Execute ptr, addr pos, addr count)
{
	ash_integer_common(ptr->local, pos, count, &pos);
	setresult_control(ptr, pos);
}

static void type_ash(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Integer);
	var2_argtype(&arg, arg, arg);
	GetCallType(&values, Values_Integer);
	type_compiled_heap(arg, values, ret);
}

static void defun_ash(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ASH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, function_ash);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_ash(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun integer-length (integer) ...) -> bits
 *   integer  integer
 *   bits     a non-negative integer
 */
static void function_integer_length(Execute ptr, addr var)
{
	integer_length_common(var, &var);
	setresult_control(ptr, var);
}

static void type_integer_length(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Integer);
	var1_argtype(&arg, arg);
	GetCallType(&values, Values_Intplus);
	type_compiled_heap(arg, values, ret);
}

static void defun_integer_length(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTEGER_LENGTH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_integer_length);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_integer_length(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun integerp (object) ...) -> boolean */
static void function_integerp(Execute ptr, addr pos)
{
	setbool_control(ptr, integerp(pos));
}

static void defun_integerp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTEGERP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_integerp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parse-integer (string &key start end radix junk-allowed) ...)
 *     -> integer, pos
 *   string        string
 *   start         keyword-start
 *   end           keyword-end
 *   radix         radix-integer  ;; default 10
 *   junk-allowed  t  ;; boolean, default nil
 *   integer       (or null integer)
 *   pos           index
 */
static void function_parse_integer(Execute ptr, addr var, addr rest)
{
	addr radix, junk;
	size_t size, start, end;

	string_length(var, &size);
	keyword_start_end(size, rest, &start, &end);
	if (getkeyargs(rest, KEYWORD_RADIX, &radix)) fixnum_heap(&radix, 10);
	if (getkeyargs(rest, KEYWORD_JUNK_ALLOWED, &junk)) junk = Nil;
	parse_integer_common(ptr->local, var, start, end,
			(unsigned)RefFixnum(radix), junk != Nil, &var, &rest);
	setvalues_va_control(ptr, var, rest, NULL);
}

static void type_parse_integer(addr *ret)
{
	addr arg, values, type, key, key1, key2, key3, key4;

	/* key */
	GetConst(KEYWORD_START, &key1);
	GetCallType(&values, KeywordStart);
	cons_heap(&key1, key1, values);
	GetConst(KEYWORD_END, &key2);
	GetCallType(&values, KeywordEnd);
	cons_heap(&key2, key2, values);
	GetConst(KEYWORD_RADIX, &key3);
	GetCallType(&values, RadixInteger);
	cons_heap(&key3, key3, values);
	GetConst(KEYWORD_JUNK_ALLOWED, &key4);
	GetCallType(&values, T);
	cons_heap(&key4, key4, values);
	list_heap(&key, key1, key2, key3, key4, NULL);
	/* type */
	GetCallType(&arg, String);
	var1key_argtype(&arg, arg, key);
	GetCallType(&values, IntegerNull);
	GetCallType(&type, Index);
	values2_valuestype(&values, values, type);
	type_compiled_heap(arg, values, ret);
}

static void defun_parse_integer(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PARSE_INTEGER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, function_parse_integer);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_parse_integer(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun float (real &optional prototype) ...) -> float */
static void function_float(Execute ptr, addr var, addr type)
{
	float_common(&var, var, type);
	setresult_control(ptr, var);
}

static void type_float_function(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, Real);
	GetCallType(&values, Float);
	var1opt1_argtype(&arg, arg, values);
	GetCallType(&values, Values_Float);
	type_compiled_heap(arg, values, ret);
}

static void defun_float(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FLOAT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_float);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_float_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun floatp (object) ...) -> boolean */
static void function_floatp(Execute ptr, addr pos)
{
	setbool_control(ptr, floatp(pos));
}

static void defun_floatp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FLOATP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_floatp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetCallType(&type, Compiled_Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun arithmetic-error-operands (arithmetic-error) ...) -> list */
static void function_arithmetic_error_operands(Execute ptr, addr var)
{
	arithmetic_error_operands(var, &var);
	setresult_control(ptr, var);
}

static void type_arithmetic_error_operands(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, ArithmeticError);
	opt1_argtype(&arg, arg);
	GetCallType(&values, Values_List);
	type_compiled_heap(arg, values, ret);
}

static void defun_arithmetic_error_operands(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARITHMETIC_ERROR_OPERANDS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_arithmetic_error_operands);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_arithmetic_error_operands(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun arithmetic-error-operation (arithmetic-error) ...) -> list */
static void function_arithmetic_error_operation(Execute ptr, addr var)
{
	arithmetic_error_operation(var, &var);
	setresult_control(ptr, var);
}

static void type_arithmetic_error_operation(addr *ret)
{
	addr arg, values;

	GetCallType(&arg, ArithmeticError);
	opt1_argtype(&arg, arg);
	GetCallType(&values, Values_List);
	type_compiled_heap(arg, values, ret);
}

static void defun_arithmetic_error_operation(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ARITHMETIC_ERROR_OPERATION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, function_arithmetic_error_operation);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_arithmetic_error_operation(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defconstant pi 3.14159265358979323844l0) */
static void defconstant_pi(void)
{
	addr symbol, value;

	GetConst(COMMON_PI, &symbol);
	/* $ echo 'scale=20; a(1)*4' | bc -l */
	long_float_heap(&value, 3.14159265358979323844L);
	defconstant_symbol(symbol, value);
}


/* (defconstant boole-1 ...) */
static void defconstant_boole(constindex index, enum Boole_Index value)
{
	addr symbol, pos;

	GetConstant(index, &symbol);
	fixnum_heap(&pos, (fixnum)value);
	defconstant_symbol(symbol, pos);
}
static void defconstant_boole_1(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_1, Boole_1);
}
static void defconstant_boole_2(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_2, Boole_2);
}
static void defconstant_boole_and(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_AND, Boole_And);
}
static void defconstant_boole_andc1(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_ANDC1, Boole_AndC1);
}
static void defconstant_boole_andc2(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_ANDC2, Boole_AndC2);
}
static void defconstant_boole_c1(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_C1, Boole_C1);
}
static void defconstant_boole_c2(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_C2, Boole_C2);
}
static void defconstant_boole_clr(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_CLR, Boole_Clr);
}
static void defconstant_boole_eqv(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_EQV, Boole_Eqv);
}
static void defconstant_boole_ior(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_IOR, Boole_Ior);
}
static void defconstant_boole_nand(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_NAND, Boole_Nand);
}
static void defconstant_boole_nor(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_NOR, Boole_Nor);
}
static void defconstant_boole_orc1(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_ORC1, Boole_Orc1);
}
static void defconstant_boole_orc2(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_ORC2, Boole_Orc2);
}
static void defconstant_boole_set(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_SET, Boole_Set);
}
static void defconstant_boole_xor(void)
{
	defconstant_boole(CONSTANT_COMMON_BOOLE_XOR, Boole_Xor);
}


/* (defconstant most-positive-fixnum ...) */
static void defconstant_most_positive_fixnum(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_POSITIVE_FIXNUM, &symbol);
	GetConst(FIXNUM_MAX, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-negative-fixnum ...) */
static void defconstant_most_negative_fixnum(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_NEGATIVE_FIXNUM, &symbol);
	GetConst(FIXNUM_MIN, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-positive-short-float ...) */
static void defconstant_most_positive_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_POSITIVE_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_MOST_POSITIVE, &value); /* single */
	defconstant_symbol(symbol, value);
}


/* (defconstant most-positive-single-float ...) */
static void defconstant_most_positive_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_POSITIVE_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_MOST_POSITIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-positive-double-float ...) */
static void defconstant_most_positive_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_POSITIVE_DOUBLE_FLOAT, &symbol);
	double_float_heap(&value, DBL_MAX);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-positive-long-float ...) */
static void defconstant_most_positive_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_POSITIVE_LONG_FLOAT, &symbol);
	long_float_heap(&value, LDBL_MAX);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-negative-short-float ...) */
static void defconstant_most_negative_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_NEGATIVE_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_MOST_NEGATIVE, &value); /* single */
	defconstant_symbol(symbol, value);
}


/* (defconstant most-negative-single-float ...) */
static void defconstant_most_negative_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_NEGATIVE_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_MOST_NEGATIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-negative-double-float ...) */
static void defconstant_most_negative_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_NEGATIVE_DOUBLE_FLOAT, &symbol);
	double_float_heap(&value, -DBL_MAX);
	defconstant_symbol(symbol, value);
}


/* (defconstant most-negative-long-float ...) */
static void defconstant_most_negative_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_MOST_NEGATIVE_LONG_FLOAT, &symbol);
	long_float_heap(&value, -LDBL_MAX);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-short-float ...) */
static void defconstant_least_positive_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_POSITIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-normalized-short-float ...) */
static void defconstant_least_positive_normalized_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_POSITIVE_NORMALIZED, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-single-float ...) */
static void defconstant_least_positive_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_POSITIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-normalized-single-float ...) */
static void defconstant_least_positive_normalized_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_POSITIVE_NORMALIZED, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-double-float ...) */
static void defconstant_least_positive_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_DOUBLE_FLOAT, &symbol);
	double_float_least_positive(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-normalized-double-float ...) */
static void defconstant_least_positive_normalized_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT, &symbol);
	double_float_least_positive_normalized(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-long-float ...) */
static void defconstant_least_positive_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_LONG_FLOAT, &symbol);
	long_float_least_positive(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-positive-normalized-long-float ...) */
static void defconstant_least_positive_normalized_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_POSITIVE_NORMALIZED_LONG_FLOAT, &symbol);
	long_float_least_positive_normalized(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-short-float ...) */
static void defconstant_least_negative_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_NEGATIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-normalized-short-float ...) */
static void defconstant_least_negative_normalized_short_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_NEGATIVE_NORMALIZED, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-single-float ...) */
static void defconstant_least_negative_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_NEGATIVE, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-normalized-single-float ...) */
static void defconstant_least_negative_normalized_single_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT, &symbol);
	GetConst(SINGLE_FLOAT_LEAST_NEGATIVE_NORMALIZED, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-double-float ...) */
static void defconstant_least_negative_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_DOUBLE_FLOAT, &symbol);
	double_float_least_negative(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-normalized-double-float ...) */
static void defconstant_least_negative_normalized_double_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT, &symbol);
	double_float_least_negative_normalized(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-long-float ...) */
static void defconstant_least_negative_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_LONG_FLOAT, &symbol);
	long_float_least_negative(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant least-negative-normalized-long-float ...) */
static void defconstant_least_negative_normalized_long_float(void)
{
	addr symbol, value;

	GetConst(COMMON_LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT, &symbol);
	long_float_least_negative_normalized(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant short-float-epsilon ...) */
static void defconstant_short_float_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_SHORT_FLOAT_EPSILON, &symbol);
	GetConst(SINGLE_FLOAT_EPSILON, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant short-float-negative-epsilon ...) */
static void defconstant_short_float_negative_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_SHORT_FLOAT_NEGATIVE_EPSILON, &symbol);
	GetConst(SINGLE_FLOAT_NEGATIVE_EPSILON, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant single-float-epsilon ...) */
static void defconstant_single_float_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_SINGLE_FLOAT_EPSILON, &symbol);
	GetConst(SINGLE_FLOAT_EPSILON, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant single-float-negative-epsilon ...) */
static void defconstant_single_float_negative_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_SINGLE_FLOAT_NEGATIVE_EPSILON, &symbol);
	GetConst(SINGLE_FLOAT_NEGATIVE_EPSILON, &value);
	defconstant_symbol(symbol, value);
}


/* (defconstant double-float-epsilon ...) */
static void defconstant_double_float_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_DOUBLE_FLOAT_EPSILON, &symbol);
	double_float_epsilon(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant double-float-negative-epsilon ...) */
static void defconstant_double_float_negative_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_DOUBLE_FLOAT_NEGATIVE_EPSILON, &symbol);
	double_float_negative_epsilon(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant long-float-epsilon ...) */
static void defconstant_long_float_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_LONG_FLOAT_EPSILON, &symbol);
	long_float_epsilon(&value);
	defconstant_symbol(symbol, value);
}


/* (defconstant long-float-negative-epsilon ...) */
static void defconstant_long_float_negative_epsilon(void)
{
	addr symbol, value;

	GetConst(COMMON_LONG_FLOAT_NEGATIVE_EPSILON, &symbol);
	long_float_negative_epsilon(&value);
	defconstant_symbol(symbol, value);
}


/*
 *  intern
 */
void intern_common_numbers(void)
{
	defun_number_equal();
	defun_number_not_equal();
	defun_number_less();
	defun_number_greater();
	defun_number_less_equal();
	defun_number_greater_equal();
	defun_max();
	defun_min();
	defun_minusp();
	defun_plusp();
	defun_zerop();
	defun_floor();
	defun_ffloor();
	defun_ceiling();
	defun_fceiling();
	defun_truncate();
	defun_ftruncate();
	defun_round();
	defun_fround();
	defun_cis();
	defun_sin();
	defun_cos();
	defun_tan();
	defun_cosh();
	defun_tanh();
	defun_asinh();
	defun_asin();
	defun_acos();
	defun_atan();
	defun_sinh();
	defun_acosh();
	defun_atanh();
	defun_exp();
	defun_expt();
	defun_plus();
	defun_minus();
	defun_asterisk();
	defun_slash();
	defun_oneplus();
	defun_oneminus();
	defun_abs();
	defun_evenp();
	defun_oddp();
	defun_gcd();
	defun_lcm();
	defmacro_incf();
	defmacro_decf();
	defun_log();
	defun_mod();
	defun_rem();
	defun_signum();
	defun_sqrt();
	/* defun_isqrt(); */
	defun_make_random_state();
	defun_random();
	defun_random_state_p();
	defvar_random_state();
	defun_numberp();
	defun_complex();
	defun_complexp();
	defun_conjugate();
	defun_phase();
	defun_realpart();
	defun_imagpart();
	defun_upgraded_complex_part_type(); /* TODO */
	defun_realp();
	defun_numerator();
	defun_denominator();
	/* defun_rational(); */
	/* defun_rationalize(); */
	defun_rationalp();
	defun_ash();
	defun_integer_length();
	defun_integerp();
	defun_parse_integer();
	/* defun_boole(); */
	/* defun_logand(); */
	/* defun_logandc1(); */
	/* defun_logandc2(); */
	/* defun_logeqv(); */
	/* defun_logior(); */
	/* defun_lognand(); */
	/* defun_lognor(); */
	/* defun_lognot(); */
	/* defun_logorc1(); */
	/* defun_logorc2(); */
	/* defun_logxor(); */
	/* defun_logbitp(); */
	/* defun_logcount(); */
	/* defun_logtest(); */
	/* defun_byte(); */
	/* defun_byte_size(); */
	/* defun_byte_position(); */
	/* defun_deposit_field(); */
	/* defun_dpb(); */
	/* defun_ldb(); */
	/* defun_ldb_test(); */
	/* defun_mask_field(); */
	/* defun_decode_float(); */
	/* defun_scale_float(); */
	/* defun_float_radix(); */
	/* defun_float_sign(); */
	/* defun_float_digits(); */
	/* defun_float_precision(); */
	/* defun_integer_decode_float(); */
	defun_float();
	defun_floatp();
	defun_arithmetic_error_operands();
	defun_arithmetic_error_operation();

	defconstant_pi();
	defconstant_boole_1();
	defconstant_boole_2();
	defconstant_boole_and();
	defconstant_boole_andc1();
	defconstant_boole_andc2();
	defconstant_boole_c1();
	defconstant_boole_c2();
	defconstant_boole_clr();
	defconstant_boole_eqv();
	defconstant_boole_ior();
	defconstant_boole_nand();
	defconstant_boole_nor();
	defconstant_boole_orc1();
	defconstant_boole_orc2();
	defconstant_boole_set();
	defconstant_boole_xor();
	defconstant_most_positive_fixnum();
	defconstant_most_negative_fixnum();
	defconstant_most_positive_short_float();
	defconstant_most_positive_single_float();
	defconstant_most_positive_double_float();
	defconstant_most_positive_long_float();
	defconstant_most_negative_short_float();
	defconstant_most_negative_single_float();
	defconstant_most_negative_double_float();
	defconstant_most_negative_long_float();
	defconstant_least_positive_short_float();
	defconstant_least_positive_normalized_short_float();
	defconstant_least_positive_single_float();
	defconstant_least_positive_normalized_single_float();
	defconstant_least_positive_double_float();
	defconstant_least_positive_normalized_double_float();
	defconstant_least_positive_long_float();
	defconstant_least_positive_normalized_long_float();
	defconstant_least_negative_short_float();
	defconstant_least_negative_normalized_short_float();
	defconstant_least_negative_single_float();
	defconstant_least_negative_normalized_single_float();
	defconstant_least_negative_double_float();
	defconstant_least_negative_normalized_double_float();
	defconstant_least_negative_long_float();
	defconstant_least_negative_normalized_long_float();
	defconstant_short_float_epsilon();
	defconstant_short_float_negative_epsilon();
	defconstant_single_float_epsilon();
	defconstant_single_float_negative_epsilon();
	defconstant_double_float_epsilon();
	defconstant_double_float_negative_epsilon();
	defconstant_long_float_epsilon();
	defconstant_long_float_negative_epsilon();
}

