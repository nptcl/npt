#include "compile.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_operator.h"
#include "function.h"
#include "type_constant.h"
#include "type_table.h"
#include "pointer.h"
#include "syscall_function.h"
#include "syscode_function.h"
#include "symbol.h"

/* (defun hello () ...) -> null */
static int syscall_hello(Execute ptr)
{
	Return(hello_syscode(ptr));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_hello(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_HELLO, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_syscall_hello);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, CompiledFunction);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun infobit (&rest args) ...) -> object */
static int syscall_infobit(Execute ptr, addr rest)
{
	infobit_syscode(rest, &rest);
	setresult_control(ptr, rest);
	return 0;
}

static void defun_infobit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_INFOBIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_infobit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InfoBit);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun infoprint (&rest args) ...) -> object */
static int syscall_infoprint(Execute ptr, addr rest)
{
	infoprint_syscode(rest, &rest);
	setresult_control(ptr, rest);
	return 0;
}

static void defun_infoprint(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_INFOPRINT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_infoprint);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InfoBit);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun gc (&key full) ...) -> null */
static int syscall_gc(Execute ptr, addr rest)
{
	gc_syscode(rest);
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_gc(addr *ret)
{
	addr args, values;

	/* key */
	KeyTypeTable(&args, FULL, T);
	list_heap(&args, args, NULL);
	/* type */
	typeargs_key(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_gc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_GC, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_gc);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_gc(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun savecore (pathname-designer) ...) -> null */
static int syscall_savecore(Execute ptr, addr file)
{
	Return(savecore_syscode(ptr, file));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_savecore(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_savecore(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SAVECORE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_savecore);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_savecore(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-export-list (package-designer) ...) -> list */
static int syscall_package_export_list(Execute ptr, addr var)
{
	Return(package_export_list_syscode_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_package_export_list(addr *ret)
{
	/* (function (package_designer) (values list &rest nil)) */
	addr args, values;

	GetTypeTable(&args, PackageDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_package_export_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PACKAGE_EXPORT_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_package_export_list);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_package_export_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun specialp (symbol) ...) -> boolean */
static int syscall_specialp(Execute ptr, addr var)
{
	specialp_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_specialp(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_specialp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SPECIALP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_specialp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_specialp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-general-p (object) ...) -> boolean */
static int syscall_array_general_p(Execute ptr, addr var)
{
	array_general_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_array_general_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ARRAY_GENERAL_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_array_general_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-specialized-p (object) ...) -> boolean */
static int syscall_array_specialized_p(Execute ptr, addr var)
{
	array_specialized_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_array_specialized_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ARRAY_SPECIALIZED_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_array_specialized_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun simple-sort (sequence call &key key) ...) -> sequence */
static int syscall_simple_sort(Execute ptr, addr pos, addr call, addr rest)
{
	Return(simple_sort_syscode(ptr, pos, call, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_simple_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SIMPLE_SORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_simple_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bubble-sort (sequence call &key key) ...) -> sequence */
static int syscall_bubble_sort(Execute ptr, addr pos, addr call, addr rest)
{
	Return(bubble_sort_syscode(ptr, pos, call, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_bubble_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BUBBLE_SORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_bubble_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun quick-sort (sequence call &key key) ...) -> sequence */
static int syscall_quick_sort(Execute ptr, addr pos, addr call, addr rest)
{
	Return(quick_sort_syscode(ptr, pos, call, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_quick_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_QUICK_SORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_quick_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun merge-sort (sequence call &key key) ...) -> sequence */
static int syscall_merge_sort(Execute ptr, addr pos, addr call, addr rest)
{
	Return(merge_sort_syscode(ptr, pos, call, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void defun_merge_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MERGE_SORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_merge_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun exit/quit (&optional code) ...) -> (values &rest nil) */
static int syscall_exit(Execute ptr, addr code)
{
	Return(exit_syscode_(ptr, code));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_exit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EXIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_syscall_exit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Exit);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void defun_quit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_QUIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_syscall_exit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Exit);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun closp (object) ...) -> boolean */
static int syscall_closp(Execute ptr, addr var)
{
	closp_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_closp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_CLOSP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_closp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fixnump (object) ...) -> boolean */
static int syscall_fixnump(Execute ptr, addr var)
{
	fixnump_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_fixnump(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_FIXNUMP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_fixnump);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bignump (object) ...) -> boolean */
static int syscall_bignump(Execute ptr, addr var)
{
	bignump_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_bignump(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BIGNUMP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_bignump);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ratiop (object) ...) -> boolean */
static int syscall_ratiop(Execute ptr, addr var)
{
	ratiop_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_ratiop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_RATIOP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_ratiop);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun short-float-p (object) ...) -> boolean */
static int syscall_short_float_p(Execute ptr, addr var)
{
	short_float_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_short_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SHORT_FLOAT_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_short_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun single-float-p (object) ...) -> boolean */
static int syscall_single_float_p(Execute ptr, addr var)
{
	single_float_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_single_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SINGLE_FLOAT_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_single_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun double-float-p (object) ...) -> boolean */
static int syscall_double_float_p(Execute ptr, addr var)
{
	double_float_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_double_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DOUBLE_FLOAT_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_double_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun long-float-p (object) ...) -> boolean */
static int syscall_long_float_p(Execute ptr, addr var)
{
	long_float_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_long_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_LONG_FLOAT_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_long_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun callnamep (object) ...) -> boolean */
static int syscall_callnamep(Execute ptr, addr var)
{
	callnamep_syscall(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void defun_callnamep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_CALLNAMEP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_callnamep);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun large-number (value &optional (cardinal t)) ...) -> string
 *   value  (integer 0 fixnum-max)
 */
static int syscall_large_number(Execute ptr, addr var, addr opt)
{
	Return(large_number_syscode_(ptr->local, var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_large_number(addr *ret)
{
	addr args, values;

	type4integer_heap(Nil, 0, Nil, FIXNUM_MAX, &args);
	GetTypeTable(&values, T);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, ret);
}

static void defun_large_number(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_LARGE_NUMBER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_syscall_large_number);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_large_number(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-character (x) ...) -> character
 *   x:  (or character integer)
 */
static int syscall_make_character(Execute ptr, addr var)
{
	Return(make_character_syscode(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_character(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Character);
	GetTypeTable(&values, Integer);
	type2or_heap(args, values, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Character);
	type_compiled_heap(args, values, ret);
}

static void defun_make_character(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_CHARACTER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_character);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_character(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-fixnum (integer) ...) -> fixnum */
static int syscall_make_fixnum(Execute ptr, addr var)
{
	Return(make_fixnum_syscode(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_fixnum(addr *ret)
{
	addr args, values;

	GetTypeTable(&values, Fixnum);
	typeargs_var1(&args, values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_fixnum(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_FIXNUM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_fixnum);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_fixnum(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-bignum (integer) ...) -> bignum */
static int syscall_make_bignum(Execute ptr, addr var)
{
	Return(make_bignum_syscode(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_make_bignum(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Integer);
	type_compiled_heap(args, values, ret);
}

static void defun_make_bignum(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_BIGNUM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_bignum);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_bignum(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-ratio (numer denom) ...) -> ratio */
static int syscall_make_ratio(Execute ptr, addr numer, addr denom)
{
	Return(make_ratio_syscode(numer, denom, &numer));
	setresult_control(ptr, numer);
	return 0;
}

static void type_make_ratio(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Integer);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Rational);
	type_compiled_heap(args, values, ret);
}

static void defun_make_ratio(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_RATIO, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_make_ratio);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_ratio(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-complex (real imag) ...) -> complex */
static int syscall_make_complex(Execute ptr, addr real, addr imag)
{
	Return(make_complex_code_(real, imag, &real));
	setresult_control(ptr, real);
	return 0;
}

static void type_make_complex(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Real);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Complex);
	type_compiled_heap(args, values, ret);
}

static void defun_make_complex(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_COMPLEX, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_make_complex);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_complex(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equal-random-state (a b) ...) -> boolean */
static int syscall_equal_random_state(Execute ptr, addr left, addr right)
{
	equal_random_state_syscode(left, right, &left);
	setresult_control(ptr, left);
	return 0;
}

static void type_equal_random_state(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, RandomState);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_equal_random_state(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EQUAL_RANDOM_STATE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_equal_random_state);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_equal_random_state(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subtypep! (x y &optional env check) ...) -> symbol */
static int syscall_subtypep_extend(Execute ptr, addr x, addr y, addr env, addr check)
{
	Return(subtypep_extend_syscode_(ptr, x, y, env, check, &x));
	setresult_control(ptr, x);
	return 0;
}

static void type_syscall_subtypep_extend(addr *ret)
{
	addr args, values, env;

	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&env, EnvironmentNull);
	GetTypeTable(&values, T);
	typeargs_var2opt2(&args, args, args, env, values);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_subtypep_extend(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SUBTYPEP_EXTEND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt2(pos, p_defun_syscall_subtypep_extend);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_subtypep_extend(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eastasian-set (string-designer intplus &optional error) ...) -> boolean) */
static int syscall_eastasian_set(Execute ptr, addr var, addr value, addr errorp)
{
	Return(eastasian_set_syscode_(var, value, errorp, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_eastasian_set(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, StringDesigner);
	GetTypeTable(&values, Intplus);
	GetTypeTable(&type, T);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_eastasian_set(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EASTASIAN_SET, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_eastasian_set);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_eastasian_set(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eastasian-get (string-designer) ...) -> (values IntplusNull symbol) */
static int syscall_eastasian_get(Execute ptr, addr var)
{
	addr symbol;

	Return(eastasian_get_syscode_(var, &var, &symbol));
	setvalues_control(ptr, var, symbol, NULL);

	return 0;
}

static void type_syscall_eastasian_get(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, StringDesigner);
	typeargs_var1(&args, args);
	GetTypeTable(&values, IntplusNull);
	GetTypeTable(&type, Symbol);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_eastasian_get(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EASTASIAN_GET, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_eastasian_get);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_eastasian_get(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eastasian-width (var) ...) -> (values IntplusNull boolean)
 *   var  (or integer character string)
 */
static int syscall_eastasian_width(Execute ptr, addr pos)
{
	addr value;

	Return(eastasian_width_syscode_(pos, &pos, &value));
	setvalues_control(ptr, pos, value, NULL);

	return 0;
}

static void type_syscall_eastasian_width(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Integer);
	GetTypeTable(&values, Character);
	GetTypeTable(&type, String);
	type3or_heap(args, values, type, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, ret);
}

static void defun_eastasian_width(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EASTASIAN_WIDTH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_eastasian_width);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_eastasian_width(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun run-program (program args &rest args) ...) -> t */
static int syscall_run_program(Execute ptr, addr var, addr args, addr rest)
{
	Return(run_program_syscode_(ptr, var, args, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_run_program(addr *ret)
{
	addr args, values, rest;

	GetTypeTable(&args, String);
	GetTypeTable(&values, List);
	GetTypeTable(&rest, T);
	typeargs_var2rest(&args, args, values, rest);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_run_program(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_RUN_PROGRAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_run_program);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_run_program(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-callname (var) ...) -> callname */
static int syscall_make_callname(Execute ptr, addr var)
{
	Return(make_callname_syscode_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_make_callname(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, FunctionName);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_make_callname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_CALLNAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_callname);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_make_callname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun remove-file (pathname &optional (error t)) ...) -> boolean */
static int syscall_remove_file(Execute ptr, addr var, addr opt)
{
	Return(remove_file_syscode(ptr, var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_remove_file(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_REMOVE_FILE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_syscall_remove_file);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RemoveFile);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun remove-directory (pathname &optional (error t)) ...) -> boolean */
static int syscall_remove_directory(Execute ptr, addr var, addr opt)
{
	Return(remove_directory_syscode(ptr, var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_remove_directory(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_REMOVE_DIRECTORY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_syscall_remove_directory);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, RemoveFile);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro declare-parse (symbol) ...) -> integer */
static int syscall_declare_parse(Execute ptr, addr form, addr env)
{
	Return(declare_parse_syscode(form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_declare_parse(void)
{
	addr symbol, pos, type;

	GetConst(SYSTEM_DECLARE_PARSE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_syscall_declare_parse);
	setmacro_symbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun parse-type (object) ...) -> type */
static int syscall_parse_type(Execute ptr, addr var)
{
	Return(parse_type_syscode(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_parse_type(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeTable(&values, TypeSpec);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_parse_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARSE_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_parse_type);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_parse_type(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun upgraded-open-element-type (type) ...) -> type */
static int syscall_upgraded_open_element_type(Execute ptr, addr var)
{
	Return(upgraded_open_element_type_syscode_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void defun_upgraded_open_element_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_UPGRADED_OPEN_ELEMENT_TYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_upgraded_open_element_type);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, UpgradedType);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-memory-input-stream (sequence &key size array cache) ...) -> stream
 *   sequence  sequence
 *   size      (or null (integer 1 *))
 *   array     (or null (integer 1 *))
 *   cache     t  ;; boolean
 *   stream    input-memory-stream
 */
static int syscall_make_memory_input_stream(Execute ptr, addr var, addr rest)
{
	Return(make_memory_input_stream_syscode_(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_make_memory_input_stream(addr *ret)
{
	addr args, values, key1, key2, key3, key;

	/* key */
	KeyTypeTable(&key1, SIZE, Plus1Null);
	KeyTypeTable(&key2, ARRAY, Plus1Null);
	KeyTypeTable(&key3, CACHE, T);
	list_heap(&key, key1, key2, key3, NULL);
	/* type */
	GetTypeTable(&args, Sequence);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, MemoryStream);
	type_compiled_heap(args, values, ret);
}

static void defun_make_memory_input_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_MEMORY_INPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_make_memory_input_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_make_memory_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-memroy-output-stream (&key input size array cache) ...) -> stream
 *   input     sequence
 *   size      (or null (integer 1 *))
 *   array     (or null (integer 1 *))
 *   cache     t  ;; boolean
 *   stream    output-memory-stream
 */
static int syscall_make_memory_output_stream(Execute ptr, addr rest)
{
	Return(make_memory_output_stream_syscode_(rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_make_memory_output_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_MEMORY_OUTPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_make_memory_output_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MakeMemoryOutputStream);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-memroy-io-stream (&key input size array cache) ...) -> stream
 *   input     sequence
 *   size      (or null (integer 1 *))
 *   array     (or null (integer 1 *))
 *   cache     t  ;; boolean
 *   stream    io-memory-stream
 */
static int syscall_make_memory_io_stream(Execute ptr, addr rest)
{
	Return(make_memory_io_stream_syscode_(rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void defun_make_memory_io_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_MEMORY_IO_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_make_memory_io_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MakeMemoryOutputStream);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-input-from-memory
 *   ((stream vector &key size array) declaration* form*) ...)
 *   -> result
 */
static int syscall_with_input_from_memory(Execute ptr, addr form, addr env)
{
	Return(with_input_from_memory_syscode_(ptr, form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_input_from_memory(void)
{
	addr symbol, pos, type;

	GetConst(SYSTEM_WITH_INPUT_FROM_MEMORY, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_syscall_with_input_from_memory);
	setmacro_symbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro with-output-to-memory
 *   ((var &key input size array) declaration* form*) ...)
 *   -> result
 */
static int syscall_with_output_to_memory(Execute ptr, addr form, addr env)
{
	Return(with_output_to_memory_syscode_(ptr, form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_output_to_memory(void)
{
	addr symbol, pos, type;

	GetConst(SYSTEM_WITH_OUTPUT_TO_MEMORY, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_syscall_with_output_to_memory);
	setmacro_symbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun get-output-stream-memory (stream) ...) -> vector */
static int syscall_get_output_stream_memory(Execute ptr, addr var)
{
	Return(get_output_stream_memory_syscode_(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_get_output_stream_memory(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, MemoryStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

static void defun_get_output_stream_memory(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_GET_OUTPUT_STREAM_MEMORY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_get_output_stream_memory);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_get_output_stream_memory(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun memory-stream-p (t) ...) -> (member :input :output :io nil) */
static int syscall_memory_stream_p(Execute ptr, addr var)
{
	memory_stream_p_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_memory_stream_p(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetConst(KEYWORD_INPUT, &type1);
	GetConst(KEYWORD_OUTPUT, &type2);
	GetConst(KEYWORD_IO, &type3);
	type_member_heap(&values, type1, type2, type3, Nil, NULL);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_memory_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MEMORY_STREAM_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_memory_stream_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_memory_stream_p(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf memory-stream-p) (result stream) ...) -> result
 *   stream  memory-stream
 *   result  (member :input :output :io)
 */
static int syscall_setf_memory_stream_p(Execute ptr, addr value, addr var)
{
	Return(setf_memory_stream_p_syscode_(var, value));
	setresult_control(ptr, value);
	return 0;
}

static void type_syscall_setf_memory_stream_p(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&args, MemoryStream);
	GetConst(KEYWORD_INPUT, &type1);
	GetConst(KEYWORD_OUTPUT, &type2);
	GetConst(KEYWORD_IO, &type3);
	type_member_heap(&values, type1, type2, type3, NULL);
	typeargs_var2(&args, values, args);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_memory_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MEMORY_STREAM_P, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_setf_memory_stream_p);
	setsetf_symbol(symbol, pos);
	/* type */
	type_syscall_setf_memory_stream_p(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun byte-integer (&rest unsigned-byte-8) ...) -> (integer 0 *) */
static int syscall_byte_integer(Execute ptr, addr list)
{
	Return(byte_integer_syscode_(list, &list));
	setresult_control(ptr, list);
	return 0;
}

static void type_syscall_byte_integer(addr *ret)
{
	addr args, values;

	type4integer_heap(Nil, 0, Nil, 0xFF, &args);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Intplus);
	type_compiled_heap(args, values, ret);
}

static void defun_byte_integer(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BYTE_INTEGER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_byte_integer);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_byte_integer(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun question (object &rest args) ...) -> (values &rest t) */
static int syscall_question(Execute ptr, addr var, addr args)
{
	return question_syscode_(ptr, var, args);
}

static void type_syscall_question(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, T);
	typeargs_var1rest(&args, type, type);
	typevalues_rest(&values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_question(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_QUESTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_question);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_question(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun extension (t) ...) -> t */
static int syscall_extension(Execute ptr, addr var)
{
	return extension_syscode(ptr, var);
}

static void type_syscall_extension(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_extension(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EXTENSION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_extension);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_extension(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_syscall_function(void)
{
	SetPointerSysCall(defun, empty, hello);
	SetPointerSysCall(defun, dynamic, infobit);
	SetPointerSysCall(defun, dynamic, infoprint);
	SetPointerSysCall(defun, dynamic, gc);
	SetPointerSysCall(defun, var1, savecore);
	SetPointerSysCall(defun, var1, package_export_list);
	SetPointerSysCall(defun, var1, specialp);
	SetPointerSysCall(defun, var1, array_general_p);
	SetPointerSysCall(defun, var1, array_specialized_p);
	SetPointerSysCall(defun, var2dynamic, simple_sort);
	SetPointerSysCall(defun, var2dynamic, bubble_sort);
	SetPointerSysCall(defun, var2dynamic, quick_sort);
	SetPointerSysCall(defun, var2dynamic, merge_sort);
	SetPointerSysCall(defun, opt1, exit);
	SetPointerSysCall(defun, var1, closp);
	SetPointerSysCall(defun, var1, fixnump);
	SetPointerSysCall(defun, var1, bignump);
	SetPointerSysCall(defun, var1, ratiop);
	SetPointerSysCall(defun, var1, callnamep);
	SetPointerSysCall(defun, var1, short_float_p);
	SetPointerSysCall(defun, var1, single_float_p);
	SetPointerSysCall(defun, var1, double_float_p);
	SetPointerSysCall(defun, var1, long_float_p);
	SetPointerSysCall(defun, var1opt1, large_number);
	SetPointerSysCall(defun, var1, make_character);
	SetPointerSysCall(defun, var1, make_fixnum);
	SetPointerSysCall(defun, var1, make_bignum);
	SetPointerSysCall(defun, var2, make_ratio);
	SetPointerSysCall(defun, var2, make_complex);
	SetPointerSysCall(defun, var2, equal_random_state);
	SetPointerSysCall(defun, var2opt2, subtypep_extend);
	SetPointerSysCall(defun, var3, eastasian_set);
	SetPointerSysCall(defun, var1, eastasian_get);
	SetPointerSysCall(defun, var1, eastasian_width);
	SetPointerSysCall(defun, var2dynamic, run_program);
	SetPointerSysCall(defun, var1, make_callname);
	SetPointerSysCall(defun, var1opt1, remove_file);
	SetPointerSysCall(defun, var1opt1, remove_directory);
	SetPointerSysCall(defmacro, macro, declare_parse);
	SetPointerSysCall(defun, var1, parse_type);
	SetPointerSysCall(defun, var1, upgraded_open_element_type);
	SetPointerSysCall(defun, var1dynamic, make_memory_input_stream);
	SetPointerSysCall(defun, dynamic, make_memory_output_stream);
	SetPointerSysCall(defun, dynamic, make_memory_io_stream);
	SetPointerSysCall(defmacro, macro, with_input_from_memory);
	SetPointerSysCall(defmacro, macro, with_output_to_memory);
	SetPointerSysCall(defun, var1, get_output_stream_memory);
	SetPointerSysCall(defun, var1, memory_stream_p);
	SetPointerSysCall(defun, var2, setf_memory_stream_p);
	SetPointerSysCall(defun, dynamic, byte_integer);
	SetPointerSysCall(defun, var1dynamic, question);
	SetPointerSysCall(defun, var1, extension);
}

void build_syscall_function(void)
{
	defun_hello();
	defun_infobit();
	defun_infoprint();
	defun_gc();
	defun_savecore();
	defun_package_export_list();
	defun_specialp();
	defun_array_general_p();
	defun_array_specialized_p();
	defun_simple_sort();
	defun_bubble_sort();
	defun_quick_sort();
	defun_merge_sort();
	defun_exit();
	defun_quit();
	defun_closp();
	defun_fixnump();
	defun_bignump();
	defun_ratiop();
	defun_short_float_p();
	defun_single_float_p();
	defun_double_float_p();
	defun_long_float_p();
	defun_callnamep();
	defun_large_number();
	defun_make_character();
	defun_make_fixnum();
	defun_make_bignum();
	defun_make_ratio();
	defun_make_complex();
	defun_equal_random_state();
	defun_subtypep_extend();
	defun_eastasian_set();
	defun_eastasian_get();
	defun_eastasian_width();
	defun_run_program();
	defun_make_callname();
	defun_remove_file();
	defun_remove_directory();
	defmacro_declare_parse();
	defun_parse_type();
	defun_upgraded_open_element_type();
	defun_make_memory_input_stream();
	defun_make_memory_output_stream();
	defun_make_memory_io_stream();
	defmacro_with_input_from_memory();
	defmacro_with_output_to_memory();
	defun_get_output_stream_memory();
	defun_memory_stream_p();
	defun_setf_memory_stream_p();
	defun_byte_integer();
	defun_question();
	defun_extension();
}

