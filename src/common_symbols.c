/*
 *  ANSI COMMON LISP: 10. Symbols
 */
#include "call_symbols.h"
#include "common_header.h"
#include "restart_value.h"

/* (defvar *gensym-counter* 1) */
static void defvar_gensym_counter(void)
{
	addr symbol, value, type;

	/* symbol */
	GetConst(SPECIAL_GENSYM_COUNTER, &symbol);
	fixnum_heap(&value, 1);
	SetValueSymbol(symbol, value);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, Integer);
	settype_value_symbol(symbol, type);
}


/* (defun symbolp (object) ...) -> boolean */
static int function_symbolp(Execute ptr, addr var)
{
	setbool_control(ptr, symbolp(var));
	return 0;
}

static void defun_symbolp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOLP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbolp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun keywordp (object) ...) -> boolean */
static int function_keywordp(Execute ptr, addr var)
{
	setbool_control(ptr, keywordp(var));
	return 0;
}

static void defun_keywordp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_KEYWORDP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_keywordp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-symbol (string) ...) -> symbol */
static int function_make_symbol(Execute ptr, addr var)
{
	make_symbol_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_make_symbol(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, String);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_make_symbol(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_SYMBOL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_make_symbol);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_symbol(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun copy-symbol (symbol &optional boolean) ...) -> symbol */
static int function_copy_symbol(Execute ptr, addr var, addr opt)
{
	Return(copy_symbol_common_(var, opt, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_copy_symbol(addr *ret)
{
	addr args, values, boolean;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&boolean, Boolean);
	typeargs_var1opt1(&args, args, boolean);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_copy_symbol(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COPY_SYMBOL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_copy_symbol);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_copy_symbol(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun gensym (&optional x) ...) -> symbol
 *   x  (or string Intplus)
 */
static int function_gensym(Execute ptr, addr opt)
{
	Return(gensym_common_(ptr, opt, &opt));
	setresult_control(ptr, opt);
	return 0;
}

static void type_gensym(addr *ret)
{
	addr args, values, pos;

	GetTypeTable(&args, String);
	GetTypeTable(&pos, Intplus);
	type2or_heap(args, pos, &args);
	typeargs_opt1(&args, args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_gensym(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GENSYM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_gensym);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_gensym(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun gentemp (&optional prefix package) ...) -> symbol
 *   prefix   string
 *   package  (or package string symbol character)  ;; package-designator
 */
static int function_gentemp(Execute ptr, addr opt1, addr opt2)
{
	Return(gentemp_common_(ptr, opt1, opt2, &opt1));
	setresult_control(ptr, opt1);
	return 0;
}

static void type_gentemp(addr *ret)
{
	addr args, values, pos;

	GetTypeTable(&args, String);
	GetTypeTable(&pos, PackageDesignator);
	typeargs_opt2(&args, args, pos);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_gentemp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GENTEMP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt2(pos, p_defun_gentemp);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_gentemp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun symbol-function (symbol) ...) -> function */
static int function_symbol_function(Execute ptr, addr var)
{
	Return(function_global_restart_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_function(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_FUNCTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbol_function);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_symbol_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf symbol-function) (function symbol) ...) -> function */
static int function_setf_symbol_function(Execute ptr, addr value, addr symbol)
{
	Return(setf_symbol_function_common_(value, symbol));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_symbol_function(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Function);
	GetTypeTable(&type, Symbol);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, Function);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_symbol_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_FUNCTION, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_symbol_function);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_symbol_function(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun symbol-value (symbol) ...) -> object */
static int function_symbol_value(Execute ptr, addr var)
{
	Return(symbol_special_restart_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_value(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_VALUE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbol_value);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_symbol_value(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf symbol-value) (object symbol) ...) -> object */
static int function_setf_symbol_value(Execute ptr, addr value, addr symbol)
{
	Return(setf_symbol_value_common_(ptr, value, symbol));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_symbol_value(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&type, Symbol);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_symbol_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_VALUE, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_symbol_value);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_symbol_value(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun symbol-plist (symbol) ...) -> list */
static int function_symbol_plist(Execute ptr, addr var)
{
	GetPlistSymbol(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_plist(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_plist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_PLIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbol_plist);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_symbol_plist(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun (setf symbol-plist) (list symbol) ...) -> list */
static int function_setf_symbol_plist(Execute ptr, addr value, addr symbol)
{
	Return(setf_symbol_plist_common_(value, symbol));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_symbol_plist(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, List);
	GetTypeTable(&type, Symbol);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_symbol_plist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_PLIST, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_setf_symbol_plist);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_symbol_plist(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun symbol-name (symbol) ...) -> string */
static int function_symbol_name(Execute ptr, addr var)
{
	GetNameSymbol(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_NAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbol_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_symbol_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun symbol-package (symbol) ...) -> contents
 *   contents  (or package null)
 */
static int function_symbol_package(Execute ptr, addr var)
{
	GetPackageSymbol(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_package(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, PackageNull);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYMBOL_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_symbol_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_symbol_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get (symbol indicator &optional default) ...) -> object
 *   symbol     symbol
 *   indicator  object
 *   default    object  ;; default nil
 */
static int function_get(Execute ptr, addr var1, addr var2, addr opt)
{
	Return(get_common_(var1, var2, opt, &var1));
	setresult_control(ptr, var1);
	return 0;
}

static void type_get(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&type, T);
	typeargs_var2opt1(&args, args, type, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_get(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_get);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 * (defun (setf get) (symbol indicator &optional default) ...) -> object
 *   symbol     symbol
 *   indicator  object
 *   default    object  ;; default nil
 */
static int function_setf_get(Execute ptr,
		addr value, addr symbol, addr key, addr ignored)
{
	Return(setf_get_common_(value, symbol, key));
	setresult_control(ptr, value);
	return 0;
}

static void type_setf_get(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&type, T);
	typeargs_var3opt1(&args, type, args, type, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_setf_get(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET, &symbol);
	compiled_setf_system(&pos, symbol);
	setcompiled_var3opt1(pos, p_defun_setf_get);
	setsetf_symbol(symbol, pos);
	/* type */
	type_setf_get(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/* (defun remprop (symbol indicator) ...) -> boolean
 *   symbol     symbol
 *   indicator  object
 */
static int function_remprop(Execute ptr, addr symbol, addr key)
{
	Return(remprop_common_(symbol, key, &symbol));
	setresult_control(ptr, symbol);
	return 0;
}

static void type_remprop(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&type, T);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_remprop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REMPROP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_remprop);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_remprop(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun boundp (symbol) ...) -> boolean */
static int function_boundp(Execute ptr, addr var)
{
	getspecial_local(ptr, var, &var);
	setbool_control(ptr, var != Unbound);
	return 0;
}

static void defun_boundp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BOUNDP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_boundp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Symbol_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun makunbound (symbol) ...) -> symbol */
static int function_makunbound(Execute ptr, addr symbol)
{
	Return(makunbound_common_(ptr, symbol));
	setresult_control(ptr, symbol);
	return 0;
}

static void type_makunbound(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_makunbound(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKUNBOUND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_makunbound);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_makunbound(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun set (symbol value) ...) -> value */
static int function_set(Execute ptr, addr symbol, addr value)
{
	Return(set_common_(ptr, symbol, value));
	setresult_control(ptr, value);
	return 0;
}

static void type_set(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&type, T);
	typeargs_var2(&args, args, type);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_set(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SET, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_set);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_set(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_symbols(void)
{
	SetPointerCall(defun, var1, symbolp);
	SetPointerCall(defun, var1, keywordp);
	SetPointerCall(defun, var1, make_symbol);
	SetPointerCall(defun, var1opt1, copy_symbol);
	SetPointerCall(defun, opt1, gensym);
	SetPointerCall(defun, opt2, gentemp);
	SetPointerCall(defun, var1, symbol_function);
	SetPointerCall(defun, var2, setf_symbol_function);
	SetPointerCall(defun, var1, symbol_value);
	SetPointerCall(defun, var2, setf_symbol_value);
	SetPointerCall(defun, var1, symbol_plist);
	SetPointerCall(defun, var2, setf_symbol_plist);
	SetPointerCall(defun, var1, symbol_name);
	SetPointerCall(defun, var1, symbol_package);
	SetPointerCall(defun, var2opt1, get);
	SetPointerCall(defun, var3opt1, setf_get);
	SetPointerCall(defun, var2, remprop);
	SetPointerCall(defun, var1, boundp);
	SetPointerCall(defun, var1, makunbound);
	SetPointerCall(defun, var2, set);
}

void build_common_symbols(void)
{
	defvar_gensym_counter();
	defun_symbolp();
	defun_keywordp();
	defun_make_symbol();
	defun_copy_symbol();
	defun_gensym();
	defun_gentemp();
	defun_symbol_function();
	defun_setf_symbol_function();
	defun_symbol_value();
	defun_setf_symbol_value();
	defun_symbol_plist();
	defun_setf_symbol_plist();
	defun_symbol_name();
	defun_symbol_package();
	defun_get();
	defun_setf_get();
	defun_remprop();
	defun_boundp();
	defun_makunbound();
	defun_set();
}

