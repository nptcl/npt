/*
 *  ANSI COMMON LISP: 24. System Construction
 */
#include "call_system.h"
#include "common_header.h"
#include "compile.h"
#include "cons.h"
#include "cons_plist.h"
#include "control.h"
#include "eval.h"
#include "file.h"
#include "require.h"
#include "type_parse.h"

/* (defun compile-file
 *     (input-file &key output-file verbose print external-format)
 *     -> output-truename, warnings-p, failure-p
 *   input-file       pathname-designer ;; merge *default-pathname-defaults*
 *   output-file      pathname-designer
 *   verbose          T  ;; boolean, *compile-verbose*
 *   print            T  ;; boolean, *compile-print*
 *   external-format  external-format-designer
 *   output-truename  (or pathname null)  ;; truename
 *   warnings-p       boolean
 *   failure-p        boolean
 */
static int function_compile_file(Execute ptr, addr file, addr rest)
{
	addr ret1, ret2, ret3;

	Return(compile_file_common(ptr, file, rest, &ret1, &ret2, &ret3));
	setvalues_control(ptr, ret1, ret2, ret3, NULL);
	return 0;
}

static void type_compile_file(addr *ret)
{
	addr args, values, key, key1, key2, key3, key4;
	addr type1, type2;

	/* key */
	KeyTypeTable(&key1, OUTPUT_FILE, PathnameDesigner);
	KeyTypeTable(&key2, VERBOSE, T);
	KeyTypeTable(&key3, PRINT, T);
	KeyTypeTable(&key4, EXTERNAL_FORMAT, ExternalFormat);
	list_heap(&key, key1, key2, key3, key4, NULL);

	/* type */
	GetTypeTable(&args, PathnameDesigner);
	typeargs_var1key(&args, args, key);
	GetTypeTable(&type1, PathnameNull);
	GetTypeTable(&type2, Boolean);
	typevalues_values3(&values, type1, type2, type2);
	type_compiled_heap(args, values, ret);
}

static void defun_compile_file(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILE_FILE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_compile_file);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_compile_file(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun compile-file-pathname
 *     (input-file &key output-file &allow-other-keys)
 *     -> pathname
 *   input-file       pathname-designer ;; merge *default-pathname-defaults*
 *   output-file      pathname-designer
 *   pathname         pathname
 */
static int function_compile_file_pathname(Execute ptr, addr var, addr rest)
{
	compile_file_pathname_common(ptr, var, rest, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_compile_file_pathname(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesigner);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Pathname);
	type_compiled_heap(args, values, ret);
}

static void defun_compile_file_pathname(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPILE_FILE_PATHNAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_compile_file_pathname);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_compile_file_pathname(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun load
 *     (filespec &key verbose print if-does-not-exist external-format) ...)
 *     -> boolean
 *   filespec           (or stream pathname-designer)
 *   verbose            t  ;; boolean
 *   print              t  ;; boolean
 *   if-does-not-exist  t  ;; boolean
 *   external-format    t  ;; external-format-designer
 */
static void function_load_verbose(Execute ptr, addr rest, addr *ret)
{
	if (getplist_constant_safe(rest, CONSTANT_KEYWORD_VERBOSE, ret))
		*ret = Unbound;
}

static void function_load_print(Execute ptr, addr rest, addr *ret)
{
	if (getplist_constant_safe(rest, CONSTANT_KEYWORD_PRINT, ret))
		*ret = Unbound;
}

static void function_load_exist(Execute ptr, addr rest, int *ret)
{
	if (getplist_constant_safe(rest, CONSTANT_KEYWORD_IF_DOES_NOT_EXIST, &rest))
		*ret = 1;
	else
		*ret = (rest != Nil);
}

static void function_load_external(Execute ptr, addr rest, addr *ret)
{
	addr pos, check;

	if (getplist_constant_safe(rest, CONSTANT_KEYWORD_EXTERNAL_FORMAT, &pos)) {
		*ret = Unbound;
	}
	else {
		GetConst(KEYWORD_DEFAULT, &check);
		*ret = (check == pos)? Unbound: pos;
	}
}

static int function_load(Execute ptr, addr filespec, addr rest)
{
	int exist, check;
	addr verbose, print, external;

	function_load_verbose(ptr, rest, &verbose);
	function_load_print(ptr, rest, &print);
	function_load_exist(ptr, rest, &exist);
	function_load_external(ptr, rest, &external);
	Return(eval_load(ptr, &check, filespec, verbose, print, exist, external));
	setbool_control(ptr, check);

	return 0;
}

static void type_load(addr *ret)
{
	addr args, values, type, key1, key2, key3, key4, key;

	/* args */
	GetTypeTable(&args, Stream);
	GetTypeTable(&type, PathnameDesigner);
	type2or_heap(args, type, &args);
	GetTypeTable(&type, T);
	GetConst(KEYWORD_VERBOSE, &key1);
	cons_heap(&key1, key1, type);
	GetConst(KEYWORD_PRINT, &key2);
	cons_heap(&key2, key2, type);
	GetConst(KEYWORD_IF_DOES_NOT_EXIST, &key3);
	cons_heap(&key3, key3, type);
	GetConst(KEYWORD_EXTERNAL_FORMAT, &key4);
	cons_heap(&key4, key4, type);
	list_heap(&key, key1, key2, key3, key4, NULL);
	typeargs_var1key(&args, args, key);
	/* values */
	GetTypeValues(&values, Boolean);
	/* result */
	type_compiled_heap(args, values, ret);
}

static void defun_load(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LOAD, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_load);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_load(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-compilation-unit
 *     ((&key &allow-other-keys) &body body) ...)
 *     -> result
 */
static int function_with_compilation_unit(Execute ptr, addr form, addr env)
{
	with_compilation_unit_common(form, &form);
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_compilation_unit(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_COMPILATION_UNIT, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_compilation_unit);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defvar *features* list) */
static void defvar_features(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_FEATURES, &symbol);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, List);
	settype_value_symbol(symbol, type);
}


/* (defvar *compile-file-pathname* (or pathname null)) */
static void defvar_compile_file_pathname(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_COMPILE_FILE_PATHNAME, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PathnameNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *compile-file-truename* (or pathname null)) */
static void defvar_compile_file_truename(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_COMPILE_FILE_TRUENAME, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PathnameNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *load-pathname* (or pathname null)) */
static void defvar_load_pathname(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_LOAD_PATHNAME, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PathnameNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *load-truename* (or pathname null)) */
static void defvar_load_truename(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_LOAD_TRUENAME, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, PathnameNull);
	settype_value_symbol(symbol, type);
}


/* (defvar *compile-print* boolean) */
static void defvar_compile_print(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_COMPILE_PRINT, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *compile-verbose* boolean) */
static void defvar_compile_verbose(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_COMPILE_VERBOSE, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *load-print* boolean) */
static void defvar_load_print(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_LOAD_PRINT, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *load-verbose* boolean) */
static void defvar_load_verbose(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_LOAD_VERBOSE, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, T);
	settype_value_symbol(symbol, type);
}


/* (defvar *modules* boolean) */
static void defvar_modules(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_MODULES, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, List);
	settype_value_symbol(symbol, type);
}


/* (defun provide (var) ...) -> null */
static int function_provide(Execute ptr, addr var)
{
	provide_common(ptr, var);
	setresult_control(ptr, Nil);
	return 0;
}

static void type_provide(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_provide(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PROVIDE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_provide);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_provide(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun require (var) ...) -> null */
static int function_require(Execute ptr, addr var, addr opt)
{
	Return(require_common(ptr, var, opt));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_require(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, StringDesigner);
	GetTypeTable(&values, List);
	GetTypeTable(&type, PathnameDesigner);
	type2or_heap(values, type, &values);
	typeargs_var1opt1(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_require(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_REQUIRE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_require);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_require(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_system(void)
{
	SetPointerCall(defun, var1dynamic, compile_file);
	SetPointerCall(defun, var1dynamic, compile_file_pathname);
	SetPointerCall(defmacro, macro, with_compilation_unit);
	SetPointerCall(defun, var1dynamic, load);
	SetPointerCall(defun, var1, provide);
	SetPointerCall(defun, var1opt1, require);
}

_g void build_common_system(void)
{
	defun_compile_file();
	defun_compile_file_pathname();
	defun_load();
	defmacro_with_compilation_unit();
	defvar_features();
	defvar_compile_file_pathname();
	defvar_compile_file_truename();
	defvar_load_pathname();
	defvar_load_truename();
	defvar_compile_print();
	defvar_compile_verbose();
	defvar_load_print();
	defvar_load_verbose();
	defvar_modules();
	defun_provide();
	defun_require();
}

