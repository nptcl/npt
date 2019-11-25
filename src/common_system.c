/*
 *  ANSI COMMON LISP: 24. System Construction
 */
#include "common_header.h"
#include "cons.h"
#include "cons_plist.h"
#include "eval.h"
#include "file.h"
#include "type_parse.h"

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


/* (defvar *load-print* boolean) */
static void defvar_load_print(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_LOAD_PRINT, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, Boolean);
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
	GetTypeTable(&type, Boolean);
	settype_value_symbol(symbol, type);
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

static void function_load(Execute ptr, addr filespec, addr rest)
{
	int exist, check;
	addr verbose, print, external;

	function_load_verbose(ptr, rest, &verbose);
	function_load_print(ptr, rest, &print);
	function_load_exist(ptr, rest, &exist);
	function_load_external(ptr, rest, &external);
	if (eval_load(ptr, &check, filespec, verbose, print, exist, external)) return;
	setbool_control(ptr, check);
}

static void type_load(addr *ret)
{
	addr arg, values, type, key1, key2, key3, key4, key;

	/* arg */
	GetTypeTable(&arg, Stream);
	GetTypeTable(&type, PathnameDesigner);
	type2or_heap(arg, type, &arg);
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
	typeargs_var1key(&arg, arg, key);
	/* values */
	GetTypeValues(&values, Boolean);
	/* result */
	type_compiled_heap(arg, values, ret);
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


/*
 *  function
 */
_g void init_common_system(void)
{
	SetPointerCall(defun, var1dynamic, load);
}

_g void build_common_system(void)
{
	defvar_features();
	defvar_compile_file_pathname();
	defvar_compile_file_truename();
	defvar_load_pathname();
	defvar_load_truename();
	defvar_load_print();
	defvar_load_verbose();
	defun_load();
}

