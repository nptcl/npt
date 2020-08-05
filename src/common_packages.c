/*
 *  ANSI COMMON LISP: 11. Packages
 */
#include "call_packages.h"
#include "common_header.h"
#include "cons.h"
#include "cons_list.h"
#include "package.h"
#include "package_object.h"

/* (defun export (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  (or string character symbol package) ;; package-designer
 */
static int function_export(Execute ptr, addr symbols, addr package)
{
	Return(export_common_(ptr, symbols, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_export(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EXPORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_export);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find-symbol (string &optional package) ...) -> symbol, status
 *   package  package-designer
 *   status   (member :inherited :external :interal nil)
 */
static int function_find_symbol(Execute ptr, addr name, addr package)
{
	Return(find_symbol_common_(ptr, name, package, &name, &package));
	setvalues_control(ptr, name, package, NULL);
	return 0;
}

static void defun_find_symbol(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_SYMBOL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_find_symbol);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intern);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find-package (name) ...) -> package */
static int function_find_package(Execute ptr, addr name)
{
	Return(find_package_(name, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_find_package(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, StringDesigner);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, PackageNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_find_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_find_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find-all-symbols (string) ...) -> symbols
 *   string   string-designer
 *   symbols  list
 */
static int function_find_all_symbols(Execute ptr, addr name)
{
	Return(find_allsymbols_package_(name, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_find_all_symbols(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, StringDesigner);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_find_all_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_ALL_SYMBOLS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_find_all_symbols);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_all_symbols(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun import (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  package-designer
 */
static int function_import(Execute ptr, addr symbols, addr package)
{
	Return(import_common_(ptr, symbols, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_import(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_IMPORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_import);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list-all-packages () ...) -> list */
static int function_list_all_packages(Execute ptr)
{
	addr list;

	Return(list_all_packages_(&list));
	setresult_control(ptr, list);

	return 0;
}

static void type_list_all_packages(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_list_all_packages(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LIST_ALL_PACKAGES, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_list_all_packages);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_list_all_packages(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rename-package (package name &optional nicknames) ...) -> value
 *   package    package-designer
 *   name       package-designer
 *   nicknames  list
 *   value      package
 */
static int function_rename_package(Execute ptr,
		addr package, addr name, addr nicknames)
{
	Return(rename_package_common_(ptr, package, name, nicknames, &package));
	setresult_control(ptr, package);
	return 0;
}

static void type_rename_package(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, PackageDesigner);
	GetTypeTable(&values, List);
	typeargs_var2opt1(&arg, arg, arg, values);
	GetTypeValues(&values, Package);
	type_compiled_heap(arg, values, ret);
}

static void defun_rename_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RENAME_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_rename_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_rename_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun shadow (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list string-designer)
 *   package  package-designer
 */
static int function_shadow(Execute ptr, addr symbols, addr package)
{
	Return(shadow_common_(ptr, symbols, package));
	setresult_control(ptr, T);
	return 0;
}

static void type_shadow(addr *ret)
{
	addr arg, values, type1, type2;

	GetTypeTable(&type1, List);
	GetTypeTable(&type2, StringDesigner);
	type2or_heap(type1, type2, &arg);
	GetTypeTable(&values, PackageDesigner);
	typeargs_var1opt1(&arg, arg, values);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(arg, values, ret);
}

static void defun_shadow(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SHADOW, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_shadow);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_shadow(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun shadowing-import (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  package-designer
 */
static int function_shadowing_import(Execute ptr, addr symbols, addr package)
{
	Return(shadowing_import_common_(ptr, symbols, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_shadowing_import(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SHADOWING_IMPORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_shadowing_import);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete-package (pacakge) ...) -> booelan
 *   package  package-designer
 */
static int function_delete_package(Execute ptr, addr package)
{
	int check;

	Return(delete_package_(package, &check));
	setbool_control(ptr, ! check);

	return 0;
}

static void type_delete_package(addr *ret)
{
	addr arg, values;

	GetTypeArgs(&arg, PackageDesigner);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_delete_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DELETE_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_delete_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_delete_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-package (name &key nicknames use) ...) -> package
 *   name       string-designer
 *   nicknames  list
 *   use        list
 *   package    package
 */
static int function_make_package(Execute ptr, addr name, addr rest)
{
	Return(make_package_common_(ptr, name, rest, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_make_package(addr *ret)
{
	addr arg, values, type1, type2, symbol, type, key;

	/* arg */
	GetTypeTable(&arg, StringDesigner);
	GetTypeTable(&type, List);
	GetConst(KEYWORD_NICKNAMES, &symbol);
	cons_heap(&type1, symbol, type);
	GetConst(KEYWORD_USE, &symbol);
	cons_heap(&type2, symbol, type);
	list_heap(&key, type1, type2, NULL);
	typeargs_var1key(&arg, arg, key);
	/* values */
	GetTypeValues(&values, Package);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-package-iterator ((name list &rest types) &body body) ...) */
static int function_with_package_iterator(Execute ptr, addr form, addr env)
{
	Return(with_package_iterator_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_package_iterator(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_PACKAGE_ITERATOR, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_package_iterator);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun unexport (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  (or string character symbol package) ;; package-designer
 */
static int function_unexport(Execute ptr, addr symbols, addr package)
{
	Return(unexport_common_(ptr, symbols, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_unexport(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNEXPORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unexport);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun unintern (symbol &optional package) ...) -> boolean
 *   package  package-designer
 */
static int function_unintern(Execute ptr, addr symbol, addr package)
{
	Return(unintern_common_(ptr, symbol, package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void type_unintern(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Symbol);
	GetTypeTable(&values, PackageDesigner);
	typeargs_var1opt1(&arg, arg, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_unintern(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNINTERN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unintern);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_unintern(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro in-package (name) ...) */
static int function_in_package(Execute ptr, addr form, addr env)
{
	Return(in_package_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_in_package(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_IN_PACKAGE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_in_package);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun unuse-package (list &optional package) ...) -> t
 *    list     (or package-designer list)
 *    package  package-designer
 */
static int function_unuse_package(Execute ptr, addr unuse, addr package)
{
	Return(unuse_package_common_(ptr, unuse, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_unuse_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNUSE_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unuse_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, UsePackage);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun use-package (list &optional package) ...) -> t
 *    list     (or package-designer list)
 *    package  package-designer
 */
static int function_use_package(Execute ptr, addr use, addr package)
{
	Return(use_package_common_(ptr, use, package));
	setresult_control(ptr, T);
	return 0;
}

static void defun_use_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_USE_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_use_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, UsePackage);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro defpackage (name &rest options) ...) -> package
 *   options ::= (:nicknames nickname*)* |
 *               (:documentation string) |
 *               (:use package-name*)* |
 *               (:shadow {symbol-name}*)* |
 *               (:shadowing-import-from package-name {symbol-name}*)* |
 *               (:import-from package-name {symbol-name}*)* |
 *               (:export {symbol-name}*)* |
 *               (:intern {symbol-name}*)* |
 *               (:size integer)
 */
static int function_defpackage(Execute ptr, addr form, addr env)
{
	Return(defpackage_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_defpackage(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFPACKAGE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defpackage);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro do-symbols (var &optional package result) . tagbody) */
static int function_do_symbols(Execute ptr, addr form, addr env)
{
	Return(do_symbols_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_do_symbols(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO_SYMBOLS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do_symbols);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro do-external-symbols (var &optional package result) . tagbody) */
static int function_do_external_symbols(Execute ptr, addr form, addr env)
{
	Return(do_external_symbols_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_do_external_symbols(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO_EXTERNAL_SYMBOLS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do_external_symbols);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro do-all-symbols (var &optional result) . tagbody) */
static int function_do_all_symbols(Execute ptr, addr form, addr env)
{
	Return(do_all_symbols_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_do_all_symbols(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO_ALL_SYMBOLS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do_all_symbols);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun intern (string &optional package) ...) -> symbol, status
 *   package  package-designer
 *   status   (member :inherited :external :interal nil)
 */
static int function_intern(Execute ptr, addr name, addr package)
{
	Return(intern_common_(ptr, name, package, &name, &package));
	setvalues_control(ptr, name, package, NULL);
	return 0;
}

static void defun_intern(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTERN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_intern);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intern);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-name (package) ...) -> name
 *   package  package-designer
 *   name     (or string null)
 */
static int function_package_name(Execute ptr, addr package)
{
	Return(getname_package_(package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void type_package_name(addr *ret)
{
	addr arg, values;

	GetTypeArgs(&arg, PackageDesigner);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_package_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_NAME, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_package_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-nicknames (package) ...) -> list */
static int function_package_nicknames(Execute ptr, addr package)
{
	Return(getnickname_package_(package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void defun_package_nicknames(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_NICKNAMES, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_nicknames);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-shadowing-symbols (package) ...) -> list */
static int function_package_shadowing_symbols(Execute ptr, addr package)
{
	Return(getshadow_package_(package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void defun_package_shadowing_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_SHADOWING_SYMBOLS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_shadowing_symbols);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-use-list (package) ...) -> list */
static int function_package_use_list(Execute ptr, addr package)
{
	Return(getuselist_package_(package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void defun_package_use_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_USE_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_use_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-used-by-list (package) ...) -> list */
static int function_package_used_by_list(Execute ptr, addr package)
{
	Return(getusedbylist_package_(package, &package));
	setresult_control(ptr, package);
	return 0;
}

static void defun_package_used_by_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_USED_BY_LIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_used_by_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun packagep (object) ...) -> boolean */
static int function_packagep(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_PACKAGE);
	return 0;
}

static void defun_packagep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGEP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_packagep);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar *pacakge*) */
static void defvar_package(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PACKAGE, &symbol);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, Package);
	settype_value_symbol(symbol, type);
}


/* (defun package-error-package (package-error) ...) -> package-designer */
static int function_package_error_package(Execute ptr, addr var)
{
	package_error_package(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_package_error_package(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, PackageError);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, PackageDesigner);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_package_error_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_ERROR_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_error_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_package_error_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_packages(void)
{
	SetPointerCall(defun, var1opt1, export);
	SetPointerCall(defun, var1opt1, find_symbol);
	SetPointerCall(defun, var1, find_package);
	SetPointerCall(defun, var1, find_all_symbols);
	SetPointerCall(defun, var1opt1, import);
	SetPointerCall(defun, empty, list_all_packages);
	SetPointerCall(defun, var2opt1, rename_package);
	SetPointerCall(defun, var1opt1, shadow);
	SetPointerCall(defun, var1opt1, shadowing_import);
	SetPointerCall(defun, var1, delete_package);
	SetPointerCall(defun, var1dynamic, make_package);
	SetPointerCall(defmacro, macro, with_package_iterator);
	SetPointerCall(defun, var1opt1, unexport);
	SetPointerCall(defun, var1opt1, unintern);
	SetPointerCall(defmacro, macro, in_package);
	SetPointerCall(defun, var1opt1, unuse_package);
	SetPointerCall(defun, var1opt1, use_package);
	SetPointerCall(defmacro, macro, defpackage);
	SetPointerCall(defmacro, macro, do_symbols);
	SetPointerCall(defmacro, macro, do_external_symbols);
	SetPointerCall(defmacro, macro, do_all_symbols);
	SetPointerCall(defun, var1opt1, intern);
	SetPointerCall(defun, var1, package_name);
	SetPointerCall(defun, var1, package_nicknames);
	SetPointerCall(defun, var1, package_shadowing_symbols);
	SetPointerCall(defun, var1, package_use_list);
	SetPointerCall(defun, var1, package_used_by_list);
	SetPointerCall(defun, var1, packagep);
	SetPointerCall(defun, var1, package_error_package);
}

_g void build_common_packages(void)
{
	defun_export();
	defun_find_symbol();
	defun_find_package();
	defun_find_all_symbols();
	defun_import();
	defun_list_all_packages();
	defun_rename_package();
	defun_shadow();
	defun_shadowing_import();
	defun_delete_package();
	defun_make_package();
	defmacro_with_package_iterator();
	defun_unexport();
	defun_unintern();
	defmacro_in_package();
	defun_unuse_package();
	defun_use_package();
	defmacro_defpackage();
	defmacro_do_symbols();
	defmacro_do_external_symbols();
	defmacro_do_all_symbols();
	defun_intern();
	defun_package_name();
	defun_package_nicknames();
	defun_package_shadowing_symbols();
	defun_package_use_list();
	defun_package_used_by_list();
	defun_packagep();
	defvar_package();
	defun_package_error_package();
}

