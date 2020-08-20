#include "compile.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_operator.h"
#include "function.h"
#include "type_constant.h"
#include "type_table.h"
#include "pointer.h"
#include "syscall_code.h"
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


/* (defun redirect-restart (condition list) ...) -> null */
static int syscall_redirect_restart(Execute ptr, addr condition, addr list)
{
	Return(redirect_restart_syscode(ptr, condition, list));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_redirect_restart(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Condition);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_redirect_restart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_REDIRECT_RESTART, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_redirect_restart);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_redirect_restart(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun symbol-macro-expander (&rest form) form) */
static int syscall_symbol_macro_expander(Execute ptr, addr form, addr env)
{
	setresult_control(ptr, form);
	return 0;
}

static void defun_symbol_macro_expander(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SYMBOL_MACRO_EXPANDER, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_syscall_symbol_macro_expander);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defconstant (symbol value document) ...) -> symbol */
static int syscall_defconstant(Execute ptr, addr symbol, addr value, addr doc)
{
	Return(defconstant_syscode(symbol, value, doc));
	setresult_control(ptr, symbol);
	return 0;
}

static void type_syscall_defconstant(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	GetTypeTable(&type, StringNull);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_defconstant(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFCONSTANT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_defconstant);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_defconstant(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun in-package (string-desinger) ...) -> package */
static int syscall_in_package(Execute ptr, addr name)
{
	Return(in_package_syscode_(ptr, name, &name));
	setresult_control(ptr, name);
	return 0;
}

static void type_syscall_in_package(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Package);
	type_compiled_heap(args, values, ret);
}

static void defun_in_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_IN_PACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_in_package);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_in_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun setplist (key value list) ...) -> list */
static int syscall_setplist(Execute ptr, addr key, addr value, addr list)
{
	setplist_syscode(key, value, list, &list);
	setresult_control(ptr, list);
	return 0;
}

static void defun_setplist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SETPLIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_setplist);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Acons);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun remplist (key list) ...) -> value, check
 *   key    t
 *   list   list
 *   value  list
 *   check  boolean
 */
static int syscall_remplist(Execute ptr, addr key, addr list)
{
	Return(remplist_syscode_(key, list, &key, &list));
	setvalues_control(ptr, key, list, NULL);
	return 0;
}

static void type_syscall_remplist(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeTable(&type, Boolean);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void defun_remplist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_REMPLIST, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_remplist);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_remplist(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-hash-iterator (table) ...) -> hash-iterator */
static int syscall_make_hash_iterator(Execute ptr, addr pos)
{
	make_hash_iterator_syscode(pos, &pos);
	setresult_control(ptr, pos);
	return 0;
}

static void type_make_hash_iterator(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Hashtable);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_make_hash_iterator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_HASH_ITERATOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_hash_iterator);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_hash_iterator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun next-hash-iterator (iterator) ...) -> (values boolean key value) */
static int syscall_next_hash_iterator(Execute ptr, addr pos)
{
	addr key, value;

	next_hash_iterator_syscode(pos, &pos, &key, &value);
	setvalues_control(ptr, pos, key, value, NULL);

	return 0;
}

static void type_next_hash_iterator(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, T);
	typeargs_var1(&args, type);
	GetTypeValues(&values, Boolean);
	typevalues_values3(&values, values, type, type);
	type_compiled_heap(args, values, ret);
}

static void defun_next_hash_iterator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_NEXT_HASH_ITERATOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_next_hash_iterator);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_next_hash_iterator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-package-iterator (table internal external inherited) ...)
 *     -> package-iterator
 *   internal   t
 *   external   t
 *   inherited  t
 */
static int syscall_make_package_iterator(Execute ptr, addr pos, addr a, addr b, addr c)
{
	Return(make_package_iterator_syscode_(pos, a, b, c, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static void type_make_package_iterator(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PackageDesigner);
	GetTypeTable(&values, List);
	type2or_heap(args, values, &args);
	GetTypeTable(&values, T);
	typeargs_var4(&args, args, values, values, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_make_package_iterator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_PACKAGE_ITERATOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var4(pos, p_defun_syscall_make_package_iterator);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_package_iterator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun next-package-iterator (iterator) ...)
 *     -> (values boolean symbol status package)
 *   status  (member :internal :external :inherited)
 */
static int syscall_next_package_iterator(Execute ptr, addr pos)
{
	addr symbol, status, package;

	Return(next_package_iterator_syscode_(ptr, pos, &pos, &symbol, &status, &package));
	setvalues_control(ptr, pos, symbol, status, package, NULL);

	return 0;
}

static void type_next_package_iterator(addr *ret)
{
	addr args, values, type1, type2, type3, type4, key1, key2, key3;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeTable(&type1, Boolean);
	GetTypeTable(&type2, Symbol);
	GetConst(KEYWORD_INTERNAL, &key1);
	GetConst(KEYWORD_EXTERNAL, &key2);
	GetConst(KEYWORD_INHERITED, &key3);
	type_member_heap(&type3, key1, key2, key3, NULL);
	GetTypeTable(&type4, Package);
	typevalues_values4(&values, type1, type2, type3, type4);
	type_compiled_heap(args, values, ret);
}

static void defun_next_package_iterator(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_NEXT_PACKAGE_ITERATOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_next_package_iterator);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_next_package_iterator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defpackage (name size docuemntation nicknames use
 *     shadow shadowing-import-from import-from export intern)
 *     -> package
 *   name                   string-designer
 *   size                   (or null (integer 0 *))
 *   documentation          (or null string)
 *   nicknames              list
 *   use                    list
 *   shadow                 list
 *   shadowing-import-from  list
 *   import-from            list
 *   export                 list
 *   intern                 list
 */
static int syscall_defpackage(Execute ptr, addr rest)
{
	Return(defpackage_syscode(ptr, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void type_defpackage(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Package);
	type_compiled_heap(args, values, ret);
}

static void defun_defpackage(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFPACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_defpackage);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_defpackage(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun do-symbols (function package) ...) -> nil */
static int syscall_do_symbols(Execute ptr, addr call, addr package)
{
	Return(do_symbols_syscode(ptr, call, package));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_do_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DO_SYMBOLS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_do_symbols);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, DoSymbols);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun do-external-symbols (function package) ...) -> nil */
static int syscall_do_external_symbols(Execute ptr, addr call, addr package)
{
	Return(do_external_symbols_syscode(ptr, call, package));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_do_external_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DO_EXTERNAL_SYMBOLS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_do_external_symbols);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, DoSymbols);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun do-all-symbols (function) ...) -> nil */
static int syscall_do_all_symbols(Execute ptr, addr call)
{
	Return(do_all_symbols_syscode_(ptr, call));
	setvalues_nil_control(ptr);
	return 0;
}

static void type_do_all_symbols(addr *ret)
{
	/* (function (function) (values &rest nil)) */
	addr args, values;

	GetTypeTable(&args, Function);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, ret);
}

static void defun_do_all_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DO_ALL_SYMBOLS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_do_all_symbols);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_do_all_symbols(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun getdoc-variable (symbol) ...) -> (or string null) */
static int syscall_getdoc_variable(Execute ptr, addr var)
{
	getdoc_variable_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_getdoc_variable(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, ret);
}

static void defun_getdoc_variable(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_GETDOC_VARIABLE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_getdoc_variable);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_getdoc_variable(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun setdoc-variable (symbol string) ...) -> string */
static int syscall_setdoc_variable(Execute ptr, addr var, addr value)
{
	setdoc_variable_syscode(var, value);
	setresult_control(ptr, value);
	return 0;
}

static void type_setdoc_variable(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, String);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, ret);
}

static void defun_setdoc_variable(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SETDOC_VARIABLE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_setdoc_variable);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_setdoc_variable(&type);
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


/* (defun ecase-error (value list) ...) -> nil */
static int syscall_ecase_error(Execute ptr, addr value, addr list)
{
	Return(ecase_error_syscode_(ptr, value, list));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_ecase_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ECASE_ERROR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_ecase_error);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EcaseError);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun etypecase-error (value list) ...) -> nil */
static int syscall_etypecase_error(Execute ptr, addr value, addr list)
{
	Return(etypecase_error_syscode_(ptr, value, list));
	setvalues_nil_control(ptr);
	return 0;
}

static void defun_etypecase_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ETYPECASE_ERROR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_etypecase_error);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EcaseError);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun define-setf-expander (name lambda) ...) -> name */
static int syscall_define_setf_expander(Execute ptr, addr symbol, addr call)
{
	Return(define_setf_expander_syscode_(symbol, call));
	setresult_control(ptr, symbol);
	return 0;
}

static void type_syscall_define_setf_expander(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, Function);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_define_setf_expander(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFINE_SETF_EXPANDER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_define_setf_expander);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_define_setf_expander(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defsetf-short (access update args) ...) -> (values ...) */
static int syscall_defsetf_short(Execute ptr,
		addr access, addr update, addr args, addr env)
{
	addr a, b, g, w, r;

	Return(defsetf_short_syscode(ptr, access, update, args, env, &a, &b, &g, &w, &r));
	setvalues_control(ptr, a, b, g, w, r, NULL);

	return 0;
}

static void type_defsetf_short(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, List);
	GetTypeTable(&type, EnvironmentNull);
	typeargs_var3opt1(&args, args, args, values, type);
	GetTypeTable(&values, T);
	typevalues_values5(&values, values, values, values, values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_defsetf_short(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFSETF_SHORT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3opt1(pos, p_defun_syscall_defsetf_short);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_defsetf_short(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defsetf-long (access lambda store body args env) ...) */
static int syscall_defsetf_long(Execute ptr, addr rest)
{
	addr a, b, g, w, r;

	Return(defsetf_long_syscode(ptr, rest, &a, &b, &g, &w, &r));
	setvalues_control(ptr, a, b, g, w, r, NULL);

	return 0;
}

static void defun_defsetf_long(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFSETF_LONG, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_defsetf_long);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, Asterisk);
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


/* (defun end-input-stream (string-stream) -> index */
static int syscall_end_input_stream(Execute ptr, addr var)
{
	end_input_stream_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_end_input_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Index);
	type_compiled_heap(args, values, ret);
}

static void defun_end_input_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_END_INPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_end_input_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_end_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-extend-output-stream (string &key element-type) ...)
 *     -> string-stream
 */
static int syscall_make_extend_output_stream(Execute ptr, addr var, addr rest)
{
	make_extend_output_stream_syscode(var, rest, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_make_extend_output_stream(addr *ret)
{
	addr args, values;

	/* key */
	KeyTypeTable(&args, ELEMENT_TYPE, Symbol);
	list_heap(&args, args, NULL);
	GetTypeTable(&values, String);
	/* type */
	typeargs_var1key(&args, values, args);
	GetTypeTable(&values, StringStream);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_extend_output_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_EXTEND_OUTPUT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_make_extend_output_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_extend_output_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun prompt-for (type &rest args) ...) -> t */
static int syscall_prompt_for(Execute ptr, addr type, addr args)
{
	Return(prompt_for_syscode(ptr, type, args, &type));
	setresult_control(ptr, type);
	return 0;
}

static void type_prompt_for(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, TypeSpec);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_prompt_for(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PROMPT_FOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_prompt_for);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_prompt_for(&type);
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


/* (defun print-unreadable-call (stream pos type identity body) ...) -> null */
static int syscall_print_unreadable_call(Execute ptr,
		addr stream, addr pos, addr type, addr identity, addr body)
{
	Return(print_unreadable_call_syscode(ptr, stream, pos, type, identity, body));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_print_unreadable_call(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, StreamDesigner);
	typeargs_var5(&args, values, args, args, args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_print_unreadable_call(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PRINT_UNREADABLE_CALL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var5(pos, p_defun_syscall_print_unreadable_call);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_print_unreadable_call(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-default (stream object) ...) -> t */
static int syscall_write_default(Execute ptr, addr stream, addr var)
{
	Return(write_default_syscode(ptr, stream, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_write_default(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StreamDesigner);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_write_default(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_WRITE_DEFAULT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_write_default);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_write_default(&type);
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


/* (defun symbol-deftype (symbol) ...) -> (or null function) */
static int syscall_symbol_deftype(Execute ptr, addr var)
{
	symbol_deftype_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_symbol_deftype(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeTable(&values, FunctionNull);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_symbol_deftype(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SYMBOL_DEFTYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_symbol_deftype);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_symbol_deftype(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete-deftype (symbol) ...) -> boolean */
static int syscall_delete_deftype(Execute ptr, addr var)
{
	delete_deftype_syscode(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_delete_deftype(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(args, values, ret);
}

static void defun_delete_deftype(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DELETE_DEFTYPE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_delete_deftype);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_delete_deftype(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun subtypep-result (left right) ...) -> keyword */
static int syscall_subtypep_result(Execute ptr, addr left, addr right)
{
	Return(subtypep_result_syscode(ptr, left, right, &left));
	setresult_control(ptr, left);
	return 0;
}

static void type_syscall_subtypep_result(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, TypeSpec);
	typeargs_var2(&args, args, args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_subtypep_result(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SUBTYPEP_RESULT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_subtypep_result);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_subtypep_result(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ensure-structure (symbol list &rest args &key &allow-other-keys) ...)
 *   -> symbol
 */
static int syscall_ensure_structure(Execute ptr, addr name, addr slots, addr rest)
{
	Return(ensure_structure_syscode_(ptr, name, slots, rest));
	setresult_control(ptr, name);
	return 0;
}

static void type_ensure_structure(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, List);
	GetTypeTable(&type, T);
	typeargs_var2rest(&args, args, values, type);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_structure(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ENSURE_STRUCTURE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_ensure_structure);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_structure(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun structure-constructor (symbol &rest t &key &other-allow-keys) ...)
 *   -> structure-object
 */
static int syscall_structure_constructor(Execute ptr, addr symbol, addr rest)
{
	Return(structure_constructor_syscode(ptr, symbol, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void type_structure_constructor(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeTable(&values, StructureObject);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_constructor(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_STRUCTURE_CONSTRUCTOR, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_structure_constructor);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_structure_constructor(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun loop-bind (tree type value) ...) -> tree */
static int syscall_loop_bind(Execute ptr, addr a, addr b, addr c)
{
	Return(loop_bind_syscode(ptr, a, b, c, &a));
	setresult_control(ptr, a);
	return 0;
}

static void type_loop_bind(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var3(&args, args, args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_loop_bind(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_LOOP_BIND, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_loop_bind);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_loop_bind(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-pprint-stream (stream object
 *     prefix per-line-prefix suffix) ...) -> result
 *   stream           stream
 *   object           t
 *   prefix           string
 *   per-line-prefix  string
 *   suffix           string
 *   result           stream-pretty
 */
static int syscall_make_pprint_stream(Execute ptr,
		addr stream, addr object, addr prefix, addr perline, addr suffix)
{
	Return(make_pprint_stream_syscode_(ptr,
				&stream, stream, object, prefix, perline, suffix));
	setresult_control(ptr, stream);
	return 0;
}

static void type_syscall_make_pprint_stream(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&type1, Stream);
	GetTypeTable(&type2, T);
	GetTypeTable(&type3, StringNull);
	typeargs_var5(&args, type1, type2, type3, type3, type3);
	GetTypeValues(&values, PrettyStream);
	type_compiled_heap(args, values, ret);
}

static void defun_make_pprint_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_PPRINT_STREAM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var5(pos, p_defun_syscall_make_pprint_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_make_pprint_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-gensym (stream-pretty) ...) -> symbol */
static int syscall_pprint_gensym(Execute ptr, addr stream)
{
	Return(pprint_gensym_syscode(stream, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void type_syscall_pprint_gensym(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_gensym(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_GENSYM, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_gensym);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_gensym(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-exit (stream-pretty) ...) -> null */
static int syscall_pprint_exit(Execute ptr, addr stream)
{
	Return(pprint_exit_syscode(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_pprint_exit(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_exit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_EXIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_exit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_exit(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-pop (stream-pretty) ...) -> t */
static int syscall_pprint_pop(Execute ptr, addr stream)
{
	Return(pprint_pop_syscode(ptr, stream, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void type_syscall_pprint_pop(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_pop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_POP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_pop);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_pop(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-check (stream-pretty) ...) -> nil */
static int syscall_pprint_check(Execute ptr, addr stream)
{
	Return(pprint_check_syscode(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_pprint_check(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_check(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_CHECK, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_check);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_check(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-close (stream-pretty) ...) -> nil */
static int syscall_pprint_close(Execute ptr, addr stream)
{
	Return(pprint_close_syscode(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_pprint_close(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_close(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_CLOSE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_close);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_close(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-pretty (stream-pretty) ...) -> nil */
static int syscall_pprint_pretty(Execute ptr, addr stream, addr call)
{
	Return(pprint_pretty_syscode(ptr, stream, call));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_pprint_pretty(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PrettyStream);
	GetTypeTable(&values, Function);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_pprint_pretty(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PPRINT_PRETTY, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_pprint_pretty);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_pretty(&type);
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


/* (defun timeinfo () ...) -> (values intplus intplus intplus intplus) */
static int syscall_timeinfo(Execute ptr)
{
	addr real, run, size, count;

	Return(timeinfo_syscode_(ptr->local, &real, &run, &size, &count));
	setvalues_control(ptr, real, run, size, count, NULL);

	return 0;
}

static void type_syscall_timeinfo(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Empty);
	GetTypeTable(&values, Intplus);
	typevalues_values_va(&values, values, values, values, values, NULL);
	type_compiled_heap(args, values, ret);
}

static void defun_timeinfo(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_TIMEINFO, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_empty(pos, p_defun_syscall_timeinfo);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_timeinfo(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ed-function (file) ...) -> null
 *    file  (or null string)
 */
static int syscall_ed_function(Execute ptr, addr file)
{
	Return(ed_function_syscode_(ptr, file));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_ed_function(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StringNull);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_ed_function(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ED_FUNCTION, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_ed_function);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_ed_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
	/* (defvar *ed-function* [function]) */
	SetValueSymbol(symbol, pos);
}


/* (defun run-program (program args) ...) -> status */
static int syscall_run_program(Execute ptr, addr var, addr args, addr rest)
{
	Return(run_program_syscode_(ptr->local, var, args, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_run_program(addr *ret)
{
	addr args, values, key;

	/* key */
	key = Nil;

	/* type */
	GetTypeTable(&args, String);
	GetTypeTable(&values, List);
	typeargs_var2key(&args, args, values, key);
	GetTypeValues(&values, Integer);
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


/* (defun trace-add (list) ...) -> list */
static int syscall_trace_add(Execute ptr, addr var)
{
	Return(trace_add_syscode_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_trace_add(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_trace_add(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_TRACE_ADD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_trace_add);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_trace_add(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun trace-del (list-or-t) ...) -> list */
static int syscall_trace_del(Execute ptr, addr var)
{
	Return(trace_del_syscode_(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_trace_del(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	GetTypeTable(&values, EqlT);
	type2or_heap(args, values, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_trace_del(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_TRACE_DEL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_trace_del);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_trace_del(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar lisp-system::*compiler-macro* nil) */
static void defvar_compiler_macro(void)
{
	addr symbol;
	GetConst(SYSTEM_COMPILER_MACRO, &symbol);
	SetValueSymbol(symbol, Nil);
}


/* (defun with-compilation-unit (override args lambda) ...) -> any */
static void type_syscall_with_compilation_unit(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&type1, T);
	GetTypeTable(&type2, List);
	GetTypeTable(&type3, Function);
	typeargs_var3(&args, type1, type2, type3);
	GetTypeTable(&values, T);
	typevalues_rest(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_with_compilation_unit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_WITH_COMPILATION_UNIT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_with_compilation_unit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_with_compilation_unit(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun set-slots (instance slots values) ...) -> t */
static int syscall_set_slots(Execute ptr, addr var, addr slots, addr values)
{
	Return(set_slots_syscode(var, slots, values));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_set_slots(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var3(&args, args, values, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_set_slots(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SET_SLOTS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_set_slots);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_set_slots(&type);
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
	GetTypeTable(&values, Type);
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


/* (defun type-object (type) ...) -> (or cons symbol) */
static int syscall_type_object(Execute ptr, addr var)
{
	Return(type_object_syscode(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_type_object(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Type);
	typeargs_var1(&args, args);
	GetTypeValues(&values, TypeSymbol);
	type_compiled_heap(args, values, ret);
}

static void defun_type_object(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_TYPE_OBJECT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_type_object);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_type_object(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_syscall(void)
{
	SetPointerSysCall(defun, empty, hello);
	SetPointerSysCall(defun, dynamic, infobit);
	SetPointerSysCall(defun, dynamic, infoprint);
	SetPointerSysCall(defun, dynamic, gc);
	SetPointerSysCall(defun, var1, savecore);
	SetPointerSysCall(defun, var2, redirect_restart);
	SetPointerSysCall(defmacro, macro, symbol_macro_expander);
	SetPointerSysCall(defun, var3, defconstant);
	SetPointerSysCall(defun, var1, in_package);
	SetPointerSysCall(defun, var3, setplist);
	SetPointerSysCall(defun, var2, remplist);
	SetPointerSysCall(defun, var1, make_hash_iterator);
	SetPointerSysCall(defun, var1, next_hash_iterator);
	SetPointerSysCall(defun, var4, make_package_iterator);
	SetPointerSysCall(defun, var1, next_package_iterator);
	SetPointerSysCall(defun, dynamic, defpackage);
	SetPointerSysCall(defun, var2, do_symbols);
	SetPointerSysCall(defun, var2, do_external_symbols);
	SetPointerSysCall(defun, var1, do_all_symbols);
	SetPointerSysCall(defun, var1, getdoc_variable);
	SetPointerSysCall(defun, var2, setdoc_variable);
	SetPointerSysCall(defun, var1, specialp);
	SetPointerSysCall(defun, var2, ecase_error);
	SetPointerSysCall(defun, var2, etypecase_error);
	SetPointerSysCall(defun, var2, define_setf_expander);
	SetPointerSysCall(defun, var3opt1, defsetf_short);
	SetPointerSysCall(defun, dynamic, defsetf_long);
	SetPointerSysCall(defun, var1, array_general_p);
	SetPointerSysCall(defun, var1, array_specialized_p);
	SetPointerSysCall(defun, var2dynamic, simple_sort);
	SetPointerSysCall(defun, var2dynamic, bubble_sort);
	SetPointerSysCall(defun, var2dynamic, quick_sort);
	SetPointerSysCall(defun, var2dynamic, merge_sort);
	SetPointerSysCall(defun, opt1, exit);
	SetPointerSysCall(defun, opt1, exit);
	SetPointerSysCall(defun, var1, end_input_stream);
	SetPointerSysCall(defun, var1dynamic, make_extend_output_stream);
	SetPointerSysCall(defun, var1dynamic, prompt_for);
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
	SetPointerSysCall(defun, var5, print_unreadable_call);
	SetPointerSysCall(defun, var2, write_default);
	SetPointerSysCall(defun, var1, make_bignum);
	SetPointerSysCall(defun, var2, make_ratio);
	SetPointerSysCall(defun, var2, make_complex);
	SetPointerSysCall(defun, var2, equal_random_state);
	SetPointerSysCall(defun, var1, symbol_deftype);
	SetPointerSysCall(defun, var1, delete_deftype);
	SetPointerSysCall(defun, var2, subtypep_result);
	SetPointerSysCall(defun, var2dynamic, ensure_structure);
	SetPointerSysCall(defun, var1dynamic, structure_constructor);
	SetPointerSysCall(defun, var3, loop_bind);
	SetPointerSysCall(defun, var5, make_pprint_stream);
	SetPointerSysCall(defun, var1, pprint_gensym);
	SetPointerSysCall(defun, var1, pprint_exit);
	SetPointerSysCall(defun, var1, pprint_pop);
	SetPointerSysCall(defun, var1, pprint_check);
	SetPointerSysCall(defun, var1, pprint_close);
	SetPointerSysCall(defun, var2, pprint_pretty);
	SetPointerSysCall(defun, var3, eastasian_set);
	SetPointerSysCall(defun, var1, eastasian_get);
	SetPointerSysCall(defun, var1, eastasian_width);
	SetPointerSysCall(defun, empty, timeinfo);
	SetPointerSysCall(defun, var1, ed_function);
	SetPointerSysCall(defun, var2dynamic, run_program);
	SetPointerSysCall(defun, var1, make_callname);
	SetPointerSysCall(defun, var1, trace_add);
	SetPointerSysCall(defun, var1, trace_del);
	SetPointerSysCall(defun, var3, with_compilation_unit);
	SetPointerSysCall(defun, var3, set_slots);
	SetPointerSysCall(defun, var1opt1, remove_file);
	SetPointerSysCall(defun, var1opt1, remove_directory);
	SetPointerSysCall(defmacro, macro, declare_parse);
	SetPointerSysCall(defun, var1, parse_type);
	SetPointerSysCall(defun, var1, type_object);
}

_g void build_syscall(void)
{
	defun_hello();
	defun_infobit();
	defun_infoprint();
	defun_gc();
	defun_savecore();
	defun_redirect_restart();
	defun_symbol_macro_expander();
	defun_defconstant();
	defun_in_package();
	defun_setplist();
	defun_remplist();
	defun_make_hash_iterator();
	defun_next_hash_iterator();
	defun_make_package_iterator();
	defun_next_package_iterator();
	defun_defpackage();
	defun_do_symbols();
	defun_do_external_symbols();
	defun_do_all_symbols();
	defun_getdoc_variable();
	defun_setdoc_variable();
	defun_specialp();
	defun_ecase_error();
	defun_etypecase_error();
	defun_define_setf_expander();
	defun_defsetf_short();
	defun_defsetf_long();
	defun_array_general_p();
	defun_array_specialized_p();
	defun_simple_sort();
	defun_bubble_sort();
	defun_quick_sort();
	defun_merge_sort();
	defun_exit();
	defun_quit();
	defun_end_input_stream();
	defun_make_extend_output_stream();
	defun_prompt_for();
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
	defun_print_unreadable_call();
	defun_write_default();
	defun_make_bignum();
	defun_make_ratio();
	defun_make_complex();
	defun_equal_random_state();
	defun_symbol_deftype();
	defun_delete_deftype();
	defun_subtypep_result();
	defun_ensure_structure();
	defun_structure_constructor();
	defun_loop_bind();
	defun_make_pprint_stream();
	defun_pprint_gensym();
	defun_pprint_exit();
	defun_pprint_pop();
	defun_pprint_check();
	defun_pprint_close();
	defun_pprint_pretty();
	defun_eastasian_set();
	defun_eastasian_get();
	defun_eastasian_width();
	defun_timeinfo();
	defun_ed_function();
	defun_run_program();
	defun_make_callname();
	defun_trace_add();
	defun_trace_del();
	defvar_compiler_macro();
	defun_with_compilation_unit();
	defun_set_slots();
	defun_remove_file();
	defun_remove_directory();
	defmacro_declare_parse();
	defun_parse_type();
	defun_type_object();
}

