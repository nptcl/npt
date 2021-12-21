#include "compile.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_operator.h"
#include "function.h"
#include "type_constant.h"
#include "type_table.h"
#include "pointer.h"
#include "syscall_common.h"
#include "syscode_common.h"
#include "symbol.h"

/* (defun define-symbol-macro (symbol form) ...) -> symbol */
static int syscall_define_symbol_macro(Execute ptr, addr symbol, addr form)
{
	Return(setsymbol_macro_symbol_(symbol, form));
	setresult_control(ptr, symbol);
	return 0;
}

static void type_syscall_define_symbol_macro(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_define_symbol_macro(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFINE_SYMBOL_MACRO, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_define_symbol_macro);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_define_symbol_macro(&type);
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


/* (defun in-package (string-designer) ...) -> package */
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


/* (defun next-hash-iterator (iterator) ...) -> (values boolean &rest t) */
static int syscall_next_hash_iterator(Execute ptr, addr pos)
{
	addr key, value;

	next_hash_iterator_syscode(pos, &pos, &key, &value);
	if (pos == Nil)
		setresult_control(ptr, Nil);
	else
		setvalues_control(ptr, pos, key, value, NULL);

	return 0;
}

static void type_next_hash_iterator(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&type, T);
	typeargs_var1(&args, type);
	/* (values boolean &rest t) */
	GetTypeValues(&values, Boolean);
	conscar_heap(&values, values);
	type_values_heap(values, Nil, type, Nil, &values);
	/* result */
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


/* (defun next-package-iterator (iterator) ...) -> * */
static int syscall_next_package_iterator(Execute ptr, addr pos)
{
	addr symbol, status, package;

	Return(next_package_iterator_syscode_(ptr, pos, &pos, &symbol, &status, &package));
	if (pos == Nil)
		setresult_control(ptr, Nil);
	else
		setvalues_control(ptr, pos, symbol, status, package, NULL);

	return 0;
}

static void type_next_package_iterator(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Asterisk);
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


/* (defun defpackage (name &key size docuemntation nicknames use
 *     shadow shadowing-import-from import-from export intern)
 *     -> package
 *   name                    string-designer
 *   :size                   (or null (integer 0 *))
 *   :documentation          (or null string)
 *   :nicknames              list
 *   :use                    list
 *   :shadow                 list
 *   :shadowing-import-from  list
 *   :import-from            list
 *   :export                 list
 *   :intern                 list
 */
static int syscall_defpackage(Execute ptr, addr var, addr rest)
{
	Return(defpackage_syscode(ptr, var, rest, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_defpackage(addr *ret)
{
	addr args, values;
	addr key, key1, key2, key3, key4, key5, key6, key7, key8, key9, key10;

	GetTypeTable(&args, StringDesigner);
	KeyTypeTable(&key1, SIZE, IntplusNull);
	KeyTypeTable(&key2, DOCUMENTATION, StringNull);
	KeyTypeTable(&key3, NICKNAMES, List);
	KeyTypeTable(&key4, USE, List);
	KeyTypeTable(&key5, SHADOW, List);
	KeyTypeTable(&key6, SHADOWING_IMPORT_FROM, List);
	KeyTypeTable(&key7, SHADOWING_IMPORT_FROM, List);
	KeyTypeTable(&key8, IMPORT_FROM, List);
	KeyTypeTable(&key9, EXPORT, List);
	KeyTypeTable(&key10, INTERN, List);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, key8, key9, key10, NULL);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, Package);
	type_compiled_heap(args, values, ret);
}

static void defun_defpackage(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFPACKAGE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_defpackage);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_defpackage(&type);
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


/* (defvar *ed-function* (lambda (file) ... null))
 *    file    (or null string)
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

static void defvar_ed_function(void)
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


/* (defun intern-eql-specializer (instance slots values) ...) -> t */
static int syscall_intern_eql_specializer(Execute ptr, addr var)
{
	Return(intern_eql_specializer_syscode(var, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_intern_eql_specializer(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_intern_eql_specializer(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_INTERN_EQL_SPECIALIZER, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_intern_eql_specializer);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_intern_eql_specializer(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defgeneric-define (symbol &rest &key &allow-other-keys) ...) -> instance */
static int syscall_defgeneric_define(Execute ptr, addr var, addr args)
{
	Return(defgeneric_define_syscode_(ptr, var, args, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_defgeneric_define(addr *ret)
{
	addr args, values;
	addr key, key1, key2, key3, key4, key5, key6, key7;

	KeyTypeTable(&key1, ARGUMENT_PRECEDENCE_ORDER, T);
	KeyTypeTable(&key2, DECLARE, T);
	KeyTypeTable(&key3, DOCUMENTATION, String);
	KeyTypeTable(&key4, LAMBDA_LIST, T);
	KeyTypeTable(&key5, GENERIC_FUNCTION_CLASS, T);
	KeyTypeTable(&key6, METHOD_CLASS, T);
	KeyTypeTable(&key7, METHOD_COMBINATION, T);
	list_heap(&key, key1, key2, key3, key4, key5, key6, key7, NULL);
	/* type */
	GetTypeTable(&args, T);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, GenericFunction);
	type_compiled_heap(args, values, ret);
}

static void defun_defgeneric_define(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFGENERIC_DEFINE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_defgeneric_define);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_defgeneric_define(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defgeneric-method (instance &rest args) ...) -> instance */
static int syscall_defgeneric_method(Execute ptr, addr var, addr args)
{
	Return(defgeneric_method_syscode_(var, args));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_defgeneric_method(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, GenericFunction);
	GetTypeTable(&values, Method);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, GenericFunction);
	type_compiled_heap(args, values, ret);
}

static void defun_defgeneric_method(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFGENERIC_METHOD, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_defgeneric_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_defgeneric_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun condition-restarts-push (condition restarts) ...) -> null */
static int syscall_condition_restarts_push(Execute ptr, addr var, addr list)
{
	Return(condition_restarts_push_syscode_(var, list));
	setresult_control(ptr, Nil);
	return 0;
}

static void type_syscall_condition_restarts_push(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_condition_restarts_push(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_CONDITION_RESTARTS_PUSH, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_condition_restarts_push);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_condition_restarts_push(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun condition-restarts-pop (condition restarts) ...) -> null */
static int syscall_condition_restarts_pop(Execute ptr, addr var, addr list)
{
	Return(condition_restarts_pop_syscode_(var, list));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_condition_restarts_pop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_CONDITION_RESTARTS_POP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_condition_restarts_pop);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_condition_restarts_push(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun condition-restarts-make (type &rest args) ...) -> null */
static int syscall_condition_restarts_make(Execute ptr, addr var, addr list)
{
	Return(condition_restarts_make_syscode_(ptr, var, list, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_condition_restarts_make(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);

	/* values condition */
	GetConst(CLOS_CONDITION, &values);
	CheckType(values, LISPTYPE_CLOS);
	type_clos_heap(values, &values);

	/* compiled-function */
	type_compiled_heap(args, values, ret);
}

static void defun_condition_restarts_make(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_CONDITION_RESTARTS_MAKE, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_syscall_condition_restarts_make);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_condition_restarts_make(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-restart (name function &key
 *     interactive-function report-function test-function escape)
 *   ...) -> restart
 */
static int syscall_make_restart(Execute ptr, addr var, addr call, addr list)
{
	Return(make_restart_syscode_(var, call, list, &var));
	setresult_control(ptr, var);
	return 0;
}

static void type_syscall_make_restart(addr *ret)
{
	addr args, call, values;
	addr key, key1, key2, key3, key4;
	addr type, type1, type2, type3;

	/* (or function null string) */
	GetTypeTable(&type1, Function);
	GetTypeTable(&type2, Null);
	GetTypeTable(&type3, String);
	type3or_heap(type1, type2, type3, &type);

	/* compiled */
	GetTypeTable(&args, Symbol);
	GetTypeTable(&call, Function);
	KeyTypeTable(&key1, INTERACTIVE_FUNCTION, FunctionNull);
	GetConst(KEYWORD_REPORT_FUNCTION, &key2);
	cons_heap(&key2, key2, type);
	KeyTypeTable(&key3, TEST_FUNCTION, FunctionNull);
	KeyTypeTable(&key4, ESCAPE, T);
	list_heap(&key, key1, key2, key3, key4, NULL);
	typeargs_var2key(&args, args, call, key);
	GetTypeTable(&values, Restart);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_restart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MAKE_RESTART, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_make_restart);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_make_restart(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_syscall_common(void)
{
	SetPointerSysCall(defun, var2, define_symbol_macro);
	SetPointerSysCall(defmacro, macro, symbol_macro_expander);
	SetPointerSysCall(defun, var3, defconstant);
	SetPointerSysCall(defun, var1, in_package);
	SetPointerSysCall(defun, var3, setplist);
	SetPointerSysCall(defun, var2, remplist);
	SetPointerSysCall(defun, var1, make_hash_iterator);
	SetPointerSysCall(defun, var1, next_hash_iterator);
	SetPointerSysCall(defun, var4, make_package_iterator);
	SetPointerSysCall(defun, var1, next_package_iterator);
	SetPointerSysCall(defun, var1dynamic, defpackage);
	SetPointerSysCall(defun, var2, do_symbols);
	SetPointerSysCall(defun, var2, do_external_symbols);
	SetPointerSysCall(defun, var1, do_all_symbols);
	SetPointerSysCall(defun, var1, getdoc_variable);
	SetPointerSysCall(defun, var2, setdoc_variable);
	SetPointerSysCall(defun, var2, ecase_error);
	SetPointerSysCall(defun, var2, etypecase_error);
	SetPointerSysCall(defun, var2, define_setf_expander);
	SetPointerSysCall(defun, var1, end_input_stream);
	SetPointerSysCall(defun, var1dynamic, make_extend_output_stream);
	SetPointerSysCall(defun, var1dynamic, prompt_for);
	SetPointerSysCall(defun, var5, print_unreadable_call);
	SetPointerSysCall(defun, var2, write_default);
	SetPointerSysCall(defun, var1, symbol_deftype);
	SetPointerSysCall(defun, var1, delete_deftype);
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
	SetPointerSysCall(defun, empty, timeinfo);
	SetPointerSysCall(defun, var1, ed_function);
	SetPointerSysCall(defun, var1, trace_add);
	SetPointerSysCall(defun, var1, trace_del);
	SetPointerSysCall(defun, var3, with_compilation_unit);
	SetPointerSysCall(defun, var3, set_slots);
	SetPointerSysCall(defun, var1, intern_eql_specializer);
	SetPointerSysCall(defun, var1dynamic, defgeneric_define);
	SetPointerSysCall(defun, var1dynamic, defgeneric_method);
	SetPointerSysCall(defun, var2, condition_restarts_push);
	SetPointerSysCall(defun, var2, condition_restarts_pop);
	SetPointerSysCall(defun, var1rest, condition_restarts_make);
	SetPointerSysCall(defun, var2dynamic, make_restart);
}

void build_syscall_common(void)
{
	defun_define_symbol_macro();
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
	defun_ecase_error();
	defun_etypecase_error();
	defun_define_setf_expander();
	defun_end_input_stream();
	defun_make_extend_output_stream();
	defun_prompt_for();
	defun_print_unreadable_call();
	defun_write_default();
	defun_symbol_deftype();
	defun_delete_deftype();
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
	defun_timeinfo();
	defvar_ed_function();
	defun_trace_add();
	defun_trace_del();
	defun_with_compilation_unit();
	defun_set_slots();
	defun_intern_eql_specializer();
	defun_defgeneric_define();
	defun_defgeneric_method();
	defun_condition_restarts_push();
	defun_condition_restarts_pop();
	defun_condition_restarts_make();
	defun_make_restart();
}

