#include "array.h"
#include "bigdata.h"
#include "bignum.h"
#include "character.h"
#include "cmpl.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "control.h"
#include "core.h"
#include "document.h"
#include "eastasian.h"
#include "equal.h"
#include "eval.h"
#include "file.h"
#include "format.h"
#include "function.h"
#include "gc.h"
#include "hashtable.h"
#include "integer.h"
#include "lambda.h"
#include "loop_bind.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "print.h"
#include "print_pretty.h"
#include "print_write.h"
#include "quote.h"
#include "radix.h"
#include "random_state.h"
#include "ratio.h"
#include "readtable.h"
#include "sequence.h"
#include "sort.h"
#include "stream.h"
#include "stream_pretty.h"
#include "stream_string.h"
#include "strtype.h"
#include "structure.h"
#include "symbol.h"
#include "syscall.h"
#include "type_parse.h"
#include "type_table.h"

/*
 *  hello
 */
static void syscall_hello(Execute ptr)
{
	addr stream;

	standard_input_stream(ptr, &stream);
	fresh_line_stream(stream);
	print_ascii_stream(stream, "Hello");
	terpri_stream(stream);
	setresult_control(ptr, Nil);
}

static void defun_hello(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_HELLO, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_syscall_hello);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, CompiledFunction);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  infobit
 */
static void syscall_infobit(Execute ptr, addr list)
{
	addr a, b;

	for (b = Nil; list != Nil; b = a) {
		GetCons(list, &a, &list);
		infobit(a);
	}
	setresult_control(ptr, b);
}

static void type_infobit(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_infobit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_INFOBIT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_infobit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_infobit(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  infoprint
 */
static void syscall_infoprint(Execute ptr, addr list)
{
	addr a, b;

	for (b = Nil; list != Nil; b = a) {
		GetCons(list, &a, &list);
		infoprint(a);
	}
	setresult_control(ptr, b);
}

static void type_infoprint(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_infoprint(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_INFOPRINT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_infoprint);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_infoprint(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  gc
 */
static void syscall_gc(Execute ptr, addr rest)
{
	if (getkeyargs(rest, KEYWORD_FULL, &rest)) rest = Nil;
	gcstate_execute();
	setresult_control(ptr, Nil);
}

static void type_gc(addr *ret)
{
	addr args, values;

	/* key */
	KeyTypeTable(&args, FULL, T);
	list_heap(&args, args, NULL);
	/* type */
	typeargs_key(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_gc(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_GC, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_gc);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_gc(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  savecore
 */
static void syscall_savecore(Execute ptr, addr file)
{
	pathname_designer_local(ptr, file, &file);
	savecore_execute(ptr, file);
	setresult_control(ptr, Nil);
}

static void type_savecore(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, PathnameDesigner);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_savecore(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SAVECORE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_savecore);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_savecore(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  hander/restart
 */
static void syscall_redirect_restart_call(Execute ptr, addr right)
{
	addr condition, value, pos;

	list_bind(right, &condition, &value, NULL);
	if (! conditionp(condition))
		fmte("The argument ~S must be a condition.", condition, NULL);
	while (value != Nil) {
		getcons(value, &pos, &value);
		if (GetType(pos) != LISPTYPE_RESTART)
			fmte("The argument ~S must be a restart.", pos, NULL);
		pushbind_restart_control(ptr, pos, 0);
	}
	reverse_restart_control(ptr);
}

static void defun_redirect_restart(void)
{
	addr symbol, pos;

	GetConst(SYSTEM_REDIRECT_RESTART, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_redirect_restart_call);
	SetFunctionSymbol(symbol, pos);
}


/* symbol macro */
static void syscall_symbol_macro_expander(Execute ptr, addr form, addr env)
{
	setresult_control(ptr, form);
}

static void defun_symbol_macro_expander(void)
{
	addr symbol, pos;

	GetConst(SYSTEM_SYMBOL_MACRO_EXPANDER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_syscall_symbol_macro_expander);
	SetFunctionSymbol(symbol, pos);
}

/* (defun defconstant (symbol value document) ...) -> symbol */
static void syscall_defconstant(Execute ptr, addr symbol, addr value, addr doc)
{
	addr check;

	Check(! symbolp(symbol), "type symbol error");
	Check(doc != Nil && (! stringp(doc)), "type documentation error");
	GetValueSymbol(symbol, &check);
	if (check != Unbound && (! eql(check, value)))
		fmte("The defconstant cannot setq ~S value.", symbol, NULL);
	ResetStatusReadOnly(symbol);
	SetValueSymbol(symbol, value);
	setdocument_variable_symbol(symbol, doc);
	setspecial_symbol(symbol);
	SetStatusReadOnly(symbol);
	setresult_control(ptr, symbol);
}

static void type_defconstant(addr *ret)
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
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_defconstant);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_defconstant(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun in-package (string-desinger) ...) -> package */
static void syscall_in_package(Execute ptr, addr name)
{
	in_package(ptr, name, &name);
	setresult_control(ptr, name);
}

static void type_in_package(addr *ret)
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_in_package);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_in_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun setplist (key value list) ...) -> list */
static void syscall_setplist(Execute ptr, addr key, addr value, addr list)
{
	(void)setplist_heap_safe(list, key, value, &list);
	setresult_control(ptr, list);
}

static void defun_setplist(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SETPLIST, &symbol);
	compiled_heap(&pos, symbol);
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
static void syscall_remplist(Execute ptr, addr key, addr list)
{
	int check = (remplist_check_safe(list, key, &list) != RemPlist_NotFound);
	setvalues_control(ptr, list, (check? T: Nil), NULL);
}

static void type_remplist(addr *ret)
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
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_remplist);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_remplist(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-hash-iterator (table) ...) -> hash-iterator */
static void syscall_make_hash_iterator(Execute ptr, addr pos)
{
	hash_iterator_heap(&pos, pos);
	setresult_control(ptr, pos);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_hash_iterator);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_hash_iterator(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun next-hash-iterator (iterator) ...) -> (values boolean key value) */
static void syscall_next_hash_iterator(Execute ptr, addr pos)
{
	int check;
	addr key, value;

	check = next_hash_iterator(pos, &key, &value);
	if (check)
		setvalues_control(ptr, T, key, value, NULL);
	else
		setvalues_control(ptr, Nil, Nil, Nil, NULL);
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
	compiled_heap(&pos, symbol);
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
static void syscall_make_package_iterator(Execute ptr, addr rest)
{
	addr pos, a, b, c;

	GetCons(rest, &pos, &rest);
	GetCons(rest, &a, &rest);
	GetCons(rest, &b, &rest);
	GetCar(rest, &c);
	package_iterator_heap(&pos, pos, (a != Nil), (b != Nil), (c != Nil));
	setresult_control(ptr, pos);
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
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_make_package_iterator);
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
static void syscall_next_package_iterator(Execute ptr, addr pos)
{
	enum PACKAGE_TYPE check;
	addr symbol, status, package;

	check = next_package_iterator(pos, &symbol, &package);
	if (check == PACKAGE_TYPE_NIL) {
		setvalues_control(ptr, Nil, Nil, Nil, Nil, NULL);
	}
	else {
		keyword_packagetype(check, &status);
		setvalues_control(ptr, T, symbol, status, package, NULL);
	}
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
	compiled_heap(&pos, symbol);
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
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_defpackage);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_defpackage(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun do-symbols (function package) ...) -> nil */
static void defun_do_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DO_SYMBOLS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_do_symbols);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, DoSymbols);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun do-external-symbols (function package) ...) -> nil */
static void defun_do_external_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DO_EXTERNAL_SYMBOLS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_do_external_symbols);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, DoSymbols);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun do-all-symbols (function) ...) -> nil */
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_do_all_symbols);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_do_all_symbols(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun getdoc-variable (symbol) ...) -> (or string null) */
static void syscall_getdoc_variable(Execute ptr, addr var)
{
	get_variable_document(var, &var);
	setresult_control(ptr, var);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_getdoc_variable);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_getdoc_variable(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun setdoc-variable (symbol string) ...) -> string */
static void syscall_setdoc_variable(Execute ptr, addr var, addr value)
{
	set_variable_document(var, value);
	setresult_control(ptr, value);
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
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_setdoc_variable);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_setdoc_variable(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun specialp (symbol) ...) -> boolean */
static void syscall_specialp(Execute ptr, addr var)
{
	setbool_control(ptr, specialp_symbol(var));
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_specialp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_specialp(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ecase-error (value list) ...) -> nil */
static void syscall_ecase_error(Execute ptr, addr value, addr list)
{
	make_vector4_from_list(&list, list);
	type1_heap(LISPDECL_MEMBER, list, &list);
	type_error(value, list);
	setvalues_nil_control(ptr);
}

static void defun_ecase_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ECASE_ERROR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_ecase_error);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EcaseError);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun etypecase-error (value list) ...) -> nil */
static void syscall_etypecase_error(Execute ptr, addr value, addr list)
{
	make_vector4_from_list(&list, list);
	type1_heap(LISPDECL_OR, list, &list);
	type_error(value, list);
	setvalues_nil_control(ptr);
}

static void defun_etypecase_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ETYPECASE_ERROR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_etypecase_error);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, EcaseError);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun define-setf-expander (name lambda) ...) -> name */
static void syscall_define_setf_expander(Execute ptr, addr symbol, addr call)
{
	setsetfmacro_symbol(symbol, call);
	setresult_control(ptr, symbol);
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
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_define_setf_expander);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_define_setf_expander(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defsetf-short (access update args) ...) -> (values ...) */
static void syscall_defsetf_short(Execute ptr,
		addr access, addr update, addr args, addr env)
{
	int check;
	addr a, b, g, w, r, pos, v;
	LocalHold hold;

	if (env == Unbound) env = Nil;
	make_gensym(ptr, &g);
	conscar_heap(&w, update);
	conscar_heap(&r, access);
	a = b = Nil;

	hold = LocalHold_array(ptr, 5);
	localhold_set(hold, 2, g);
	localhold_set(hold, 3, w);
	localhold_set(hold, 4, r);
	while (args != Nil) {
		if (! consp(args))
			fmte("Invalid call argument ~S.", args, NULL);
		GetCons(args, &pos, &args);
		if (eval_constantp(ptr, pos, env, &check))
			return;
		if (check) {
			cons_heap(&w, pos, w);
			cons_heap(&r, pos, r);
		}
		else {
			make_gensym(ptr, &v);
			cons_heap(&a, v, a);
			cons_heap(&b, pos, b);
			cons_heap(&w, v, w);
			cons_heap(&r, v, r);
			localhold_set(hold, 0, a);
			localhold_set(hold, 1, b);
		}
		localhold_set(hold, 3, w);
		localhold_set(hold, 4, r);
	}
	localhold_end(hold);

	cons_heap(&w, g, w);
	nreverse_list_unsafe(&a, a);
	nreverse_list_unsafe(&b, b);
	conscar_heap(&g, g);
	nreverse_list_unsafe(&w, w);
	nreverse_list_unsafe(&r, r);
	setvalues_control(ptr, a, b, g, w, r, NULL);
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
	compiled_heap(&pos, symbol);
	setcompiled_var3opt1(pos, p_defun_syscall_defsetf_short);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_defsetf_short(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defsetf-long (access lambda store body args env) ...) */
static void defsetf_push(addr array, int index, addr pos)
{
	addr root;
	GetArrayA2(array, index, &root);
	cons_heap(&root, pos, root);
	SetArrayA2(array, index, root);
}

static void defsetf_var_bind(Execute ptr, addr *args, addr list, addr array)
{
	addr pos, gensym, value;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (! consp(*args))
			fmte("The argument ~S must be list type.", *args, NULL);
		GetCons(*args, &value, args);
		make_gensym(ptr, &gensym);
		defsetf_push(array, 0, gensym);
		defsetf_push(array, 1, value);
		defsetf_push(array, 2, pos);
		defsetf_push(array, 3, gensym);
	}
}

static void defsetf_opt_bind(Execute ptr, addr *args, addr list, addr array)
{
	int check;
	addr pos, var, init, sup, gensym;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		List_bind(pos, &var, &init, &sup, NULL);
		check = (*args != Nil);
		if (check) {
			if (! consp(*args))
				fmte("The argument ~S must be list type.", *args, NULL);
			GetCons(*args, &init, args);
		}
		make_gensym(ptr, &gensym);
		defsetf_push(array, 0, gensym);
		defsetf_push(array, 1, init);
		defsetf_push(array, 2, var);
		defsetf_push(array, 3, gensym);
		if (sup != Nil) {
			make_gensym(ptr, &gensym);
			defsetf_push(array, 0, gensym);
			defsetf_push(array, 1, check? T: Nil);
			defsetf_push(array, 2, sup);
			defsetf_push(array, 3, gensym);
		}
	}
}

static void defsetf_store_bind(Execute ptr, addr list, addr array, addr *ret)
{
	addr root, symbol, gensym;

	root = Nil;
	while (list != Nil) {
		if (! consp(list))
			fmte("defsetf store ~S must be a list type.", list, NULL);
		GetCons(list, &symbol, &list);
		if (! symbolp(symbol))
			fmte("defsetf store ~S must be a symbol type.", symbol, NULL);
		make_gensym(ptr, &gensym);
		defsetf_push(array, 2, symbol);
		defsetf_push(array, 3, gensym);
		cons_heap(&root, gensym, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void defsetf_bind(Execute ptr, addr args,
		addr lambda, addr store,
		addr *a, addr *b, addr *c, addr *d, addr *g)
{
	addr var, opt, rest, key, allow, env, array;

	List_bind(lambda, &var, &opt, &rest, &key, &allow, &env, NULL);
	vector2_heap(&array, 4);
	defsetf_var_bind(ptr, &args, var, array);
	defsetf_opt_bind(ptr, &args, opt, array);
	defsetf_store_bind(ptr, store, array, g);
	/* args */
	GetArrayA2(array, 0, a);
	GetArrayA2(array, 1, b);
	GetArrayA2(array, 2, c);
	GetArrayA2(array, 3, d);
	nreverse_list_unsafe(a, *a);
	nreverse_list_unsafe(b, *b);
	nreverse_list_unsafe(c, *c);
	nreverse_list_unsafe(d, *d);
}

static void syscall_defsetf_write(Execute ptr, addr *ret, addr c, addr d, addr body)
{
	/*  (let ((c1 'd1)
	 *        (c2 'd2))
	 *    (declare (ignorable c...))
	 *    body...)
	 */
	addr root, x, y;
	addr quote, let, declare, ignorable;
	LocalHold hold;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);

	/* let-args */
	for (root = Nil; c != Nil; ) {
		getcons(c, &x, &c);
		getcons(d, &y, &d);
		list_heap(&y, quote, y, NULL);
		list_heap(&y, x, y, NULL);
		cons_heap(&root, y, root);
	}
	nreverse_list_unsafe(&root, root);
	/* (declare ...) */
	cons_heap(&ignorable, ignorable, c);
	list_heap(&declare, declare, ignorable, NULL);
	/* let */
	lista_heap(&root, let, root, declare, body, NULL);
	hold = LocalHold_local_push(ptr, root);
	eval_object(ptr, root, ret);
	localhold_end(hold);
}

static void syscall_defsetf_long(Execute ptr, addr rest)
{
	addr access, lambda, store, body, args, env;
	addr quote, a, b, c, d, g, w, r;
	LocalHold hold;

	list_bind(rest, &access, &lambda, &store, &body, &args, &env, NULL);
	lambda_defsetf(ptr->local, &lambda, lambda);
	defsetf_bind(ptr, args, lambda, store, &a, &b, &c, &d, &g);
	/* (values 'a 'b 'g
	 *   `(let ((c1 'd1)
	 *          (c2 'd2))
	 *      (declare (ignorable c...))
	 *      ,@body...)
	 *   '(access d...))
	 */
	GetConst(COMMON_QUOTE, &quote);

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, a, b, c, d, g, NULL);
	syscall_defsetf_write(ptr, &w, c, d, body);
	localhold_end(hold);

	cons_heap(&r, access, d);
	setvalues_control(ptr, a, b, g, w, r, NULL);
}

static void defun_defsetf_long(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DEFSETF_LONG, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_syscall_defsetf_long);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, Asterisk);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-general-p (object) ...) -> boolean */
static void syscall_array_general_p(Execute ptr, addr var)
{
	setbool_control(ptr, array_general_p(var));
}

static void defun_array_general_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ARRAY_GENERAL_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_array_general_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-specialized-p (object) ...) -> boolean */
static void syscall_array_specialized_p(Execute ptr, addr var)
{
	setbool_control(ptr, array_specialized_p(var));
}

static void defun_array_specialized_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ARRAY_SPECIALIZED_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_array_specialized_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun simple-sort (sequence call &key key) ...) -> sequence */
static void syscall_simple_sort(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (simple_sort_sequence(ptr, pos, call, key)) return;
	setresult_control(ptr, pos);
}

static void defun_simple_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SIMPLE_SORT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_simple_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bubble-sort (sequence call &key key) ...) -> sequence */
static void syscall_bubble_sort(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (bubble_sort_sequence(ptr, pos, call, key)) return;
	setresult_control(ptr, pos);
}

static void defun_bubble_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BUBBLE_SORT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_bubble_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun quick-sort (sequence call &key key) ...) -> sequence */
static void syscall_quick_sort(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (quick_sort_sequence(ptr, pos, call, key)) return;
	setresult_control(ptr, pos);
}

static void defun_quick_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_QUICK_SORT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_quick_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun merge-sort (sequence call &key key) ...) -> sequence */
static void syscall_merge_sort(Execute ptr, addr pos, addr call, addr rest)
{
	addr key;

	if (getkeyargs(rest, KEYWORD_KEY, &key)) key = Nil;
	if (merge_sort_sequence(ptr, pos, call, key)) return;
	setresult_control(ptr, pos);
}

static void defun_merge_sort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MERGE_SORT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_syscall_merge_sort);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Sort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun exit/quit (&optional code) ...) -> (values &rest nil) */
static void syscall_exit(Execute ptr, addr code)
{
	int result;
	fixnum value;

	if (code == Unbound) {
		exit_execute(0);
	}
	else {
		if (GetType(code) != LISPTYPE_FIXNUM)
			fmte("Invalid code type ~S.", code, NULL);
		GetFixnum(code, &value);
		result = (int)value;
		if (value != (fixnum)result)
			fmte("The result code ~S must be a int type.", code, NULL);
		exit_execute(result);
	}
}

static void defun_exit(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EXIT, &symbol);
	compiled_heap(&pos, symbol);
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
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_syscall_exit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Exit);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun end-input-stream (string-stream) -> index */
static void syscall_end_input_stream(Execute ptr, addr var)
{
	size_t size;
	getindex_input_stream(var, &size);
	setresult_control(ptr, intsizeh(size));
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
	compiled_heap(&pos, symbol);
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
static void syscall_make_extend_output_stream(Execute ptr, addr var, addr rest)
{
	/* ignore rest */
	open_extend_output_stream(&var, var);
	setresult_control(ptr, var);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_make_extend_output_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_extend_output_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun prompt-for (type &rest args) ...) -> t */
static void syscall_prompt_for(Execute ptr, addr type, addr args)
{
	addr format;
	LocalHold hold;

	if (args == Nil) {
		strvect_char_heap(&format, "Input> ");
	}
	else {
		getcons(args, &format, &args);
		if (format_string_lisp(ptr, format, args, &format))
			return;
	}

	hold = LocalHold_local_push(ptr, format);
	if (prompt_for_stream(ptr, type, format, &format))
		return;
	localhold_end(hold);
	setresult_control(ptr, format);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_prompt_for);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_prompt_for(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun closp (object) ...) -> boolean */
static void syscall_closp(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_CLOS);
}

static void defun_closp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_CLOSP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_closp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fixnump (object) ...) -> boolean */
static void syscall_fixnump(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_FIXNUM);
}

static void defun_fixnump(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_FIXNUMP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_fixnump);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun bignump (object) ...) -> boolean */
static void syscall_bignump(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_BIGNUM);
}

static void defun_bignump(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BIGNUMP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_bignump);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun ratiop (object) ...) -> boolean */
static void syscall_ratiop(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_RATIO);
}

static void defun_ratiop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_RATIOP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_ratiop);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun short-float-p (object) ...) -> boolean */
static void syscall_short_float_p(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_SHORT_FLOAT);
}

static void defun_short_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SHORT_FLOAT_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_short_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun single-float-p (object) ...) -> boolean */
static void syscall_single_float_p(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_SINGLE_FLOAT);
}

static void defun_single_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SINGLE_FLOAT_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_single_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun double-float-p (object) ...) -> boolean */
static void syscall_double_float_p(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_DOUBLE_FLOAT);
}

static void defun_double_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DOUBLE_FLOAT_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_double_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun long-float-p (object) ...) -> boolean */
static void syscall_long_float_p(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_LONG_FLOAT);
}

static void defun_long_float_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_LONG_FLOAT_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_long_float_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun large-number (value &optional (cardinal t)) ...) -> string
 *   value  (integer 0 fixnum-max)
 */
static void syscall_large_number(Execute ptr, addr var, addr opt)
{
	if (opt == Unbound) opt = T;
	english_unit_heap(ptr->local, &var, var, opt != Nil);
	setresult_control(ptr, var);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_syscall_large_number);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_large_number(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun format-formatter (string &rest args) ...) -> list */
static void syscall_format_formatter(Execute ptr, addr string, addr args)
{
	addr stream;
	LocalHold hold;

	standard_output_stream(ptr, &stream);
	stream_designer(ptr, stream, &stream, 0);
	hold = LocalHold_local_push(ptr, stream);
	if (format_stream_args(ptr, stream, string, args, &args))
		return;
	localhold_end(hold);
	setresult_control(ptr, args);
}

static void type_format_formatter(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, String);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_format_formatter(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_FORMAT_FORMATTER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_format_formatter);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_format_formatter(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun print-unreadable-call (stream pos type identity body) ...) -> null */
static void syscall_print_unreadable_call(Execute ptr,
		addr stream, addr pos, addr type, addr identity, addr body)
{
	int check1, check2;

	check1 = (type != Nil);
	check2 = (identity != Nil);
	if (print_unreadable_common(ptr, stream, pos, check1, check2, body))
		return;
	setresult_control(ptr, Nil);
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
	compiled_heap(&pos, symbol);
	setcompiled_var5(pos, p_defun_syscall_print_unreadable_call);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_print_unreadable_call(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-default (stream object) ...) -> t */
static void syscall_write_default(Execute ptr, addr stream, addr var)
{
	addr control;
	LocalHold hold;

	output_stream_designer(ptr, stream, &stream);
	push_close_control(ptr, &control);
	hold = LocalHold_local_push(ptr, stream);
	if (write_default_print(ptr, stream, var))
		return;
	localhold_end(hold);
	setresult_control(ptr, var);
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
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_write_default);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_write_default(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-bignum (integer) ...) -> bignum */
static void syscall_make_bignum(Execute ptr, addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_heap(&var, var);
			break;

		case LISPTYPE_BIGNUM:
			bignum_throw_heap(var, &var);
			break;

		default:
			TypeError(var, INTEGER);
			return;
	}
	setresult_control(ptr, var);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_make_bignum);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_bignum(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-ratio (numer denom) ...) -> ratio */
static void force_make_bignum(addr *ret, addr var)
{
	switch (GetType(var)) {
		case LISPTYPE_FIXNUM:
			bignum_fixnum_heap(ret, var);
			break;

		case LISPTYPE_BIGNUM:
			bignum_copy_heap(ret, var);
			break;

		default:
			TypeError(var, INTEGER);
			return;
	}
}

static void syscall_make_ratio(Execute ptr, addr numer, addr denom)
{
	int sign1, sign2;

	force_make_bignum(&numer, numer);
	force_make_bignum(&denom, denom);
	GetSignBignum(numer, &sign1);
	GetSignBignum(denom, &sign2);
	SetSignBignum(numer, SignPlus);
	SetSignBignum(denom, SignPlus);
	sign1 = SignMulti(sign1, sign2);
	make_ratio_alloc_unsafe(NULL, &numer, sign1, numer, denom);
	setresult_control(ptr, numer);
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
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_make_ratio);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_ratio(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-complex (real imag) ...) -> complex */
static void syscall_make_complex(Execute ptr, addr real, addr imag)
{
	complex_force_heap(&real, real, imag, ComplexType_error);
	setresult_control(ptr, real);
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
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_make_complex);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_make_complex(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equal-random-state (a b) ...) -> boolean */
static void syscall_equal_random_state(Execute ptr, addr left, addr right)
{
	int check = equal_random_state_addr(left, right);
	setbool_control(ptr, check);
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
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_equal_random_state);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_equal_random_state(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun symbol-deftype (symbol) ...) -> (or null function) */
static void syscall_symbol_deftype(Execute ptr, addr var)
{
	getdeftype_symbol(var, &var);
	setresult_control(ptr, var);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_symbol_deftype);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_symbol_deftype(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete-deftype (symbol) ...) -> boolean */
static void syscall_delete_deftype(Execute ptr, addr var)
{
	addr check;

	getdeftype_symbol(var, &check);
	if (check == Nil) {
		setresult_control(ptr, Nil);
	}
	else {
		remdeftype_symbol(var);
		setresult_control(ptr, T);
	}
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
	compiled_heap(&pos, symbol);
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
static void syscall_ensure_structure(Execute ptr, addr name, addr slots, addr rest)
{
	ensure_structure_common(ptr, name, slots, rest);
	setresult_control(ptr, name);
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
	compiled_heap(&pos, symbol);
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
static void syscall_structure_constructor(Execute ptr, addr symbol, addr rest)
{
	if (structure_constructor_common(ptr, symbol, rest, &rest))
		return;
	setresult_control(ptr, rest);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_syscall_structure_constructor);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_structure_constructor(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun loop-bind (tree type value) ...) -> tree */
static void syscall_loop_bind(Execute ptr, addr a, addr b, addr c)
{
	if (loop_bind_common(ptr, a, b, c, &a))
		return;
	setresult_control(ptr, a);
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
	compiled_heap(&pos, symbol);
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
static void syscall_make_pprint_stream(Execute ptr,
		addr stream, addr object, addr prefix, addr perline, addr suffix)
{
	open_pretty_stream(ptr, &stream, stream, object, prefix, perline, suffix);
	setresult_control(ptr, stream);
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
	compiled_heap(&pos, symbol);
	setcompiled_var5(pos, p_defun_syscall_make_pprint_stream);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_make_pprint_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-gensym (stream-pretty) ...) -> symbol */
static void syscall_pprint_gensym(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	gensym_pretty_stream(stream, &stream);
	setresult_control(ptr, stream);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_gensym);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_gensym(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-exit (stream-pretty) ...) -> null */
static void syscall_pprint_exit(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	if (pprint_exit_common(ptr, stream))
		return;
	setresult_control(ptr, Nil);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_exit);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_exit(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-pop (stream-pretty) ...) -> t */
static void syscall_pprint_pop(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	if (pprint_pop_common(ptr, stream, &stream)) return;
	setresult_control(ptr, stream);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_pop);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_pop(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-check (stream-pretty) ...) -> nil */
static void syscall_pprint_check(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	if (check_pretty_stream(ptr, stream))
		return;
	setresult_control(ptr, Nil);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_check);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_check(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-close (stream-pretty) ...) -> nil */
static void syscall_pprint_close(Execute ptr, addr stream)
{
	Check(! pretty_stream_p(stream), "type error");
	close_pretty_stream(ptr, stream);
	setresult_control(ptr, Nil);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_pprint_close);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_close(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pprint-pretty (stream-pretty) ...) -> nil */
static void syscall_pprint_pretty(Execute ptr, addr stream, addr call)
{
	Check(! pretty_stream_p(stream), "type error");
	if (call_pretty_stream(ptr, stream, call))
		return;
	setresult_control(ptr, Nil);
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
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_syscall_pprint_pretty);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_pprint_pretty(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eastasian-set (string-designer intplus &optional error) ...) -> boolean) */
static void syscall_eastasian_set(Execute ptr, addr var, addr value, addr errorp)
{
	eastasian_set_syscall(var, value, errorp, &var);
	setresult_control(ptr, var);
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
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_syscall_eastasian_set);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_eastasian_set(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun eastasian-get (string-designer) ...) -> (values IntplusNull symbol) */
static void syscall_eastasian_get(Execute ptr, addr var)
{
	addr symbol;
	eastasian_get_syscall(var, &var, &symbol);
	setvalues_control(ptr, var, symbol, NULL);
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
	compiled_heap(&pos, symbol);
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
static void syscall_eastasian_width(Execute ptr, addr pos)
{
	addr value;
	eastasian_width_syscall(pos, &pos, &value);
	setvalues_control(ptr, pos, value, NULL);
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
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_syscall_eastasian_width);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_syscall_eastasian_width(&type);
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
	SetPointerSysCall(defun, dynamic, redirect_restart_call);
	SetPointerSysCall(defmacro, macro, symbol_macro_expander);
	SetPointerSysCall(defun, var3, defconstant);
	SetPointerSysCall(defun, var1, in_package);
	SetPointerSysCall(defun, var3, setplist);
	SetPointerSysCall(defun, var2, remplist);
	SetPointerSysCall(defun, var1, make_hash_iterator);
	SetPointerSysCall(defun, var1, next_hash_iterator);
	SetPointerSysCall(defun, dynamic, make_package_iterator);
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
	SetPointerSysCall(defun, var1, short_float_p);
	SetPointerSysCall(defun, var1, single_float_p);
	SetPointerSysCall(defun, var1, double_float_p);
	SetPointerSysCall(defun, var1, long_float_p);
	SetPointerSysCall(defun, var1opt1, large_number);
	SetPointerSysCall(defun, var1dynamic, format_formatter);
	SetPointerSysCall(defun, var5, print_unreadable_call);
	SetPointerSysCall(defun, var2, write_default);
	SetPointerSysCall(defun, var1, make_bignum);
	SetPointerSysCall(defun, var2, make_ratio);
	SetPointerSysCall(defun, var2, make_complex);
	SetPointerSysCall(defun, var2, equal_random_state);
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
	SetPointerSysCall(defun, var3, eastasian_set);
	SetPointerSysCall(defun, var1, eastasian_get);
	SetPointerSysCall(defun, var1, eastasian_width);
}

_g void build_syscall(void)
{
	/* system call */
	defun_hello();
	defun_infobit();
	defun_infoprint();
	defun_gc();
	defun_savecore();
	/* hander/restart */
	defun_redirect_restart();
	/* symbol macro */
	defun_symbol_macro_expander();
	defun_defconstant();
	/* package */
	defun_in_package();
	/* getf */
	defun_setplist();
	defun_remplist();
	/* hash-table */
	defun_make_hash_iterator();
	defun_next_hash_iterator();
	/* package */
	defun_make_package_iterator();
	defun_next_package_iterator();
	defun_defpackage();
	defun_do_symbols();
	defun_do_external_symbols();
	defun_do_all_symbols();
	/* document */
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
	/* streams */
	defun_end_input_stream();
	defun_make_extend_output_stream();
	defun_prompt_for();
	/* type */
	defun_closp();
	defun_fixnump();
	defun_bignump();
	defun_ratiop();
	defun_short_float_p();
	defun_single_float_p();
	defun_double_float_p();
	defun_long_float_p();
	/* printer */
	defun_large_number();
	defun_format_formatter();
	defun_print_unreadable_call();
	defun_write_default();
	/* number */
	defun_make_bignum();
	defun_make_ratio();
	defun_make_complex();
	defun_equal_random_state();
	/* type */
	defun_symbol_deftype();
	defun_delete_deftype();
	/* structure */
	defun_ensure_structure();
	defun_structure_constructor();
	/* loop */
	defun_loop_bind();
	/* print */
	defun_make_pprint_stream();
	defun_pprint_gensym();
	defun_pprint_exit();
	defun_pprint_pop();
	defun_pprint_check();
	defun_pprint_close();
	defun_pprint_pretty();
	/* eastasian */
	defun_eastasian_set();
	defun_eastasian_get();
	defun_eastasian_width();
}

