/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */
#include "clos.h"
#include "clos_combination.h"
#include "clos_common.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control.h"
#include "function.h"
#include "integer.h"
#include "lambda.h"
#include "mop.h"
#include "symbol.h"
#include "type_table.h"

/***********************************************************************
 *  no-applicable-method
 ***********************************************************************/
static void defgeneric_no_applicable_method_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_NO_APPLICABLE_METHOD, &symbol);
	mop_argument_generic_var1rest(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	common_method_finalize(gen);
}


/***********************************************************************
 *  no-next-method
 ***********************************************************************/
static void defgeneric_no_next_method_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_NO_NEXT_METHOD, &symbol);
	mop_argument_generic_var2rest(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	common_method_finalize(gen);
}


/***********************************************************************
 *  ensure-generic-function-using-class
 ***********************************************************************/
static void method_ensure_generic_function_struct(struct generic_argument *str,
		Execute ptr, addr clos, addr name, addr rest)
{
	addr order, decl, doc, env, gen, lambda, method, comb;

	/* arguments */
	if (getkeyargs(rest, KEYWORD_ARGUMENT_PRECEDENCE_ORDER, &order))
		order = Nil;
	if (getkeyargs(rest, KEYWORD_DECLARE, &decl))
		decl = Nil;
	if (getkeyargs(rest, KEYWORD_DOCUMENTATION, &doc))
		doc = Nil;
	if (getkeyargs(rest, KEYWORD_ENVIRONMENT, &env))
		env = Nil;
	if (getkeyargs(rest, KEYWORD_LAMBDA_LIST, &lambda))
		lambda = Nil;
	if (getkeyargs(rest, KEYWORD_METHOD_COMBINATION, &comb))
		comb = Nil;
	if (getkeyargs(rest, KEYWORD_GENERIC_FUNCTION_CLASS, &gen))
		GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &gen);
	if (getkeyargs(rest, KEYWORD_METHOD_CLASS, &method))
		GetConst(CLOS_STANDARD_METHOD, &method);

	/* parse */
	if (! callnamep(name))
		parse_callname_error(&name, name);
	if (! argumentp(lambda))
		argument_generic_heap(ptr->local, &lambda, lambda);

	/* generic-addr */
	str->ptr = ptr;
	str->env = env;
	str->name = name;
	str->lambda = lambda;
	str->generic = gen;
	str->method = method;
	str->combination = comb;
	str->order = order;
	str->declare = decl;
	str->doc = doc;
}

static void method_ensure_generic_function_class(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	struct generic_argument str;

	method_ensure_generic_function_struct(&str, ptr, clos, name, rest);
	if (generic_change(&str, &name)) return;
	setresult_control(ptr, name);
}

static void method_type_ensure_generic_function_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, T);
	typeargs_var2rest(&args, args, values, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_ensure_generic_function_class(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	str->rest = 1;
	str->keyp = 1;
	ArgumentMethod_var(&type1, GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static void defmethod_ensure_generic_function_class(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4dynamic(call, p_method_ensure_generic_function_class);
	method_type_ensure_generic_function_class(&type);
	settype_function(call, type);
	/* method */
	method_argument_ensure_generic_function_class(&pos);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void method_ensure_generic_function_null(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	struct generic_argument str;

	method_ensure_generic_function_struct(&str, ptr, clos, name, rest);
	if (generic_add(&str, &name)) return;
	setresult_control(ptr, name);
}

static void method_type_ensure_generic_function_null(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Null);
	GetTypeTable(&values, T);
	typeargs_var2rest(&args, args, values, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_ensure_generic_function_null(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	str->rest = 1;
	str->keyp = 1;
	ArgumentMethod_var(&type1, NULL);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static void defmethod_ensure_generic_function_null(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4dynamic(call, p_method_ensure_generic_function_null);
	method_type_ensure_generic_function_null(&type);
	settype_function(call, type);
	/* method */
	method_argument_ensure_generic_function_null(&pos);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void defgeneric_ensure_generic_function_using_class_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_ENSURE_GENERIC_FUNCTION_USING_CLASS, &symbol);
	mop_argument_generic_var2rest(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_ensure_generic_function_class(ptr, name, gen);
	defmethod_ensure_generic_function_null(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  ensure-method
 ***********************************************************************/
/* `(defun ensure-method (name
 *      &key lambda-list qualifiers specializers function) ...)
 *      -> method
 *    name           function-name
 *    lambda-list    list
 *    qualifiers     list
 *    specializers   list
 *    function       function
 */
static void function_ensure_method(Execute ptr, addr name, addr rest)
{
	addr lambda, qua, spec, call;

	/* arguments */
	if (getkeyargs(rest, CLOSKEY_LAMBDA_LIST, &lambda))
		lambda = Nil;
	if (getkeyargs(rest, CLOSKEY_QUALIFIERS, &qua))
		qua = Nil;
	if (getkeyargs(rest, CLOSKEY_SPECIALIZERS, &spec))
		spec = Nil;
	if (getkeyargs(rest, CLOSKEY_FUNCTION, &call))
		fmte("Invalid ensure-method argument :function ~S.", call, NULL);

	/* add method */
	ensure_method_common(ptr, &name, name, lambda, qua, spec, call);
	setresult_control(ptr, name);
}

static void type_ensure_method(addr *ret)
{
	addr args, values, key, key1, key2, key3, key4;

	/* key */
	keytypetable(CONSTANT_CLOSKEY_LAMBDA_LIST, TypeTable_T, &key1);
	keytypetable(CONSTANT_CLOSKEY_QUALIFIERS, TypeTable_List, &key2);
	keytypetable(CONSTANT_CLOSKEY_SPECIALIZERS, TypeTable_List, &key3);
	keytypetable(CONSTANT_CLOSKEY_FUNCTION, TypeTable_Function, &key4);
	list_heap(&key, key1, key2, key3, key4, NULL);
	/* type */
	GetTypeTable(&args, FunctionName);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_method_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_METHOD, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_function_ensure_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  function-keywords
 ***********************************************************************/
static void method_function_keywords(Execute ptr, addr method, addr next, addr var)
{
	int allow;

	stdget_method_lambda_list(var, &var);
	argument_method_keywords_heap(var, &var, &allow);
	setvalues_control(ptr, var, (allow? T: Nil), NULL);
}

static void method_type_function_keywords(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeTable(&values, List);
	GetTypeTable(&type, Boolean);
	typevalues_values2(&values, values, type);
	type_compiled_heap(args, values, ret);
}

static void method_argument_function_keywords(addr *ret)
{
	addr pos, list;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 1;
	ArgumentMethod_var(&list, STANDARD_METHOD);
	list_heap(&list, list, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static void defmethod_function_keywords(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_function_keywords);
	method_type_function_keywords(&type);
	settype_function(call, type);
	/* method */
	method_argument_function_keywords(&pos);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void defgeneric_function_keywords_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_FUNCTION_KEYWORDS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_function_keywords(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  flet-method-p
 ***********************************************************************/
/* (defun clos::flet-method-p (var) ...) -> boolean */
static void function_flet_method_p(Execute ptr, addr var)
{
	setbool_control(ptr, var != Nil);
}

static void defun_flet_method_p_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_FLET_METHOD_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_flet_method_p);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  flet-next-method
 ***********************************************************************/
/* (defun clos::flet-next-method (method next args rest) ...) -> t */
static void function_flet_next_method(Execute ptr,
		addr method, addr next, addr args, addr rest)
{
	addr call;
	LocalRoot local;
	LocalStack stack;

	if (next == Nil) {
		stdget_method_generic_function(method, &method);
		fmte("There is no method in generic function ~S.", method, NULL);
		return;
	}
	getcons(next, &method, &next);
	stdget_method_function(method, &call);
	if (rest == Nil)
		rest = args;
	/* call method */
	local = ptr->local;
	push_local(local, &stack);
	lista_local(local, &rest, method, next, rest, NULL);
	if (apply_control(ptr, call, rest)) return;
	rollback_local(local, stack);
}

static void type_flet_next_method(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var4(&args, args, values, values, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_flet_next_method_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_FLET_NEXT_METHOD, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var4(pos, p_defun_flet_next_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_flet_next_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  method-combination-instance
 ***********************************************************************/
static void function_method_combination_instance(Execute ptr, addr var)
{
	clos_find_combination_nil(var, &var);
	setresult_control(ptr, var);
}

static void type_method_combination_instance(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void define_method_combination_instance_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_METHOD_COMBINATION_INSTANCE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_method_combination_instance);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_method_combination_instance(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  define-method-combination-short
 ***********************************************************************/
static void function_ensure_method_combination_short(Execute ptr, addr var, addr rest)
{
	addr doc, ident, oper;

	if (getkeyargs(rest, KEYWORD_DOCUMENTATION, &doc))
		doc = Nil;
	if (getkeyargs(rest, KEYWORD_IDENTITY_WITH_ONE_ARGUMENT, &ident))
		ident = Nil;
	if (getkeyargs(rest, KEYWORD_OPERATOR, &oper))
		oper = var;
	ensure_define_combination_short_common(var, doc, ident, oper);
	setresult_control(ptr, var);
}

static void type_ensure_method_combination_short(addr *ret)
{
	addr args, values, key, key1, key2, key3;

	/* key */
	KeyTypeTable(&key1, DOCUMENTATION, String);
	KeyTypeTable(&key2, IDENTITY_WITH_ONE_ARGUMENT, T);
	KeyTypeTable(&key3, OPERATOR, Symbol);
	list_heap(&key, key1, key2, key3, NULL);
	/* type */
	GetTypeTable(&args, Symbol);
	typeargs_var1key(&args, args, key);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_define_combination_short_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_METHOD_COMBINATION_SHORT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_ensure_method_combination_short);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_method_combination_short(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  define-method-combination-long
 ***********************************************************************/
static void function_ensure_method_combination_long(Execute ptr,
		addr name, addr lambda, addr spec, addr rest)
{
	addr args, gen, doc, form, decl;

	if (getkeyargs(rest, KEYWORD_ARGUMENTS, &args))
		args = Nil;
	if (getkeyargs(rest, KEYWORD_GENERIC_FUNCTION, &gen))
		gen = Nil;
	if (getkeyargs(rest, KEYWORD_DOCUMENTATION, &doc))
		doc = Nil;
	if (getkeyargs(rest, CLOSKEY_FORM, &form))
		form = Nil;
	if (getkeyargs(rest, CLOSKEY_DECLARE, &decl))
		decl = Nil;
	ensure_define_combination_long_common(name,
			lambda, spec, args, gen, doc, form, decl);
	setresult_control(ptr, name);
}

static void type_ensure_method_combination_long(addr *ret)
{
	addr args, values, key, key1, key2, key3, key4;

	/* key */
	KeyTypeTable(&key1, ARGUMENTS, T);
	KeyTypeTable(&key2, GENERIC_FUNCTION, T);
	KeyTypeTable(&key3, DOCUMENTATION, String);
	keytypetable(CONSTANT_CLOSKEY_FORM, TypeTable_T, &key4);
	list_heap(&key, key1, key2, key3, key4, NULL);
	/* type */
	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var3key(&args, args, values, values, key);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_define_combination_long_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_METHOD_COMBINATION_LONG, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3dynamic(pos, p_defun_ensure_method_combination_long);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_method_combination_long(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  qualifiers-elt
 ***********************************************************************/
/* (defun qualifiers-elt (symbol vector index order required) ...) -> result
 *   symbol   symbol
 *   vector   vector
 *   index    index
 *   order    keyword
 *   require  t
 *   result   list
 */
static int qualifiers_elt_order(addr symbol, addr order)
{
	addr check;

	GetConst(KEYWORD_MOST_SPECIFIC_FIRST, &check);
	if (check == order)
		return 0;
	GetConst(KEYWORD_MOST_SPECIFIC_LAST, &check);
	if (check == order)
		return 1;
	/* error */
	fmte("Invalid :order ~S in the qualifiers ~S.", order, symbol, NULL);
	return 0;
}

static void function_qualifiers_elt(Execute ptr,
		addr symbol, addr pos, addr index, addr order, addr req)
{
	size_t value;

	getindex_error(index, &value);
	getarray(pos, value, &pos);
	if (req != Nil && pos == Nil)
		fmte("The qualifier ~S must be at least one method.", symbol, NULL);
	if (qualifiers_elt_order(symbol, order))
		reverse_list_heap_safe(&pos, pos);
	setresult_control(ptr, pos);
}

static void type_qualifiers_elt(addr *ret)
{
	addr args, values, type1, type2, type3;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, Vector);
	GetTypeTable(&type1, Index);
	GetTypeTable(&type2, Keyword);
	GetTypeTable(&type3, T);
	typeargs_var5(&args, args, values, type1, type2, type3);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_qualifiers_elt_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_QUALIFIERS_ELT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var5(pos, p_defun_qualifiers_elt);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_qualifiers_elt(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  combination-binding
 ***********************************************************************/
static void function_combination_binding(Execute ptr, addr var)
{
	stdget_longcomb_binding(var, &var);
	setresult_control(ptr, var);
}

static void type_combination_binding(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_combination_binding_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_COMBINATION_BINDING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_combination_binding);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_combination_binding(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  macro-make-method
 ***********************************************************************/
static void function_macro_make_method(Execute ptr, addr gen, addr form)
{
	/* `(macro-method-lambda
	 *    ,gen
	 *    (lambda (#:method #:next &rest #:args)
	 *      (declare (ignore #:method #:next #:args))
	 *      ,form))
	 */
	addr make, lambda, method, next, args, rest, declare, ignore;

	GetConst(CLOSNAME_MACRO_METHOD_LAMBDA, &make);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORE, &ignore);
	GetConst(AMPERSAND_REST, &rest);
	make_symbolchar(&method, "METHOD");
	make_symbolchar(&next, "NEXT");
	make_symbolchar(&args, "ARGS");
	list_heap(&ignore, ignore, method, next, args, NULL);
	list_heap(&declare, declare, ignore, NULL);
	list_heap(&method, method, next, rest, args, NULL);
	list_heap(&lambda, lambda, method, declare, form, NULL);
	list_heap(&form, make, gen, lambda, NULL);
	setresult_control(ptr, form);
}

static void type_macro_make_method(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_macro_make_method(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_MACRO_MAKE_METHOD, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_macro_make_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_macro_make_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  macro-call-method
 ***********************************************************************/
static void function_macro_call_method(Execute ptr, addr car, addr cdr, addr symbol)
{
	/* `(let ((#:method ,car))
	 *    (apply (method-function ,method) ,method (list ,@cdr) ,symbol))
	 */
	addr let, apply, methodf, list, method;
	addr args, root, pos;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_APPLY, &apply);
	GetConst(CLOSNAME_METHOD_FUNCTION, &methodf);
	GetConst(COMMON_LIST, &list);
	make_symbolchar(&method, "METHOD");
	/* args */
	list_heap(&args, method, car, NULL);
	list_heap(&args, args, NULL);
	/* list */
	conscar_heap(&root, list);
	while (cdr != Nil) {
		getcons(cdr, &pos, &cdr);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(&root, root);
	/* apply */
	list_heap(&methodf, methodf, method, NULL);
	list_heap(&apply, apply, methodf, method, root, symbol, NULL);
	list_heap(&let, let, args, apply, NULL);
	/* result */
	setresult_control(ptr, let);
}

static void type_macro_call_method(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	GetTypeTable(&type, Symbol);
	typeargs_var3(&args, args, values, type);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_macro_call_method(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_MACRO_CALL_METHOD, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_macro_call_method);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_macro_call_method(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  macro-method-lambda
 ***********************************************************************/
static void function_macro_method_lambda(Execute ptr, addr gen, addr call)
{
	addr make, clos;

	/* make-instance */
	stdget_generic_method_class(gen, &clos);
	GetConst(COMMON_MAKE_INSTANCE, &make);
	getfunctioncheck_local(ptr, make, &make);
	if (funcall_control(ptr, make, clos, NULL))
		return;
	getresult_control(ptr, &clos);
	stdset_method_function(clos, call);
	setresult_control(ptr, clos);
}

static void type_macro_method_lambda(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_macro_method_lambda(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_MACRO_METHOD_LAMBDA, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_macro_method_lambda);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_macro_method_lambda(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  compute-applicable-methods
 ***********************************************************************/
static void method_compute_applicable_methods_std(Execute ptr,
		addr method, addr next, addr clos, addr args)
{
	generic_compute_applicable_methods(ptr->local, clos, args, &args);
	setresult_control(ptr, args);
}

static void method_type_compute_applicable_methods_std(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void method_argument_compute_applicable_methods_std(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	ArgumentMethod_var(&type1, STANDARD_GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static void defmethod_compute_applicable_methods_std(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_compute_applicable_methods_std);
	method_type_compute_applicable_methods_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_compute_applicable_methods_std(&pos);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void defgeneric_compute_applicable_methods_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_COMPUTE_APPLICABLE_METHODS, &symbol);
	mop_argument_generic_var2(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	defmethod_compute_applicable_methods_std(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  find-method
 ***********************************************************************/
static void method_find_method_std(Execute ptr,
		addr method, addr next, addr clos, addr qua, addr spec, addr errorp)
{
	if (errorp == Unbound)
		errorp = T;
	generic_find_method(ptr, clos, qua, spec, errorp, &qua);
	setresult_control(ptr, qua);
}

static void method_type_find_method_std(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var3opt1(&args, args, values, values, args);
	typeargs_method(args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void method_argument_find_method_std(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 3;
	str->opt = 1;
	ArgumentMethod_var(&type1, STANDARD_GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static void defmethod_find_method_std(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5opt1(call, p_method_find_method_std);
	method_type_find_method_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_find_method_std(&pos);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void defgeneric_find_method_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_FIND_METHOD, &symbol);
	mop_argument_generic_var3opt1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	defmethod_find_method_std(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  add-method
 ***********************************************************************/
static void method_add_method_std(Execute ptr,
		addr method, addr next, addr gen, addr met)
{
	method_add_method(ptr, gen, met);
	setresult_control(ptr, gen);
}

static void method_type_add_method_std(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var2(&args, args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_add_method_std(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	ArgumentMethod_var(&type1, STANDARD_GENERIC_FUNCTION);
	ArgumentMethod_var(&type2, METHOD);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static void defmethod_add_method_std(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_add_method_std);
	method_type_add_method_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_add_method_std(&pos);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void defgeneric_add_method_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_ADD_METHOD, &symbol);
	mop_argument_generic_var2(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	defmethod_add_method_std(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  intern
 ***********************************************************************/
static void method_remove_method_std(Execute ptr,
		addr method, addr next, addr gen, addr met)
{
	method_remove_method(ptr, gen, met);
	setresult_control(ptr, gen);
}

static void defmethod_remove_method_std(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_remove_method_std);
	method_type_add_method_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_add_method_std(&pos);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void defgeneric_remove_method_mop(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_REMOVE_METHOD, &symbol);
	mop_argument_generic_var2(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	defmethod_remove_method_std(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  function
 ***********************************************************************/
_g void init_mop_generic(void)
{
	SetPointerType(var4dynamic, method_ensure_generic_function_class);
	SetPointerType(var4dynamic, method_ensure_generic_function_null);
	SetPointerType(var1dynamic, function_ensure_method);
	SetPointerType(var3, method_function_keywords);
	SetPointerCall(defun, var1, flet_method_p);
	SetPointerCall(defun, var4, flet_next_method);
	SetPointerCall(defun, var1, method_combination_instance);
	SetPointerCall(defun, var1dynamic, ensure_method_combination_short);
	SetPointerCall(defun, var3dynamic, ensure_method_combination_long);
	SetPointerCall(defun, var5, qualifiers_elt);
	SetPointerCall(defun, var1, combination_binding);
	SetPointerCall(defun, var2, macro_make_method);
	SetPointerCall(defun, var3, macro_call_method);
	SetPointerCall(defun, var2, macro_method_lambda);
	SetPointerType(var4, method_compute_applicable_methods_std);
	SetPointerType(var5opt1, method_find_method_std);
	SetPointerType(var4, method_add_method_std);
	SetPointerType(var4, method_remove_method_std);
}

_g void build_mop_generic(Execute ptr)
{
	/* defclass */
	defgeneric_no_applicable_method_mop(ptr);
	defgeneric_no_next_method_mop(ptr);
	/* defgeneric */
	defgeneric_ensure_generic_function_using_class_mop(ptr);
	defun_ensure_method_mop();
	defgeneric_function_keywords_mop(ptr);
	/* defmethod */
	defun_flet_method_p_mop();
	defun_flet_next_method_mop();
	/* define-method-combination */
	define_method_combination_instance_mop();
	defun_ensure_define_combination_short_mop();
	defun_ensure_define_combination_long_mop();
	defun_qualifiers_elt_mop();
	defun_combination_binding_mop();
	defun_macro_make_method();
	defun_macro_call_method();
	defun_macro_method_lambda();
	/* common */
	defgeneric_compute_applicable_methods_mop(ptr);
	defgeneric_find_method_mop(ptr);
	defgeneric_add_method_mop(ptr);
	defgeneric_remove_method_mop(ptr);
}

