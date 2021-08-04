/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */
#include "callname.h"
#include "clos.h"
#include "clos_combination.h"
#include "clos_defgeneric.h"
#include "clos_method.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "control_operator.h"
#include "function.h"
#include "integer.h"
#include "lambda.h"
#include "mop.h"
#include "mop_common.h"
#include "symbol.h"
#include "type_table.h"

/***********************************************************************
 *  no-applicable-method
 ***********************************************************************/
static int defgeneric_no_applicable_method_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_NO_APPLICABLE_METHOD, &symbol);
	mop_argument_generic_var1rest(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  no-next-method
 ***********************************************************************/
static int defgeneric_no_next_method_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_NO_NEXT_METHOD, &symbol);
	mop_argument_generic_var2rest(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  ensure-generic-function-using-class
 ***********************************************************************/
static int method_ensure_generic_function_class(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	Return(mop_generic_change_(ptr, clos, name, rest));
	setresult_control(ptr, clos);
	return 0;
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

static int defmethod_ensure_generic_function_class_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_ensure_generic_function_class);
	method_type_ensure_generic_function_class(&type);
	settype_function(call, type);
	/* method */
	method_argument_ensure_generic_function_class(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int method_ensure_generic_function_null(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	Check(clos != Nil, "error");
	Return(mop_generic_new_(ptr, name, rest, &name));
	setresult_control(ptr, name);
	return 0;
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

static int defmethod_ensure_generic_function_null_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_ensure_generic_function_null);
	method_type_ensure_generic_function_null(&type);
	settype_function(call, type);
	/* method */
	method_argument_ensure_generic_function_null(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_ensure_generic_function_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_ENSURE_GENERIC_FUNCTION_USING_CLASS, &symbol);
	mop_argument_generic_var2rest(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_ensure_generic_function_class_(ptr, name, gen));
	Return(defmethod_ensure_generic_function_null_(ptr, name, gen));
	return common_method_finalize_(gen);
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
static int function_ensure_method(Execute ptr, addr name, addr rest)
{
	addr lambda, qua, spec, call;

	/* arguments */
	if (GetKeyArgs(rest, CLOSKEY_LAMBDA_LIST, &lambda))
		lambda = Nil;
	if (GetKeyArgs(rest, CLOSKEY_QUALIFIERS, &qua))
		qua = Nil;
	if (GetKeyArgs(rest, CLOSKEY_SPECIALIZERS, &spec))
		spec = Nil;
	if (GetKeyArgs(rest, CLOSKEY_FUNCTION, &call))
		return fmte_("Invalid ensure-method argument :function ~S.", call, NULL);

	/* add method */
	Return(ensure_method_common_(ptr, &name, name, lambda, qua, spec, call));
	setresult_control(ptr, name);

	return 0;
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
	compiled_system(&pos, symbol);
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
static int method_function_keywords(Execute ptr, addr method, addr next, addr var)
{
	int allow;

	Return(stdget_method_lambda_list_(var, &var));
	Return(argument_method_keywords_heap_(var, &var, &allow));
	setvalues_control(ptr, var, (allow? T: Nil), NULL);

	return 0;
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

static int defmethod_function_keywords_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_function_keywords);
	method_type_function_keywords(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_function_keywords_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_FUNCTION_KEYWORDS, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_function_keywords_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  flet-method-p
 ***********************************************************************/
/* (defun clos::flet-method-p (var) ...) -> boolean */
static int function_flet_method_p(Execute ptr, addr var)
{
	setbool_control(ptr, var != Nil);
	return 0;
}

static void defun_flet_method_p_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_FLET_METHOD_P, &symbol);
	compiled_system(&pos, symbol);
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
static int function_flet_next_method(Execute ptr,
		addr method, addr next, addr args, addr rest)
{
	addr call;
	LocalRoot local;

	if (next == Nil) {
		Return(stdget_method_generic_function_(method, &method));
		return fmte_("There is no method in generic function ~S.", method, NULL);
	}
	Return_getcons(next, &method, &next);
	Return(stdget_method_function_(method, &call));
	if (rest == Nil)
		rest = args;
	/* call method */
	local = ptr->local;
	lista_local(local, &rest, method, next, rest, NULL);
	return apply_control(ptr, call, rest);
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
	compiled_system(&pos, symbol);
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
static int function_method_combination_instance(Execute ptr, addr var)
{
	clos_find_combination_nil(var, &var);
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_ensure_method_combination_short(Execute ptr, addr var, addr rest)
{
	addr doc, ident, oper;

	if (GetKeyArgs(rest, KEYWORD_DOCUMENTATION, &doc))
		doc = Nil;
	if (GetKeyArgs(rest, KEYWORD_IDENTITY_WITH_ONE_ARGUMENT, &ident))
		ident = Nil;
	if (GetKeyArgs(rest, KEYWORD_OPERATOR, &oper))
		oper = var;
	Return(ensure_define_combination_short_common_(var, doc, ident, oper));
	setresult_control(ptr, var);

	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_ensure_method_combination_long(Execute ptr,
		addr name, addr lambda, addr spec, addr rest)
{
	addr args, gen, doc, form, decl;

	if (GetKeyArgs(rest, KEYWORD_ARGUMENTS, &args))
		args = Nil;
	if (GetKeyArgs(rest, KEYWORD_GENERIC_FUNCTION, &gen))
		gen = Nil;
	if (GetKeyArgs(rest, KEYWORD_DOCUMENTATION, &doc))
		doc = Nil;
	if (GetKeyArgs(rest, CLOSKEY_FORM, &form))
		form = Nil;
	if (GetKeyArgs(rest, CLOSKEY_DECLARE, &decl))
		decl = Nil;
	Return(ensure_define_combination_long_common_(name,
				lambda, spec, args, gen, doc, form, decl));
	setresult_control(ptr, name);

	return 0;
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
	compiled_system(&pos, symbol);
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
static int qualifiers_elt_order_(addr symbol, addr order, int *ret)
{
	addr value;

	GetConst(KEYWORD_MOST_SPECIFIC_FIRST, &value);
	if (value == order)
		return Result(ret, 0);
	GetConst(KEYWORD_MOST_SPECIFIC_LAST, &value);
	if (value == order)
		return Result(ret, 1);
	/* error */
	*ret = 0;
	return fmte_("Invalid :order ~S in the qualifiers ~S.", order, symbol, NULL);
}

static int function_qualifiers_elt(Execute ptr,
		addr symbol, addr pos, addr index, addr order, addr req)
{
	int check;
	size_t size;

	Return(getindex_integer_(index, &size));
	getarray(pos, size, &pos);
	if (req != Nil && pos == Nil)
		return fmte_("The qualifier ~S must be at least one method.", symbol, NULL);
	Return(qualifiers_elt_order_(symbol, order, &check));
	if (check) {
		Return(reverse_list_heap_safe_(&pos, pos));
	}
	setresult_control(ptr, pos);

	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_combination_binding(Execute ptr, addr var)
{
	Return(stdget_longcomb_binding_(var, &var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_macro_make_method(Execute ptr, addr gen, addr form)
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

	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_macro_call_method(Execute ptr, addr car, addr cdr, addr symbol)
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
		Return_getcons(cdr, &pos, &cdr);
		cons_heap(&root, pos, root);
	}
	nreverse(&root, root);
	/* apply */
	list_heap(&methodf, methodf, method, NULL);
	list_heap(&apply, apply, methodf, method, root, symbol, NULL);
	list_heap(&let, let, args, apply, NULL);
	/* result */
	setresult_control(ptr, let);

	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_macro_method_lambda(Execute ptr, addr gen, addr call)
{
	addr make, clos;

	/* make-instance */
	Return(stdget_generic_method_class_(gen, &clos));
	GetConst(COMMON_MAKE_INSTANCE, &make);
	Return(getfunction_global_(make, &make));
	Return(funcall_control(ptr, make, clos, NULL));
	getresult_control(ptr, &clos);
	Return(stdset_method_function_(clos, call));
	setresult_control(ptr, clos);

	return 0;
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
	compiled_system(&pos, symbol);
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
static int method_compute_applicable_methods_std(Execute ptr,
		addr method, addr next, addr clos, addr args)
{
	Return(generic_compute_applicable_methods_(ptr->local, clos, args, &args));
	setresult_control(ptr, args);
	return 0;
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

static int defmethod_compute_applicable_methods_std_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4(call, p_method_compute_applicable_methods_std);
	method_type_compute_applicable_methods_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_compute_applicable_methods_std(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_compute_applicable_methods_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_COMPUTE_APPLICABLE_METHODS, &symbol);
	mop_argument_generic_var2(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	Return(defmethod_compute_applicable_methods_std_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  find-method
 ***********************************************************************/
static int method_find_method_std(Execute ptr,
		addr method, addr next, addr clos, addr qua, addr spec, addr errorp)
{
	if (errorp == Unbound)
		errorp = T;
	Return(generic_find_method_(ptr, clos, qua, spec, errorp, &qua));
	setresult_control(ptr, qua);

	return 0;
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

static int defmethod_find_method_std_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var5opt1(call, p_method_find_method_std);
	method_type_find_method_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_find_method_std(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_find_method_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_FIND_METHOD, &symbol);
	mop_argument_generic_var3opt1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	Return(defmethod_find_method_std_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  add-method
 ***********************************************************************/
static int method_add_method_std(Execute ptr,
		addr method, addr next, addr gen, addr met)
{
	Return(method_add_method_(ptr, gen, met));
	setresult_control(ptr, gen);
	return 0;
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

static int defmethod_add_method_std_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4(call, p_method_add_method_std);
	method_type_add_method_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_add_method_std(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_add_method_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_ADD_METHOD, &symbol);
	mop_argument_generic_var2(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	Return(defmethod_add_method_std_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  intern
 ***********************************************************************/
static int method_remove_method_std(Execute ptr,
		addr method, addr next, addr gen, addr met)
{
	Return(method_remove_method_(ptr, gen, met));
	setresult_control(ptr, gen);
	return 0;
}

static int defmethod_remove_method_std_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4(call, p_method_remove_method_std);
	method_type_add_method_std(&type);
	settype_function(call, type);
	/* method */
	method_argument_add_method_std(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_remove_method_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_REMOVE_METHOD, &symbol);
	mop_argument_generic_var2(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* no-method */
	Return(defmethod_remove_method_std_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  function
 ***********************************************************************/
void init_mop_generic(void)
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

int build_mop_generic_(Execute ptr)
{
	/* defclass */
	Return(defgeneric_no_applicable_method_mop_(ptr));
	Return(defgeneric_no_next_method_mop_(ptr));
	/* defgeneric */
	Return(defgeneric_ensure_generic_function_using_class_mop_(ptr));
	defun_ensure_method_mop();
	Return(defgeneric_function_keywords_mop_(ptr));
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
	Return(defgeneric_compute_applicable_methods_mop_(ptr));
	Return(defgeneric_find_method_mop_(ptr));
	Return(defgeneric_add_method_mop_(ptr));
	Return(defgeneric_remove_method_mop_(ptr));

	return 0;
}

