#include "clos.h"
#include "clos_class.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control.h"
#include "document.h"
#include "function.h"
#include "lambda.h"
#include "mop.h"
#include "package.h"
#include "structure.h"
#include "symbol.h"
#include "type_deftype.h"
#include "type_table.h"

/*
 *  generic function
 */
static void defun_documentation_order(addr *ret, addr *rlist, addr *rindex)
{
	addr pos, object, doc_type, list, index;
	struct argument_struct *str;

	/* lambda */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 2;

	/* var */
	GetConst(SYSTEM_OBJECT, &object);
	GetConst(SYSTEM_DOC_TYPE, &doc_type);
	list_heap(&list, object, doc_type, NULL);
	SetArgument(pos, ArgumentIndex_var, list);

	/* order */
	list_heap(&list, doc_type, object, NULL);
	index_heap(&object, 0);
	index_heap(&doc_type, 1);
	list_heap(&index, doc_type, object, NULL);

	/* result */
	*ret = pos;
	*rlist = list;
	*rindex = index;
}

static void defun_setf_documentation_order(addr *ret, addr *rlist, addr *rindex)
{
	addr pos, value, object, doc_type, list, index;
	struct argument_struct *str;

	/* lambda */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 3;

	/* var */
	GetConst(SYSTEM_VALUE, &value);
	GetConst(SYSTEM_OBJECT, &object);
	GetConst(SYSTEM_DOC_TYPE, &doc_type);
	list_heap(&list, value, object, doc_type, NULL);
	SetArgument(pos, ArgumentIndex_var, list);

	/* order */
	list_heap(&list, doc_type, object, value, NULL);
	index_heap(&value, 0);
	index_heap(&object, 1);
	index_heap(&doc_type, 2);
	list_heap(&index, doc_type, object, value, NULL);

	/* result */
	*ret = pos;
	*rlist = list;
	*rindex = index;
}

static void method_type_documentation(addr *ret, enum TypeTable type, constindex index)
{
	addr args, values;

	gettypetable(type, &args);
	GetConstant(index, &values);
	type_eql_heap(values, &values);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(args, values, ret);
}
#define MethodTypeDocumentation(r,a,b) \
	method_type_documentation((r), TypeTable_##a, CONSTANT_COMMON_##b)

static void mop_argument_method_documentation(addr *ret, constindex a, constindex b)
{
	addr pos, pos1, pos2, x, y;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* (object class) */
	GetConst(SYSTEM_OBJECT, &x);
	GetConstant(a, &y);
	Check(! closp(y), "type error, class");
	list_heap(&pos1, x, y, NULL);
	/* (doc-type (eql symbol)) */
	GetConst(SYSTEM_DOC_TYPE, &x);
	GetConstant(b, &y);
	Check(! symbolp(y), "type error, symbol.");
	clos_intern_specializer(y, &y);
	list_heap(&pos2, x, y, NULL);
	/* var */
	str->var = 2;
	list_heap(&pos1, pos1, pos2, NULL);
	SetArgument(pos, ArgumentIndex_var, pos1);
	/* result */
	*ret = pos;
}
#define MopArgumentMethodDocumentation(r,b,c) \
	mop_argument_method_documentation((r), CONSTANT_CLOS_##b, CONSTANT_COMMON_##c)

static void method_type_setf_documentation(addr *ret,
		enum TypeTable type, constindex index)
{
	addr args, values, first;

	GetTypeTable(&first, T);
	gettypetable(type, &args);
	GetConstant(index, &values);
	type_eql_heap(values, &values);
	typeargs_var3(&args, first, args, values);
	typeargs_method(args);
	GetTypeValues(&values, String);
	type_compiled_heap(args, values, ret);
}
#define MethodTypeSetfDocumentation(r,a,b) \
	method_type_setf_documentation((r), TypeTable_##a, CONSTANT_COMMON_##b)

static void mop_argument_method_setf_documentation(addr *ret,
		constindex a, constindex b)
{
	addr pos, pos1, pos2, pos3, x, y;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* (value T) */
	GetConst(SYSTEM_VALUE, &x);
	GetConst(CLOS_T, &y);
	Check(! closp(y), "type error, class");
	list_heap(&pos1, x, y, NULL);
	/* (object class) */
	GetConst(SYSTEM_OBJECT, &x);
	GetConstant(a, &y);
	Check(! closp(y), "type error, class");
	list_heap(&pos2, x, y, NULL);
	/* (doc-type (eql symbol)) */
	GetConst(SYSTEM_DOC_TYPE, &x);
	GetConstant(b, &y);
	Check(! symbolp(y), "type error, symbol.");
	clos_intern_specializer(y, &y);
	list_heap(&pos3, x, y, NULL);
	/* var */
	str->var = 3;
	list_heap(&pos1, pos1, pos2, pos3, NULL);
	SetArgument(pos, ArgumentIndex_var, pos1);
	/* result */
	*ret = pos;
}
#define MopArgumentMethodSetfDocumentation(r,b,c) \
	mop_argument_method_setf_documentation((r), CONSTANT_CLOS_##b, CONSTANT_COMMON_##c)


/*
 *  (function (eql 't))
 */
static int method_documentation_function_t(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	getdocumentation_function(object, &object);
	setresult_control(ptr, object);
	return 0;
}

static int method_setf_documentation_function_t(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	setdocumentation_function(object, value);
	setresult_control(ptr, value);
	return 0;
}

static void documentation_function_t(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_function_t);
	MethodTypeDocumentation(&type, Function, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, FUNCTION, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_function_t(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_function_t);
	MethodTypeSetfDocumentation(&type, Function, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, FUNCTION, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (function (eql 'function))
 */
static void documentation_function_function(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_function_t);
	MethodTypeDocumentation(&type, Function, FUNCTION);
	/* method */
	MopArgumentMethodDocumentation(&pos, FUNCTION, FUNCTION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_function_function(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_function_t);
	MethodTypeSetfDocumentation(&type, Function, FUNCTION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, FUNCTION, FUNCTION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (list (eql 'function))
 */
static int method_documentation_list_function(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	parse_callname_error(&object, object);
	getfunctioncheck_callname_local(ptr, object, &object);
	getdocumentation_function(object, &object);
	setresult_control(ptr, object);

	return 0;
}

static int method_setf_documentation_list_function(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	parse_callname_error(&object, object);
	getfunctioncheck_callname_local(ptr, object, &object);
	setdocumentation_function(object, value);
	setresult_control(ptr, value);

	return 0;
}

static void documentation_list_function(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_list_function);
	MethodTypeDocumentation(&type, List, FUNCTION);
	/* method */
	MopArgumentMethodDocumentation(&pos, LIST, FUNCTION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_list_function(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_list_function);
	MethodTypeSetfDocumentation(&type, List, FUNCTION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, LIST, FUNCTION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (list (eql 'compiled-function))
 */
static int method_documentation_list_compiled_function(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	parse_callname_error(&object, object);
	getfunctioncheck_callname_local(ptr, object, &object);
	if (! compiled_function_p(object))
		TypeError(object, COMPILED_FUNCTION);
	getdocumentation_function(object, &object);
	setresult_control(ptr, object);

	return 0;
}

static int method_setf_documentation_list_compiled_function(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	parse_callname_error(&object, object);
	getfunctioncheck_callname_local(ptr, object, &object);
	if (! compiled_function_p(object))
		TypeError(object, COMPILED_FUNCTION);
	setdocumentation_function(object, value);
	setresult_control(ptr, value);

	return 0;
}

static void documentation_list_compiled_function(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_list_compiled_function);
	MethodTypeDocumentation(&type, List, COMPILED_FUNCTION);
	/* method */
	MopArgumentMethodDocumentation(&pos, LIST, COMPILED_FUNCTION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_list_compiled_function(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_list_compiled_function);
	MethodTypeSetfDocumentation(&type, List, COMPILED_FUNCTION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, LIST, COMPILED_FUNCTION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (symbol (eql 'function))
 */
static int method_documentation_symbol_function(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	getfunctioncheck_local(ptr, object, &object);
	getdocumentation_function(object, &object);
	setresult_control(ptr, object);

	return 0;
}

static int method_setf_documentation_symbol_function(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	getfunctioncheck_local(ptr, object, &object);
	setdocumentation_function(object, value);
	setresult_control(ptr, value);

	return 0;
}

static void documentation_symbol_function(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_function);
	MethodTypeDocumentation(&type, Symbol, FUNCTION);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, FUNCTION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_symbol_function(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_function);
	MethodTypeSetfDocumentation(&type, Symbol, FUNCTION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, FUNCTION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (symbol (eql 'compiler-function))
 */
static int method_documentation_symbol_compiled_function(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	getfunctioncheck_local(ptr, object, &object);
	if (! compiled_function_p(object))
		TypeError(object, COMPILED_FUNCTION);
	getdocumentation_function(object, &object);
	setresult_control(ptr, object);

	return 0;
}

static int method_setf_documentation_symbol_compiled_function(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	getfunctioncheck_local(ptr, object, &object);
	if (! compiled_function_p(object))
		TypeError(object, COMPILED_FUNCTION);
	setdocumentation_function(object, value);
	setresult_control(ptr, value);

	return 0;
}

static void documentation_symbol_compiled_function(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_compiled_function);
	MethodTypeDocumentation(&type, Symbol, COMPILED_FUNCTION);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, COMPILED_FUNCTION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_symbol_compiled_function(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_compiled_function);
	MethodTypeSetfDocumentation(&type, Symbol, COMPILED_FUNCTION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, COMPILED_FUNCTION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (symbol (eql 'setf))
 */
static int method_documentation_symbol_setf(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	addr pos;

	/* define-setf-expander, defsetf */
	getsetfmacro_symbol(object, &pos);
	/* setf-function */
	if (pos == Unbound)
		getsetfcheck_local(ptr, object, &pos);
	/* get documentation */
	getdocumentation_function(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int method_setf_documentation_symbol_setf(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	addr pos;

	/* define-setf-expander, defsetf */
	getsetfmacro_symbol(object, &pos);
	/* setf-function */
	if (pos == Unbound)
		getsetfcheck_local(ptr, object, &pos);
	/* set documentation */
	setdocumentation_function(pos, value);
	setresult_control(ptr, value);

	return 0;
}

static void documentation_symbol_setf(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_setf);
	MethodTypeDocumentation(&type, Symbol, SETF);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, SETF);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_symbol_setf(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_setf);
	MethodTypeSetfDocumentation(&type, Symbol, SETF);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, SETF);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (method-combination (eql 't))
 */
static int method_documentation_method_combination_t(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	stdget_longcomb_document(object, &object);
	setresult_control(ptr, object);
	return 0;
}

static int method_setf_documentation_method_combination_t(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	stdset_longcomb_document(object, value);
	setresult_control(ptr, value);
	return 0;
}

static void documentation_method_combination_t(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_method_combination_t);
	MethodTypeDocumentation(&type, MethodCombination, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, METHOD_COMBINATION, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_method_combination_t(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_method_combination_t);
	MethodTypeSetfDocumentation(&type, MethodCombination, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, METHOD_COMBINATION, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (method-combination (eql 'method-combination))
 */
static void documentation_method_combination_method_combination(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_method_combination_t);
	MethodTypeDocumentation(&type, MethodCombination, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodDocumentation(&pos, METHOD_COMBINATION, METHOD_COMBINATION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_method_combination_method_combination(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_method_combination_t);
	MethodTypeSetfDocumentation(&type, MethodCombination, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, METHOD_COMBINATION, METHOD_COMBINATION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (symbol (eql 'method-combination))
 */
static int method_documentation_symbol_method_combination(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	clos_find_combination(object, &object);
	stdget_longcomb_document(object, &object);
	setresult_control(ptr, object);

	return 0;
}

static int method_setf_documentation_symbol_method_combination(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	clos_find_combination(object, &object);
	stdset_longcomb_document(object, value);
	setresult_control(ptr, value);

	return 0;
}

static void documentation_symbol_method_combination(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_method_combination);
	MethodTypeDocumentation(&type, Symbol, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, METHOD_COMBINATION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_symbol_method_combination(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_method_combination);
	MethodTypeSetfDocumentation(&type, Symbol, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, METHOD_COMBINATION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (standard-method (eql 't))
 */
static int method_documentation_standard_method_t(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	methodget_document(object, &object);
	setresult_control(ptr, object);
	return 0;
}

static int method_setf_documentation_standard_method_t(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	methodset_document(object, value);
	setresult_control(ptr, value);
	return 0;
}

static void documentation_standard_method_t(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_standard_method_t);
	MethodTypeDocumentation(&type, StandardMethod, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, STANDARD_METHOD, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_standard_method_t(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_standard_method_t);
	MethodTypeSetfDocumentation(&type, StandardMethod, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STANDARD_METHOD, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (package (eql 't))
 */
static int method_documentation_package_t(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	getdocument_package(object, &object);
	setresult_control(ptr, object);
	return 0;
}

static int method_setf_documentation_package_t(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	setdocument_package(object, value);
	setresult_control(ptr, value);
	return 0;
}

static void documentation_package_t(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_package_t);
	MethodTypeDocumentation(&type, Package, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, PACKAGE, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_package_t(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_package_t);
	MethodTypeSetfDocumentation(&type, Package, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, PACKAGE, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (standard-class (eql 't))
 */
static int method_documentation_standard_class_t(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	stdget_class_document(object, &object);
	setresult_control(ptr, object);
	return 0;
}

static int method_setf_documentation_standard_class_t(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	stdset_class_document(object, value);
	setresult_control(ptr, value);
	return 0;
}

static void documentation_standard_class_t(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_standard_class_t);
	MethodTypeDocumentation(&type, StandardClass, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, STANDARD_CLASS, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_standard_class_t(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_standard_class_t);
	MethodTypeSetfDocumentation(&type, StandardClass, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STANDARD_CLASS, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (standard-class (eql 'type))
 */
static void documentation_standard_class_type(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_standard_class_t);
	MethodTypeDocumentation(&type, StandardClass, TYPE);
	/* method */
	MopArgumentMethodDocumentation(&pos, STANDARD_CLASS, TYPE);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_standard_class_type(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_standard_class_t);
	MethodTypeSetfDocumentation(&type, StandardClass, TYPE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STANDARD_CLASS, TYPE);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (structure-class (eql 't))
 */
static int method_documentation_structure_class_t(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	stdget_structure_documentation(object, &object);
	setresult_control(ptr, object);
	return 0;
}

static int method_setf_documentation_structure_class_t(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	stdset_structure_documentation(object, value);
	setresult_control(ptr, value);
	return 0;
}

static void documentation_structure_class_t(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_structure_class_t);
	MethodTypeDocumentation(&type, StructureClass, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, STRUCTURE_CLASS, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_structure_class_t(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_structure_class_t);
	MethodTypeSetfDocumentation(&type, StructureClass, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STRUCTURE_CLASS, T);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (structure-class (eql 'type))
 */
static void documentation_structure_class_type(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_structure_class_t);
	MethodTypeDocumentation(&type, StructureClass, TYPE);
	/* method */
	MopArgumentMethodDocumentation(&pos, STRUCTURE_CLASS, TYPE);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_structure_class_type(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_structure_class_t);
	MethodTypeSetfDocumentation(&type, StructureClass, TYPE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STRUCTURE_CLASS, TYPE);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (symbol (eql 'type))
 */
static int method_documentation_symbol_type(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	addr clos, pos;

	/* clos object */
	clos_find_class_nil(object, &clos);
	if (clos != Nil) {
		GetConst(COMMON_DOCUMENTATION, &pos);
		getfunctioncheck_local(ptr, pos, &pos);
		return funcall_control(ptr, pos, clos, doc_type, NULL);
	}

	/* deftype */
	getdeftype(object, &pos);
	if (object == Nil) {
		_fmte("The symbol ~S don't have a deftype function.", object, NULL);
		return 0;
	}
	getdocumentation_function(pos, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int method_setf_documentation_symbol_type(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	addr clos, pos;

	/* clos object */
	clos_find_class_nil(object, &clos);
	if (clos != Nil) {
		GetConst(COMMON_DOCUMENTATION, &pos);
		getsetfcheck_local(ptr, pos, &pos);
		return funcall_control(ptr, pos, value, clos, doc_type, NULL);
	}

	/* deftype */
	getdeftype(object, &pos);
	if (pos == Nil) {
		_fmte("The symbol ~S don't have a deftype function.", object, NULL);
		return 0;
	}
	setdocumentation_function(pos, value);
	setresult_control(ptr, value);

	return 0;
}

static void documentation_symbol_type(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_type);
	MethodTypeDocumentation(&type, Symbol, TYPE);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, TYPE);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_symbol_type(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_type);
	MethodTypeSetfDocumentation(&type, Symbol, TYPE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, TYPE);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (symbol (eql 'structure))
 */
static int method_documentation_symbol_structure(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	clos_find_class(object, &object);
	if (! structure_class_p(object))
		TypeError(object, STRUCTURE_CLASS);
	stdget_structure_documentation(object, &object);
	setresult_control(ptr, object);

	return 0;
}

static int method_setf_documentation_symbol_structure(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	clos_find_class(object, &object);
	if (! structure_class_p(object))
		TypeError(object, STRUCTURE_CLASS);
	stdset_structure_documentation(object, value);
	setresult_control(ptr, value);

	return 0;
}

static void documentation_symbol_structure(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_structure);
	MethodTypeDocumentation(&type, Symbol, STRUCTURE);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, STRUCTURE);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_symbol_structure(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_structure);
	MethodTypeSetfDocumentation(&type, Symbol, STRUCTURE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, STRUCTURE);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (symbol (eql 'variable))
 */
static int method_documentation_symbol_variable(Execute ptr,
		addr method, addr next, addr object, addr doc_type)
{
	getdocument_variable_symbol(object, &object);
	setresult_control(ptr, object);
	return 0;
}

static int method_setf_documentation_symbol_variable(Execute ptr,
		addr method, addr next, addr value, addr object, addr doc_type)
{
	setdocument_variable_symbol(object, value);
	setresult_control(ptr, value);
	return 0;
}

static void documentation_symbol_variable(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_variable);
	MethodTypeDocumentation(&type, Symbol, VARIABLE);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, VARIABLE);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

static void setf_documentation_symbol_variable(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_variable);
	MethodTypeSetfDocumentation(&type, Symbol, VARIABLE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, VARIABLE);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/*
 *  (defgeneric documentation (object doc-type)
 *      (:argument-precedence-order doc-type object)) -> documentation
 */
static void defun_documentation(Execute ptr)
{
	addr symbol, name, gen, var, order;

	GetConst(COMMON_DOCUMENTATION, &symbol);
	defun_documentation_order(&gen, &var, &order);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	generic_common_order(gen, var, order);
	SetFunctionSymbol(symbol, gen);
	/* method */
	documentation_function_t(ptr, name, gen);
	documentation_function_function(ptr, name, gen);
	documentation_list_function(ptr, name, gen);
	documentation_list_compiled_function(ptr, name, gen);
	documentation_symbol_function(ptr, name, gen);
	documentation_symbol_compiled_function(ptr, name, gen);
	documentation_symbol_setf(ptr, name, gen);
	documentation_method_combination_t(ptr, name, gen);
	documentation_method_combination_method_combination(ptr, name, gen);
	documentation_symbol_method_combination(ptr, name, gen);
	documentation_standard_method_t(ptr, name, gen);
	documentation_package_t(ptr, name, gen);
	documentation_standard_class_t(ptr, name, gen);
	documentation_standard_class_type(ptr, name, gen);
	documentation_structure_class_t(ptr, name, gen);
	documentation_structure_class_type(ptr, name, gen);
	documentation_symbol_type(ptr, name, gen);
	documentation_symbol_structure(ptr, name, gen);
	documentation_symbol_variable(ptr, name, gen);
	common_method_finalize(gen);
}


/*
 *  (defgeneric (setf documentation) (value object doc-type) ...)
 *      (:argument-precedence-order doc-type object value)) -> value
 */
static void defun_setf_documentation(Execute ptr)
{
	addr symbol, name, gen, var, order;

	GetConst(COMMON_DOCUMENTATION, &symbol);
	defun_setf_documentation_order(&gen, &var, &order);
	setf_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	generic_common_order(gen, var, order);
	setsetf_symbol(symbol, gen);
	/* method */
	setf_documentation_function_t(ptr, name, gen);
	setf_documentation_function_function(ptr, name, gen);
	setf_documentation_list_function(ptr, name, gen);
	setf_documentation_list_compiled_function(ptr, name, gen);
	setf_documentation_symbol_function(ptr, name, gen);
	setf_documentation_symbol_compiled_function(ptr, name, gen);
	setf_documentation_symbol_setf(ptr, name, gen);
	setf_documentation_method_combination_t(ptr, name, gen);
	setf_documentation_method_combination_method_combination(ptr, name, gen);
	setf_documentation_symbol_method_combination(ptr, name, gen);
	setf_documentation_standard_method_t(ptr, name, gen);
	setf_documentation_package_t(ptr, name, gen);
	setf_documentation_standard_class_t(ptr, name, gen);
	setf_documentation_standard_class_type(ptr, name, gen);
	setf_documentation_structure_class_t(ptr, name, gen);
	setf_documentation_structure_class_type(ptr, name, gen);
	setf_documentation_symbol_type(ptr, name, gen);
	setf_documentation_symbol_structure(ptr, name, gen);
	setf_documentation_symbol_variable(ptr, name, gen);
	common_method_finalize(gen);
}


/*
 *  build
 */
_g void init_documentation(void)
{
	SetPointerType(var4, method_documentation_function_t);
	SetPointerType(var4, method_documentation_list_function);
	SetPointerType(var4, method_documentation_list_compiled_function);
	SetPointerType(var4, method_documentation_symbol_function);
	SetPointerType(var4, method_documentation_symbol_compiled_function);
	SetPointerType(var4, method_documentation_symbol_setf);
	SetPointerType(var4, method_documentation_method_combination_t);
	SetPointerType(var4, method_documentation_symbol_method_combination);
	SetPointerType(var4, method_documentation_standard_method_t);
	SetPointerType(var4, method_documentation_package_t);
	SetPointerType(var4, method_documentation_standard_class_t);
	SetPointerType(var4, method_documentation_structure_class_t);
	SetPointerType(var4, method_documentation_symbol_type);
	SetPointerType(var4, method_documentation_symbol_structure);
	SetPointerType(var4, method_documentation_symbol_variable);

	SetPointerType(var5, method_setf_documentation_function_t);
	SetPointerType(var5, method_setf_documentation_list_function);
	SetPointerType(var5, method_setf_documentation_list_compiled_function);
	SetPointerType(var5, method_setf_documentation_symbol_function);
	SetPointerType(var5, method_setf_documentation_symbol_compiled_function);
	SetPointerType(var5, method_setf_documentation_symbol_setf);
	SetPointerType(var5, method_setf_documentation_method_combination_t);
	SetPointerType(var5, method_setf_documentation_symbol_method_combination);
	SetPointerType(var5, method_setf_documentation_standard_method_t);
	SetPointerType(var5, method_setf_documentation_package_t);
	SetPointerType(var5, method_setf_documentation_standard_class_t);
	SetPointerType(var5, method_setf_documentation_structure_class_t);
	SetPointerType(var5, method_setf_documentation_symbol_type);
	SetPointerType(var5, method_setf_documentation_symbol_structure);
	SetPointerType(var5, method_setf_documentation_symbol_variable);
}

_g void build_documentation(Execute ptr)
{
	defun_documentation(ptr);
	defun_setf_documentation(ptr);
}

