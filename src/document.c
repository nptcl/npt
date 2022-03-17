#include "callname.h"
#include "clos.h"
#include "clos_defgeneric.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "clos_type.h"
#include "cons.h"
#include "constant.h"
#include "document.h"
#include "document_call.h"
#include "execute_values.h"
#include "function.h"
#include "lambda.h"
#include "symbol.h"
#include "type_table.h"
#include "typedef.h"

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

static int mop_argument_method_documentation_(addr *ret, constindex a, constindex b)
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
	Return(clos_intern_specializer_(y, &y));
	list_heap(&pos2, x, y, NULL);
	/* var */
	str->var = 2;
	list_heap(&pos1, pos1, pos2, NULL);
	SetArgument(pos, ArgumentIndex_var, pos1);
	/* result */
	return Result(ret, pos);
}
#define MopArgumentMethodDocumentation(r,b,c) { \
	Return(mop_argument_method_documentation_((r), \
				CONSTANT_CLOS_##b, CONSTANT_COMMON_##c)) \
}

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

static int mop_argument_method_setf_documentation_(addr *ret,
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
	Return(clos_intern_specializer_(y, &y));
	list_heap(&pos3, x, y, NULL);
	/* var */
	str->var = 3;
	list_heap(&pos1, pos1, pos2, pos3, NULL);
	SetArgument(pos, ArgumentIndex_var, pos1);
	/* result */
	return Result(ret, pos);
}
#define MopArgumentMethodSetfDocumentation(r,b,c) { \
	Return(mop_argument_method_setf_documentation_((r), \
				CONSTANT_CLOS_##b, CONSTANT_COMMON_##c)); \
}


/*
 *  (function (eql 't))
 */
static int method_documentation_function_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_function_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_function_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_function_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_function_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_function_t);
	MethodTypeDocumentation(&type, Function, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, FUNCTION, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_function_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_function_t);
	MethodTypeSetfDocumentation(&type, Function, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, FUNCTION, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (function (eql 'function))
 */
static int documentation_function_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_function_t);
	MethodTypeDocumentation(&type, Function, FUNCTION);
	/* method */
	MopArgumentMethodDocumentation(&pos, FUNCTION, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_function_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_function_t);
	MethodTypeSetfDocumentation(&type, Function, FUNCTION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, FUNCTION, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (list (eql 'function))
 */
static int method_documentation_list_function(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_function_setf_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_list_function(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_function_setf_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_list_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_list_function);
	MethodTypeDocumentation(&type, List, FUNCTION);
	/* method */
	MopArgumentMethodDocumentation(&pos, LIST, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_list_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_list_function);
	MethodTypeSetfDocumentation(&type, List, FUNCTION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, LIST, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (list (eql 'compiler-macro))
 */
static int method_documentation_list_compiler_macro(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_compiler_macro_setf_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_list_compiler_macro(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_compiler_macro_setf_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_list_compiler_macro_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_list_compiler_macro);
	MethodTypeDocumentation(&type, List, COMPILER_MACRO);
	/* method */
	MopArgumentMethodDocumentation(&pos, LIST, COMPILER_MACRO);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_list_compiler_macro_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_list_compiler_macro);
	MethodTypeSetfDocumentation(&type, List, COMPILER_MACRO);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, LIST, COMPILER_MACRO);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'function))
 */
static int method_documentation_symbol_function(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_function_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_function(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_function_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_function);
	MethodTypeDocumentation(&type, Symbol, FUNCTION);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_function);
	MethodTypeSetfDocumentation(&type, Symbol, FUNCTION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'compiler-macro))
 */
static int method_documentation_symbol_compiler_macro(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_compiler_macro_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_compiler_macro(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_compiler_macro_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_compiler_macro_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_compiler_macro);
	MethodTypeDocumentation(&type, Symbol, COMPILER_MACRO);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, COMPILER_MACRO);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_compiler_macro_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_compiler_macro);
	MethodTypeSetfDocumentation(&type, Symbol, COMPILER_MACRO);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, COMPILER_MACRO);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'setf))
 */
static int method_documentation_symbol_setf(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_defsetf_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_setf(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_defsetf_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_setf_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_setf);
	MethodTypeDocumentation(&type, Symbol, SETF);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, SETF);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_setf_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_setf);
	MethodTypeSetfDocumentation(&type, Symbol, SETF);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, SETF);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (method-combination (eql 't))
 */
static int method_documentation_method_combination_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_method_combination_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_method_combination_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_method_combination_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_method_combination_t_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_method_combination_t);
	MethodTypeDocumentation(&type, MethodCombination, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, METHOD_COMBINATION, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_method_combination_t_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_method_combination_t);
	MethodTypeSetfDocumentation(&type, MethodCombination, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, METHOD_COMBINATION, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (method-combination (eql 'method-combination))
 */
static int documentation_method_combination_method_combination_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_method_combination_t);
	MethodTypeDocumentation(&type, MethodCombination, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodDocumentation(&pos, METHOD_COMBINATION, METHOD_COMBINATION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_method_combination_method_combination_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_method_combination_t);
	MethodTypeSetfDocumentation(&type, MethodCombination, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, METHOD_COMBINATION, METHOD_COMBINATION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'method-combination))
 */
static int method_documentation_symbol_method_combination(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_method_combination_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_method_combination(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_method_combination_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_method_combination_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_method_combination);
	MethodTypeDocumentation(&type, Symbol, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, METHOD_COMBINATION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_method_combination_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_method_combination);
	MethodTypeSetfDocumentation(&type, Symbol, METHOD_COMBINATION);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, METHOD_COMBINATION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (standard-method (eql 't))
 */
static int method_documentation_standard_method_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_standard_method_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_standard_method_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_standard_method_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_standard_method_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_standard_method_t);
	MethodTypeDocumentation(&type, StandardMethod, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, STANDARD_METHOD, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_standard_method_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_standard_method_t);
	MethodTypeSetfDocumentation(&type, StandardMethod, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STANDARD_METHOD, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (package (eql 't))
 */
static int method_documentation_package_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_package_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_package_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_package_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_package_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_package_t);
	MethodTypeDocumentation(&type, Package, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, PACKAGE, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_package_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_package_t);
	MethodTypeSetfDocumentation(&type, Package, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, PACKAGE, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (standard-class (eql 't))
 */
static int method_documentation_standard_class_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_standard_class_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_standard_class_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_standard_class_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_standard_class_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_standard_class_t);
	MethodTypeDocumentation(&type, StandardClass, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, STANDARD_CLASS, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_standard_class_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_standard_class_t);
	MethodTypeSetfDocumentation(&type, StandardClass, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STANDARD_CLASS, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (standard-class (eql 'type))
 */
static int documentation_standard_class_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_standard_class_t);
	MethodTypeDocumentation(&type, StandardClass, TYPE);
	/* method */
	MopArgumentMethodDocumentation(&pos, STANDARD_CLASS, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_standard_class_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_standard_class_t);
	MethodTypeSetfDocumentation(&type, StandardClass, TYPE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STANDARD_CLASS, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (structure-class (eql 't))
 */
static int method_documentation_structure_class_t(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_structure_class_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_structure_class_t(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_structure_class_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_structure_class_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_structure_class_t);
	MethodTypeDocumentation(&type, StructureClass, T);
	/* method */
	MopArgumentMethodDocumentation(&pos, STRUCTURE_CLASS, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_structure_class_t_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_structure_class_t);
	MethodTypeSetfDocumentation(&type, StructureClass, T);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STRUCTURE_CLASS, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (structure-class (eql 'type))
 */
static int documentation_structure_class_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_structure_class_t);
	MethodTypeDocumentation(&type, StructureClass, TYPE);
	/* method */
	MopArgumentMethodDocumentation(&pos, STRUCTURE_CLASS, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_structure_class_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_structure_class_t);
	MethodTypeSetfDocumentation(&type, StructureClass, TYPE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, STRUCTURE_CLASS, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'type))
 */
static int method_documentation_symbol_type(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_type_symbol_get_(ptr, pos, doc_type, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_type(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_type_symbol_set_(ptr, pos, doc_type, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_type);
	MethodTypeDocumentation(&type, Symbol, TYPE);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_type);
	MethodTypeSetfDocumentation(&type, Symbol, TYPE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, TYPE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'structure))
 */
static int method_documentation_symbol_structure(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_structure_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_structure(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_structure_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_structure_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_structure);
	MethodTypeDocumentation(&type, Symbol, STRUCTURE);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, STRUCTURE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_structure_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_structure);
	MethodTypeSetfDocumentation(&type, Symbol, STRUCTURE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, STRUCTURE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (symbol (eql 'variable))
 */
static int method_documentation_symbol_variable(Execute ptr,
		addr method, addr next, addr pos, addr doc_type)
{
	Return(document_variable_symbol_get_(pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int method_setf_documentation_symbol_variable(Execute ptr,
		addr method, addr next, addr value, addr pos, addr doc_type)
{
	Return(document_variable_symbol_set_(pos, value));
	setresult_control(ptr, value);
	return 0;
}

static int documentation_symbol_variable_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_documentation_symbol_variable);
	MethodTypeDocumentation(&type, Symbol, VARIABLE);
	/* method */
	MopArgumentMethodDocumentation(&pos, SYMBOL, VARIABLE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int setf_documentation_symbol_variable_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var5(call, p_method_setf_documentation_symbol_variable);
	MethodTypeSetfDocumentation(&type, Symbol, VARIABLE);
	/* method */
	MopArgumentMethodSetfDocumentation(&pos, SYMBOL, VARIABLE);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/*
 *  (defgeneric documentation (object doc-type)
 *      (:argument-precedence-order doc-type object)) -> documentation
 */
static int defun_documentation_(Execute ptr)
{
	addr symbol, name, gen, var, order;

	GetConst(COMMON_DOCUMENTATION, &symbol);
	defun_documentation_order(&gen, &var, &order);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_make_(&gen, name, gen));
	Return(generic_order_(gen, var, order));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(documentation_function_t_(ptr, name, gen));
	Return(documentation_function_function_(ptr, name, gen));
	Return(documentation_list_function_(ptr, name, gen));
	Return(documentation_list_compiler_macro_(ptr, name, gen));
	Return(documentation_symbol_function_(ptr, name, gen));
	Return(documentation_symbol_compiler_macro_(ptr, name, gen));
	Return(documentation_symbol_setf_(ptr, name, gen));
	Return(documentation_method_combination_t_(ptr, name, gen));
	Return(documentation_method_combination_method_combination_(ptr, name, gen));
	Return(documentation_symbol_method_combination_(ptr, name, gen));
	Return(documentation_standard_method_t_(ptr, name, gen));
	Return(documentation_package_t_(ptr, name, gen));
	Return(documentation_standard_class_t_(ptr, name, gen));
	Return(documentation_standard_class_type_(ptr, name, gen));
	Return(documentation_structure_class_t_(ptr, name, gen));
	Return(documentation_structure_class_type_(ptr, name, gen));
	Return(documentation_symbol_type_(ptr, name, gen));
	Return(documentation_symbol_structure_(ptr, name, gen));
	Return(documentation_symbol_variable_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/*
 *  (defgeneric (setf documentation) (value object doc-type) ...)
 *      (:argument-precedence-order doc-type object value)) -> value
 */
static int defun_setf_documentation_(Execute ptr)
{
	addr symbol, name, gen, var, order;

	GetConst(COMMON_DOCUMENTATION, &symbol);
	defun_setf_documentation_order(&gen, &var, &order);
	setf_callname_heap(&name, symbol);
	Return(generic_make_(&gen, name, gen));
	Return(generic_order_(gen, var, order));
	setsetf_symbol(symbol, gen);
	/* method */
	Return(setf_documentation_function_t_(ptr, name, gen));
	Return(setf_documentation_function_function_(ptr, name, gen));
	Return(setf_documentation_list_function_(ptr, name, gen));
	Return(setf_documentation_list_compiler_macro_(ptr, name, gen));
	Return(setf_documentation_symbol_function_(ptr, name, gen));
	Return(setf_documentation_symbol_compiler_macro_(ptr, name, gen));
	Return(setf_documentation_symbol_setf_(ptr, name, gen));
	Return(setf_documentation_method_combination_t_(ptr, name, gen));
	Return(setf_documentation_method_combination_method_combination_(ptr, name, gen));
	Return(setf_documentation_symbol_method_combination_(ptr, name, gen));
	Return(setf_documentation_standard_method_t_(ptr, name, gen));
	Return(setf_documentation_package_t_(ptr, name, gen));
	Return(setf_documentation_standard_class_t_(ptr, name, gen));
	Return(setf_documentation_standard_class_type_(ptr, name, gen));
	Return(setf_documentation_structure_class_t_(ptr, name, gen));
	Return(setf_documentation_structure_class_type_(ptr, name, gen));
	Return(setf_documentation_symbol_type_(ptr, name, gen));
	Return(setf_documentation_symbol_structure_(ptr, name, gen));
	Return(setf_documentation_symbol_variable_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/*
 *  build
 */
void init_documentation(void)
{
	SetPointerType(var4, method_documentation_function_t);
	SetPointerType(var4, method_documentation_list_function);
	SetPointerType(var4, method_documentation_list_compiler_macro);
	SetPointerType(var4, method_documentation_symbol_function);
	SetPointerType(var4, method_documentation_symbol_compiler_macro);
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
	SetPointerType(var5, method_setf_documentation_list_compiler_macro);
	SetPointerType(var5, method_setf_documentation_symbol_function);
	SetPointerType(var5, method_setf_documentation_symbol_compiler_macro);
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

void build_documentation(Execute ptr)
{
	Error(defun_documentation_(ptr));
	Error(defun_setf_documentation_(ptr));
}

