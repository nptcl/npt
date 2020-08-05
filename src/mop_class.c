/*
 *  ANSI COMMON LISP: 7. Objects
 *    Common Lisp Object System - Metaobject Protocol
 */
#include "callname.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "clos_make.h"
#include "clos_method.h"
#include "clos_redefine.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control_execute.h"
#include "control_operator.h"
#include "execute.h"
#include "function.h"
#include "lambda.h"
#include "mop.h"
#include "structure.h"
#include "symbol.h"
#include "type_table.h"

/***********************************************************************
 *  referenced-class
 ***********************************************************************/
/* (defun system::referenced-class (symbol) ...) -> class */
static int function_referenced_class(Execute ptr, addr symbol)
{
	addr pos;

	/* find-class */
	clos_find_class_nil(symbol, &pos);
	if (pos != Nil) {
		setresult_control(ptr, pos);
		return 0;
	}

	/* forward-referenced-class */
	GetConst(CLOS_FORWARD_REFERENCED_CLASS, &pos);
	Return(clos_instance_heap_(pos, &pos));
	Return(stdset_class_name_(pos, symbol));
	setresult_control(ptr, pos);

	return 0;
}

static void type_referenced_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static void defun_referenced_class_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_REFERENCED_CLASS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_referenced_class);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_referenced_class(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  ensure-class
 ***********************************************************************/
/* (defun system::ensure-class
 *     (name &rest args &key &allow-other-keys) ...)
 *     -> class
 *   name   symbol
 *   args   t
 *   class  class
 */
static int function_ensure_class(Execute ptr, addr name, addr rest)
{
	addr symbol, clos, check;

	/* class check */
	clos_find_class_nil(name, &clos);
	if (clos != Nil) {
		GetConst(COMMON_CLASS_NAME, &symbol);
		Return(callclang_funcall(ptr, &check, symbol, clos, NULL));
		if (check != name)
			clos = Nil;
	}

	/* call */
	GetConst(CLOSNAME_ENSURE_CLASS_USING_CLASS, &symbol);
	getfunction_global(symbol, &symbol);
	return applya_control(ptr, symbol, clos, name, rest, NULL);
}

static void type_ensure_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static void defun_ensure_class_mop(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(CLOSNAME_ENSURE_CLASS, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_ensure_class);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_ensure_class(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/***********************************************************************
 *  ensure-class-using-class
 ***********************************************************************/
/* (defmethod ensure-class-using-class
 *     ((inst null) name
 *      &key metaclass direct-superclasses &allow-other-keys) ...)
 *     -> class
 *   inst                 null
 *   name                 symbol
 *   metaclass            class
 *   direct-superclasses  list
 *   class                class
 */
static int method_ensure_class_using_class_null(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	Check(clos != Nil, "type error");
	Return(clos_ensure_class_(ptr, name, rest, &clos));
	setresult_control(ptr, clos);

	return 0;
}

static void method_type_ensure_class_using_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var2rest(&args, args, values, args);
	typeargs_method(args);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static void argument_method_ensure_class_using_class(addr *ret, constindex type)
{
	addr pos, key, key1, key2;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	str->var = 2;
	str->keyp = 1;
	str->key = 2;
	str->allow = 1;
	/* var */
	GetConstant(type, &key1);
	list_heap(&key1, Nil, key1, NULL);
	GetConst(CLOS_T, &key2);
	list_heap(&key2, Nil, key2, NULL);
	list_heap(&key, key1, key2, NULL);
	SetArgument(pos, ArgumentIndex_var, key);
	/* key */
	GetConst(CLOSKEY_METACLASS, &key1);
	conscar_heap(&key1, key1);
	GetConst(CLOSKEY_DIRECT_SUPERCLASSES, &key2);
	conscar_heap(&key2, key2);
	list_heap(&key, key1, key2, NULL);
	SetArgument(pos, ArgumentIndex_key, key);
	/* result */
	*ret = pos;
}

static int defmethod_ensure_class_using_class_null_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_ensure_class_using_class_null);
	method_type_ensure_class_using_class(&type);
	settype_function(call, type);
	/* method */
	argument_method_ensure_class_using_class(&pos, CONSTANT_CLOS_NULL);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defmethod ensure-class-using-class
 *     ((inst standard-class) name
 *      &key metaclass direct-superclasses direct-slots &allow-other-keys) ...)
 *     -> class
 *   inst                 class
 *   name                 symbol
 *   metaclass            class
 *   direct-superclasses  list
 *   direct-slots         list
 *   class                class
 */
static int method_ensure_class_using_class_class(Execute ptr,
		addr method, addr next, addr clos, addr name, addr rest)
{
	CheckType(clos, LISPTYPE_CLOS);
	Return(clos_ensure_class_redefine_(ptr, clos, name, rest));
	setresult_control(ptr, clos);

	return 0;
}

static int defmethod_ensure_class_using_class_class_(Execute ptr,
		addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_ensure_class_using_class_class);
	method_type_ensure_class_using_class(&type);
	settype_function(call, type);
	/* method */
	argument_method_ensure_class_using_class(&pos, CONSTANT_CLOS_CLASS);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric ensure-class-using-class
 *     (class name &rest initargs &key &allow-other-keys) ...)
 *   -> class
 */
static int defgeneric_ensure_class_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_ENSURE_CLASS_USING_CLASS, &symbol);
	mop_argument_generic_var2rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_ensure_class_using_class_null_(ptr, name, gen));
	Return(defmethod_ensure_class_using_class_class_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  allocate-instance
 ***********************************************************************/
/* (defmethod allocate-instance
 *     ((class standard-class) &rest args &key)) -> instance
 */
static int method_allocate_instance_stdclass(Execute ptr,
		addr method, addr next, addr clos, addr rest)
{
	Return(allocate_instance_stdclass_(ptr, clos, &clos));
	setresult_control(ptr, clos);
	return 0;
}

static void method_type_allocate_instance(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1rest(&args, args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void argument_method_allocate_instance(addr *ret, constindex type)
{
	addr pos, key;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	str->var = 1;
	str->keyp = 1;
	str->allow = 1;
	/* var */
	GetConstant(type, &key);
	list_heap(&key, Nil, key, NULL);
	list_heap(&key, key, NULL);
	SetArgument(pos, ArgumentIndex_var, key);
	/* result */
	*ret = pos;
}

static int defmethod_allocate_instance_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3dynamic(call, p_method_allocate_instance_stdclass);
	method_type_allocate_instance(&type);
	settype_function(call, type);
	/* method */
	argument_method_allocate_instance(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric allocate-instance (class &rest args &key)) -> instance */
static int defgeneric_allocate_instance_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_ALLOCATE_INSTANCE, &symbol);
	mop_argument_generic_var1rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_allocate_instance_(ptr, name, gen, CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_allocate_instance_(ptr, name, gen, CONSTANT_CLOS_STRUCTURE_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  initialize-instance
 ***********************************************************************/
/* (defmethod initialize-instance
 *     ((object standard-object) &rest args &key)) -> instance
 */
static int method_initialize_instance_stdobject(Execute ptr,
		addr method, addr next, addr pos, addr rest)
{
	Return(initialize_instance_stdobject_(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int defmethod_initialize_instance_stdobject_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3dynamic(call, p_method_initialize_instance_stdobject);
	method_type_allocate_instance(&type);
	settype_function(call, type);
	/* method */
	argument_method_allocate_instance(&pos, CONSTANT_CLOS_STANDARD_OBJECT);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric initialize-instance (class &rest args &key)) -> instance */
static int defgeneric_initialize_instance_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_INITIALIZE_INSTANCE, &symbol);
	mop_argument_generic_var1rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_initialize_instance_stdobject_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  reinitialize-instance
 ***********************************************************************/
/* (defmethod reinitialize-instance
 *     ((object standard-object) &rest args &key)) -> instance
 */
static int method_reinitialize_instance_stdobject(Execute ptr,
		addr method, addr next, addr pos, addr rest)
{
	Return(reinitialize_instance_stdobject_(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int defmethod_reinitialize_instance_stdobject_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3dynamic(call, p_method_reinitialize_instance_stdobject);
	method_type_allocate_instance(&type);
	settype_function(call, type);
	/* method */
	argument_method_allocate_instance(&pos, CONSTANT_CLOS_STANDARD_OBJECT);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric reinitialize-instance (class &rest args &key)) -> instance */
static int defgeneric_reinitialize_instance_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_REINITIALIZE_INSTANCE, &symbol);
	mop_argument_generic_var1rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_reinitialize_instance_stdobject_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  shared-initialize
 ***********************************************************************/
/* (defmethod shared-initialize
 *     ((object standard-object) name &rest args &key)) -> instance
 */
static int method_shared_initialize_stdobject(Execute ptr,
		addr method, addr next, addr pos, addr name, addr rest)
{
	Return(shared_initialize_stdobject_(ptr, pos, name, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void method_type_shared_initialize(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var2rest(&args, args, args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void argument_method_shared_initialize(addr *ret, constindex type)
{
	addr pos, key, key1, key2;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	str->var = 2;
	str->keyp = 1;
	str->allow = 1;
	/* var */
	GetConstant(type, &key1);
	list_heap(&key1, Nil, key1, NULL);
	GetConst(CLOS_T, &key2);
	list_heap(&key2, Nil, key2, NULL);
	list_heap(&key, key1, key2, NULL);
	SetArgument(pos, ArgumentIndex_var, key);
	/* result */
	*ret = pos;
}

static int defmethod_shared_initialize_stdobject_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_shared_initialize_stdobject);
	method_type_shared_initialize(&type);
	settype_function(call, type);
	/* method */
	argument_method_shared_initialize(&pos, CONSTANT_CLOS_STANDARD_OBJECT);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric shared-initialize (class &rest args &key)) -> instance */
static int defgeneric_shared_initialize_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_SHARED_INITIALIZE, &symbol);
	mop_argument_generic_var2rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_shared_initialize_stdobject_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  make-instance
 ***********************************************************************/
/* (defmethod make-instance
 *     ((class symbol) &rest initargs &key) ...)
 *   -> instance
 */
static int method_make_instance_symbol(Execute ptr,
		addr method, addr next, addr var, addr rest)
{
	addr symbol;

	Check(! symbolp(var), "type error");
	Return(clos_find_class_(var, &var));
	/* call generic-function */
	GetConst(COMMON_MAKE_INSTANCE, &symbol);
	getfunction_global(symbol, &symbol);
	return applya_control(ptr, symbol, var, rest, NULL);
}

static void type_make_instance_symbol(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_instance_symbol_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3dynamic(call, p_method_make_instance_symbol);
	type_make_instance_symbol(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1rest(&pos, SYMBOL);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defmethod make-instance
 *     ((class standard-class) &rest initargs &key) ...)
 *   -> instance
 */
static int method_make_instance_stdclass(Execute ptr,
		addr method, addr next, addr rest)
{
	Return(make_instance_stdclass_(ptr, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void method_type_make_instance_stdclass(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StandardClass);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_instance_stdclass_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, p_method_make_instance_stdclass);
	method_type_make_instance_stdclass(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1rest(&pos, STANDARD_CLASS);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defmethod make-instance
 *     ((class structure-class) &rest initargs &key) ...)
 *   -> instance
 */
static int method_make_instance_structure(Execute ptr,
		addr method, addr next, addr rest)
{
	Return(make_instance_structure(ptr, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
}

static void method_type_make_instance_structure(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StructureClass);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_instance_structure_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, p_method_make_instance_structure);
	method_type_make_instance_structure(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1rest(&pos, STRUCTURE_CLASS);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}


/* (defgeneric make-instance
 *      (class &rest initargs &key allow-other-keys) ...)
 *    -> instance
 */
static int defgeneric_make_instance_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_MAKE_INSTANCE, &symbol);
	mop_argument_generic_var1rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_make_instance_symbol_(ptr, name, gen));
	Return(defmethod_make_instance_stdclass_(ptr, name, gen));
	Return(defmethod_make_instance_structure_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  make-instances-obsolete
 ***********************************************************************/
/* (defmethod make-instances-obsolete ((var symbol)) ...) */
static int method_make_instances_obsolete_symbol(Execute ptr,
		addr method, addr next, addr var)
{
	addr call;

	GetConst(COMMON_MAKE_INSTANCES_OBSOLETE, &call);
	getfunction_global(call, &call);
	Return(clos_find_class_(var, &var));
	return funcall_control(ptr, call, var, NULL);
}

static void method_type_make_instances_obsolete_symbol(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_instances_obsolete_symbol_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_make_instances_obsolete_symbol);
	method_type_make_instances_obsolete_symbol(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1(&pos, SYMBOL);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

/* (defmethod make-instances-obsolete ((var standard-class)) ...) */
static int method_make_instances_obsolete_stdclass(Execute ptr,
		addr method, addr next, addr var)
{
	setresult_control(ptr, var);
	return 0;
}

static void method_type_make_instances_obsolete_stdclass(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StandardClass);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, Class);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_instances_obsolete_stdclass_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3(call, p_method_make_instances_obsolete_stdclass);
	method_type_make_instances_obsolete_stdclass(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1(&pos, STANDARD_CLASS);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_make_instances_obsolete_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_MAKE_INSTANCES_OBSOLETE, &symbol);
	mop_argument_generic_var1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_make_instances_obsolete_symbol_(ptr, name, gen));
	Return(defmethod_make_instances_obsolete_stdclass_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  make-load-form
 ***********************************************************************/
static int method_make_load_form_class(Execute ptr,
		addr method, addr next, addr var, addr env)
{
	addr call, find;

	/* (class-name var) */
	GetConst(COMMON_CLASS_NAME, &call);
	getfunction_global(call, &call);
	Return(callclang_funcall(ptr, &var, call, var, NULL));
	/* (find-class (quote var)) */
	GetConst(COMMON_FIND_CLASS, &find);
	quotelist_heap(&var, var);
	list_heap(&var, find, var, NULL);
	setresult_control(ptr, var);

	return 0;
}

static void method_type_make_load_form_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Environment);
	typeargs_var1opt1(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static int defmethod_make_load_form_class_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3opt1(call, p_method_make_load_form_class);
	method_type_make_load_form_class(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1opt1(&pos, CLASS, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defmethod_make_load_form_condition_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3opt1(call, p_method_make_load_form_class);
	method_type_make_load_form_class(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1opt1(&pos, CONDITION, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int method_make_load_form_object(Execute ptr,
		addr method, addr next, addr var, addr env)
{
	return fmte_("There is no function to make form ~S.", var, NULL);
}

static int defmethod_make_load_form_standard_object_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3opt1(call, p_method_make_load_form_object);
	method_type_make_load_form_class(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1opt1(&pos, STANDARD_OBJECT, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defmethod_make_load_form_structure_object_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var3opt1(call, p_method_make_load_form_object);
	method_type_make_load_form_class(&type);
	settype_function(call, type);
	/* method */
	ArgumentMethod_var1opt1(&pos, STRUCTURE_OBJECT, T);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_make_load_form_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_MAKE_LOAD_FORM, &symbol);
	mop_argument_generic_var1opt1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_make_load_form_class_(ptr, name, gen));
	Return(defmethod_make_load_form_condition_(ptr, name, gen));
	Return(defmethod_make_load_form_standard_object_(ptr, name, gen));
	Return(defmethod_make_load_form_structure_object_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-missing
 ***********************************************************************/
static int method_slot_missing(Execute ptr, addr method, addr next, addr rest)
{
	addr c, obj, name, op, value;
	lista_bind(rest, &c, &obj, &name, &op, &value, NULL);
	return fmte_("The class ~S has no slot ~S name ~S operation.", c, name, op, NULL);
}

static void method_type_slot_missing(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var4opt1(&args, args, args, values, values, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_slot_missing(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 4;
	str->opt = 1;
	ArgumentMethod_var(&type1, T);
	ArgumentMethod_var(&type2, SYMBOL);
	list_heap(&list, type1, type1, type2, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_slot_missing_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, p_method_slot_missing);
	method_type_slot_missing(&type);
	settype_function(call, type);
	/* method */
	method_argument_slot_missing(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static void mop_argument_generic_var4opt1(addr *ret)
{
	addr pos;
	struct argument_struct *str;

	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_generic;
	str->var = 4;
	str->opt = 1;
	*ret = pos;
}

static int defgeneric_slot_missing_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_SLOT_MISSING, &symbol);
	mop_argument_generic_var4opt1(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_missing_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-unbound
 ***********************************************************************/
static int method_slot_unbound(Execute ptr, addr method, addr next, addr rest)
{
	addr clos, obj, name;
	list_bind(rest, &clos, &obj, &name, NULL);
	return fmte_("The slot ~S is unbound in the ~S.", name, obj, NULL);
}

static void method_type_slot_unbound(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var3(&args, args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_slot_unbound(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 3;
	ArgumentMethod_var(&type1, T);
	ArgumentMethod_var(&type2, SYMBOL);
	list_heap(&list, type1, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_slot_unbound_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, p_method_slot_unbound);
	method_type_slot_unbound(&type);
	settype_function(call, type);
	/* method */
	method_argument_slot_unbound(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_slot_unbound_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_SLOT_UNBOUND, &symbol);
	mop_argument_generic_var3(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_unbound_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  update-instance-for-different-class
 ***********************************************************************/
static int method_update_instance_for_different_class(
		Execute ptr, addr method, addr next,
		addr previous, addr current, addr rest)
{
	Return(clos_change_method_(ptr, previous, current, rest));
	setresult_control(ptr, Nil);
	return 0;
}

static void method_type_update_instance_for_different_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var2rest(&args, args, args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_update_instance_for_different_class(addr *ret)
{
	mop_argument_method_var2rest(ret,
			CONSTANT_CLOS_STANDARD_OBJECT,
			CONSTANT_CLOS_STANDARD_OBJECT);
}

static int defmethod_update_instance_for_different_class_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_update_instance_for_different_class);
	method_type_update_instance_for_different_class(&type);
	settype_function(call, type);
	/* method */
	method_argument_update_instance_for_different_class(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_update_instance_for_different_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_UPDATE_INSTANCE_FOR_DIFFERENT_CLASS, &symbol);
	mop_argument_generic_var2rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_update_instance_for_different_class_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  update-instance-for-redefined-class
 ***********************************************************************/
static int method_update_instance_for_redefined_class(
		Execute ptr, addr method, addr next, addr rest)
{
	addr pos, add, del, prop, args;

	lista_bind(rest, &pos, &add, &del, &prop, &args, NULL);
	Return(clos_redefine_method_(ptr, pos, add, del, prop, args));
	setresult_control(ptr, Nil);

	return 0;
}

static void method_type_update_instance_for_redefined_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var2rest(&args, args, args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_update_instance_for_redefined_class(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 4;
	ArgumentMethod_var(&type1, STANDARD_OBJECT);
	ArgumentMethod_var(&type2, T);
	list_heap(&list, type1, type2, type2, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* rest */
	str->rest = 1;
	/* result */
	*ret = pos;
}

static int defmethod_update_instance_for_redefined_class_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, p_method_update_instance_for_redefined_class);
	method_type_update_instance_for_redefined_class(&type);
	settype_function(call, type);
	/* method */
	method_argument_update_instance_for_redefined_class(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_update_instance_for_redefined_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_UPDATE_INSTANCE_FOR_REDEFINED_CLASS, &symbol);
	mop_argument_generic_var4rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_update_instance_for_redefined_class_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-boundp-using-class
 ***********************************************************************/
static int method_slot_boundp_using_class(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	int check;

	Return(slot_boundp_using_class_common_(ptr, clos, pos, name, &check));
	setbool_control(ptr, check);

	return 0;
}

static void method_argument_slot_boundp_using_class(addr *ret, constindex index)
{
	addr pos, list, type1, type2, type3;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 3;
	mop_argument_method_var(&type1, index);
	ArgumentMethod_var(&type2, T);
	ArgumentMethod_var(&type3, SYMBOL);
	list_heap(&list, type1, type2, type3, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_slot_boundp_using_class_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var5(call, p_method_slot_boundp_using_class);
	GetTypeCompiled(&type, SlotBoundp_Method);
	settype_function(call, type);
	/* method */
	method_argument_slot_boundp_using_class(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_slot_boundp_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_BOUNDP_USING_CLASS, &symbol);
	mop_argument_generic_var3(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_boundp_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_slot_boundp_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-exists-p-using-class
 ***********************************************************************/
static int method_slot_exists_p_using_class(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	setbool_control(ptr, clos_slot_exists_p(pos, name));
	return 0;
}

static int defmethod_slot_exists_p_using_class_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var5(call, p_method_slot_exists_p_using_class);
	GetTypeCompiled(&type, SlotBoundp_Method);
	settype_function(call, type);
	/* method */
	method_argument_slot_boundp_using_class(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_slot_exists_p_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_EXISTS_P_USING_CLASS, &symbol);
	mop_argument_generic_var3(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_exists_p_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_slot_exists_p_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-makunbound-using-class
 ***********************************************************************/
static int method_slot_makunbound_using_class(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	Return(slot_makunbound_using_class_(ptr, clos, pos, name));
	setresult_control(ptr, pos);
	return 0;
}

static int defmethod_slot_makunbound_using_class_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var5(call, p_method_slot_makunbound_using_class);
	GetTypeCompiled(&type, SlotBoundp_Method);
	settype_function(call, type);
	/* method */
	method_argument_slot_boundp_using_class(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_slot_makunbound_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_MAKUNBOUND_USING_CLASS, &symbol);
	mop_argument_generic_var3(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_makunbound_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_slot_makunbound_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  slot-value-using-class
 ***********************************************************************/
static int method_slot_value_using_class(Execute ptr,
		addr method, addr next, addr clos, addr pos, addr name)
{
	Return(slot_value_using_class_common_(ptr, clos, pos, name, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int defmethod_slot_value_using_class_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var5(call, p_method_slot_value_using_class);
	GetTypeCompiled(&type, SlotBoundp_Method);
	settype_function(call, type);
	/* method */
	method_argument_slot_boundp_using_class(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_slot_value_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_VALUE_USING_CLASS, &symbol);
	mop_argument_generic_var3(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_slot_value_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_slot_value_using_class_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  (setf slot-value-using-class)
 ***********************************************************************/
static int method_setf_slot_value_using_class(Execute ptr,
		addr method, addr next, addr rest)
{
	addr value, clos, pos, name;

	list_bind(rest, &value, &clos, &pos, &name, NULL);
	Return(setf_slot_value_using_class_common_(ptr, clos, pos, name, value));
	setresult_control(ptr, value);

	return 0;
}

static void method_type_setf_slot_value_using_class(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var4(&args, args, args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_setf_slot_value_using_class(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 4;
	ArgumentMethod_var(&type1, T);
	ArgumentMethod_var(&type2, SYMBOL);
	list_heap(&list, type1, type1, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_setf_slot_value_using_class_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var2dynamic(call, p_method_setf_slot_value_using_class);
	method_type_setf_slot_value_using_class(&type);
	settype_function(call, type);
	/* method */
	method_argument_setf_slot_value_using_class(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_setf_slot_value_using_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_VALUE_USING_CLASS, &symbol);
	mop_argument_generic_var4(&gen);
	setf_callname_heap(&name, symbol);
	Return(generic_common_instance_(&gen, name, gen));
	setsetf_symbol(symbol, gen);
	/* method */
	Return(defmethod_setf_slot_value_using_class_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  change-class
 ***********************************************************************/
static int method_change_class_stdclass(Execute ptr,
		addr method, addr next, addr pos, addr clos, addr rest)
{
	Return(clos_change_class_(ptr, pos, clos, rest));
	setresult_control(ptr, pos);
	return 0;
}

static void method_type_change_class_stdclass(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StandardObject);
	GetTypeTable(&values, StandardClass);
	typeargs_var2rest(&args, args, values, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_change_class_stdclass(addr *ret)
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
	ArgumentMethod_var(&type1, STANDARD_OBJECT);
	ArgumentMethod_var(&type2, STANDARD_CLASS);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_change_class_stdclass_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_change_class_stdclass);
	method_type_change_class_stdclass(&type);
	settype_function(call, type);
	/* method */
	method_argument_change_class_stdclass(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int method_change_class_symbol(Execute ptr,
		addr method, addr next, addr pos, addr clos, addr rest)
{
	addr call;

	GetConst(COMMON_CHANGE_CLASS, &call);
	getfunction_global(call, &call);
	Return(clos_find_class_(clos, &clos));
	return applya_control(ptr, call, pos, clos, rest, NULL);
}

static void method_type_change_class_symbol(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Symbol);
	typeargs_var2rest(&args, args, values, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_change_class_symbol(addr *ret)
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
	ArgumentMethod_var(&type1, T);
	ArgumentMethod_var(&type2, SYMBOL);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_change_class_symbol_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_system(&call, name);
	setcompiled_var4dynamic(call, p_method_change_class_symbol);
	method_type_change_class_symbol(&type);
	settype_function(call, type);
	/* method */
	method_argument_change_class_symbol(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return common_method_add_(ptr, gen, pos);
}

static int defgeneric_change_class_mop_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_CHANGE_CLASS, &symbol);
	mop_argument_generic_var2rest1key0(&gen);
	Return(parse_callname_error_(&name, symbol));
	Return(generic_common_instance_(&gen, name, gen));
	SetFunctionSymbol(symbol, gen);
	/* method */
	Return(defmethod_change_class_stdclass_(ptr, name, gen));
	Return(defmethod_change_class_symbol_(ptr, name, gen));
	return common_method_finalize_(gen);
}


/***********************************************************************
 *  function
 ***********************************************************************/
_g void init_mop_class(void)
{
	SetPointerCall(defun, var1, referenced_class);
	SetPointerCall(defun, var1dynamic, ensure_class);
	SetPointerType(var4dynamic, method_ensure_class_using_class_null);
	SetPointerType(var4dynamic, method_ensure_class_using_class_class);
	SetPointerType(var3dynamic, method_allocate_instance_stdclass);
	SetPointerType(var3dynamic, method_initialize_instance_stdobject);
	SetPointerType(var3dynamic, method_reinitialize_instance_stdobject);
	SetPointerType(var4dynamic, method_shared_initialize_stdobject);
	SetPointerType(var3dynamic, method_make_instance_symbol);
	SetPointerType(var2dynamic, method_make_instance_stdclass);
	SetPointerType(var2dynamic, method_make_instance_structure);
	SetPointerType(var3, method_make_instances_obsolete_symbol);
	SetPointerType(var3, method_make_instances_obsolete_stdclass);
	SetPointerType(var3opt1, method_make_load_form_class);
	SetPointerType(var3opt1, method_make_load_form_object);
	SetPointerType(var2dynamic, method_slot_missing);
	SetPointerType(var2dynamic, method_slot_unbound);
	SetPointerType(var4dynamic, method_update_instance_for_different_class);
	SetPointerType(var2dynamic, method_update_instance_for_redefined_class);
	SetPointerType(var5, method_slot_boundp_using_class);
	SetPointerType(var5, method_slot_exists_p_using_class);
	SetPointerType(var5, method_slot_makunbound_using_class);
	SetPointerType(var5, method_slot_value_using_class);
	SetPointerType(var2dynamic, method_setf_slot_value_using_class);
	SetPointerType(var4dynamic, method_change_class_stdclass);
	SetPointerType(var4dynamic, method_change_class_symbol);
}

_g int build_mop_class_(Execute ptr)
{
	defun_referenced_class_mop();
	defun_ensure_class_mop();
	Return(defgeneric_ensure_class_using_class_mop_(ptr));
	Return(defgeneric_allocate_instance_mop_(ptr));
	Return(defgeneric_initialize_instance_mop_(ptr));
	Return(defgeneric_reinitialize_instance_mop_(ptr));
	Return(defgeneric_shared_initialize_mop_(ptr));
	Return(defgeneric_make_instance_mop_(ptr));
	Return(defgeneric_make_instances_obsolete_mop_(ptr));
	Return(defgeneric_make_load_form_(ptr));
	Return(defgeneric_slot_missing_mop_(ptr));
	Return(defgeneric_slot_unbound_mop_(ptr));
	Return(defgeneric_update_instance_for_different_class_mop_(ptr));
	Return(defgeneric_update_instance_for_redefined_class_mop_(ptr));
	Return(defgeneric_slot_boundp_using_class_mop_(ptr));
	Return(defgeneric_slot_exists_p_using_class_mop_(ptr));
	Return(defgeneric_slot_makunbound_using_class_mop_(ptr));
	Return(defgeneric_slot_value_using_class_mop_(ptr));
	Return(defgeneric_setf_slot_value_using_class_mop_(ptr));
	Return(defgeneric_change_class_mop_(ptr));

	return 0;
}

