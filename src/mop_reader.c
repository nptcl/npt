#include "clos.h"
#include "clos_class.h"
#include "clos_common.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "execute.h"
#include "function.h"
#include "lambda.h"
#include "mop.h"
#include "symbol.h"
#include "type_object.h"
#include "type_table.h"

/***********************************************************************
 *  class-name
 ***********************************************************************/
/* (defmethod class-name (class) ...) -> symbol */
static void method_class_name(Execute ptr, addr method, addr next, addr var)
{
	stdget_class_name(var, &var);
	setresult_control(ptr, var);
}

static void method_type_class_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Class);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defmethod_class_name(Execute ptr, addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_class_name);
	method_type_class_name(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

/* (defgeneric class-name (class)) -> symbol */
static void defgeneric_class_name(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_CLASS_NAME, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_class_name(ptr, name, gen, CONSTANT_CLOS_STANDARD_CLASS);
	defmethod_class_name(ptr, name, gen, CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS);
	defmethod_class_name(ptr, name, gen, CONSTANT_CLOS_FORWARD_REFERENCED_CLASS);
	defmethod_class_name(ptr, name, gen, CONSTANT_CLOS_BUILT_IN_CLASS);
	common_method_finalize(gen);
}


/***********************************************************************
 *  (setf class-name)
 ***********************************************************************/
/* (defmethod (setf class-name) (t class) ...) -> t */
static void defmethod_setf_class_name_class(Execute ptr, addr name, addr gen)
{
}


/* (defgeneric (setf class-name) (t class)) -> t */
static void defgeneric_setf_class_name(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_CLASS_NAME, &symbol);
	mop_argument_generic_var1(&gen);
	setf_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	setsetf_symbol(symbol, gen);
	/* method */
	defmethod_setf_class_name_class(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  class-slots
 ***********************************************************************/
static void make_slot_definition(addr slot, addr *ret)
{
	addr clos, key, value;

	GetConst(CLOS_STANDARD_SLOT_DEFINITION, &clos);
	clos_instance_heap(clos, &clos);
	/* slot-definition-name */
	GetNameSlot(slot, &value);
	GetConst(CLOSKEY_NAME, &key);
	clos_set(clos, key, value);
	/* slot-definition-type */
	GetTypeSlot(slot, &value);
	if (GetType(value) == LISPTYPE_TYPE)
		type_object(&value, value);
	GetConst(CLOSKEY_TYPE, &key);
	clos_set(clos, key, value);
	/* slot-definition-allocation */
	if (slot_instance_p(slot))
		GetConst(KEYWORD_INSTANCE, &value);
	else
		GetConst(KEYWORD_CLASS, &value);
	GetConst(CLOSKEY_ALLOCATION, &key);
	clos_set(clos, key, value);
	/* slot-definition-initargs */
	GetArgsSlot(slot, &value);
	GetConst(CLOSKEY_INITARGS, &key);
	clos_set(clos, key, value);
	/* slot-definition-initform */
	GetFormSlot(slot, &value);
	GetConst(CLOSKEY_INITFORM, &key);
	clos_set(clos, key, value);
	/* slot-definition-initfunction */
	GetFunctionSlot(slot, &value);
	GetConst(CLOSKEY_INITFUNCTION, &key);
	clos_set(clos, key, value);
	/* result */
	*ret = clos;
}

static void list_from_slot_vector(addr pos, addr *ret)
{
	addr root, slot;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	LenSlotVector(pos, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(pos, i, &slot);
		make_slot_definition(slot, &slot);
		cons_heap(&root, slot, root);
	}
	nreverse_list_unsafe(ret, root);
}

/* (defmethod class-slots (class) ...) -> t */
static void method_class_slots(Execute ptr, addr method, addr next, addr var)
{
	stdget_class_slots(var, &var);
	list_from_slot_vector(var, &var);
	setresult_control(ptr, var);
}

static void method_type_class_slots(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StandardClass);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defmethod_class_slots(Execute ptr, addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_class_slots);
	method_type_class_slots(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

/* (defgeneric class-slots (class)) -> t */
static void defgeneric_class_slots(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_SLOTS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_class_slots(ptr, name, gen, CONSTANT_CLOS_STANDARD_CLASS);
	defmethod_class_slots(ptr, name, gen, CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS);
	defmethod_class_slots(ptr, name, gen, CONSTANT_CLOS_FORWARD_REFERENCED_CLASS);
	defmethod_class_slots(ptr, name, gen, CONSTANT_CLOS_BUILT_IN_CLASS);
	common_method_finalize(gen);
}


/***********************************************************************
 *  class-direct-slots
 ***********************************************************************/
/* (defmethod class-direct-slots (class) ...) -> t */
static void method_class_direct_slots(Execute ptr, addr method, addr next, addr var)
{
	stdget_class_direct_slots(var, &var);
	list_from_slot_vector(var, &var);
	setresult_control(ptr, var);
}

static void defmethod_class_direct_slots(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_class_direct_slots);
	method_type_class_slots(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

/* (defgeneric class-direct-slots (class)) -> t */
static void defgeneric_class_direct_slots(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_SLOTS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_class_direct_slots(ptr, name, gen,
			CONSTANT_CLOS_STANDARD_CLASS);
	defmethod_class_direct_slots(ptr, name, gen,
			CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS);
	defmethod_class_direct_slots(ptr, name, gen,
			CONSTANT_CLOS_FORWARD_REFERENCED_CLASS);
	defmethod_class_direct_slots(ptr, name, gen,
			CONSTANT_CLOS_BUILT_IN_CLASS);
	common_method_finalize(gen);
}


/***********************************************************************
 *  class-default-initargs
 ***********************************************************************/
/* (defmethod class-default-initargs (class) ...) -> t */
static void method_class_default_initargs(Execute ptr, addr method, addr next, addr var)
{
	stdget_class_default_initargs(var, &var);
	setresult_control(ptr, var);
}

static void defmethod_class_default_initargs(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_class_default_initargs);
	method_type_class_slots(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

/* (defgeneric class-default-initargs (class)) -> t */
static void defgeneric_class_default_initargs(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DEFAULT_INITARGS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_class_default_initargs(ptr, name, gen,
			CONSTANT_CLOS_STANDARD_CLASS);
	defmethod_class_default_initargs(ptr, name, gen,
			CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS);
	defmethod_class_default_initargs(ptr, name, gen,
			CONSTANT_CLOS_FORWARD_REFERENCED_CLASS);
	defmethod_class_default_initargs(ptr, name, gen,
			CONSTANT_CLOS_BUILT_IN_CLASS);
	common_method_finalize(gen);
}


/***********************************************************************
 *  class-direct-default-initargs
 ***********************************************************************/
/* (defmethod class-direct-default-initargs (class) ...) -> t */
static void method_class_direct_default_initargs(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_direct_default_initargs(var, &var);
	setresult_control(ptr, var);
}

static void defmethod_class_direct_default_initargs(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_class_direct_default_initargs);
	method_type_class_slots(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

/* (defgeneric class-direct-default-initargs (class)) -> t */
static void defgeneric_class_direct_default_initargs(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_DEFAULT_INITARGS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_class_direct_default_initargs(ptr, name, gen,
			CONSTANT_CLOS_STANDARD_CLASS);
	defmethod_class_direct_default_initargs(ptr, name, gen,
			CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS);
	defmethod_class_direct_default_initargs(ptr, name, gen,
			CONSTANT_CLOS_FORWARD_REFERENCED_CLASS);
	defmethod_class_direct_default_initargs(ptr, name, gen,
			CONSTANT_CLOS_BUILT_IN_CLASS);
	common_method_finalize(gen);
}


/***********************************************************************
 *  class-precedence-list
 ***********************************************************************/
/* (defmethod class-precedence-list (class) ...) -> t */
static void method_class_precedence_list(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_precedence_list(var, &var);
	setresult_control(ptr, var);
}

static void defmethod_class_precedence_list(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_class_precedence_list);
	method_type_class_slots(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

/* (defgeneric class-precedence-list (class)) -> t */
static void defgeneric_class_precedence_list(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_PRECEDENCE_LIST, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_class_precedence_list(ptr, name, gen,
			CONSTANT_CLOS_STANDARD_CLASS);
	defmethod_class_precedence_list(ptr, name, gen,
			CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS);
	defmethod_class_precedence_list(ptr, name, gen,
			CONSTANT_CLOS_FORWARD_REFERENCED_CLASS);
	defmethod_class_precedence_list(ptr, name, gen,
			CONSTANT_CLOS_BUILT_IN_CLASS);
	common_method_finalize(gen);
}


/***********************************************************************
 *  class-direct-superclasses
 ***********************************************************************/
/* (defmethod class-direct-superclasses (class) ...) -> t */
static void method_class_direct_superclasses(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_direct_superclasses(var, &var);
	setresult_control(ptr, var);
}

static void defmethod_class_direct_superclasses(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_class_direct_superclasses);
	method_type_class_slots(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

/* (defgeneric class-direct-superclasses (class)) -> t */
static void defgeneric_class_direct_superclasses(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_SUPERCLASSES, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_class_direct_superclasses(ptr, name, gen,
			CONSTANT_CLOS_STANDARD_CLASS);
	defmethod_class_direct_superclasses(ptr, name, gen,
			CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS);
	defmethod_class_direct_superclasses(ptr, name, gen,
			CONSTANT_CLOS_FORWARD_REFERENCED_CLASS);
	defmethod_class_direct_superclasses(ptr, name, gen,
			CONSTANT_CLOS_BUILT_IN_CLASS);
	common_method_finalize(gen);
}


/***********************************************************************
 *  class-direct-subclasses
 ***********************************************************************/
/* (defmethod class-direct-subclasses (class) ...) -> t */
static void method_class_direct_subclasses(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_direct_subclasses(var, &var);
	setresult_control(ptr, var);
}

static void defmethod_class_direct_subclasses(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_class_direct_subclasses);
	method_type_class_slots(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

/* (defgeneric class-direct-subclasses (class)) -> t */
static void defgeneric_class_direct_subclasses(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_SUBCLASSES, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_class_direct_subclasses(ptr, name, gen,
			CONSTANT_CLOS_STANDARD_CLASS);
	defmethod_class_direct_subclasses(ptr, name, gen,
			CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS);
	defmethod_class_direct_subclasses(ptr, name, gen,
			CONSTANT_CLOS_FORWARD_REFERENCED_CLASS);
	defmethod_class_direct_subclasses(ptr, name, gen,
			CONSTANT_CLOS_BUILT_IN_CLASS);
	common_method_finalize(gen);
}


/***********************************************************************
 *  class-finalized-p
 ***********************************************************************/
/* (defmethod class-finalized-p (class) ...) -> t */
static void method_class_finalized_p(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_finalized_p(var, &var);
	setresult_control(ptr, var);
}

static void defmethod_class_finalized_p(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_class_finalized_p);
	method_type_class_slots(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

/* (defgeneric class-finalized-p (class)) -> t */
static void defgeneric_class_finalized_p(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_FINALIZED_P, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_class_finalized_p(ptr, name, gen,
			CONSTANT_CLOS_STANDARD_CLASS);
	defmethod_class_finalized_p(ptr, name, gen,
			CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS);
	defmethod_class_finalized_p(ptr, name, gen,
			CONSTANT_CLOS_FORWARD_REFERENCED_CLASS);
	defmethod_class_finalized_p(ptr, name, gen,
			CONSTANT_CLOS_BUILT_IN_CLASS);
	common_method_finalize(gen);
}


/***********************************************************************
 *  class-prototype
 ***********************************************************************/
/* (defmethod class-prototype (class) ...) -> t */
static void method_class_prototype(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_prototype(var, &var);
	setresult_control(ptr, var);
}

static void defmethod_class_prototype(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_class_prototype);
	method_type_class_slots(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}

/* (defgeneric class-prototype (class)) -> t */
static void defgeneric_class_prototype(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_PROTOTYPE, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_class_prototype(ptr, name, gen,
			CONSTANT_CLOS_STANDARD_CLASS);
	defmethod_class_prototype(ptr, name, gen,
			CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS);
	defmethod_class_prototype(ptr, name, gen,
			CONSTANT_CLOS_FORWARD_REFERENCED_CLASS);
	defmethod_class_prototype(ptr, name, gen,
			CONSTANT_CLOS_BUILT_IN_CLASS);
	common_method_finalize(gen);
}


/***********************************************************************
 *  slot-definition-name
 ***********************************************************************/
/* (defmethod slot-definition-name
 *     ((inst standard-slot-definition)) ...) -> symbol
 */
static void method_slot_definition_name(Execute ptr, addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSKEY_NAME, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);
}

static void method_type_slot_definition_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defmethod_slot_definition_name(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_slot_definition_name);
	method_type_slot_definition_name(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/* (defgeneric slot-definition-name (class)) -> symbol */
static void defgeneric_slot_definition_name(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_NAME, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_slot_definition_name(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  slot-definition-type
 ***********************************************************************/
/* (defmethod slot-definition-type
 *     ((inst standard-slot-definition)) ...) -> type
 */
static void method_slot_definition_type(Execute ptr, addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSKEY_TYPE, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);
}

static void method_type_slot_definition_type(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defmethod_slot_definition_type(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_slot_definition_type);
	method_type_slot_definition_type(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/* (defgeneric slot-definition-type (class)) -> type */
static void defgeneric_slot_definition_type(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_TYPE, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_slot_definition_type(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  slot-definition-allocation
 ***********************************************************************/
/* (defmethod slot-definition-allocation
 *     ((inst standard-slot-definition)) ...) -> symbol
 */
static void method_slot_definition_allocation(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSKEY_ALLOCATION, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);
}

static void method_type_slot_definition_allocation(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defmethod_slot_definition_allocation(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_slot_definition_allocation);
	method_type_slot_definition_allocation(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/* (defgeneric slot-definition-allocation (class)) -> symbol */
static void defgeneric_slot_definition_allocation(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_ALLOCATION, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_slot_definition_allocation(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  slot-definition-initargs
 ***********************************************************************/
/* (defmethod slot-definition-initargs
 *     ((inst standard-slot-definition)) ...) -> t
 */
static void method_slot_definition_initargs(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSKEY_INITARGS, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);
}

static void method_type_slot_definition_initargs(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defmethod_slot_definition_initargs(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_slot_definition_initargs);
	method_type_slot_definition_initargs(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/* (defgeneric slot-definition-initargs (class)) -> t */
static void defgeneric_slot_definition_initargs(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_INITARGS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_slot_definition_initargs(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  slot-definition-initform
 ***********************************************************************/
/* (defmethod slot-definition-initform
 *     ((inst standard-slot-definition)) ...) -> t
 */
static void method_slot_definition_initform(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSKEY_INITFORM, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);
}

static void method_type_slot_definition_initform(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defmethod_slot_definition_initform(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_slot_definition_initform);
	method_type_slot_definition_initform(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/* (defgeneric slot-definition-initform (class)) -> t */
static void defgeneric_slot_definition_initform(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_INITFORM, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_slot_definition_initform(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  slot-definition-initfunction
 ***********************************************************************/
/* (defmethod slot-definition-initfunction
 *     ((inst standard-slot-definition)) ...) -> t
 */
static void method_slot_definition_initfunction(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSKEY_INITFUNCTION, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);
}

static void method_type_slot_definition_initfunction(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_var1(&args, args);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defmethod_slot_definition_initfunction(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, method_slot_definition_initfunction);
	method_type_slot_definition_initfunction(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);
}


/* (defgeneric slot-definition-initfunction (class)) -> t */
static void defgeneric_slot_definition_initfunction(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_INITFUNCTION, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	/* method */
	defmethod_slot_definition_initfunction(ptr, name, gen);
	common_method_finalize(gen);
}


/***********************************************************************
 *  intern
 ***********************************************************************/
void intern_mop_reader(Execute ptr)
{
	/* Classes */
	defgeneric_class_name(ptr);
	defgeneric_setf_class_name(ptr);
	defgeneric_class_slots(ptr);
	defgeneric_class_direct_slots(ptr);
	defgeneric_class_default_initargs(ptr);
	defgeneric_class_direct_default_initargs(ptr);
	defgeneric_class_precedence_list(ptr);
	defgeneric_class_direct_superclasses(ptr);
	defgeneric_class_direct_subclasses(ptr);
	defgeneric_class_finalized_p(ptr);
	defgeneric_class_prototype(ptr);
	/* Slot-definitions */
	defgeneric_slot_definition_name(ptr);
	defgeneric_slot_definition_type(ptr);
	defgeneric_slot_definition_allocation(ptr);
	defgeneric_slot_definition_initargs(ptr);
	defgeneric_slot_definition_initform(ptr);
	defgeneric_slot_definition_initfunction(ptr);
}

