#include "callname.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
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
static int method_class_name(Execute ptr, addr method, addr next, addr var)
{
	stdget_class_name(var, &var);
	setresult_control(ptr, var);
	return 0;
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

static int defmethod_class_name_(Execute ptr, addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_class_name);
	method_type_class_name(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric class-name (class)) -> symbol */
static int defgeneric_class_name_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_CLASS_NAME, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_name_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS));
	Return(defmethod_class_name_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_name_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_name_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_name_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  (setf class-name)
 ***********************************************************************/
/* (defmethod (setf class-name) (t class) ...) -> t */
static int method_setf_class_name(Execute ptr,
		addr method, addr next, addr symbol, addr pos)
{
	stdset_class_name(pos, symbol);
	setresult_control(ptr, symbol);
	return 0;
}

static void method_type_setf_class_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Symbol);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void method_argument_setf_class_name(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	ArgumentMethod_var(&type1, SYMBOL);
	ArgumentMethod_var(&type2, STANDARD_CLASS);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_setf_class_name_class_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_setf_class_name);
	method_type_setf_class_name(&type);
	settype_function(call, type);
	/* method */
	method_argument_setf_class_name(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric (setf class-name) (t class)) -> t */
static int defgeneric_setf_class_name_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_CLASS_NAME, &symbol);
	mop_argument_generic_var2(&gen);
	setf_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	setsetf_symbol(symbol, gen);
	/* method */
	Return(defmethod_setf_class_name_class_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  class-slots
 ***********************************************************************/
static int make_slot_definition_call(Execute ptr)
{
	addr value;

	getdata_control(ptr, &value);
	setresult_control(ptr, value);

	return 0;
}

static void make_slot_definition_function(addr value, addr *ret)
{
	addr pos;

	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_make_slot_definition_call);
	SetDataFunction(pos, value);
	*ret = pos;
}

static int make_slot_definition_(addr slot, addr *ret)
{
	addr clos, key, value, check;

	GetConst(CLOS_STANDARD_SLOT_DEFINITION, &clos);
	clos_instance_heap(clos, &clos);
	/* slot-definition-name */
	GetNameSlot(slot, &value);
	GetConst(CLOSNAME_NAME, &key);
	clos_set(clos, key, value);
	/* slot-definition-type */
	GetTypeSlot(slot, &value);
	if (GetType(value) == LISPTYPE_TYPE) {
		Return(type_object_(&value, value));
	}
	GetConst(CLOSNAME_TYPE, &key);
	clos_set(clos, key, value);
	/* slot-definition-allocation */
	if (slot_instance_p(slot))
		GetConst(KEYWORD_INSTANCE, &value);
	else
		GetConst(KEYWORD_CLASS, &value);
	GetConst(CLOSNAME_ALLOCATION, &key);
	clos_set(clos, key, value);
	/* slot-definition-initargs */
	GetArgsSlot(slot, &value);
	GetConst(CLOSNAME_INITARGS, &key);
	clos_set(clos, key, value);
	/* slot-definition-initform */
	GetFormSlot(slot, &value);
	if (value != Unbound) {
		GetConst(CLOSNAME_INITFORM, &key);
		clos_set(clos, key, value);
		/* slot-definition-initfunction */
		GetFunctionSlot(slot, &check);
		if (check == Nil)
			make_slot_definition_function(value, &value);
		else
			value = check;
		GetConst(CLOSNAME_INITFUNCTION, &key);
		clos_set(clos, key, value);
	}
	/* result */
	return Result(ret, clos);
}

static int list_from_slot_vector_(addr pos, addr *ret)
{
	addr root, slot;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	LenSlotVector(pos, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(pos, i, &slot);
		Return(make_slot_definition_(slot, &slot));
		cons_heap(&root, slot, root);
	}
	nreverse(ret, root);

	return 0;
}

/* (defmethod class-slots (class) ...) -> t */
static int method_class_slots(Execute ptr, addr method, addr next, addr var)
{
	stdget_class_slots(var, &var);
	Return(list_from_slot_vector_(var, &var));
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_class_slots_(Execute ptr, addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_class_slots);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric class-slots (class)) -> t */
static int defgeneric_class_slots_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_SLOTS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_slots_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_slots_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_slots_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_slots_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  class-direct-slots
 ***********************************************************************/
/* (defmethod class-direct-slots (class) ...) -> t */
static int method_class_direct_slots(Execute ptr, addr method, addr next, addr var)
{
	stdget_class_direct_slots(var, &var);
	Return(list_from_slot_vector_(var, &var));
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_class_direct_slots_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_class_direct_slots);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric class-direct-slots (class)) -> t */
static int defgeneric_class_direct_slots_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_SLOTS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_direct_slots_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_direct_slots_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_direct_slots_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_direct_slots_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  class-default-initargs
 ***********************************************************************/
/* (defmethod class-default-initargs (class) ...) -> t */
static int method_class_default_initargs(Execute ptr, addr method, addr next, addr var)
{
	stdget_class_default_initargs(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_default_initargs_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_class_default_initargs);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric class-default-initargs (class)) -> t */
static int defgeneric_class_default_initargs_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DEFAULT_INITARGS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  class-direct-default-initargs
 ***********************************************************************/
/* (defmethod class-direct-default-initargs (class) ...) -> t */
static int method_class_direct_default_initargs(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_direct_default_initargs(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_direct_default_initargs_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_class_direct_default_initargs);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric class-direct-default-initargs (class)) -> t */
static int defgeneric_class_direct_default_initargs_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_DEFAULT_INITARGS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_direct_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_direct_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_direct_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_direct_default_initargs_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  class-precedence-list
 ***********************************************************************/
/* (defmethod class-precedence-list (class) ...) -> t */
static int method_class_precedence_list(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_precedence_list(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_precedence_list_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_class_precedence_list);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric class-precedence-list (class)) -> t */
static int defgeneric_class_precedence_list_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_PRECEDENCE_LIST, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_precedence_list_(ptr, name, gen,
				CONSTANT_CLOS_STRUCTURE_CLASS));
	Return(defmethod_class_precedence_list_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_precedence_list_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_precedence_list_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_precedence_list_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  class-direct-superclasses
 ***********************************************************************/
/* (defmethod class-direct-superclasses (class) ...) -> t */
static int method_class_direct_superclasses(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_direct_superclasses(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_direct_superclasses_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_class_direct_superclasses);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric class-direct-superclasses (class)) -> t */
static int defgeneric_class_direct_superclasses_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_SUPERCLASSES, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_direct_superclasses_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_direct_superclasses_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_direct_superclasses_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_direct_superclasses_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  class-direct-subclasses
 ***********************************************************************/
/* (defmethod class-direct-subclasses (class) ...) -> t */
static int method_class_direct_subclasses(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_direct_subclasses(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_direct_subclasses_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_class_direct_subclasses);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric class-direct-subclasses (class)) -> t */
static int defgeneric_class_direct_subclasses_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_DIRECT_SUBCLASSES, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_direct_subclasses_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_direct_subclasses_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_direct_subclasses_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_direct_subclasses_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  class-finalized-p
 ***********************************************************************/
/* (defmethod class-finalized-p (class) ...) -> t */
static int method_class_finalized_p(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_finalized_p(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_finalized_p_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_class_finalized_p);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric class-finalized-p (class)) -> t */
static int defgeneric_class_finalized_p_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_FINALIZED_P, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_finalized_p_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_finalized_p_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_finalized_p_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_finalized_p_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  class-prototype
 ***********************************************************************/
/* (defmethod class-prototype (class) ...) -> t */
static int method_class_prototype(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_class_prototype(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_class_prototype_(Execute ptr,
		addr name, addr gen, constindex index)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_class_prototype);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, index);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric class-prototype (class)) -> t */
static int defgeneric_class_prototype_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_CLASS_PROTOTYPE, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_class_prototype_(ptr, name, gen,
				CONSTANT_CLOS_STANDARD_CLASS));
	Return(defmethod_class_prototype_(ptr, name, gen,
				CONSTANT_CLOS_FUNCALLABLE_STANDARD_CLASS));
	Return(defmethod_class_prototype_(ptr, name, gen,
				CONSTANT_CLOS_FORWARD_REFERENCED_CLASS));
	Return(defmethod_class_prototype_(ptr, name, gen,
				CONSTANT_CLOS_BUILT_IN_CLASS));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  slot-definition-name
 ***********************************************************************/
/* (defmethod slot-definition-name
 *     ((inst standard-slot-definition)) ...) -> symbol
 */
static int method_slot_definition_name(Execute ptr, addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_NAME, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);

	return 0;
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

static int defmethod_slot_definition_name_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_slot_definition_name);
	method_type_slot_definition_name(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}


/* (defgeneric slot-definition-name (class)) -> symbol */
static int defgeneric_slot_definition_name_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_NAME, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_name_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  slot-definition-type
 ***********************************************************************/
/* (defmethod slot-definition-type
 *     ((inst standard-slot-definition)) ...) -> type
 */
static int method_slot_definition_type(Execute ptr, addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_TYPE, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_slot_definition_type_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_slot_definition_type);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}


/* (defgeneric slot-definition-type (class)) -> type */
static int defgeneric_slot_definition_type_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_TYPE, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_type_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  slot-definition-allocation
 ***********************************************************************/
/* (defmethod slot-definition-allocation
 *     ((inst standard-slot-definition)) ...) -> symbol
 */
static int method_slot_definition_allocation(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_ALLOCATION, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);

	return 0;
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

static int defmethod_slot_definition_allocation_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_slot_definition_allocation);
	method_type_slot_definition_allocation(&type);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}


/* (defgeneric slot-definition-allocation (class)) -> symbol */
static int defgeneric_slot_definition_allocation_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_ALLOCATION, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_allocation_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  slot-definition-initargs
 ***********************************************************************/
/* (defmethod slot-definition-initargs
 *     ((inst standard-slot-definition)) ...) -> t
 */
static int method_slot_definition_initargs(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_INITARGS, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_slot_definition_initargs_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_slot_definition_initargs);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}


/* (defgeneric slot-definition-initargs (class)) -> t */
static int defgeneric_slot_definition_initargs_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_INITARGS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_initargs_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  slot-definition-initform
 ***********************************************************************/
/* (defmethod slot-definition-initform
 *     ((inst standard-slot-definition)) ...) -> t
 */
static int method_slot_definition_initform(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_INITFORM, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_slot_definition_initform_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_slot_definition_initform);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}


/* (defgeneric slot-definition-initform (class)) -> t */
static int defgeneric_slot_definition_initform_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_INITFORM, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_initform_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  slot-definition-initfunction
 ***********************************************************************/
/* (defmethod slot-definition-initfunction
 *     ((inst standard-slot-definition)) ...) -> t
 */
static int method_slot_definition_initfunction(Execute ptr,
		addr method, addr next, addr var)
{
	addr key;

	GetConst(CLOSNAME_INITFUNCTION, &key);
	clos_check(var, key, &var);
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_slot_definition_initfunction_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_slot_definition_initfunction);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_SLOT_DEFINITION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}


/* (defgeneric slot-definition-initfunction (class)) -> t */
static int defgeneric_slot_definition_initfunction_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_SLOT_DEFINITION_INITFUNCTION, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_slot_definition_initfunction_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  generic-function-name
 ***********************************************************************/
/* (defmethod generic-function-name (clos) ...) -> symbol */
static int method_generic_function_name(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_generic_name(var, &var);
	if (callnamep(var))
		name_callname_heap(var, &var);
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_generic_function_name_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_generic_function_name);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric generic-function-name (clos)) -> symbol */
static int defgeneric_generic_function_name_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_NAME, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_name_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  (setf generic-function-name)
 ***********************************************************************/
/* (defmethod (setf generic-function-name) (t class) ...) -> t */
static int method_setf_generic_function_name(Execute ptr,
		addr method, addr next, addr var, addr clos)
{
	addr name;

	parse_callname_error(&name, var);
	stdset_generic_name(clos, name);
	setresult_control(ptr, var);

	return 0;
}

static void method_type_setf_generic_function_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, T);
	typeargs_var2(&args, args, values);
	typeargs_method(args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void method_argument_setf_generic_function_name(addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	ArgumentMethod_var(&type1, T);
	ArgumentMethod_var(&type2, STANDARD_GENERIC_FUNCTION);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int defmethod_setf_generic_function_name_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_setf_generic_function_name);
	method_type_setf_generic_function_name(&type);
	settype_function(call, type);
	/* method */
	method_argument_setf_generic_function_name(&pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric (setf generic-function-name) (t class)) -> t */
static int defgeneric_setf_generic_function_name_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_NAME, &symbol);
	mop_argument_generic_var2(&gen);
	setf_callname_heap(&name, symbol);
	generic_common_instance(&gen, name, gen);
	setsetf_symbol(symbol, gen);
	/* method */
	Return(defmethod_setf_generic_function_name_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  generic-function-methods
 ***********************************************************************/
/* (defmethod generic-function-methods (clos) ...) -> symbol */
static int method_generic_function_methods(Execute ptr,
		addr method, addr next, addr var)
{
	addr root, list, pos;
	size_t size, i;

	stdget_generic_methods(var, &var);
	lenarray(var, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		getarray(var, i, &list);
		while (list != Nil) {
			getcons(list, &pos, &list);
			cons_heap(&root, pos, root);
		}
	}
	nreverse(&root, root);
	setresult_control(ptr, root);

	return 0;
}

static int defmethod_generic_function_methods_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_generic_function_methods);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric generic-function-methods (clos)) -> symbol */
static int defgeneric_generic_function_methods_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_METHODS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_methods_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  generic-function-lambda-list
 ***********************************************************************/
static int method_generic_function_lambda_list(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_generic_lambda_list(var, &var);
	if (argumentp(var))
		argument_generic_lambda_heap(&var, var);
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_generic_function_lambda_list_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_generic_function_lambda_list);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric generic-function-lambda-list (clos)) -> symbol */
static int defgeneric_generic_function_lambda_list_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_LAMBDA_LIST, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_lambda_list_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  generic-function-argument-precedence-order
 ***********************************************************************/
static int method_generic_function_argument_precedence_order(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_generic_argument_precedence_order(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_generic_function_argument_precedence_order_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_generic_function_argument_precedence_order);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric generic-function-argument-precedence-order (clos)) -> symbol */
static int defgeneric_generic_function_argument_precedence_order_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_ARGUMENT_PRECEDENCE_ORDER, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_argument_precedence_order_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  generic-function-declarations
 ***********************************************************************/
static int method_generic_function_declarations(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_generic_declarations(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_generic_function_declarations_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_generic_function_declarations);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric generic-function-declarations (clos)) -> symbol */
static int defgeneric_generic_function_declarations_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_DECLARATIONS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_declarations_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  generic-function-method-class
 ***********************************************************************/
static int method_generic_function_method_class(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_generic_method_class(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_generic_function_method_class_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_generic_function_method_class);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric generic-function-method-class (clos)) -> symbol */
static int defgeneric_generic_function_method_class_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_METHOD_CLASS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_method_class_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  generic-function-method-combination
 ***********************************************************************/
static int method_generic_function_method_combination(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_generic_method_combination(var, &var);
	if (var == Nil)
		GetConst(CLOS_COMBINATION_STANDARD, &var);
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_generic_function_method_combination_(
		Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_generic_function_method_combination);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric generic-function-method-combination (clos)) -> symbol */
static int defgeneric_generic_function_method_combination_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_GENERIC_FUNCTION_METHOD_COMBINATION, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_generic_function_method_combination_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  method-function
 ***********************************************************************/
static int method_method_function(Execute ptr, addr method, addr next, addr var)
{
	stdget_method_function(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_method_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_method_function);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric method-function (clos)) -> symbol */
static int defgeneric_method_function_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_METHOD_FUNCTION, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_method_function_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  method-generic-function
 ***********************************************************************/
static int method_method_generic_function(Execute ptr,
		addr method, addr next, addr var)
{
	stdget_method_generic_function(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_method_generic_function_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_method_generic_function);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric method-generic-function (clos)) -> symbol */
static int defgeneric_method_generic_function_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_METHOD_GENERIC_FUNCTION, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_method_generic_function_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  method-lambda-list
 ***********************************************************************/
static int method_method_lambda_list(Execute ptr, addr method, addr next, addr var)
{
	stdget_method_lambda_list(var, &var);
	if (argumentp(var))
		argument_method_lambda_heap(&var, var);
	setresult_control(ptr, var);

	return 0;
}

static int defmethod_method_lambda_list_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_method_lambda_list);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric method-lambda-list (clos)) -> symbol */
static int defgeneric_method_lambda_list_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_METHOD_LAMBDA_LIST, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_method_lambda_list_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  method-specializers
 ***********************************************************************/
static int method_method_specializers(Execute ptr, addr method, addr next, addr var)
{
	stdget_method_specializers(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_method_specializers_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_method_specializers);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric method-specializers (clos)) -> symbol */
static int defgeneric_method_specializers_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(CLOSNAME_METHOD_SPECIALIZERS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_method_specializers_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  method-qualifiers
 ***********************************************************************/
static int method_method_qualifiers(Execute ptr, addr method, addr next, addr var)
{
	stdget_method_qualifiers(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static int defmethod_method_qualifiers_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_method_qualifiers);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

/* (defgeneric method-qualifiers (clos)) -> symbol */
static int defgeneric_method_qualifiers_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_METHOD_QUALIFIERS, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_method_qualifiers_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  accessor-method-slot-definition
 ***********************************************************************/
static int method_accessor_method_slot_definition(Execute ptr,
		addr method, addr next, addr var)
{
	return fmte_("There is no accessor-method in ~S.", NULL);
}

static int defmethod_accessor_method_slot_definition_(Execute ptr, addr name, addr gen)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	setcompiled_var3(call, p_method_accessor_method_slot_definition);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_var1(&pos, CONSTANT_CLOS_STANDARD_METHOD);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	stdset_method_function(pos, call);
	common_method_add(ptr, gen, pos);

	return 0;
}

static int defgeneric_accessor_method_slot_definition_(Execute ptr)
{
	addr symbol, name, gen;

	GetConst(COMMON_METHOD_QUALIFIERS, &symbol);
	GetConst(CLOSNAME_ACCESSOR_METHOD_SLOT_DEFINITION, &symbol);
	mop_argument_generic_var1(&gen);
	parse_callname_error(&name, symbol);
	generic_common_instance(&gen, name, gen);
	SetFunctionSymbol(symbol, gen);
	Return(mop_export_symbol_(symbol));
	/* method */
	Return(defmethod_accessor_method_slot_definition_(ptr, name, gen));
	common_method_finalize(gen);

	return 0;
}


/***********************************************************************
 *  function
 ***********************************************************************/
_g void init_mop_reader(void)
{
	/* Classes */
	SetPointerType(var3, method_class_name);
	SetPointerType(var4, method_setf_class_name);
	SetPointerType(empty, make_slot_definition_call);
	SetPointerType(var3, method_class_slots);
	SetPointerType(var3, method_class_direct_slots);
	SetPointerType(var3, method_class_default_initargs);
	SetPointerType(var3, method_class_direct_default_initargs);
	SetPointerType(var3, method_class_precedence_list);
	SetPointerType(var3, method_class_direct_superclasses);
	SetPointerType(var3, method_class_direct_subclasses);
	SetPointerType(var3, method_class_finalized_p);
	SetPointerType(var3, method_class_prototype);
	/* Slot definitions */
	SetPointerType(var3, method_slot_definition_name);
	SetPointerType(var3, method_slot_definition_type);
	SetPointerType(var3, method_slot_definition_allocation);
	SetPointerType(var3, method_slot_definition_initargs);
	SetPointerType(var3, method_slot_definition_initform);
	SetPointerType(var3, method_slot_definition_initfunction);
	/* Generic functions */
	SetPointerType(var3, method_generic_function_name);
	SetPointerType(var4, method_setf_generic_function_name);
	SetPointerType(var3, method_generic_function_methods);
	SetPointerType(var3, method_generic_function_lambda_list);
	SetPointerType(var3, method_generic_function_argument_precedence_order);
	SetPointerType(var3, method_generic_function_declarations);
	SetPointerType(var3, method_generic_function_method_class);
	SetPointerType(var3, method_generic_function_method_combination);
	/* Methods */
	SetPointerType(var3, method_method_function);
	SetPointerType(var3, method_method_generic_function);
	SetPointerType(var3, method_method_lambda_list);
	SetPointerType(var3, method_method_specializers);
	SetPointerType(var3, method_method_qualifiers);
	SetPointerType(var3, method_accessor_method_slot_definition);
}

_g int build_mop_reader_(Execute ptr)
{
	/* Classes */
	Return(defgeneric_class_name_(ptr));
	Return(defgeneric_setf_class_name_(ptr));
	Return(defgeneric_class_slots_(ptr));
	Return(defgeneric_class_direct_slots_(ptr));
	Return(defgeneric_class_default_initargs_(ptr));
	Return(defgeneric_class_direct_default_initargs_(ptr));
	Return(defgeneric_class_precedence_list_(ptr));
	Return(defgeneric_class_direct_superclasses_(ptr));
	Return(defgeneric_class_direct_subclasses_(ptr));
	Return(defgeneric_class_finalized_p_(ptr));
	Return(defgeneric_class_prototype_(ptr));
	/* Slot definitions */
	Return(defgeneric_slot_definition_name_(ptr));
	Return(defgeneric_slot_definition_type_(ptr));
	Return(defgeneric_slot_definition_allocation_(ptr));
	Return(defgeneric_slot_definition_initargs_(ptr));
	Return(defgeneric_slot_definition_initform_(ptr));
	Return(defgeneric_slot_definition_initfunction_(ptr));
	/* Generic functions */
	Return(defgeneric_generic_function_name_(ptr));
	Return(defgeneric_setf_generic_function_name_(ptr));
	Return(defgeneric_generic_function_methods_(ptr));
	Return(defgeneric_generic_function_lambda_list_(ptr));
	Return(defgeneric_generic_function_argument_precedence_order_(ptr));
	Return(defgeneric_generic_function_declarations_(ptr));
	Return(defgeneric_generic_function_method_class_(ptr));
	Return(defgeneric_generic_function_method_combination_(ptr));
	/* Methods */
	Return(defgeneric_method_function_(ptr));
	Return(defgeneric_method_generic_function_(ptr));
	Return(defgeneric_method_lambda_list_(ptr));
	Return(defgeneric_method_specializers_(ptr));
	Return(defgeneric_method_qualifiers_(ptr));
	Return(defgeneric_accessor_method_slot_definition_(ptr));

	return 0;
}

