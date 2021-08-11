#include "callname.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_method.h"
#include "condition.h"
#include "cons_list.h"
#include "constant.h"
#include "control_object.h"
#include "control_execute.h"
#include "execute_values.h"
#include "function.h"
#include "mop.h"
#include "print_object.h"
#include "structure.h"
#include "structure_define.h"
#include "structure_make.h"
#include "structure_object.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  structure-reader-list
 */
static int function_structure_reader_list(Execute ptr, addr var)
{
	int check;
	addr type;
	size_t index;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_list_p(type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-list.", var, NULL);
	/* access */
	GetSlotStructureType(type, &type);
	GetAccessSlot(type, &index);
	getnth_unsafe(var, index, &var);
	setresult_control(ptr, var);

	return 0;
}

static void structure_type_slot_reader_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

void structure_slot_reader_list(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader_list);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_reader_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  structure-writer-list
 */
static int function_structure_writer_list(Execute ptr, addr value, addr var)
{
	int check;
	addr type;
	size_t index;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_list_p(type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-list.", var, NULL);
	/* access */
	GetSlotStructureType(type, &type);
	GetAccessSlot(type, &index);
	setnth_unsafe(var, index, value);
	setresult_control(ptr, value);

	return 0;
}

static void structure_type_slot_writer_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

void structure_slot_writer_list(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer_list);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_writer_list(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/*
 *  structure-reader-vector
 */
static int function_structure_reader_vector(Execute ptr, addr var)
{
	int check;
	addr type, slot, pos;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_vector_p(ptr, type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-vector.", var, NULL);
	/* access */
	GetSlotStructureType(type, &slot);
	GetVectorStructureType(type, &pos);
	Return(structure_getarray_(ptr, var, slot, pos, &var));
	setresult_control(ptr, var);

	return 0;
}

static void structure_type_slot_reader_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

void structure_slot_reader_vector(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader_vector);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_reader_vector(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  structure-write-vector
 */
static int function_structure_writer_vector(Execute ptr, addr value, addr var)
{
	int check;
	addr type, slot;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_vector_p(ptr, type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-vector.", var, NULL);
	/* access */
	GetSlotStructureType(type, &slot);
	GetVectorStructureType(type, &type);
	Return(structure_setarray_(ptr, var, slot, type, value));
	setresult_control(ptr, value);

	return 0;
}

static void structure_type_slot_writer_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

void structure_slot_writer_vector(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer_vector);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_writer_vector(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/*
 *  structure-reader-clos
 */
static int function_structure_reader_clos(Execute ptr, addr var)
{
	int check;
	addr slot, pos;
	size_t index;

	/* closure */
	getdata_control(ptr, &slot);
	/* check-type */
	GetClassSlot(slot, &pos);
	Return(typep_structure_(var, pos, &check));
	if (! check)
		return fmte_("The structure ~S must be a ~S type.", pos, var, NULL);
	/* result */
	GetLocationSlot(slot, &index);
	GetValueClos(var, &var);
	GetClosValue(var, index, &var);
	setresult_control(ptr, var);

	return 0;
}

static void structure_type_slot_reader_clos(addr *ret, addr instance)
{
	addr args, values;

	type_clos_heap(instance, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

void structure_slot_reader_clos(addr instance, addr slot, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader_clos);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, slot);
	/* type */
	structure_type_slot_reader_clos(&type, instance);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  structure-writer-clos
 */
static int function_structure_writer_clos(Execute ptr, addr value, addr var)
{
	int check;
	addr slot, pos;
	size_t index;

	/* closure */
	getdata_control(ptr, &slot);
	/* check-type */
	GetClassSlot(slot, &pos);
	Return(typep_structure_(var, pos, &check));
	if (! check)
		return fmte_("The structure ~S must be a ~S type.", pos, var, NULL);
	/* result */
	GetLocationSlot(slot, &index);
	GetValueClos(var, &var);
	SetClosValue(var, index, value);
	setresult_control(ptr, value);

	return 0;
}

static void structure_type_slot_writer_clos(addr *ret, addr instance)
{
	addr args, values;

	GetTypeTable(&args, T);
	type_clos_heap(instance, &values);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

void structure_slot_writer_clos(addr instance, addr slot, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer_clos);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, slot);
	/* type */
	structure_type_slot_writer_clos(&type, instance);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/*
 *  structure-constructor-list
 */
static int function_structure_constructor_list(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure_list_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void structure_type_constructor_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

void structure_constructor_default_list(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor_list);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_constructor_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  structure-constructor-vector
 */
static int function_structure_constructor_vector(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure_vector_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void structure_type_constructor_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

void structure_constructor_default_vector(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor_vector);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_constructor_vector(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  structure-constructor-clos
 */
static int function_structure_constructor_clos(Execute ptr, addr args)
{
	addr pos;

	/* closure */
	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure_clos_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void structure_type_constructor_clos(addr *ret, addr data)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetInstanceStructureType(data, &data);
	type_clos_heap(data, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

void structure_constructor_default_clos(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor_clos);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_constructor_clos(&type, data);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  structure-copier-list
 */
static int function_structure_copier_list(Execute ptr, addr var)
{
	int check;
	addr type;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_list_p(type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-list.", var, NULL);
	/* copy */
	copy_list_heap_unsafe(&var, var);
	setresult_control(ptr, var);

	return 0;
}

static void structure_type_slot_copier_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

void structure_copier_list(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier_list);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	structure_type_slot_copier_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  structure-copier-vector
 */
static int function_structure_copier_vector(Execute ptr, addr var)
{
	int check;
	addr type;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_vector_p(ptr, type, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-vector.", var, NULL);
	/* copy */
	copy_vector_heap(&var, var);
	setresult_control(ptr, var);

	return 0;
}

static void structure_type_slot_copier_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

void structure_copier_vector(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier_vector);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	structure_type_slot_copier_vector(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  structure-copier-clos
 */
static int function_structure_copier_clos(Execute ptr, addr var)
{
	copy_structure_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void structure_type_slot_copier_clos(addr *ret, addr instance)
{
	addr args, values;

	type_clos_heap(instance, &values);
	typeargs_var1(&args, values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

void structure_copier_clos(addr instance, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier_clos);
	SetFunctionSymbol(symbol, pos);
	/* type */
	structure_type_slot_copier_clos(&type, instance);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  structure-predicate-list
 */
static int function_structure_predicate_list(Execute ptr, addr var)
{
	int check;
	addr type;

	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	Return(structure_type_list_p(type, var, &check));
	setbool_control(ptr, check);

	return 0;
}

void structure_predicate_list(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate_list);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  structure-predicate-vector
 */
static int function_structure_predicate_vector(Execute ptr, addr var)
{
	int check;
	addr type;

	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	Return(structure_type_vector_p(ptr, type, var, &check));
	setbool_control(ptr, check);

	return 0;
}

void structure_predicate_vector(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate_vector);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  structure-predicate-clos
 */
static int function_structure_predicate_clos(Execute ptr, addr var)
{
	int check;
	addr instance;

	getdata_control(ptr, &instance);
	Return(typep_structure_(var, instance, &check));
	setbool_control(ptr, check);

	return 0;
}

void structure_predicate_clos(addr instance, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate_clos);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, instance);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  print-object, default
 */
static int method_defstruct_default(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	int check;

	Return(structure_instance_p_(var, &check));
	if (! check)
		return fmte_("Invalid structure type ~S.", var, NULL);
	Return(print_structure(ptr, stream, var));
	setresult_control(ptr, var);

	return 0;
}

static int structure_print_default_method_(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	GetCallName(name, &name);
	setcompiled_var4(call, p_method_defstruct_default);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	Return(method_instance_lambda_(str->ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));

	return Result(ret, pos);
}

static int structure_print_add_method_(struct defstruct *str, addr name, addr method)
{
	addr generic;
	Execute ptr;

	ptr = str->ptr;
	Return(getglobalcheck_callname_(name, &generic));
	Check(! clos_generic_p_debug(generic), "type error");
	return method_add_method_(ptr, generic, method);
}

int structure_print_default_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_default_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}


/*
 *  print-object, object
 */
static int method_defstruct_object(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	int check;
	addr call;

	Return(structure_instance_p_(var, &check));
	if (! check)
		return fmte_("Invalid structure type ~S.", var, NULL);
	getdata_control(ptr, &call);
	Return(callclang_apply(ptr, &call, call, Nil));
	Return(callclang_funcall(ptr, &call, call, var, stream, NULL));
	setresult_control(ptr, var);

	return 0;
}

static int structure_print_object_method_(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	GetCallName(name, &name);
	setcompiled_var4(call, p_method_defstruct_object);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	SetDataFunction(call, str->print_object);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	Return(method_instance_lambda_(str->ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));

	return Result(ret, pos);
}

int structure_print_object_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_object_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}


/*
 *  print-object, function
 */
static int method_defstruct_function(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	int check;
	addr call, pos;

	Return(structure_instance_p_(var, &check));
	if (! check)
		return fmte_("Invalid structure type ~S.", var, NULL);
	GetConst(SPECIAL_PRINT_LEVEL, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	getdata_control(ptr, &call);
	Return(callclang_apply(ptr, &call, call, Nil));
	Return(callclang_funcall(ptr, &call, call, var, stream, pos, NULL));
	setresult_control(ptr, var);

	return 0;
}

static int structure_print_function_method_(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	GetCallName(name, &name);
	setcompiled_var4(call, p_method_defstruct_function);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	SetDataFunction(call, str->print_function);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	Return(method_instance_lambda_(str->ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));

	return Result(ret, pos);
}

int structure_print_function_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_function_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}


/*
 *  initialize
 */
void init_structure_define(void)
{
	SetPointerCall(defun, var1, structure_reader_list);
	SetPointerCall(defun, var1, structure_reader_vector);
	SetPointerCall(defun, var1, structure_reader_clos);
	SetPointerCall(defun, var2, structure_writer_list);
	SetPointerCall(defun, var2, structure_writer_vector);
	SetPointerCall(defun, var2, structure_writer_clos);
	SetPointerCall(defun, dynamic, structure_constructor_list);
	SetPointerCall(defun, dynamic, structure_constructor_vector);
	SetPointerCall(defun, dynamic, structure_constructor_clos);
	SetPointerCall(defun, var1, structure_copier_list);
	SetPointerCall(defun, var1, structure_copier_vector);
	SetPointerCall(defun, var1, structure_copier_clos);
	SetPointerCall(defun, var1, structure_predicate_list);
	SetPointerCall(defun, var1, structure_predicate_vector);
	SetPointerCall(defun, var1, structure_predicate_clos);
	SetPointerType(var4, method_defstruct_default);
	SetPointerType(var4, method_defstruct_object);
	SetPointerType(var4, method_defstruct_function);
}

