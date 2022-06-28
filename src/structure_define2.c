#include "array_vector.h"
#include "callname.h"
#include "condition.h"
#include "clos.h"
#include "clos_object.h"
#include "clos_slot.h"
#include "closget_slot.h"
#include "cons.h"
#include "cons_list.h"
#include "control_object.h"
#include "execute_values.h"
#include "function.h"
#include "package.h"
#include "package_intern.h"
#include "sequence.h"
#include "structure_define2.h"
#include "structure_direct.h"
#include "structure_make.h"
#include "structure_object.h"
#include "strtype.h"
#include "symbol.h"
#include "type_parse.h"
#include "type_table.h"
#include "type_typep.h"
#include "typedef.h"

/*
 *  make-instance
 */
static void structure_instance2_include(struct defstruct *str, addr instance)
{
	addr list, pos;

	/* include */
	if (str->include_p) {
		SetIncludeStructure(instance, str->iname);
	}

	/* precedence-list */
	list = Nil;
	for (pos = instance; pos != Nil; ) {
		cons_heap(&list, pos, list);
		GetIncludeStructure(pos, &pos);
	}
	nreverse(&list, list);
	SetPrecedenceStructure(instance, list);
}

void structure_instance2(struct defstruct *str)
{
	addr instance;

	/* structure */
	structure_heap(&instance);
	/* name */
	SetNameStructure(instance, str->name);
	/* documentation */
	if (str->doc != Nil) {
		SetDocStructure(instance, str->doc);
	}
	/* include, precedence-list */
	structure_instance2_include(str, instance);
	/* type */
	Check(! str->type_p, "type error");
	if (str->type_list_p) {
		set_list_p_structure(instance, 1);
	}
	if (str->type_vector_p) {
		set_vector_p_structure(instance, 1);
		settype_structure(instance, str->type1, str->type2);
		SetSpecializedStructure(instance, str->type_vector);
	}
	/* named */
	set_named_p_structure(instance, str->named_p);
	/* result */
	str->instance = instance;
}


/*
 *  slots
 */
static int structure_define2_slots_exists_p_(addr include, addr name, int *ret)
{
	int check;
	addr slots, pos;
	size_t size, i;

	if (include == Nil)
		return Result(ret, 0);

	GetSlotsStructure(include, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		getname_slot(pos, &pos);
		GetNameSymbol(pos, &pos);
		Return(string_equal_(name, pos, &check));
		if (check)
			return Result(ret, 1);
	}

	GetIncludeStructure(include, &include);
	return structure_define2_slots_exists_p_(include, name, ret);
}

static int structure_define2_slots_include_(struct defstruct *str)
{
	int check;
	addr include, list, pos, name;

	if (! str->include_p)
		return 0;
	list = str->iargs;
	include = str->iname;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		getname_slot(pos, &name);
		GetNameSymbol(name, &name);
		Return(structure_define2_slots_exists_p_(include, name, &check));
		if (! check)
			return fmte_("The slot ~S is not included in structure.", pos, NULL);
	}

	return 0;
}

static void structure_define2_slots_named(addr name, size_t size_all, addr *ret)
{
	addr pos, symbol, type;

	slot_heap(&pos);
	GetConst(SYSTEM_STRUCTURE_NAMED, &symbol);
	setname_slot(pos, symbol);
	setform_slot(pos, name);
	setlocation_slot(pos, size_all);
	setaccess_slot(pos, size_all);
	GetTypeTable(&type, T);
	settype_slot(pos, type);
	*ret = pos;
}

static int structure_define2_slots_copy_(addr pos, size_t size_all, addr *ret)
{
	slot_copy_heap(&pos, pos);
	setlocation_slot(pos, size_all);
	setaccess_slot(pos, size_all);
	return Result(ret, pos);
}

static int structure_define2_slots_direct_(struct defstruct *str)
{
	addr instance, direct, list, pos;
	size_t size, size_all, i;

	/* slot vector */
	list = str->slots;
	Return(length_list_safe_(list, &size));
	if (str->named_p)
		size++;
	slot_vector_heap(&direct, size);
	instance = str->instance;
	str->size = size;

	/* location */
	size_all = 0;
	if (str->include_p)
		size_all += get_size_all_structure(str->iname);
	size_all += str->offset;

	/* named */
	i = 0;
	if (str->named_p) {
		str->named_index = size_all;
		structure_define2_slots_named(str->name, size_all, &pos);
		SetSlotVector(direct, i++, pos);
		size_all++;
	}

	/* slots */
	while (i < size) {
		Return_getcons(list, &pos, &list);
		Return(structure_define2_slots_copy_(pos, size_all, &pos));
		SetSlotVector(direct, i++, pos);
		size_all++;
	}
	str->size_all = size_all;

	/* instance */
	SetDirectStructure(instance, direct);
	set_size_structure(instance, str->size);
	set_size_all_structure(instance, str->size_all);

	return 0;
}

static int structure_define2_slots_arguments_(addr list, addr name1, addr *ret)
{
	int check;
	addr pos, name2;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		getname_slot(pos, &name2);
		GetNameSymbol(name2, &name2);
		Return(string_equal_(name1, name2, &check));
		if (check)
			return Result(ret, pos);
	}

	return Result(ret, Unbound);
}

static int structure_define2_slots_update_(struct defstruct *str, addr pos, addr *ret)
{
	addr name, make;
	size_t value;

	/* include arguments */
	if (str->include_p) {
		getname_slot(pos, &name);
		GetNameSymbol(name, &name);
		Return(structure_define2_slots_arguments_(str->iargs, name, &make));
		if (make != Unbound)
			pos = make;
	}

	/* copy */
	slot_copy_heap(&make, pos);
	getlocation_slot(make, &value);
	setlocation_slot(pos, value);
	getaccess_slot(make, &value);
	setaccess_slot(pos, value);

	return Result(ret, make);
}

static int structure_define2_slots_effective_(struct defstruct *str)
{
	addr instance, direct, super, slots, pos;
	size_t size1, size2, i, k;

	/* no include */
	instance = str->instance;
	GetDirectStructure(instance, &direct);
	if (! str->include_p)
		goto no_include;

	/* include */
	GetSlotsStructure(str->iname, &super);
	LenSlotVector(super, &size1);
	if (size1 == 0)
		goto no_include;

	/* copy */
	size2 = str->size;
	slot_vector_heap(&slots, size1 + size2);
	for (i = 0; i < size1; i++) {
		GetSlotVector(super, i, &pos);
		Return(structure_define2_slots_update_(str, pos, &pos));
		SetSlotVector(slots, i, pos);
	}
	for (k = 0; k < size2; k++, i++) {
		GetSlotVector(direct, k, &pos);
		slot_copy_heap(&pos, pos);
		SetSlotVector(slots, i, pos);
	}
	SetSlotsStructure(instance, slots);
	str->slots = slots;
	return 0;

no_include:
	SetSlotsStructure(instance, direct);
	str->slots = direct;
	return 0;
}

int structure_define2_slots_(struct defstruct *str)
{
	Return(structure_define2_slots_include_(str));
	Return(structure_define2_slots_direct_(str));
	Return(structure_define2_slots_effective_(str));

	return 0;
}

int structure_define3_slots_(struct defstruct *str)
{
	return structure_define2_slots_(str);
}


/* reader2 */
static int function_structure_reader2(Execute ptr, addr var)
{
	int check;
	addr data, slot;
	size_t index;

	/* closure */
	getdata_control(ptr, &data);
	CheckType(data, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_list_p(data, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-list.", var, NULL);
	/* access */
	GetSlotStructureType(data, &slot);
	getaccess_slot(slot, &index);
	Return(getnth_(var, index, &var));
	setresult_control(ptr, var);

	return 0;
}

static void type_structure_reader2(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_reader2(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader2);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_reader2(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* reader3 */
static int function_structure_reader3(Execute ptr, addr var)
{
	int check;
	addr data, slot, type;

	/* closure */
	getdata_control(ptr, &data);
	CheckType(data, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_vector_p(ptr, data, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-vector.", var, NULL);
	/* access */
	GetSlotStructureType(data, &slot);
	GetVectorStructureType(data, &type);
	Return(structure_getarray_(var, slot, &var));
	setresult_control(ptr, var);

	return 0;
}

static void type_structure_reader3(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_reader3(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader3);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_reader3(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* writer2 */
static int function_structure_writer2(Execute ptr, addr value, addr var)
{
	int check;
	addr data, slot;

	/* closure */
	getdata_control(ptr, &data);
	CheckType(data, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_list_p(data, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-list.", var, NULL);
	/* access */
	GetSlotStructureType(data, &slot);
	Return(structure_write2_(ptr, var, slot, value));
	setresult_control(ptr, value);

	return 0;
}

static void type_structure_writer2(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_writer2(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer2);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_writer2(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}

/* writer3 */
static int function_structure_writer3(Execute ptr, addr value, addr var)
{
	int check;
	addr data, slot, vector_type;

	/* closure */
	getdata_control(ptr, &data);
	CheckType(data, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	Return(structure_type_vector_p(ptr, data, var, &check));
	if (! check)
		return fmte_("The argument ~S must be a structure-vector.", var, NULL);
	/* access */
	GetSlotStructureType(data, &slot);
	GetVectorStructureType(data, &vector_type);
	Return(structure_write3_(ptr, var, slot, vector_type, value));
	setresult_control(ptr, value);

	return 0;
}

static void type_structure_writer3(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_writer3(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer3);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_writer3(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/*
 *  call
 */
static int structure_define2_callname_(struct defstruct *str, addr *ret, addr pos)
{
	addr name;

	Check(! slotp(pos), "type error");
	getname_slot(pos, &pos);
	Check(! symbolp(pos), "type error");
	GetNameSymbol(pos, &pos);
	if (str->conc_name == Unbound) {
		GetNameStructure(str->instance, &name);
		GetNameSymbol(name, &name);
		Return(string_concat_hyphen_heap_(ret, name, pos));
	}
	else if (str->conc_name == Nil) {
		*ret = pos;
	}
	else {
		Check(! stringp(str->conc_name), "type error");
		Return(string_concat_heap_(ret, str->conc_name, pos));
	}

	return 0;
}

static int structure_define2_intern_(struct defstruct *str,
		addr package, addr pos, addr *ret)
{
	addr symbol, call, instance, cons;

	/* callname */
	Return(structure_define2_callname_(str, &symbol, pos));
	Return(intern_package_(package, symbol, &symbol, NULL));
	Return(parse_callname_error_(&call, symbol));

	/* push access */
	getname_slot(pos, &pos);
	cons_heap(&pos, pos, symbol);
	instance = str->instance;
	GetAccessStructure(instance, &cons);
	cons_heap(&cons, pos, cons);
	SetAccessStructure(instance, cons);

	/* result */
	return Result(ret, call);
}

int structure_define2_call_(struct defstruct *str)
{
	addr package, type, slots, pos, call, readonly;
	size_t size, i;

	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_define2_intern_(str, package, pos, &call));

		structure_type(str, pos, &type);
		defun_structure_reader2(type, call);
		getreadonly_slot(pos, &readonly);
		if (readonly == Nil)
			defun_structure_writer2(type, call);
	}

	return 0;
}

int structure_define3_call_(struct defstruct *str)
{
	addr package, type, slots, pos, call, readonly;
	size_t size, i;

	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_define2_intern_(str, package, pos, &call));

		structure_type(str, pos, &type);
		defun_structure_reader3(type, call);
		getreadonly_slot(pos, &readonly);
		if (readonly == Nil)
			defun_structure_writer3(type, call);
	}

	return 0;
}


/*
 *  constructor
 */
/* list */
static int function_structure_constructor2(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure2_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void type_structure_constructor2(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_constructor2(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor2);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_constructor2(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* vector */
static int function_structure_constructor3(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure3_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void type_structure_constructor3(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_constructor3(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor3);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_constructor3(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* default */
static int structure_constructor2_push_(struct defstruct *str, addr symbol)
{
	addr instance, list;

	Check(! symbolp(symbol), "type error");
	instance = str->instance;
	GetConstructorStructure(instance, &list);
	cons_heap(&list, symbol, list);
	SetConstructorStructure(instance, list);

	return 0;
}

static int structure_constructor2_default_(struct defstruct *str, addr symbol)
{
	addr pos, call;

	Return(parse_callname_error_(&call, symbol));
	Return(structure_constructor2_push_(str, symbol));
	structure_type(str, str->slots, &pos);
	if (str->type_list_p)
		defun_structure_constructor2(pos, call);
	else if (str->type_vector_p)
		defun_structure_constructor3(pos, call);
	else
		return fmte_("Invalid structure type.", NULL);

	return 0;
}

static int structure_constructor2_make_(struct defstruct *str)
{
	addr name;

	/* name */
	GetNameStructure(str->instance, &name);
	GetNameSymbol(name, &name);
	Return(string_concat_char1_heap_(&name, "MAKE-", name));
	Return(intern_default_package_(str->ptr, name, &name, NULL));
	/* make */
	return structure_constructor2_default_(str, name);
}

static int structure_constructor2_lambda_(struct defstruct *str, addr list)
{
	addr symbol, name, pos, type;

	/* (symbol function) */
	Return(list_bind_(list, &symbol, &pos, NULL));
	Return(parse_callname_error_(&name, symbol));
	Return(structure_constructor2_push_(str, symbol));
	SetNameFunction(pos, name);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, Function);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);

	return 0;
}

int structure_define2_constructor_(struct defstruct *str)
{
	addr list, pos, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = str->constructor; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		if (pos == g) {
			Return(structure_constructor2_make_(str));
		}
		else if (symbolp(pos)) {
			Return(structure_constructor2_default_(str, pos));
		}
		else if (consp(pos)) {
			Return(structure_constructor2_lambda_(str, pos));
		}
		else {
			return fmte_("Invalid constructor parameter ~S.", pos, NULL);
		}
	}

	return 0;
}

int structure_define3_constructor_(struct defstruct *str)
{
	return structure_define2_constructor_(str);
}


/*
 *  copier
 */
/* list */
static int function_structure_copier2(Execute ptr, addr var)
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
	copy_list_heap_safe(&var, var);
	setresult_control(ptr, var);

	return 0;
}

static void type_structure_copier2(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_copier2(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier2);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	type_structure_copier2(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* vector */
static int function_structure_copier3(Execute ptr, addr var)
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
	Return(vector_copy_heap_(&var, var));
	setresult_control(ptr, var);

	return 0;
}

static void type_structure_copier3(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_copier3(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier3);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	type_structure_copier3(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* define */
static int structure_define2_copier_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->copier_p) {
		GetNameStructure(str->instance, &name);
		GetNameSymbol(name, &name);
		Return(string_concat_char1_heap_(&name, "COPY-", name));
	}
	else if (str->copier == Nil) {
		return Result(ret, Nil);
	}
	else {
		Check(! stringp(str->copier), "type error");
		name = str->copier;
	}

	return intern_default_package_(str->ptr, name, ret, NULL);
}

int structure_define2_copier_(struct defstruct *str)
{
	addr symbol;

	Return(structure_define2_copier_callname_(str, &symbol));
	SetCopierStructure(str->instance, symbol);
	if (symbol == Nil)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_list_p)
		defun_structure_copier2(str, symbol);
	else if (str->type_vector_p)
		defun_structure_copier3(str, symbol);
	else
		return fmte_("Invalid structure type.", NULL);

	return 0;
}

int structure_define3_copier_(struct defstruct *str)
{
	return structure_define2_copier_(str);
}


/*
 *  predicate
 */
/* list */
static int function_structure_predicate2(Execute ptr, addr var)
{
	int check;
	addr type;

	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	Return(structure_type_list_p(type, var, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_structure_predicate2(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate2);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* vector */
static int function_structure_predicate3(Execute ptr, addr var)
{
	int check;
	addr type;

	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	Return(structure_type_vector_p(ptr, type, var, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_structure_predicate3(struct defstruct *str, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate3);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(str, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* define */
static int structure_define2_predicate_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->predicate_p) {
		GetNameStructure(str->instance, &name);
		GetNameSymbol(name, &name);
		Return(string_concat_char2_heap_(&name, name, "-P"));
	}
	else if (str->predicate == Nil) {
		return Result(ret, Nil);
	}
	else {
		Check(! stringp(str->predicate), "type error");
		name = str->predicate;
	}

	return intern_default_package_(str->ptr, name, ret, NULL);
}

int structure_define2_predicate_(struct defstruct *str)
{
	addr instance, symbol;

	instance = str->instance;
	Return(structure_define2_predicate_callname_(str, &symbol));
	SetPredicateStructure(instance, symbol);
	if (symbol == Nil)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_list_p)
		defun_structure_predicate2(str, symbol);
	else if (str->type_vector_p)
		defun_structure_predicate3(str, symbol);
	else
		return fmte_("Invalid structure type.", NULL);

	return 0;
}

int structure_define3_predicate_(struct defstruct *str)
{
	return structure_define2_predicate_(str);
}


/*
 *  print
 */
int structure_define2_print_(struct defstruct *str)
{
	if (str->type_p && str->print_object_p)
		return fmte_("Can't make print-object on :TYPE structure.", NULL);
	if (str->type_p && str->print_function_p)
		return fmte_("Can't make print-function on :TYPE structure.", NULL);

	return 0;
}

int structure_define3_print_(struct defstruct *str)
{
	return structure_define2_print_(str);
}


/*
 *  define
 */
int structure_define2_(struct defstruct *str)
{
	/* make instance */
	structure_instance2(str);
	Check(! structure_object_p(str->instance), "type error");
	setstructure_symbol(str->name, str->instance);

	/* settings */
	Return(structure_define2_slots_(str));
	Return(structure_define2_call_(str));
	Return(structure_define2_copier_(str));
	Return(structure_define2_predicate_(str));
	Return(structure_define2_constructor_(str));
	Return(structure_define2_print_(str));

	return 0;
}

int structure_define3_(struct defstruct *str)
{
	/* make instance */
	structure_instance2(str);
	Check(! structure_object_p(str->instance), "type error");
	setstructure_symbol(str->name, str->instance);

	/* settings */
	Return(structure_define3_slots_(str));
	Return(structure_define3_call_(str));
	Return(structure_define3_copier_(str));
	Return(structure_define3_predicate_(str));
	Return(structure_define3_constructor_(str));
	Return(structure_define3_print_(str));

	return 0;
}


/*
 *  initialize
 */
void init_structure_define2(void)
{
	SetPointerCall(defun, var1, structure_reader2);
	SetPointerCall(defun, var2, structure_writer2);
	SetPointerCall(defun, dynamic, structure_constructor2);
	SetPointerCall(defun, var1, structure_copier2);
	SetPointerCall(defun, var1, structure_predicate2);
}

void init_structure_define3(void)
{
	SetPointerCall(defun, var1, structure_reader3);
	SetPointerCall(defun, var2, structure_writer3);
	SetPointerCall(defun, dynamic, structure_constructor3);
	SetPointerCall(defun, var1, structure_copier3);
	SetPointerCall(defun, var1, structure_predicate3);
}

