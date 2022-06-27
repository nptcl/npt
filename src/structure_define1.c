#include "callname.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_method.h"
#include "clos_slot.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "execute_values.h"
#include "function.h"
#include "integer.h"
#include "mop.h"
#include "package.h"
#include "package_intern.h"
#include "print_object.h"
#include "strtype.h"
#include "structure.h"
#include "structure_access.h"
#include "structure_define1.h"
#include "structure_direct.h"
#include "structure_make.h"
#include "structure_object.h"
#include "symbol.h"
#include "type_memory.h"
#include "type_table.h"
#include "typedef.h"

/*
 *  make-instance
 */
static int structure_instance1_include_(struct defstruct *str, addr instance)
{
	addr list, pos;

	/* include */
	if (str->include_p) {
		Return(stdset_structure_include_(instance, str->iname));
	}

	/* precedence-list */
	list = Nil;
	for (pos = instance; pos != Nil; ) {
		cons_heap(&list, pos, list);
		Return(stdget_structure_include_(pos, &pos));
	}
	GetConst(CLOS_STRUCTURE_OBJECT, &pos);
	cons_heap(&list, pos, list);
	GetConst(CLOS_T, &pos);
	cons_heap(&list, pos, list);
	nreverse(&list, list);
	Return(stdset_structure_precedence_list_(instance, list));

	return 0;
}

int structure_instance1_(struct defstruct *str)
{
	addr clos, instance;

	/* structure */
	GetConst(CLOS_STRUCTURE_CLASS, &clos);
	Return(clos_instance_heap_(clos, &instance));
	SetClassOfClos(instance, clos);
	/* name */
	Return(stdset_structure_name_(instance, str->name));
	/* documentation */
	if (str->doc != Nil) {
		Return(stdset_structure_documentation_(instance, str->doc));
	}
	/* include, precedence-list */
	Return(structure_instance1_include_(str, instance));
	/* check */
	Check(str->type_p, "type error");
	Check(str->type_list_p, "type error");
	Check(str->type_vector_p, "type error");
	Check(str->named_p, "named error");
	/* result */
	str->instance = instance;

	return 0;
}


/*
 *  slots
 */
static int structure_define1_slots_pushnew_(LocalRoot local,
		addr *push, addr value, addr list, int *ret)
{
	int check;
	addr root, x;

	Check(! stringp(value), "type error");
	for (root = list; list != Nil; ) {
		Return_getcons(list, &x, &list);
		Return(string_equal_(x, value, &check));
		if (check)
			return Result(ret, 0);
	}
	cons_local(local, push, value, root);

	return Result(ret, 1);
}

static int structure_define1_slots_find_(addr name, addr list, addr *ret)
{
	int check;
	addr pos, value;

	Check(! stringp(name), "type error");
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetNameSlot(pos, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(name, value, &check));
		if (check)
			return Result(ret, pos);
	}

	return 0;
}

static int structure_define1_slots_get_(struct defstruct *str, size_t *ret)
{
	addr pos;
	size_t value;

	*ret = 0;
	value = 0;
	if (str->include_p) {
		pos = str->iname;
		Return(stdget_structure_value_(pos, &pos));
		if (pos != Nil) {
			Return(getindex_integer_(pos, &value));
		}
	}
	value += str->offset;

	return Result(ret, value);
}

static int structure_define1_slots_set_(struct defstruct *str, size_t value)
{
	addr pos, instance;

	/* value */
	instance = str->instance;
	str->size_all = value;
	make_index_integer_heap(&pos, value);
	Return(stdset_structure_value_(instance, pos));

	return 0;
}

static int structure_define1_slots_include_(struct defstruct *str,
		addr *slots, addr *root, size_t *ret)
{
	LocalRoot local;
	addr list, pos, name, args;
	size_t size, i;

	local = str->ptr->local;
	*root = *slots = Nil;
	*ret = 0;
	size = 0;
	if (str->include_p) {
		pos = str->iname;
		Return(stdget_structure_slots_(pos, &list));
		args = str->iargs;
		LenSlotVector(list, &size);
		for (i = 0; i < size; i++) {
			GetSlotVector(list, i, &pos);
			slot_copy_heap(&pos, pos);
			GetNameSlot(pos, &name);
			GetNameSymbol(name, &name);
			cons_local(local, root, name, *root);
			Return(structure_define1_slots_find_(name, args, &pos));
			Check(! slotp(pos), "type error");
			cons_local(local, slots, pos, *slots);
			/* location */
			SetLocationSlot(pos, i);
		}
	}

	return Result(ret, size);
}

static int structure_define1_slots_slots_(struct defstruct *str,
		addr *slots, addr *root, addr *rdirect, size_t *ret)
{
	int check;
	LocalRoot local;
	addr list, pos, name, direct;
	size_t value, size, size_direct;

	local = str->ptr->local;
	Return(structure_define1_slots_get_(str, &value));

	/* size */
	*ret = 0;
	size = 0;
	if (str->include_p) {
		Return(stdget_structure_slots_(str->iname, &pos));
		LenSlotVector(pos, &size);
	}

	/* loop */
	direct = Nil;
	size_direct = 0;
	list = str->slots;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetNameSlot(pos, &name);
		GetNameSymbol(name, &name);
		Return(structure_define1_slots_pushnew_(local, root, name, *root, &check));
		if (check) {
			/* slots */
			cons_local(local, slots, pos, *slots);
			SetLocationSlot(pos, size++);
			SetAccessSlot(pos, value++);
			/* direct-slots */
			cons_local(local, &direct, pos, direct);
			size_direct++;
		}
	}

	/* result */
	Return(structure_define1_slots_set_(str, value));
	*rdirect = direct;
	return Result(ret, size_direct);
}

static int structure_define1_slots_array_(struct defstruct *str, addr slots, size_t size)
{
	addr instance, array, pos;
	size_t i;

	instance = str->instance;
	slot_vector_heap(&array, size);
	while (slots != Nil) {
		Return_getcons(slots, &pos, &slots);
		GetLocationSlot(pos, &i);
		SetClassSlot(pos, instance);
		SetSlotVector(array, i, pos);
	}
	str->slots = array;
	Return(stdset_structure_slots_(instance, array));

	return 0;
}

static int structure_define1_slots_direct_(struct defstruct *str, addr list, size_t size)
{
	addr instance, array, pos;
	size_t i;

	/* direct-slots array */
	instance = str->instance;
	slot_vector_heap(&array, size);
	nreverse(&list, list);
	for (i = 0; list != Nil; i++) {
		Return_getcons(list, &pos, &list);
		SetClassSlot(pos, instance);
		SetSlotVector(array, i, pos);
	}
	Return(stdset_structure_direct_slots_(instance, array));

	return 0;
}

static int structure_define1_slots_call_(struct defstruct *str)
{
	addr slots, root, direct;
	size_t size, size_direct;

	/* make slots */
	Return(structure_define1_slots_include_(str, &slots, &root, &size));
	Return(structure_define1_slots_slots_(str, &slots, &root, &direct, &size_direct));
	size += size_direct;
	str->size = size;

	/* set slots */
	Return(structure_define1_slots_array_(str, slots, size));
	Return(structure_define1_slots_direct_(str, direct, size_direct));

	return 0;
}

int structure_define1_slots_(struct defstruct *str)
{
	LocalRoot local;
	LocalStack stack;

	local = str->ptr->local;
	push_local(local, &stack);
	Return(structure_define1_slots_call_(str));
	rollback_local(local, stack);

	return 0;
}


/*
 *  accessor
 */
/* reader1 */
static int function_structure_reader1(Execute ptr, addr var)
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

static void type_structure_reader1(addr *ret, addr instance)
{
	addr args, values;

	type_clos_heap(instance, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_reader1(addr instance, addr slot, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_reader1);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, slot);
	/* type */
	type_structure_reader1(&type, instance);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* writer1 */
static int function_structure_writer1(Execute ptr, addr value, addr var)
{
	int check;
	addr slot, pos;

	/* closure */
	getdata_control(ptr, &slot);
	/* check-type */
	GetClassSlot(slot, &pos);
	Return(typep_structure_(var, pos, &check));
	if (! check)
		return fmte_("The structure ~S must be a ~S type.", pos, var, NULL);
	/* result */
	Return(structure_write1_(ptr, var, slot, value));
	setresult_control(ptr, value);

	return 0;
}

static void type_structure_writer1(addr *ret, addr instance)
{
	addr args, values;

	GetTypeTable(&args, T);
	type_clos_heap(instance, &values);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_writer1(addr instance, addr slot, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_setf_system(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var2(pos, p_defun_structure_writer1);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, slot);
	/* type */
	type_structure_writer1(&type, instance);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}


/*
 *  call
 */
static int structure_define1_callname_(struct defstruct *str, addr *ret, addr pos)
{
	addr name;

	Check(! slotp(pos), "type error");
	GetNameSlot(pos, &pos);
	Check(! symbolp(pos), "type error");
	GetNameSymbol(pos, &pos);
	if (str->conc_name == Unbound) {
		Return(stdget_structure_name_(str->instance, &name));
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

static int structure_define1_intern_(struct defstruct *str,
		addr package, addr pos, addr *ret)
{
	addr symbol, call, instance, cons;

	/* callname */
	Return(structure_define1_callname_(str, &symbol, pos));
	Return(intern_package_(package, symbol, &symbol, NULL));
	Return(parse_callname_error_(&call, symbol));

	/* push access */
	GetNameSlot(pos, &pos);
	cons_heap(&pos, pos, symbol);
	instance = str->instance;
	Return(stdget_structure_access_(instance, &cons));
	cons_heap(&cons, pos, cons);
	Return(stdset_structure_access_(instance, cons));

	/* result */
	return Result(ret, call);
}

int structure_define1_call_(struct defstruct *str)
{
	addr instance, package, slots, pos, call, readonly;
	size_t size, i;

	instance = str->instance;
	Check(! structure_class_p_debug(instance), "type error");
	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_define1_intern_(str, package, pos, &call));

		defun_structure_reader1(instance, pos, call);
		GetReadOnlySlot(pos, &readonly);
		if (readonly == Nil)
			defun_structure_writer1(instance, pos, call);
	}

	return 0;
}


/*
 *  constructor
 */
static int function_structure_constructor1(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	Return(make_structure1_(ptr, &pos, pos, args, 1));
	setresult_control(ptr, pos);

	return 0;
}

static void type_structure_constructor1(addr *ret, addr data)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetInstanceStructureType(data, &data);
	type_clos_heap(data, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_constructor1(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor1);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	type_structure_constructor1(&type, data);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* default */
static int structure_constructor1_push_(struct defstruct *str, addr symbol)
{
	addr instance, list;

	Check(! symbolp(symbol), "type error");
	instance = str->instance;
	Return(stdget_structure_constructor_(instance, &list));
	cons_heap(&list, symbol, list);
	Return(stdset_structure_constructor_(instance, list));

	return 0;
}

static int structure_constructor1_default_(struct defstruct *str, addr symbol)
{
	addr pos, call;

	Return(parse_callname_error_(&call, symbol));
	Return(structure_constructor1_push_(str, symbol));
	structure_type(str, str->slots, &pos);
	if (str->type_p)
		return fmte_("Invalid structure type.", NULL);
	defun_structure_constructor1(pos, call);

	return 0;
}

static int structure_constructor1_make_(struct defstruct *str)
{
	addr name;

	/* name */
	Return(stdget_structure_name_(str->instance, &name));
	GetNameSymbol(name, &name);
	Return(string_concat_char1_heap_(&name, "MAKE-", name));
	Return(intern_default_package_(str->ptr, name, &name, NULL));
	/* make */
	return structure_constructor1_default_(str, name);
}

static int structure_constructor1_lambda_(struct defstruct *str, addr list)
{
	addr symbol, name, pos, type;

	/* (symbol function) */
	Return(list_bind_(list, &symbol, &pos, NULL));
	Return(parse_callname_error_(&name, symbol));
	Return(structure_constructor1_push_(str, symbol));
	SetNameFunction(pos, name);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, Function);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);

	return 0;
}

int structure_define1_constructor_(struct defstruct *str)
{
	addr list, pos, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = str->constructor; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		if (pos == g) {
			Return(structure_constructor1_make_(str));
		}
		else if (symbolp(pos)) {
			Return(structure_constructor1_default_(str, pos));
		}
		else if (consp(pos)) {
			Return(structure_constructor1_lambda_(str, pos));
		}
		else {
			return fmte_("Invalid constructor parameter ~S.", pos, NULL);
		}
	}

	return 0;
}


/*
 *  copier
 */
static int function_structure_copier1(Execute ptr, addr var)
{
	copy_structure_common(var, &var);
	setresult_control(ptr, var);
	return 0;
}

static void type_structure_copier1(addr *ret, addr instance)
{
	addr args, values;

	type_clos_heap(instance, &values);
	typeargs_var1(&args, values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_structure_copier1(addr instance, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_copier1);
	SetFunctionSymbol(symbol, pos);
	/* type */
	type_structure_copier1(&type, instance);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int structure_define1_copier_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->copier_p) {
		Return(stdget_structure_name_(str->instance, &name));
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

int structure_define1_copier_(struct defstruct *str)
{
	addr symbol;

	Return(structure_define1_copier_callname_(str, &symbol));
	Return(stdset_structure_copier_(str->instance, symbol));
	if (symbol == Nil)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_p)
		return fmte_("Invalid structure type.", NULL);
	defun_structure_copier1(str->instance, symbol);

	return 0;
}


/*
 *  predicate
 */
static int function_structure_predicate1(Execute ptr, addr var)
{
	int check;
	addr instance;

	getdata_control(ptr, &instance);
	Return(typep_structure_(var, instance, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_structure_predicate1(addr instance, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	GetCallName(symbol, &symbol);
	setcompiled_var1(pos, p_defun_structure_predicate1);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, instance);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static int structure_define1_predicate_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->predicate_p) {
		Return(stdget_structure_name_(str->instance, &name));
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

int structure_define1_predicate_(struct defstruct *str)
{
	addr instance, symbol;

	instance = str->instance;
	Return(structure_define1_predicate_callname_(str, &symbol));
	Return(stdset_structure_predicate_(instance, symbol));
	if (symbol == Nil)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_p)
		return fmte_("Invalid structure type.", NULL);
	defun_structure_predicate1(instance, symbol);

	return 0;
}


/*
 *  printer
 */
/* default */
static int method_defstruct_default(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	int check;

	Return(structure_instance_p_(var, &check));
	if (! check)
		return fmte_("Invalid structure type ~S.", var, NULL);
	Return(print_structure_(ptr, stream, var));
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

static int structure_print_default_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_default_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}


/* object */
static int method_defstruct_object(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	int check;
	addr call;

	Return(structure_instance_p_(var, &check));
	if (! check)
		return fmte_("Invalid structure type ~S.", var, NULL);
	getdata_control(ptr, &call);
	Return(apply1_control_(ptr, &call, call, Nil));
	Return(funcall1_control_(ptr, &call, call, var, stream, NULL));
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

static int structure_print_object_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_object_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}


/* function */
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
	Return(apply1_control_(ptr, &call, call, Nil));
	Return(funcall1_control_(ptr, &call, call, var, stream, pos, NULL));
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

static int structure_print_function_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_function_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}


/* define */
static int structure_print_default_p(struct defstruct *str)
{
	addr g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	if (str->print_object_p && str->print_object == g)
		return 1;
	if (str->print_function_p && str->print_function == g)
		return 1;

	return 0;
}

int structure_define1_print_(struct defstruct *str)
{
	if (str->type_p && str->print_object_p)
		return fmte_("Can't make print-object on :TYPE structure.", NULL);
	if (str->type_p && str->print_function_p)
		return fmte_("Can't make print-function on :TYPE structure.", NULL);
	if (structure_print_default_p(str))
		return structure_print_default_(str);
	else if (str->print_object_p)
		return structure_print_object_(str);
	else if (str->print_function_p)
		return structure_print_function_(str);

	return 0;
}

int structure_define1_(struct defstruct *str)
{
	/* make instance */
	Return(structure_instance1_(str));
	Check(! structure_class_p_debug(str->instance), "type error");
	clos_define_class(str->name, str->instance);

	/* settings */
	Return(structure_define1_slots_(str));
	Return(structure_define1_call_(str));
	Return(structure_define1_copier_(str));
	Return(structure_define1_predicate_(str));
	Return(structure_define1_constructor_(str));
	Return(structure_define1_print_(str));

	return 0;
}


/*
 *  initialize
 */
void init_structure_define1(void)
{
	SetPointerCall(defun, var1, structure_reader1);
	SetPointerCall(defun, var2, structure_writer1);
	SetPointerCall(defun, dynamic, structure_constructor1);
	SetPointerCall(defun, var1, structure_copier1);
	SetPointerCall(defun, var1, structure_predicate1);
	SetPointerType(var4, method_defstruct_default);
	SetPointerType(var4, method_defstruct_object);
	SetPointerType(var4, method_defstruct_function);
}

