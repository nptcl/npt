#include "callname.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_method.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "equal.h"
#include "execute_values.h"
#include "function.h"
#include "integer.h"
#include "mop.h"
#include "package.h"
#include "package_intern.h"
#include "print_object.h"
#include "sequence.h"
#include "subtypep.h"
#include "strtype.h"
#include "structure.h"
#include "structure_define.h"
#include "structure_object.h"
#include "symbol.h"
#include "type_table.h"
#include "typedef.h"

static int ensure_structure_constructor_(addr args, addr *result, int *ret)
{
	addr key, value, keyword, root;

	GetConst(KEYWORD_CONSTRUCTOR, &keyword);
	for (root = Nil; args != Nil; ) {
		Return_getcons(args, &key, &args);
		Return_getcons(args, &value, &args);
		if (key != keyword)
			continue;
		cons_heap(&root, value, root);
	}
	nreverse(result, root);

	return Result(ret, root != Nil);
}

static int ensure_structure_struct_(struct defstruct *str,
		Execute ptr, addr name, addr slots, addr args)
{
	int check;
	addr pos, value;

	defstruct_clean(str);
	str->ptr = ptr;
	str->slots = slots;
	str->name = name;
	/* :documentation */
	if (GetKeyArgs(args, KEYWORD_DOCUMENTATION, &pos)) pos = Nil;
	str->doc = pos;
	/* :conc-name */
	if (! GetKeyArgs(args, KEYWORD_CONC_NAME, &pos)) {
		str->conc_name_p = 1;
		str->conc_name = pos;
	}
	/* :type */
	if (! GetKeyArgs(args, KEYWORD_TYPE, &pos)) {
		GetConst(COMMON_LIST, &value);
		if (pos == value) {
			str->type_list_p = 1;
		}
		else {
			str->type_vector_p = 1;
			str->type_vector = pos;
		}
		str->type_p = 1;
	}
	/* :initial-offset */
	if (! GetKeyArgs(args, KEYWORD_INITIAL_OFFSET, &pos)) {
		str->initial_offset_p = 1;
		str->initial_offset = pos;
		Return(getindex_integer_(pos, &(str->offset)));
	}
	/* :named */
	if (! GetKeyArgs(args, KEYWORD_NAMED, &pos)) {
		str->named_p = (pos != Nil);
		str->offset++;
	}
	/* :copier */
	if (! GetKeyArgs(args, KEYWORD_COPIER, &pos)) {
		str->copier_p = 1;
		str->copier = pos;
	}
	/* :predicate */
	if (! GetKeyArgs(args, KEYWORD_PREDICATE, &pos)) {
		str->predicate_p = 1;
		str->predicate = pos;
	}
	/* :include */
	if (! GetKeyArgs(args, KEYWORD_INCLUDE, &pos)) {
		if (! consp(pos))
			return fmte_("Invalid :include format ~S.", pos, NULL);
		GetCons(pos, &pos, &value);
		str->include_p = 1;
		str->iname = pos;
		str->iargs = value;
	}
	/* :print-object */
	if (! GetKeyArgs(args, KEYWORD_PRINT_OBJECT, &pos)) {
		str->print_object_p = 1;
		str->print_object = pos;
	}
	/* :print-function */
	if (! GetKeyArgs(args, KEYWORD_PRINT_FUNCTION, &pos)) {
		str->print_function_p = 1;
		str->print_function = pos;
	}
	/* :constructor */
	Return(ensure_structure_constructor_(args, &pos, &check));
	if (check) {
		str->constructor_p = 1;
		str->constructor = pos;
	}

	return 0;
}


/*
 *  check-instance
 */
static int structure_check_name_(struct defstruct *ptr)
{
	addr pos;

	clos_find_class_nil(ptr->name, &pos);
	if (pos != Nil) {
		Return(fmtw_("The structure ~S already exists.", ptr->name, NULL));
	}

	return 0;
}

static int structure_slots_heap_(addr list, addr *ret)
{
	addr pos, name, init, type, readonly, root;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		Return(list_bind_(pos, &name, &init, &type, &readonly, NULL));
		slot_heap(&pos);
		SetNameSlot(pos, name);
		SetTypeSlot(pos, type);
		SetFunctionSlot(pos, init);
		SetReadOnlySlot(pos, readonly);
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static int structure_check_slots_(addr list)
{
	int check;
	addr pos, a, b, tail;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		GetNameSlot(pos, &a);
		Check(! symbolp(a), "type error");
		GetNameSymbol(a, &a);
		for (tail = list; tail != Nil; ) {
			Return_getcons(tail, &pos, &tail);
			GetNameSlot(pos, &b);
			Check(! symbolp(b), "type error");
			GetNameSymbol(b, &b);
			Return(string_equal_(a, b, &check));
			if (check) {
				return fmte_("The slot name ~S "
						"is duplicated in the defstruct.", a, NULL);
			}
		}
	}

	return 0;
}

static int structure_check_predicate_(struct defstruct *str)
{
	if (str->type_p && (! str->named_p)) {
		/* no-predicate */
		if (! str->predicate_p) {
			str->predicate_p = 1;
			str->predicate = Nil;
			return 0;
		}
		if (str->predicate == Nil) {
			return 0;
		}
		return fmte_("DEFSTRUCT ~S is defined :PREDICATE, "
				"but the structure is not named.", str->name, NULL);
	}
	if (str->predicate_p && str->predicate == T) {
		str->predicate_p = 0;
		return 0;
	}

	return 0;
}

static int structure_include_(struct defstruct *str)
{
	int check;
	addr instance, x, y;

	if (! str->include_p)
		return 0;
	/* instance check */
	clos_find_class_nil(str->iname, &instance);
	if (instance == Nil)
		return fmte_(":INCLUDE ~S structure don't exist.", str->iname, NULL);
	Return(structure_class_p_(instance, &check));
	if (! check)
		return fmte_(":INCLUDE ~S must be structure type.", instance, NULL);

	/* class check */
	Return(stdget_structure_type_(instance, &x));
	GetConst(COMMON_CLASS, &y);
	if (x == y) {
		if (str->type_list_p || str->type_vector_p) {
			return fmte_(":TYPE option is CLASS, "
					"but :INCLUDE type is not CLASS.", NULL);
		}
	}

	/* list check */
	GetConst(COMMON_LIST, &y);
	if (x == y) {
		if (! str->type_list_p)
			return fmte_(":TYPE option is LIST, but :INCLUDE type is not LIST.", NULL);
	}

	/* vector check */
	GetConst(COMMON_VECTOR, &y);
	if (x == y) {
		if (! str->type_vector_p) {
			return fmte_(":TYPE option is VECTOR, "
					"but :INCLUDE type is not VECTOR.", NULL);
		}
		x = str->type_vector;
		Return(stdget_structure_vector_(instance, &y));
		Return(subtypep_check_(str->ptr, x, y, Nil, &check, NULL));
		if (! check) {
			return fmte_(":TYPE ~A is not in the include ~A type.", x, y, NULL);
		}
	}

	/* instance */
	str->iname = instance;
	return 0;
}

static int structure_find_slots_(addr instance, addr name, addr *ret)
{
	int check;
	addr slots, pos, value;
	size_t size, i;

	Check(! structure_class_p_debug(instance), "type error");
	Check(! symbolp(name), "type error");

	/* find */
	GetNameSymbol(name, &name);
	Return(stdget_structure_slots_(instance, &slots));
	Check(! slot_vector_p(slots), "type error");
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		GetNameSlot(pos, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(name, value, &check));
		if (check)
			return Result(ret, pos);
	}

	return Result(ret, Unbound);
}

static int structure_include_slots_(struct defstruct *str)
{
	addr name, list, pos, instance;

	if (! str->include_p)
		return 0;
	instance = str->iname;
	for (list = str->slots; list != Nil; ) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &name);
		Return(structure_find_slots_(instance, name, &pos));
		if (pos != Unbound) {
			return fmte_("The slot ~S "
					"already exist in :INCLUDE structure.", name, NULL);
		}
	}

	return 0;
}

static int structure_include_arguments_(struct defstruct *str)
{
	int result;
	addr name, list, instance, a, b, x, y, gensym;

	if (! str->include_p)
		return 0;
	instance = str->iname;
	GetConst(SYSTEM_STRUCTURE_GENSYM, &gensym);
	for (list = str->iargs; list != Nil; ) {
		GetCons(list, &a, &list);
		GetNameSlot(a, &name);
		Return(structure_find_slots_(instance, name, &b));
		if (b == Unbound) {
			return fmte_("The :include argument ~S don't exist "
					"in :INCLUDE structure.", name, NULL);
		}
		/* form */
		GetFunctionSlot(a, &x);
		if (x == gensym) {
			GetFunctionSlot(b, &y);
			SetFunctionSlot(a, y);
		}
		/* type */
		GetTypeSlot(a, &x);
		GetTypeSlot(b, &y);
		if (x == gensym) {
			SetTypeSlot(a, y);
		}
		else {
			Return(subtypep_check_(str->ptr, x, y, Nil, &result, NULL));
			if (! result) {
				return fmte_("The slot ~S type ~A is not "
						"in the include ~A type.", name, x, y, NULL);
			}
		}
		/* readonly */
		GetReadOnlySlot(a, &x);
		GetReadOnlySlot(b, &y);
		if (x == gensym) {
			SetReadOnlySlot(a, y);
		}
		else if (x == Nil && y == T) {
			return fmte_("The slot ~S is readonly "
					"but include slot is not readonly.", name, NULL);
		}
	}

	return 0;
}

static int structure_print_check_(struct defstruct *str)
{
	if (str->print_function_p && str->print_object_p) {
		return fmte_("The defstruct option must be have "
				"either :PRINT-OBJECT or :PRINT-FUNCTION, "
				"but there are both options", NULL);
	}

	return 0;
}

static void structure_slots_value(struct defstruct *str)
{
	addr list, pos, check, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = str->slots; list != Nil; ) {
		GetCons(list, &pos, &list);
		/* init */
		GetFunctionSlot(pos, &check);
		if (check == g) {
			SetFunctionSlot(pos, Nil);
		}
		/* type */
		GetTypeSlot(pos, &check);
		if (check == g) {
			GetTypeTable(&check, T);
			SetTypeSlot(pos, check);
		}
		/* readonly */
		GetReadOnlySlot(pos, &check);
		if (check == g) {
			SetReadOnlySlot(pos, Nil);
		}
	}
}


/*
 *  make-instance
 */
static int structure_instance_include_(struct defstruct *str, addr instance)
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

static int structure_instance_(struct defstruct *str)
{
	addr clos, instance, pos;

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
	Return(structure_instance_include_(str, instance));
	/* type */
	if (str->type_list_p) {
		GetConst(COMMON_LIST, &pos);
		Return(stdset_structure_type_(instance, pos));
	}
	else if (str->type_vector_p) {
		GetConst(COMMON_VECTOR, &pos);
		Return(stdset_structure_type_(instance, pos));
		Return(stdset_structure_vector_(instance, str->type_vector));
	}
	else {
		GetConst(COMMON_CLASS, &pos);
		Return(stdset_structure_type_(instance, pos));
	}
	/* named */
	Return(stdset_structure_named_(instance, str->named_p? T: Nil));
	/* result */
	str->instance = instance;

	return 0;
}


/*
 *  slots-make
 */
static int structure_pushnew_local_(LocalRoot local,
		addr *push, addr value, addr list, int *ret)
{
	int check;
	addr root, x;

	Check(! stringp(value), "type error");
	for (root = list; list != Nil; ) {
		GetCons(list, &x, &list);
		Return(string_equal_(x, value, &check));
		if (check)
			return Result(ret, 0);
	}
	cons_local(local, push, value, root);

	return Result(ret, 1);
}

static int structure_find_slotslist_(addr name, addr list, addr *ret)
{
	int check;
	addr pos, value;

	Check(! stringp(name), "type error");
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(name, value, &check));
		if (check)
			return Result(ret, pos);
	}

	return 0;
}

static int structure_slots_make_(struct defstruct *str)
{
	int check;
	addr root, slots, list, pos, name, args, instance;
	LocalRoot local;
	LocalStack stack;
	size_t size, value, i, count;

	local = str->ptr->local;
	push_local(local, &stack);
	root = slots = Nil;
	size = value = 0;
	/* include */
	if (str->include_p) {
		pos = str->iname;
		Return(stdget_structure_slots_(pos, &list));
		Return(stdget_structure_value_(pos, &pos));
		if (pos != Nil) {
			Return(getindex_integer_(pos, &value));
		}
		args = str->iargs;
		LenSlotVector(list, &count);
		for (i = 0; i < count; i++) {
			GetSlotVector(list, i, &pos);
			slot_copy_heap(&pos, pos);
			GetNameSlot(pos, &name);
			GetNameSymbol(name, &name);
			cons_local(local, &root, name, root);
			Return(structure_find_slotslist_(name, args, &pos));
			Check(! slotp(pos), "type error");
			cons_local(local, &slots, pos, slots);
			/* location */
			SetLocationSlot(pos, size++);
		}
	}
	/* slots */
	if (str->named_p)
		str->named_index = value;
	value += str->offset;
	for (list = str->slots; list != Nil; ) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &name);
		GetNameSymbol(name, &name);
		Return(structure_pushnew_local_(local, &root, name, root, &check));
		if (check) {
			cons_local(local, &slots, pos, slots);
			SetLocationSlot(pos, size++);
			SetAccessSlot(pos, value++);
		}
	}
	/* array */
	instance = str->instance;
	slot_vector_heap(&list, size);
	while (slots != Nil) {
		GetCons(slots, &pos, &slots);
		GetLocationSlot(pos, &i);
		SetClassSlot(pos, instance);
		SetSlotVector(list, i, pos);
	}
	/* result */
	str->size = size;
	str->size_value = value;
	str->slots = list;
	Return(stdset_structure_slots_(instance, list));
	Return(stdset_structure_value_(instance, intsizeh(value)));
	Return(stdset_structure_named_index_(instance,
				str->named_p? intsizeh(str->named_index): Nil));
	rollback_local(local, stack);

	return 0;
}


/*
 *  accessor
 */
static int structure_slot_callname_(struct defstruct *str, addr *ret, addr pos)
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

/* list */
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

static void structure_slot_reader_list(addr data, addr symbol)
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

static void structure_slot_writer_list(addr data, addr symbol)
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

static int structure_slot_readonly_p(addr slot)
{
	GetReadOnlySlot(slot, &slot);
	return slot != Nil;
}

static int structure_slots_call_list_(struct defstruct *str)
{
	addr package, type, slots, pos, symbol;
	size_t size, i;

	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_slot_callname_(str, &symbol, pos));
		Return(intern_package_(package, symbol, &symbol, NULL));
		structure_type(str, pos, &type);
		Return(parse_callname_error_(&symbol, symbol));
		structure_slot_reader_list(type, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_list(type, symbol);
	}

	return 0;
}

/* vector */
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

static void structure_slot_reader_vector(addr data, addr symbol)
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

static void structure_slot_writer_vector(addr data, addr symbol)
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

static int structure_slots_call_vector_(struct defstruct *str)
{
	addr package, type, slots, pos, symbol;
	size_t size, i;

	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_slot_callname_(str, &symbol, pos));
		Return(intern_package_(package, symbol, &symbol, NULL));
		structure_type(str, pos, &type);
		Return(parse_callname_error_(&symbol, symbol));
		structure_slot_reader_vector(type, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_vector(type, symbol);
	}

	return 0;
}

/* clos */
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
		return fmte_("The reader don't read ~S structure.", pos, NULL);
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

static void structure_slot_reader_clos(addr instance, addr slot, addr symbol)
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
		return fmte_("The reader don't read ~S structure.", pos, NULL);
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

static void structure_slot_writer_clos(addr instance, addr slot, addr symbol)
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

static int structure_slots_call_clos_(struct defstruct *str)
{
	addr instance, package, slots, pos, symbol;
	size_t size, i;

	instance = str->instance;
	Check(! structure_class_p_debug(instance), "type error");
	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_slot_callname_(str, &symbol, pos));
		Return(intern_package_(package, symbol, &symbol, NULL));
		Return(parse_callname_error_(&symbol, symbol));
		structure_slot_reader_clos(instance, pos, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_clos(instance, pos, symbol);
	}

	return 0;
}

/* call */
static int structure_slots_call_(struct defstruct *str)
{
	if (str->type_list_p)
		return structure_slots_call_list_(str);
	else if (str->type_vector_p)
		return structure_slots_call_vector_(str);
	else
		return structure_slots_call_clos_(str);
}


/*
 *  constructor
 */
static int structure_constructor_find_slots_(addr key, addr slots, int *ret)
{
	int check;
	addr value;
	size_t size, i;

	Check(! symbolp(key), "type error");
	Check(! slot_vector_p(slots), "type error");
	GetNameSymbol(key, &key);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &value);
		GetNameSlot(value, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(key, value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int structure_constructor_dynamic_p_(
		int errorp, addr key, addr slots, int *ret)
{
	int check;

	if (! errorp)
		return Result(ret, 0);
	Return(structure_constructor_find_slots_(key, slots, &check));
	return Result(ret, ! check);
}

static int structure_constructor_dynamic_(addr instance,
		addr slots, addr list, int errorp)
{
	int check;
	addr key;

	while (list != Nil) {
		if (! consp(list))
			return fmte_("Invalid keyword-argumets ~S.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(list))
			return fmte_("There is no value in the key ~S arguemnts.", key, NULL);
		if (! symbolp(key))
			return fmte_("The key ~S must be a symbol type.", key, NULL);
		Return(structure_constructor_dynamic_p_(errorp, key, slots, &check));
		if (check) {
			return fmte_("There is no slot ~S "
					"in the structure ~S.", key, instance, NULL);
		}
		GetCdr(list, &list);
	}

	return 0;
}

static int function_structure_constructor_find_(
		addr key, addr list, addr *value, int *ret)
{
	int check;
	addr left, right, g;

	Check(! slotp(key), "type error");
	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	GetNameSlot(key, &key);
	GetNameSymbol(key, &key);
	while (list != Nil) {
		GetCons(list, &left, &list);
		GetCons(list, &right, &list);
		Check(! symbolp(left), "type error");
		if (right == g)
			continue;
		GetNameSymbol(left, &left);
		Return(string_equal_(key, left, &check));
		if (check) {
			*value = right;
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

/* list */
static int structure_constructor_instance_list_(Execute ptr,
		addr list, addr slots, addr args)
{
	int check;
	addr slot, pos;
	size_t size, i, index;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetAccessSlot(slot, &index);
		Return(function_structure_constructor_find_(slot, args, &pos, &check));
		if (! check) {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				Return(callclang_apply(ptr, &pos, pos, Nil));
			}
		}
		setnth_unsafe(list, index, pos);
	}

	return 0;
}

static void make_structure_nil(addr *ret, size_t size)
{
	addr list;
	size_t i;

	list = Nil;
	for (i = 0; i < size; i++)
		cons_heap(&list, Nil, list);
	*ret = list;
}

int make_structure_list_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	addr instance, slots, list, name;
	struct structure_type_struct *str;
	LocalHold hold;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	/* make */
	Return(structure_constructor_dynamic_(instance, slots, args, str->errorp));
	make_structure_nil(&list, str->size_value);

	if (initp) {
		hold = LocalHold_local_push(ptr, list);
		Return(structure_constructor_instance_list_(ptr, list, slots, args));
		localhold_end(hold);
	}

	if (str->named) {
		GetNameStructureType(pos, &name);
		Return(setnth_(list, str->named_index, name));
	}

	return Result(ret, list);
}

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

static void structure_constructor_default_list(addr data, addr symbol)
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


/* vector */
static int structure_constructor_instance_vector_(Execute ptr,
		addr vector, addr slots, addr value, addr args)
{
	int update, check;
	addr slot, pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		update = 0;
		Return(function_structure_constructor_find_(slot, args, &pos, &check));
		if (check) {
			update = 1;
		}
		else {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				Return(callclang_apply(ptr, &pos, pos, Nil));
				update = 1;
			}
		}
		if (update) {
			Return(structure_setarray_(ptr, vector, slot, value, pos));
		}
	}

	return 0;
}

int make_structure_vector_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	addr instance, slots, vector, type, name;
	struct structure_type_struct *str;
	LocalHold hold;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetVectorStructureType(pos, &type);
	/* make */
	Return(structure_constructor_dynamic_(instance, slots, args, str->errorp));
	vector_heap(&vector, str->size_value);

	if (initp) {
		hold = LocalHold_local_push(ptr, vector);
		Return(structure_constructor_instance_vector_(ptr, vector, slots, type, args));
		localhold_end(hold);
	}

	if (str->named) {
		GetNameStructureType(pos, &name);
		Return(setelt_sequence_(vector, str->named_index, name));
	}

	return Result(ret, vector);
}

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

static void structure_constructor_default_vector(addr data, addr symbol)
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

/* clos */
static int structure_constructor_instance_clos_(
		Execute ptr, addr clos, addr args, int initp)
{
	int check;
	addr slots, slot, value, pos;
	size_t size, i, location;

	GetSlotClos(clos, &slots);
	GetValueClos(clos, &value);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetLocationSlot(slot, &location);
		if (! initp) {
			SetClosValue(value, location, Nil);
			continue;
		}
		Return(function_structure_constructor_find_(slot, args, &pos, &check));
		if (! check) {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				Return(callclang_apply(ptr, &pos, pos, Nil));
			}
		}
		SetClosValue(value, location, pos);
	}

	return 0;
}

int make_structure_clos_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	int errorp;
	addr instance, slots, clos;
	LocalHold hold;

	/* variables */
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetErrorpStructureType(pos, &errorp);
	Return(structure_constructor_dynamic_(instance, slots, args, errorp));
	/* make */
	clos_heap(&clos, slots);
	SetClassOfClos(clos, instance);

	hold = LocalHold_local_push(ptr, clos);
	Return(structure_constructor_instance_clos_(ptr, clos, args, initp));
	localhold_end(hold);

	return Result(ret, clos);
}

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

static void structure_constructor_default_clos(addr data, addr symbol)
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

/* default */
static int structure_constructor_default_(struct defstruct *str, addr symbol)
{
	addr pos;

	Return(parse_callname_error_(&symbol, symbol));
	structure_type(str, str->slots, &pos);
	if (str->type_list_p)
		structure_constructor_default_list(pos, symbol);
	else if (str->type_vector_p)
		structure_constructor_default_vector(pos, symbol);
	else
		structure_constructor_default_clos(pos, symbol);

	return 0;
}

/* constructor */
static int structure_constructor_make_(struct defstruct *str)
{
	addr name;

	/* name */
	Return(stdget_structure_name_(str->instance, &name));
	GetNameSymbol(name, &name);
	Return(string_concat_char1_heap_(&name, "MAKE-", name));
	Return(intern_default_package_(str->ptr, name, &name, NULL));
	/* make */
	return structure_constructor_default_(str, name);
}

static int structure_constructor_lambda_(addr list)
{
	addr symbol, name, pos, type;

	/* (symbol function) */
	Return(list_bind_(list, &symbol, &pos, NULL));
	Return(parse_callname_error_(&name, symbol));
	SetNameFunction(pos, name);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, Function);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);

	return 0;
}

static int structure_constructor_(struct defstruct *str)
{
	addr list, pos, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = str->constructor; list != Nil; ) {
		GetCons(list, &pos, &list);
		if (pos == g) {
			Return(structure_constructor_make_(str));
		}
		else if (symbolp(pos)) {
			Return(structure_constructor_default_(str, pos));
		}
		else if (consp(pos)) {
			Return(structure_constructor_lambda_(pos));
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
/* list */
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

static void structure_copier_list(struct defstruct *str, addr symbol)
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

/* vector */
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

static void structure_copier_vector(struct defstruct *str, addr symbol)
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

/* clos */
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

static void structure_copier_clos(addr instance, addr symbol)
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

/* copier */
static int structure_copier_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->copier_p) {
		Return(stdget_structure_name_(str->instance, &name));
		GetNameSymbol(name, &name);
		Return(string_concat_char1_heap_(&name, "COPY-", name));
	}
	else if (str->copier == Nil) {
		return Result(ret, Unbound);
	}
	else {
		Check(! stringp(str->copier), "type error");
		name = str->copier;
	}
	return intern_default_package_(str->ptr, name, ret, NULL);
}

static int structure_copier_(struct defstruct *str)
{
	addr symbol;

	Return(structure_copier_callname_(str, &symbol));
	if (symbol == Unbound)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_list_p)
		structure_copier_list(str, symbol);
	else if (str->type_vector_p)
		structure_copier_vector(str, symbol);
	else
		structure_copier_clos(str->instance, symbol);

	return 0;
}


/*
 *  predicate
 */
/* list */
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

static void structure_predicate_list(struct defstruct *str, addr symbol)
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

/* vector */
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

static void structure_predicate_vector(struct defstruct *str, addr symbol)
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

/* clos */
static int function_structure_predicate_clos(Execute ptr, addr var)
{
	int check;
	addr instance;

	getdata_control(ptr, &instance);
	Return(typep_structure_(var, instance, &check));
	setbool_control(ptr, check);

	return 0;
}

static void structure_predicate_clos(addr instance, addr symbol)
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

/* predicate */
static int structure_predicate_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->predicate_p) {
		Return(stdget_structure_name_(str->instance, &name));
		GetNameSymbol(name, &name);
		Return(string_concat_char2_heap_(&name, name, "-P"));
	}
	else if (str->predicate == Nil) {
		return Result(ret, Unbound);
	}
	else {
		Check(! stringp(str->predicate), "type error");
		name = str->predicate;
	}
	return intern_default_package_(str->ptr, name, ret, NULL);
}

static int structure_predicate_(struct defstruct *str)
{
	addr symbol;

	Return(structure_predicate_callname_(str, &symbol));
	if (symbol == Unbound)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_list_p)
		structure_predicate_list(str, symbol);
	else if (str->type_vector_p)
		structure_predicate_vector(str, symbol);
	else
		structure_predicate_clos(str->instance, symbol);

	return 0;
}


/*
 *  printer
 */
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

static int structure_print_default_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_default_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}

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

static int structure_print_object_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_object_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}

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

static int structure_print_function_(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	Return(parse_callname_error_(&name, name));
	Return(structure_print_function_method_(str, name, &method));
	return structure_print_add_method_(str, name, method);
}

static int structure_print_(struct defstruct *str)
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


/*
 *  ensure-structure
 */
int ensure_structure_common_(Execute ptr, addr name, addr slots, addr args)
{
	struct defstruct str;
	LocalHold hold;

	Check(! symbolp(name), "type error");
	Check(! listp(slots), "type error");
	Return(ensure_structure_struct_(&str, ptr, name, slots, args));

	hold = LocalHold_local(ptr);
	localhold_destruct(hold, &str);

	/* check */
	Return(structure_check_name_(&str));
	Return(structure_slots_heap_(str.slots, &(str.slots)));
	Return(structure_slots_heap_(str.iargs, &(str.iargs)));
	Return(structure_check_slots_(str.slots));
	Return(structure_check_slots_(str.iargs));
	Return(structure_check_predicate_(&str));
	Return(structure_include_(&str));
	Return(structure_include_slots_(&str));
	Return(structure_include_arguments_(&str));
	Return(structure_print_check_(&str));
	structure_slots_value(&str);
	/* make instance */
	Return(structure_instance_(&str));
	Check(! structure_class_p_debug(str.instance), "type error");
	clos_define_class(str.name, str.instance);
	/* settings */
	Return(structure_slots_make_(&str));
	Return(structure_slots_call_(&str));
	Return(structure_copier_(&str));
	Return(structure_predicate_(&str));
	Return(structure_constructor_(&str));
	Return(structure_print_(&str));
	/* rollback */
	localhold_end(hold);

	return 0;
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

