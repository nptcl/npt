#include "clos.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "integer.h"
#include "structure.h"
#include "structure_object.h"
#include "structure_parse.h"
#include "strtype.h"
#include "subtypep.h"
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

int ensure_structure_struct_(struct defstruct *str,
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

int structure_arguments_(struct defstruct *str)
{
	Return(structure_slots_heap_(str->slots, &(str->slots)));
	Return(structure_slots_heap_(str->iargs, &(str->iargs)));
	Return(structure_check_slots_(str->slots));
	Return(structure_check_slots_(str->iargs));
	Return(structure_check_predicate_(str));
	Return(structure_include_(str));
	Return(structure_include_slots_(str));
	Return(structure_include_arguments_(str));
	Return(structure_print_check_(str));
	structure_slots_value(str);

	return 0;
}

