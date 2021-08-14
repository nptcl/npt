#include "clos.h"
#include "clos_method.h"
#include "condition.h"
#include "control_object.h"
#include "structure.h"
#include "structure_change.h"
#include "structure_make.h"
#include "structure_object.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  check
 */
static int structure_change_include_(struct defstruct *str)
{
	addr x, y;

	Return(stdget_structure_include_(str->instance, &x));
	Return(stdget_structure_include_(str->change, &y));
	if (x == y)
		return 0;

	return fmte_(":INCLUDE value ~S must be a ~S.", x, y, NULL);
}

static int structure_change_slots_(struct defstruct *str)
{
	addr slots1, slots2, x, y, list;
	size_t size, i;

	Return(structure_make_slots_(str));
	Return(stdget_structure_direct_slots_(str->instance, &slots1));
	Return(stdget_structure_direct_slots_(str->change, &slots2));

	/* length */
	LenSlotVector(slots1, &size);
	LenSlotVector(slots2, &i);
	if (size != i)
		goto error;

	/* slots */
	for (i = 0; i < size; i++) {
		GetSlotVector(slots1, i, &x);
		GetSlotVector(slots2, i, &y);
		GetNameSlot(x, &x);
		GetNameSlot(y, &y);
		if (x != y)
			goto error;
	}
	return 0;

error:
	list = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(slots1, i, &x);
		GetNameSlot(x, &x);
		cons_heap(&list, x, list);
	}
	return fmte_("Cannot change slots ~S in ~S.", list, str->instance, NULL);
}

static int structure_change_call_(struct defstruct *str)
{
	addr list, pos;

	/* fmakunbound */
	Return(stdget_structure_access_(str->change, &list));
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(pos, &pos);
		SetFunctionSymbol(pos, Unbound);
		remsetf_symbol(pos);
	}

	/* make function */
	return structure_make_call_(str);
}

static int structure_change_copier_(struct defstruct *str)
{
	addr symbol;

	/* fmakunbound */
	Return(stdget_structure_copier_(str->change, &symbol));
	if (symbol != Nil) {
		SetFunctionSymbol(symbol, Unbound);
	}

	/* make function */
	return structure_make_copier_(str);
}

static int structure_change_predicate_(struct defstruct *str)
{
	addr symbol;

	/* delete */
	Return(stdget_structure_predicate_(str->change, &symbol));
	if (symbol != Nil) {
		SetFunctionSymbol(symbol, Unbound);
	}

	/* make */
	return structure_make_predicate_(str);
}

static int structure_change_constructor_(struct defstruct *str)
{
	addr list, symbol;

	/* delete */
	Return(stdget_structure_constructor_(str->change, &list));
	while (list != Nil) {
		GetCons(list, &symbol, &list);
		SetFunctionSymbol(symbol, Unbound);
	}

	/* make */
	return structure_make_constructor_(str);
}


/*
 *  interface
 */
static int structure_change_instance_(struct defstruct *str)
{
	addr instance, change, list;

	/* swap */
	instance = str->instance;
	Return(structure_instance_(str));
	change = str->instance;
	str->instance = instance;
	str->change = change;
	Check(! structure_class_p_debug(instance), "type error");

	/* swap */
	clos_swap(instance, change);

	/* precedence-list */
	Return(stdget_structure_precedence_list_(change, &list));
	Return(stdset_structure_precedence_list_(instance, list));

	return 0;
}

static int structure_change_print_(struct defstruct *str)
{
	addr gen, method;

	/* remove method */
	GetConst(COMMON_PRINT_OBJECT, &gen);
	GetFunctionSymbol(gen, &gen);
	Return(stdget_structure_print_(str->change, &method));
	if (method != Nil) {
		Return(method_remove_method_(str->ptr, gen, method));
		Return(stdset_structure_print_(str->change, Nil));
	}

	/* make method */
	return structure_make_print_(str);
}

static int structure_change_execute_(struct defstruct *str)
{
	Return(structure_change_instance_(str));
	Return(structure_change_include_(str));
	Return(structure_change_slots_(str));
	Return(structure_change_call_(str));
	Return(structure_change_copier_(str));
	Return(structure_change_predicate_(str));
	Return(structure_change_constructor_(str));
	Return(structure_change_print_(str));

	return 0;
}

int structure_change_(struct defstruct *str)
{
	Execute ptr;
	addr control;

	ptr = str->ptr;
	push_control(ptr, &control);
	(void)structure_change_execute_(str);
	return pop_control_(ptr, control);
}

