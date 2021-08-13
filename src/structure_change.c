#include "clos.h"
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
	return fmte_("Cannot change slots ~S in ~S.", list, str->change, NULL);
}

static int structure_change_predicate_(struct defstruct *str)
{
	addr symbol, instance;

	/* delete */
	instance = str->change;
	Return(stdget_structure_predicate_(instance, &symbol));
	if (symbol != Nil) {
		SetFunctionSymbol(symbol, Unbound);
	}

	/* make */
	return structure_make_predicate_(str, instance);
}


/*
 *  interface
 */
static int structure_change_execute_(struct defstruct *str)
{
	/* check */
	Return(structure_change_include_(str));
	Return(structure_change_slots_(str));
	Return(structure_make_call_(str));
	Return(structure_make_copier_(str));
	Return(structure_change_predicate_(str));
	Return(structure_make_constructor_(str));
	Return(structure_make_print_(str));

	return 0;
}

static int structure_change_call_(struct defstruct *str)
{
	/* make instance */
	Return(structure_instance_(str));
	Check(! structure_class_p_debug(str->instance), "type error");

	/* settings */
	Return(structure_change_execute_(str));

	/* swap */
	clos_swap(str->instance, str->change);

	return 0;
}


int structure_change_(struct defstruct *str)
{
	Execute ptr;
	addr control;

	ptr = str->ptr;
	push_control(ptr, &control);
	(void)structure_change_call_(str);
	return pop_control_(ptr, control);
}

