#include "clos.h"
#include "clos_method.h"
#include "clos_slot.h"
#include "condition.h"
#include "cons.h"
#include "control_object.h"
#include "strtype.h"
#include "structure.h"
#include "structure_access.h"
#include "structure_change.h"
#include "structure_delete.h"
#include "structure_define1.h"
#include "structure_define2.h"
#include "structure_object.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  instance
 */
static int structure_change1_instance_(struct defstruct *str)
{
	addr instance, change, list;

	/* swap */
	instance = str->instance;
	Return(structure_instance1_(str));
	change = str->instance;
	str->instance = instance;
	str->change = change;
	Check(! structure_class_p_debug(change), "type error");
	Check(! structure_class_p_debug(instance), "type error");

	/* swap */
	clos_swap(instance, change);

	/* precedence-list */
	Return(stdget_structure_precedence_list_(change, &list));
	Return(stdset_structure_precedence_list_(instance, list));

	return 0;
}

static void structure_change2_instance(struct defstruct *str)
{
	addr instance, change, list;

	/* swap */
	instance = str->instance;
	structure_instance2(str);
	change = str->instance;
	str->instance = instance;
	str->change = change;
	Check(! structure_object_p(change), "type error");
	Check(! structure_object_p(instance), "type error");

	/* swap */
	structure_swap(instance, change);

	/* precedence-list */
	GetPrecedenceStructure(change, &list);
	SetPrecedenceStructure(instance, list);
}

static void structure_change3_instance(struct defstruct *str)
{
	structure_change2_instance(str);
}


/*
 *  include
 */
static int structure_change1_include_(struct defstruct *str)
{
	addr x, y;

	Return(stdget_structure_include_(str->instance, &x));
	Return(stdget_structure_include_(str->change, &y));
	if (x == y)
		return 0;

	return fmte_(":INCLUDE value ~S must be a ~S.", x, y, NULL);
}

static int structure_change2_include_(struct defstruct *str)
{
	addr x, y;

	GetIncludeStructure(str->instance, &x);
	GetIncludeStructure(str->change, &y);
	if (x == y)
		return 0;

	return fmte_(":INCLUDE value ~S must be a ~S.", x, y, NULL);
}

static int structure_change3_include_(struct defstruct *str)
{
	return structure_change2_include_(str);
}


/*
 *  slots
 */
static int structure_change_slots_(addr instance, addr slots1, addr slots2)
{
	int check;
	addr x, y, list;
	size_t size1, size2, i;

	/* length */
	LenSlotVector(slots1, &size1);
	LenSlotVector(slots2, &size2);
	if (size1 != size2)
		goto error;

	/* slots */
	for (i = 0; i < size1; i++) {
		GetSlotVector(slots1, i, &x);
		GetSlotVector(slots2, i, &y);
		GetNameSlot(x, &x);
		GetNameSlot(y, &y);
		GetNameSymbol(x, &x);
		GetNameSymbol(y, &y);
		Return(string_equal_(x, y, &check));
		if (! check)
			goto error;
	}
	return 0;

error:
	list = Nil;
	for (i = 0; i < size1; i++) {
		GetSlotVector(slots1, i, &x);
		GetNameSlot(x, &x);
		cons_heap(&list, x, list);
	}
	return fmte_("Cannot change slots ~S in ~S.", list, instance, NULL);
}

static int structure_change1_slots_(struct defstruct *str)
{
	addr slots1, slots2;

	Return(structure_define1_slots_(str));
	Return(stdget_structure_direct_slots_(str->instance, &slots1));
	Return(stdget_structure_direct_slots_(str->change, &slots2));

	return structure_change_slots_(str->instance, slots1, slots2);
}

static int structure_change2_slots_(struct defstruct *str)
{
	addr slots1, slots2;

	Return(structure_define2_slots_(str));
	GetSlotsStructure(str->instance, &slots1);
	GetSlotsStructure(str->change, &slots2);

	return structure_change_slots_(str->instance, slots1, slots2);
}

static int structure_change3_slots_(struct defstruct *str)
{
	addr slots1, slots2;

	Return(structure_define3_slots_(str));
	GetSlotsStructure(str->instance, &slots1);
	GetSlotsStructure(str->change, &slots2);

	return structure_change_slots_(str->instance, slots1, slots2);
}


/*
 *  call
 */
static int structure_change1_call_(struct defstruct *str)
{
	Return(structure_delete1_call_(str->change));
	return structure_define1_call_(str);
}

static int structure_change2_call_(struct defstruct *str)
{
	Return(structure_delete2_call_(str->change));
	return structure_define2_call_(str);
}

static int structure_change3_call_(struct defstruct *str)
{
	Return(structure_delete3_call_(str->change));
	return structure_define3_call_(str);
}


/*
 *  copier
 */
static int structure_change1_copier_(struct defstruct *str)
{
	Return(structure_delete1_copier_(str->change));
	return structure_define1_copier_(str);
}

static int structure_change2_copier_(struct defstruct *str)
{
	Return(structure_delete2_copier_(str->change));
	return structure_define2_copier_(str);
}

static int structure_change3_copier_(struct defstruct *str)
{
	Return(structure_delete3_copier_(str->change));
	return structure_define3_copier_(str);
}


/*
 *  predicate
 */
static int structure_change1_predicate_(struct defstruct *str)
{
	Return(structure_delete1_predicate_(str->change));
	return structure_define1_predicate_(str);
}

static int structure_change2_predicate_(struct defstruct *str)
{
	Return(structure_delete2_predicate_(str->change));
	return structure_define2_predicate_(str);
}

static int structure_change3_predicate_(struct defstruct *str)
{
	Return(structure_delete3_predicate_(str->change));
	return structure_define3_predicate_(str);
}


/*
 *  constructor
 */
static int structure_change1_constructor_(struct defstruct *str)
{
	Return(structure_delete1_constructor_(str->change));
	return structure_define1_constructor_(str);
}

static int structure_change2_constructor_(struct defstruct *str)
{
	Return(structure_delete2_constructor_(str->change));
	return structure_define2_constructor_(str);
}

static int structure_change3_constructor_(struct defstruct *str)
{
	Return(structure_delete3_constructor_(str->change));
	return structure_define3_constructor_(str);
}


/*
 *  print-object
 */
static int structure_change1_print_(struct defstruct *str)
{
	Return(structure_delete1_print_(str->ptr, str->instance));
	return structure_define1_print_(str);
}

static int structure_change2_print_(struct defstruct *str)
{
	return structure_define2_print_(str);
}

static int structure_change3_print_(struct defstruct *str)
{
	return structure_define3_print_(str);
}


/*
 *  change
 */
static int structure_change1_execute_(struct defstruct *str)
{
	Return(structure_change1_instance_(str));
	Return(structure_change1_include_(str));
	Return(structure_change1_slots_(str));
	Return(structure_change1_call_(str));
	Return(structure_change1_copier_(str));
	Return(structure_change1_predicate_(str));
	Return(structure_change1_constructor_(str));
	Return(structure_change1_print_(str));

	return 0;
}

static int structure_change2_execute_(struct defstruct *str)
{
	structure_change2_instance(str);
	Return(structure_change2_include_(str));
	Return(structure_change2_slots_(str));
	Return(structure_change2_call_(str));
	Return(structure_change2_copier_(str));
	Return(structure_change2_predicate_(str));
	Return(structure_change2_constructor_(str));
	Return(structure_change2_print_(str));

	return 0;
}

static int structure_change3_execute_(struct defstruct *str)
{
	structure_change3_instance(str);
	Return(structure_change3_include_(str));
	Return(structure_change3_slots_(str));
	Return(structure_change3_call_(str));
	Return(structure_change3_copier_(str));
	Return(structure_change3_predicate_(str));
	Return(structure_change3_constructor_(str));
	Return(structure_change3_print_(str));

	return 0;
}

int structure_change1_(struct defstruct *str)
{
	Execute ptr;
	addr control;

	ptr = str->ptr;
	push_control(ptr, &control);
	(void)structure_change1_execute_(str);
	return pop_control_(ptr, control);
}

int structure_change2_(struct defstruct *str)
{
	Execute ptr;
	addr control;

	ptr = str->ptr;
	push_control(ptr, &control);
	(void)structure_change2_execute_(str);
	return pop_control_(ptr, control);
}

int structure_change3_(struct defstruct *str)
{
	Execute ptr;
	addr control;

	ptr = str->ptr;
	push_control(ptr, &control);
	(void)structure_change3_execute_(str);
	return pop_control_(ptr, control);
}

