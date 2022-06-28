#include "closget_structure.h"
#include "condition.h"
#include "structure.h"
#include "structure_change.h"
#include "structure_define.h"
#include "structure_define1.h"
#include "structure_define2.h"
#include "structure_parse.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  ensure-structure
 */
static int ensure_structure_class_p(struct defstruct *str)
{
	addr pos;

	if (! structure_get_class(str->name, &pos))
		return 0;
	str->instance = pos;
	return 1;
}

static int ensure_structure_object_p(struct defstruct *str)
{
	addr pos;

	if (! structure_get_object(str->name, &pos))
		return 0;
	str->instance = pos;
	return 1;
}

static int ensure_structure_call_(struct defstruct *str)
{
	/* class */
	if (str->type_p == 0) {
		if (ensure_structure_class_p(str))
			return structure_change1_(str);
		else
			return structure_define1_(str);
	}

	/* (:type list) */
	if (str->type_list_p) {
		if (ensure_structure_object_p(str))
			return structure_change2_(str);
		else
			return structure_define2_(str);
	}

	/* (:type vector) */
	if (str->type_vector_p) {
		if (ensure_structure_object_p(str))
			return structure_change3_(str);
		else
			return structure_define3_(str);
	}

	/* error */
	return fmte_("Invalid structure type.", NULL);
}

int ensure_structure_common_(Execute ptr, addr name, addr slots, addr args)
{
	struct defstruct str;
	LocalHold hold;

	Check(! symbolp(name), "type error");
	Check(! listp(slots), "type error");
	Return(ensure_structure_struct_(&str, ptr, name, slots, args));

	hold = LocalHold_array(ptr, 2);
	localhold_defstruct(&str, hold);
	Return(structure_arguments_(&str, hold));
	Return(ensure_structure_call_(&str));
	localhold_end(hold);

	return 0;
}


/*
 *  initialize
 */
void init_structure_define(void)
{
	init_structure_define1();
	init_structure_define2();
	init_structure_define3();
}

