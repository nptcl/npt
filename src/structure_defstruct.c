#include "hold.h"
#include "structure_defstruct.h"
#include "typedef.h"

void localhold_defstruct(struct defstruct *str, LocalHold hold)
{
	localhold_pushva_force(hold, str->instance, str->env, str->doc, str->slots,
			str->name, str->conc_name, str->copier, str->predicate,
			str->constructor, str->iname, str->iargs,
			str->print_object, str->print_function,
			str->type_vector, str->initial_offset, NULL);
}

void defstruct_clean(struct defstruct *str)
{
	clearpoint(str);
	str->instance = Unbound;
	str->env = Unbound;
	str->doc = Unbound;
	str->slots = Unbound;
	str->name = Unbound;
	str->conc_name = Unbound;
	str->copier = Nil;
	str->predicate = Nil;
	str->constructor = Nil;
	str->iname = Nil;
	str->iargs = Nil;
	str->print_object = Unbound;
	str->print_function = Unbound;
	str->type_vector = Unbound;
	str->initial_offset = Unbound;
	str->change = Nil;
}

