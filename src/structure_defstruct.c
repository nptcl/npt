#include "hold.h"
#include "structure_defstruct.h"
#include "typedef.h"

void localhold_destruct(LocalHold hold, struct defstruct *str)
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
	str->conc_name = Unbound;
	str->copier = Nil;
	str->predicate = Nil;
	str->iname = Nil;
	str->iargs = Nil;
	str->constructor = Nil;
	str->type_vector = Unbound;
	str->print_function = Unbound;
	str->print_object = Unbound;
	str->size = 0;
	str->offset = 0;
	str->named_index = 0;
	str->change = Nil;
}

