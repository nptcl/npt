#ifndef __STRUCTURE_DEFSTRUCT_HEADER__
#define __STRUCTURE_DEFSTRUCT_HEADER__

#include "execute.h"
#include "hold.h"
#include "typedef.h"

struct defstruct {
	unsigned conc_name_p : 1;
	unsigned constructor_p : 1;
	unsigned copier_p : 1;
	unsigned predicate_p : 1;
	unsigned include_p : 1;
	unsigned print_object_p : 1;
	unsigned print_function_p : 1;
	unsigned type_p : 1;
	unsigned type_list_p : 1;
	unsigned type_vector_p : 1;
	unsigned named_p : 1;
	unsigned initial_offset_p : 1;
	enum ARRAY_TYPE type1;
	int type2;
	Execute ptr;
	addr instance, env, doc, slots;
	addr name, conc_name, copier, predicate;
	addr constructor, iname, iargs;
	addr print_object, print_function;
	addr type_vector, initial_offset;
	addr change;
	size_t size, size_all, offset, named_index;
};

#define localhold_defstruct _n(localhold_defstruct)
#define defstruct_clean _n(defstruct_clean)

void localhold_defstruct(struct defstruct *str, LocalHold hold);
void defstruct_clean(struct defstruct *ptr);

#endif

