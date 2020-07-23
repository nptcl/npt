#ifndef __STRUCTURE_HEADER__
#define __STRUCTURE_HEADER__

#include "execute.h"
#include "hold.h"
#include "typedef.h"

/*
 *  structure
 */
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
	Execute ptr;
	addr instance, env, doc, slots;
	addr name, conc_name, copier, predicate;
	addr constructor, iname, iargs;
	addr print_object, print_function;
	addr type_vector, initial_offset;
	size_t size, offset, size_value, named_index;
};

_g void localhold_destruct(LocalHold hold, struct defstruct *str);
_g void defstruct_clean(struct defstruct *ptr);

/* access */
_g void stdget_structure_name(addr pos, addr *ret);
_g void stdset_structure_name(addr pos, addr value);
_g void stdget_structure_slots(addr pos, addr *ret);
_g void stdset_structure_slots(addr pos, addr value);
_g void stdget_structure_documentation(addr pos, addr *ret);
_g void stdset_structure_documentation(addr pos, addr value);
_g void stdget_structure_include(addr pos, addr *ret);
_g void stdset_structure_include(addr pos, addr value);
_g void stdget_structure_precedence_list(addr pos, addr *ret);
_g void stdset_structure_precedence_list(addr pos, addr value);
_g void stdget_structure_type(addr pos, addr *ret);
_g void stdset_structure_type(addr pos, addr value);
_g void stdget_structure_vector(addr pos, addr *ret);
_g void stdset_structure_vector(addr pos, addr value);
_g void stdget_structure_named(addr pos, addr *ret);
_g void stdset_structure_named(addr pos, addr value);
_g void stdget_structure_named_index(addr pos, addr *ret);
_g void stdset_structure_named_index(addr pos, addr value);
_g void stdget_structure_value(addr pos, addr *ret);
_g void stdset_structure_value(addr pos, addr value);

/* structure */
_g int structure_class_p(addr pos);
_g int structure_instance_p(addr pos);
_g int equalp_structure_(addr a, addr b, int *ret);
_g int equalrt_structure_(addr a, addr b, int *ret);
_g int ensure_structure_common_(Execute ptr, addr name, addr slots, addr rest);
_g int structure_constructor_common(Execute ptr, addr symbol, addr rest, addr *ret);
_g int make_instance_structure(Execute ptr, addr rest, addr *ret);
_g void copy_structure_common(addr var, addr *ret);
_g void init_structure(void);

#endif

