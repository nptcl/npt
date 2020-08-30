#ifndef __STRUCTURE_HEADER__
#define __STRUCTURE_HEADER__

#include "execute.h"
#include "hold.h"
#include "typedef.h"

#define localhold_destruct _n(localhold_destruct)
#define defstruct_clean _n(defstruct_clean)
#define stdget_structure_name_ _n(stdget_structure_name_)
#define stdset_structure_name_ _n(stdset_structure_name_)
#define stdget_structure_slots_ _n(stdget_structure_slots_)
#define stdset_structure_slots_ _n(stdset_structure_slots_)
#define stdget_structure_documentation_ _n(stdget_structure_documentation_)
#define stdset_structure_documentation_ _n(stdset_structure_documentation_)
#define stdget_structure_include_ _n(stdget_structure_include_)
#define stdset_structure_include_ _n(stdset_structure_include_)
#define stdget_structure_precedence_list_ _n(stdget_structure_precedence_list_)
#define stdset_structure_precedence_list_ _n(stdset_structure_precedence_list_)
#define stdget_structure_type_ _n(stdget_structure_type_)
#define stdset_structure_type_ _n(stdset_structure_type_)
#define stdget_structure_vector_ _n(stdget_structure_vector_)
#define stdset_structure_vector_ _n(stdset_structure_vector_)
#define stdget_structure_named_ _n(stdget_structure_named_)
#define stdset_structure_named_ _n(stdset_structure_named_)
#define stdget_structure_named_index_ _n(stdget_structure_named_index_)
#define stdset_structure_named_index_ _n(stdset_structure_named_index_)
#define stdget_structure_value_ _n(stdget_structure_value_)
#define stdset_structure_value_ _n(stdset_structure_value_)
#define structure_class_p_ _n(structure_class_p_)
#define structure_class_p_debug _n(structure_class_p_debug)
#define structure_instance_p_ _n(structure_instance_p_)
#define structure_instance_p_debug _n(structure_instance_p_debug)
#define equalp_structure_ _n(equalp_structure_)
#define equalrt_structure_ _n(equalrt_structure_)
#define ensure_structure_common_ _n(ensure_structure_common_)
#define structure_constructor_common _n(structure_constructor_common)
#define make_instance_structure _n(make_instance_structure)
#define copy_structure_common _n(copy_structure_common)
#define init_structure _n(init_structure)

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
_g int stdget_structure_name_(addr pos, addr *ret);
_g int stdset_structure_name_(addr pos, addr value);
_g int stdget_structure_slots_(addr pos, addr *ret);
_g int stdset_structure_slots_(addr pos, addr value);
_g int stdget_structure_documentation_(addr pos, addr *ret);
_g int stdset_structure_documentation_(addr pos, addr value);
_g int stdget_structure_include_(addr pos, addr *ret);
_g int stdset_structure_include_(addr pos, addr value);
_g int stdget_structure_precedence_list_(addr pos, addr *ret);
_g int stdset_structure_precedence_list_(addr pos, addr value);
_g int stdget_structure_type_(addr pos, addr *ret);
_g int stdset_structure_type_(addr pos, addr value);
_g int stdget_structure_vector_(addr pos, addr *ret);
_g int stdset_structure_vector_(addr pos, addr value);
_g int stdget_structure_named_(addr pos, addr *ret);
_g int stdset_structure_named_(addr pos, addr value);
_g int stdget_structure_named_index_(addr pos, addr *ret);
_g int stdset_structure_named_index_(addr pos, addr value);
_g int stdget_structure_value_(addr pos, addr *ret);
_g int stdset_structure_value_(addr pos, addr value);

/* structure */
_g int structure_class_p_(addr pos, int *ret);
_g int structure_class_p_debug(addr pos);
_g int structure_instance_p_(addr pos, int *ret);
_g int equalp_structure_(addr a, addr b, int *ret);
_g int equalrt_structure_(addr a, addr b, int *ret);
_g int ensure_structure_common_(Execute ptr, addr name, addr slots, addr rest);
_g int structure_constructor_common(Execute ptr, addr symbol, addr rest, addr *ret);
_g int make_instance_structure(Execute ptr, addr rest, addr *ret);
_g void copy_structure_common(addr var, addr *ret);
_g void init_structure(void);

#endif

