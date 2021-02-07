#ifndef __STRUCTURE_TYPEDEF_HEADER__
#define __STRUCTURE_TYPEDEF_HEADER__

#include "execute.h"
#include "typedef.h"

/* defstruct */
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

/* structure-type */
struct structure_type_struct {
	unsigned named : 1;
	unsigned errorp : 1;
	size_t size, size_value, named_index;
};
enum StructureTypeIndex {
	StructureType_instance,
	StructureType_name,
	StructureType_slot,
	StructureType_vector,
	StructureType_size
};

#endif

