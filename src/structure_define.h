#ifndef __STRUCTURE_DEFINE_HEADER__
#define __STRUCTURE_DEFINE_HEADER__

#include "structure_typedef.h"
#include "typedef.h"

#define structure_slot_reader_list _n(structure_slot_reader_list)
#define structure_slot_writer_list _n(structure_slot_writer_list)
#define structure_slot_reader_vector _n(structure_slot_reader_vector)
#define structure_slot_writer_vector _n(structure_slot_writer_vector)
#define structure_slot_reader_clos _n(structure_slot_reader_clos)
#define structure_slot_writer_clos _n(structure_slot_writer_clos)
#define structure_constructor_default_list _n(structure_constructor_default_list)
#define structure_constructor_default_vector _n(structure_constructor_default_vector)
#define structure_constructor_default_clos _n(structure_constructor_default_clos)
#define structure_copier_list _n(structure_copier_list)
#define structure_copier_vector _n(structure_copier_vector)
#define structure_copier_clos _n(structure_copier_clos)
#define structure_predicate_list _n(structure_predicate_list)
#define structure_predicate_vector _n(structure_predicate_vector)
#define structure_predicate_clos _n(structure_predicate_clos)
#define structure_print_default_ _n(structure_print_default_)
#define structure_print_object_ _n(structure_print_object_)
#define structure_print_function_ _n(structure_print_function_)
#define init_structure_define _n(init_structure_define)

void structure_slot_reader_list(addr data, addr symbol);
void structure_slot_writer_list(addr data, addr symbol);
void structure_slot_reader_vector(addr data, addr symbol);
void structure_slot_writer_vector(addr data, addr symbol);
void structure_slot_reader_clos(addr instance, addr slot, addr symbol);
void structure_slot_writer_clos(addr instance, addr slot, addr symbol);
void structure_constructor_default_list(addr data, addr symbol);
void structure_constructor_default_vector(addr data, addr symbol);
void structure_constructor_default_clos(addr data, addr symbol);
void structure_copier_list(struct defstruct *str, addr symbol);
void structure_copier_vector(struct defstruct *str, addr symbol);
void structure_copier_clos(addr instance, addr symbol);
void structure_predicate_list(struct defstruct *str, addr symbol);
void structure_predicate_vector(struct defstruct *str, addr symbol);
void structure_predicate_clos(addr instance, addr symbol);
int structure_print_default_(struct defstruct *str);
int structure_print_object_(struct defstruct *str);
int structure_print_function_(struct defstruct *str);
void init_structure_define(void);

#endif

