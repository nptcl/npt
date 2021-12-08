#ifndef __STRUCTURE_HEADER__
#define __STRUCTURE_HEADER__

#include "execute.h"
#include "typedef.h"

#define structure_class_p_ _n(structure_class_p_)
#define structure_class_p_debug _n(structure_class_p_debug)
#define structure_class_object_p_debug _n(structure_class_object_p_debug)
#define structure_instance_p_ _n(structure_instance_p_)
#define structure_instance_p_debug _n(structure_instance_p_debug)
#define equalp_structure_ _n(equalp_structure_)
#define equalrt_structure_ _n(equalrt_structure_)
#define typep_structure_ _n(typep_structure_)
#define structure_get_object _n(structure_get_object)
#define structure_get_class _n(structure_get_class)
#define structure_get _n(structure_get)
#define structure_get_type_ _n(structure_get_type_)
#define getdoc_structure_ _n(getdoc_structure_)
#define setdoc_structure_ _n(setdoc_structure_)

#define structure_reader_ _n(structure_reader_)
#define structure_constructor_common_ _n(structure_constructor_common_)
#define allocate_instance_structure_ _n(allocate_instance_structure_)
#define make_instance_structure_ _n(make_instance_structure_)
#define copy_structure_common _n(copy_structure_common)
#define init_structure _n(init_structure)

int structure_class_p_(addr pos, int *ret);
int structure_class_p_debug(addr pos);
int structure_class_object_p_debug(addr pos);
int structure_instance_p_(addr pos, int *ret);
int equalp_structure_(addr a, addr b, int *ret);
int equalrt_structure_(addr a, addr b, int *ret);
int typep_structure_(addr value, addr instance, int *ret);
int structure_get_object(addr pos, addr *ret);
int structure_get_class(addr pos, addr *ret);
int structure_get(addr pos, addr *ret);
int structure_get_type_(addr pos, addr *ret);
int getdoc_structure_(addr symbol, addr *ret);
int setdoc_structure_(addr symbol, addr value);

int structure_reader_(Execute ptr, addr symbol, addr rest, addr *ret);
int structure_constructor_common_(Execute ptr, addr symbol, addr rest, addr *ret);
int allocate_instance_structure_(Execute ptr, addr clos, addr *ret);
int make_instance_structure_(Execute ptr, addr rest, addr *ret);
void copy_structure_common(addr var, addr *ret);
void init_structure(void);

#endif

