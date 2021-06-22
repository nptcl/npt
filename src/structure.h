#ifndef __STRUCTURE_HEADER__
#define __STRUCTURE_HEADER__

#include "execute.h"
#include "hold.h"
#include "typedef.h"

#define structure_class_p_ _n(structure_class_p_)
#define structure_class_p_debug _n(structure_class_p_debug)
#define structure_instance_p_ _n(structure_instance_p_)
#define structure_instance_p_debug _n(structure_instance_p_debug)
#define equalp_structure_ _n(equalp_structure_)
#define equalrt_structure_ _n(equalrt_structure_)
#define typep_structure_ _n(typep_structure_)

#define structure_constructor_common _n(structure_constructor_common)
#define allocate_instance_structure_ _n(allocate_instance_structure_)
#define make_instance_structure _n(make_instance_structure)
#define copy_structure_common _n(copy_structure_common)
#define init_structure _n(init_structure)

int structure_class_p_(addr pos, int *ret);
int structure_class_p_debug(addr pos);
int structure_instance_p_(addr pos, int *ret);
int equalp_structure_(addr a, addr b, int *ret);
int equalrt_structure_(addr a, addr b, int *ret);
int typep_structure_(addr value, addr instance, int *ret);

int structure_constructor_common(Execute ptr, addr symbol, addr rest, addr *ret);
int allocate_instance_structure_(Execute ptr, addr clos, addr *ret);
int make_instance_structure(Execute ptr, addr rest, addr *ret);
void copy_structure_common(addr var, addr *ret);
void init_structure(void);

#endif

