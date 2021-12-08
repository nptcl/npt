#ifndef __STRUCTURE_DEFINE2_HEADER__
#define __STRUCTURE_DEFINE2_HEADER__

#include "structure_defstruct.h"
#include "typedef.h"

#define structure_instance2 _n(structure_instance2)
#define structure_define2_slots_ _n(structure_define2_slots_)
#define structure_define3_slots_ _n(structure_define3_slots_)
#define structure_define2_call_ _n(structure_define2_call_)
#define structure_define3_call_ _n(structure_define3_call_)
#define structure_define2_constructor_ _n(structure_define2_constructor_)
#define structure_define3_constructor_ _n(structure_define3_constructor_)
#define structure_define2_copier_ _n(structure_define2_copier_)
#define structure_define3_copier_ _n(structure_define3_copier_)
#define structure_define2_predicate_ _n(structure_define2_predicate_)
#define structure_define3_predicate_ _n(structure_define3_predicate_)
#define structure_define2_print_ _n(structure_define2_print_)
#define structure_define3_print_ _n(structure_define3_print_)
#define structure_define2_ _n(structure_define2_)
#define structure_define3_ _n(structure_define3_)
#define init_structure_define2 _n(init_structure_define2)
#define init_structure_define3 _n(init_structure_define3)

void structure_instance2(struct defstruct *str);
int structure_define2_slots_(struct defstruct *str);
int structure_define3_slots_(struct defstruct *str);
int structure_define2_call_(struct defstruct *str);
int structure_define3_call_(struct defstruct *str);
int structure_define2_constructor_(struct defstruct *str);
int structure_define3_constructor_(struct defstruct *str);
int structure_define2_copier_(struct defstruct *str);
int structure_define3_copier_(struct defstruct *str);
int structure_define2_predicate_(struct defstruct *str);
int structure_define3_predicate_(struct defstruct *str);
int structure_define2_print_(struct defstruct *str);
int structure_define3_print_(struct defstruct *str);
int structure_define2_(struct defstruct *str);
int structure_define3_(struct defstruct *str);
void init_structure_define2(void);
void init_structure_define3(void);

#endif

