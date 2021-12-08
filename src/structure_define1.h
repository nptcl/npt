#ifndef __STRUCTURE_DEFINE1_HEADER__
#define __STRUCTURE_DEFINE1_HEADER__

#include "structure_defstruct.h"
#include "typedef.h"

#define structure_instance1_ _n(structure_instance1_)
#define structure_define1_slots_ _n(structure_define1_slots_)
#define structure_define1_call_ _n(structure_define1_call_)
#define structure_define1_copier_ _n(structure_define1_copier_)
#define structure_define1_predicate_ _n(structure_define1_predicate_)
#define structure_define1_constructor_ _n(structure_define1_constructor_)
#define structure_define1_print_ _n(structure_define1_print_)
#define structure_define1_ _n(structure_define1_)
#define init_structure_define1 _n(init_structure_define1)

int structure_instance1_(struct defstruct *str);
int structure_define1_slots_(struct defstruct *str);
int structure_define1_call_(struct defstruct *str);
int structure_define1_copier_(struct defstruct *str);
int structure_define1_predicate_(struct defstruct *str);
int structure_define1_constructor_(struct defstruct *str);
int structure_define1_print_(struct defstruct *str);
int structure_define1_(struct defstruct *str);
void init_structure_define1(void);

#endif

