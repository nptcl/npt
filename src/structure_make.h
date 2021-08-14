#ifndef __STRUCTURE_MAKE_HEADER__
#define __STRUCTURE_MAKE_HEADER__

#include "execute.h"
#include "structure_typedef.h"
#include "typedef.h"

#define make_structure_list_ _n(make_structure_list_)
#define make_structure_vector_ _n(make_structure_vector_)
#define make_structure_clos_ _n(make_structure_clos_)
#define structure_make_slots_ _n(structure_make_slots_)
#define structure_make_call_ _n(structure_make_call_)
#define structure_make_copier_ _n(structure_make_copier_)
#define structure_make_predicate_ _n(structure_make_predicate_)
#define structure_make_constructor_ _n(structure_make_constructor_)
#define structure_make_print_ _n(structure_make_print_)
#define structure_instance_ _n(structure_instance_)
#define ensure_structure_common_ _n(ensure_structure_common_)

int make_structure_list_(Execute ptr, addr *ret, addr pos, addr args, int initp);
int make_structure_vector_(Execute ptr, addr *ret, addr pos, addr args, int initp);
int make_structure_clos_(Execute ptr, addr *ret, addr pos, addr args, int initp);
int structure_make_slots_(struct defstruct *str);
int structure_make_call_(struct defstruct *str);
int structure_make_copier_(struct defstruct *str);
int structure_make_predicate_(struct defstruct *str);
int structure_make_constructor_(struct defstruct *str);
int structure_make_print_(struct defstruct *str);
int structure_instance_(struct defstruct *str);
int ensure_structure_common_(Execute ptr, addr name, addr slots, addr rest);

#endif

