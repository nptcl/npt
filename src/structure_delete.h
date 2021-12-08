#ifndef __STRUCTURE_DELETE_HEADER__
#define __STRUCTURE_DELETE_HEADER__

#include "execute.h"
#include "typedef.h"

#define structure_delete1_call_ _n(structure_delete1_call_)
#define structure_delete2_call_ _n(structure_delete2_call_)
#define structure_delete3_call_ _n(structure_delete3_call_)
#define structure_delete1_copier_ _n(structure_delete1_copier_)
#define structure_delete2_copier_ _n(structure_delete2_copier_)
#define structure_delete3_copier_ _n(structure_delete3_copier_)
#define structure_delete1_predicate_ _n(structure_delete1_predicate_)
#define structure_delete2_predicate_ _n(structure_delete2_predicate_)
#define structure_delete3_predicate_ _n(structure_delete3_predicate_)
#define structure_delete1_constructor_ _n(structure_delete1_constructor_)
#define structure_delete2_constructor_ _n(structure_delete2_constructor_)
#define structure_delete3_constructor_ _n(structure_delete3_constructor_)
#define structure_delete1_print_ _n(structure_delete1_print_)
#define structure_delete_ _n(structure_delete_)

int structure_delete1_call_(addr instance);
int structure_delete2_call_(addr instance);
int structure_delete3_call_(addr instance);
int structure_delete1_copier_(addr instance);
int structure_delete2_copier_(addr instance);
int structure_delete3_copier_(addr instance);
int structure_delete1_predicate_(addr instance);
int structure_delete2_predicate_(addr instance);
int structure_delete3_predicate_(addr instance);
int structure_delete1_constructor_(addr instance);
int structure_delete2_constructor_(addr instance);
int structure_delete3_constructor_(addr instance);
int structure_delete1_print_(Execute ptr, addr instance);
int structure_delete_(Execute ptr, addr name, int *ret);

#endif

