#ifndef __STRUCTURE_ACCESS_HEADER__
#define __STRUCTURE_ACCESS_HEADER__

#include "typedef.h"

#define stdget_structure_name_ _n(stdget_structure_name_)
#define stdset_structure_name_ _n(stdset_structure_name_)
#define stdget_structure_direct_slots_ _n(stdget_structure_direct_slots_)
#define stdset_structure_direct_slots_ _n(stdset_structure_direct_slots_)
#define stdget_structure_slots_ _n(stdget_structure_slots_)
#define stdset_structure_slots_ _n(stdset_structure_slots_)
#define stdget_structure_documentation_ _n(stdget_structure_documentation_)
#define stdset_structure_documentation_ _n(stdset_structure_documentation_)
#define stdget_structure_include_ _n(stdget_structure_include_)
#define stdset_structure_include_ _n(stdset_structure_include_)
#define stdget_structure_precedence_list_ _n(stdget_structure_precedence_list_)
#define stdset_structure_precedence_list_ _n(stdset_structure_precedence_list_)
#define stdget_structure_value_ _n(stdget_structure_value_)
#define stdset_structure_value_ _n(stdset_structure_value_)
#define stdget_structure_predicate_ _n(stdget_structure_predicate_)
#define stdset_structure_predicate_ _n(stdset_structure_predicate_)
#define stdget_structure_access_ _n(stdget_structure_access_)
#define stdset_structure_access_ _n(stdset_structure_access_)
#define stdget_structure_copier_ _n(stdget_structure_copier_)
#define stdset_structure_copier_ _n(stdset_structure_copier_)
#define stdget_structure_constructor_ _n(stdget_structure_constructor_)
#define stdset_structure_constructor_ _n(stdset_structure_constructor_)

int stdget_structure_name_(addr pos, addr *ret);
int stdset_structure_name_(addr pos, addr value);
int stdget_structure_direct_slots_(addr pos, addr *ret);
int stdset_structure_direct_slots_(addr pos, addr value);
int stdget_structure_slots_(addr pos, addr *ret);
int stdset_structure_slots_(addr pos, addr value);
int stdget_structure_documentation_(addr pos, addr *ret);
int stdset_structure_documentation_(addr pos, addr value);
int stdget_structure_include_(addr pos, addr *ret);
int stdset_structure_include_(addr pos, addr value);
int stdget_structure_precedence_list_(addr pos, addr *ret);
int stdset_structure_precedence_list_(addr pos, addr value);
int stdget_structure_value_(addr pos, addr *ret);
int stdset_structure_value_(addr pos, addr value);
int stdget_structure_predicate_(addr pos, addr *ret);
int stdset_structure_predicate_(addr pos, addr value);
int stdget_structure_access_(addr pos, addr *ret);
int stdset_structure_access_(addr pos, addr value);
int stdget_structure_copier_(addr pos, addr *ret);
int stdset_structure_copier_(addr pos, addr value);
int stdget_structure_constructor_(addr pos, addr *ret);
int stdset_structure_constructor_(addr pos, addr value);

#endif

