#ifndef __CLOSGET_CLASS_HEADER__
#define __CLOSGET_CLASS_HEADER__

#include "execute.h"
#include "typedef.h"

#define stdget_class_name_check _n(stdget_class_name_check)
#define stdget_class_name_ _n(stdget_class_name_)
#define stdget_class_direct_slots_ _n(stdget_class_direct_slots_)
#define stdget_class_direct_subclasses_ _n(stdget_class_direct_subclasses_)
#define stdget_class_direct_superclasses_ _n(stdget_class_direct_superclasses_)
#define stdget_class_precedence_list_ _n(stdget_class_precedence_list_)
#define stdget_class_slots_ _n(stdget_class_slots_)
#define stdget_class_finalized_p_ _n(stdget_class_finalized_p_)
#define stdget_class_prototype_ _n(stdget_class_prototype_)
#define stdget_class_default_initargs_ _n(stdget_class_default_initargs_)
#define stdget_class_direct_default_initargs_ _n(stdget_class_direct_default_initargs_)
#define stdget_class_version_ _n(stdget_class_version_)
#define stdget_class_document_ _n(stdget_class_document_)
#define stdget_class_redefined_class_ _n(stdget_class_redefined_class_)

#define stdset_class_name_ _n(stdset_class_name_)
#define stdset_class_direct_slots_ _n(stdset_class_direct_slots_)
#define stdset_class_direct_subclasses_ _n(stdset_class_direct_subclasses_)
#define stdset_class_direct_superclasses_ _n(stdset_class_direct_superclasses_)
#define stdset_class_precedence_list_ _n(stdset_class_precedence_list_)
#define stdset_class_slots_ _n(stdset_class_slots_)
#define stdset_class_finalized_p_ _n(stdset_class_finalized_p_)
#define stdset_class_prototype_ _n(stdset_class_prototype_)
#define stdset_class_default_initargs_ _n(stdset_class_default_initargs_)
#define stdset_class_direct_default_initargs_ _n(stdset_class_direct_default_initargs_)
#define stdset_class_version_ _n(stdset_class_version_)
#define stdset_class_document_ _n(stdset_class_document_)
#define stdset_class_redefined_class_ _n(stdset_class_redefined_class_)

#define stdboundp_class_name_ _n(stdboundp_class_name_)
#define stdboundp_class_direct_slots_ _n(stdboundp_class_direct_slots_)
#define stdboundp_class_direct_subclasses_ _n(stdboundp_class_direct_subclasses_)
#define stdboundp_class_direct_superclasses_ _n(stdboundp_class_direct_superclasses_)
#define stdboundp_class_precedence_list_ _n(stdboundp_class_precedence_list_)
#define stdboundp_class_slots_ _n(stdboundp_class_slots_)
#define stdboundp_class_finalized_p_ _n(stdboundp_class_finalized_p_)
#define stdboundp_class_prototype_ _n(stdboundp_class_prototype_)
#define stdboundp_class_default_initargs_ _n(stdboundp_class_default_initargs_)
#define stdboundp_class_direct_default_initargs_ _n(stdboundp_class_direct_default_initargs_)
#define stdboundp_class_version_ _n(stdboundp_class_version_)
#define stdboundp_class_document_ _n(stdboundp_class_document_)
#define stdboundp_class_redefined_class_ _n(stdboundp_class_redefined_class_)

void stdget_class_name_check(addr pos, addr *ret);
int stdget_class_name_(addr pos, addr *ret);
int stdget_class_direct_slots_(addr pos, addr *ret);
int stdget_class_direct_subclasses_(addr pos, addr *ret);
int stdget_class_direct_superclasses_(addr pos, addr *ret);
int stdget_class_precedence_list_(addr pos, addr *ret);
int stdget_class_slots_(addr pos, addr *ret);
int stdget_class_finalized_p_(addr pos, addr *ret);
int stdget_class_prototype_(addr pos, addr *ret);
int stdget_class_default_initargs_(addr pos, addr *ret);
int stdget_class_direct_default_initargs_(addr pos, addr *ret);
int stdget_class_version_(addr pos, addr *ret);
int stdget_class_document_(addr pos, addr *ret);
int stdget_class_redefined_class_(addr pos, addr *ret);

int stdset_class_name_(addr pos, addr value);
int stdset_class_direct_slots_(addr pos, addr value);
int stdset_class_direct_subclasses_(addr pos, addr value);
int stdset_class_direct_superclasses_(addr pos, addr value);
int stdset_class_precedence_list_(addr pos, addr value);
int stdset_class_slots_(addr pos, addr value);
int stdset_class_finalized_p_(addr pos, addr value);
int stdset_class_prototype_(addr pos, addr value);
int stdset_class_default_initargs_(addr pos, addr value);
int stdset_class_direct_default_initargs_(addr pos, addr value);
int stdset_class_version_(addr pos, addr value);
int stdset_class_document_(addr pos, addr value);
int stdset_class_redefined_class_(addr pos, addr value);

int stdboundp_class_name_(addr pos, int *ret);
int stdboundp_class_direct_slots_(addr pos, int *ret);
int stdboundp_class_direct_subclasses_(addr pos, int *ret);
int stdboundp_class_direct_superclasses_(addr pos, int *ret);
int stdboundp_class_precedence_list_(addr pos, int *ret);
int stdboundp_class_slots_(addr pos, int *ret);
int stdboundp_class_finalized_p_(addr pos, int *ret);
int stdboundp_class_prototype_(addr pos, int *ret);
int stdboundp_class_default_initargs_(addr pos, int *ret);
int stdboundp_class_direct_default_initargs_(addr pos, int *ret);
int stdboundp_class_version_(addr pos, int *ret);
int stdboundp_class_document_(addr pos, int *ret);
int stdboundp_class_redefined_class_(addr pos, int *ret);

#endif

