#ifndef __CLOSGET_SPECIALIZER_HEADER__
#define __CLOSGET_SPECIALIZER_HEADER__

#include "execute.h"
#include "typedef.h"

#define stdget_specializer_object_ _n(stdget_specializer_object_)
#define stdget_specializer_type_ _n(stdget_specializer_type_)
#define stdset_specializer_object_ _n(stdset_specializer_object_)
#define stdset_specializer_type_ _n(stdset_specializer_type_)
#define stdboundp_specializer_object_ _n(stdboundp_specializer_object_)
#define stdboundp_specializer_type_ _n(stdboundp_specializer_type_)

int stdget_specializer_object_(addr pos, addr *ret);
int stdget_specializer_type_(addr pos, addr *ret);
int stdset_specializer_object_(addr pos, addr value);
int stdset_specializer_type_(addr pos, addr value);
int stdboundp_specializer_object_(addr pos, int *ret);
int stdboundp_specializer_type_(addr pos, int *ret);

#endif

