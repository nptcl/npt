#ifndef __SUBTYPEP_ANDOR_HEADER__
#define __SUBTYPEP_ANDOR_HEADER__

#include "execute.h"
#include "subtypep_typedef.h"
#include "typedef.h"

#define subtypep_and_and_ _n(subtypep_and_and_)
#define subtypep_and_or_ _n(subtypep_and_or_)
#define subtypep_or_and_ _n(subtypep_or_and_)
#define subtypep_or_or_ _n(subtypep_or_or_)
#define subtypep_and_type_ _n(subtypep_and_type_)
#define subtypep_or_type_ _n(subtypep_or_type_)
#define subtypep_type_and_ _n(subtypep_type_and_)
#define subtypep_type_or_ _n(subtypep_type_or_)

int subtypep_and_and_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_and_or_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_or_and_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_or_or_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_and_type_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_or_type_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_type_and_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_type_or_(Execute ptr, addr x, addr y, SubtypepResult *ret);

#endif

