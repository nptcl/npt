#ifndef __SUBTYPEP_ANDOR_HEADER__
#define __SUBTYPEP_ANDOR_HEADER__

#include "execute.h"
#include "subtypep_typedef.h"
#include "typedef.h"

#define LISP_DEBUG_SUBTYPEP
#undef LISP_DEBUG_SUBTYPEP

#define subtypep_and_right_ _n(subtypep_and_right_)
#define subtypep_or_right_ _n(subtypep_or_right_)
#define subtypep_and_left_ _n(subtypep_and_left_)
#define subtypep_or_left_ _n(subtypep_or_left_)
#define subtypep_or_right_switch_ _n(subtypep_or_right_switch_)
#define subtypep_and_right_switch_ _n(subtypep_and_right_switch_)

int subtypep_and_right_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_or_right_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_and_left_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_or_left_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_or_right_switch_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_and_right_switch_(Execute ptr, addr x, addr y, SubtypepResult *ret);

#ifdef LISP_DEBUG_SUBTYPEP
#define subtypep_andargs_right_ _n(subtypep_andargs_right_)
#define subtypep_orargs_right_ _n(subtypep_orargs_right_)
#define subtypep_andargs_left_ _n(subtypep_andargs_left_)
#define subtypep_orargs_left_ _n(subtypep_orargs_left_)
int subtypep_andargs_right_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_orargs_right_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_andargs_left_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_orargs_left_(Execute ptr, addr x, addr y, SubtypepResult *ret);
#else
#define subtypep_orargs_left_ subtypep_or_left_
#define subtypep_andargs_left_ subtypep_and_left_
#define subtypep_orargs_right_ subtypep_or_right_
#define subtypep_andargs_right_ subtypep_and_right_
#endif

#endif

