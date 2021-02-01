#ifndef __TYPE_SUBTYPEP_HEADER__
#define __TYPE_SUBTYPEP_HEADER__

#include "execute.h"
#include "type_compare.h"
#include "typedef.h"

#define init_type_subtypep _n(init_type_subtypep)
#define subtypep_array_ _n(subtypep_array_)
#define subtypep_value_ _n(subtypep_value_)
#define subtypep_syscall_ _n(subtypep_syscall_)
#define subtypep_check_ _n(subtypep_check_)
#define subtypep_check_common_ _n(subtypep_check_common_)
#define subtypep_table_ _n(subtypep_table_)

void init_type_subtypep(void);
int subtypep_array_(Execute ptr, addr left, addr right, SubtypepResult *ret);
int subtypep_value_(Execute ptr, addr x, addr y, addr env, int as, SubtypepResult *ret);
int subtypep_check_(Execute ptr, addr x, addr y, addr env, int *ret, int *validp);
int subtypep_check_common_(Execute ptr, addr x, addr y, addr env, int *ret, int *validp);
int subtypep_syscall_(Execute ptr, addr x, addr y, addr env, addr *ret);
int subtypep_table_(Execute ptr, addr x, addr y, addr env, addr *ret);

#endif

