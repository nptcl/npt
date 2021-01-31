#ifndef __TYPE_SUBTYPEP_HEADER__
#define __TYPE_SUBTYPEP_HEADER__

#include "type.h"
#include "typedef.h"

#define init_type_subtypep _n(init_type_subtypep)
#define subtypep_value_ _n(subtypep_value_)
#define subtypep_syscall_ _n(subtypep_syscall_)
#define subtypep_check_ _n(subtypep_check_)

enum SUBTYPEP_RESULT {
	SUBTYPEP_INCLUDE,
	SUBTYPEP_EXCLUDE,
	SUBTYPEP_FALSE,
	SUBTYPEP_INVALID,
	SUBTYPEP_SIZE
};
typedef enum SUBTYPEP_RESULT SubtypepResult;

void init_type_subtypep(void);
int subtypep_value_(Execute ptr, addr x, addr y, addr env, int as, SubtypepResult *ret);
int subtypep_check_(Execute ptr, addr x, addr y, addr env, int *ret, int *validp);
int subtypep_syscall_(Execute ptr, addr x, addr y, addr env, addr *ret);

#endif

