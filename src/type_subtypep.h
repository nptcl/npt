#ifndef __TYPE_SUBTYPEP_HEADER__
#define __TYPE_SUBTYPEP_HEADER__

#include "type.h"
#include "typedef.h"

enum SUBTYPEP_RESULT {
	SUBTYPEP_INCLUDE,
	SUBTYPEP_EXCLUDE,
	SUBTYPEP_FALSE,
	SUBTYPEP_INVALID,
	SUBTYPEP_SIZE
};
typedef enum SUBTYPEP_RESULT SubtypepResult;

SubtypepResult subtypep_result(addr left, addr right, int asterisk);
int subtypep_asterisk_clang(addr left, addr right, int *validp);
int subtypep_clang(addr left, addr right, int *validp);
void init_type_subtypep(void);

#endif

