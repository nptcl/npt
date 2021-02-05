#ifndef __SUBTYPEP_TYPEDEF_HEADER__
#define __SUBTYPEP_TYPEDEF_HEADER__

#include "execute.h"
#include "typedef.h"

enum SUBTYPEP_RESULT {
	SUBTYPEP_INCLUDE,
	SUBTYPEP_EXCLUDE,
	SUBTYPEP_FALSE,
	SUBTYPEP_INVALID,
	SUBTYPEP_SIZE
};

enum SubtypepExtend {
	SubtypepExtend_Atomic,
	SubtypepExtend_AtomicNot,
	SubtypepExtend_Compound,
	SubtypepExtend_ForceNumber,
	SubtypepExtend_Normal
};

typedef enum SUBTYPEP_RESULT SubtypepResult;
typedef int (*call_type_subtypep)(Execute, addr, addr, SubtypepResult *);

#define ReturnInvalid(ret) Result(ret, SUBTYPEP_INVALID)
#define ReturnInclude(ret) Result(ret, SUBTYPEP_INCLUDE)
#define ReturnFalse(ret) Result(ret, SUBTYPEP_FALSE)
#define ReturnExclude(ret) Result(ret, SUBTYPEP_EXCLUDE)
#define ReturnIncludeExclude(ret, p) \
	Result(ret, (p)? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE)

#define ReturnSecondThrow(ret, v) { \
	switch (v) { \
		case SUBTYPEP_EXCLUDE: return ReturnExclude(ret); \
		case SUBTYPEP_INVALID: return ReturnInvalid(ret); \
		default: break; \
	} \
}
#define ReturnSecondValue(ret, v) \
	Result(ret, ((v) == SUBTYPEP_INCLUDE)? SUBTYPEP_FALSE: SUBTYPEP_EXCLUDE)

#endif

