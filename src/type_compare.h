#ifndef __TYPE_COMPARE_HEADER__
#define __TYPE_COMPARE_HEADER__

#include "execute.h"
#include "typedef.h"

enum SUBTYPEP_RESULT {
	SUBTYPEP_INCLUDE,
	SUBTYPEP_EXCLUDE,
	SUBTYPEP_FALSE,
	SUBTYPEP_INVALID,
	SUBTYPEP_SIZE
};

typedef enum SUBTYPEP_RESULT SubtypepResult;
typedef int (*call_type_subtypep)(Execute, addr, addr, SubtypepResult *);

#define ReturnInvalid(ret) Result(ret, SUBTYPEP_INVALID)
#define ReturnInclude(ret) Result(ret, SUBTYPEP_INCLUDE)
#define ReturnFalse(ret) Result(ret, SUBTYPEP_FALSE)
#define ReturnExclude(ret) Result(ret, SUBTYPEP_EXCLUDE)
#define ReturnBool(ret, p) Result(ret, (p)? SUBTYPEP_INCLUDE: SUBTYPEP_FALSE)
#define ReturnSwitchInclude(ret, v) { \
	switch (v) { \
		case SUBTYPEP_INVALID: return ReturnInvalid(ret); \
		case SUBTYPEP_INCLUDE: break; \
		default: return ReturnFalse(ret); \
	} \
}


/* function */
#define subtypep_call_type_ _n(subtypep_call_type_)
#define subtypep_call_clos_ _n(subtypep_call_clos_)
#define subtypep_call_nil_ _n(subtypep_call_nil_)
#define subtypep_call_t_ _n(subtypep_call_t_)
#define subtypep_call_null_ _n(subtypep_call_null_)
#define subtypep_call_eqltype_ _n(subtypep_call_eqltype_)
#define subtypep_call_symbol_ _n(subtypep_call_symbol_)
#define subtypep_call_keyword_ _n(subtypep_call_keyword_)
#define subtypep_call_pathname_ _n(subtypep_call_pathname_)
#define subtypep_call_logical_pathname_ _n(subtypep_call_logical_pathname_)
#define subtypep_call_sequence_ _n(subtypep_call_sequence_)
#define subtypep_call_array_ _n(subtypep_call_array_)
#define subtypep_call_simple_array_ _n(subtypep_call_simple_array_)
#define subtypep_call_character_ _n(subtypep_call_character_)
#define subtypep_call_base_char_ _n(subtypep_call_base_char_)
#define subtypep_call_standard_char_ _n(subtypep_call_standard_char_)
#define subtypep_call_integer_ _n(subtypep_call_integer_)
#define subtypep_call_rational_ _n(subtypep_call_rational_)
#define subtypep_call_real_ _n(subtypep_call_real_)
#define subtypep_call_number_ _n(subtypep_call_number_)
#define subtypep_call_float_ _n(subtypep_call_float_)
#define subtypep_call_short_float_ _n(subtypep_call_short_float_)
#define subtypep_call_single_float_ _n(subtypep_call_single_float_)
#define subtypep_call_double_float_ _n(subtypep_call_double_float_)
#define subtypep_call_long_float_ _n(subtypep_call_long_float_)
#define subtypep_call_ratio_ _n(subtypep_call_ratio_)

int subtypep_call_type_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_clos_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_nil_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_t_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_null_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_eqltype_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_symbol_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_keyword_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_pathname_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_logical_pathname_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_sequence_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_array_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_simple_array_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_character_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_base_char_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_standard_char_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_integer_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_rational_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_real_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_number_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_float_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_short_float_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_single_float_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_double_float_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_long_float_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_ratio_(Execute ptr, addr x, addr y, SubtypepResult *ret);

#endif

