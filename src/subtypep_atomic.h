#ifndef __SUBTYPEP_ATOMIC_HEADER__
#define __SUBTYPEP_ATOMIC_HEADER__

#include "execute.h"
#include "subtypep_typedef.h"
#include "typedef.h"

#define subtypep_call_invalid_ _n(subtypep_call_invalid_)
#define subtypep_call_type_ _n(subtypep_call_type_)
#define subtypep_call_clos_ _n(subtypep_call_clos_)
#define subtypep_call_asterisk_ _n(subtypep_call_asterisk_)
#define subtypep_call_nil_ _n(subtypep_call_nil_)
#define subtypep_call_t_ _n(subtypep_call_t_)
#define subtypep_call_null_ _n(subtypep_call_null_)
#define subtypep_call_eqltype_ _n(subtypep_call_eqltype_)
#define subtypep_call_symbol_ _n(subtypep_call_symbol_)
#define subtypep_call_keyword_ _n(subtypep_call_keyword_)
#define subtypep_call_pathname_ _n(subtypep_call_pathname_)
#define subtypep_call_logical_pathname_ _n(subtypep_call_logical_pathname_)
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
#define subtypep_call_stream_ _n(subtypep_call_stream_)
#define subtypep_call_stream_type_ _n(subtypep_call_stream_type_)

int subtypep_call_invalid_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_type_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_clos_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_asterisk_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_nil_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_t_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_null_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_eqltype_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_symbol_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_keyword_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_pathname_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_logical_pathname_(Execute ptr, addr x, addr y, SubtypepResult *ret);
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
int subtypep_call_stream_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_call_stream_type_(Execute ptr, addr x, addr y, SubtypepResult *ret);

#endif

