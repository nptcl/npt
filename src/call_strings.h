#ifndef __CALL_STRINGS_HEADER__
#define __CALL_STRINGS_HEADER__

#include "execute.h"
#include "typedef.h"

_g void simple_string_p_common(addr var, addr *ret);
_g int char_common(addr str, addr pos, addr *ret);
_g int schar_common(addr str, addr pos, addr *ret);
_g int setf_char_common(addr value, addr pos, addr index);
_g int string_common(addr var, addr *ret);
_g int string_upcase_common(addr var, addr rest, addr *ret);
_g int string_downcase_common(addr var, addr rest, addr *ret);
_g int string_capitalize_common(addr var, addr rest, addr *ret);
_g int nstring_upcase_common(addr var, addr rest);
_g int nstring_downcase_common(addr var, addr rest);
_g int nstring_capitalize_common(addr var, addr rest);
_g int string_trim_common(addr trim, addr pos, addr *ret);
_g int string_left_trim_common(addr trim, addr pos, addr *ret);
_g int string_right_trim_common(addr trim, addr pos, addr *ret);
_g int string_eql_common(addr var1, addr var2, addr rest, addr *ret);
_g int string_not_eql_common(addr var1, addr var2, addr rest, addr *ret);
_g int string_less_common(addr var1, addr var2, addr rest, addr *ret);
_g int string_greater_common(addr var1, addr var2, addr rest, addr *ret);
_g int string_less_equal_common(addr var1, addr var2, addr rest, addr *ret);
_g int string_greater_equal_common(addr var1, addr var2, addr rest, addr *ret);
_g int string_equal_common(addr var1, addr var2, addr rest, addr *ret);
_g int string_not_equal_common(addr var1, addr var2, addr rest, addr *ret);
_g int string_lessp_common(addr var1, addr var2, addr rest, addr *ret);
_g int string_greaterp_common(addr var1, addr var2, addr rest, addr *ret);
_g int string_not_greaterp_common(addr var1, addr var2, addr rest, addr *ret);
_g int string_not_lessp_common(addr var1, addr var2, addr rest, addr *ret);
_g int make_string_common(Execute ptr, addr var, addr rest, addr *ret);

#endif

