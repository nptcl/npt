#ifndef __CALL_STRINGS_HEADER__
#define __CALL_STRINGS_HEADER__

#include "execute.h"
#include "typedef.h"

#define simple_string_p_common _n(simple_string_p_common)
#define char_common _n(char_common)
#define setf_char_common _n(setf_char_common)
#define string_common _n(string_common)
#define string_upcase_common _n(string_upcase_common)
#define string_downcase_common _n(string_downcase_common)
#define string_capitalize_common _n(string_capitalize_common)
#define nstring_upcase_common _n(nstring_upcase_common)
#define nstring_downcase_common _n(nstring_downcase_common)
#define nstring_capitalize_common _n(nstring_capitalize_common)
#define string_trim_common _n(string_trim_common)
#define string_left_trim_common _n(string_left_trim_common)
#define string_right_trim_common _n(string_right_trim_common)
#define string_eql_common _n(string_eql_common)
#define string_not_eql_common _n(string_not_eql_common)
#define string_less_common _n(string_less_common)
#define string_greater_common _n(string_greater_common)
#define string_less_equal_common _n(string_less_equal_common)
#define string_greater_equal_common _n(string_greater_equal_common)
#define string_equal_common _n(string_equal_common)
#define string_not_equal_common _n(string_not_equal_common)
#define string_lessp_common _n(string_lessp_common)
#define string_greaterp_common _n(string_greaterp_common)
#define string_not_greaterp_common _n(string_not_greaterp_common)
#define string_not_lessp_common _n(string_not_lessp_common)
#define make_string_common _n(make_string_common)

_g void simple_string_p_common(addr var, addr *ret);
_g int char_common(addr str, addr pos, addr *ret);
_g int setf_char_common(addr value, addr str, addr pos);
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

