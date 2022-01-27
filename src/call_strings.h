#ifndef __CALL_STRINGS_HEADER__
#define __CALL_STRINGS_HEADER__

#include "execute.h"
#include "typedef.h"

#define simple_string_p_common _n(simple_string_p_common)
#define char_common_ _n(char_common_)
#define setf_char_common_ _n(setf_char_common_)
#define string_common_ _n(string_common_)
#define string_upcase_common_ _n(string_upcase_common_)
#define string_downcase_common_ _n(string_downcase_common_)
#define string_capitalize_common_ _n(string_capitalize_common_)
#define nstring_upcase_common_ _n(nstring_upcase_common_)
#define nstring_downcase_common_ _n(nstring_downcase_common_)
#define nstring_capitalize_common_ _n(nstring_capitalize_common_)
#define string_trim_common_ _n(string_trim_common_)
#define string_left_trim_common_ _n(string_left_trim_common_)
#define string_right_trim_common_ _n(string_right_trim_common_)
#define string_eql_common_ _n(string_eql_common_)
#define string_not_eql_common_ _n(string_not_eql_common_)
#define string_less_common_ _n(string_less_common_)
#define string_greater_common_ _n(string_greater_common_)
#define string_less_equal_common_ _n(string_less_equal_common_)
#define string_greater_equal_common_ _n(string_greater_equal_common_)
#define string_equal_common_ _n(string_equal_common_)
#define string_not_equal_common_ _n(string_not_equal_common_)
#define string_lessp_common_ _n(string_lessp_common_)
#define string_greaterp_common_ _n(string_greaterp_common_)
#define string_not_greaterp_common_ _n(string_not_greaterp_common_)
#define string_not_lessp_common_ _n(string_not_lessp_common_)
#define make_string_common_ _n(make_string_common_)

void simple_string_p_common(addr var, addr *ret);
int char_common_(addr str, addr pos, addr *ret);
int setf_char_common_(addr value, addr str, addr pos);
int string_common_(addr var, addr *ret);
int string_upcase_common_(addr var, addr rest, addr *ret);
int string_downcase_common_(addr var, addr rest, addr *ret);
int string_capitalize_common_(addr var, addr rest, addr *ret);
int nstring_upcase_common_(addr var, addr rest);
int nstring_downcase_common_(addr var, addr rest);
int nstring_capitalize_common_(addr var, addr rest);
int string_trim_common_(addr trim, addr pos, addr *ret);
int string_left_trim_common_(addr trim, addr pos, addr *ret);
int string_right_trim_common_(addr trim, addr pos, addr *ret);
int string_eql_common_(addr var1, addr var2, addr rest, addr *ret);
int string_not_eql_common_(addr var1, addr var2, addr rest, addr *ret);
int string_less_common_(addr var1, addr var2, addr rest, addr *ret);
int string_greater_common_(addr var1, addr var2, addr rest, addr *ret);
int string_less_equal_common_(addr var1, addr var2, addr rest, addr *ret);
int string_greater_equal_common_(addr var1, addr var2, addr rest, addr *ret);
int string_equal_common_(addr var1, addr var2, addr rest, addr *ret);
int string_not_equal_common_(addr var1, addr var2, addr rest, addr *ret);
int string_lessp_common_(addr var1, addr var2, addr rest, addr *ret);
int string_greaterp_common_(addr var1, addr var2, addr rest, addr *ret);
int string_not_greaterp_common_(addr var1, addr var2, addr rest, addr *ret);
int string_not_lessp_common_(addr var1, addr var2, addr rest, addr *ret);
int make_string_common_(Execute ptr, addr var, addr rest, addr *ret);

#endif

