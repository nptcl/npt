#ifndef __CALL_CHARACTERS_HEADER__
#define __CALL_CHARACTERS_HEADER__

#include "local.h"
#include "typedef.h"

#define char_eql_common _n(char_eql_common)
#define char_not_eql_common _n(char_not_eql_common)
#define char_less_common _n(char_less_common)
#define char_greater_common _n(char_greater_common)
#define char_less_equal_common _n(char_less_equal_common)
#define char_greater_equal_common _n(char_greater_equal_common)
#define char_equal_common _n(char_equal_common)
#define char_not_equal_common _n(char_not_equal_common)
#define char_lessp_common _n(char_lessp_common)
#define char_greaterp_common _n(char_greaterp_common)
#define char_not_lessp_common _n(char_not_lessp_common)
#define char_not_greaterp_common _n(char_not_greaterp_common)
#define character_common _n(character_common)
#define alpha_char_p_common _n(alpha_char_p_common)
#define alphanumericp_common _n(alphanumericp_common)
#define digit_char_common _n(digit_char_common)
#define digit_char_p_common _n(digit_char_p_common)
#define graphic_char_p_common _n(graphic_char_p_common)
#define standard_char_p_common _n(standard_char_p_common)
#define char_upcase_common _n(char_upcase_common)
#define char_downcase_common _n(char_downcase_common)
#define upper_case_p_common _n(upper_case_p_common)
#define lower_case_p_common _n(lower_case_p_common)
#define both_case_p_common _n(both_case_p_common)
#define char_code_common _n(char_code_common)
#define code_char_common _n(code_char_common)
#define char_name_common_ _n(char_name_common_)
#define name_char_common_ _n(name_char_common_)

_g int char_eql_common(addr list, addr *ret);
_g int char_not_eql_common(addr list, addr *ret);
_g int char_less_common(addr list, addr *ret);
_g int char_greater_common(addr list, addr *ret);
_g int char_less_equal_common(addr list, addr *ret);
_g int char_greater_equal_common(addr list, addr *ret);
_g int char_equal_common(addr list, addr *ret);
_g int char_not_equal_common(addr list, addr *ret);
_g int char_lessp_common(addr list, addr *ret);
_g int char_greaterp_common(addr list, addr *ret);
_g int char_not_lessp_common(addr list, addr *ret);
_g int char_not_greaterp_common(addr list, addr *ret);
_g int character_common(addr var, addr *ret);
_g void alpha_char_p_common(addr var, addr *ret);
_g void alphanumericp_common(addr var, addr *ret);
_g void digit_char_common(addr var, addr opt, addr *ret);
_g void digit_char_p_common(addr var, addr opt, addr *ret);
_g void graphic_char_p_common(addr var, addr *ret);
_g void standard_char_p_common(addr var, addr *ret);
_g void char_upcase_common(addr var, addr *ret);
_g void char_downcase_common(addr var, addr *ret);
_g void upper_case_p_common(addr var, addr *ret);
_g void lower_case_p_common(addr var, addr *ret);
_g void both_case_p_common(addr var, addr *ret);
_g void char_code_common(addr var, addr *ret);
_g void code_char_common(addr var, addr *ret);
_g int char_name_common_(addr var, addr *ret);
_g int name_char_common_(LocalRoot local, addr var, addr *ret);

#endif

