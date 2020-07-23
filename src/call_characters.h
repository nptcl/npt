#ifndef __CALL_CHARACTERS_HEADER__
#define __CALL_CHARACTERS_HEADER__

#include "local.h"
#include "typedef.h"

_g int char_eql_common(addr var, addr list, addr *ret);
_g int char_not_eql_common(addr list, addr *ret);
_g int char_less_common(addr var, addr list, addr *ret);
_g int char_greater_common(addr var, addr list, addr *ret);
_g int char_less_equal_common(addr var, addr list, addr *ret);
_g int char_greater_equal_common(addr var, addr list, addr *ret);
_g int char_equal_common(addr var, addr list, addr *ret);
_g int char_not_equal_common(addr list, addr *ret);
_g int char_lessp_common(addr var, addr list, addr *ret);
_g int char_greaterp_common(addr var, addr list, addr *ret);
_g int char_not_lessp_common(addr var, addr list, addr *ret);
_g int char_not_greaterp_common(addr var, addr list, addr *ret);
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

