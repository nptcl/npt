#ifndef __CALL_CHARACTERS_HEADER__
#define __CALL_CHARACTERS_HEADER__

#include "local.h"
#include "typedef.h"

#define char_eql_common_ _n(char_eql_common_)
#define char_not_eql_common_ _n(char_not_eql_common_)
#define char_less_common_ _n(char_less_common_)
#define char_greater_common_ _n(char_greater_common_)
#define char_less_equal_common_ _n(char_less_equal_common_)
#define char_greater_equal_common_ _n(char_greater_equal_common_)
#define char_equal_common_ _n(char_equal_common_)
#define char_not_equal_common_ _n(char_not_equal_common_)
#define char_lessp_common_ _n(char_lessp_common_)
#define char_greaterp_common_ _n(char_greaterp_common_)
#define char_not_lessp_common_ _n(char_not_lessp_common_)
#define char_not_greaterp_common_ _n(char_not_greaterp_common_)
#define character_common_ _n(character_common_)
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

int char_eql_common_(addr list, addr *ret);
int char_not_eql_common_(addr list, addr *ret);
int char_less_common_(addr list, addr *ret);
int char_greater_common_(addr list, addr *ret);
int char_less_equal_common_(addr list, addr *ret);
int char_greater_equal_common_(addr list, addr *ret);
int char_equal_common_(addr list, addr *ret);
int char_not_equal_common_(addr list, addr *ret);
int char_lessp_common_(addr list, addr *ret);
int char_greaterp_common_(addr list, addr *ret);
int char_not_lessp_common_(addr list, addr *ret);
int char_not_greaterp_common_(addr list, addr *ret);
int character_common_(addr var, addr *ret);
void alpha_char_p_common(addr var, addr *ret);
void alphanumericp_common(addr var, addr *ret);
void digit_char_common(addr var, addr opt, addr *ret);
void digit_char_p_common(addr var, addr opt, addr *ret);
void graphic_char_p_common(addr var, addr *ret);
void standard_char_p_common(addr var, addr *ret);
void char_upcase_common(addr var, addr *ret);
void char_downcase_common(addr var, addr *ret);
void upper_case_p_common(addr var, addr *ret);
void lower_case_p_common(addr var, addr *ret);
void both_case_p_common(addr var, addr *ret);
void char_code_common(addr var, addr *ret);
void code_char_common(addr var, addr *ret);
int char_name_common_(addr var, addr *ret);
int name_char_common_(LocalRoot local, addr var, addr *ret);

#endif

