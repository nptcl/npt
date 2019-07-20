#ifndef __LOOP_SYMBOL_HEADER__
#define __LOOP_SYMBOL_HEADER__

#include "define.h"
#include "typedef.h"

_g int loop_symbol_named_p(addr pos);
_g int loop_symbol_with_p(addr pos);
_g int loop_symbol_initially_p(addr pos);
_g int loop_symbol_finally_p(addr pos);
_g int loop_symbol_initial_final_p(addr pos);
_g int loop_symbol_for_p(addr pos);
_g int loop_symbol_as_p(addr pos);
_g int loop_symbol_for_as_p(addr pos);
_g int loop_symbol_do_p(addr pos);
_g int loop_symbol_return_p(addr pos);
_g int loop_symbol_uncondition_p(addr pos);
_g int loop_symbol_if_p(addr pos);
_g int loop_symbol_when_p(addr pos);
_g int loop_symbol_unless_p(addr pos);
_g int loop_symbol_condition_p(addr pos);
_g int loop_symbol_collect_p(addr pos);
_g int loop_symbol_append_p(addr pos);
_g int loop_symbol_nconc_p(addr pos);
_g int loop_symbol_count_p(addr pos);
_g int loop_symbol_sum_p(addr pos);
_g int loop_symbol_maximize_p(addr pos);
_g int loop_symbol_minimize_p(addr pos);
_g int loop_symbol_list_accumulation_p(addr pos);
_g int loop_symbol_numeric_accumulation_p(addr pos);
_g int loop_symbol_accumulation_p(addr pos);
_g int loop_symbol_repeat_p(addr pos);
_g int loop_symbol_always_p(addr pos);
_g int loop_symbol_never_p(addr pos);
_g int loop_symbol_thereis_p(addr pos);
_g int loop_symbol_while_p(addr pos);
_g int loop_symbol_until_p(addr pos);
_g int loop_symbol_termination_p(addr pos);
_g int loop_symbol_equal_p(addr pos);
_g int loop_symbol_and_p(addr pos);
_g int loop_symbol_in_p(addr pos);
_g int loop_symbol_on_p(addr pos);
_g int loop_symbol_by_p(addr pos);
_g int loop_symbol_then_p(addr pos);
_g int loop_symbol_across_p(addr pos);
_g int loop_symbol_being_p(addr pos);
_g int loop_symbol_each_p(addr pos);
_g int loop_symbol_the_p(addr pos);
_g int loop_symbol_each_the_p(addr pos);
_g int loop_symbol_of_p(addr pos);
_g int loop_symbol_in_of_p(addr pos);
_g int loop_symbol_hash_key_p(addr pos);
_g int loop_symbol_hash_keys_p(addr pos);
_g int loop_symbol_hash_key2_p(addr pos);
_g int loop_symbol_hash_value_p(addr pos);
_g int loop_symbol_hash_values_p(addr pos);
_g int loop_symbol_hash_value2_p(addr pos);
_g int loop_symbol_using_p(addr pos);
_g int loop_symbol_symbol_p(addr pos);
_g int loop_symbol_symbols_p(addr pos);
_g int loop_symbol_symbol2_p(addr pos);
_g int loop_symbol_present_symbol_p(addr pos);
_g int loop_symbol_present_symbols_p(addr pos);
_g int loop_symbol_present_symbol2_p(addr pos);
_g int loop_symbol_external_symbol_p(addr pos);
_g int loop_symbol_external_symbols_p(addr pos);
_g int loop_symbol_external_symbol2_p(addr pos);
_g int loop_symbol_from_p(addr pos);
_g int loop_symbol_upfrom_p(addr pos);
_g int loop_symbol_downfrom_p(addr pos);
_g int loop_symbol_to_p(addr pos);
_g int loop_symbol_upto_p(addr pos);
_g int loop_symbol_downto_p(addr pos);
_g int loop_symbol_above_p(addr pos);
_g int loop_symbol_below_p(addr pos);
_g int loop_symbol_arithmetic1_p(addr pos);
_g int loop_symbol_arithmetic2_p(addr pos);
_g int loop_symbol_arithmetic_p(addr pos);
_g int loop_symbol_it_p(addr pos);
_g int loop_symbol_else_p(addr pos);
_g int loop_symbol_end_p(addr pos);
_g int loop_symbol_into_p(addr pos);
_g int loop_symbol_form_main_p(addr pos);
_g int loop_symbol_form_p(addr pos);

#endif

