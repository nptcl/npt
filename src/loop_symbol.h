#ifndef __LOOP_SYMBOL_HEADER__
#define __LOOP_SYMBOL_HEADER__

#include "define.h"
#include "typedef.h"

#define loop_symbol_named_p_ _n(loop_symbol_named_p_)
#define loop_symbol_with_p_ _n(loop_symbol_with_p_)
#define loop_symbol_initially_p_ _n(loop_symbol_initially_p_)
#define loop_symbol_finally_p_ _n(loop_symbol_finally_p_)
#define loop_symbol_initial_final_p_ _n(loop_symbol_initial_final_p_)
#define loop_symbol_for_p_ _n(loop_symbol_for_p_)
#define loop_symbol_as_p_ _n(loop_symbol_as_p_)
#define loop_symbol_for_as_p_ _n(loop_symbol_for_as_p_)
#define loop_symbol_do_p_ _n(loop_symbol_do_p_)
#define loop_symbol_return_p_ _n(loop_symbol_return_p_)
#define loop_symbol_uncondition_p_ _n(loop_symbol_uncondition_p_)
#define loop_symbol_if_p_ _n(loop_symbol_if_p_)
#define loop_symbol_when_p_ _n(loop_symbol_when_p_)
#define loop_symbol_if_when_p_ _n(loop_symbol_if_when_p_)
#define loop_symbol_unless_p_ _n(loop_symbol_unless_p_)
#define loop_symbol_condition_p_ _n(loop_symbol_condition_p_)
#define loop_symbol_collect_p_ _n(loop_symbol_collect_p_)
#define loop_symbol_append_p_ _n(loop_symbol_append_p_)
#define loop_symbol_nconc_p_ _n(loop_symbol_nconc_p_)
#define loop_symbol_count_p_ _n(loop_symbol_count_p_)
#define loop_symbol_sum_p_ _n(loop_symbol_sum_p_)
#define loop_symbol_maximize_p_ _n(loop_symbol_maximize_p_)
#define loop_symbol_minimize_p_ _n(loop_symbol_minimize_p_)
#define loop_symbol_list_accumulation_p_ _n(loop_symbol_list_accumulation_p_)
#define loop_symbol_numeric_accumulation_p_ _n(loop_symbol_numeric_accumulation_p_)
#define loop_symbol_accumulation_p_ _n(loop_symbol_accumulation_p_)
#define loop_symbol_repeat_p_ _n(loop_symbol_repeat_p_)
#define loop_symbol_always_p_ _n(loop_symbol_always_p_)
#define loop_symbol_never_p_ _n(loop_symbol_never_p_)
#define loop_symbol_thereis_p_ _n(loop_symbol_thereis_p_)
#define loop_symbol_while_p_ _n(loop_symbol_while_p_)
#define loop_symbol_until_p_ _n(loop_symbol_until_p_)
#define loop_symbol_termination_p_ _n(loop_symbol_termination_p_)
#define loop_symbol_equal_p_ _n(loop_symbol_equal_p_)
#define loop_symbol_and_p_ _n(loop_symbol_and_p_)
#define loop_symbol_in_p_ _n(loop_symbol_in_p_)
#define loop_symbol_on_p_ _n(loop_symbol_on_p_)
#define loop_symbol_by_p_ _n(loop_symbol_by_p_)
#define loop_symbol_then_p_ _n(loop_symbol_then_p_)
#define loop_symbol_across_p_ _n(loop_symbol_across_p_)
#define loop_symbol_being_p_ _n(loop_symbol_being_p_)
#define loop_symbol_each_p_ _n(loop_symbol_each_p_)
#define loop_symbol_the_p_ _n(loop_symbol_the_p_)
#define loop_symbol_each_the_p_ _n(loop_symbol_each_the_p_)
#define loop_symbol_of_p_ _n(loop_symbol_of_p_)
#define loop_symbol_in_of_p_ _n(loop_symbol_in_of_p_)
#define loop_symbol_hash_key_p_ _n(loop_symbol_hash_key_p_)
#define loop_symbol_hash_keys_p_ _n(loop_symbol_hash_keys_p_)
#define loop_symbol_hash_key2_p_ _n(loop_symbol_hash_key2_p_)
#define loop_symbol_hash_value_p_ _n(loop_symbol_hash_value_p_)
#define loop_symbol_hash_values_p_ _n(loop_symbol_hash_values_p_)
#define loop_symbol_hash_value2_p_ _n(loop_symbol_hash_value2_p_)
#define loop_symbol_using_p_ _n(loop_symbol_using_p_)
#define loop_symbol_symbol_p_ _n(loop_symbol_symbol_p_)
#define loop_symbol_symbols_p_ _n(loop_symbol_symbols_p_)
#define loop_symbol_symbol2_p_ _n(loop_symbol_symbol2_p_)
#define loop_symbol_present_symbol_p_ _n(loop_symbol_present_symbol_p_)
#define loop_symbol_present_symbols_p_ _n(loop_symbol_present_symbols_p_)
#define loop_symbol_present_symbol2_p_ _n(loop_symbol_present_symbol2_p_)
#define loop_symbol_external_symbol_p_ _n(loop_symbol_external_symbol_p_)
#define loop_symbol_external_symbols_p_ _n(loop_symbol_external_symbols_p_)
#define loop_symbol_external_symbol2_p_ _n(loop_symbol_external_symbol2_p_)
#define loop_symbol_from_p_ _n(loop_symbol_from_p_)
#define loop_symbol_upfrom_p_ _n(loop_symbol_upfrom_p_)
#define loop_symbol_downfrom_p_ _n(loop_symbol_downfrom_p_)
#define loop_symbol_to_p_ _n(loop_symbol_to_p_)
#define loop_symbol_upto_p_ _n(loop_symbol_upto_p_)
#define loop_symbol_downto_p_ _n(loop_symbol_downto_p_)
#define loop_symbol_above_p_ _n(loop_symbol_above_p_)
#define loop_symbol_below_p_ _n(loop_symbol_below_p_)
#define loop_symbol_arithmetic1_p_ _n(loop_symbol_arithmetic1_p_)
#define loop_symbol_arithmetic2_p_ _n(loop_symbol_arithmetic2_p_)
#define loop_symbol_arithmetic_p_ _n(loop_symbol_arithmetic_p_)
#define loop_symbol_it_p_ _n(loop_symbol_it_p_)
#define loop_symbol_else_p_ _n(loop_symbol_else_p_)
#define loop_symbol_end_p_ _n(loop_symbol_end_p_)
#define loop_symbol_into_p_ _n(loop_symbol_into_p_)
#define loop_symbol_form_main_p_ _n(loop_symbol_form_main_p_)
#define loop_symbol_form_p_ _n(loop_symbol_form_p_)

int loop_symbol_named_p_(addr pos, int *ret);
int loop_symbol_with_p_(addr pos, int *ret);
int loop_symbol_initially_p_(addr pos, int *ret);
int loop_symbol_finally_p_(addr pos, int *ret);
int loop_symbol_initial_final_p_(addr pos, int *ret);
int loop_symbol_for_p_(addr pos, int *ret);
int loop_symbol_as_p_(addr pos, int *ret);
int loop_symbol_for_as_p_(addr pos, int *ret);
int loop_symbol_do_p_(addr pos, int *ret);
int loop_symbol_return_p_(addr pos, int *ret);
int loop_symbol_uncondition_p_(addr pos, int *ret);
int loop_symbol_if_p_(addr pos, int *ret);
int loop_symbol_when_p_(addr pos, int *ret);
int loop_symbol_if_when_p_(addr pos, int *ret);
int loop_symbol_unless_p_(addr pos, int *ret);
int loop_symbol_condition_p_(addr pos, int *ret);
int loop_symbol_collect_p_(addr pos, int *ret);
int loop_symbol_append_p_(addr pos, int *ret);
int loop_symbol_nconc_p_(addr pos, int *ret);
int loop_symbol_count_p_(addr pos, int *ret);
int loop_symbol_sum_p_(addr pos, int *ret);
int loop_symbol_maximize_p_(addr pos, int *ret);
int loop_symbol_minimize_p_(addr pos, int *ret);
int loop_symbol_list_accumulation_p_(addr pos, int *ret);
int loop_symbol_numeric_accumulation_p_(addr pos, int *ret);
int loop_symbol_accumulation_p_(addr pos, int *ret);
int loop_symbol_repeat_p_(addr pos, int *ret);
int loop_symbol_always_p_(addr pos, int *ret);
int loop_symbol_never_p_(addr pos, int *ret);
int loop_symbol_thereis_p_(addr pos, int *ret);
int loop_symbol_while_p_(addr pos, int *ret);
int loop_symbol_until_p_(addr pos, int *ret);
int loop_symbol_termination_p_(addr pos, int *ret);
int loop_symbol_equal_p_(addr pos, int *ret);
int loop_symbol_and_p_(addr pos, int *ret);
int loop_symbol_in_p_(addr pos, int *ret);
int loop_symbol_on_p_(addr pos, int *ret);
int loop_symbol_by_p_(addr pos, int *ret);
int loop_symbol_then_p_(addr pos, int *ret);
int loop_symbol_across_p_(addr pos, int *ret);
int loop_symbol_being_p_(addr pos, int *ret);
int loop_symbol_each_p_(addr pos, int *ret);
int loop_symbol_the_p_(addr pos, int *ret);
int loop_symbol_each_the_p_(addr pos, int *ret);
int loop_symbol_of_p_(addr pos, int *ret);
int loop_symbol_in_of_p_(addr pos, int *ret);
int loop_symbol_hash_key_p_(addr pos, int *ret);
int loop_symbol_hash_keys_p_(addr pos, int *ret);
int loop_symbol_hash_key2_p_(addr pos, int *ret);
int loop_symbol_hash_value_p_(addr pos, int *ret);
int loop_symbol_hash_values_p_(addr pos, int *ret);
int loop_symbol_hash_value2_p_(addr pos, int *ret);
int loop_symbol_using_p_(addr pos, int *ret);
int loop_symbol_symbol_p_(addr pos, int *ret);
int loop_symbol_symbols_p_(addr pos, int *ret);
int loop_symbol_symbol2_p_(addr pos, int *ret);
int loop_symbol_present_symbol_p_(addr pos, int *ret);
int loop_symbol_present_symbols_p_(addr pos, int *ret);
int loop_symbol_present_symbol2_p_(addr pos, int *ret);
int loop_symbol_external_symbol_p_(addr pos, int *ret);
int loop_symbol_external_symbols_p_(addr pos, int *ret);
int loop_symbol_external_symbol2_p_(addr pos, int *ret);
int loop_symbol_from_p_(addr pos, int *ret);
int loop_symbol_upfrom_p_(addr pos, int *ret);
int loop_symbol_downfrom_p_(addr pos, int *ret);
int loop_symbol_to_p_(addr pos, int *ret);
int loop_symbol_upto_p_(addr pos, int *ret);
int loop_symbol_downto_p_(addr pos, int *ret);
int loop_symbol_above_p_(addr pos, int *ret);
int loop_symbol_below_p_(addr pos, int *ret);
int loop_symbol_arithmetic1_p_(addr pos, int *ret);
int loop_symbol_arithmetic2_p_(addr pos, int *ret);
int loop_symbol_arithmetic_p_(addr pos, int *ret);
int loop_symbol_it_p_(addr pos, int *ret);
int loop_symbol_else_p_(addr pos, int *ret);
int loop_symbol_end_p_(addr pos, int *ret);
int loop_symbol_into_p_(addr pos, int *ret);
int loop_symbol_form_main_p_(addr pos, int *ret);
int loop_symbol_form_p_(addr pos, int *ret);

#endif

