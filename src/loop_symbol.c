#include "loop_symbol.h"
#include "object.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

static int symbol_equal_char_p_(addr pos, const char *str, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equal_char_(pos, str, ret);
}

_g int loop_symbol_named_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "NAMED", ret);
}

_g int loop_symbol_with_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "WITH", ret);
}

/* initial-final */
_g int loop_symbol_initially_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "INITIALLY", ret);
}

_g int loop_symbol_finally_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "FINALLY", ret);
}

_g int loop_symbol_initial_final_p_(addr pos, int *ret)
{
	Return(loop_symbol_initially_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_finally_p_(pos, ret);
}

/* for-as */
_g int loop_symbol_for_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "FOR", ret);
}

_g int loop_symbol_as_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "AS", ret);
}

_g int loop_symbol_for_as_p_(addr pos, int *ret)
{
	Return(loop_symbol_for_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_as_p_(pos, ret);
}

/* uncondition */
_g int loop_symbol_do_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "DO", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "DOING", ret);
}

_g int loop_symbol_return_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "RETURN", ret);
}

_g int loop_symbol_uncondition_p_(addr pos, int *ret)
{
	Return(loop_symbol_do_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_return_p_(pos, ret);
}

/* condition */
_g int loop_symbol_if_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "IF", ret);
}

_g int loop_symbol_when_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "WHEN", ret);
}

_g int loop_symbol_if_when_p_(addr pos, int *ret)
{
	Return(loop_symbol_if_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_when_p_(pos, ret);
}

_g int loop_symbol_unless_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UNLESS", ret);
}

_g int loop_symbol_condition_p_(addr pos, int *ret)
{
	Return(loop_symbol_if_p_(pos, ret));
	if (*ret)
		return 0;
	Return(loop_symbol_when_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_unless_p_(pos, ret);
}

/* accumulation */
_g int loop_symbol_collect_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "COLLECT", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "COLLECTING", ret);
}

_g int loop_symbol_append_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "APPEND", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "APPENDING", ret);
}

_g int loop_symbol_nconc_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "NCONC", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "NCONCING", ret);
}

_g int loop_symbol_count_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "COUNT", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "COUNTING", ret);
}

_g int loop_symbol_sum_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "SUM", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "SUMMING", ret);
}

_g int loop_symbol_maximize_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "MAXIMIZE", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "MAXIMIZING", ret);
}

_g int loop_symbol_minimize_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "MINIMIZE", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "MINIMIZING", ret);
}

_g int loop_symbol_list_accumulation_p_(addr pos, int *ret)
{
	Return(loop_symbol_collect_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_append_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_nconc_p_(pos, ret);
}

_g int loop_symbol_numeric_accumulation_p_(addr pos, int *ret)
{
	Return(loop_symbol_count_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_sum_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_maximize_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_minimize_p_(pos, ret);
}

_g int loop_symbol_accumulation_p_(addr pos, int *ret)
{
	Return(loop_symbol_list_accumulation_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_numeric_accumulation_p_(pos, ret);
}

/* termination */
_g int loop_symbol_repeat_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "REPEAT", ret);
}

_g int loop_symbol_always_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ALWAYS", ret);
}

_g int loop_symbol_never_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "NEVER", ret);
}

_g int loop_symbol_thereis_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "THEREIS", ret);
}

_g int loop_symbol_while_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "WHILE", ret);
}

_g int loop_symbol_until_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UNTIL", ret);
}

_g int loop_symbol_termination_p_(addr pos, int *ret)
{
	Return(loop_symbol_repeat_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_always_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_never_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_thereis_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_while_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_until_p_(pos, ret);
}

/* parse */
_g int loop_symbol_equal_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "=", ret);
}

_g int loop_symbol_and_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "AND", ret);
}

_g int loop_symbol_in_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "IN", ret);
}

_g int loop_symbol_on_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ON", ret);
}

_g int loop_symbol_by_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "BY", ret);
}

_g int loop_symbol_then_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "THEN", ret);
}

_g int loop_symbol_across_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ACROSS", ret);
}

_g int loop_symbol_being_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "BEING", ret);
}

_g int loop_symbol_each_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "EACH", ret);
}

_g int loop_symbol_the_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "THE", ret);
}

_g int loop_symbol_each_the_p_(addr pos, int *ret)
{
	Return(loop_symbol_each_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_the_p_(pos, ret);
}

_g int loop_symbol_of_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "OF", ret);
}

_g int loop_symbol_in_of_p_(addr pos, int *ret)
{
	Return(loop_symbol_in_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_of_p_(pos, ret);
}

_g int loop_symbol_hash_key_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-KEY", ret);
}

_g int loop_symbol_hash_keys_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-KEYS", ret);
}

_g int loop_symbol_hash_key2_p_(addr pos, int *ret)
{
	Return(loop_symbol_hash_key_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_hash_keys_p_(pos, ret);
}

_g int loop_symbol_hash_value_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-VALUE", ret);
}

_g int loop_symbol_hash_values_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-VALUES", ret);
}

_g int loop_symbol_hash_value2_p_(addr pos, int *ret)
{
	Return(loop_symbol_hash_value_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_hash_values_p_(pos, ret);
}

_g int loop_symbol_using_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "USING", ret);
}

_g int loop_symbol_symbol_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "SYMBOL", ret);
}

_g int loop_symbol_symbols_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "SYMBOLS", ret);
}

_g int loop_symbol_symbol2_p_(addr pos, int *ret)
{
	Return(loop_symbol_symbol_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_symbols_p_(pos, ret);
}

_g int loop_symbol_present_symbol_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "PRESENT-SYMBOL", ret);
}

_g int loop_symbol_present_symbols_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "PRESENT-SYMBOLS", ret);
}

_g int loop_symbol_present_symbol2_p_(addr pos, int *ret)
{
	Return(loop_symbol_present_symbol_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_present_symbols_p_(pos, ret);
}

_g int loop_symbol_external_symbol_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "EXTERNAL-SYMBOL", ret);
}

_g int loop_symbol_external_symbols_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "EXTERNAL-SYMBOLS", ret);
}

_g int loop_symbol_external_symbol2_p_(addr pos, int *ret)
{
	Return(loop_symbol_external_symbol_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_external_symbols_p_(pos, ret);
}

_g int loop_symbol_from_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "FROM", ret);
}

_g int loop_symbol_upfrom_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UPFROM", ret);
}

_g int loop_symbol_downfrom_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "DOWNFROM", ret);
}

_g int loop_symbol_to_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "TO", ret);
}

_g int loop_symbol_upto_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UPTO", ret);
}

_g int loop_symbol_downto_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "DOWNTO", ret);
}

_g int loop_symbol_above_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ABOVE", ret);
}

_g int loop_symbol_below_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "BELOW", ret);
}

_g int loop_symbol_arithmetic1_p_(addr pos, int *ret)
{
	Return(loop_symbol_from_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_upfrom_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_downfrom_p_(pos, ret);
}

_g int loop_symbol_arithmetic2_p_(addr pos, int *ret)
{
	Return(loop_symbol_to_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_upto_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_downto_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_above_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_below_p_(pos, ret);
}

_g int loop_symbol_arithmetic_p_(addr pos, int *ret)
{
	Return(loop_symbol_arithmetic1_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_arithmetic2_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_by_p_(pos, ret);
}

_g int loop_symbol_it_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "IT", ret);
}

_g int loop_symbol_else_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ELSE", ret);
}

_g int loop_symbol_end_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "END", ret);
}

_g int loop_symbol_into_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "INTO", ret);
}

/* main form */
_g int loop_symbol_form_main_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);

	Return(loop_symbol_uncondition_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_condition_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_accumulation_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_termination_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_initial_final_p_(pos, ret);
}

/* variables form */
_g int loop_symbol_form_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);

	Return(loop_symbol_form_main_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_with_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_for_as_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_else_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_end_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_and_p_(pos, ret);
}

