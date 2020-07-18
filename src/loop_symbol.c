#include "loop_symbol.h"
#include "object.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

static int symbol_equal_char_p(addr pos, const char *str)
{
	if (! symbolp(pos))
		return 0;
	GetNameSymbol(pos, &pos);
	return string_equal_char(pos, str);
}

_g int loop_symbol_named_p(addr pos)
{
	return symbol_equal_char_p(pos, "NAMED");
}

_g int loop_symbol_with_p(addr pos)
{
	return symbol_equal_char_p(pos, "WITH");
}

/* initial-final */
_g int loop_symbol_initially_p(addr pos)
{
	return symbol_equal_char_p(pos, "INITIALLY");
}

_g int loop_symbol_finally_p(addr pos)
{
	return symbol_equal_char_p(pos, "FINALLY");
}

_g int loop_symbol_initial_final_p(addr pos)
{
	return loop_symbol_initially_p(pos)
		|| loop_symbol_finally_p(pos);
}

/* for-as */
_g int loop_symbol_for_p(addr pos)
{
	return symbol_equal_char_p(pos, "FOR");
}

_g int loop_symbol_as_p(addr pos)
{
	return symbol_equal_char_p(pos, "AS");
}

_g int loop_symbol_for_as_p(addr pos)
{
	return loop_symbol_for_p(pos)
		|| loop_symbol_as_p(pos);
}

/* uncondition */
_g int loop_symbol_do_p(addr pos)
{
	return symbol_equal_char_p(pos, "DO")
		|| symbol_equal_char_p(pos, "DOING");
}

_g int loop_symbol_return_p(addr pos)
{
	return symbol_equal_char_p(pos, "RETURN");
}

_g int loop_symbol_uncondition_p(addr pos)
{
	return loop_symbol_do_p(pos)
		|| loop_symbol_return_p(pos);
}

/* condition */
_g int loop_symbol_if_p(addr pos)
{
	return symbol_equal_char_p(pos, "IF");
}

_g int loop_symbol_when_p(addr pos)
{
	return symbol_equal_char_p(pos, "WHEN");
}

_g int loop_symbol_unless_p(addr pos)
{
	return symbol_equal_char_p(pos, "UNLESS");
}

_g int loop_symbol_condition_p(addr pos)
{
	return loop_symbol_if_p(pos)
		|| loop_symbol_when_p(pos)
		|| loop_symbol_unless_p(pos);
}

/* accumulation */
_g int loop_symbol_collect_p(addr pos)
{
	return symbol_equal_char_p(pos, "COLLECT")
		|| symbol_equal_char_p(pos, "COLLECTING");
}

_g int loop_symbol_append_p(addr pos)
{
	return symbol_equal_char_p(pos, "APPEND")
		|| symbol_equal_char_p(pos, "APPENDING");
}

_g int loop_symbol_nconc_p(addr pos)
{
	return symbol_equal_char_p(pos, "NCONC")
		|| symbol_equal_char_p(pos, "NCONCING");
}

_g int loop_symbol_count_p(addr pos)
{
	return symbol_equal_char_p(pos, "COUNT")
		|| symbol_equal_char_p(pos, "COUNTING");
}

_g int loop_symbol_sum_p(addr pos)
{
	return symbol_equal_char_p(pos, "SUM")
		|| symbol_equal_char_p(pos, "SUMMING");
}

_g int loop_symbol_maximize_p(addr pos)
{
	return symbol_equal_char_p(pos, "MAXIMIZE")
		|| symbol_equal_char_p(pos, "MAXIMIZING");
}

_g int loop_symbol_minimize_p(addr pos)
{
	return symbol_equal_char_p(pos, "MINIMIZE")
		|| symbol_equal_char_p(pos, "MINIMIZING");
}

_g int loop_symbol_list_accumulation_p(addr pos)
{
	return loop_symbol_collect_p(pos)
		|| loop_symbol_append_p(pos)
		|| loop_symbol_nconc_p(pos);
}

_g int loop_symbol_numeric_accumulation_p(addr pos)
{
	return loop_symbol_count_p(pos)
		|| loop_symbol_sum_p(pos)
		|| loop_symbol_maximize_p(pos)
		|| loop_symbol_minimize_p(pos);
}

_g int loop_symbol_accumulation_p(addr pos)
{
	return loop_symbol_list_accumulation_p(pos)
		|| loop_symbol_numeric_accumulation_p(pos);
}

/* termination */
_g int loop_symbol_repeat_p(addr pos)
{
	return symbol_equal_char_p(pos, "REPEAT");
}

_g int loop_symbol_always_p(addr pos)
{
	return symbol_equal_char_p(pos, "ALWAYS");
}

_g int loop_symbol_never_p(addr pos)
{
	return symbol_equal_char_p(pos, "NEVER");
}

_g int loop_symbol_thereis_p(addr pos)
{
	return symbol_equal_char_p(pos, "THEREIS");
}

_g int loop_symbol_while_p(addr pos)
{
	return symbol_equal_char_p(pos, "WHILE");
}

_g int loop_symbol_until_p(addr pos)
{
	return symbol_equal_char_p(pos, "UNTIL");
}

_g int loop_symbol_termination_p(addr pos)
{
	return loop_symbol_repeat_p(pos)
		|| loop_symbol_always_p(pos)
		|| loop_symbol_never_p(pos)
		|| loop_symbol_thereis_p(pos)
		|| loop_symbol_while_p(pos)
		|| loop_symbol_until_p(pos);
}

/* parse */
_g int loop_symbol_equal_p(addr pos)
{
	return symbol_equal_char_p(pos, "=");
}

_g int loop_symbol_and_p(addr pos)
{
	return symbol_equal_char_p(pos, "AND");
}

_g int loop_symbol_in_p(addr pos)
{
	return symbol_equal_char_p(pos, "IN");
}

_g int loop_symbol_on_p(addr pos)
{
	return symbol_equal_char_p(pos, "ON");
}

_g int loop_symbol_by_p(addr pos)
{
	return symbol_equal_char_p(pos, "BY");
}

_g int loop_symbol_then_p(addr pos)
{
	return symbol_equal_char_p(pos, "THEN");
}

_g int loop_symbol_across_p(addr pos)
{
	return symbol_equal_char_p(pos, "ACROSS");
}

_g int loop_symbol_being_p(addr pos)
{
	return symbol_equal_char_p(pos, "BEING");
}

_g int loop_symbol_each_p(addr pos)
{
	return symbol_equal_char_p(pos, "EACH");
}

_g int loop_symbol_the_p(addr pos)
{
	return symbol_equal_char_p(pos, "THE");
}

_g int loop_symbol_each_the_p(addr pos)
{
	return loop_symbol_each_p(pos)
		|| loop_symbol_the_p(pos);
}

_g int loop_symbol_of_p(addr pos)
{
	return symbol_equal_char_p(pos, "OF");
}

_g int loop_symbol_in_of_p(addr pos)
{
	return loop_symbol_in_p(pos)
		|| loop_symbol_of_p(pos);
}

_g int loop_symbol_hash_key_p(addr pos)
{
	return symbol_equal_char_p(pos, "HASH-KEY");
}

_g int loop_symbol_hash_keys_p(addr pos)
{
	return symbol_equal_char_p(pos, "HASH-KEYS");
}

_g int loop_symbol_hash_key2_p(addr pos)
{
	return loop_symbol_hash_key_p(pos)
		|| loop_symbol_hash_keys_p(pos);
}

_g int loop_symbol_hash_value_p(addr pos)
{
	return symbol_equal_char_p(pos, "HASH-VALUE");
}

_g int loop_symbol_hash_values_p(addr pos)
{
	return symbol_equal_char_p(pos, "HASH-VALUES");
}

_g int loop_symbol_hash_value2_p(addr pos)
{
	return loop_symbol_hash_value_p(pos)
		|| loop_symbol_hash_values_p(pos);
}

_g int loop_symbol_using_p(addr pos)
{
	return symbol_equal_char_p(pos, "USING");
}

_g int loop_symbol_symbol_p(addr pos)
{
	return symbol_equal_char_p(pos, "SYMBOL");
}

_g int loop_symbol_symbols_p(addr pos)
{
	return symbol_equal_char_p(pos, "SYMBOLS");
}

_g int loop_symbol_symbol2_p(addr pos)
{
	return loop_symbol_symbol_p(pos)
		|| loop_symbol_symbols_p(pos);
}

_g int loop_symbol_present_symbol_p(addr pos)
{
	return symbol_equal_char_p(pos, "PRESENT-SYMBOL");
}

_g int loop_symbol_present_symbols_p(addr pos)
{
	return symbol_equal_char_p(pos, "PRESENT-SYMBOLS");
}

_g int loop_symbol_present_symbol2_p(addr pos)
{
	return loop_symbol_present_symbol_p(pos)
		|| loop_symbol_present_symbols_p(pos);
}

_g int loop_symbol_external_symbol_p(addr pos)
{
	return symbol_equal_char_p(pos, "EXTERNAL-SYMBOL");
}

_g int loop_symbol_external_symbols_p(addr pos)
{
	return symbol_equal_char_p(pos, "EXTERNAL-SYMBOLS");
}

_g int loop_symbol_external_symbol2_p(addr pos)
{
	return loop_symbol_external_symbol_p(pos)
		|| loop_symbol_external_symbols_p(pos);
}

_g int loop_symbol_from_p(addr pos)
{
	return symbol_equal_char_p(pos, "FROM");
}

_g int loop_symbol_upfrom_p(addr pos)
{
	return symbol_equal_char_p(pos, "UPFROM");
}

_g int loop_symbol_downfrom_p(addr pos)
{
	return symbol_equal_char_p(pos, "DOWNFROM");
}

_g int loop_symbol_to_p(addr pos)
{
	return symbol_equal_char_p(pos, "TO");
}

_g int loop_symbol_upto_p(addr pos)
{
	return symbol_equal_char_p(pos, "UPTO");
}

_g int loop_symbol_downto_p(addr pos)
{
	return symbol_equal_char_p(pos, "DOWNTO");
}

_g int loop_symbol_above_p(addr pos)
{
	return symbol_equal_char_p(pos, "ABOVE");
}

_g int loop_symbol_below_p(addr pos)
{
	return symbol_equal_char_p(pos, "BELOW");
}

_g int loop_symbol_arithmetic1_p(addr pos)
{
	return loop_symbol_from_p(pos)
		|| loop_symbol_upfrom_p(pos)
		|| loop_symbol_downfrom_p(pos);
}

_g int loop_symbol_arithmetic2_p(addr pos)
{
	return loop_symbol_to_p(pos)
		|| loop_symbol_upto_p(pos)
		|| loop_symbol_downto_p(pos)
		|| loop_symbol_above_p(pos)
		|| loop_symbol_below_p(pos);
}
_g int loop_symbol_arithmetic_p(addr pos)
{
	return loop_symbol_arithmetic1_p(pos)
		|| loop_symbol_arithmetic2_p(pos)
		|| loop_symbol_by_p(pos);
}

_g int loop_symbol_it_p(addr pos)
{
	return symbol_equal_char_p(pos, "IT");
}

_g int loop_symbol_else_p(addr pos)
{
	return symbol_equal_char_p(pos, "ELSE");
}

_g int loop_symbol_end_p(addr pos)
{
	return symbol_equal_char_p(pos, "END");
}

_g int loop_symbol_into_p(addr pos)
{
	return symbol_equal_char_p(pos, "INTO");
}

/* main form */
_g int loop_symbol_form_main_p(addr pos)
{
	if (! symbolp(pos))
		return 0;
	return loop_symbol_uncondition_p(pos)
		|| loop_symbol_condition_p(pos)
		|| loop_symbol_accumulation_p(pos)
		|| loop_symbol_termination_p(pos)
		|| loop_symbol_initial_final_p(pos);
}

/* variables form */
_g int loop_symbol_form_p(addr pos)
{
	if (! symbolp(pos))
		return 0;
	return loop_symbol_form_main_p(pos)
		|| loop_symbol_with_p(pos)
		|| loop_symbol_for_as_p(pos)
		|| loop_symbol_else_p(pos)
		|| loop_symbol_end_p(pos)
		|| loop_symbol_and_p(pos);
}

