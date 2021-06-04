#include "constant.h"
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

int loop_symbol_named_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "NAMED", ret);
}

int loop_symbol_with_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "WITH", ret);
}

/* initial-final */
int loop_symbol_initially_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "INITIALLY", ret);
}

int loop_symbol_finally_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "FINALLY", ret);
}

int loop_symbol_initial_final_p_(addr pos, int *ret)
{
	Return(loop_symbol_initially_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_finally_p_(pos, ret);
}

int loop_symbol_of_type_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "OF-TYPE", ret);
}

/* for-as */
int loop_symbol_for_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "FOR", ret);
}

int loop_symbol_as_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "AS", ret);
}

int loop_symbol_for_as_p_(addr pos, int *ret)
{
	Return(loop_symbol_for_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_as_p_(pos, ret);
}

/* uncondition */
int loop_symbol_do_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "DO", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "DOING", ret);
}

int loop_symbol_return_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "RETURN", ret);
}

int loop_symbol_uncondition_p_(addr pos, int *ret)
{
	Return(loop_symbol_do_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_return_p_(pos, ret);
}

/* condition */
int loop_symbol_if_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "IF", ret);
}

int loop_symbol_when_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "WHEN", ret);
}

int loop_symbol_if_when_p_(addr pos, int *ret)
{
	Return(loop_symbol_if_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_when_p_(pos, ret);
}

int loop_symbol_unless_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UNLESS", ret);
}

int loop_symbol_condition_p_(addr pos, int *ret)
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
int loop_symbol_collect_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "COLLECT", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "COLLECTING", ret);
}

int loop_symbol_append_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "APPEND", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "APPENDING", ret);
}

int loop_symbol_nconc_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "NCONC", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "NCONCING", ret);
}

int loop_symbol_count_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "COUNT", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "COUNTING", ret);
}

int loop_symbol_sum_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "SUM", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "SUMMING", ret);
}

int loop_symbol_maximize_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "MAXIMIZE", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "MAXIMIZING", ret);
}

int loop_symbol_minimize_p_(addr pos, int *ret)
{
	Return(symbol_equal_char_p_(pos, "MINIMIZE", ret));
	if (*ret)
		return 0;

	return symbol_equal_char_p_(pos, "MINIMIZING", ret);
}

int loop_symbol_list_accumulation_p_(addr pos, int *ret)
{
	Return(loop_symbol_collect_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_append_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_nconc_p_(pos, ret);
}

int loop_symbol_numeric_accumulation_p_(addr pos, int *ret)
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

int loop_symbol_accumulation_p_(addr pos, int *ret)
{
	Return(loop_symbol_list_accumulation_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_numeric_accumulation_p_(pos, ret);
}

/* termination */
int loop_symbol_repeat_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "REPEAT", ret);
}

int loop_symbol_always_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ALWAYS", ret);
}

int loop_symbol_never_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "NEVER", ret);
}

int loop_symbol_thereis_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "THEREIS", ret);
}

int loop_symbol_while_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "WHILE", ret);
}

int loop_symbol_until_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UNTIL", ret);
}

int loop_symbol_termination_p_(addr pos, int *ret)
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
int loop_symbol_equal_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "=", ret);
}

int loop_symbol_and_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "AND", ret);
}

int loop_symbol_in_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "IN", ret);
}

int loop_symbol_on_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ON", ret);
}

int loop_symbol_by_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "BY", ret);
}

int loop_symbol_then_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "THEN", ret);
}

int loop_symbol_across_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ACROSS", ret);
}

int loop_symbol_being_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "BEING", ret);
}

int loop_symbol_each_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "EACH", ret);
}

int loop_symbol_the_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "THE", ret);
}

int loop_symbol_each_the_p_(addr pos, int *ret)
{
	Return(loop_symbol_each_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_the_p_(pos, ret);
}

int loop_symbol_of_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "OF", ret);
}

int loop_symbol_in_of_p_(addr pos, int *ret)
{
	Return(loop_symbol_in_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_of_p_(pos, ret);
}

int loop_symbol_hash_key_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-KEY", ret);
}

int loop_symbol_hash_keys_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-KEYS", ret);
}

int loop_symbol_hash_key2_p_(addr pos, int *ret)
{
	Return(loop_symbol_hash_key_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_hash_keys_p_(pos, ret);
}

int loop_symbol_hash_value_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-VALUE", ret);
}

int loop_symbol_hash_values_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "HASH-VALUES", ret);
}

int loop_symbol_hash_value2_p_(addr pos, int *ret)
{
	Return(loop_symbol_hash_value_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_hash_values_p_(pos, ret);
}

int loop_symbol_using_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "USING", ret);
}

int loop_symbol_symbol_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "SYMBOL", ret);
}

int loop_symbol_symbols_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "SYMBOLS", ret);
}

int loop_symbol_symbol2_p_(addr pos, int *ret)
{
	Return(loop_symbol_symbol_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_symbols_p_(pos, ret);
}

int loop_symbol_present_symbol_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "PRESENT-SYMBOL", ret);
}

int loop_symbol_present_symbols_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "PRESENT-SYMBOLS", ret);
}

int loop_symbol_present_symbol2_p_(addr pos, int *ret)
{
	Return(loop_symbol_present_symbol_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_present_symbols_p_(pos, ret);
}

int loop_symbol_external_symbol_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "EXTERNAL-SYMBOL", ret);
}

int loop_symbol_external_symbols_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "EXTERNAL-SYMBOLS", ret);
}

int loop_symbol_external_symbol2_p_(addr pos, int *ret)
{
	Return(loop_symbol_external_symbol_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_external_symbols_p_(pos, ret);
}

int loop_symbol_from_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "FROM", ret);
}

int loop_symbol_upfrom_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UPFROM", ret);
}

int loop_symbol_downfrom_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "DOWNFROM", ret);
}

int loop_symbol_to_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "TO", ret);
}

int loop_symbol_upto_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "UPTO", ret);
}

int loop_symbol_downto_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "DOWNTO", ret);
}

int loop_symbol_above_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ABOVE", ret);
}

int loop_symbol_below_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "BELOW", ret);
}

int loop_symbol_arithmetic1_p_(addr pos, int *ret)
{
	Return(loop_symbol_from_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_upfrom_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_downfrom_p_(pos, ret);
}

int loop_symbol_arithmetic2_p_(addr pos, int *ret)
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

int loop_symbol_arithmetic_p_(addr pos, int *ret)
{
	Return(loop_symbol_arithmetic1_p_(pos, ret));
	if (*ret)
		return 0;

	Return(loop_symbol_arithmetic2_p_(pos, ret));
	if (*ret)
		return 0;

	return loop_symbol_by_p_(pos, ret);
}

int loop_symbol_it_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "IT", ret);
}

int loop_symbol_else_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "ELSE", ret);
}

int loop_symbol_end_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "END", ret);
}

int loop_symbol_into_p_(addr pos, int *ret)
{
	return symbol_equal_char_p_(pos, "INTO", ret);
}

/* main form */
int loop_symbol_form_main_p_(addr pos, int *ret)
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
int loop_symbol_form_p_(addr pos, int *ret)
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


/*
 *  eq
 */
static int loop_symbol_type_p(addr pos, constindex index)
{
	addr check;
	GetConstant(index, &check);
	return pos == check;
}

int loop_symbol_initially_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_INITIALLY);
}

int loop_symbol_finally_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FINALLY);
}

int loop_symbol_with_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_WITH);
}

int loop_symbol_for_as_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS);
}

int loop_symbol_do_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_DO);
}

int loop_symbol_return_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_RETURN);
}

int loop_symbol_collect_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_COLLECT);
}

int loop_symbol_append_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_APPEND);
}

int loop_symbol_nconc_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_NCONC);
}

int loop_symbol_count_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_COUNT);
}

int loop_symbol_sum_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_SUM);
}

int loop_symbol_maximize_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_MAXIMIZE);
}

int loop_symbol_minimize_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_MINIMIZE);
}

int loop_symbol_if_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_IF);
}

int loop_symbol_unless_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_UNLESS);
}

int loop_symbol_while_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_WHILE);
}

int loop_symbol_until_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_UNTIL);
}

int loop_symbol_always_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_ALWAYS);
}

int loop_symbol_never_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_NEVER);
}

int loop_symbol_thereis_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_THEREIS);
}

int loop_symbol_repeat_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_REPEAT);
}

int loop_symbol_for_as_arithmetic_up_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP);
}

int loop_symbol_for_as_arithmetic_downto_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO);
}

int loop_symbol_for_as_arithmetic_downfrom_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM);
}

int loop_symbol_for_as_in_list_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_IN_LIST);
}

int loop_symbol_for_as_on_list_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_ON_LIST);
}

int loop_symbol_for_as_equals_then_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_EQUALS_THEN);
}

int loop_symbol_for_as_across_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_ACROSS);
}

int loop_symbol_for_as_hash_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_HASH);
}

int loop_symbol_for_as_package_symbol_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL);
}

int loop_symbol_for_as_package_present_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT);
}

int loop_symbol_for_as_package_external_p(addr pos)
{
	return loop_symbol_type_p(pos, CONSTANT_SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL);
}

