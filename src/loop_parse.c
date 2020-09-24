#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "loop_parse.h"
#include "loop_symbol.h"
#include "object.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  clause
 */
static int loop_parse_named_clause_(addr *ret, addr *list)
{
	int check;
	addr pos, args;

	/* check */
	*ret = Nil;
	if (! consp_getcons(*list, &pos, &args))
		return 0;
	Return(loop_symbol_named_p_(pos, &check));
	if (! check)
		return 0;

	/* parse */
	if (! consp_getcons(args, &pos, &args))
		return fmte_("NAMED clause must be a name argument in loop.", NULL);
	if (! symbolp(pos))
		return fmte_("NAMED argument ~S must be a symbol type.", pos, NULL);

	/* result */
	*ret = pos;
	*list = args;
	return 0;
}

static int loop_parse_with_variable_(addr pos)
{
	addr a, b;

	if (symbolp(pos))
		return 0;
	if (! consp_getcons(pos, &a, &b))
		return fmte_("The value ~S must be a symbol type.", pos, NULL);
	Return(loop_parse_with_variable_(a));
	Return(loop_parse_with_variable_(b));

	return 0;
}

static int loop_parse_with_clause1_(addr *list, addr *ret)
{
	int check;
	addr var, type, form, args, pos;

	args = *list;
	type = form = Unbound;
	/* var */
	GetCons(args, &var, &args);
	Return(loop_parse_with_variable_(var));
	if (args == Nil)
		goto loop_result;
	/* next */
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_equal_p_(pos, &check));
	if (check)
		goto loop_form;
	Return(loop_symbol_form_p_(pos, &check));
	if (check)
		goto loop_result;
	else
		goto loop_type;

	/* [type-spec] */
loop_type:
	type = pos;
	GetCdr(args, &args);
	if (args == Nil)
		goto loop_result;
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_equal_p_(pos, &check));
	if (check)
		goto loop_form;
	Return(loop_symbol_form_p_(pos, &check));
	if (check)
		goto loop_result;
	else
		goto error;

	/* [= form] */
loop_form:
	GetCdr(args, &args);
	if (! consp_getcons(args, &form, &args))
		goto error;
	if (args == Nil)
		goto loop_result;
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_form_p_(pos, &check));
	if (check)
		goto loop_result;
	else
		goto error;

loop_result:
	list_heap(ret, var, type, form, NULL);
	return Result(list, args);

error:
	*list = *ret = Nil;
	return fmte_("Invalid WITH form ~S in loop.", *list, NULL);
}

static int loop_parse_with_clause_(addr *root, addr *list)
{
	int check;
	addr args, vars, pos;

	/* loop */
	Return_getcdr(*list, &args);
	vars = Nil;
	if (! consp(args))
		goto error;
	for (;;) {
		/* push */
		Return(loop_parse_with_clause1_(&args, &pos));
		cons_heap(&vars, pos, vars);
		/* and */
		if (args == Nil)
			break;
		if (! consp_getcar(args, &pos))
			goto error;
		Return(loop_symbol_and_p_(pos, &check));
		if (! check)
			break;
		GetCdr(args, &args);
	}

	/* result */
	if (vars == Nil)
		goto error;
	GetConst(SYSTEM_LOOP_WITH, &pos);
	nreverse(&vars, vars);
	cons_heap(&pos, pos, vars);
	cons_heap(root, pos, *root);
	return Result(list, args);

error:
	return fmte_("Invalid WITH form ~S in loop.", *list, NULL);
}

static int loop_parse_form_variables_(addr *list, addr *ret)
{
	int check;
	addr root, pos, next;

	for (root = Nil; *list != Nil; ) {
		if (! consp_getcons(*list, &pos, &next))
			return fmte_("Invalid loop form ~S.", *list, NULL);
		Return(loop_symbol_form_p_(pos, &check));
		if (check)
			break;
		cons_heap(&root, pos, root);
		*list = next;
	}
	nreverse(ret, root);

	return 0;
}

static int loop_parse_initial_final_clause_(addr *root, addr *list)
{
	int check;
	addr key, pos;

	/* symbol */
	Return_getcons(*list, &pos, list);
	Return(loop_symbol_initially_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_INITIALLY, &key);
		goto variables;
	}
	Return(loop_symbol_finally_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_FINALLY, &key);
		goto variables;
	}
	return fmte_("Invalid value ~S.", pos, NULL);

variables:
	/* compound-form-variables */
	Return(loop_parse_form_variables_(list, &pos));
	cons_heap(&pos, key, pos);
	cons_heap(root, pos, *root);

	return 0;
}

struct for_as_arithmetic {
	addr from1, from2, to1, to2, by;
};

static void clear_for_as_arithmetic(struct for_as_arithmetic *str)
{
	str->from1 = str->from2 = str->to1 = str->to2 = str->by = Unbound;
}

static int loop_parse_for_as_arithmetic_struct_(
		struct for_as_arithmetic *str, addr *list, addr pos, int *ret)
{
	int check;
	*ret = 0;
loop:
	Return(loop_symbol_arithmetic1_p_(pos, &check));
	if (check) {
		if (str->from1 != Unbound)
			return fmte_("FOR-AS FROM expr already exists.", NULL);
		str->from1 = pos;
		if (! consp_getcons(*list, &(str->from2), list))
			return Result(ret, 1);
		goto next;
	}
	Return(loop_symbol_arithmetic2_p_(pos, &check));
	if (check) {
		if (str->to1 != Unbound)
			return fmte_("FOR-AS TO expr already exists.", NULL);
		str->to1 = pos;
		if (! consp_getcons(*list, &(str->to2), list))
			return Result(ret, 1);
		goto next;
	}
	Return(loop_symbol_by_p_(pos, &check));
	if (check) {
		if (str->by != Unbound)
			return fmte_("FOR-AS BY expr already exists.", NULL);
		if (! consp_getcons(*list, &(str->by), list))
			return Result(ret, 1);
		goto next;
	}
	return Result(ret, 1);

next:
	if (*list == Nil)
		return Result(ret, 0);
	if (! consp_getcar(*list, &pos))
		return Result(ret, 1);
	Return(loop_symbol_arithmetic_p_(pos, &check));
	if (! check)
		return Result(ret, 0);
	GetCdr(*list, list);
	goto loop;
}

static int loop_parse_for_as_arithmetic1_p_(struct for_as_arithmetic *str, int *ret)
{
	int check, check1, check2, check3, a, b, c;
	addr x, y, z;

	x = str->from1;
	y = str->to1;
	z = str->by;
	check1 = (x == Unbound);
	check2 = (y == Unbound);
	check3 = (z == Unbound);
	if (check1 && check2 && check3)
		return Result(ret, 0);
	/* from */
	if (! check1) {
		Return(loop_symbol_from_p_(x, &check));
		a = ! check;
		Return(loop_symbol_upfrom_p_(x, &check));
		b = ! check;
		if (a && b)
			return Result(ret, 0);
	}
	/* to */
	if (! check2) {
		Return(loop_symbol_to_p_(y, &check));
		a = ! check;
		Return(loop_symbol_upto_p_(y, &check));
		b = ! check;
		Return(loop_symbol_below_p_(y, &check));
		c = ! check;
		if (a && b && c)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int loop_parse_for_as_arithmetic2_p_(struct for_as_arithmetic *str, int *ret)
{
	int check, check1, check2, a, b;
	addr x, y;

	x = str->from1;
	y = str->to1;
	check1 = (x == Unbound);
	check2 = (y == Unbound);
	/* from */
	if (check1)
		return Result(ret, 0);
	Return(loop_symbol_from_p_(x, &check));
	if (! check)
		return Result(ret, 0);
	/* to */
	if (check2)
		return Result(ret, 0);
	Return(loop_symbol_downto_p_(y, &check));
	a = ! check;
	Return(loop_symbol_above_p_(y, &check));
	b = ! check;
	if (a && b)
		return Result(ret, 0);

	return Result(ret, 1);
}

static int loop_parse_for_as_arithmetic3_p_(struct for_as_arithmetic *str, int *ret)
{
	int check, check1, check2, a, b, c;
	addr x, y;

	x = str->from1;
	y = str->to1;
	check1 = (x == Unbound);
	check2 = (y == Unbound);
	/* from */
	if (check1)
		return Result(ret, 0);
	Return(loop_symbol_downfrom_p_(x, &check));
	if (! check)
		return Result(ret, 0);
	/* to */
	if (! check2) {
		Return(loop_symbol_to_p_(y, &check));
		a = ! check;
		Return(loop_symbol_downto_p_(y, &check));
		b = ! check;
		Return(loop_symbol_above_p_(y, &check));
		c = ! check;
		if (a && b && c)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int loop_parse_for_as_arithmetic_(Execute ptr,
		addr *list, addr *value, int *ret)
{
	int check;
	addr var, args, pos, g1, g2;
	struct for_as_arithmetic str;

	clear_for_as_arithmetic(&str);
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_arithmetic_p_(pos, &check));
	if (! check) {
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
		Return(loop_symbol_arithmetic_p_(pos, &check));
		if (! check)
			return Result(ret, 0);
	}
	Return(loop_parse_for_as_arithmetic_struct_(&str, &args, pos, &check));
	if (check)
		return Result(ret, 0);
	Return(loop_parse_for_as_arithmetic1_p_(&str, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP, &pos);
		goto next;
	}
	Return(loop_parse_for_as_arithmetic2_p_(&str, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO, &pos);
		goto next;
	}
	Return(loop_parse_for_as_arithmetic3_p_(&str, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM, &pos);
		goto next;
	}
	return Result(ret, 0);
next:
	/* gensym */
	Return(make_gensym_(ptr, &g1));
	Return(make_gensym_(ptr, &g2));
	/* result */
	list_heap(value, pos, var,
			str.from1, str.from2, str.to1, str.to2, str.by, g1, g2, NULL);
	*list = args;
	return Result(ret, 1);
}

static int loop_parse_for_as_call_list_(Execute ptr,
		addr *list, addr *value, int *ret,
		int (*check1_)(addr, int *),
		int (*check2_)(addr, int *),
		constindex index)
{
	/* var [type-spec] in form [by step] */
	int check;
	addr args, var, type, form, step, pos, g;

	type = step = Unbound;
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return((*check1_)(pos, &check));
	if (! check) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
		Return((*check1_)(pos, &check));
		if (! check)
			return Result(ret, 0);
	}
	if (! consp_getcons(args, &form, &args))
		return Result(ret, 0);
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		return Result(ret, 0);
	Return((*check2_)(pos, &check));
	if (! check)
		goto result;
	GetCdr(args, &args);
	if (! consp_getcons(args, &step, &args))
		return Result(ret, 0);
	goto result;

result:
	Return(make_gensym_(ptr, &g));
	GetConstant(index, &pos);
	list_heap(value, pos, var, type, form, step, g, NULL);
	*list = args;
	return Result(ret, 1);
}

static int loop_parse_for_as_in_list_(Execute ptr,
		addr *list, addr *value, int *ret)
{
	return loop_parse_for_as_call_list_(ptr, list, value, ret,
			loop_symbol_in_p_, loop_symbol_by_p_,
			CONSTANT_SYSTEM_LOOP_FOR_AS_IN_LIST);
}

static int loop_parse_for_as_on_list_(Execute ptr,
		addr *list, addr *value, int *ret)
{
	return loop_parse_for_as_call_list_(ptr, list, value, ret,
			loop_symbol_on_p_, loop_symbol_by_p_,
			CONSTANT_SYSTEM_LOOP_FOR_AS_ON_LIST);
}

static int loop_parse_for_as_equals_then_(Execute ptr,
		addr *list, addr *value, int *ret)
{
	return loop_parse_for_as_call_list_(ptr, list, value, ret,
			loop_symbol_equal_p_, loop_symbol_then_p_,
			CONSTANT_SYSTEM_LOOP_FOR_AS_EQUALS_THEN);
}

static int loop_parse_for_as_across_(Execute ptr, addr *list, addr *value, int *ret)
{
	/* var [type-spec] across vector */
	int check;
	addr args, var, type, vector, pos, g1, g2, g3;

	type = Unbound;
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_across_p_(pos, &check));
	if (! check) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
		Return(loop_symbol_across_p_(pos, &check));
		if (! check)
			return Result(ret, 0);
	}
	if (! consp_getcons(args, &vector, &args))
		return Result(ret, 0);
	/* result */
	Return(make_gensym_(ptr, &g1));
	Return(make_gensym_(ptr, &g2));
	Return(make_gensym_(ptr, &g3));
	GetConst(SYSTEM_LOOP_FOR_AS_ACROSS, &pos);
	list_heap(value, pos, var, type, vector, g1, g2, g3, NULL);
	*list = args;
	return Result(ret, 1);
}

static int loop_parse_for_as_hash_(Execute ptr, addr *list, addr *value, int *ret)
{
	/* var [type-spec] being {each|the}
	 *     { hash-key | hash-keys | hash-value | hash-values }
	 *     {in|of} table [using ({hash-key|hash-value} var2)]
	 */
	int check;
	addr var, type, keyp, table, use, args, pos, g;

	type = use = Unbound;
	/* var */
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	/* type-spec, being */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_being_p_(pos, &check));
	if (! check) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
		Return(loop_symbol_being_p_(pos, &check));
		if (! check)
			return Result(ret, 0);
	}
	/* each, the */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_each_the_p_(pos, &check));
	if (! check)
		return Result(ret, 0);
	/* hash-key, hash-keys, hash-value, hash-values */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_hash_key2_p_(pos, &check));
	if (check) {
		keyp = T;
		goto next1;
	}
	Return(loop_symbol_hash_value2_p_(pos, &check));
	if (check) {
		keyp = Nil;
		goto next1;
	}
	return Result(ret, 0);
next1:
	/* in, of */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_in_of_p_(pos, &check));
	if (! check)
		return Result(ret, 0);
	/* table */
	if (! consp_getcons(args, &table, &args))
		return Result(ret, 0);
	/* using */
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		return Result(ret, 0);
	Return(loop_symbol_using_p_(pos, &check));
	if (! check)
		goto result;
	/* hash-key, hash-value */
	GetCdr(args, &args);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (! consp_getcons(pos, &pos, &use))
		return Result(ret, 0);
	Return(loop_symbol_hash_key_p_(pos, &check));
	if (check) {
		if (keyp == T)
			return Result(ret, 0);
		goto next2;
	}
	Return(loop_symbol_hash_value_p_(pos, &check));
	if (check) {
		if (keyp == Nil)
			return Result(ret, 0);
		goto next2;
	}
	return Result(ret, 0);
next2:
	/* var2 */
	if (! consp_getcons(use, &use, &pos))
		return Result(ret, 0);
	if (pos != Nil)
		return Result(ret, 0);
	goto result;

result:
	Return(make_gensym_(ptr, &g));
	GetConst(SYSTEM_LOOP_FOR_AS_HASH, &pos);
	list_heap(value, pos, var, type, keyp, table, use, g, NULL);
	*list = args;
	return Result(ret, 1);
}

static int loop_parse_for_as_package_(Execute ptr, addr *list, addr *value, int *ret)
{
	/* var [type-spec] being {each|the}
	 *     { symbol | symbols |
	 *       present-symbol | present-symbols |
	 *       external-symbol | external-symbols }
	 *     [{in|of} package]
	 */
	int check, symbolp, presentp, externalp;
	addr var, type, package, args, pos, g;

	symbolp = presentp = externalp = 0;
	type = package = Unbound;
	/* var */
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	/* type-spec, being */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_being_p_(pos, &check));
	if (! check) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
		Return(loop_symbol_being_p_(pos, &check));
		if (! check)
			return Result(ret, 0);
	}
	/* each, the */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_each_the_p_(pos, &check));
	if (! check)
		return Result(ret, 0);
	/* { symbol | symbols |
	 *   present-symbol | present-symbols |
	 *   external-symbol | external-symbols }
	 */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_symbol2_p_(pos, &check));
	if (check) {
		symbolp = 1;
		goto next;
	}
	Return(loop_symbol_present_symbol2_p_(pos, &check));
	if (check) {
		presentp = 1;
		goto next;
	}
	Return(loop_symbol_external_symbol2_p_(pos, &check));
	if (check) {
		externalp = 1;
		goto next;
	}
	return Result(ret, 0);
next:
	/* in, of */
	if (args == Nil)
		goto result;
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	Return(loop_symbol_in_of_p_(pos, &check));
	if (! check)
		return Result(ret, 0);
	/* package */
	if (! consp_getcons(args, &package, &args))
		return Result(ret, 0);
	goto result;

result:
	if (symbolp)
		GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL, &pos);
	else if (presentp)
		GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT, &pos);
	else if (externalp)
		GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL, &pos);
	else
		return Result(ret, 0);
	/* result */
	Return(make_gensym_(ptr, &g));
	list_heap(value, pos, var, type, package, g, NULL);
	*list = args;
	return Result(ret, 1);
}

static int loop_parse_for_as_clause1_(Execute ptr, addr *list, addr *value, int *ret)
{
	int check;

	Return(loop_parse_for_as_arithmetic_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_in_list_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_on_list_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_equals_then_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_across_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_hash_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	Return(loop_parse_for_as_package_(ptr, list, value, &check));
	if (check)
		return Result(ret, 1);

	return 0;
}

static int loop_parse_for_as_clause_(Execute ptr, addr *root, addr *list)
{
	int check;
	addr args, vars, pos;

	/* loop */
	Return_getcdr(*list, &args);
	vars = Nil;
	if (! consp(args))
		goto error;
	for (;;) {
		/* push */
		Return(loop_parse_for_as_clause1_(ptr, &args, &pos, &check));
		if (! check)
			goto error;
		cons_heap(&vars, pos, vars);
		/* and */
		if (args == Nil)
			break;
		if (! consp_getcar(args, &pos))
			goto error;
		Return(loop_symbol_and_p_(pos, &check));
		if (! check)
			break;
		GetCdr(args, &args);
	}

	/* result */
	if (vars == Nil)
		goto error;
	GetConst(SYSTEM_LOOP_FOR_AS, &pos);
	cons_heap(&pos, pos, vars);
	cons_heap(root, pos, *root);
	return Result(list, args);

error:
	return fmte_("Invalid FOR-AS form ~S in loop.", *list, NULL);
}

static int loop_parse_variable_clause_update_(Execute ptr, addr *root, addr *list)
{
	int check;
	addr pos;

	if (! consp_getcar(*list, &pos))
		return 0;

	/* with */
	Return(loop_symbol_with_p_(pos, &check));
	if (check)
		return loop_parse_with_clause_(root, list);

	/* initial */
	Return(loop_symbol_initial_final_p_(pos, &check));
	if (check)
		return loop_parse_initial_final_clause_(root, list);

	/* for_as */
	Return(loop_symbol_for_as_p_(pos, &check));
	if (check)
		return loop_parse_for_as_clause_(ptr, root, list);

	return 0;
}

static int loop_parse_variable_clause_(Execute ptr, addr *root, addr *list)
{
	addr check;

	for (;;) {
		check = *root;
		Return(loop_parse_variable_clause_update_(ptr, root, list));
		if (check == *root)
			break;
	}

	return 0;
}

static int loop_parse_uncondition_result_(addr *ret, addr *list)
{
	int check;
	addr key, pos;

	Return_getcons(*list, &pos, list);
	Return(loop_symbol_do_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_DO, &key);
		Return(loop_parse_form_variables_(list, &pos));
		cons_heap(ret, key, pos);
		return 0;
	}

	Return(loop_symbol_return_p_(pos, &check));
	if (check) {
		if (! consp_getcons(*list, &pos, list))
			return fmte_("Invalid RETURN form ~S in loop.", *list, NULL);
		Return(loop_symbol_it_p_(pos, &check));
		if (check)
			GetConst(SYSTEM_IT_LOOP, &pos);
		GetConst(SYSTEM_LOOP_RETURN, &key);
		list_heap(ret, key, pos, NULL);
		return 0;
	}

	return fmte_("Invalid value ~S.", pos, NULL);
}

static int loop_parse_uncondition_clause_(addr *root, addr *list)
{
	addr pos;

	Return(loop_parse_uncondition_result_(&pos, list));
	cons_heap(root, pos, *root);

	return 0;
}

static int loop_condition_selectable_result_(addr *ret, addr *list);
static int loop_parse_condition_result_(addr *ret, addr *list)
{
	int check;
	addr args, pos, key, form, expr1, expr2;

	Return_getcons(*list, &pos, &args);
	Return(loop_symbol_if_when_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_IF, &key);
		goto next;
	}
	Return(loop_symbol_unless_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_UNLESS, &key);
		goto next;
	}
	goto error;
next:
	/* form */
	expr2 = Unbound;
	if (! consp_getcons(args, &form, &args))
		goto error;
	/* then */
	if (! consp(args))
		goto error;
	Return(loop_condition_selectable_result_(&expr1, &args));
	/* else */
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_else_p_(pos, &check));
	if (! check)
		goto result;
	GetCdr(args, &args);
	Return(loop_condition_selectable_result_(&expr2, &args));
	/* end */
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_end_p_(pos, &check));
	if (! check)
		goto result;
	GetCdr(args, &args);
	goto result;

result:
	list_heap(ret, key, form, expr1, expr2, NULL);
	return Result(list, args);

error:
	return fmte_("Invalid loop form ~S.", *list, NULL);
}

static int loop_parse_condition_clause_(addr *root, addr *list)
{
	addr pos;

	Return(loop_parse_condition_result_(&pos, list));
	cons_heap(root, pos, *root);

	return 0;
}

static int loop_parse_list_accumulation_result_(addr *ret, addr *list)
{
	int check;
	addr pos, key, args, form, into;

	/* first */
	Return_getcons(*list, &pos, &args);
	into = Unbound;
	Return(loop_symbol_collect_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_COLLECT, &key);
		goto next;
	}
	Return(loop_symbol_append_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_APPEND, &key);
		goto next;
	}
	Return(loop_symbol_nconc_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_NCONC, &key);
		goto next;
	}
	goto error;
next:
	/* form, it */
	if (! consp_getcons(args, &form, &args))
		goto error;
	Return(loop_symbol_it_p_(form, &check));
	if (check)
		GetConst(SYSTEM_IT_LOOP, &form);
	if (! consp_getcar(args, &pos))
		goto result;
	Return(loop_symbol_into_p_(pos, &check));
	if (! check)
		goto result;
	GetCdr(args, &args);
	if (! consp_getcons(args, &into, &args))
		goto error;
	else
		goto result;

result:
	list_heap(ret, key, form, into, NULL);
	return Result(list, args);

error:
	return fmte_("Invalid loop form ~S.", *list, NULL);
}

static int loop_parse_numeric_accumulation_result_(addr *ret, addr *list)
{
	int check;
	addr pos, key, args, form, into, type;

	/* first */
	Return_getcons(*list, &pos, &args);
	into = type = Unbound;
	Return(loop_symbol_count_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_COUNT, &key);
		goto next;
	}
	Return(loop_symbol_sum_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_SUM, &key);
		goto next;
	}
	Return(loop_symbol_maximize_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_MAXIMIZE, &key);
		goto next;
	}
	Return(loop_symbol_minimize_p_(pos, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_MINIMIZE, &key);
		goto next;
	}
	goto error;
next:
	/* form, it */
	if (! consp_getcons(args, &form, &args))
		goto error;
	Return(loop_symbol_it_p_(form, &check));
	if (check)
		GetConst(SYSTEM_IT_LOOP, &form);
	if (! consp_getcar(args, &pos))
		goto result;
	Return(loop_symbol_into_p_(pos, &check));
	if (! check)
		goto result;
	GetCdr(args, &args);
	if (! consp_getcons(args, &into, &args))
		goto error;
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		goto error;
	Return(loop_symbol_form_main_p_(pos, &check));
	if (check)
		goto result;
	GetCons(args, &type, &args);
	goto result;

result:
	list_heap(ret, key, form, into, type, NULL);
	return Result(list, args);

error:
	return fmte_("Invalid loop form ~S.", *list, NULL);
}

static int loop_parse_accumulation_result_(addr *ret, addr *list)
{
	int check;
	addr pos;

	GetCar(*list, &pos);
	Return(loop_symbol_list_accumulation_p_(pos, &check));
	if (check)
		return loop_parse_list_accumulation_result_(ret, list);

	Return(loop_symbol_numeric_accumulation_p_(pos, &check));
	if (check)
		return loop_parse_numeric_accumulation_result_(ret, list);

	*ret = Nil;
	return fmte_("Invalid accumulation symbol ~S.", pos, NULL);
}

static int loop_parse_accumulation_clause_(addr *root, addr *list)
{
	addr pos;

	Return(loop_parse_accumulation_result_(&pos, list));
	cons_heap(root, pos, *root);

	return 0;
}

static int loop_parse_selectable_p_(addr *list, addr *pos, int *ret)
{
	int check;

	if (! consp_getcar(*list, pos)) {
		*pos = Nil;
		return Result(ret, 0);
	}

	/* condition */
	Return(loop_symbol_condition_p_(*pos, &check));
	if (check) {
		*ret = 1;
		return loop_parse_condition_result_(pos, list);
	}

	/* uncondition */
	Return(loop_symbol_uncondition_p_(*pos, &check));
	if (check) {
		*ret = 1;
		return loop_parse_uncondition_result_(pos, list);
	}

	/* accumulation */
	Return(loop_symbol_accumulation_p_(*pos, &check));
	if (check) {
		*ret = 1;
		return loop_parse_accumulation_result_(pos, list);
	}

	/* error */
	*pos = Nil;
	return Result(ret, 0);
}

static int loop_condition_selectable_result_(addr *ret, addr *list)
{
	int check;
	addr root, pos;

	root = Nil;
	for (;;) {
		Return(loop_parse_selectable_p_(list, &pos, &check));
		if (! check)
			break;
		cons_heap(&root, pos, root);
		/* and */
		if (! consp_getcar(*list, &pos))
			break;
		Return(loop_symbol_and_p_(pos, &check));
		if (! check)
			break;
		GetCdr(*list, list);
	}
	nreverse(ret, root);

	return 0;
}

static int loop_parse_termination_clause_(addr *root, addr *list)
{
	int check;
	addr symbol, pos, args, key, a, b;

	Return_getcons(*list, &symbol, &args);
	Return(loop_symbol_while_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_WHILE, &key);
		goto next;
	}
	Return(loop_symbol_until_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_UNTIL, &key);
		goto next;
	}
	Return(loop_symbol_repeat_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_REPEAT, &key);
		goto next;
	}
	Return(loop_symbol_always_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_ALWAYS, &key);
		goto next;
	}
	Return(loop_symbol_never_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_NEVER, &key);
		goto next;
	}
	Return(loop_symbol_thereis_p_(symbol, &check));
	if (check) {
		GetConst(SYSTEM_LOOP_THEREIS, &key);
		goto next;
	}
	goto error;

next:
	if (! consp_getcons(args, &pos, &args))
		goto error;
	Return(loop_symbol_repeat_p_(symbol, &check));
	if (check) {
		make_symbolchar(&a, "A");
		make_symbolchar(&b, "B");
		list_heap(&pos, key, pos, a, b, NULL);
	}
	else {
		list_heap(&pos, key, pos, NULL);
	}
	cons_heap(root, pos, *root);
	return Result(list, args);

error:
	return fmte_("Invalid loop form ~S.", *list, NULL);
}

static int loop_parse_main_clause_update_(addr *root, addr *list)
{
	int check;
	addr pos;

	if (! consp_getcar(*list, &pos))
		return 0;

	/* uncondition */
	Return(loop_symbol_uncondition_p_(pos, &check));
	if (check)
		return loop_parse_uncondition_clause_(root, list);

	/* condition */
	Return(loop_symbol_condition_p_(pos, &check));
	if (check)
		return loop_parse_condition_clause_(root, list);

	/* accumulation */
	Return(loop_symbol_accumulation_p_(pos, &check));
	if (check)
		return loop_parse_accumulation_clause_(root, list);

	/* termination */
	Return(loop_symbol_termination_p_(pos, &check));
	if (check)
		return loop_parse_termination_clause_(root, list);

	/* initial */
	Return(loop_symbol_initial_final_p_(pos, &check));
	if (check)
		return loop_parse_initial_final_clause_(root, list);

	return 0;
}

static int loop_parse_main_clause_(addr *root, addr *list)
{
	addr check;

	for (;;) {
		check = *root;
		Return(loop_parse_main_clause_update_(root, list));
		if (check == *root)
			break;
	}

	return 0;
}


/*
 *  main
 */
_g int loop_parse_common(Execute ptr, addr *named, addr *vars, addr *main, addr *list)
{
	Return(loop_parse_named_clause_(named, list));
	Return(loop_parse_variable_clause_(ptr, vars, list));
	Return(loop_parse_main_clause_(main, list));
	nreverse(vars, *vars);
	nreverse(main, *main);

	return 0;
}

