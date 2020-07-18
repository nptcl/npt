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
	addr pos, args;

	/* check */
	*ret = Nil;
	if (! consp_getcons(*list, &pos, &args))
		return 0;
	if (! loop_symbol_named_p(pos))
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
	if (loop_symbol_equal_p(pos))
		goto loop_form;
	if (loop_symbol_form_p(pos))
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
	if (loop_symbol_equal_p(pos))
		goto loop_form;
	if (loop_symbol_form_p(pos))
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
	if (loop_symbol_form_p(pos))
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
		if (! loop_symbol_and_p(pos))
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
	addr root, pos, next;

	for (root = Nil; *list != Nil; ) {
		if (! consp_getcons(*list, &pos, &next))
			return fmte_("Invalid loop form ~S.", *list, NULL);
		if (loop_symbol_form_p(pos))
			break;
		cons_heap(&root, pos, root);
		*list = next;
	}
	nreverse(ret, root);

	return 0;
}

static int loop_parse_initial_final_clause_(addr *root, addr *list)
{
	addr key, pos;

	/* symbol */
	Return_getcons(*list, &pos, list);
	if (loop_symbol_initially_p(pos))
		GetConst(SYSTEM_LOOP_INITIALLY, &key);
	else if (loop_symbol_finally_p(pos))
		GetConst(SYSTEM_LOOP_FINALLY, &key);
	else {
		return fmte_("Invalid value ~S.", pos, NULL);
	}

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
	*ret = 0;
loop:
	if (loop_symbol_arithmetic1_p(pos)) {
		if (str->from1 != Unbound)
			return fmte_("FOR-AS FROM expr already exists.", NULL);
		str->from1 = pos;
		if (! consp_getcons(*list, &(str->from2), list))
			return Result(ret, 1);
	}
	else if (loop_symbol_arithmetic2_p(pos)) {
		if (str->to1 != Unbound)
			return fmte_("FOR-AS TO expr already exists.", NULL);
		str->to1 = pos;
		if (! consp_getcons(*list, &(str->to2), list))
			return Result(ret, 1);
	}
	else if (loop_symbol_by_p(pos)) {
		if (str->by != Unbound)
			return fmte_("FOR-AS BY expr already exists.", NULL);
		if (! consp_getcons(*list, &(str->by), list))
			return Result(ret, 1);
	}
	else {
		return Result(ret, 1);
	}
	if (*list == Nil)
		return Result(ret, 0);
	if (! consp_getcar(*list, &pos))
		return Result(ret, 1);
	if (! loop_symbol_arithmetic_p(pos))
		return Result(ret, 0);
	GetCdr(*list, list);
	goto loop;
}

static int loop_parse_for_as_arithmetic1_p(struct for_as_arithmetic *str)
{
	int check1, check2, check3, a, b, c;
	addr x, y, z;

	x = str->from1;
	y = str->to1;
	z = str->by;
	check1 = (x == Unbound);
	check2 = (y == Unbound);
	check3 = (z == Unbound);
	if (check1 && check2 && check3)
		return 0;
	/* from */
	if (! check1) {
		a = ! loop_symbol_from_p(x);
		b = ! loop_symbol_upfrom_p(x);
		if (a && b)
			return 0;
	}
	/* to */
	if (! check2) {
		a = ! loop_symbol_to_p(y);
		b = ! loop_symbol_upto_p(y);
		c = ! loop_symbol_below_p(y);
		if (a && b && c)
			return 0;
	}

	return 1;
}

static int loop_parse_for_as_arithmetic2_p(struct for_as_arithmetic *str)
{
	int check1, check2, a, b;
	addr x, y;

	x = str->from1;
	y = str->to1;
	check1 = (x == Unbound);
	check2 = (y == Unbound);
	/* from */
	if (check1)
		return 0;
	if (! loop_symbol_from_p(x))
		return 0;
	/* to */
	if (check2)
		return 0;
	a = ! loop_symbol_downto_p(y);
	b = ! loop_symbol_above_p(y);
	if (a && b)
		return 0;

	return 1;
}

static int loop_parse_for_as_arithmetic3_p(struct for_as_arithmetic *str)
{
	int check1, check2, a, b, c;
	addr x, y;

	x = str->from1;
	y = str->to1;
	check1 = (x == Unbound);
	check2 = (y == Unbound);
	/* from */
	if (check1)
		return 0;
	if (! loop_symbol_downfrom_p(x))
		return 0;
	/* to */
	if (! check2) {
		a = ! loop_symbol_to_p(y);
		b = ! loop_symbol_downto_p(y);
		c = ! loop_symbol_above_p(y);
		if (a && b && c)
			return 0;
	}

	return 1;
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
	if (! loop_symbol_arithmetic_p(pos)) {
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
		if (! loop_symbol_arithmetic_p(pos))
			return Result(ret, 0);
	}
	Return(loop_parse_for_as_arithmetic_struct_(&str, &args, pos, &check));
	if (check)
		return Result(ret, 0);
	if (loop_parse_for_as_arithmetic1_p(&str))
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP, &pos);
	else if (loop_parse_for_as_arithmetic2_p(&str))
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO, &pos);
	else if (loop_parse_for_as_arithmetic3_p(&str))
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM, &pos);
	else
		return Result(ret, 0);
	/* gensym */
	Return(make_gensym_(ptr, &g1));
	Return(make_gensym_(ptr, &g2));
	/* result */
	list_heap(value, pos, var,
			str.from1, str.from2, str.to1, str.to2, str.by, g1, g2, NULL);
	*list = args;
	return Result(ret, 1);
}

static int loop_parse_for_as_call_list_(Execute ptr, addr *list, addr *value, int *ret,
		int (*check1)(addr), int (*check2)(addr), constindex index)
{
	/* var [type-spec] in form [by step] */
	addr args, var, type, form, step, pos, g;

	type = step = Unbound;
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (! (*check1)(pos)) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
		if (! (*check1)(pos))
			return Result(ret, 0);
	}
	if (! consp_getcons(args, &form, &args))
		return Result(ret, 0);
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		return Result(ret, 0);
	if (! (*check2)(pos))
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
			loop_symbol_in_p, loop_symbol_by_p,
			CONSTANT_SYSTEM_LOOP_FOR_AS_IN_LIST);
}

static int loop_parse_for_as_on_list_(Execute ptr,
		addr *list, addr *value, int *ret)
{
	return loop_parse_for_as_call_list_(ptr, list, value, ret,
			loop_symbol_on_p, loop_symbol_by_p,
			CONSTANT_SYSTEM_LOOP_FOR_AS_ON_LIST);
}

static int loop_parse_for_as_equals_then_(Execute ptr,
		addr *list, addr *value, int *ret)
{
	return loop_parse_for_as_call_list_(ptr, list, value, ret,
			loop_symbol_equal_p, loop_symbol_then_p,
			CONSTANT_SYSTEM_LOOP_FOR_AS_EQUALS_THEN);
}

static int loop_parse_for_as_across_(Execute ptr, addr *list, addr *value, int *ret)
{
	/* var [type-spec] across vector */
	addr args, var, type, vector, pos, g1, g2, g3;

	type = Unbound;
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (! loop_symbol_across_p(pos)) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
		if (! loop_symbol_across_p(pos))
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
	addr var, type, keyp, table, use, args, pos, g;

	type = use = Unbound;
	/* var */
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	/* type-spec, being */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (! loop_symbol_being_p(pos)) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
		if (! loop_symbol_being_p(pos))
			return Result(ret, 0);
	}
	/* each, the */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (! loop_symbol_each_the_p(pos))
		return Result(ret, 0);
	/* hash-key, hash-keys, hash-value, hash-values */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (loop_symbol_hash_key2_p(pos))
		keyp = T;
	else if (loop_symbol_hash_value2_p(pos))
		keyp = Nil;
	else
		return Result(ret, 0);
	/* in, of */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (! loop_symbol_in_of_p(pos))
		return Result(ret, 0);
	/* table */
	if (! consp_getcons(args, &table, &args))
		return Result(ret, 0);
	/* using */
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		return Result(ret, 0);
	if (! loop_symbol_using_p(pos))
		goto result;
	/* hash-key, hash-value */
	GetCdr(args, &args);
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (! consp_getcons(pos, &pos, &use))
		return Result(ret, 0);
	if (loop_symbol_hash_key_p(pos)) {
		if (keyp == T)
			return Result(ret, 0);
	}
	else if (loop_symbol_hash_value_p(pos)) {
		if (keyp == Nil)
			return Result(ret, 0);
	}
	else
		return Result(ret, 0);
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
	int symbolp, presentp, externalp;
	addr var, type, package, args, pos, g;

	symbolp = presentp = externalp = 0;
	type = package = Unbound;
	/* var */
	if (! consp_getcons(*list, &var, &args))
		return Result(ret, 0);
	/* type-spec, being */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (! loop_symbol_being_p(pos)) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return Result(ret, 0);
		if (! loop_symbol_being_p(pos))
			return Result(ret, 0);
	}
	/* each, the */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (! loop_symbol_each_the_p(pos))
		return Result(ret, 0);
	/* { symbol | symbols |
	 *   present-symbol | present-symbols |
	 *   external-symbol | external-symbols }
	 */
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (loop_symbol_symbol2_p(pos))
		symbolp = 1;
	else if (loop_symbol_present_symbol2_p(pos))
		presentp = 1;
	else if (loop_symbol_external_symbol2_p(pos))
		externalp = 1;
	else
		return Result(ret, 0);
	/* in, of */
	if (args == Nil)
		goto result;
	if (! consp_getcons(args, &pos, &args))
		return Result(ret, 0);
	if (! loop_symbol_in_of_p(pos))
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
		if (! loop_symbol_and_p(pos))
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
	addr pos;

	if (! consp_getcar(*list, &pos))
		return 0;

	/* with */
	if (loop_symbol_with_p(pos))
		return loop_parse_with_clause_(root, list);

	/* initial */
	if (loop_symbol_initial_final_p(pos))
		return loop_parse_initial_final_clause_(root, list);

	/* for_as */
	if (loop_symbol_for_as_p(pos))
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
	addr key, pos;

	Return_getcons(*list, &pos, list);
	if (loop_symbol_do_p(pos)) {
		GetConst(SYSTEM_LOOP_DO, &key);
		Return(loop_parse_form_variables_(list, &pos));
		cons_heap(ret, key, pos);
	}
	else if (loop_symbol_return_p(pos)) {
		if (! consp_getcons(*list, &pos, list))
			return fmte_("Invalid RETURN form ~S in loop.", *list, NULL);
		if (loop_symbol_it_p(pos))
			GetConst(SYSTEM_IT_LOOP, &pos);
		GetConst(SYSTEM_LOOP_RETURN, &key);
		list_heap(ret, key, pos, NULL);
	}
	else {
		return fmte_("Invalid value ~S.", pos, NULL);
	}

	return 0;
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
	addr args, pos, key, form, expr1, expr2;

	Return_getcons(*list, &pos, &args);
	if (loop_symbol_if_p(pos) || loop_symbol_when_p(pos))
		GetConst(SYSTEM_LOOP_IF, &key);
	else if (loop_symbol_unless_p(pos))
		GetConst(SYSTEM_LOOP_UNLESS, &key);
	else
		goto error;
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
	if (! loop_symbol_else_p(pos))
		goto result;
	GetCdr(args, &args);
	Return(loop_condition_selectable_result_(&expr2, &args));
	/* end */
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		goto error;
	if (! loop_symbol_end_p(pos))
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
	addr pos, key, args, form, into;

	/* first */
	Return_getcons(*list, &pos, &args);
	into = Unbound;
	if (loop_symbol_collect_p(pos))
		GetConst(SYSTEM_LOOP_COLLECT, &key);
	else if (loop_symbol_append_p(pos))
		GetConst(SYSTEM_LOOP_APPEND, &key);
	else if (loop_symbol_nconc_p(pos))
		GetConst(SYSTEM_LOOP_NCONC, &key);
	else
		goto error;
	/* form, it */
	if (! consp_getcons(args, &form, &args))
		goto error;
	if (loop_symbol_it_p(form))
		GetConst(SYSTEM_IT_LOOP, &form);
	if (! consp_getcar(args, &pos))
		goto result;
	if (! loop_symbol_into_p(pos))
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
	addr pos, key, args, form, into, type;

	/* first */
	Return_getcons(*list, &pos, &args);
	into = type = Unbound;
	if (loop_symbol_count_p(pos))
		GetConst(SYSTEM_LOOP_COUNT, &key);
	else if (loop_symbol_sum_p(pos))
		GetConst(SYSTEM_LOOP_SUM, &key);
	else if (loop_symbol_maximize_p(pos))
		GetConst(SYSTEM_LOOP_MAXIMIZE, &key);
	else if (loop_symbol_minimize_p(pos))
		GetConst(SYSTEM_LOOP_MINIMIZE, &key);
	else
		goto error;
	/* form, it */
	if (! consp_getcons(args, &form, &args))
		goto error;
	if (loop_symbol_it_p(form))
		GetConst(SYSTEM_IT_LOOP, &form);
	if (! consp_getcar(args, &pos))
		goto result;
	if (! loop_symbol_into_p(pos))
		goto result;
	GetCdr(args, &args);
	if (! consp_getcons(args, &into, &args))
		goto error;
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		goto error;
	if (loop_symbol_form_main_p(pos))
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
	addr pos;

	GetCar(*list, &pos);
	if (loop_symbol_list_accumulation_p(pos))
		return loop_parse_list_accumulation_result_(ret, list);
	else if (loop_symbol_numeric_accumulation_p(pos))
		return loop_parse_numeric_accumulation_result_(ret, list);
	else
		return fmte_("Invalid accumulation symbol ~S.", pos, NULL);
}

static int loop_parse_accumulation_clause_(addr *root, addr *list)
{
	addr pos;

	Return(loop_parse_accumulation_result_(&pos, list));
	cons_heap(root, pos, *root);

	return 0;
}

static int loop_parse_selectable_p(addr list)
{
	addr pos;

	if (! consp_getcar(list, &pos))
		return 0;
	return loop_symbol_condition_p(pos)
		|| loop_symbol_uncondition_p(pos)
		|| loop_symbol_accumulation_p(pos);
}

static int loop_condition_selectable_result_(addr *ret, addr *list)
{
	addr root, pos;

	for (root = Nil; loop_parse_selectable_p(*list); ) {
		GetCar(*list, &pos);
		if (loop_symbol_uncondition_p(pos)) {
			Return(loop_parse_uncondition_result_(&pos, list));
		}
		else if (loop_symbol_condition_p(pos)) {
			Return(loop_parse_condition_result_(&pos, list));
		}
		else if (loop_symbol_accumulation_p(pos)) {
			Return(loop_parse_accumulation_result_(&pos, list));
		}
		else {
			return fmte_("Invalid loop form ~S.", *list, NULL);
		}
		cons_heap(&root, pos, root);
		/* and */
		if (! consp_getcar(*list, &pos))
			break;
		if (! loop_symbol_and_p(pos))
			break;
		GetCdr(*list, list);
	}
	nreverse(ret, root);

	return 0;
}

static int loop_parse_termination_clause_(addr *root, addr *list)
{
	addr symbol, pos, args, key, a, b;

	Return_getcons(*list, &symbol, &args);
	if (loop_symbol_while_p(symbol))
		GetConst(SYSTEM_LOOP_WHILE, &key);
	else if (loop_symbol_until_p(symbol))
		GetConst(SYSTEM_LOOP_UNTIL, &key);
	else if (loop_symbol_repeat_p(symbol))
		GetConst(SYSTEM_LOOP_REPEAT, &key);
	else if (loop_symbol_always_p(symbol))
		GetConst(SYSTEM_LOOP_ALWAYS, &key);
	else if (loop_symbol_never_p(symbol))
		GetConst(SYSTEM_LOOP_NEVER, &key);
	else if (loop_symbol_thereis_p(symbol))
		GetConst(SYSTEM_LOOP_THEREIS, &key);
	else
		goto error;
	if (! consp_getcons(args, &pos, &args))
		goto error;
	if (loop_symbol_repeat_p(symbol)) {
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
	addr pos;

	if (! consp_getcar(*list, &pos))
		return 0;

	/* uncondition */
	if (loop_symbol_uncondition_p(pos))
		return loop_parse_uncondition_clause_(root, list);

	/* condition */
	if (loop_symbol_condition_p(pos))
		return loop_parse_condition_clause_(root, list);

	/* accumulation */
	if (loop_symbol_accumulation_p(pos))
		return loop_parse_accumulation_clause_(root, list);

	/* termination */
	if (loop_symbol_termination_p(pos))
		return loop_parse_termination_clause_(root, list);

	/* initial */
	if (loop_symbol_initial_final_p(pos))
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

