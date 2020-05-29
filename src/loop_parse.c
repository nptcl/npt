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
static void loop_parse_named_clause(addr *ret, addr *list)
{
	addr pos, args;

	/* check */
	*ret = Nil;
	if (! consp_getcons(*list, &pos, &args))
		return;
	if (! loop_symbol_named_p(pos))
		return;

	/* parse */
	if (! consp_getcons(args, &pos, &args))
		fmte("NAMED clause must be a name argument in loop.", NULL);
	if (! symbolp(pos))
		fmte("NAMED argument ~S must be a symbol type.", pos, NULL);

	/* result */
	*ret = pos;
	*list = args;
}

static void loop_parse_with_variable(addr pos)
{
	addr a, b;

	if (symbolp(pos))
		return;
	if (! consp_getcons(pos, &a, &b))
		fmte("The value ~S must be a symbol type.", pos, NULL);
	loop_parse_with_variable(a);
	loop_parse_with_variable(b);
}

static void loop_parse_with_clause1(addr *list, addr *ret)
{
	addr var, type, form, args, pos;

	args = *list;
	type = form = Unbound;
	/* var */
	GetCons(args, &var, &args);
	loop_parse_with_variable(var);
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
	*list = args;
	return;

error:
	fmte("Invalid WITH form ~S in loop.", *list, NULL);
	*list = *ret = Nil;
}

static void loop_parse_with_clause(addr *root, addr *list)
{
	addr args, vars, pos;

	/* loop */
	getcdr(*list, &args);
	vars = Nil;
	if (! consp(args))
		goto error;
	for (;;) {
		/* push */
		loop_parse_with_clause1(&args, &pos);
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
	*list = args;
	return;

error:
	fmte("Invalid WITH form ~S in loop.", *list);
}

static void loop_parse_form_variables(addr *list, addr *ret)
{
	addr root, pos, next;

	for (root = Nil; *list != Nil; ) {
		if (! consp_getcons(*list, &pos, &next))
			fmte("Invalid loop form ~S.", *list, NULL);
		if (loop_symbol_form_p(pos))
			break;
		cons_heap(&root, pos, root);
		*list = next;
	}
	nreverse(ret, root);
}

static void loop_parse_initial_final_clause(addr *root, addr *list)
{
	addr key, pos;

	/* symbol */
	getcons(*list, &pos, list);
	if (loop_symbol_initially_p(pos))
		GetConst(SYSTEM_LOOP_INITIALLY, &key);
	else if (loop_symbol_finally_p(pos))
		GetConst(SYSTEM_LOOP_FINALLY, &key);
	else {
		fmte("Invalid value ~S.", pos, NULL);
		return;
	}

	/* compound-form-variables */
	loop_parse_form_variables(list, &pos);
	cons_heap(&pos, key, pos);
	cons_heap(root, pos, *root);
}

struct for_as_arithmetic {
	addr from1, from2, to1, to2, by;
};

static void clear_for_as_arithmetic(struct for_as_arithmetic *str)
{
	str->from1 = str->from2 = str->to1 = str->to2 = str->by = Unbound;
}

static int loop_parse_for_as_arithmetic_struct(
		struct for_as_arithmetic *str, addr *list, addr pos)
{
loop:
	if (loop_symbol_arithmetic1_p(pos)) {
		if (str->from1 != Unbound)
			fmte("FOR-AS FROM expr already exists.", NULL);
		str->from1 = pos;
		if (! consp_getcons(*list, &(str->from2), list))
			return 1;
	}
	else if (loop_symbol_arithmetic2_p(pos)) {
		if (str->to1 != Unbound)
			fmte("FOR-AS TO expr already exists.", NULL);
		str->to1 = pos;
		if (! consp_getcons(*list, &(str->to2), list))
			return 1;
	}
	else if (loop_symbol_by_p(pos)) {
		if (str->by != Unbound)
			fmte("FOR-AS BY expr already exists.", NULL);
		if (! consp_getcons(*list, &(str->by), list))
			return 1;
	}
	else {
		return 1;
	}
	if (*list == Nil)
		return 0;
	if (! consp_getcar(*list, &pos))
		return 1;
	if (! loop_symbol_arithmetic_p(pos))
		return 0;
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

static int loop_parse_for_as_arithmetic(Execute ptr, addr *list, addr *ret)
{
	addr var, args, pos, g1, g2;
	struct for_as_arithmetic str;

	clear_for_as_arithmetic(&str);
	if (! consp_getcons(*list, &var, &args))
		return 0;
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (! loop_symbol_arithmetic_p(pos)) {
		if (! consp_getcons(args, &pos, &args))
			return 0;
		if (! loop_symbol_arithmetic_p(pos))
			return 0;
	}
	if (loop_parse_for_as_arithmetic_struct(&str, &args, pos))
		return 0;
	if (loop_parse_for_as_arithmetic1_p(&str))
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP, &pos);
	else if (loop_parse_for_as_arithmetic2_p(&str))
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO, &pos);
	else if (loop_parse_for_as_arithmetic3_p(&str))
		GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM, &pos);
	else
		return 0;
	/* gensym */
	make_gensym(ptr, &g1);
	make_gensym(ptr, &g2);
	/* result */
	list_heap(ret, pos, var,
			str.from1, str.from2, str.to1, str.to2, str.by, g1, g2, NULL);
	*list = args;
	return 1;
}

static int loop_parse_for_as_call_list(Execute ptr, addr *list, addr *ret,
		int (*check1)(addr), int (*check2)(addr), constindex index)
{
	/* var [type-spec] in form [by step] */
	addr args, var, type, form, step, pos, g;

	type = step = Unbound;
	if (! consp_getcons(*list, &var, &args))
		return 0;
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (! (*check1)(pos)) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return 0;
		if (! (*check1)(pos))
			return 0;
	}
	if (! consp_getcons(args, &form, &args))
		return 0;
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		return 0;
	if (! (*check2)(pos))
		goto result;
	GetCdr(args, &args);
	if (! consp_getcons(args, &step, &args))
		return 0;
	goto result;

result:
	make_gensym(ptr, &g);
	GetConstant(index, &pos);
	list_heap(ret, pos, var, type, form, step, g, NULL);
	*list = args;
	return 1;
}

static int loop_parse_for_as_in_list(Execute ptr, addr *list, addr *ret)
{
	return loop_parse_for_as_call_list(ptr, list, ret,
			loop_symbol_in_p, loop_symbol_by_p,
			CONSTANT_SYSTEM_LOOP_FOR_AS_IN_LIST);
}

static int loop_parse_for_as_on_list(Execute ptr, addr *list, addr *ret)
{
	return loop_parse_for_as_call_list(ptr, list, ret,
			loop_symbol_on_p, loop_symbol_by_p,
			CONSTANT_SYSTEM_LOOP_FOR_AS_ON_LIST);
}

static int loop_parse_for_as_equals_then(Execute ptr, addr *list, addr *ret)
{
	return loop_parse_for_as_call_list(ptr, list, ret,
			loop_symbol_equal_p, loop_symbol_then_p,
			CONSTANT_SYSTEM_LOOP_FOR_AS_EQUALS_THEN);
}

static int loop_parse_for_as_across(Execute ptr, addr *list, addr *ret)
{
	/* var [type-spec] across vector */
	addr args, var, type, vector, pos, g1, g2, g3;

	type = Unbound;
	if (! consp_getcons(*list, &var, &args))
		return 0;
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (! loop_symbol_across_p(pos)) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return 0;
		if (! loop_symbol_across_p(pos))
			return 0;
	}
	if (! consp_getcons(args, &vector, &args))
		return 0;
	/* result */
	make_gensym(ptr, &g1);
	make_gensym(ptr, &g2);
	make_gensym(ptr, &g3);
	GetConst(SYSTEM_LOOP_FOR_AS_ACROSS, &pos);
	list_heap(ret, pos, var, type, vector, g1, g2, g3, NULL);
	*list = args;
	return 1;
}

static int loop_parse_for_as_hash(Execute ptr, addr *list, addr *ret)
{
	/* var [type-spec] being {each|the}
	 *     { hash-key | hash-keys | hash-value | hash-values }
	 *     {in|of} table [using ({hash-key|hash-value} var2)]
	 */
	addr var, type, keyp, table, use, args, pos, g;

	type = use = Unbound;
	/* var */
	if (! consp_getcons(*list, &var, &args))
		return 0;
	/* type-spec, being */
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (! loop_symbol_being_p(pos)) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return 0;
		if (! loop_symbol_being_p(pos))
			return 0;
	}
	/* each, the */
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (! loop_symbol_each_the_p(pos))
		return 0;
	/* hash-key, hash-keys, hash-value, hash-values */
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (loop_symbol_hash_key2_p(pos))
		keyp = T;
	else if (loop_symbol_hash_value2_p(pos))
		keyp = Nil;
	else
		return 0;
	/* in, of */
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (! loop_symbol_in_of_p(pos))
		return 0;
	/* table */
	if (! consp_getcons(args, &table, &args))
		return 0;
	/* using */
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		return 0;
	if (! loop_symbol_using_p(pos))
		goto result;
	/* hash-key, hash-value */
	GetCdr(args, &args);
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (! consp_getcons(pos, &pos, &use))
		return 0;
	if (loop_symbol_hash_key_p(pos)) {
		if (keyp == T)
			return 0;
	}
	else if (loop_symbol_hash_value_p(pos)) {
		if (keyp == Nil)
			return 0;
	}
	else
		return 0;
	/* var2 */
	if (! consp_getcons(use, &use, &pos))
		return 0;
	if (pos != Nil)
		return 0;
	goto result;

result:
	make_gensym(ptr, &g);
	GetConst(SYSTEM_LOOP_FOR_AS_HASH, &pos);
	list_heap(ret, pos, var, type, keyp, table, use, g, NULL);
	*list = args;
	return 1;
}

static int loop_parse_for_as_package(Execute ptr, addr *list, addr *ret)
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
		return 0;
	/* type-spec, being */
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (! loop_symbol_being_p(pos)) {
		type = pos;
		if (! consp_getcons(args, &pos, &args))
			return 0;
		if (! loop_symbol_being_p(pos))
			return 0;
	}
	/* each, the */
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (! loop_symbol_each_the_p(pos))
		return 0;
	/* { symbol | symbols |
	 *   present-symbol | present-symbols |
	 *   external-symbol | external-symbols }
	 */
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (loop_symbol_symbol2_p(pos))
		symbolp = 1;
	else if (loop_symbol_present_symbol2_p(pos))
		presentp = 1;
	else if (loop_symbol_external_symbol2_p(pos))
		externalp = 1;
	else
		return 0;
	/* in, of */
	if (args == Nil)
		goto result;
	if (! consp_getcons(args, &pos, &args))
		return 0;
	if (! loop_symbol_in_of_p(pos))
		return 0;
	/* package */
	if (! consp_getcons(args, &package, &args))
		return 0;
	goto result;

result:
	if (symbolp)
		GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL, &pos);
	else if (presentp)
		GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT, &pos);
	else if (externalp)
		GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL, &pos);
	else
		return 0;
	/* result */
	make_gensym(ptr, &g);
	list_heap(ret, pos, var, type, package, g, NULL);
	*list = args;
	return 1;
}

static int loop_parse_for_as_clause1(Execute ptr, addr *list, addr *ret)
{
	return loop_parse_for_as_arithmetic(ptr, list, ret)
		|| loop_parse_for_as_in_list(ptr, list, ret)
		|| loop_parse_for_as_on_list(ptr, list, ret)
		|| loop_parse_for_as_equals_then(ptr, list, ret)
		|| loop_parse_for_as_across(ptr, list, ret)
		|| loop_parse_for_as_hash(ptr, list, ret)
		|| loop_parse_for_as_package(ptr, list, ret);
}

static void loop_parse_for_as_clause(Execute ptr, addr *root, addr *list)
{
	addr args, vars, pos;

	/* loop */
	getcdr(*list, &args);
	vars = Nil;
	if (! consp(args))
		goto error;
	for (;;) {
		/* push */
		if (! loop_parse_for_as_clause1(ptr, &args, &pos))
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
	*list = args;
	return;

error:
	fmte("Invalid FOR-AS form ~S in loop.", *list, NULL);
}

static void loop_parse_variable_clause_update(Execute ptr, addr *root, addr *list)
{
	addr pos;

	if (! consp_getcar(*list, &pos))
		return;

	/* with */
	if (loop_symbol_with_p(pos)) {
		loop_parse_with_clause(root, list);
		return;
	}

	/* initial */
	if (loop_symbol_initial_final_p(pos)) {
		loop_parse_initial_final_clause(root, list);
		return;
	}

	/* for_as */
	if (loop_symbol_for_as_p(pos)) {
		loop_parse_for_as_clause(ptr, root, list);
		return;
	}
}

static void loop_parse_variable_clause(Execute ptr, addr *root, addr *list)
{
	addr check;

	for (;;) {
		check = *root;
		loop_parse_variable_clause_update(ptr, root, list);
		if (check == *root)
			break;
	}
}

static void loop_parse_uncondition_result(addr *ret, addr *list)
{
	addr key, pos;

	getcons(*list, &pos, list);
	if (loop_symbol_do_p(pos)) {
		GetConst(SYSTEM_LOOP_DO, &key);
		loop_parse_form_variables(list, &pos);
		cons_heap(ret, key, pos);
	}
	else if (loop_symbol_return_p(pos)) {
		if (! consp_getcons(*list, &pos, list))
			fmte("Invalid RETURN form ~S in loop.", *list, NULL);
		if (loop_symbol_it_p(pos))
			GetConst(SYSTEM_IT_LOOP, &pos);
		GetConst(SYSTEM_LOOP_RETURN, &key);
		list_heap(ret, key, pos, NULL);
	}
	else {
		fmte("Invalid value ~S.", pos, NULL);
	}
}

static void loop_parse_uncondition_clause(addr *root, addr *list)
{
	addr pos;
	loop_parse_uncondition_result(&pos, list);
	cons_heap(root, pos, *root);
}

static void loop_condition_selectable_result(addr *ret, addr *list);
static void loop_parse_condition_result(addr *ret, addr *list)
{
	addr args, pos, key, form, expr1, expr2;

	getcons(*list, &pos, &args);
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
	loop_condition_selectable_result(&expr1, &args);
	/* else */
	if (args == Nil)
		goto result;
	if (! consp_getcar(args, &pos))
		goto error;
	if (! loop_symbol_else_p(pos))
		goto result;
	GetCdr(args, &args);
	loop_condition_selectable_result(&expr2, &args);
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
	*list = args;
	return;

error:
	fmte("Invalid loop form ~S.", *list, NULL);
}

static void loop_parse_condition_clause(addr *root, addr *list)
{
	addr pos;
	loop_parse_condition_result(&pos, list);
	cons_heap(root, pos, *root);
}

static void loop_parse_list_accumulation_result(addr *ret, addr *list)
{
	addr pos, key, args, form, into;

	/* first */
	getcons(*list, &pos, &args);
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
	*list = args;
	return;

error:
	fmte("Invalid loop form ~S.", *list, NULL);
}

static void loop_parse_numeric_accumulation_result(addr *ret, addr *list)
{
	addr pos, key, args, form, into, type;

	/* first */
	getcons(*list, &pos, &args);
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
	*list = args;
	return;

error:
	fmte("Invalid loop form ~S.", *list, NULL);
}

static void loop_parse_accumulation_result(addr *ret, addr *list)
{
	addr pos;

	GetCar(*list, &pos);
	if (loop_symbol_list_accumulation_p(pos))
		loop_parse_list_accumulation_result(ret, list);
	else if (loop_symbol_numeric_accumulation_p(pos))
		loop_parse_numeric_accumulation_result(ret, list);
	else
		fmte("Invalid accumulation symbol ~S.", pos, NULL);
}

static void loop_parse_accumulation_clause(addr *root, addr *list)
{
	addr pos;
	loop_parse_accumulation_result(&pos, list);
	cons_heap(root, pos, *root);
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

static void loop_condition_selectable_result(addr *ret, addr *list)
{
	addr root, pos;

	for (root = Nil; loop_parse_selectable_p(*list); ) {
		GetCar(*list, &pos);
		if (loop_symbol_uncondition_p(pos))
			loop_parse_uncondition_result(&pos, list);
		else if (loop_symbol_condition_p(pos))
			loop_parse_condition_result(&pos, list);
		else if (loop_symbol_accumulation_p(pos))
			loop_parse_accumulation_result(&pos, list);
		else
			fmte("Invalid loop form ~S.", *list, NULL);
		cons_heap(&root, pos, root);
		/* and */
		if (! consp_getcar(*list, &pos))
			break;
		if (! loop_symbol_and_p(pos))
			break;
		GetCdr(*list, list);
	}
	nreverse(ret, root);
}

static void loop_parse_termination_clause(addr *root, addr *list)
{
	addr symbol, pos, args, key, a, b;

	getcons(*list, &symbol, &args);
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
	*list = args;
	return;

error:
	fmte("Invalid loop form ~S.", *list, NULL);
}

static void loop_parse_main_clause_update(addr *root, addr *list)
{
	addr pos;

	if (! consp_getcar(*list, &pos))
		return;

	/* uncondition */
	if (loop_symbol_uncondition_p(pos)) {
		loop_parse_uncondition_clause(root, list);
		return;
	}

	/* condition */
	if (loop_symbol_condition_p(pos)) {
		loop_parse_condition_clause(root, list);
		return;
	}

	/* accumulation */
	if (loop_symbol_accumulation_p(pos)) {
		loop_parse_accumulation_clause(root, list);
		return;
	}

	/* termination */
	if (loop_symbol_termination_p(pos)) {
		loop_parse_termination_clause(root, list);
		return;
	}

	/* initial */
	if (loop_symbol_initial_final_p(pos)) {
		loop_parse_initial_final_clause(root, list);
		return;
	}
}

static void loop_parse_main_clause(addr *root, addr *list)
{
	addr check;

	for (;;) {
		check = *root;
		loop_parse_main_clause_update(root, list);
		if (check == *root)
			break;
	}
}


/*
 *  main
 */
_g void loop_parse_common(Execute ptr, addr *named, addr *vars, addr *main, addr *list)
{
	loop_parse_named_clause(named, list);
	loop_parse_variable_clause(ptr, vars, list);
	loop_parse_main_clause(main, list);
	nreverse(vars, *vars);
	nreverse(main, *main);
}

