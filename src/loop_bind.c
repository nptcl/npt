#include "condition.h"
#include "execute.h"
#include "hold.h"
#include "loop_bind.h"
#include "object.h"
#include "symbol.h"
#include "subtypep.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_table.h"
#include "type_typep.h"

/*
 *  loop-bind
 */
static int loop_subtypep_(Execute ptr, addr a, addr b, int *ret)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, a, b, NULL);
	if (GetType(a) != LISPTYPE_TYPE) {
		Return(parse_type_(ptr, &a, a, Nil));
		localhold_push(hold, a);
	}
	if (GetType(b) != LISPTYPE_TYPE) {
		Return(parse_type_(ptr, &b, b, Nil));
		localhold_push(hold, b);
	}
	localhold_end(hold);
	Return(subtypep_check_(ptr, a, b, Nil, ret, NULL));

	return 0;
}

static int loop_bind_initial_(Execute ptr, addr type, addr *ret)
{
	int check;
	addr right;

	if (type == Unbound)
		return Result(ret, Nil);
	if (GetType(type) != LISPTYPE_TYPE) {
		Return(parse_type_(ptr, &type, type, Nil));
	}
	/* float */
	GetConst(COMMON_FLOAT, &right);
	Return(loop_subtypep_(ptr, type, right, &check));
	if (check) {
		single_float_heap(ret, 0.0f);
		return 0;
	}
	/* integer */
	GetConst(COMMON_NUMBER, &right);
	Return(loop_subtypep_(ptr, type, right, &check));
	if (check) {
		fixnum_heap(ret, 0);
		return 0;
	}
	else {
		/* others */
		return Result(ret, Nil);
	}
}

static int loop_bind_initial_recursive_(Execute ptr, addr var, addr type, addr *ret)
{
	addr var1, var2, type1, type2, value1, value2;
	LocalHold hold;

	/* variable */
	if (var == Nil)
		return Result(ret, Nil);
	if (! consp(var))
		return loop_bind_initial_(ptr, type, ret);
	GetCons(var, &var1, &var2);
	/* type */
	if (consp(type)) {
		GetCons(type, &type1, &type2);
		if (type1 == Nil)
			type1 = type;
		if (type2 == Nil)
			type2 = type;
	}
	else {
		type1 = type2 = type;
	}
	/* initial value */
	Return(loop_bind_initial_recursive_(ptr, var1, type1, &value1));
	hold = LocalHold_local_push(ptr, value1);
	Return(loop_bind_initial_recursive_(ptr, var2, type2, &value2));
	localhold_end(hold);
	cons_heap(ret, value1, value2);

	return 0;
}

int loop_bind_initial_list_(Execute ptr, addr var, addr type, addr *ret)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, var, type, NULL);
	Return(loop_bind_initial_recursive_(ptr, var, type, &var));
	localhold_end(hold);

	if (consp(var))
		quotelist_heap(&var, var);

	return Result(ret, var);
}

static int loop_typep_(Execute ptr, addr pos, addr value, addr type)
{
	int check;

	if (GetType(type) != LISPTYPE_TYPE) {
		Return(parse_type_(ptr, &type, type, Nil));
	}
	Return(typep_clang_(ptr, value, type, &check));
	if (! check) {
		Return(type_object_(&type, type));
		return fmte_("LOOP let ~A form ~S must be a ~A type.", pos, value, type, NULL);
	}

	return 0;
}

static int loop_bind_recursive_(Execute ptr, addr pos, addr type, addr value, addr *ret)
{
	addr pos1, pos2, type1, type2, value1, value2;
	LocalHold hold;

	Check(pos == Nil, "type error");
	/* symbol */
	if (! consp(pos)) {
		Check(! symbolp(pos), "type error");
		Return(loop_typep_(ptr, pos, value, type));
		return Result(ret, value);
	}
	/* cons */
	GetCons(pos, &pos1, &pos2);
	if (consp(type)) {
		GetCons(type, &type1, &type2);
		if (type1 == Nil)
			type1 = type;
		if (type2 == Nil)
			type2 = type;
	}
	else {
		type1 = type2 = type;
	}

	/* default */
	hold = LocalHold_local(ptr);
	if (value == Nil) {
		if (pos1 == Nil) {
			value1 = Nil;
		}
		else {
			Return(loop_bind_initial_(ptr, type1, &value1));
		}
		if (pos2 == Nil) {
			value2 = Nil;
		}
		else {
			Return(loop_bind_initial_(ptr, type2, &value2));
		}
	}
	/* cons */
	else if (consp(value)) {
		GetCons(value, &value1, &value2);
		if (pos1 == Nil) {
			value1 = Nil;
		}
		else {
			Return(loop_bind_recursive_(ptr, pos1, type1, value1, &value1));
		}
		localhold_push(hold, value1);
		if (pos2 == Nil) {
			value2 = Nil;
		}
		else {
			Return(loop_bind_recursive_(ptr, pos2, type2, value2, &value2));
		}
		localhold_push(hold, value2);
	}
	/* error */
	else {
		return fmte_("LOOP let ~A form ~S must be a list type.", pos, value, NULL);
	}
	/* type check */
	if (! listp(pos1)) {
		Return(loop_typep_(ptr, pos1, value1, type1));
	}
	if (! listp(pos2)) {
		Return(loop_typep_(ptr, pos2, value2, type2));
	}
	localhold_end(hold);
	cons_heap(ret, value1, value2);

	return 0;
}

int loop_bind_common_(Execute ptr, addr pos, addr type, addr value, addr *ret)
{
	LocalHold hold;

	if (pos == Nil)
		return Result(ret, Nil);
	if (! listp(pos))
		return fmte_("LIST-BIND argument ~S must be a list type.", pos, NULL);
	if (type == Unbound)
		pos = T;

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, pos, type, value, NULL);
	Return(loop_bind_recursive_(ptr, pos, type, value, ret));
	localhold_end(hold);

	return 0;
}

