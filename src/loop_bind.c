#include "condition.h"
#include "execute.h"
#include "loop_bind.h"
#include "object.h"
#include "symbol.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_subtypep.h"
#include "type_table.h"
#include "type_typep.h"

/*
 *  loop-bind
 */
static int loop_subtypep(Execute ptr, addr a, addr b, int *ret)
{
	int invalid;

	if (GetType(a) != LISPTYPE_TYPE) {
		if (parse_type(ptr, &a, a, Nil))
			return 1;
	}
	if (GetType(b) != LISPTYPE_TYPE) {
		if (parse_type(ptr, &b, b, Nil))
			return 1;
	}
	*ret = subtypep_clang(a, b, &invalid);

	return 0;
}

_g int loop_bind_initial(Execute ptr, addr type, addr *ret)
{
	int check;
	addr right;

	if (type == Unbound) {
		*ret = Nil;
		return 0;
	}
	if (GetType(type) != LISPTYPE_TYPE) {
		if (parse_type(ptr, &type, type, Nil))
			return 1;
	}
	/* float */
	GetTypeTable(&right, Float);
	if (loop_subtypep(ptr, type, right, &check))
		return 1;
	if (check) {
		single_float_heap(ret, 0.0f);
		return 0;
	}
	/* integer */
	GetTypeTable(&right, Number);
	if (loop_subtypep(ptr, type, right, &check))
		return 1;
	if (check) {
		fixnum_heap(ret, 0);
		return 0;
	}
	else {
		/* others */
		*ret = Nil;
		return 0;
	}
}

static int loop_bind_initial_recursive(Execute ptr, addr var, addr type, addr *ret)
{
	addr var1, var2, type1, type2, value1, value2;

	/* variable */
	if (var == Nil) {
		*ret = Nil;
		return 0;
	}
	if (! consp(var))
		return loop_bind_initial(ptr, type, ret);
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
	loop_bind_initial_recursive(ptr, var1, type1, &value1);
	loop_bind_initial_recursive(ptr, var2, type2, &value2);
	cons_heap(ret, value1, value2);

	return 0;
}

_g int loop_bind_initial_list(Execute ptr, addr var, addr type, addr *ret)
{
	if (loop_bind_initial_recursive(ptr, var, type, &var))
		return 1;
	if (consp(var))
		quotelist_heap(&var, var);
	*ret = var;

	return 0;
}

static int loop_typep(Execute ptr, addr pos, addr value, addr type)
{
	int check;

	if (GetType(type) != LISPTYPE_TYPE) {
		if (parse_type(ptr, &type, type, Nil))
			return 1;
	}
	if (typep_clang(value, type, &check))
		return 1;
	if (! check) {
		type_object(&type, type);
		fmte("LOOP let ~A form ~S must be a ~A type.", pos, value, type, NULL);
	}

	return 0;
}

static int loop_bind_recursive(Execute ptr, addr pos, addr type, addr value, addr *ret)
{
	addr pos1, pos2, type1, type2, value1, value2;

	Check(pos == Nil, "type error");
	/* symbol */
	if (! consp(pos)) {
		Check(! symbolp(pos), "type error");
		if (loop_typep(ptr, pos, value, type))
			return 1;
		*ret = value;
		return 0;
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
	if (value == Nil) {
		if (pos1 == Nil)
			value1 = Nil;
		else if (loop_bind_initial(ptr, type1, &value1))
			return 1;
		if (pos2 == Nil)
			value2 = Nil;
		else if (loop_bind_initial(ptr, type2, &value2))
			return 1;
	}
	/* cons */
	else if (consp(value)) {
		GetCons(value, &value1, &value2);
		if (pos1 == Nil)
			value1 = Nil;
		else if (loop_bind_recursive(ptr, pos1, type1, value1, &value1))
			return 1;
		if (pos2 == Nil)
			value2 = Nil;
		else if (loop_bind_recursive(ptr, pos2, type2, value2, &value2))
			return 1;
	}
	/* error */
	else {
		fmte("LOOP let ~A form ~S must be a list type.", pos, value, NULL);
		return 0;
	}
	/* type check */
	if (! listp(pos1)) {
		if (loop_typep(ptr, pos1, value1, type1))
			return 1;
	}
	if (! listp(pos2)) {
		if (loop_typep(ptr, pos2, value2, type2))
			return 1;
	}
	cons_heap(ret, value1, value2);

	return 0;
}

_g int loop_bind_common(Execute ptr, addr pos, addr type, addr value, addr *ret)
{
	if (pos == Nil) {
		*ret = Nil;
		return 0;
	}
	if (! listp(pos))
		fmte("LIST-BIND argument ~S must be a list type.", pos, NULL);
	if (type == Unbound)
		GetTypeTable(&pos, T);
	return loop_bind_recursive(ptr, pos, type, value, ret);
}

