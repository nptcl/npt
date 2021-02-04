#include "condition.h"
#include "hold.h"
#include "subtypep.h"
#include "subtypep_check.h"
#include "subtypep_compound.h"
#include "subtypep_number.h"
#include "subtypep_optimize.h"
#include "subtypep_table.h"
#include "subtypep_typedef.h"
#include "symbol.h"
#include "type.h"
#include "type_parse.h"

/*
 *  result
 */
static void subtypep_result_keyword(SubtypepResult value, addr *ret)
{
	switch (value) {
		case SUBTYPEP_INCLUDE:
			GetConst(SYSTEM_INCLUDE, ret);
			break;

		case SUBTYPEP_EXCLUDE:
			GetConst(SYSTEM_EXCLUDE, ret);
			break;

		case SUBTYPEP_FALSE:
			GetConst(SYSTEM_FALSE, ret);
			break;

		case SUBTYPEP_INVALID:
		default:
			GetConst(SYSTEM_INVALID, ret);
			break;
	}
}

static void subtypep_result_values(SubtypepResult value, int *ret, int *validp)
{
	int x, y;

	switch (value) {
		case SUBTYPEP_INCLUDE:
			x = 1; y = 1;
			break;

		case SUBTYPEP_FALSE:
		case SUBTYPEP_EXCLUDE:
			x = 0; y = 1;
			break;

		case SUBTYPEP_INVALID:
		default:
			x = 0; y = 0;
			break;
	}

	*ret = x;
	if (validp)
		*validp = y;
}


/*
 *  subtypep-atomic
 */
static int subtypep_parse_throw_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{
	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(parse_type(ptr, rx, x, env));
	localhold_set(hold, 0, x);
	Return(parse_type(ptr, ry, y, env));
	localhold_set(hold, 1, y);

	return 0;
}

static int subtypep_extend_atomic_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_throw_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_table_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-atomic-not
 */
static int subtypep_parse_optimize_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{
	LocalRoot local;

	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(parse_type(ptr, &x, x, env));
	localhold_set(hold, 0, x);
	Return(parse_type(ptr, &y, y, env));
	localhold_set(hold, 1, y);

	local = ptr->local;
	Return(type_optimize_throw_heap_(local, x, rx));
	Return(type_optimize_throw_heap_(local, y, ry));

	return 0;
}

static int subtypep_extend_atomic_not_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_optimize_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_atomic_not_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-compound
 */
static int subtypep_extend_compound_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_optimize_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-force-number
 */
static int subtypep_parse_force_number_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{
	LocalRoot local;

	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(parse_type(ptr, &x, x, env));
	localhold_set(hold, 0, x);
	Return(parse_type(ptr, &y, y, env));
	localhold_set(hold, 1, y);

	local = ptr->local;
	Return(type_subtypep_throw_heap_(local, x, rx));
	Return(type_subtypep_throw_heap_(local, y, ry));

	return 0;
}

static int subtypep_extend_force_number_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_force_number_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  subtypep-normal
 */
static int subtypep_parse_normal_type_(Execute ptr, addr x, addr env, addr *ret)
{
	LocalRoot local;

	local = ptr->local;
	Return(parse_type(ptr, &x, x, env));
	if (subtypep_number_p(x)) {
		Return(type_subtypep_throw_heap_(local, x, &x));
	}
	else {
		Return(type_optimize_throw_heap_(local, x, &x));
	}

	return Result(ret, x);
}

static int subtypep_parse_normal_(Execute ptr, LocalHold hold,
		addr x, addr y, addr env, addr *rx, addr *ry)
{

	localhold_set(hold, 0, x);
	localhold_set(hold, 1, y);
	localhold_set(hold, 2, env);

	Return(subtypep_parse_normal_type_(ptr, x, env, rx));
	localhold_set(hold, 0, x);
	Return(subtypep_parse_normal_type_(ptr, y, env, ry));
	localhold_set(hold, 1, y);

	return 0;
}

static int subtypep_extend_normal_(Execute ptr,
		addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_normal_(ptr, hold, x, y, env, &x, &y));
	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}


/*
 *  system:subtypep!
 */
static int subtypep_extend_value_(addr pos, enum SubtypepExtend *ret)
{
	addr check;

	if (pos == Nil || pos == Unbound)
		return Result(ret, SubtypepExtend_Normal);

	/* normal */
	GetConst(SYSTEM_SUBTYPEP_NORMAL, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_Normal);

	/* atomic */
	GetConst(SYSTEM_SUBTYPEP_ATOMIC, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_Atomic);

	/* atomic-not */
	GetConst(SYSTEM_SUBTYPEP_ATOMIC_NOT, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_AtomicNot);

	/* compound */
	GetConst(SYSTEM_SUBTYPEP_COMPOUND, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_Compound);

	/* force-number */
	GetConst(SYSTEM_SUBTYPEP_FORCE_NUMBER, &check);
	if (pos == check)
		return Result(ret, SubtypepExtend_ForceNumber);

	*ret = SubtypepExtend_Normal;
	return fmte_("Invalid subtypep! type ~S.", pos, NULL);
}

static int subtypep_extend_output_(Execute ptr, addr *ret)
{
	enum SubtypepExtend value;
	addr pos;

	/* *subtypep!* */
	GetConst(SYSTEM_SUBTYPEP_VALUE, &pos);
	getspecial_local(ptr, pos, &pos);

	/* normal */
	Return(subtypep_extend_value_(pos, &value));
	if (value == SubtypepExtend_Normal) {
		GetConst(SYSTEM_SUBTYPEP_NORMAL, &pos);
	}

	return Result(ret, pos);
}

static int subtypep_extend_type_(Execute ptr, addr pos, enum SubtypepExtend *ret)
{
	if (pos != Nil)
		return subtypep_extend_value_(pos, ret);

	/* *subtypep!* */
	GetConst(SYSTEM_SUBTYPEP_VALUE, &pos);
	getspecial_local(ptr, pos, &pos);
	return subtypep_extend_value_(pos, ret);
}

static int subtypep_extend_switch_(Execute ptr, addr x, addr y, addr env,
		enum SubtypepExtend type, SubtypepResult *ret)
{
	switch (type) {
		case SubtypepExtend_Normal:
			return subtypep_extend_normal_(ptr, x, y, env, ret);

		case SubtypepExtend_Atomic:
			return subtypep_extend_atomic_(ptr, x, y, env, ret);

		case SubtypepExtend_AtomicNot:
			return subtypep_extend_atomic_not_(ptr, x, y, env, ret);

		case SubtypepExtend_Compound:
			return subtypep_extend_compound_(ptr, x, y, env, ret);

		case SubtypepExtend_ForceNumber:
			return subtypep_extend_force_number_(ptr, x, y, env, ret);

		default:
			return subtypep_extend_normal_(ptr, x, y, env, ret);
	}
}

int subtypep_extend_(Execute ptr, addr x, addr y, addr env, addr check, addr *ret)
{
	enum SubtypepExtend type;
	SubtypepResult value;

	/* output */
	if (check == T)
		return subtypep_extend_output_(ptr, ret);

	/* subtypep */
	Return(subtypep_extend_type_(ptr, check, &type));
	Return(subtypep_extend_switch_(ptr, x, y, env, type, &value));
	subtypep_result_keyword(value, ret);
	return 0;
}


/*
 *  interface
 */
int subtypep_scope_(Execute ptr, addr x, addr y, addr env, SubtypepResult *ret)
{
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);
	Return(subtypep_parse_normal_(ptr, hold, x, y, env, &x, &y));
	if (type_asterisk_p(y))
		return Result(ret, SUBTYPEP_INCLUDE);
	if (type_asterisk_p(x))
		return Result(ret, SUBTYPEP_FALSE);

	Return(subtypep_compound_(ptr, x, y, ret));
	localhold_end(hold);

	return 0;
}

int subtypep_check_(Execute ptr, addr x, addr y, addr env, int *ret, int *validp)
{
	SubtypepResult value;
	Return(subtypep_extend_normal_(ptr, x, y, env, &value));
	subtypep_result_values(value, ret, validp);

	return 0;
}


/*
 *  initialize
 */
void init_subtypep(void)
{
	init_subtypep_table();
}

