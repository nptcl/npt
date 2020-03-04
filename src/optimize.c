#include "eval_copy.h"
#include "optimize.h"

_g void save_optimize_value(const struct optimize_struct *str,
		struct optimize_value *save)
{
	memcpy(save, &(str->value), sizeoft(struct optimize_value));
}
_g void rollback_optimize_value(struct optimize_struct *str,
		const struct optimize_value *save)
{
	memcpy(str->value.local, save->local, sizeoft(OptimizeType) * EVAL_OPTIMIZE_SIZE);
}

static int optimize_declare_value(struct optimize_value *opt, int index)
{
	OptimizeType value;
	value = opt->local[index];
	return (value < 0)? opt->declaim[index]: value;
}
_g int optimize_speed_on(struct optimize_struct *str)
{
	/* (on -1 1 2 3) (off 0) */
	return optimize_declare_value(&str->value, EVAL_OPTIMIZE_SPEED) != 0;
}
_g int optimize_evaltype(addr pos, enum EVAL_PARSE type)
{
	return eval_parse_p(pos) && RefEvalParseType(pos) == type;
}
_g int optimize_evaltype_on(struct optimize_struct *str, enum EVAL_PARSE type)
{
	return optimize_speed_on(str) && optimize_evaltype(str->pos, type);
}

static void optimize_initialize_declare(struct optimize_value *value)
{
	int i;

	copy_optimize_declare(value->declaim);
	for (i = 0; i < EVAL_OPTIMIZE_SIZE; i++)
		value->local[i] = -1;
}

_g void optimize_initialize(struct optimize_struct *str, LocalRoot local, addr pos)
{
	clearpoint(str);
	optimize_initialize_declare(&(str->value));
	str->local = local;
	copy_eval_parse_local(local, &(str->pos), pos);
}

_g int optimize_extract(struct optimize_struct *str, optimize_call call)
{
	int update;

	update = 0;
	while (call(str))
		update = 1;
	str->update |= update;

	return update;
}

