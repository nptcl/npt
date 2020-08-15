#ifndef __OPTIMIZE_HEADER__
#define __OPTIMIZE_HEADER__

#include "declare.h"
#include "define.h"
#include "eval.h"
#include "local.h"
#include "parse.h"
#include "typedef.h"

struct optimize_value {
	OptimizeType declaim[EVAL_OPTIMIZE_SIZE];
	OptimizeType local[EVAL_OPTIMIZE_SIZE];
};

struct optimize_struct {
	LocalRoot local;
	int update;
	addr pos;
	struct optimize_value value;
};

typedef struct optimize_struct OptimizeInfo;
typedef int (*optimize_call)(struct optimize_struct *, int *);

_g void save_optimize_value(const struct optimize_struct *str,
		struct optimize_value *save);
_g void rollback_optimize_value(struct optimize_struct *str,
		const struct optimize_value *save);

_g int optimize_declare_value(struct optimize_struct *str, enum EVAL_OPTIMIZE index);
_g int optimize_speed_on(struct optimize_struct *str);
_g int optimize_evaltype(addr pos, EvalParse type);
_g int optimize_evaltype_on(struct optimize_struct *str, EvalParse type);
_g void optimize_initialize(struct optimize_struct *str, LocalRoot local, addr pos);
_g int optimize_extract_(struct optimize_struct *str, optimize_call call);

#endif

