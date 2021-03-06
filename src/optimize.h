#ifndef __OPTIMIZE_HEADER__
#define __OPTIMIZE_HEADER__

#include "declare.h"
#include "define.h"
#include "eval.h"
#include "execute.h"
#include "local.h"
#include "parse.h"
#include "typedef.h"

#define save_optimize_value _n(save_optimize_value)
#define rollback_optimize_value _n(rollback_optimize_value)
#define optimize_declare_value _n(optimize_declare_value)
#define optimize_speed_on _n(optimize_speed_on)
#define optimize_evaltype _n(optimize_evaltype)
#define optimize_evaltype_on _n(optimize_evaltype_on)
#define optimize_initialize _n(optimize_initialize)
#define optimize_extract_ _n(optimize_extract_)

struct optimize_value {
	OptimizeType declaim[EVAL_OPTIMIZE_SIZE];
	OptimizeType local[EVAL_OPTIMIZE_SIZE];
};

struct optimize_struct {
	Execute ptr;
	LocalRoot local;
	int update;
	addr pos;
	struct optimize_value value;
};

typedef struct optimize_struct OptimizeInfo;
typedef int (*optimize_call)(struct optimize_struct *, int *);

void save_optimize_value(const struct optimize_struct *str,
		struct optimize_value *save);
void rollback_optimize_value(struct optimize_struct *str,
		const struct optimize_value *save);

int optimize_declare_value(struct optimize_struct *str, enum EVAL_OPTIMIZE index);
int optimize_speed_on(struct optimize_struct *str);
int optimize_evaltype(addr pos, EvalParse type);
int optimize_evaltype_on(struct optimize_struct *str, EvalParse type);
void optimize_initialize(struct optimize_struct *str, Execute ptr, addr pos);
int optimize_extract_(struct optimize_struct *str, optimize_call call);

#endif

