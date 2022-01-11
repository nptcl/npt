#ifndef __OPTIMIZE_CALL_HEADER__
#define __OPTIMIZE_CALL_HEADER__

#include "optimize.h"
#include "typedef.h"

#define checkparse_implicit_ _n(checkparse_implicit_)
#define checkparse_progn_ _n(checkparse_progn_)
#define checkparse_let_ _n(checkparse_let_)
#define checkparse_setq_ _n(checkparse_setq_)
#define checkparse_destructuring_bind_ _n(checkparse_destructuring_bind_)
#define checkparse_if_ _n(checkparse_if_)
#define checkparse_unwind_protect_ _n(checkparse_unwind_protect_)
#define checkparse_tagbody_ _n(checkparse_tagbody_)
#define checkparse_block_ _n(checkparse_block_)
#define checkparse_catch_ _n(checkparse_catch_)
#define checkparse_the_ _n(checkparse_the_)
#define checkparse_eval_when_ _n(checkparse_eval_when_)
#define checkparse_locally_ _n(checkparse_locally_)
#define checkparse_call_ _n(checkparse_call_)
#define checkparse_progv_ _n(checkparse_progv_)

#define optparse_implicit_ _n(optparse_implicit_)
#define optparse_progn_ _n(optparse_progn_)
#define optparse_let_ _n(optparse_let_)
#define optparse_setq_ _n(optparse_setq_)
#define optparse_destructuring_bind_ _n(optparse_destructuring_bind_)
#define optparse_if_ _n(optparse_if_)
#define optparse_unwind_protect_ _n(optparse_unwind_protect_)
#define optparse_tagbody_ _n(optparse_tagbody_)
#define optparse_block_ _n(optparse_block_)
#define optparse_catch_ _n(optparse_catch_)
#define optparse_the_ _n(optparse_the_)
#define optparse_eval_when_ _n(optparse_eval_when_)
#define optparse_locally_ _n(optparse_locally_)
#define optparse_call_ _n(optparse_call_)
#define optparse_progv_ _n(optparse_progv_)

int checkparse_implicit_(OptimizeInfo *str, addr pos, int *ret);
int checkparse_progn_(OptimizeInfo *str, int *ret);
int checkparse_let_(OptimizeInfo *str, int *ret);
int checkparse_setq_(OptimizeInfo *str, int *ret);
int checkparse_destructuring_bind_(OptimizeInfo *str, int *ret);
int checkparse_if_(OptimizeInfo *str, int *ret);
int checkparse_unwind_protect_(OptimizeInfo *str, int *ret);
int checkparse_tagbody_(OptimizeInfo *str, int *ret);
int checkparse_block_(OptimizeInfo *str, int *ret);
int checkparse_catch_(OptimizeInfo *str, int *ret);
int checkparse_the_(OptimizeInfo *str, int *ret);
int checkparse_eval_when_(OptimizeInfo *str, int *ret);
int checkparse_locally_(OptimizeInfo *str, int *ret);
int checkparse_call_(OptimizeInfo *str, int *ret);
int checkparse_progv_(OptimizeInfo *str, int *ret);

int optparse_implicit_(OptimizeInfo *str, addr pos, addr *value, int *ret);
int optparse_progn_(OptimizeInfo *str, int *ret);
int optparse_let_(OptimizeInfo *str, int *ret);
int optparse_setq_(OptimizeInfo *str, int *ret);
int optparse_destructuring_bind_(OptimizeInfo *str, int *ret);
int optparse_if_(OptimizeInfo *str, int *ret);
int optparse_unwind_protect_(OptimizeInfo *str, int *ret);
int optparse_tagbody_(OptimizeInfo *str, int *ret);
int optparse_block_(OptimizeInfo *str, int *ret);
int optparse_catch_(OptimizeInfo *str, int *ret);
int optparse_the_(OptimizeInfo *str, int *ret);
int optparse_eval_when_(OptimizeInfo *str, int *ret);
int optparse_locally_(OptimizeInfo *str, int *ret);
int optparse_call_(OptimizeInfo *str, int *ret);
int optparse_progv_(OptimizeInfo *str, int *ret);

#endif

