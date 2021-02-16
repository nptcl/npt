#ifndef __EVAL_LOAD_HEADER__
#define __EVAL_LOAD_HEADER__

#include "execute.h"
#include "typedef.h"

#define eval_load_ _n(eval_load_)
#define eval_load_force_lisp_ _n(eval_load_force_lisp_)
#define eval_load_force_fasl_ _n(eval_load_force_fasl_)
#define compile_load_ _n(compile_load_)

int eval_load_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external);
int eval_load_force_lisp_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external);
int eval_load_force_fasl_(Execute ptr, int *ret,
		addr file, addr verbose, addr print, int exist, addr external);
int compile_load_(Execute ptr,
		addr file, addr verbose, addr print, addr external);

#endif

