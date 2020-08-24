#ifndef __LISP_MAIN_TYPEDEF_HEADER__
#define __LISP_MAIN_TYPEDEF_HEADER__

#include <stddef.h>
#include "typedef_basic.h"

struct lispstringu_struct {
	unicode *ptr;
	size_t size;
};
typedef struct lispstringu_struct *lispstringu;

struct lisparrayu_struct {
	lispstringu *ptr;
	size_t size;
};
typedef struct lisparrayu_struct *lisparrayu;

struct lispkeyvalueu {
	lispstringu key, value;
};

struct lisptableu_struct {
	struct lispkeyvalueu *table;
	size_t size;
};
typedef struct lisptableu_struct *lisptableu;

#endif

