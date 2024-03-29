#ifndef __LISP_TYPEDEF_HEADER__
#define __LISP_TYPEDEF_HEADER__

#include "define.h"
#include "typedef_basic.h"
#include "typedef_integer.h"
#include "typedef_object.h"
#include "typedef_typespec.h"
#include "typedef_value.h"
#include <stddef.h>
#include <stdarg.h>

/* setjmp */
typedef void (*lisp_abort_calltype)(void);
typedef int (*lisp_equal_calltype)(addr, addr, int *);

/* code */
union CODEVALUE {
	fixnum value;
	unicode character;
	size_t index;
	addr pos;
};
typedef union CODEVALUE CodeValue;

#endif

