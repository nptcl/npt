#ifndef __LISP_TYPEDEF_HEADER__
#define __LISP_TYPEDEF_HEADER__

#include "define.h"
#include "typedef_basic.h"
#include "typedef_value.h"
#include "typedef_integer.h"
#include <stddef.h>
#include <stdarg.h>

/* object */
typedef enum LISPTYPE LispType;
typedef enum LISPDECL LispDecl;

/* setjmp */
typedef void (*lisp_abort_calltype)(void);
typedef int (*lisp_equal_calltype)(addr, addr, int *);

/* code */
union CODEVALUE {
	fixnum value;
	unicode character;
	size_t index;
	void *voidp;
	addr pos;
};
typedef union CODEVALUE CodeValue;

#endif

