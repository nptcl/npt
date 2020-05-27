#ifndef __TYPEDEF_HEADER__
#define __TYPEDEF_HEADER__

#include "define.h"
#include "typedef_basic.h"
#include "typedef_value.h"
#include "typedef_integer.h"
#include <stddef.h>

/* setjmp */
typedef enum LISPCODE lispcode;

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

