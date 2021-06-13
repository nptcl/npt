#ifndef __MAKE_TYPEDEF_HEADER__
#define __MAKE_TYPEDEF_HEADER__

#include "execute.h"
#include "local.h"
#include "typedef.h"

#define code_make_struct _n(code_make_struct)
#define CodeMake _n(CodeMake)

struct code_make_struct {
	Execute ptr;
	LocalRoot local;
	addr code;
};
typedef struct code_make_struct *CodeMake;
typedef int (*code_make_calltype)(CodeMake, addr);

#endif

