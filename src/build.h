#ifndef __BUILD_HEADER__
#define __BUILD_HEADER__

#include <setjmp.h>
#include <stddef.h>
#include "alloc.h"
#include "build_define.h"
#include "define.h"
#include "execute.h"
#include "file_type.h"
#include "info.h"
#include "typedef.h"

#define initlisp _n(initlisp)
#define alloclisp _n(alloclisp)
#define freelisp _n(freelisp)
#define degradelisp _n(degradelisp)
#define setlisproot _n(setlisproot)
#define build_lisproot _n(build_lisproot)
#define buildlisp _n(buildlisp)
#define save_lisp _n(save_lisp)
#define load_lisp _n(load_lisp)

/* variable */
__extern int      lisp_initialize;
__extern addr     lisp_root[LISPINDEX_SIZE];
__extern addr     lisp_nil_object;
__extern addr     lisp_t_object;
__extern int      lisp_info_enable;
__extern enum GcMode lisp_gcsync;

/* function */
_g void initlisp(void);
_g int alloclisp(size_t heap, size_t stack);
_g void freelisp(void);
_g int degradelisp(void);

_g void setlisproot(enum LISPINDEX index, addr value);
_g void build_lisproot(Execute ptr);
_g void buildlisp(Execute ptr);

/* core */
_g int save_lisp(struct filememory *fm);
_g int load_lisp(struct filememory *fm);

#endif

