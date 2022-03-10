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
#define reloadlisp _n(reloadlisp)
#define save_lisp _n(save_lisp)
#define load_lisp _n(load_lisp)

/* variable */
extern int      lisp_initialize;
extern addr     lisp_root[LISPINDEX_SIZE];
extern addr     lisp_nil_object;
extern addr     lisp_t_object;
extern int      lisp_info_enable;
extern enum GcMode lisp_gcsync;

/* function */
void initlisp(void);
int alloclisp(size_t heap, size_t stack);
void freelisp(void);
int reloadlisp(void);
int degradelisp(void);

void setlisproot(enum LISPINDEX index, addr value);
void build_lisproot(Execute ptr);
void buildlisp(Execute ptr);

/* core */
int save_lisp(filestream fm);
int load_lisp(filestream fm);

#endif

