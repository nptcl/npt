#ifndef __LISP_MAIN_INIT_HEADER__
#define __LISP_MAIN_INIT_HEADER__

#include <stdio.h>
#include "main_argv.h"

extern int lisp_code;
extern int lisp_result;

void lisp_init(void);
void lisp_free(void);
int lisp_alloc(size_t heap, size_t local);

int lisp_main_help(FILE *file);
int lisp_main_version_text(FILE *file);
int lisp_main_version_script(FILE *file);
int lisp_main_version(struct lispargv *ptr, FILE *file);
int lisp_main_degrade(struct lispargv *ptr);

int lisp_argv_init(struct lispargv *ptr);
int lisp_argv_run(struct lispargv *ptr);

#endif

