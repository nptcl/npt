#ifndef __CORE_HEADER__
#define __CORE_HEADER__

#include "main_argv.h"
#include "typedef.h"

#define savecore_execute_ _n(savecore_execute_)
#define save_core _n(save_core)
#define load_core _n(load_core)
#define save_and_load_core_ _n(save_and_load_core_)

int savecore_execute_(Execute ptr, addr output, addr input, int exitp);
int save_core(Execute ptr);
int load_core(const unicode *name, size_t size);
int save_and_load_core_(Execute ptr, struct lispargv *argv, int *ret);

#endif

