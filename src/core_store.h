#ifndef __CORE_STORE_HEADER__
#define __CORE_STORE_HEADER__

#include "define.h"
#include "typedef.h"

#define load_store_init _n(load_store_init)
#define load_store_push _n(load_store_push)
#define load_store_error _n(load_store_error)
#define load_store_exec _n(load_store_exec)

_g int load_store_init(void);
_g int load_store_push(addr pos);
_g void load_store_error(void);
_g void load_store_exec(void);

#endif

