#ifndef __PROCESS_ARCH_HEADER__
#define __PROCESS_ARCH_HEADER__

#include "define.h"
#include "execute.h"
#include "pointer_type.h"
#include "typedef.h"

#define run_process_arch_ _n(run_process_arch_)
#define dlfile_check_arch_ _n(dlfile_check_arch_)
#define dlopen_arch_ _n(dlopen_arch_)
#define dlclose_arch_ _n(dlclose_arch_)
#define dlsym_arch_ _n(dlsym_arch_)
#define dlcall_arch_ _n(dlcall_arch_)

int run_process_arch_(Execute ptr, addr instance, addr *ret);
int dlfile_check_arch_(addr pos, addr *ret, int *openp);
int dlopen_arch_(Execute ptr, addr pos, addr *ret);
int dlclose_arch_(Execute ptr, addr pos, addr *ret);
int dlsym_arch_(Execute ptr, addr pos, addr name, enum CallBind_index type, addr *ret);
int dlcall_arch_(Execute ptr, addr pos, addr args);

#endif

