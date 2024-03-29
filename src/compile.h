#ifndef __COMPILE_HEADER__
#define __COMPILE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define compile_file_pathname_common_ _n(compile_file_pathname_common_)
#define with_compilation_unit_common_ _n(with_compilation_unit_common_)
#define syscall_with_compilation_unit _n(syscall_with_compilation_unit)
#define init_compile _n(init_compile)

int compile_file_pathname_common_(Execute ptr, addr file, addr rest, addr *ret);
int with_compilation_unit_common_(addr form, addr *ret);
int syscall_with_compilation_unit(Execute ptr, addr over, addr args, addr call);

void init_compile(void);

#endif

