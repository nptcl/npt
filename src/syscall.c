#include "syscall.h"
#include "syscall_common.h"
#include "syscall_function.h"
#include "typedef.h"

_g void init_syscall(void)
{
	init_syscall_common();
	init_syscall_function();
}

_g void build_syscall(void)
{
	build_syscall_common();
	build_syscall_function();
}

