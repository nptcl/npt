#ifndef __ENV_CODE_HEADER__
#define __ENV_CODE_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define disassemble_common _n(disassemble_common)
#define trace_common_ _n(trace_common_)
#define untrace_common_ _n(untrace_common_)
#define trace_add_common_ _n(trace_add_common_)
#define trace_del_common_ _n(trace_del_common_)
#define init_environment_code _n(init_environment_code)
#define build_environment_code _n(build_environment_code)

int disassemble_common(Execute ptr, addr pos);
int trace_common_(addr form, addr env, addr *ret);
int untrace_common_(addr form, addr env, addr *ret);
int trace_add_common_(Execute ptr, addr list, addr *ret);
int trace_del_common_(Execute ptr, addr list, addr *ret);

void init_environment_code(void);
void build_environment_code(void);

#endif

