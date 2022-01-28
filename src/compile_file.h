#ifndef __COMPILE_FILE_HEADER__
#define __COMPILE_FILE_HEADER__

#include "execute.h"
#include "typedef.h"

#define set_eval_compile_mode _n(set_eval_compile_mode)
#define eval_compile_p _n(eval_compile_p)
#define eval_compile_file_ _n(eval_compile_file_)
#define handler_compile_ _n(handler_compile_)
#define compile_file_common_ _n(compile_file_common_)
#define init_compile_file _n(init_compile_file)

void set_eval_compile_mode(Execute ptr, addr value);
int eval_compile_p(Execute ptr);
int eval_compile_file_(Execute ptr, addr pos);
int handler_compile_(Execute ptr);
int compile_file_common_(Execute ptr, addr file, addr rest,
		addr *ret1, addr *ret2, addr *ret3);
void init_compile_file(void);

#endif

