#ifndef __COMPILE_FILE_HEADER__
#define __COMPILE_FILE_HEADER__

#include "execute.h"
#include "typedef.h"

#define set_eval_compile_mode _n(set_eval_compile_mode)
#define eval_compile_p _n(eval_compile_p)
#define eval_compile_file _n(eval_compile_file)
#define handler_compile_ _n(handler_compile_)
#define compile_file_common _n(compile_file_common)
#define init_compile_file _n(init_compile_file)

void set_eval_compile_mode(Execute ptr, addr value);
int eval_compile_p(Execute ptr);
int eval_compile_file(Execute ptr, addr pos);
int handler_compile_(Execute ptr);
int compile_file_common(Execute ptr, addr file, addr rest,
		addr *ret1, addr *ret2, addr *ret3);
void init_compile_file(void);

#endif

