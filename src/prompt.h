#ifndef __PROMPT_HEADER__
#define __PROMPT_HEADER__

#include "execute.h"
#include "memory.h"
#include "typedef.h"

#define push_prompt _n(push_prompt)
#define push_prompt_eval_loop _n(push_prompt_eval_loop)
#define get_prompt _n(get_prompt)
#define getvalue_prompt _n(getvalue_prompt)
#define getmode_prompt _n(getmode_prompt)

enum prompt_mode {
	prompt_eval,
	prompt_for,
	prompt_debugger,
	prompt_inspect,
	prompt_step
};
struct prompt_struct {
	enum prompt_mode mode;
};

#define PtrPromptStruct(x) ((struct prompt_struct *)PtrBodySS(x))

void push_prompt(Execute ptr, addr value, enum prompt_mode mode);
void push_prompt_eval_loop(Execute ptr);
void get_prompt(Execute ptr, addr *value, enum prompt_mode *mode);
void getvalue_prompt(Execute ptr, addr *ret);
void getmode_prompt(Execute ptr, enum prompt_mode *ret);

#endif

