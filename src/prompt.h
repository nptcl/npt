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
#define read_prompt_ _n(read_prompt_)
#define read_error_prompt_ _n(read_error_prompt_)

enum prompt_mode {
	prompt_input,
	prompt_eval,
	prompt_for,
	prompt_debugger,
	prompt_inspect,
	prompt_step
};
typedef enum prompt_mode PromptMode;

struct prompt_struct {
	PromptMode mode;
};

#define PtrPromptStruct(x) ((struct prompt_struct *)PtrBodySS(x))

void push_prompt(Execute ptr, addr value, PromptMode mode);
void push_prompt_eval_loop(Execute ptr);
void get_prompt(Execute ptr, addr *value, PromptMode *mode);
void getvalue_prompt(Execute ptr, addr *ret);
void getmode_prompt(Execute ptr, PromptMode *ret);
int read_prompt_(Execute ptr, addr stream, int *result, addr *ret);
int read_error_prompt_(Execute ptr, addr stream, addr *ret);

#endif

