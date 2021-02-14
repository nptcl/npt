#ifndef __LOOP_SPECIAL_HEADER__
#define __LOOP_SPECIAL_HEADER__

#include "execute.h"
#include "typedef.h"

#define loop_push_special _n(loop_push_special)
#define setnamed_loop _n(setnamed_loop)
#define getnamed_loop_ _n(getnamed_loop_)
#define getvars_expand_loop_ _n(getvars_expand_loop_)
#define push_vars_loop_ _n(push_vars_loop_)
#define getform_expand_loop_ _n(getform_expand_loop_)
#define push_form_loop_ _n(push_form_loop_)
#define getinit_expand_loop_ _n(getinit_expand_loop_)
#define push_init_loop_ _n(push_init_loop_)
#define getfinal_expand_loop_ _n(getfinal_expand_loop_)
#define push_final_loop_ _n(push_final_loop_)
#define push_let_loop_ _n(push_let_loop_)
#define getlet_loop_ _n(getlet_loop_)

void loop_push_special(Execute ptr);
void setnamed_loop(Execute ptr, addr value);
int getnamed_loop_(Execute ptr, addr *ret);

int getvars_expand_loop_(Execute ptr, addr *ret);
int push_vars_loop_(Execute ptr, addr value);

int getform_expand_loop_(Execute ptr, addr *ret);
int push_form_loop_(Execute ptr, addr value);

int getinit_expand_loop_(Execute ptr, addr *ret);
int push_init_loop_(Execute ptr, addr value);

int getfinal_expand_loop_(Execute ptr, addr *ret);
int push_final_loop_(Execute ptr, addr value);

int push_let_loop_(Execute ptr, addr value);
int getlet_loop_(Execute ptr, addr *ret);

#endif

