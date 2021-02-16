#ifndef __PARSE_HEADER__
#define __PARSE_HEADER__

#include "execute.h"
#include "local.h"
#include "parse_typedef.h"

#define check_variable_ _n(check_variable_)
#define check_function_variable_ _n(check_function_variable_)
#define tagbody_tag_p _n(tagbody_tag_p)
#define parse_compile_toplevel_ _n(parse_compile_toplevel_)
#define begin_parse _n(begin_parse)
#define eval_parse_ _n(eval_parse_)

int check_variable_(addr symbol);
int check_function_variable_(addr symbol);
int tagbody_tag_p(addr pos);
int parse_compile_toplevel_(Execute ptr, addr expr, addr list, addr *ret);
void begin_parse(Execute ptr, addr toplevel);
int eval_parse_(Execute ptr, addr *ret, addr pos);

#endif

