#ifndef __PARSE_FUNCTION_HEADER__
#define __PARSE_FUNCTION_HEADER__

#include "execute.h"
#include "hold.h"
#include "typedef.h"

#define parse_declare_body_ _n(parse_declare_body_)
#define parse_macrolet_args_ _n(parse_macrolet_args_)
#define parse_symbol_macrolet_args_ _n(parse_symbol_macrolet_args_)

#define localhold_parse_allcons_ _n(localhold_parse_allcons_)
#define localhold_parse_execute_ _n(localhold_parse_execute_)

#define parse_allcons_ _n(parse_allcons_)
#define parse_execute_ _n(parse_execute_)
#define parse_allcons_toplevel_ _n(parse_allcons_toplevel_)
#define parse_execute_toplevel_ _n(parse_execute_toplevel_)

#define parse_compiler_macro_p _n(parse_compiler_macro_p)
#define parse_eval_when_list_ _n(parse_eval_when_list_)
#define parse_eval_when_process _n(parse_eval_when_process)
#define parse_ordinary_ _n(parse_ordinary_)

#define localhold_parse_self_(h,p,x) localhold_parse_execute_((h),(p),&(x),(x))
#define parse_self_(p, x) parse_execute_((p), &(x), (x))

int parse_declare_body_(Execute ptr, addr cons, addr *retdecl, addr *retbody);
int parse_macrolet_args_(Execute ptr, addr args);
int parse_symbol_macrolet_args_(Execute ptr, addr args, addr decl);

int localhold_parse_allcons_(LocalHold hold, Execute ptr, addr *ret, addr cons);
int localhold_parse_execute_(LocalHold hold, Execute ptr, addr *ret, addr pos);

int parse_allcons_(Execute ptr, addr *ret, addr cons);
int parse_execute_(Execute ptr, addr *ret, addr pos);
int parse_allcons_toplevel_(Execute ptr, addr *ret, addr cons);
int parse_execute_toplevel_(Execute ptr, addr *ret, addr pos);

int parse_compiler_macro_p(Execute ptr, addr *ret, addr cons);
int parse_eval_when_list_(addr list, addr *rcompile, addr *rload, addr *rexec);
int parse_eval_when_process(Execute ptr,
		addr compile, addr load, addr exec, addr toplevel, addr mode);
int parse_ordinary_(Execute ptr, addr *ret, addr args);

#endif

