#ifndef __SCOPE_CALL_HEADER__
#define __SCOPE_CALL_HEADER__

#include "execute.h"
#include "scope_typedef.h"
#include "typedef.h"

#define scope_symbol_call_ _n(scope_symbol_call_)
#define scope_setq_call _n(scope_setq_call)
#define scope_values_call_ _n(scope_values_call_)
#define scope_the_call_ _n(scope_the_call_)
#define scope_locally_call_ _n(scope_locally_call_)
#define scope_tagbody_call _n(scope_tagbody_call)
#define scope_go_call_ _n(scope_go_call_)
#define scope_block_call _n(scope_block_call)
#define scope_return_from_call _n(scope_return_from_call)
#define scope_init_mvbind _n(scope_init_mvbind)
#define scope_multiple_value_bind_call_ _n(scope_multiple_value_bind_call_)
#define scope_multiple_value_call_call_ _n(scope_multiple_value_call_call_)

int scope_symbol_call_(Execute ptr, addr *ret, addr eval);
int scope_setq_call(Execute ptr, addr cons, addr *ret, addr *type);
int scope_values_call_(Execute ptr, addr args, addr *rargs, addr *rtype);
int scope_the_call_(Execute ptr, addr form, addr type, addr expr, addr *ret);
int scope_locally_call_(Execute ptr, addr form, addr decl, addr cons, addr *ret);
int scope_tagbody_call(Execute ptr, addr tag, addr body, addr *rtag, addr *rbody);
int scope_go_call_(Execute ptr, addr *ret, addr tag);
int scope_block_call(Execute ptr, addr name, addr cons,
		addr *rname, addr *rcons, addr *rtype);
int scope_return_from_call(Execute ptr,
		addr name, addr form, addr *rname, addr *rexpr);
void scope_init_mvbind(struct mvbind_struct *str);
int scope_multiple_value_bind_call_(Execute ptr, struct mvbind_struct *str);
int scope_multiple_value_call_call_(Execute ptr,
		addr form, addr expr, addr cons, addr *ret);

#endif

