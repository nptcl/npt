#ifndef __SCOPE_CALL_HEADER__
#define __SCOPE_CALL_HEADER__

#include "execute.h"
#include "scope_let.h"
#include "typedef.h"

struct mvbind_struct {
	addr stack, args, decl, doc, cons, free, the, expr;
};

_g int scope_symbol_call(Execute ptr, addr *ret, addr eval);
_g int scope_setq_call(Execute ptr, addr cons, addr *ret, addr *type);
_g void scope_define_symbol_macro_call(Execute ptr,
		addr symbol, addr form, addr body, addr *ret);
_g int scope_symbol_macrolet_call(Execute ptr,
		addr args, addr decl, addr cons, addr *ret);
_g int scope_values_call(Execute ptr, addr args, addr *rargs, addr *rtype);
_g int scope_the_call(Execute ptr, addr type, addr form, addr *ret);
_g int scope_locally_call(Execute ptr, addr decl, addr cons, addr *ret);
_g int scope_tagbody_call(Execute ptr, addr tag, addr body, addr *rtag, addr *rbody);
_g void scope_go_call(Execute ptr, addr *ret, addr tag);
_g int scope_block_call(Execute ptr, addr name, addr cons,
		addr *rname, addr *rcons, addr *rtype);
_g int scope_return_from_call(Execute ptr,
		addr name, addr form, addr *rname, addr *rexpr);
_g int scope_eval_when_call(Execute ptr,
		addr cons, addr compilep, addr loadp, addr evalp, addr *ret);
_g void scope_init_mvbind(struct mvbind_struct *str);
_g int scope_multiple_value_bind_call(Execute ptr, struct mvbind_struct *str);
_g int scope_multiple_value_call_call(Execute ptr, addr expr, addr cons, addr *ret);

#endif

