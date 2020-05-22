#ifndef __SCOPE_LET_HEADER__
#define __SCOPE_LET_HEADER__

#include "execute.h"
#include "typedef.h"

struct let_struct {
	addr stack, args, decl, doc, cons, free, the, allocate;
};

_g void scope_init_let(struct let_struct *str);
_g void check_scope_variable(addr symbol);
_g int type_and_array(LocalRoot local, addr cons, addr *ret);
_g void push_tablevalue_global(Execute ptr, addr stack, addr symbol, addr *ret);
_g int checktype_p(addr left, addr right, int *check);
_g void checktype_value(addr value, addr init);
_g void ignore_checkvalue(addr stack);
_g void localhold_let_struct(LocalRoot local, struct let_struct *str);
_g int scope_let_call(Execute ptr, struct let_struct *str);

_g void ifdeclvalue(Execute ptr, addr stack, addr var, addr decl, addr *ret);
_g int scope_leta_call(Execute ptr, struct let_struct *str);

#endif

