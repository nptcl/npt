#ifndef __SCOPE_LET_HEADER__
#define __SCOPE_LET_HEADER__

#include "execute.h"
#include "typedef.h"

#define scope_init_let _n(scope_init_let)
#define check_scope_variable_ _n(check_scope_variable_)
#define type_and_array _n(type_and_array)
#define push_tablevalue_global_ _n(push_tablevalue_global_)
#define checktype_p _n(checktype_p)
#define checktype_value_ _n(checktype_value_)
#define ignore_checkvalue_ _n(ignore_checkvalue_)
#define localhold_let_struct _n(localhold_let_struct)
#define scope_let_call _n(scope_let_call)
#define ifdeclvalue_ _n(ifdeclvalue_)
#define scope_leta_call _n(scope_leta_call)

struct let_struct {
	addr stack, args, decl, doc, cons, free, the, allocate;
};

_g void scope_init_let(struct let_struct *str);
_g int check_scope_variable_(addr symbol);
_g int type_and_array(LocalRoot local, addr cons, addr *ret);
_g int push_tablevalue_global_(Execute ptr, addr stack, addr symbol, addr *ret);
_g int checktype_p(addr left, addr right, int *check);
_g int checktype_value_(Execute ptr, addr value, addr init);
_g int ignore_checkvalue_(addr stack);
_g void localhold_let_struct(LocalRoot local, struct let_struct *str);
_g int scope_let_call(Execute ptr, struct let_struct *str);

_g int ifdeclvalue_(Execute ptr, addr stack, addr var, addr decl, addr *ret);
_g int scope_leta_call(Execute ptr, struct let_struct *str);

#endif

