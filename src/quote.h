#ifndef __QUOTE_HEADER__
#define __QUOTE_HEADER__

#include "execute.h"
#include "typedef.h"

_g int quote_back_heap_(addr *ret, addr form);
_g void quote_comma_heap(addr *ret, addr form);
_g void quote_atsign_heap(addr *ret, addr form);
_g void quote_dot_heap(addr *ret, addr form);
_g void getvalue_quote(addr pos, addr *ret);
_g void getprint_quote(addr pos, addr *ret);

_g int quotep(addr pos);
_g int quote_back_p(addr pos);
_g int quote_comma_p(addr pos);
_g int quote_atsign_p(addr pos);
_g int quote_dot_p(addr pos);
_g int quote_quote_p(addr pos);
_g int quote_append_p(addr pos);
_g int quote_nconc_p(addr pos);
_g int quote_list_p(addr pos);
_g int quote_lista_p(addr pos);
_g int quote_clobberable_p(addr pos);

#endif

