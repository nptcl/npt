#ifndef __QUOTE_HEADER__
#define __QUOTE_HEADER__

#include "execute.h"
#include "typedef.h"

void quote_back_heap(addr *ret, addr form);
void quote_comma_heap(addr *ret, addr form);
void quote_atsign_heap(addr *ret, addr form);
void quote_dot_heap(addr *ret, addr form);
void getvalue_quote(addr pos, addr *ret);
void getprint_quote(addr pos, addr *ret);

int quotep(addr pos);
int quote_back_p(addr pos);
int quote_comma_p(addr pos);
int quote_atsign_p(addr pos);
int quote_dot_p(addr pos);
int quote_quote_p(addr pos);
int quote_append_p(addr pos);
int quote_nconc_p(addr pos);
int quote_list_p(addr pos);
int quote_lista_p(addr pos);
int quote_clobberable_p(addr pos);

#endif

