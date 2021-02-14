#ifndef __QUOTE_HEADER__
#define __QUOTE_HEADER__

#include "execute.h"
#include "typedef.h"

#define quote2_heap _n(quote2_heap)
#define quote_back_heap_ _n(quote_back_heap_)
#define quote_comma_heap _n(quote_comma_heap)
#define quote_atsign_heap _n(quote_atsign_heap)
#define quote_dot_heap _n(quote_dot_heap)
#define getvalue_quote _n(getvalue_quote)
#define getprint_quote _n(getprint_quote)
#define quotep _n(quotep)
#define quote_back_p _n(quote_back_p)
#define quote_comma_p _n(quote_comma_p)
#define quote_atsign_p _n(quote_atsign_p)
#define quote_dot_p _n(quote_dot_p)
#define quote_quote_p _n(quote_quote_p)
#define quote_append_p _n(quote_append_p)
#define quote_nconc_p _n(quote_nconc_p)
#define quote_list_p _n(quote_list_p)
#define quote_lista_p _n(quote_lista_p)
#define quote_clobberable_p _n(quote_clobberable_p)

enum QuoteType {
	QuoteType_Back,
	QuoteType_Comma,
	QuoteType_AtSign,
	QuoteType_Dot,
	QuoteExecute_Quote,
	QuoteExecute_Append,
	QuoteExecute_Nconc,
	QuoteExecute_List,
	QuoteExecute_Lista,
	QuoteExecute_Clobberable,
	QuoteType_Size
};

enum QuoteIndex {
	QuoteIndex_Value,
	QuoteIndex_Print,
	QuoteIndex_Size
};

#define RefQuote			RefArrayA2
#define GetQuote			GetArrayA2
#define SetQuote			SetArrayA2
#define RefQuoteType(x)		((enum QuoteType)GetUser(x))
#define GetQuoteType(x,v)	(*(v) = (enum QuoteType)GetUser(x))
#define SetQuoteType(x,v)	SetUser((x), (byte)(v))

void quote2_heap(addr *ret, enum QuoteType type, addr value, addr print);
int quote_back_heap_(addr *ret, addr form);
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

