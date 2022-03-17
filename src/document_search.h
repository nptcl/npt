#ifndef __DOCUMENT_SEARCH_HEADER__
#define __DOCUMENT_SEARCH_HEADER__

#include "typedef.h"

#define document_function_ _n(document_function_)
#define document_variable_ _n(document_variable_)
#define document_type_ _n(document_type_)

int document_function_(addr pos, addr *ret);
int document_variable_(addr pos, addr *ret);
int document_type_(addr pos, addr *ret);

#endif

