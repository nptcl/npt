#ifndef __CLOS_METHOD_HEADER__
#define __CLOS_METHOD_HEADER__

#include "execute.h"

void make_instance_standard_method(LocalRoot local,
		addr *ret, addr clos,
		addr lambda_list, addr lambda_parse,
		addr qualifiers, addr specializers,
		addr function);
void make_instance_standard_method_function(LocalRoot local,
		addr *ret, addr clos, addr call);

void document_standard_method(addr clos, addr *ret);
void setf_document_standard_method(addr clos, addr value);

#endif

