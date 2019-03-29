#ifndef __CLOS_METHOD_HEADER__
#define __CLOS_METHOD_HEADER__

#include "execute.h"

/* access */
void stdget_method_function(addr pos, addr *ret);
void stdset_method_function(addr pos, addr value);
void stdget_method_generic_function(addr pos, addr *ret);
void stdset_method_generic_function(addr pos, addr value);
void stdget_method_lambda_list(addr pos, addr *ret);
void stdset_method_lambda_list(addr pos, addr value);
void stdget_method_qualifiers(addr pos, addr *ret);
void stdset_method_qualifiers(addr pos, addr value);
void stdget_method_specializers(addr pos, addr *ret);
void stdset_method_specializers(addr pos, addr value);

/* control */
void method_instance_alloc(LocalRoot local, addr *ret, addr clos,
		addr lambda_list, addr qualifiers, addr specializers, addr function);
void method_instance_call(LocalRoot local, addr *ret, addr clos, addr call);
void method_instance_lambda(LocalRoot local, addr *ret, addr clos, addr lambda);
void method_add_method(Execute ptr, addr generic, addr method);
void common_method_add(Execute ptr, addr generic, addr method);
void common_method_finalize(addr generic);

/* document */
void methodget_document(addr clos, addr *ret);
void methodset_document(addr clos, addr value);

#endif

