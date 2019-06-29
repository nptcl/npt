#ifndef __CLOS_METHOD_HEADER__
#define __CLOS_METHOD_HEADER__

#include "execute.h"

/* access */
_g void stdget_method_function(addr pos, addr *ret);
_g void stdset_method_function(addr pos, addr value);
_g void stdget_method_generic_function(addr pos, addr *ret);
_g void stdset_method_generic_function(addr pos, addr value);
_g void stdget_method_lambda_list(addr pos, addr *ret);
_g void stdset_method_lambda_list(addr pos, addr value);
_g void stdget_method_qualifiers(addr pos, addr *ret);
_g void stdset_method_qualifiers(addr pos, addr value);
_g void stdget_method_specializers(addr pos, addr *ret);
_g void stdset_method_specializers(addr pos, addr value);

/* control */
_g void method_instance_alloc(LocalRoot local, addr *ret, addr clos,
		addr lambda_list, addr qualifiers, addr specializers, addr function);
_g void method_instance_call(LocalRoot local, addr *ret, addr clos, addr call);
_g void method_instance_lambda(LocalRoot local, addr *ret, addr clos, addr lambda);
_g void method_remove_method(Execute ptr, addr gen, addr method);
_g void method_add_method(Execute ptr, addr gen, addr method);
_g void common_method_add(Execute ptr, addr generic, addr method);
_g void common_method_finalize(addr generic);
_g void ensure_method_common(Execute ptr, addr *ret,
		addr name, addr lambda, addr qua, addr spec, addr call);

/* document */
_g void methodget_document(addr clos, addr *ret);
_g void methodset_document(addr clos, addr value);

/* common */
_g void method_make_method_lambda(addr list, addr env, addr *ret);

#endif

