#ifndef __CLOS_METHOD_HEADER__
#define __CLOS_METHOD_HEADER__

#include "execute.h"

/* access */
_g int stdget_method_function_(addr pos, addr *ret);
_g int stdset_method_function_(addr pos, addr value);
_g int stdget_method_generic_function_(addr pos, addr *ret);
_g int stdset_method_generic_function_(addr pos, addr value);
_g int stdget_method_lambda_list_(addr pos, addr *ret);
_g int stdset_method_lambda_list_(addr pos, addr value);
_g int stdget_method_qualifiers_(addr pos, addr *ret);
_g int stdset_method_qualifiers_(addr pos, addr value);
_g int stdget_method_specializers_(addr pos, addr *ret);
_g int stdset_method_specializers_(addr pos, addr value);

/* control */
_g int method_instance_call_(LocalRoot local, addr *ret, addr clos, addr call);
_g int method_instance_lambda_(LocalRoot local, addr *ret, addr clos, addr lambda);
_g int method_remove_method_(Execute ptr, addr gen, addr method);
_g int method_add_method_(Execute ptr, addr gen, addr method);
_g int common_method_add_(Execute ptr, addr generic, addr method);
_g int common_method_finalize_(addr generic);
_g int ensure_method_common_(Execute ptr, addr *ret,
		addr name, addr lambda, addr qua, addr spec, addr call);

/* document */
_g int methodget_document_(addr clos, addr *ret);
_g int methodset_document_(addr clos, addr value);

/* common */
_g void method_make_method_lambda(addr list, addr env, addr *ret);

#endif

