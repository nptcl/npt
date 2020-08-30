#ifndef __CLOS_METHOD_HEADER__
#define __CLOS_METHOD_HEADER__

#include "execute.h"

#define stdget_method_function_ _n(stdget_method_function_)
#define stdset_method_function_ _n(stdset_method_function_)
#define stdget_method_generic_function_ _n(stdget_method_generic_function_)
#define stdset_method_generic_function_ _n(stdset_method_generic_function_)
#define stdget_method_lambda_list_ _n(stdget_method_lambda_list_)
#define stdset_method_lambda_list_ _n(stdset_method_lambda_list_)
#define stdget_method_qualifiers_ _n(stdget_method_qualifiers_)
#define stdset_method_qualifiers_ _n(stdset_method_qualifiers_)
#define stdget_method_specializers_ _n(stdget_method_specializers_)
#define stdset_method_specializers_ _n(stdset_method_specializers_)
#define method_instance_call_ _n(method_instance_call_)
#define method_instance_lambda_ _n(method_instance_lambda_)
#define method_find_method_ _n(method_find_method_)
#define method_remove_method_ _n(method_remove_method_)
#define method_add_method_ _n(method_add_method_)
#define common_method_add_ _n(common_method_add_)
#define common_method_finalize_ _n(common_method_finalize_)
#define ensure_method_common_ _n(ensure_method_common_)
#define methodget_document_ _n(methodget_document_)
#define methodset_document_ _n(methodset_document_)
#define method_make_method_lambda _n(method_make_method_lambda)

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
_g int method_find_method_(Execute ptr, addr gen, addr qua, addr spec, addr *ret);
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

