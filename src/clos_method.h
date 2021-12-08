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
#define method_find_method_nil_ _n(method_find_method_nil_)
#define method_find_method_ _n(method_find_method_)
#define method_remove_method_unsafe_ _n(method_remove_method_unsafe_)
#define method_remove_method_ _n(method_remove_method_)
#define method_add_method_ _n(method_add_method_)
#define common_method_add_ _n(common_method_add_)
#define common_method_finalize_ _n(common_method_finalize_)
#define ensure_method_common_ _n(ensure_method_common_)
#define methodget_document_ _n(methodget_document_)
#define methodset_document_ _n(methodset_document_)
#define method_make_method_lambda _n(method_make_method_lambda)

/* access */
int stdget_method_function_(addr pos, addr *ret);
int stdset_method_function_(addr pos, addr value);
int stdget_method_generic_function_(addr pos, addr *ret);
int stdset_method_generic_function_(addr pos, addr value);
int stdget_method_lambda_list_(addr pos, addr *ret);
int stdset_method_lambda_list_(addr pos, addr value);
int stdget_method_qualifiers_(addr pos, addr *ret);
int stdset_method_qualifiers_(addr pos, addr value);
int stdget_method_specializers_(addr pos, addr *ret);
int stdset_method_specializers_(addr pos, addr value);

/* control */
int method_instance_call_(LocalRoot local, addr *ret, addr clos, addr call);
int method_instance_lambda_(LocalRoot local, addr *ret, addr clos, addr lambda);
int method_find_method_nil_(Execute ptr, addr gen, addr qua, addr spec, addr *ret);
int method_find_method_(Execute ptr, addr gen, addr qua, addr spec, addr *ret);
int method_remove_method_unsafe_(Execute ptr, addr gen, addr method, int *ret);
int method_remove_method_(Execute ptr, addr gen, addr method);
int method_add_method_(Execute ptr, addr gen, addr method);
int common_method_add_(Execute ptr, addr generic, addr method);
int common_method_finalize_(addr generic);
int ensure_method_common_(Execute ptr, addr *ret,
		addr name, addr lambda, addr qua, addr spec, addr call);

/* document */
int methodget_document_(addr clos, addr *ret);
int methodset_document_(addr clos, addr value);

/* common */
void method_make_method_lambda(addr list, addr env, addr *ret);

#endif

