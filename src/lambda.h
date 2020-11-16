#ifndef __LAMBDA_HEADER__
#define __LAMBDA_HEADER__

#include "local.h"
#include "typedef.h"

#define lambda_macro_ _n(lambda_macro_)
#define lambda_deftype_ _n(lambda_deftype_)
#define lambda_generic_function_ _n(lambda_generic_function_)
#define lambda_specialized_ _n(lambda_specialized_)
#define lambda_ordinary_ _n(lambda_ordinary_)
#define atleast_argument_count _n(atleast_argument_count)
#define lambda_defsetf_ _n(lambda_defsetf_)
#define getenvironment_macro_lambda _n(getenvironment_macro_lambda)
#define allsymbol_macro_lambda_heap_ _n(allsymbol_macro_lambda_heap_)
#define argumentp _n(argumentp)
#define getargument _n(getargument)
#define setargument _n(setargument)
#define argumentstruct _n(argumentstruct)
#define argument_alloc _n(argument_alloc)
#define argument_local _n(argument_local)
#define argument_heap _n(argument_heap)
#define argument_ordinary_heap_ _n(argument_ordinary_heap_)
#define argument_generic_heap_ _n(argument_generic_heap_)
#define argument_method_heap_ _n(argument_method_heap_)
#define argument_combination_heap_ _n(argument_combination_heap_)
#define argument_boa_heap_ _n(argument_boa_heap_)
#define argument_ordinary_lambda_heap_ _n(argument_ordinary_lambda_heap_)
#define argument_generic_lambda_heap_ _n(argument_generic_lambda_heap_)
#define argument_method_lambda_heap_ _n(argument_method_lambda_heap_)
#define argument_method_keywords_heap_ _n(argument_method_keywords_heap_)
#define argument_method_to_generic _n(argument_method_to_generic)
#define argument_boa_lambda_heap_ _n(argument_boa_lambda_heap_)
#define argument_boa_variables_heap_ _n(argument_boa_variables_heap_)
#define find_keyword_allow_other_keys _n(find_keyword_allow_other_keys)

enum ArgumentType {
	ArgumentType_macro,
	ArgumentType_ordinary,
	ArgumentType_deftype,
	ArgumentType_generic,
	ArgumentType_method,
	ArgumentType_combination,
	ArgumentType_defsetf,
	ArgumentType_boa,
	ArgumentType_size,
};

enum ArgumentIndex {
	ArgumentIndex_var,
	ArgumentIndex_opt,
	ArgumentIndex_rest,
	ArgumentIndex_body,
	ArgumentIndex_restbody,
	ArgumentIndex_key,
	ArgumentIndex_aux,
	ArgumentIndex_whole,
	ArgumentIndex_environment,
	ArgumentIndex_dotted,
	ArgumentIndex_size
};

struct argument_struct {
	unsigned keyp : 1;
	unsigned allow : 1;
	unsigned whole : 1;
	unsigned rest : 1;
	unsigned body : 1;
	unsigned restbody : 1;
	unsigned environment : 1;
	unsigned dotted : 1;
	enum ArgumentType type;
	size_t var, opt, key, aux;
};

#define PtrArgument_Low				PtrBodySS
#define GetArgument_Low				GetArraySS
#define SetArgument_Low				SetArraySS
#define ArgumentStruct_Low(x)		((struct argument_struct *)PtrArgument_Low(x))

#ifdef LISP_DEBUG
#define GetArgument					getargument
#define SetArgument					setargument
#define ArgumentStruct				argumentstruct
#else
#define GetArgument					GetArgument_Low
#define SetArgument					SetArgument_Low
#define ArgumentStruct				ArgumentStruct_Low
#endif

/* parse */
int lambda_macro_(LocalRoot local, addr *ret, addr cons, addr instance);
int lambda_deftype_(LocalRoot local, addr *ret, addr cons, addr instance);
int lambda_generic_function_(LocalRoot local, addr *ret, addr cons);
int lambda_specialized_(LocalRoot local, addr *ret, addr cons);
int lambda_ordinary_(LocalRoot local, addr *ret, addr cons);
void atleast_argument_count(addr cons, size_t *ret);
int lambda_defsetf_(LocalRoot local, addr *ret, addr cons);
void getenvironment_macro_lambda(addr pos, addr *ret);
int allsymbol_macro_lambda_heap_(LocalRoot local, addr *ret, addr args);

/* argument */
int argumentp(addr pos);
void getargument(addr pos, size_t index, addr *ret);
void setargument(addr pos, size_t index, addr value);
struct argument_struct *argumentstruct(addr pos);
void argument_alloc(LocalRoot local, addr *ret);
void argument_local(LocalRoot local, addr *ret);
void argument_heap(addr *ret);
int argument_ordinary_heap_(LocalRoot local, addr *ret, addr list);
int argument_generic_heap_(LocalRoot local, addr *ret, addr list);
int argument_method_heap_(LocalRoot local, addr *ret, addr list);
int argument_combination_heap_(LocalRoot local, addr *ret, addr list);
int argument_boa_heap_(LocalRoot local, addr *ret, addr list, addr g);

/* expand */
int argument_ordinary_lambda_heap_(addr *ret, addr pos);
int argument_generic_lambda_heap_(addr *ret, addr pos);
int argument_method_lambda_heap_(addr *ret, addr pos);
int argument_method_keywords_heap_(addr pos, addr *ret, int *allow);
void argument_method_to_generic(addr pos, addr *ret);
int argument_boa_lambda_heap_(addr *ret, addr pos);
int argument_boa_variables_heap_(addr *ret, addr pos);

/* :allow-other-keys */
int find_keyword_allow_other_keys(addr list);

#endif

