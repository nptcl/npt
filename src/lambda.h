#ifndef __LAMBDA_HEADER__
#define __LAMBDA_HEADER__

#include "local.h"
#include "typedef.h"

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
_g int lambda_macro_(LocalRoot local, addr *ret, addr cons, addr instance);
_g int lambda_deftype_(LocalRoot local, addr *ret, addr cons, addr instance);
_g int lambda_generic_function_(LocalRoot local, addr *ret, addr cons);
_g int lambda_specialized_(LocalRoot local, addr *ret, addr cons);
_g int lambda_ordinary_(LocalRoot local, addr *ret, addr cons);
_g void atleast_argument_count(addr cons, size_t *ret);
_g int lambda_defsetf_(LocalRoot local, addr *ret, addr cons);
_g void getenvironment_macro_lambda(addr pos, addr *ret);
_g int allsymbol_macro_lambda_heap_(LocalRoot local, addr *ret, addr args);

/* argument */
_g int argumentp(addr pos);
_g void getargument(addr pos, size_t index, addr *ret);
_g void setargument(addr pos, size_t index, addr value);
_g struct argument_struct *argumentstruct(addr pos);
_g void argument_alloc(LocalRoot local, addr *ret);
_g void argument_local(LocalRoot local, addr *ret);
_g void argument_heap(addr *ret);
_g int argument_ordinary_heap_(LocalRoot local, addr *ret, addr list);
_g int argument_generic_heap_(LocalRoot local, addr *ret, addr list);
_g int argument_method_heap_(LocalRoot local, addr *ret, addr list);
_g int argument_combination_heap_(LocalRoot local, addr *ret, addr list);
_g int argument_boa_heap_(LocalRoot local, addr *ret, addr list, addr g);

/* expand */
_g void argument_ordinary_lambda_heap(addr *ret, addr pos);
_g void argument_generic_lambda_heap(addr *ret, addr pos);
_g void argument_method_lambda_heap(addr *ret, addr pos);
_g int argument_method_keywords_heap_(addr pos, addr *ret, int *allow);
_g void argument_method_to_generic(addr pos, addr *ret);
_g void argument_boa_lambda_heap(addr *ret, addr pos);
_g void argument_boa_variables_heap(addr *ret, addr pos);

/* :allow-other-keys */
_g int find_keyword_allow_other_keys(addr list);

#endif

