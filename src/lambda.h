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
void lambda_macro(LocalRoot local, addr *ret, addr cons, addr instance);
void lambda_deftype(LocalRoot local, addr *ret, addr cons, addr instance);
void lambda_generic_function(LocalRoot local, addr *ret, addr cons);
void lambda_specialized(LocalRoot local, addr *ret, addr cons);
void lambda_ordinary(LocalRoot local, addr *ret, addr cons);
void atleast_argument_count(addr cons, size_t *ret);
void lambda_defsetf(LocalRoot local, addr *ret, addr cons);
void getenvironment_macro_lambda(addr pos, addr *ret);
void allsymbol_macro_lambda_heap(LocalRoot local, addr *ret, addr args);

/* argument */
int argumentp(addr pos);
void getargument(addr pos, size_t index, addr *ret);
void setargument(addr pos, size_t index, addr value);
struct argument_struct *argumentstruct(addr pos);
void argument_alloc(LocalRoot local, addr *ret);
void argument_local(LocalRoot local, addr *ret);
void argument_heap(addr *ret);
void argument_ordinary_heap(LocalRoot local, addr *ret, addr cons);
void argument_generic_heap(LocalRoot local, addr *ret, addr cons);
void argument_method_heap(LocalRoot local, addr *ret, addr cons);
void argument_combination_heap(LocalRoot local, addr *ret, addr cons);
void argument_generic_lambda_heap(addr *ret, addr pos);
void argument_method_lambda_heap(addr *ret, addr pos);
void argument_method_ordinary_heap(addr *ret, addr pos);

#endif

