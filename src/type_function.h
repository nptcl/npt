#ifndef __TYPE_FUNCTION_HEADER__
#define __TYPE_FUNCTION_HEADER__

#include "local.h"
#include "typedef.h"

struct ordinary_args {
	addr var, opt, rest, key;
	size_t size, size_var, size_opt, size_key, pos_rest;
};

struct ordinary_type {
	addr type;
	unsigned nil : 1;
	unsigned var : 1;
	unsigned rest : 1;
	unsigned key : 1;
	unsigned value : 1;
};

typedef struct ordinary_args ordargs;
typedef struct ordinary_type ordtype;

#define make_ordargs _n(make_ordargs)
#define gettype_ordargs_ _n(gettype_ordargs_)
#define simple_p_ordargs _n(simple_p_ordargs)
#define merge_ordargs _n(merge_ordargs)

#define size_check_ordcall_ _n(size_check_ordcall_)
#define gettype_ordcall_ _n(gettype_ordcall_)
#define make_ordvalues_heap _n(make_ordvalues_heap)

void make_ordargs(ordargs *ptr, addr pos);
int gettype_ordargs_(const ordargs *ptr, size_t index, ordtype *ret);
int simple_p_ordargs(const ordargs *ptr);
void merge_ordargs(LocalRoot local, addr *ret, const ordargs *ptr, const ordtype *type);

/* function arguments */
int size_check_ordcall_(addr pos, size_t size, int *ret);
int gettype_ordcall_(addr pos, size_t i, addr *ret);
/* function values */
void make_ordvalues_heap(addr pos, addr *ret);

#endif

