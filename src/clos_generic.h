#ifndef __CLOS_GENERIC_HEADER__
#define __CLOS_GENERIC_HEADER__

#include "execute.h"

typedef void (*clos_generic_call)(Execute, addr, addr, addr);

void clos_generic_call_alloc(LocalRoot local,
		addr *ret, clos_generic_call call, int size);
void clos_generic_call_heap(addr *ret, clos_generic_call call, int size);
void clos_generic_call_local(LocalRoot local,
		addr *ret, clos_generic_call call, int size);

int subclass_eql_specializer(addr left, addr right, int check);
void generic_function_instance(Execute ptr, addr *ret, addr name, addr lambda);
void std_finalize_generic_function(Execute ptr, addr generic);
void execute_clos(Execute ptr, addr pos, addr args);

#endif

