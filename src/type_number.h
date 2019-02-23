#ifndef __TYPE_NUMBER_HEADER__
#define __TYPE_NUMBER_HEADER__

#include "type.h"

void real_extract_alloc(LocalRoot local, addr *ret, addr type);
void real_extract_local(LocalRoot local, addr *ret, addr type);
void real_extract_heap(LocalRoot local, addr *ret, addr type);
void real_extract_heap_unsafe(addr *ret, addr type);
int type_subtypep_p(addr type);
int type_optimized_or_subtypep(addr type);
void get_type_subtypep(addr *ret, addr type);

#endif

