#ifndef __STREAM_SYNONYM_HEADER__
#define __STREAM_SYNONYM_HEADER__

#include "typedef.h"

void open_synonym_stream(addr *stream, addr symbol);
void get_synonym_stream(addr stream, addr *ret);
void set_synonym_stream(addr stream, addr symbol);
void init_stream_synonym(void);

#endif

