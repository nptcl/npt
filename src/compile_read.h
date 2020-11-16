#ifndef __COMPILE_READ_HEADER__
#define __COMPILE_READ_HEADER__

#include "execute.h"
#include "typedef.h"

#define faslread_header_ _n(faslread_header_)
#define faslread_footer_ _n(faslread_footer_)
#define faslread_value _n(faslread_value)
#define init_compile_read _n(init_compile_read)

int faslread_header_(addr input, int *ret);
int faslread_footer_(addr input, int *ret);
int faslread_value(Execute ptr, addr stream, addr *ret);
void init_compile_read(void);

#endif

