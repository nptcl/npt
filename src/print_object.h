#ifndef __PRINT_OBJECT__
#define __PRINT_OBJECT__

#include "define.h"
#include "execute.h"

#define print_structure_ _n(print_structure_)
#define build_print_object_ _n(build_print_object_)
#define init_print_object _n(init_print_object)

int print_structure_(Execute ptr, addr stream, addr pos);
int build_print_object_(Execute ptr);
void init_print_object(void);

#endif

