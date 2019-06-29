#ifndef __EQUAL_HEADER__
#define __EQUAL_HEADER__

#include "build.h"

_g int atom_function(addr pos);
_g int eq_function(addr left, addr right);
_g int eql_function(addr left, addr right);
_g int equal_function(addr left, addr right);
_g int equalp_function(addr left, addr right);
_g int equalrt_function(addr left, addr right);

#define atom atom_function
#define eq eq_function
#define eql eql_function
#define equal equal_function
#define equalp equalp_function

#endif

