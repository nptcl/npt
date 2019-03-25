#ifndef __EQUAL_HEADER__
#define __EQUAL_HEADER__

#include "build.h"

int atom_function(addr pos);
int eq_function(addr left, addr right);
int eql_function(addr left, addr right);
int equal_function(addr left, addr right);
int equalp_function(addr left, addr right);
int equalrt_function(addr left, addr right);

#define atom atom_function
#define eq eq_function
#define eql eql_function
#define equal equal_function
#define equalp equalp_function

#endif

