#ifndef __EQUAL_HEADER__
#define __EQUAL_HEADER__

#include "build.h"

#define atom_function _n(atom_function)
#define eq_function _n(eq_function)
#define eq_function_ _n(eq_function_)
#define eql_function _n(eql_function)
#define eql_function_ _n(eql_function_)
#define equal_function_ _n(equal_function_)
#define equalp_function_ _n(equalp_function_)
#define equalrt_function_ _n(equalrt_function_)
#define equal_debug _n(equal_debug)
#define equalp_debug _n(equalp_debug)
#define equalrt_debug _n(equalrt_debug)

_g int atom_function(addr pos);
_g int eq_function(addr left, addr right);
_g int eq_function_(addr left, addr right, int *ret);
_g int eql_function(addr left, addr right);
_g int eql_function_(addr left, addr right, int *ret);
_g int equal_function_(addr left, addr right, int *ret);
_g int equalp_function_(addr left, addr right, int *ret);
_g int equalrt_function_(addr left, addr right, int *ret);

_g int equal_debug(addr left, addr right);
_g int equalp_debug(addr left, addr right);
_g int equalrt_debug(addr left, addr right);

#endif

