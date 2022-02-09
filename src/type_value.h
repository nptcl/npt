#ifndef __TYPE_VALUE_HEADER__
#define __TYPE_VALUE_HEADER__

#include "typedef.h"

#define type_value_nil _n(type_value_nil)
#define type_value_t _n(type_value_t)
#define type_value_clos_ _n(type_value_clos_)
#define type_value_array_ _n(type_value_array_)
#define type_value_vector _n(type_value_vector)
#define type_value_character _n(type_value_character)
#define type_value_integer _n(type_value_integer)
#define type_value_rational _n(type_value_rational)
#define type_value_bitvector _n(type_value_bitvector)
#define type_value_float _n(type_value_float)
#define type_value_complex_ _n(type_value_complex_)
#define type_value_package _n(type_value_package)
#define type_value_random_state _n(type_value_random_state)
#define type_value_pathname _n(type_value_pathname)
#define type_value_environment _n(type_value_environment)
#define type_value_paper _n(type_value_paper)
#define type_value_ _n(type_value_)
#define init_type_value _n(init_type_value)

/*  function type_value
 *     input: object
 *    output: type
 */
void type_value_nil(addr *ret);
void type_value_t(addr *ret);
int type_value_clos_(addr *ret, addr value);
int type_value_array_(addr *ret, addr value);
void type_value_vector(addr *ret, addr value);
void type_value_character(addr *ret, addr value);

void type_value_integer(addr *ret, addr value);
void type_value_rational(addr *ret, addr value);
void type_value_bitvector(addr *ret, addr value);
void type_value_float(addr *ret, addr value);
int type_value_complex_(addr *ret, addr value);
void type_value_package(addr *ret, addr value);
void type_value_random_state(addr *ret, addr value);
void type_value_pathname(addr *ret, addr value);
void type_value_environment(addr *ret, addr value);
void type_value_paper(addr *ret, addr value);

int type_value_(addr *ret, addr value);
void init_type_value(void);

#endif

