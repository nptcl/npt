#ifndef __LISP_EXTERN_TYPE_HEADER__
#define __LISP_EXTERN_TYPE_HEADER__

#include "typedef_basic.h"

/* hold */
int lisp_hold_p(addr x);
void lisp_hold_value(addr x, addr *ret);
void lisp_hold_set(addr x, addr value);
addr Lisp_holdv(addr x);
void lisp_hold(addr *ret, addr value);
addr Lisp_hold(void);

/* nil, t */
void lisp0_nil(addr *ret);
void lisp0_t(addr *ret);
void lisp_nil(addr x);
void lisp_t(addr x);
addr Lisp_nil(void);
addr Lisp_t(void);

/* type */
int lisp_nil_p(addr x);
int lisp_t_p(addr x);
int lisp_null_p(addr x);
int lisp_character_p(addr x);
int lisp_cons_p(addr x);
int lisp_list_p(addr x);
int lisp_string_p(addr x);
int lisp_strvect_p(addr x);
int lisp_symbol_p(addr x);
int lisp_array_p(addr x);
int lisp_vector_p(addr x);

int lisp_fixnum_p(addr x);
int lisp_bignum_p(addr x);
int lisp_integer_p(addr x);
int lisp_ratio_p(addr x);
int lisp_rational_p(addr x);
int lisp_single_float_p(addr x);
int lisp_double_float_p(addr x);
int lisp_long_float_p(addr x);
int lisp_float_p(addr x);
int lisp_real_p(addr x);
int lisp_complex_p(addr x);
int lisp_number_p(addr x);

int lisp_clos_p(addr x);
int lisp_hashtable_p(addr x);
int lisp_readtable_p(addr x);
int lisp_control_p(addr x);
int lisp_callname_p(addr x);
int lisp_funciton_p(addr x);
int lisp_package_p(addr x);
int lisp_random_state_p(addr x);
int lisp_pathname_p(addr x);
int lisp_stream_p(addr x);
int lisp_restart_p(addr x);
int lisp_environment_p(addr x);
int lisp_bitvector_p(addr x);
int lisp_print_dispatch_p(addr x);

#endif

