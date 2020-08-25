#include "cons.h"
#include "hold.h"
#include "integer.h"
#include "number.h"
#include "object.h"
#include "ratio.h"
#include "rational.h"
#include "real.h"
#include "typedef.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  hold
 */
int lisp_hold_p(addr x)
{
	return holdp(x);
}

void lisp_hold_value(addr x, addr *ret)
{
	hold_value(x, ret);
}

void lisp_hold_set(addr x, addr value)
{
	hold_set(x, value);
}

addr Lisp_holdv(addr x)
{
	return holdv(x);
}

void lisp_hold(addr *ret, addr value)
{
	Hold_local(ret, value);
}

addr Lisp_hold(void)
{
	addr x;
	Hold_local(&x, Nil);
	return x;
}


/*
 *  nil, t
 */
void lisp0_nil(addr *ret)
{
	*ret = Nil;
}

void lisp0_t(addr *ret)
{
	*ret = T;
}

void lisp_nil(addr x)
{
	hold_set(x, Nil);
}

void lisp_t(addr x)
{
	hold_set(x, T);
}

addr Lisp_nil(void)
{
	return Nil;
}

addr Lisp_t(void)
{
	return T;
}


/*
 *  type
 */
static int lisp_type_p(addr x, enum LISPTYPE type)
{
	hold_value(x, &x);
	return GetType(x) == type;
}

int lisp_nil_p(addr x)
{
	hold_value(x, &x);
	return x == Nil;
}

int lisp_t_p(addr x)
{
	hold_value(x, &x);
	return x == T;
}

int lisp_null_p(addr x)
{
	hold_value(x, &x);
	return x == NULL;
}

int lisp_character_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_CHARACTER);
}

int lisp_cons_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_CONS);
}

int lisp_list_p(addr x)
{
	hold_value(x, &x);
	return listp(x);
}

int lisp_string_p(addr x)
{
	hold_value(x, &x);
	return stringp(x);
}

int lisp_strvect_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_STRING);
}

int lisp_symbol_p(addr x)
{
	hold_value(x, &x);
	return symbolp(x);
}

int lisp_array_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_ARRAY);
}

int lisp_vector_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_VECTOR);
}

int lisp_fixnum_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_FIXNUM);
}

int lisp_bignum_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_BIGNUM);
}

int lisp_integer_p(addr x)
{
	hold_value(x, &x);
	return integerp(x);
}

int lisp_ratio_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_RATIO);
}

int lisp_rational_p(addr x)
{
	hold_value(x, &x);
	return rationalp(x);
}

int lisp_single_float_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_SINGLE_FLOAT);
}

int lisp_double_float_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_DOUBLE_FLOAT);
}

int lisp_long_float_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_LONG_FLOAT);
}

int lisp_float_p(addr x)
{
	hold_value(x, &x);
	return floatp(x);
}

int lisp_real_p(addr x)
{
	hold_value(x, &x);
	return realp(x);
}

int lisp_complex_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_COMPLEX);
}

int lisp_number_p(addr x)
{
	hold_value(x, &x);
	return numberp(x);
}

int lisp_clos_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_CLOS);
}

int lisp_hashtable_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_HASHTABLE);
}

int lisp_readtable_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_READTABLE);
}

int lisp_control_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_CONTROL);
}

int lisp_callname_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_CALLNAME);
}

int lisp_function_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_FUNCTION);
}

int lisp_package_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_PACKAGE);
}

int lisp_random_state_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_RANDOM_STATE);
}

int lisp_pathname_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_PATHNAME);
}

int lisp_stream_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_STREAM);
}

int lisp_restart_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_RESTART);
}

int lisp_environment_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_ENVIRONMENT);
}

int lisp_bitvector_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_BITVECTOR);
}

int lisp_print_dispatch_p(addr x)
{
	return lisp_type_p(x, LISPTYPE_PRINT_DISPATCH);
}

