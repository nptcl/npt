#ifndef __CMPL_HEADER__
#define __CMPL_HEADER__

#include "local.h"
#include "typedef.h"

enum ComplexType {
	ComplexType_single,
	ComplexType_double,
	ComplexType_long,
	ComplexType_rational,
	ComplexType_error
};

#define SetRealComplex_Low(x,y) SetArrayA2((x), 0, (y))
#define GetRealComplex_Low(x,y) GetArrayA2((x), 0, (y))
#define SetImagComplex_Low(x,y) SetArrayA2((x), 1, (y))
#define GetImagComplex_Low(x,y) GetArrayA2((x), 1, (y))
#define SetTypeComplex_Low(x,y) SetUser((x), (byte)(y))
#define GetTypeComplex_Low(x)   ((enum ComplexType)GetUser(x))


#ifdef LISP_DEBUG
#define SetRealComplex SetRealComplex_Low
#define GetRealComplex GetRealComplex_Low
#define SetImagComplex SetImagComplex_Low
#define GetImagComplex GetImagComplex_Low
#define SetTypeComplex SetTypeComplex_Low
#define GetTypeComplex GetTypeComplex_Low
#else
#define SetRealComplex setreal_complex
#define GetRealComplex getreal_complex
#define SetImagComplex setimag_complex
#define GetImagComplex getimag_complex
#define SetTypeComplex settype_complex
#define GetTypeComplex gettype_complex
#endif

_g int complexp(addr pos);
_g void setreal_complex(addr pos, addr value);
_g void getreal_complex(addr pos, addr *ret);
_g void setimag_complex(addr pos, addr value);
_g void getimag_complex(addr pos, addr *ret);
_g void settype_complex(addr pos, int value);
_g enum ComplexType gettype_complex(addr pos);
_g enum ComplexType getcomplex(addr pos, enum ComplexType *type, addr *real, addr *imag);
_g enum ComplexType getcomplexr(addr pos, addr *real, addr *imag);

_g void make_complex_unsafe(LocalRoot local, addr *ret, enum ComplexType type);
_g int complex_alloc_(LocalRoot local, addr *ret, addr real, addr imag);
_g int complex_local_(LocalRoot local, addr *ret, addr real, addr imag);
_g int complex_heap_(addr *ret, addr real, addr imag);

_g int complex_single_alloc_(LocalRoot local,
		addr *ret, single_float real, single_float imag);
_g int complex_single_local_(LocalRoot local,
		addr *ret, single_float real, single_float imag);
_g int complex_single_heap_(addr *ret,
		single_float real, single_float imag);
_g int complex_double_alloc_(LocalRoot local,
		addr *ret, double_float real, double_float imag);
_g int complex_double_local_(LocalRoot local,
		addr *ret, double_float real, double_float imag);
_g int complex_double_heap_(addr *ret,
		double_float real, double_float imag);
_g int complex_long_alloc_(LocalRoot local,
		addr *ret, long_float real, long_float imag);
_g int complex_long_local_(LocalRoot local,
		addr *ret, long_float real, long_float imag);
_g int complex_long_heap_(addr *ret,
		long_float real, long_float imag);

_g int complex_copy_alloc_(LocalRoot local, addr pos, addr *ret);
_g int complex_copy_local_(LocalRoot local, addr pos, addr *ret);
_g int complex_copy_heap_(addr pos, addr *ret);
_g int complex_result_local_(LocalRoot local, addr pos, addr *ret);
_g int complex_result_heap_(LocalRoot local, addr pos, addr *ret);
_g int complex_throw_alloc_(LocalRoot local, addr pos, addr *ret);
_g int complex_throw_local_(LocalRoot local, addr pos, addr *ret);
_g int complex_throw_heap_(addr pos, addr *ret);
_g int complex_force_heap_(addr *ret, addr real, addr imag, enum ComplexType type);

_g int single_float_complex_(addr pos, single_float *re, single_float *im);
_g int double_float_complex_(addr pos, double_float *re, double_float *im);
_g int long_float_complex_(addr pos, long_float *re, long_float *im);

_g int zerop_complex_(addr pos, int *ret);
_g int eql_complex(addr left, addr right);
_g int equal_complex_(LocalRoot local, addr left, addr right, int *ret);
_g int equal_fc_number_(addr left, addr right, int *ret);
_g int equal_bc_number_(addr left, addr right, int *ret);
_g int equal_rc_number_(LocalRoot local, addr left, addr right, int *ret);
_g int equal_sc_number_(LocalRoot local, addr left, addr right, int *ret);
_g int equal_dc_number_(LocalRoot local, addr left, addr right, int *ret);
_g int equal_lc_number_(LocalRoot local, addr left, addr right, int *ret);
#define equal_cf_number_(a,b,r) equal_fc_number_((b),(a),(r))
#define equal_cb_number_(a,b,r) equal_bc_number_((b),(a),(r))
#define equal_cr_number_(m,a,b,r) equal_rc_number_((m),(b),(a),(r))
#define equal_cs_number_(m,a,b,r) equal_sc_number_((m),(b),(a),(r))
#define equal_cd_number_(m,a,b,r) equal_dc_number_((m),(b),(a),(r))
#define equal_cl_number_(m,a,b,r) equal_lc_number_((m),(b),(a),(r))

_g int sign_reverse_complex_common_(addr pos, addr *ret);
_g int abs_complex_common_(addr pos, addr *ret);
_g int signum_complex_common_(addr pos, addr *ret);

#endif

