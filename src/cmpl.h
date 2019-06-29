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
_g void complex_alloc(LocalRoot local, addr *ret, addr real, addr imag);
_g void complex_local(LocalRoot local, addr *ret, addr real, addr imag);
_g void complex_heap(addr *ret, addr real, addr imag);

_g void complex_single_alloc(LocalRoot local,
		addr *ret, single_float real, single_float imag);
_g void complex_single_local(LocalRoot local,
		addr *ret, single_float real, single_float imag);
_g void complex_single_heap(addr *ret,
		single_float real, single_float imag);
_g void complex_double_alloc(LocalRoot local,
		addr *ret, double_float real, double_float imag);
_g void complex_double_local(LocalRoot local,
		addr *ret, double_float real, double_float imag);
_g void complex_double_heap(addr *ret,
		double_float real, double_float imag);
_g void complex_long_alloc(LocalRoot local,
		addr *ret, long_float real, long_float imag);
_g void complex_long_local(LocalRoot local,
		addr *ret, long_float real, long_float imag);
_g void complex_long_heap(addr *ret,
		long_float real, long_float imag);

_g void complex_copy_alloc(LocalRoot local, addr pos, addr *ret);
_g void complex_copy_local(LocalRoot local, addr pos, addr *ret);
_g void complex_copy_heap(addr pos, addr *ret);
_g void complex_result_local(LocalRoot local, addr pos, addr *ret);
_g void complex_result_heap(LocalRoot local, addr pos, addr *ret);
_g void complex_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void complex_throw_local(LocalRoot local, addr pos, addr *ret);
_g void complex_throw_heap(addr pos, addr *ret);
_g void complex_force_heap(addr *ret, addr real, addr imag, enum ComplexType type);

_g void single_float_complex(addr pos, single_float *re, single_float *im);
_g void double_float_complex(addr pos, double_float *re, double_float *im);
_g void long_float_complex(addr pos, long_float *re, long_float *im);

_g int zerop_complex(addr pos);
_g int eql_complex(addr left, addr right);
_g int equal_complex(LocalRoot local, addr left, addr right);
#define not_equal_complex(m,a,b) (! equal_complex((m),(a),(b)))
_g int equal_fc_number(addr left, addr right);
_g int equal_bc_number(addr left, addr right);
_g int equal_rc_number(LocalRoot local, addr left, addr right);
_g int equal_sc_number(LocalRoot local, addr left, addr right);
_g int equal_dc_number(LocalRoot local, addr left, addr right);
_g int equal_lc_number(LocalRoot local, addr left, addr right);
#define equal_cf_number(a,b) equal_fc_number((b),(a))
#define equal_cb_number(a,b) equal_bc_number((b),(a))
#define equal_cr_number(m,a,b) equal_rc_number((m),(b),(a))
#define equal_cs_number(m,a,b) equal_sc_number((m),(b),(a))
#define equal_cd_number(m,a,b) equal_dc_number((m),(b),(a))
#define equal_cl_number(m,a,b) equal_lc_number((m),(b),(a))

_g void sign_reverse_complex_common(addr pos, addr *ret);
_g void sign_reverse_complex_local(LocalRoot local, addr pos, addr *ret);
_g void abs_complex_common(addr pos, addr *ret);
_g void signum_complex_common(addr pos, addr *ret);

#endif

