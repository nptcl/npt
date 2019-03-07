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

int complexp(addr pos);
void setreal_complex(addr pos, addr value);
void getreal_complex(addr pos, addr *ret);
void setimag_complex(addr pos, addr value);
void getimag_complex(addr pos, addr *ret);
void settype_complex(addr pos, int value);
enum ComplexType gettype_complex(addr pos);
enum ComplexType getcomplex(addr pos, enum ComplexType *type, addr *real, addr *imag);
enum ComplexType getcomplexr(addr pos, addr *real, addr *imag);

void make_complex_unsafe(LocalRoot local, addr *ret, enum ComplexType type);
void complex_unsafe_alloc(LocalRoot local, addr *ret,
		addr real, addr imag, enum ComplexType type);
void complex_unsafe_local(LocalRoot local, addr *ret,
		addr real, addr imag, enum ComplexType type);
void complex_unsafe_heap(addr *ret,
		addr real, addr imag, enum ComplexType type);
void complex_alloc(LocalRoot local, addr *ret, addr real, addr imag);
void complex_local(LocalRoot local, addr *ret, addr real, addr imag);
void complex_heap(addr *ret, addr real, addr imag);

void complex_single_alloc(LocalRoot local,
		addr *ret, single_float real, single_float imag);
void complex_single_local(LocalRoot local,
		addr *ret, single_float real, single_float imag);
void complex_single_heap(addr *ret,
		single_float real, single_float imag);
void complex_double_alloc(LocalRoot local,
		addr *ret, double_float real, double_float imag);
void complex_double_local(LocalRoot local,
		addr *ret, double_float real, double_float imag);
void complex_double_heap(addr *ret,
		double_float real, double_float imag);
void complex_long_alloc(LocalRoot local,
		addr *ret, long_float real, long_float imag);
void complex_long_local(LocalRoot local,
		addr *ret, long_float real, long_float imag);
void complex_long_heap(addr *ret,
		long_float real, long_float imag);

void complex_copy_alloc(LocalRoot local, addr pos, addr *ret);
void complex_copy_local(LocalRoot local, addr pos, addr *ret);
void complex_copy_heap(addr pos, addr *ret);
void complex_result_local(LocalRoot local, addr pos, addr *ret);
void complex_result_heap(LocalRoot local, addr pos, addr *ret);
void complex_throw_alloc(LocalRoot local, addr pos, addr *ret);
void complex_throw_local(LocalRoot local, addr pos, addr *ret);
void complex_throw_heap(addr pos, addr *ret);

void single_float_complex(addr pos, single_float *re, single_float *im);
void double_float_complex(addr pos, double_float *re, double_float *im);
void long_float_complex(addr pos, long_float *re, long_float *im);

int zerop_complex(addr pos);
int eql_complex(addr left, addr right);
int equal_complex(LocalRoot local, addr left, addr right);
#define not_equal_complex(m,a,b) (! equal_complex((m),(a),(b)))
int equal_fc_number(addr left, addr right);
int equal_bc_number(addr left, addr right);
int equal_rc_number(LocalRoot local, addr left, addr right);
int equal_sc_number(LocalRoot local, addr left, addr right);
int equal_dc_number(LocalRoot local, addr left, addr right);
int equal_lc_number(LocalRoot local, addr left, addr right);
#define equal_cf_number(a,b) equal_fc_number((b),(a))
#define equal_cb_number(a,b) equal_bc_number((b),(a))
#define equal_cr_number(m,a,b) equal_rc_number((m),(b),(a))
#define equal_cs_number(m,a,b) equal_sc_number((m),(b),(a))
#define equal_cd_number(m,a,b) equal_dc_number((m),(b),(a))
#define equal_cl_number(m,a,b) equal_lc_number((m),(b),(a))

void sign_reverse_complex_common(addr pos, addr *ret);
void sign_reverse_complex_local(LocalRoot local, addr pos, addr *ret);
void abs_complex_common(LocalRoot local, addr pos, addr *ret);

#endif

