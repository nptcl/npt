#ifndef __RATIO_HEADER__
#define __RATIO_HEADER__

#include "local.h"
#include "typedef.h"

#define SetNumerRatio_Low(p,v) SetArrayA2((p),0,(v))
#define GetNumerRatio_Low(p,v) GetArrayA2((p),0,(v))
#define SetDenomRatio_Low(p,v) SetArrayA2((p),1,(v))
#define GetDenomRatio_Low(p,v) GetArrayA2((p),1,(v))
#define SetSignRatio_Low(p,v) SetUser((p), (byte)(v))
#define GetSignRatio_Low(p,v) (*(v) = (int)GetUser(p))
#define RefSignRatio_Low(p)   ((int)GetUser(p))

#ifdef LISP_DEBUG
#define SetNumerRatio setnumer_ratio
#define GetNumerRatio getnumer_ratio
#define SetDenomRatio setdenom_ratio
#define GetDenomRatio getdenom_ratio
#define SetSignRatio setsign_ratio
#define GetSignRatio getsign_ratio
#define RefSignRatio refsign_ratio
#else
#define SetNumerRatio SetNumerRatio_Low
#define GetNumerRatio GetNumerRatio_Low
#define SetDenomRatio SetDenomRatio_Low
#define GetDenomRatio GetDenomRatio_Low
#define SetSignRatio SetSignRatio_Low
#define GetSignRatio GetSignRatio_Low
#define RefSignRatio RefSignRatio_Low
#endif

_g int ratiop(addr pos);
_g void setnumer_ratio(addr pos, addr value);
_g void getnumer_ratio(addr pos, addr *ret);
_g void setdenom_ratio(addr pos, addr value);
_g void getdenom_ratio(addr pos, addr *ret);
_g void setsign_ratio(addr pos, int sign);
_g void getsign_ratio(addr pos, int *ret);
_g int refsign_ratio(addr pos);
_g int getfixnum_ratio(addr pos, fixnum *ret);
_g int getfixed1_ratio(addr pos, int *sign, fixed *ret);

_g void euclidean_bignum(LocalRoot local, addr numer, addr denom);
_g void reduction_local(LocalRoot local, addr numer, addr denom);
_g void ratio_noreduction_heap(addr *ret, int sign, addr numer, addr denom);
_g void ratio_noreduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
_g void ratio_reduction_nocopy_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
_g void make_ratio_reduction_heap(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
_g void make_ratio_reduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
_g void make_ratio_heap(addr *ret, int sign, addr numer, addr denom);
_g void make_ratio_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
_g void make_ratio_alloc(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
_g void make_ratio_alloc_unsafe(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
_g void ratio_reduction_heap(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
_g void ratio_reduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);

_g void ratio_reduction_value_local(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom);
_g void ratio_reduction_value_heap(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom);
_g void ratio_noreduction_value_local(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom);
_g void ratio_noreduction_value_heap(addr *ret,
		int sign, bigtype numer, bigtype denom);

_g void ratio_zero_alloc(LocalRoot local, addr *ret);
_g void ratio_zero_local(LocalRoot local, addr *ret);
_g void ratio_zero_heap(addr *ret);

_g void ratio_copy_nosign_alloc(LocalRoot local, addr *ret, addr pos);
_g void ratio_copy_nosign_local(LocalRoot local, addr *ret, addr pos);
_g void ratio_copy_nosign_heap(addr *ret, addr pos);
_g void ratio_copy_alloc(LocalRoot local, addr *ret, addr pos);
_g void ratio_copy_local(LocalRoot local, addr *ret, addr pos);
_g void ratio_copy_heap(addr *ret, addr pos);
_g void ratio_throw_heap(addr pos, addr *ret);
_g void ratio_throw_local(LocalRoot local, addr pos, addr *ret);
_g void ratio_throw_alloc(LocalRoot local, addr pos, addr *ret);
_g void ratio_result_noreduction_local(LocalRoot local, addr pos, addr *ret);
_g void ratio_result_noreduction_heap(LocalRoot local, addr pos, addr *ret);

_g int zerop_ratio(addr left);
_g int plusp_ratio(addr left);
_g int minusp_ratio(addr left);

_g void cast_fixnum_ratio_local(LocalRoot local, addr pos, addr *ret);
_g void cast_bignum_ratio_local(LocalRoot local, addr pos, addr *ret);

_g int single_float_ratio_(addr pos, single_float *ret);
_g int double_float_ratio_(addr pos, double_float *ret);
_g int long_float_ratio_(addr pos, long_float *ret);

_g int single_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos);
_g int single_float_ratio_local_(LocalRoot local, addr *ret, addr pos);
_g int single_float_ratio_heap_(addr *ret, addr pos);
_g int double_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos);
_g int double_float_ratio_local_(LocalRoot local, addr *ret, addr pos);
_g int double_float_ratio_heap_(addr *ret, addr pos);
_g int long_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos);
_g int long_float_ratio_local_(LocalRoot local, addr *ret, addr pos);
_g int long_float_ratio_heap_(addr *ret, addr pos);

_g void abs_ratio_alloc(LocalRoot local, addr left, addr *ret);
_g void abs_ratio_local(LocalRoot local, addr left, addr *ret);
_g void abs_ratio_heap(addr left, addr *ret);

_g int output_nosign_ratio_(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp);

#endif

