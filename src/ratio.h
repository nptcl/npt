#ifndef __RATIO_HEADER__
#define __RATIO_HEADER__

#include "local.h"
#include "typedef.h"

#define ratiop _n(ratiop)
#define setnumer_ratio _n(setnumer_ratio)
#define getnumer_ratio _n(getnumer_ratio)
#define setdenom_ratio _n(setdenom_ratio)
#define getdenom_ratio _n(getdenom_ratio)
#define setsign_ratio _n(setsign_ratio)
#define getsign_ratio _n(getsign_ratio)
#define refsign_ratio _n(refsign_ratio)
#define getfixnum_ratio _n(getfixnum_ratio)
#define getfixed1_ratio _n(getfixed1_ratio)
#define euclidean_bignum _n(euclidean_bignum)
#define reduction_local _n(reduction_local)
#define ratio_noreduction_heap _n(ratio_noreduction_heap)
#define ratio_noreduction_local _n(ratio_noreduction_local)
#define ratio_reduction_nocopy_local _n(ratio_reduction_nocopy_local)
#define make_ratio_reduction_heap _n(make_ratio_reduction_heap)
#define make_ratio_reduction_local _n(make_ratio_reduction_local)
#define make_ratio_heap _n(make_ratio_heap)
#define make_ratio_local _n(make_ratio_local)
#define make_ratio_alloc _n(make_ratio_alloc)
#define make_ratio_alloc_unsafe _n(make_ratio_alloc_unsafe)
#define ratio_reduction_heap _n(ratio_reduction_heap)
#define ratio_reduction_local _n(ratio_reduction_local)
#define ratio_reduction_value_local _n(ratio_reduction_value_local)
#define ratio_reduction_value_heap _n(ratio_reduction_value_heap)
#define ratio_noreduction_value_local _n(ratio_noreduction_value_local)
#define ratio_noreduction_value_heap _n(ratio_noreduction_value_heap)
#define ratio_zero_alloc _n(ratio_zero_alloc)
#define ratio_zero_local _n(ratio_zero_local)
#define ratio_zero_heap _n(ratio_zero_heap)
#define ratio_copy_nosign_alloc _n(ratio_copy_nosign_alloc)
#define ratio_copy_nosign_local _n(ratio_copy_nosign_local)
#define ratio_copy_nosign_heap _n(ratio_copy_nosign_heap)
#define ratio_copy_alloc _n(ratio_copy_alloc)
#define ratio_copy_local _n(ratio_copy_local)
#define ratio_copy_heap _n(ratio_copy_heap)
#define ratio_throw_heap _n(ratio_throw_heap)
#define ratio_throw_local _n(ratio_throw_local)
#define ratio_throw_alloc _n(ratio_throw_alloc)
#define ratio_result_noreduction_local _n(ratio_result_noreduction_local)
#define ratio_result_noreduction_heap _n(ratio_result_noreduction_heap)
#define zerop_ratio _n(zerop_ratio)
#define plusp_ratio _n(plusp_ratio)
#define minusp_ratio _n(minusp_ratio)
#define cast_fixnum_ratio_local _n(cast_fixnum_ratio_local)
#define cast_bignum_ratio_local _n(cast_bignum_ratio_local)
#define single_float_ratio_ _n(single_float_ratio_)
#define double_float_ratio_ _n(double_float_ratio_)
#define long_float_ratio_ _n(long_float_ratio_)
#define single_float_ratio_alloc_ _n(single_float_ratio_alloc_)
#define single_float_ratio_local_ _n(single_float_ratio_local_)
#define single_float_ratio_heap_ _n(single_float_ratio_heap_)
#define double_float_ratio_alloc_ _n(double_float_ratio_alloc_)
#define double_float_ratio_local_ _n(double_float_ratio_local_)
#define double_float_ratio_heap_ _n(double_float_ratio_heap_)
#define long_float_ratio_alloc_ _n(long_float_ratio_alloc_)
#define long_float_ratio_local_ _n(long_float_ratio_local_)
#define long_float_ratio_heap_ _n(long_float_ratio_heap_)
#define abs_ratio_alloc _n(abs_ratio_alloc)
#define abs_ratio_local _n(abs_ratio_local)
#define abs_ratio_heap _n(abs_ratio_heap)
#define output_nosign_ratio_ _n(output_nosign_ratio_)

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

int ratiop(addr pos);
void setnumer_ratio(addr pos, addr value);
void getnumer_ratio(addr pos, addr *ret);
void setdenom_ratio(addr pos, addr value);
void getdenom_ratio(addr pos, addr *ret);
void setsign_ratio(addr pos, int sign);
void getsign_ratio(addr pos, int *ret);
int refsign_ratio(addr pos);
int getfixnum_ratio(addr pos, fixnum *ret);
int getfixed1_ratio(addr pos, int *sign, fixed *ret);

void euclidean_bignum(LocalRoot local, addr numer, addr denom);
void reduction_local(LocalRoot local, addr numer, addr denom);
void ratio_noreduction_heap(addr *ret, int sign, addr numer, addr denom);
void ratio_noreduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void ratio_reduction_nocopy_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void make_ratio_reduction_heap(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void make_ratio_reduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void make_ratio_heap(addr *ret, int sign, addr numer, addr denom);
void make_ratio_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void make_ratio_alloc(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void make_ratio_alloc_unsafe(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void ratio_reduction_heap(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);
void ratio_reduction_local(LocalRoot local,
		addr *ret, int sign, addr numer, addr denom);

void ratio_reduction_value_local(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom);
void ratio_reduction_value_heap(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom);
void ratio_noreduction_value_local(LocalRoot local, addr *ret,
		int sign, bigtype numer, bigtype denom);
void ratio_noreduction_value_heap(addr *ret,
		int sign, bigtype numer, bigtype denom);

void ratio_zero_alloc(LocalRoot local, addr *ret);
void ratio_zero_local(LocalRoot local, addr *ret);
void ratio_zero_heap(addr *ret);

void ratio_copy_nosign_alloc(LocalRoot local, addr *ret, addr pos);
void ratio_copy_nosign_local(LocalRoot local, addr *ret, addr pos);
void ratio_copy_nosign_heap(addr *ret, addr pos);
void ratio_copy_alloc(LocalRoot local, addr *ret, addr pos);
void ratio_copy_local(LocalRoot local, addr *ret, addr pos);
void ratio_copy_heap(addr *ret, addr pos);
void ratio_throw_heap(addr pos, addr *ret);
void ratio_throw_local(LocalRoot local, addr pos, addr *ret);
void ratio_throw_alloc(LocalRoot local, addr pos, addr *ret);
void ratio_result_noreduction_local(LocalRoot local, addr pos, addr *ret);
void ratio_result_noreduction_heap(LocalRoot local, addr pos, addr *ret);

int zerop_ratio(addr left);
int plusp_ratio(addr left);
int minusp_ratio(addr left);

void cast_fixnum_ratio_local(LocalRoot local, addr pos, addr *ret);
void cast_bignum_ratio_local(LocalRoot local, addr pos, addr *ret);

int single_float_ratio_(addr pos, single_float *ret);
int double_float_ratio_(addr pos, double_float *ret);
int long_float_ratio_(addr pos, long_float *ret);

int single_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos);
int single_float_ratio_local_(LocalRoot local, addr *ret, addr pos);
int single_float_ratio_heap_(addr *ret, addr pos);
int double_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos);
int double_float_ratio_local_(LocalRoot local, addr *ret, addr pos);
int double_float_ratio_heap_(addr *ret, addr pos);
int long_float_ratio_alloc_(LocalRoot local, addr *ret, addr pos);
int long_float_ratio_local_(LocalRoot local, addr *ret, addr pos);
int long_float_ratio_heap_(addr *ret, addr pos);

void abs_ratio_alloc(LocalRoot local, addr left, addr *ret);
void abs_ratio_local(LocalRoot local, addr left, addr *ret);
void abs_ratio_heap(addr left, addr *ret);

int output_nosign_ratio_(LocalRoot local,
		addr stream, addr pos, unsigned base, int upperp);

#endif

