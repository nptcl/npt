#ifndef __TYPE_TABLE_HEADER__
#define __TYPE_TABLE_HEADER__

#include "constant.h"
#include "type.h"
#include "type_constant.h"
#include "typedef.h"

#define reftypetable _n(reftypetable)
#define gettypetable _n(gettypetable)
#define settypetable _n(settypetable)
#define keytypetable _n(keytypetable)
#define build_type_table _n(build_type_table)
#define build_type_constant _n(build_type_constant)
#define typeargs_empty _n(typeargs_empty)
#define typeargs_full _n(typeargs_full)
#define typeargs_var1 _n(typeargs_var1)
#define typeargs_var2 _n(typeargs_var2)
#define typeargs_var3 _n(typeargs_var3)
#define typeargs_var4 _n(typeargs_var4)
#define typeargs_var5 _n(typeargs_var5)
#define typeargs_var1key _n(typeargs_var1key)
#define typeargs_var2key _n(typeargs_var2key)
#define typeargs_var3key _n(typeargs_var3key)
#define typeargs_var4key _n(typeargs_var4key)
#define typeargs_opt1 _n(typeargs_opt1)
#define typeargs_opt2 _n(typeargs_opt2)
#define typeargs_opt3 _n(typeargs_opt3)
#define typeargs_opt4 _n(typeargs_opt4)
#define typeargs_opt5 _n(typeargs_opt5)
#define typeargs_var1opt1 _n(typeargs_var1opt1)
#define typeargs_var1opt2 _n(typeargs_var1opt2)
#define typeargs_var1opt2key _n(typeargs_var1opt2key)
#define typeargs_var2opt1 _n(typeargs_var2opt1)
#define typeargs_var2opt2 _n(typeargs_var2opt2)
#define typeargs_var2opt3 _n(typeargs_var2opt3)
#define typeargs_var3opt1 _n(typeargs_var3opt1)
#define typeargs_var4opt1 _n(typeargs_var4opt1)
#define typeargs_var1rest _n(typeargs_var1rest)
#define typeargs_var2rest _n(typeargs_var2rest)
#define typeargs_var3rest _n(typeargs_var3rest)
#define typeargs_var4rest _n(typeargs_var4rest)
#define typeargs_opt1rest _n(typeargs_opt1rest)
#define typeargs_rest _n(typeargs_rest)
#define typeargs_key _n(typeargs_key)
#define typeargs_method _n(typeargs_method)
#define typeargs_methodkey _n(typeargs_methodkey)
#define typevalues_result _n(typevalues_result)
#define typevalues_values2 _n(typevalues_values2)
#define typevalues_values3 _n(typevalues_values3)
#define typevalues_values4 _n(typevalues_values4)
#define typevalues_values5 _n(typevalues_values5)
#define typevalues_values_va _n(typevalues_values_va)
#define typevalues_rest _n(typevalues_rest)
#define type1aster_alloc _n(type1aster_alloc)
#define type2aster_alloc _n(type2aster_alloc)
#define type3aster_alloc _n(type3aster_alloc)
#define type4aster_alloc _n(type4aster_alloc)
#define type1aster_local _n(type1aster_local)
#define type2aster_local _n(type2aster_local)
#define type3aster_local _n(type3aster_local)
#define type4aster_local _n(type4aster_local)
#define type1aster_heap _n(type1aster_heap)
#define type2aster_heap _n(type2aster_heap)
#define type3aster_heap _n(type3aster_heap)
#define type4aster_heap _n(type4aster_heap)
#define type2and_alloc _n(type2and_alloc)
#define type3and_alloc _n(type3and_alloc)
#define type2and_local _n(type2and_local)
#define type3and_local _n(type3and_local)
#define type2and_heap _n(type2and_heap)
#define type3and_heap _n(type3and_heap)
#define type2or_alloc _n(type2or_alloc)
#define type3or_alloc _n(type3or_alloc)
#define type4or_alloc _n(type4or_alloc)
#define type2or_local _n(type2or_local)
#define type3or_local _n(type3or_local)
#define type4or_local _n(type4or_local)
#define type2or_heap _n(type2or_heap)
#define type3or_heap _n(type3or_heap)
#define type4or_heap _n(type4or_heap)
#define type1real_heap _n(type1real_heap)
#define type4integer_heap _n(type4integer_heap)
#define type2integer_ab_heap _n(type2integer_ab_heap)
#define type2integer_cd_heap _n(type2integer_cd_heap)
#define type4float_heap _n(type4float_heap)
#define type2float_ab_heap _n(type2float_ab_heap)
#define type2float_cd_heap _n(type2float_cd_heap)
#define type4realf_heap _n(type4realf_heap)
#define type2realf_ab_heap _n(type2realf_ab_heap)
#define type2realf_cd_heap _n(type2realf_cd_heap)
#define type_vector1_heap _n(type_vector1_heap)

/* interface */
#define GetTypeTable(a,b) gettypetable(TypeTable_##b, (a))
#define GetTypeArgs(a,b) gettypetable(TypeArgs_##b, (a))
#define GetTypeValues(a,b) gettypetable(TypeValues_##b, (a))
#define GetTypeCompiled(a,b) gettypetable(TypeCompiled_##b, (a))
#define SetTypeTable(x, y) settypetable(TypeTable_##x, y)
#define SetTypeArgs(x, y) settypetable(TypeArgs_##x, y)
#define SetTypeValues(x, y) settypetable(TypeValues_##x, y)
#define SetTypeCompiled(x, y) settypetable(TypeCompiled_##x, y)
#define KeyTypeTable(r,a,b) keytypetable(CONSTANT_KEYWORD_##a, TypeTable_##b, (r))
_g addr reftypetable(enum TypeTable index);
_g void gettypetable(enum TypeTable index, addr *ret);
_g void settypetable(enum TypeTable index, addr pos);
_g void keytypetable(constindex name, enum TypeTable type, addr *ret);
_g void build_type_table(void);
_g void build_type_constant(void);

/* arguments */
_g void typeargs_empty(addr *ret);
_g void typeargs_full(addr *ret, addr var, addr opt, addr rest, addr key);
_g void typeargs_var1(addr *ret, addr v1);
_g void typeargs_var2(addr *ret, addr v1, addr v2);
_g void typeargs_var3(addr *ret, addr v1, addr v2, addr v3);
_g void typeargs_var4(addr *ret, addr v1, addr v2, addr v3, addr v4);
_g void typeargs_var5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5);
_g void typeargs_var1key(addr *ret, addr v1, addr key);
_g void typeargs_var2key(addr *ret, addr v1, addr v2, addr key);
_g void typeargs_var3key(addr *ret, addr v1, addr v2, addr v3, addr key);
_g void typeargs_var4key(addr *ret, addr v1, addr v2, addr v3, addr v4, addr key);
_g void typeargs_opt1(addr *ret, addr v1);
_g void typeargs_opt2(addr *ret, addr v1, addr v2);
_g void typeargs_opt3(addr *ret, addr v1, addr v2, addr v3);
_g void typeargs_opt4(addr *ret, addr v1, addr v2, addr v3, addr v4);
_g void typeargs_opt5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5);
_g void typeargs_var1opt1(addr *ret, addr var1, addr opt1);
_g void typeargs_var1opt2(addr *ret, addr var1, addr opt1, addr opt2);
_g void typeargs_var1opt2key(addr *ret, addr var1, addr opt1, addr opt2, addr key);
_g void typeargs_var2opt1(addr *ret, addr var1, addr var2, addr opt1);
_g void typeargs_var2opt2(addr *ret, addr var1, addr var2, addr opt1, addr opt2);
_g void typeargs_var2opt3(addr *ret, addr v1, addr v2, addr o1, addr o2, addr o3);
_g void typeargs_var3opt1(addr *ret, addr var1, addr var2, addr var3, addr opt1);
_g void typeargs_var4opt1(addr *ret, addr v1, addr v2, addr v3, addr v4, addr opt1);
_g void typeargs_var1rest(addr *ret, addr v1, addr rest);
_g void typeargs_var2rest(addr *ret, addr v1, addr v2, addr rest);
_g void typeargs_var3rest(addr *ret, addr v1, addr v2, addr v3, addr rest);
_g void typeargs_var4rest(addr *ret, addr v1, addr v2, addr v3, addr v4, addr rest);
_g void typeargs_opt1rest(addr *ret, addr opt1, addr rest);
_g void typeargs_rest(addr *ret, addr rest);
_g void typeargs_key(addr *ret, addr key);
_g void typeargs_method(addr type);
_g void typeargs_methodkey(addr type);

/* values */
_g void typevalues_result(addr *ret, addr v1);
_g void typevalues_values2(addr *ret, addr v1, addr v2);
_g void typevalues_values3(addr *ret, addr v1, addr v2, addr v3);
_g void typevalues_values4(addr *ret, addr v1, addr v2, addr v3, addr v4);
_g void typevalues_values5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5);
_g void typevalues_values_va(addr *ret, ...);
_g void typevalues_rest(addr *ret, addr type);

/* asterisk */
_g void type1aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret);
_g void type2aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret);
_g void type3aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret);
_g void type4aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret);
_g void type1aster_local(LocalRoot local, enum LISPDECL type, addr *ret);
_g void type2aster_local(LocalRoot local, enum LISPDECL type, addr *ret);
_g void type3aster_local(LocalRoot local, enum LISPDECL type, addr *ret);
_g void type4aster_local(LocalRoot local, enum LISPDECL type, addr *ret);
_g void type1aster_heap(enum LISPDECL type, addr *ret);
_g void type2aster_heap(enum LISPDECL type, addr *ret);
_g void type3aster_heap(enum LISPDECL type, addr *ret);
_g void type4aster_heap(enum LISPDECL type, addr *ret);

/* and/or */
_g void type2and_alloc(LocalRoot local, addr a, addr b, addr *ret);
_g void type3and_alloc(LocalRoot local, addr a, addr b, addr c, addr *ret);
_g void type2and_local(LocalRoot local, addr a, addr b, addr *ret);
_g void type3and_local(LocalRoot local, addr a, addr b, addr c, addr *ret);
_g void type2and_heap(addr a, addr b, addr *ret);
_g void type3and_heap(addr a, addr b, addr c, addr *ret);
_g void type2or_alloc(LocalRoot local, addr a, addr b, addr *ret);
_g void type3or_alloc(LocalRoot local, addr a, addr b, addr c, addr *ret);
_g void type4or_alloc(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret);
_g void type2or_local(LocalRoot local, addr a, addr b, addr *ret);
_g void type3or_local(LocalRoot local, addr a, addr b, addr c, addr *ret);
_g void type4or_local(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret);
_g void type2or_heap(addr a, addr b, addr *ret);
_g void type3or_heap(addr a, addr b, addr c, addr *ret);
_g void type4or_heap(addr a, addr b, addr c, addr d, addr *ret);

/* range */
_g void type1real_heap(enum LISPDECL type, addr value, addr *ret);
_g void type4integer_heap(addr a, fixnum b, addr c, fixnum d, addr *ret);
_g void type2integer_ab_heap(addr a, fixnum b, addr *ret);
_g void type2integer_cd_heap(addr c, fixnum d, addr *ret);
_g void type4float_heap(addr a, float b, addr c, float d, addr *ret);
_g void type2float_ab_heap(addr a, float b, addr *ret);
_g void type2float_cd_heap(addr c, float d, addr *ret);
_g void type4realf_heap(addr a, float b, addr c, float d, addr *ret);
_g void type2realf_ab_heap(addr a, float b, addr *ret);
_g void type2realf_cd_heap(addr c, float d, addr *ret);

/* vector */
_g void type_vector1_heap(size_t size, addr *ret);

#endif

