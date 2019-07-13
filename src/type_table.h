#ifndef __TYPE_TABLE_HEADER__
#define __TYPE_TABLE_HEADER__

#include "constant.h"
#include "type.h"
#include "type_constant.h"
#include "typedef.h"

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
_g void type2or_alloc(LocalRoot local, addr a, addr b, addr *ret);
_g void type3or_alloc(LocalRoot local, addr a, addr b, addr c, addr *ret);
_g void type4or_alloc(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret);
_g void type2and_local(LocalRoot local, addr a, addr b, addr *ret);
_g void type2or_local(LocalRoot local, addr a, addr b, addr *ret);
_g void type3or_local(LocalRoot local, addr a, addr b, addr c, addr *ret);
_g void type4or_local(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret);
_g void type2and_heap(addr a, addr b, addr *ret);
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

