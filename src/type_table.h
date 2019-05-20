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
addr reftypetable(enum TypeTable index);
void gettypetable(enum TypeTable index, addr *ret);
void settypetable(enum TypeTable index, addr pos);
void keytypetable(constindex name, enum TypeTable type, addr *ret);
void build_type_table(void);
void build_type_constant(void);

/* arguments */
void typeargs_empty(addr *ret);
void typeargs_full(addr *ret, addr var, addr opt, addr rest, addr key);
void typeargs_var1(addr *ret, addr v1);
void typeargs_var2(addr *ret, addr v1, addr v2);
void typeargs_var3(addr *ret, addr v1, addr v2, addr v3);
void typeargs_var4(addr *ret, addr v1, addr v2, addr v3, addr v4);
void typeargs_var5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5);
void typeargs_var1key(addr *ret, addr v1, addr key);
void typeargs_var2key(addr *ret, addr v1, addr v2, addr key);
void typeargs_var3key(addr *ret, addr v1, addr v2, addr v3, addr key);
void typeargs_var4key(addr *ret, addr v1, addr v2, addr v3, addr v4, addr key);
void typeargs_opt1(addr *ret, addr v1);
void typeargs_opt2(addr *ret, addr v1, addr v2);
void typeargs_opt3(addr *ret, addr v1, addr v2, addr v3);
void typeargs_opt4(addr *ret, addr v1, addr v2, addr v3, addr v4);
void typeargs_opt5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5);
void typeargs_var1opt1(addr *ret, addr var1, addr opt1);
void typeargs_var1opt2(addr *ret, addr var1, addr opt1, addr opt2);
void typeargs_var1opt2key(addr *ret, addr var1, addr opt1, addr opt2, addr key);
void typeargs_var2opt1(addr *ret, addr var1, addr var2, addr opt1);
void typeargs_var2opt2(addr *ret, addr var1, addr var2, addr opt1, addr opt2);
void typeargs_var3opt1(addr *ret, addr var1, addr var2, addr var3, addr opt1);
void typeargs_var4opt1(addr *ret, addr v1, addr v2, addr v3, addr v4, addr opt1);
void typeargs_var1rest(addr *ret, addr v1, addr rest);
void typeargs_var2rest(addr *ret, addr v1, addr v2, addr rest);
void typeargs_var3rest(addr *ret, addr v1, addr v2, addr v3, addr rest);
void typeargs_var4rest(addr *ret, addr v1, addr v2, addr v3, addr v4, addr rest);
void typeargs_rest(addr *ret, addr rest);
void typeargs_key(addr *ret, addr key);
void typeargs_method(addr type);
void typeargs_methodkey(addr type);

/* values */
void typevalues_result(addr *ret, addr v1);
void typevalues_values2(addr *ret, addr v1, addr v2);
void typevalues_values3(addr *ret, addr v1, addr v2, addr v3);
void typevalues_values4(addr *ret, addr v1, addr v2, addr v3, addr v4);
void typevalues_values5(addr *ret, addr v1, addr v2, addr v3, addr v4, addr v5);
void typevalues_rest(addr *ret, addr type);

/* asterisk */
void type1aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret);
void type2aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret);
void type3aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret);
void type4aster_alloc(LocalRoot local, enum LISPDECL type, addr *ret);
void type1aster_local(LocalRoot local, enum LISPDECL type, addr *ret);
void type2aster_local(LocalRoot local, enum LISPDECL type, addr *ret);
void type3aster_local(LocalRoot local, enum LISPDECL type, addr *ret);
void type4aster_local(LocalRoot local, enum LISPDECL type, addr *ret);
void type1aster_heap(enum LISPDECL type, addr *ret);
void type2aster_heap(enum LISPDECL type, addr *ret);
void type3aster_heap(enum LISPDECL type, addr *ret);
void type4aster_heap(enum LISPDECL type, addr *ret);

/* and/or */
void type2and_alloc(LocalRoot local, addr a, addr b, addr *ret);
void type2or_alloc(LocalRoot local, addr a, addr b, addr *ret);
void type3or_alloc(LocalRoot local, addr a, addr b, addr c, addr *ret);
void type4or_alloc(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret);
void type2and_local(LocalRoot local, addr a, addr b, addr *ret);
void type2or_local(LocalRoot local, addr a, addr b, addr *ret);
void type3or_local(LocalRoot local, addr a, addr b, addr c, addr *ret);
void type4or_local(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret);
void type2and_heap(addr a, addr b, addr *ret);
void type2or_heap(addr a, addr b, addr *ret);
void type3or_heap(addr a, addr b, addr c, addr *ret);
void type4or_heap(addr a, addr b, addr c, addr d, addr *ret);

/* range */
void type1real_heap(enum LISPDECL type, addr value, addr *ret);
void type4integer_heap(addr a, fixnum b, addr c, fixnum d, addr *ret);
void type2integer_ab_heap(addr a, fixnum b, addr *ret);
void type2integer_cd_heap(addr c, fixnum d, addr *ret);
void type4float_heap(addr a, float b, addr c, float d, addr *ret);
void type2float_ab_heap(addr a, float b, addr *ret);
void type2float_cd_heap(addr c, float d, addr *ret);
void type4realf_heap(addr a, float b, addr c, float d, addr *ret);
void type2realf_ab_heap(addr a, float b, addr *ret);
void type2realf_cd_heap(addr c, float d, addr *ret);

#endif

