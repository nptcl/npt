#ifndef __TYPE_MEMORY_HEADER__
#define __TYPE_MEMORY_HEADER__

#include "local.h"
#include "memory.h"
#include "typedef.h"

#define type_alloc _n(type_alloc)
#define type_local _n(type_local)
#define type_heap _n(type_heap)

#define type_lowlispdecl _n(type_lowlispdecl)
#define type_reflispdecl _n(type_reflispdecl)
#define type_getlispdecl _n(type_getlispdecl)
#define type_setlispdecl _n(type_setlispdecl)
#define type_refnotdecl _n(type_refnotdecl)
#define type_getnotdecl _n(type_getnotdecl)
#define type_setnotdecl _n(type_setnotdecl)
#define type_revnotdecl _n(type_revnotdecl)
#define type_setnotobject _n(type_setnotobject)
#define type_refarraytype _n(type_refarraytype)
#define type_getarraytype _n(type_getarraytype)
#define type_setarraytype _n(type_setarraytype)
#define type_lenarraytype _n(type_lenarraytype)
#define type_getvalues1 _n(type_getvalues1)

#define type0_alloc _n(type0_alloc)
#define type1_alloc _n(type1_alloc)
#define type2_alloc _n(type2_alloc)
#define type3_alloc _n(type3_alloc)
#define type4_alloc _n(type4_alloc)
#define type0_local _n(type0_local)
#define type1_local _n(type1_local)
#define type2_local _n(type2_local)
#define type3_local _n(type3_local)
#define type4_local _n(type4_local)
#define type0_heap _n(type0_heap)
#define type1_heap _n(type1_heap)
#define type2_heap _n(type2_heap)
#define type3_heap _n(type3_heap)
#define type4_heap _n(type4_heap)
#define type0not_alloc _n(type0not_alloc)
#define type1not_alloc _n(type1not_alloc)
#define type2not_alloc _n(type2not_alloc)
#define type3not_alloc _n(type3not_alloc)
#define type4not_alloc _n(type4not_alloc)
#define type0not_local _n(type0not_local)
#define type1not_local _n(type1not_local)
#define type2not_local _n(type2not_local)
#define type3not_local _n(type3not_local)
#define type4not_local _n(type4not_local)
#define type0not_heap _n(type0not_heap)
#define type1not_heap _n(type1not_heap)
#define type2not_heap _n(type2not_heap)
#define type3not_heap _n(type3not_heap)
#define type4not_heap _n(type4not_heap)
#define type1aster_localall _n(type1aster_localall)
#define type2aster_localall _n(type2aster_localall)
#define type3aster_localall _n(type3aster_localall)
#define type4aster_localall _n(type4aster_localall)

#define type_copy_unsafe_alloc _n(type_copy_unsafe_alloc)
#define type_copy_unsafe_local _n(type_copy_unsafe_local)
#define type_copy_unsafe_heap _n(type_copy_unsafe_heap)
#define type_copydecl_unsafe_alloc _n(type_copydecl_unsafe_alloc)
#define type_copydecl_unsafe_local _n(type_copydecl_unsafe_local)
#define type_copydecl_unsafe_heap _n(type_copydecl_unsafe_heap)

#define type_eql_alloc _n(type_eql_alloc)
#define type_eql_local _n(type_eql_local)
#define type_eql_heap _n(type_eql_heap)
#define type_member_heap _n(type_member_heap)
#define type_satisfies_heap _n(type_satisfies_heap)
#define type_values_local _n(type_values_local)
#define type_values_heap _n(type_values_heap)
#define type_signed_alloc _n(type_signed_alloc)
#define type_signed_local _n(type_signed_local)
#define type_signed_heap _n(type_signed_heap)
#define type_unsigned_alloc _n(type_unsigned_alloc)
#define type_unsigned_local _n(type_unsigned_local)
#define type_unsigned_heap _n(type_unsigned_heap)
#define type_function_heap _n(type_function_heap)
#define type_compiled_heap _n(type_compiled_heap)
#define type_clos_heap _n(type_clos_heap)
#define type_error_heap _n(type_error_heap)

#define LowLispDecl_Low(p)		((LispDecl)(GetUser(p)))
#define RefLispDecl_Low(p)		((LispDecl)(LowLispDecl_Low(p) & 0x7F))
#define GetLispDecl_Low(p,v)	(*(v) = RefLispDecl(p))
#define SetLispDecl_Low(p,v)	SetUser((p), (byte)(v))
#define RefNotDecl_Low(p)		((int)(GetUser(p) & 0x80))
#define GetNotDecl_Low(p,v)		(*(v) = (int)RefNotDecl(p))
#define SetNotDecl_Low			type_setnotdecl
#define RefArrayType_Low		RefArrayA2
#define GetArrayType_Low		GetArrayA2
#define SetArrayType_Low		SetArrayA2
#define LenArrayType_Low		LenArrayA2

#ifdef LISP_DEBUG
#define LowLispDecl				LowLispDecl_Low
#define RefLispDecl				RefLispDecl_Low
#define GetLispDecl				GetLispDecl_Low
#define SetLispDecl				SetLispDecl_Low
#define RefNotDecl				RefNotDecl_Low
#define GetNotDecl				GetNotDecl_Low
#define SetNotDecl				type_setnotdecl
#define RefArrayType			RefArrayType_Low
#define GetArrayType			GetArrayType_Low
#define SetArrayType			SetArrayType_Low
#define LenArrayType			LenArrayType_Low
#else
#define LowLispDecl				type_lowlispdecl
#define RefLispDecl				type_reflispdecl
#define GetLispDecl				type_getlispdecl
#define SetLispDecl				type_setlispdecl
#define RefNotDecl				type_refnotdecl
#define GetNotDecl				type_getnotdecl
#define SetNotDecl				type_setnotdecl
#define RefArrayType			type_refarraytype
#define GetArrayType			type_getarraytype
#define SetArrayType			type_setarraytype
#define LenArrayType			type_lenarraytype
#endif

/* allocate */
void type_alloc(LocalRoot local, addr *ret, LispDecl type, size_t size);
void type_local(LocalRoot local, addr *ret, LispDecl type, size_t size);
void type_heap(addr *ret, LispDecl type, size_t size);

/* access */
LispDecl type_lowlispdecl(addr pos);
LispDecl type_reflispdecl(addr pos);
void type_getlispdecl(addr pos, LispDecl *ret);
void type_setlispdecl(addr pos, LispDecl value);
int type_refnotdecl(addr pos);
void type_getnotdecl(addr pos, int *ret);
void type_setnotdecl(addr pos, int value);
void type_revnotdecl(addr pos);
void type_setnotobject(addr pos, addr value);
addr type_refarraytype(addr pos, size_t index);
void type_getarraytype(addr pos, size_t index, addr *ret);
void type_setarraytype(addr pos, size_t index, addr value);
void type_lenarraytype(addr pos, size_t *ret);
void type_getvalues1(addr type, addr *ret);

/* object */
void type0_alloc(LocalRoot local, LispDecl type, addr *ret);
void type1_alloc(LocalRoot local, LispDecl type, addr a, addr *ret);
void type2_alloc(LocalRoot local, LispDecl type, addr a, addr b, addr *ret);
void type3_alloc(LocalRoot local, LispDecl type, addr a, addr b, addr c, addr *ret);
void type4_alloc(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret);
void type0_local(LocalRoot local, LispDecl type, addr *ret);
void type1_local(LocalRoot local, LispDecl type, addr a, addr *ret);
void type2_local(LocalRoot local, LispDecl type, addr a, addr b, addr *ret);
void type3_local(LocalRoot local, LispDecl type, addr a, addr b, addr c, addr *ret);
void type4_local(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret);
void type0_heap(LispDecl type, addr *ret);
void type1_heap(LispDecl type, addr a, addr *ret);
void type2_heap(LispDecl type, addr a, addr b, addr *ret);
void type3_heap(LispDecl type, addr a, addr b, addr c, addr *ret);
void type4_heap(LispDecl type, addr a, addr b, addr c, addr d, addr *ret);

void type0not_alloc(LocalRoot local, LispDecl type, addr *ret);
void type1not_alloc(LocalRoot local, LispDecl type, addr a, addr *ret);
void type2not_alloc(LocalRoot local, LispDecl type, addr a, addr b, addr *ret);
void type3not_alloc(LocalRoot local, LispDecl type, addr a, addr b, addr c, addr *ret);
void type4not_alloc(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret);
void type0not_local(LocalRoot local, LispDecl type, addr *ret);
void type1not_local(LocalRoot local, LispDecl type, addr a, addr *ret);
void type2not_local(LocalRoot local, LispDecl type, addr a, addr b, addr *ret);
void type3not_local(LocalRoot local, LispDecl type, addr a, addr b, addr c, addr *ret);
void type4not_local(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret);
void type0not_heap(LispDecl type, addr *ret);
void type1not_heap(LispDecl type, addr a, addr *ret);
void type2not_heap(LispDecl type, addr a, addr b, addr *ret);
void type3not_heap(LispDecl type, addr a, addr b, addr c, addr *ret);
void type4not_heap(LispDecl type, addr a, addr b, addr c, addr d, addr *ret);

void type1aster_localall(LocalRoot local, LispDecl type, addr *ret);
void type2aster_localall(LocalRoot local, LispDecl type, addr *ret);
void type3aster_localall(LocalRoot local, LispDecl type, addr *ret);
void type4aster_localall(LocalRoot local, LispDecl type, addr *ret);

/* copy */
void type_copy_unsafe_alloc(LocalRoot local, addr *ret, addr left);
void type_copy_unsafe_local(LocalRoot local, addr *ret, addr left);
void type_copy_unsafe_heap(addr *ret, addr left);
void type_copydecl_unsafe_alloc(LocalRoot local, addr *ret, addr left);
void type_copydecl_unsafe_local(LocalRoot local, addr *ret, addr left);
void type_copydecl_unsafe_heap(addr *ret, addr left);

/* make */
void type_eql_alloc(LocalRoot local, addr pos, addr *ret);
void type_eql_local(LocalRoot local, addr pos, addr *ret);
void type_eql_heap(addr pos, addr *ret);
void type_member_heap(addr *ret, ...);
void type_satisfies_heap(addr call, addr *ret);
void type_values_local(LocalRoot local, addr v1, addr v2, addr v3, addr v4, addr *ret);
void type_values_heap(addr v1, addr v2, addr v3, addr v4, addr *ret);
void type_signed_alloc(LocalRoot local, fixnum value, addr *ret);
void type_signed_local(LocalRoot local, fixnum value, addr *ret);
void type_signed_heap(fixnum value, addr *ret);
void type_unsigned_alloc(LocalRoot local, fixnum value, addr *ret);
void type_unsigned_local(LocalRoot local, fixnum value, addr *ret);
void type_unsigned_heap(fixnum value, addr *ret);
void type_function_heap(addr args, addr values, addr *ret);
void type_compiled_heap(addr args, addr values, addr *ret);
void type_clos_heap(addr clos, addr *ret);
void type_error_heap(addr pos, addr *ret);

#endif

