#ifndef __TYPE_HEADER__
#define __TYPE_HEADER__

#include "memory.h"
#include "type_decl.h"
#include "typedef.h"

#define LispDecl_Low(p)			((enum LISPDECL)(GetUser(p)))
#define RefLispDecl_Low(p)		((enum LISPDECL)(LispDecl_Low(p) & 0x7F))
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
#define LispDecl				LispDecl_Low
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
#define LispDecl				type_lispdecl
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
addr type_allocr(LocalRoot local, enum LISPDECL type, size_t size);
addr type_localr(LocalRoot local, enum LISPDECL type, size_t size);
addr type_heapr(enum LISPDECL type, size_t size);
void type_alloc(LocalRoot local, addr *ret, enum LISPDECL type, size_t size);
void type_local(LocalRoot local, addr *ret, enum LISPDECL type, size_t size);
void type_heap(addr *ret, enum LISPDECL type, size_t size);

enum LISPDECL type_lispdecl(addr pos);
enum LISPDECL type_reflispdecl(addr pos);
void type_getlispdecl(addr pos, enum LISPDECL *ret);
void type_setlispdecl(addr pos, enum LISPDECL value);
int type_refnotdecl(addr pos);
void type_getnotdecl(addr pos, int *ret);
void type_setnotdecl(addr pos, int value);
void type_revnotdecl(addr pos);
void type_setnotobject(addr pos, addr value);
addr type_refarraytype(addr pos, size_t index);
void type_getarraytype(addr pos, size_t index, addr *ret);
void type_setarraytype(addr pos, size_t index, addr value);
void type_lenarraytype(addr pos, size_t *ret);

void init_type(void);
void build_type(void);

/* check */
int decl_character_p(enum LISPDECL type);
int decl_float_p(enum LISPDECL type);
int decl_range_p(enum LISPDECL type);
int decl_subtypep_real(enum LISPDECL left, enum LISPDECL right);
int type_function_p(addr pos);
int type_astert_p(addr pos);
int type_function_aster_p(addr pos);
int type_asterisk_p(addr pos);
int type_range_p(addr pos);

/* copy */
void type_copy_unsafe_alloc(LocalRoot local, addr *ret, addr left);
void type_copy_unsafe_local(LocalRoot local, addr *ret, addr left);
void type_copy_unsafe_heap(addr *ret, addr left);
void type_copydecl_unsafe_alloc(LocalRoot local, addr *ret, addr left);
void type_copydecl_unsafe_local(LocalRoot local, addr *ret, addr left);
void type_copydecl_unsafe_heap(addr *ret, addr left);

/* object */
void type0_alloc(LocalRoot local, enum LISPDECL type, addr *ret);
void type1_alloc(LocalRoot local, enum LISPDECL type, addr a, addr *ret);
void type2_alloc(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret);
void type3_alloc(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr *ret);
void type4_alloc(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr d, addr *ret);
void type0_local(LocalRoot local, enum LISPDECL type, addr *ret);
void type1_local(LocalRoot local, enum LISPDECL type, addr a, addr *ret);
void type2_local(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret);
void type3_local(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr *ret);
void type4_local(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr d, addr *ret);
void type0_heap(enum LISPDECL type, addr *ret);
void type1_heap(enum LISPDECL type, addr a, addr *ret);
void type2_heap(enum LISPDECL type, addr a, addr b, addr *ret);
void type3_heap(enum LISPDECL type, addr a, addr b, addr c, addr *ret);
void type4_heap(enum LISPDECL type, addr a, addr b, addr c, addr d, addr *ret);

void type0not_alloc(LocalRoot local, enum LISPDECL type, addr *ret);
void type1not_alloc(LocalRoot local, enum LISPDECL type, addr a, addr *ret);
void type2not_alloc(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret);
void type3not_alloc(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr *ret);
void type4not_alloc(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr d, addr *ret);
void type0not_local(LocalRoot local, enum LISPDECL type, addr *ret);
void type1not_local(LocalRoot local, enum LISPDECL type, addr a, addr *ret);
void type2not_local(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret);
void type3not_local(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr *ret);
void type4not_local(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr d, addr *ret);
void type0not_heap(enum LISPDECL type, addr *ret);
void type1not_heap(enum LISPDECL type, addr a, addr *ret);
void type2not_heap(enum LISPDECL type, addr a, addr b, addr *ret);
void type3not_heap(enum LISPDECL type, addr a, addr b, addr c, addr *ret);
void type4not_heap(enum LISPDECL type, addr a, addr b, addr c, addr d, addr *ret);

void type1aster_localall(LocalRoot local, enum LISPDECL type, addr *ret);
void type2aster_localall(LocalRoot local, enum LISPDECL type, addr *ret);
void type3aster_localall(LocalRoot local, enum LISPDECL type, addr *ret);
void type4aster_localall(LocalRoot local, enum LISPDECL type, addr *ret);

/* etc */
void type_eql_alloc(LocalRoot local, addr pos, addr *ret);
void type_eql_local(LocalRoot local, addr pos, addr *ret);
void type_eql_heap(addr pos, addr *ret);
void type_member_heap(addr *ret, ...);
void type_satisfies_heap(addr call, addr *ret);
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

#endif

