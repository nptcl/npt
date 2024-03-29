#include "function.h"
#include "heap.h"
#include "local.h"
#include "memory.h"
#include "object.h"
#include "symbol.h"
#include "type_memory.h"
#include "typedef.h"

/*
 *  allocate
 */
void type_alloc(LocalRoot local, addr *ret, LispDecl type, size_t size)
{
	addr pos;

	Check(LISPDECL_SIZE <= type, "type too large.");
	alloc_array2(local, &pos, LISPTYPE_TYPE, size);
	SetUser(pos, (byte)type);
	*ret = pos;
}

void type_local(LocalRoot local, addr *ret, LispDecl type, size_t size)
{
	CheckLocal(local);
	type_alloc(local, ret, type, size);
}

void type_heap(addr *ret, LispDecl type, size_t size)
{
	type_alloc(NULL, ret, type, size);
}


/*
 *  access
 */
LispDecl type_lowlispdecl(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return LowLispDecl_Low(pos);
}

LispDecl type_reflispdecl(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return RefLispDecl_Low(pos);
}

void type_getlispdecl(addr pos, LispDecl *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	GetLispDecl_Low(pos, ret);
}

void type_setlispdecl(addr pos, LispDecl value)
{
	CheckType(pos, LISPTYPE_TYPE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetLispDecl_Low(pos, value);
}

int type_refnotdecl(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return RefNotDecl_Low(pos);
}

void type_getnotdecl(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	GetNotDecl_Low(pos, ret);
}

void type_setnotdecl(addr pos, int value)
{
	byte user;

	CheckType(pos, LISPTYPE_TYPE);
	Check(GetStatusReadOnly(pos), "readonly error");
	user = GetUser(pos);
	if (value)
		SetUser(pos, 0x80 | user);
	else
		SetUser(pos, 0x7F & user);
}

void type_revnotdecl(addr pos)
{
	type_setnotdecl(pos, RefNotDecl(pos) == 0);
}

void type_setnotobject(addr pos, addr value)
{
	SetNotDecl(pos, RefNotDecl(value));
}

addr type_refarraytype(addr pos, size_t index)
{
	CheckType(pos, LISPTYPE_TYPE);
	return RefArrayType_Low(pos, index);
}

void type_getarraytype(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	GetArrayType_Low(pos, index, ret);
}

void type_setarraytype(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPTYPE_TYPE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArrayType_Low(pos, index, value);
}

void type_lenarraytype(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	LenArrayType_Low(pos, ret);
}

void type_getvalues1(addr type, addr *ret)
{
	addr check;

	CheckType(type, LISPTYPE_TYPE);
	/* normal type */
	if (RefLispDecl(type) != LISPDECL_VALUES) {
		*ret = type;
		return;
	}
	/* var */
	GetArrayType(type, 0, &check);
	if (check != Nil) {
		GetCar(check, ret);
		return;
	}
	/* opt */
	GetArrayType(type, 1, &check);
	if (check != Nil) {
		GetCar(check, ret);
		return;
	}
	/* rest */
	GetArrayType(type, 2, ret);
}


/*
 *  object
 */
void type0_alloc(LocalRoot local, LispDecl type, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 0);
	*ret = pos;
}

void type1_alloc(LocalRoot local, LispDecl type, addr a, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 1);
	SetArrayType(pos, 0, a);
	*ret = pos;
}

void type2_alloc(LocalRoot local, LispDecl type, addr a, addr b, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 2);
	SetArrayType(pos, 0, a);
	SetArrayType(pos, 1, b);
	*ret = pos;
}

void type3_alloc(LocalRoot local, LispDecl type, addr a, addr b, addr c, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 3);
	SetArrayType(pos, 0, a);
	SetArrayType(pos, 1, b);
	SetArrayType(pos, 2, c);
	*ret = pos;
}

void type4_alloc(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 4);
	SetArrayType(pos, 0, a);
	SetArrayType(pos, 1, b);
	SetArrayType(pos, 2, c);
	SetArrayType(pos, 3, d);
	*ret = pos;
}

void type0_local(LocalRoot local, LispDecl type, addr *ret)
{
	CheckLocal(local);
	type0_alloc(local, type, ret);
}

void type1_local(LocalRoot local, LispDecl type, addr a, addr *ret)
{
	CheckLocal(local);
	type1_alloc(local, type, a, ret);
}

void type2_local(LocalRoot local, LispDecl type, addr a, addr b, addr *ret)
{
	CheckLocal(local);
	type2_alloc(local, type, a, b, ret);
}

void type3_local(LocalRoot local, LispDecl type, addr a, addr b, addr c, addr *ret)
{
	CheckLocal(local);
	type3_alloc(local, type, a, b, c, ret);
}

void type4_local(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret)
{
	CheckLocal(local);
	type4_alloc(local, type, a, b, c, d, ret);
}

void type0_heap(LispDecl type, addr *ret)
{
	type0_alloc(NULL, type, ret);
}

void type1_heap(LispDecl type, addr a, addr *ret)
{
	type1_alloc(NULL, type, a, ret);
}

void type2_heap(LispDecl type, addr a, addr b, addr *ret)
{
	type2_alloc(NULL, type, a, b, ret);
}

void type3_heap(LispDecl type, addr a, addr b, addr c, addr *ret)
{
	type3_alloc(NULL, type, a, b, c, ret);
}

void type4_heap(LispDecl type, addr a, addr b, addr c, addr d, addr *ret)
{
	type4_alloc(NULL, type, a, b, c, d, ret);
}

void type0not_alloc(LocalRoot local, LispDecl type, addr *ret)
{
	type0_alloc(local, type, ret);
	SetNotDecl(*ret, 1);
}

void type1not_alloc(LocalRoot local, LispDecl type, addr a, addr *ret)
{
	type1_alloc(local, type, a, ret);
	SetNotDecl(*ret, 1);
}

void type2not_alloc(LocalRoot local, LispDecl type, addr a, addr b, addr *ret)
{
	type2_alloc(local, type, a, b, ret);
	SetNotDecl(*ret, 1);
}

void type3not_alloc(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr *ret)
{
	type3_alloc(local, type, a, b, c, ret);
	SetNotDecl(*ret, 1);
}

void type4not_alloc(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret)
{
	type4_alloc(local, type, a, b, c, d, ret);
	SetNotDecl(*ret, 1);
}

void type0not_local(LocalRoot local, LispDecl type, addr *ret)
{
	CheckLocal(local);
	type0not_alloc(local, type, ret);
}

void type1not_local(LocalRoot local, LispDecl type, addr a, addr *ret)
{
	CheckLocal(local);
	type1not_alloc(local, type, a, ret);
}

void type2not_local(LocalRoot local, LispDecl type, addr a, addr b, addr *ret)
{
	CheckLocal(local);
	type2not_alloc(local, type, a, b, ret);
}

void type3not_local(LocalRoot local, LispDecl type, addr a, addr b, addr c, addr *ret)
{
	CheckLocal(local);
	type3not_alloc(local, type, a, b, c, ret);
}

void type4not_local(LocalRoot local, LispDecl type,
		addr a, addr b, addr c, addr d, addr *ret)
{
	CheckLocal(local);
	type4not_alloc(local, type, a, b, c, d, ret);
}

void type0not_heap(LispDecl type, addr *ret)
{
	type0not_alloc(NULL, type, ret);
}

void type1not_heap(LispDecl type, addr a, addr *ret)
{
	type1not_alloc(NULL, type, a, ret);
}

void type2not_heap(LispDecl type, addr a, addr b, addr *ret)
{
	type2not_alloc(NULL, type, a, b, ret);
}

void type3not_heap(LispDecl type, addr a, addr b, addr c, addr *ret)
{
	type3not_alloc(NULL, type, a, b, c, ret);
}

void type4not_heap(LispDecl type, addr a, addr b, addr c, addr d, addr *ret)
{
	type4not_alloc(NULL, type, a, b, c, d, ret);
}

static void type_aster_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	type0_local(local, LISPDECL_ASTERISK, ret);
}

void type1aster_localall(LocalRoot local, LispDecl type, addr *ret)
{
	addr a1;

	CheckLocal(local);
	type_aster_local(local, &a1);
	type1_local(local, type, a1, ret);
}

void type2aster_localall(LocalRoot local, LispDecl type, addr *ret)
{
	addr a1, a2;

	CheckLocal(local);
	type_aster_local(local, &a1);
	type_aster_local(local, &a2);
	type2_local(local, type, a1, a2, ret);
}

void type3aster_localall(LocalRoot local, LispDecl type, addr *ret)
{
	addr a1, a2, a3;

	CheckLocal(local);
	type_aster_local(local, &a1);
	type_aster_local(local, &a2);
	type_aster_local(local, &a3);
	type3_local(local, type, a1, a2, a3, ret);
}

void type4aster_localall(LocalRoot local, LispDecl type, addr *ret)
{
	addr a1, a2, a3, a4;

	CheckLocal(local);
	type_aster_local(local, &a1);
	type_aster_local(local, &a2);
	type_aster_local(local, &a3);
	type_aster_local(local, &a4);
	type4_local(local, type, a1, a2, a3, a4, ret);
}


/*
 *  copy
 */
static void type_copy_unsafe(LocalRoot local, addr *ret, addr left, byte value)
{
	addr right, one;
	size_t size, i;

	Check(GetType(left) != LISPTYPE_TYPE, "type error");
	LenArrayType(left, &size);
	type_alloc(local, &right, LISPDECL_EMPTY, (byte)size);
	SetUser(right, value);
	for (i = 0; i < size; i++) {
		GetArrayType(left, i, &one);
		SetArrayType(right, i, one);
	}
	*ret = right;
}

void type_copy_unsafe_alloc(LocalRoot local, addr *ret, addr left)
{
	type_copy_unsafe(local, ret, left, GetUser(left));
}

void type_copy_unsafe_local(LocalRoot local, addr *ret, addr left)
{
	CheckLocal(local);
	type_copy_unsafe_alloc(local, ret, left);
}

void type_copy_unsafe_heap(addr *ret, addr left)
{
	type_copy_unsafe_alloc(NULL, ret, left);
}

void type_copydecl_unsafe_alloc(LocalRoot local, addr *ret, addr left)
{
	type_copy_unsafe(local, ret, left, RefLispDecl(left));
}

void type_copydecl_unsafe_local(LocalRoot local, addr *ret, addr left)
{
	CheckLocal(local);
	type_copydecl_unsafe_alloc(local, ret, left);
}

void type_copydecl_unsafe_heap(addr *ret, addr left)
{
	type_copydecl_unsafe_alloc(NULL, ret, left);
}


/*
 *  make
 */
void type_eql_alloc(LocalRoot local, addr pos, addr *ret)
{
	type1_alloc(local, LISPDECL_EQL, pos, ret);
}

void type_eql_local(LocalRoot local, addr pos, addr *ret)
{
	type1_local(local, LISPDECL_EQL, pos, ret);
}

void type_eql_heap(addr pos, addr *ret)
{
	type1_heap(LISPDECL_EQL, pos, ret);
}

static void vector4_va_heap(addr *ret, va_list args)
{
	va_list temp;
	addr pos, array;
	size_t size, i;

	/* length args */
	va_copy(temp, args);
	for (size = 0; ; size++) {
		pos = va_arg(temp, addr);
		if (pos == NULL) break;
	}

	/* make vector4 */
	vector4_heap(&array, size);
	for (i = 0; i < size; i++) {
		pos = va_arg(args, addr);
		if (pos == NULL) break;
		SetArrayA4(array, i, pos);
	}
	*ret = array;
}

void type_member_heap(addr *ret, ...)
{
	addr array;
	va_list args;

	va_start(args, ret);
	vector4_va_heap(&array, args);
	va_end(args);
	type1_heap(LISPDECL_MEMBER, array, ret);
}

void type_satisfies_heap(addr call, addr *ret)
{
	Check((! symbolp(call)) && (! functionp(call)), "type error");
	type1_heap(LISPDECL_SATISFIES, call, ret);
}

void type_values_local(LocalRoot local, addr v1, addr v2, addr v3, addr v4, addr *ret)
{
	type4_local(local, LISPDECL_VALUES, v1, v2, v3, v4, ret);
}

void type_values_heap(addr v1, addr v2, addr v3, addr v4, addr *ret)
{
	type4_heap(LISPDECL_VALUES, v1, v2, v3, v4, ret);
}

void type_signed_alloc(LocalRoot local, fixnum value, addr *ret)
{
	addr pos;

	Check(value <= 0, "size error");
	fixnum_alloc(local, &pos, value);
	type1_alloc(local, LISPDECL_SIGNED_BYTE, pos, ret);
}

void type_signed_local(LocalRoot local, fixnum value, addr *ret)
{
	CheckLocal(local);
	type_signed_alloc(local, value, ret);
}

void type_signed_heap(fixnum value, addr *ret)
{
	type_signed_alloc(NULL, value, ret);
}

void type_unsigned_alloc(LocalRoot local, fixnum value, addr *ret)
{
	addr pos;

	Check(value <= 0, "size error");
	fixnum_alloc(local, &pos, value);
	type1_alloc(local, LISPDECL_UNSIGNED_BYTE, pos, ret);
}

void type_unsigned_local(LocalRoot local, fixnum value, addr *ret)
{
	CheckLocal(local);
	type_unsigned_alloc(local, value, ret);
}

void type_unsigned_heap(fixnum value, addr *ret)
{
	type_unsigned_alloc(NULL, value, ret);
}

/*
 *  function / compiled-function
 *    function         -> (function * * [*])
 *    (function)       -> (function * * [nil])
 *    (function * *)   -> (function * * [nil])
 *    (function * * *) -> ERROR
 *
 *  (typep value 'function) -> ok
 *  (typep value '(function)) -> ERROR
 *  (declare (type function a b)) -> ok
 *  (declare (type (function) a b)) -> ok
 */
void type_function_heap(addr args, addr values, addr *ret)
{
	type3_heap(LISPDECL_FUNCTION, args, values, Nil, ret);
}

void type_compiled_heap(addr args, addr values, addr *ret)
{
	type3_heap(LISPDECL_COMPILED_FUNCTION, args, values, Nil, ret);
}

void type_clos_heap(addr clos, addr *ret)
{
	CheckType(clos, LISPTYPE_CLOS);
	type1_heap(LISPDECL_CLOS, clos, ret);
}

void type_delay_heap(addr pos, addr *ret)
{
	type2_heap(LISPDECL_DELAY, pos, Nil, ret);
}

