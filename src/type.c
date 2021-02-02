#include <stdarg.h>
#include "build.h"
#include "function.h"
#include "local.h"
#include "heap.h"
#include "object.h"
#include "symbol.h"
#include "type.h"
#include "type_coerce.h"
#include "type_copy.h"
#include "type_name.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_symbol.h"
#include "type_table.h"
#include "type_typep.h"
#include "type_value.h"
#include "type_upgraded.h"

/*
 *  allocate
 */
addr type_allocr(LocalRoot local, enum LISPDECL type, size_t size)
{
	addr pos;

	Check(LISPDECL_SIZE <= type, "type too large.");
	alloc_array2(local, &pos, LISPTYPE_TYPE, size);
	SetUser(pos, (byte)type);

	return pos;
}

addr type_localr(LocalRoot local, enum LISPDECL type, size_t size)
{
	CheckLocal(local);
	return type_allocr(local, type, size);
}

addr type_heapr(enum LISPDECL type, size_t size)
{
	return type_allocr(NULL, type, size);
}

void type_alloc(LocalRoot local, addr *ret, enum LISPDECL type, size_t size)
{
	*ret = type_allocr(local, type, size);
}

void type_local(LocalRoot local, addr *ret, enum LISPDECL type, size_t size)
{
	CheckLocal(local);
	*ret = type_allocr(local, type, size);
}

void type_heap(addr *ret, enum LISPDECL type, size_t size)
{
	*ret = type_allocr(NULL, type, size);
}


/*
 *  accessor
 */
enum LISPDECL type_lispdecl(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return LispDecl_Low(pos);
}

enum LISPDECL type_reflispdecl(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return RefLispDecl_Low(pos);
}

void type_getlispdecl(addr pos, enum LISPDECL *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	GetLispDecl_Low(pos, ret);
}

void type_setlispdecl(addr pos, enum LISPDECL value)
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
 *  init
 */
void init_type(void)
{
	init_type_coerce();
	init_type_copy();
	init_type_name();
	init_type_object();
	init_type_parse();
	init_type_symbol();
	init_type_typep();
	init_type_value();
}

void build_type(void)
{
	build_type_table();
	build_type_constant();
	build_type_upgraded();
	build_type_symbol();
	build_type_parse();
}


/*
 *  check
 */
static int decl_function_p(enum LISPDECL type)
{
	return type == LISPDECL_FUNCTION
		|| type == LISPDECL_COMPILED_FUNCTION;
}
static int decl_astert_p(enum LISPDECL type)
{
	return type == LISPDECL_ASTERISK
		|| type == LISPDECL_T;
}

int decl_character_p(enum LISPDECL type)
{
	return type == LISPDECL_CHARACTER
		|| type == LISPDECL_BASE_CHAR
		|| type == LISPDECL_STANDARD_CHAR
		|| type == LISPDECL_EXTENDED_CHAR;
}

int decl_float_p(enum LISPDECL type)
{
	return type == LISPDECL_FLOAT
		|| type == LISPDECL_SINGLE_FLOAT
		|| type == LISPDECL_DOUBLE_FLOAT
		|| type == LISPDECL_LONG_FLOAT
		|| type == LISPDECL_SHORT_FLOAT;
}

int decl_range_p(enum LISPDECL type)
{
	return type == LISPDECL_INTEGER
		|| type == LISPDECL_RATIONAL
		|| type == LISPDECL_REAL
		|| decl_float_p(type);
}

int decl_subtypep_real(enum LISPDECL left, enum LISPDECL right)
{
	switch (right) {
		case LISPDECL_INTEGER:
			return left == LISPDECL_INTEGER;

		case LISPDECL_RATIONAL:
			return left == LISPDECL_INTEGER
				|| left == LISPDECL_RATIONAL;

		case LISPDECL_REAL:
			return left == LISPDECL_INTEGER
				|| left == LISPDECL_RATIONAL
				|| left == LISPDECL_REAL
				|| decl_float_p(left);

		case LISPDECL_FLOAT:
			return decl_float_p(left);

		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		case LISPDECL_SHORT_FLOAT:
			return left == right;

		default:
			break;
	}

	return 0;
}

int type_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && decl_function_p(LispDecl(pos));
}

int type_astert_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && decl_astert_p(LispDecl(pos));
}

int type_function_aster_p(addr pos)
{
	enum LISPDECL type;
	addr args, values;

	if (GetType(pos) != LISPTYPE_TYPE)
		return 0;
	type = LispDecl(pos);
	if (type != LISPDECL_FUNCTION && type != LISPDECL_COMPILED_FUNCTION)
		return 0;
	GetArrayType(pos, 0, &args);
	GetArrayType(pos, 1, &values);
	return type_asterisk_p(args) && type_asterisk_p(values);
}

int type_asterisk_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && LispDecl(pos) == LISPDECL_ASTERISK;
}

int type_range_p(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	return decl_range_p(LispDecl(pos));
}

int type_string_p(addr pos)
{
	CheckType(pos, LISPTYPE_TYPE);
	switch (LispDecl(pos)) {
		case LISPDECL_STRING:
		case LISPDECL_BASE_STRING:
		case LISPDECL_SIMPLE_STRING:
		case LISPDECL_SIMPLE_BASE_STRING:
			return 1;

		default:
			return 0;
	}
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
 *  object
 */
void type0_alloc(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 0);
	*ret = pos;
}

void type1_alloc(LocalRoot local, enum LISPDECL type, addr a, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 1);
	SetArrayType(pos, 0, a);
	*ret = pos;
}

void type2_alloc(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 2);
	SetArrayType(pos, 0, a);
	SetArrayType(pos, 1, b);
	*ret = pos;
}

void type3_alloc(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 3);
	SetArrayType(pos, 0, a);
	SetArrayType(pos, 1, b);
	SetArrayType(pos, 2, c);
	*ret = pos;
}

void type4_alloc(LocalRoot local, enum LISPDECL type,
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

void type0_local(LocalRoot local, enum LISPDECL type, addr *ret)
{
	CheckLocal(local);
	type0_alloc(local, type, ret);
}

void type1_local(LocalRoot local, enum LISPDECL type, addr a, addr *ret)
{
	CheckLocal(local);
	type1_alloc(local, type, a, ret);
}

void type2_local(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret)
{
	CheckLocal(local);
	type2_alloc(local, type, a, b, ret);
}

void type3_local(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr *ret)
{
	CheckLocal(local);
	type3_alloc(local, type, a, b, c, ret);
}

void type4_local(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr d, addr *ret)
{
	CheckLocal(local);
	type4_alloc(local, type, a, b, c, d, ret);
}

void type0_heap(enum LISPDECL type, addr *ret)
{
	type0_alloc(NULL, type, ret);
}

void type1_heap(enum LISPDECL type, addr a, addr *ret)
{
	type1_alloc(NULL, type, a, ret);
}

void type2_heap(enum LISPDECL type, addr a, addr b, addr *ret)
{
	type2_alloc(NULL, type, a, b, ret);
}

void type3_heap(enum LISPDECL type, addr a, addr b, addr c, addr *ret)
{
	type3_alloc(NULL, type, a, b, c, ret);
}

void type4_heap(enum LISPDECL type, addr a, addr b, addr c, addr d, addr *ret)
{
	type4_alloc(NULL, type, a, b, c, d, ret);
}

void type0not_alloc(LocalRoot local, enum LISPDECL type, addr *ret)
{
	type0_alloc(local, type, ret);
	SetNotDecl(*ret, 1);
}

void type1not_alloc(LocalRoot local, enum LISPDECL type, addr a, addr *ret)
{
	type1_alloc(local, type, a, ret);
	SetNotDecl(*ret, 1);
}

void type2not_alloc(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret)
{
	type2_alloc(local, type, a, b, ret);
	SetNotDecl(*ret, 1);
}

void type3not_alloc(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr *ret)
{
	type3_alloc(local, type, a, b, c, ret);
	SetNotDecl(*ret, 1);
}

void type4not_alloc(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr d, addr *ret)
{
	type4_alloc(local, type, a, b, c, d, ret);
	SetNotDecl(*ret, 1);
}

void type0not_local(LocalRoot local, enum LISPDECL type, addr *ret)
{
	CheckLocal(local);
	type0not_alloc(local, type, ret);
}

void type1not_local(LocalRoot local, enum LISPDECL type, addr a, addr *ret)
{
	CheckLocal(local);
	type1not_alloc(local, type, a, ret);
}

void type2not_local(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret)
{
	CheckLocal(local);
	type2not_alloc(local, type, a, b, ret);
}

void type3not_local(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr *ret)
{
	CheckLocal(local);
	type3not_alloc(local, type, a, b, c, ret);
}

void type4not_local(LocalRoot local, enum LISPDECL type,
		addr a, addr b, addr c, addr d, addr *ret)
{
	CheckLocal(local);
	type4not_alloc(local, type, a, b, c, d, ret);
}

void type0not_heap(enum LISPDECL type, addr *ret)
{
	type0not_alloc(NULL, type, ret);
}

void type1not_heap(enum LISPDECL type, addr a, addr *ret)
{
	type1not_alloc(NULL, type, a, ret);
}

void type2not_heap(enum LISPDECL type, addr a, addr b, addr *ret)
{
	type2not_alloc(NULL, type, a, b, ret);
}

void type3not_heap(enum LISPDECL type, addr a, addr b, addr c, addr *ret)
{
	type3not_alloc(NULL, type, a, b, c, ret);
}

void type4not_heap(enum LISPDECL type, addr a, addr b, addr c, addr d, addr *ret)
{
	type4not_alloc(NULL, type, a, b, c, d, ret);
}

static void type_aster_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	type0_local(local, LISPDECL_ASTERISK, ret);
}

void type1aster_localall(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr a1;

	CheckLocal(local);
	type_aster_local(local, &a1);
	type1_local(local, type, a1, ret);
}

void type2aster_localall(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr a1, a2;

	CheckLocal(local);
	type_aster_local(local, &a1);
	type_aster_local(local, &a2);
	type2_local(local, type, a1, a2, ret);
}

void type3aster_localall(LocalRoot local, enum LISPDECL type, addr *ret)
{
	addr a1, a2, a3;

	CheckLocal(local);
	type_aster_local(local, &a1);
	type_aster_local(local, &a2);
	type_aster_local(local, &a3);
	type3_local(local, type, a1, a2, a3, ret);
}

void type4aster_localall(LocalRoot local, enum LISPDECL type, addr *ret)
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
 *  etc
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

