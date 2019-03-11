#include <stdarg.h>
#include "bigdata.h"
#include "bignum.h"
#include "clos_object.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "hashtable.h"
#include "integer.h"
#include "object.h"
#include "package.h"
#include "rational.h"
#include "sequence.h"
#include "type.h"
#include "type_parse.h"
#include "type_optimize.h"

/*****************************************************************************
 *  parse-type
 *****************************************************************************/
static void parse_symbol(LocalRoot local, addr *ret, addr type);

/* type_alloc */
addr type_allocr(LocalRoot local, enum LISPDECL type, byte size)
{
	addr pos;

	Check(LISPDECL_SIZE <= type, "type too large.");
	alloc_array2(local, &pos, LISPTYPE_TYPE, size);
	SetUser(pos, (byte)type);

	return pos;
}
addr type_localr(LocalRoot local, enum LISPDECL type, byte size)
{
	Check(local == NULL, "local error");
	return type_allocr(local, type, size);
}
addr type_heapr(enum LISPDECL type, byte size)
{
	return type_allocr(NULL, type, size);
}
void type_alloc(LocalRoot local, addr *ret, enum LISPDECL type, byte size)
{
	*ret = type_allocr(local, type, size);
}
void type_local(LocalRoot local, addr *ret, enum LISPDECL type, byte size)
{
	Check(local == NULL, "local error");
	*ret = type_allocr(local, type, size);
}
void type_heap(addr *ret, enum LISPDECL type, byte size)
{
	*ret = type_allocr(NULL, type, size);
}

/* copy-no-recursive */
static void copy_no_recursive(LocalRoot local, addr *ret, addr left, byte value)
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
void copy_no_recursive_type_alloc(LocalRoot local, addr *ret, addr left)
{
	copy_no_recursive(local, ret, left, GetUser(left));
}
void copy_no_recursive_type_local(LocalRoot local, addr *ret, addr left)
{
	Check(local == NULL, "local error");
	copy_no_recursive_type_alloc(local, ret, left);
}
void copy_no_recursive_type_heap(addr *ret, addr left)
{
	copy_no_recursive_type_alloc(NULL, ret, left);
}
void copy_no_recursive_typeonly_alloc(LocalRoot local, addr *ret, addr left)
{
	copy_no_recursive(local, ret, left, RefLispDecl(left));
}
void copy_no_recursive_typeonly_local(LocalRoot local, addr *ret, addr left)
{
	Check(local == NULL, "local error");
	copy_no_recursive_typeonly_alloc(local, ret, left);
}
void copy_no_recursive_typeonly_heap(addr *ret, addr left)
{
	copy_no_recursive_typeonly_alloc(NULL, ret, left);
}

/* type_object */
void type_object1(LocalRoot local, enum LISPDECL type, addr a, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 1);
	SetArrayType(pos, 0, a);
	*ret = pos;
}
void type_object2(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 2);
	SetArrayType(pos, 0, a);
	SetArrayType(pos, 1, b);
	*ret = pos;
}
void type_object3(LocalRoot local,
		enum LISPDECL type, addr a, addr b, addr c, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 3);
	SetArrayType(pos, 0, a);
	SetArrayType(pos, 1, b);
	SetArrayType(pos, 2, c);
	*ret = pos;
}
void type_object4(LocalRoot local,
		enum LISPDECL type, addr a, addr b, addr c, addr d, addr *ret)
{
	addr pos;
	type_alloc(local, &pos, type, 4);
	SetArrayType(pos, 0, a);
	SetArrayType(pos, 1, b);
	SetArrayType(pos, 2, c);
	SetArrayType(pos, 3, d);
	*ret = pos;
}
void type_object1_not(LocalRoot local, enum LISPDECL type, addr a, addr *ret)
{
	type_object1(local, type, a, ret);
	SetNotDecl(*ret, 1);
}
void type_object2_not(LocalRoot local, enum LISPDECL type, addr a, addr b, addr *ret)
{
	type_object2(local, type, a, b, ret);
	SetNotDecl(*ret, 1);
}
void type_object3_not(LocalRoot local,
		enum LISPDECL type, addr a, addr b, addr c, addr *ret)
{
	type_object3(local, type, a, b, c, ret);
	SetNotDecl(*ret, 1);
}
void type_object4_not(LocalRoot local,
		enum LISPDECL type, addr a, addr b, addr c, addr d, addr *ret)
{
	type_object4(local, type, a, b, c, d, ret);
	SetNotDecl(*ret, 1);
}

/* type_empty */
static addr type_emptyr(LocalRoot local, enum LISPDECL type)
{
	addr pos;
	type_alloc(local, &pos, type, 0);
	return pos;
}
void type_empty(LocalRoot local, enum LISPDECL type, addr *ret)
{
	type_alloc(local, ret, type, 0);
}
void type_empty_not(LocalRoot local, enum LISPDECL type, addr *ret)
{
	type_alloc(local, ret, type, 0);
	SetNotDecl(*ret, 1);
}

/* type_asterisk */
addr type_asterisk_allocr(LocalRoot local)
{
	return type_emptyr(local, LISPDECL_ASTERISK);
}
addr type_asterisk_localr(LocalRoot local)
{
	Check(local == NULL, "local error");
	return type_asterisk_allocr(local);
}
addr type_asterisk_heapr(void)
{
	return type_asterisk_allocr(NULL);
}
void type_asterisk_alloc(LocalRoot local, addr *ret)
{
	type_empty(local, LISPDECL_ASTERISK, ret);
}
void type_asterisk_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	type_asterisk_alloc(local, ret);
}
void type_asterisk_heap(addr *ret)
{
	type_asterisk_alloc(NULL, ret);
}
int function_decl_p(enum LISPDECL decl)
{
	return decl == LISPDECL_FUNCTION || decl == LISPDECL_COMPILED_FUNCTION;
}
int function_type_p(addr pos)
{
	return (GetType(pos) == LISPTYPE_TYPE)
		&& function_decl_p(RefLispDecl(pos));
}
int asterisk_or_t_decl_p(enum LISPDECL decl)
{
	return decl == LISPDECL_ASTERISK || decl == LISPDECL_T;
}
int asterisk_or_t_p(addr pos)
{
	return (GetType(pos) == LISPTYPE_TYPE)
		&& asterisk_or_t_decl_p(RefLispDecl(pos));
}
int function_asterisk_p(addr pos)
{
	addr args, values;

	if (GetType(pos) == LISPTYPE_TYPE && RefLispDecl(pos) == LISPDECL_FUNCTION) {
		GetArrayType(pos, 0, &args);
		GetArrayType(pos, 1, &values);
		return asterisk_p(args) && asterisk_p(values);
	}
	return 0;
}
int asterisk_p(addr pos)
{
	return GetType(pos) == LISPTYPE_TYPE && RefLispDecl(pos) == LISPDECL_ASTERISK;
}

/* type_nil */
addr type_nil_allocr(LocalRoot local)
{
	return type_emptyr(local, LISPDECL_NIL);
}
addr type_nil_localr(LocalRoot local)
{
	Check(local == NULL, "local error");
	return type_nil_allocr(local);
}
addr type_nil_heapr(void)
{
	return type_nil_allocr(NULL);
}
void type_nil_alloc(LocalRoot local, addr *ret)
{
	type_empty(local, LISPDECL_NIL, ret);
}
void type_nil_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	type_nil_alloc(local, ret);
}
void type_nil_heap(addr *ret)
{
	type_nil_alloc(NULL, ret);
}

/* type_t */
addr type_t_allocr(LocalRoot local)
{
	return type_emptyr(local, LISPDECL_T);
}
addr type_t_localr(LocalRoot local)
{
	Check(local == NULL, "local error");
	return type_t_allocr(local);
}
addr type_t_heapr(void)
{
	return type_t_allocr(NULL);
}
void type_t_alloc(LocalRoot local, addr *ret)
{
	type_empty(local, LISPDECL_T, ret);
}
void type_t_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	type_t_alloc(local, ret);
}
void type_t_heap(addr *ret)
{
	type_t_alloc(NULL, ret);
}

/* type_bool */
void type_bool_alloc(LocalRoot local, int value, addr *ret)
{
	if (value)
		type_t_alloc(local, ret);
	else
		type_nil_alloc(local, ret);
}
void type_bool_local(LocalRoot local, int value, addr *ret)
{
	Check(local == NULL, "local error");
	type_bool_alloc(local, value, ret);
}
void type_bool_heap(int value, addr *ret)
{
	type_bool_alloc(NULL, value, ret);
}

void type_realvalue(LocalRoot local, enum LISPDECL type, addr value, addr *ret)
{
	type_object4(local, type, Nil, value, Nil, value, ret);
}

void type_aster1(LocalRoot local, enum LISPDECL type, addr *ret)
{
	type_object1(local, type, type_asterisk_allocr(local) , ret);
}

void type_aster2(LocalRoot local, enum LISPDECL type, addr *ret)
{
	type_object2(local, type,
			type_asterisk_allocr(local),
			type_asterisk_allocr(local),
			ret);
}

void type_aster3(LocalRoot local, enum LISPDECL type, addr *ret)
{
	type_object3(local, type,
			type_asterisk_allocr(local),
			type_asterisk_allocr(local),
			type_asterisk_allocr(local),
			ret);
}

void type_aster4(LocalRoot local, enum LISPDECL type, addr *ret)
{
	type_object4(local, type,
			type_asterisk_allocr(local),
			type_asterisk_allocr(local),
			type_asterisk_allocr(local),
			type_asterisk_allocr(local),
			ret);
}

void type_intrange(addr left1, fixnum left2, addr right1, fixnum right2, addr *ret)
{
	addr leftv, rightv;

	Check(left1 != Nil && left1 != T, "left1 error");
	Check(right1 != Nil && right1 != T, "right1 error");
	fixnum_heap(&leftv, left2);
	fixnum_heap(&rightv, right2);
	type_object4(NULL, LISPDECL_INTEGER, left1, leftv, right1, rightv, ret);
}

void type_intrange_left(addr left1, fixnum left2, addr *ret)
{
	addr leftv;

	Check(left1 != Nil && left1 != T, "left1 error");
	fixnum_heap(&leftv, left2);
	type_object4(NULL, LISPDECL_INTEGER, left1, leftv,
			type_asterisk_heapr(),
			type_asterisk_heapr(),
			ret);
}

void type_intrange_right(addr right1, fixnum right2, addr *ret)
{
	addr rightv;

	Check(right1 != Nil && right1 != T, "left1 error");
	fixnum_heap(&rightv, right2);
	type_object4(NULL, LISPDECL_INTEGER,
			type_asterisk_heapr(),
			type_asterisk_heapr(),
			right1, rightv, ret);
}

void type_fixnum_alloc(LocalRoot local, addr *ret)
{
	addr min, max;

	GetConst(FIXNUM_MIN, &min);
	GetConst(FIXNUM_MAX, &max);
	type_object4(NULL, LISPDECL_INTEGER,
			type_asterisk_heapr(), min,
			type_asterisk_heapr(), max, ret);
}

void type_realrange_float(addr left1,
		single_float left2, addr right1, single_float right2, addr *ret)
{
	addr leftv, rightv;

	Check(left1 != Nil && left1 != T, "left1 error");
	Check(right1 != Nil && right1 != T, "right1 error");
	single_float_heap(&leftv, left2);
	single_float_heap(&rightv, right2);
	type_object4(NULL, LISPDECL_REAL, left1, leftv, right1, rightv, ret);
}

void type_floatrange_left(addr left1, single_float left2, addr *ret)
{
	addr leftv;

	Check(left1 != Nil && left1 != T, "left1 error");
	single_float_heap(&leftv, left2);
	type_object4(NULL, LISPDECL_FLOAT, left1, leftv,
			type_asterisk_heapr(),
			type_asterisk_heapr(),
			ret);
}

void type_syscall(addr args, addr values, addr *ret)
{
	if (args == NULL) type_asterisk_heap(&args);
	if (values == NULL) type_asterisk_heap(&values);
	type_object3(NULL, LISPDECL_COMPILED_FUNCTION, args, values, Nil, ret);
}

void type_compiled_function_asterisk(LocalRoot local, addr *ret)
{
	type_aster3(local, LISPDECL_COMPILED_FUNCTION, ret);
}

void type_function_asterisk(LocalRoot local, addr *ret)
{
	type_aster3(local, LISPDECL_FUNCTION, ret);
}

void type_and(LocalRoot local, addr left, addr right, addr *ret)
{
	enum LISPDECL decl;
	addr array;

	CheckType2(left, LISPTYPE_TYPE, "type left error");
	CheckType2(right, LISPTYPE_TYPE, "type right error");
	GetLispDecl(left, &decl);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		*ret = right;
		return;
	}
	if (decl == LISPDECL_NIL) {
		type_nil_alloc(local, ret);
		return;
	}
	GetLispDecl(right, &decl);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		*ret = left;
		return;
	}
	if (decl == LISPDECL_NIL) {
		type_nil_alloc(local, ret);
		return;
	}

	vector4_alloc(local, &array, 2);
	SetArrayA4(array, 0, left);
	SetArrayA4(array, 1, right);
	type_object1(local, LISPDECL_AND, array, ret);
}

void type_or(LocalRoot local, addr left, addr right, addr *ret)
{
	enum LISPDECL decl;
	addr array;

	CheckType2(left, LISPTYPE_TYPE, "type left error");
	CheckType2(right, LISPTYPE_TYPE, "type right error");
	GetLispDecl(left, &decl);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		type_t_alloc(local, ret);
		return;
	}
	if (decl == LISPDECL_NIL) {
		*ret = right;
		return;
	}
	GetLispDecl(right, &decl);
	if (decl == LISPDECL_ASTERISK || decl == LISPDECL_T) {
		type_t_alloc(local, ret);
		return;
	}
	if (decl == LISPDECL_NIL) {
		*ret = left;
		return;
	}

	vector4_alloc(local, &array, 2);
	SetArrayA4(array, 0, left);
	SetArrayA4(array, 1, right);
	type_object1(local, LISPDECL_OR, array, ret);
}

void type_or3(LocalRoot local, addr a, addr b, addr c, addr *ret)
{
	addr array;

	vector4_alloc(local, &array, 3);
	SetArrayA4(array, 0, a);
	SetArrayA4(array, 1, b);
	SetArrayA4(array, 2, c);
	type_object1(local, LISPDECL_OR, array, ret);
}

void type_or4(LocalRoot local, addr a, addr b, addr c, addr d, addr *ret)
{
	addr array;

	vector4_alloc(local, &array, 4);
	SetArrayA4(array, 0, a);
	SetArrayA4(array, 1, b);
	SetArrayA4(array, 2, c);
	SetArrayA4(array, 3, d);
	type_object1(local, LISPDECL_OR, array, ret);
}

void type_function_heap(addr arg, addr values, addr *ret)
{
	type_object3(NULL, LISPDECL_FUNCTION, arg, values, Nil, ret);
}

void type_compiled_heap(addr arg, addr values, addr *ret)
{
	type_object3(NULL, LISPDECL_COMPILED_FUNCTION, arg, values, Nil, ret);
}

static void vector4_alloc_stdarg(LocalRoot local, addr *ret, va_list args)
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
	vector4_alloc(local, &array, size);
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
	vector4_alloc_stdarg(NULL, &array, args);
	va_end(args);
	type_object1(NULL, LISPDECL_MEMBER, array, ret);
}

void setnotdecl_value(addr pos, int value)
{
	byte user;

	Check(GetType(pos) != LISPTYPE_TYPE, "type error");
	user = GetUser(pos);
	if (value)
		SetUser(pos, 0x80 | user);
	else
		SetUser(pos, 0x7F & user);
}

void setnotdecl_object(addr left, addr right)
{
	SetNotDecl(left, RefNotDecl(right));
}

void reversenotdecl(addr pos)
{
	byte user;

	Check(GetType(pos) != LISPTYPE_TYPE, "type error");
	user = GetUser(pos);
	if ((user & 0x80) == 0)
		SetUser(pos, 0x80 | user);
	else
		SetUser(pos, 0x7F & user);
}

int float_value_p(enum LISPDECL type)
{
	return type == LISPDECL_FLOAT ||
		type == LISPDECL_SINGLE_FLOAT ||
		type == LISPDECL_DOUBLE_FLOAT ||
		type == LISPDECL_LONG_FLOAT ||
		type == LISPDECL_SHORT_FLOAT;
}

int range_value_p(enum LISPDECL type)
{
	return type == LISPDECL_INTEGER ||
		type == LISPDECL_RATIONAL ||
		type == LISPDECL_REAL ||
		float_value_p(type);
}

int range_type_p(addr pos)
{
	Check(GetType(pos) != LISPTYPE_TYPE, "type error");
	return range_value_p(RefLispDecl(pos));
}

int subtype_real_p(enum LISPDECL left, enum LISPDECL right)
{
	switch (right) {
		case LISPDECL_INTEGER:
			return left == LISPDECL_INTEGER;

		case LISPDECL_RATIONAL:
			return left == LISPDECL_INTEGER || left == LISPDECL_RATIONAL;

		case LISPDECL_REAL:
			return left == LISPDECL_INTEGER ||
				left == LISPDECL_RATIONAL ||
				left == LISPDECL_REAL ||
				float_value_p(left);

		case LISPDECL_FLOAT:
			return float_value_p(left);

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

/*
 *  upgraded-array-element-type
 */
static int upgraded_array_unsigned(bigtype value)
{
	if (value <= UINT8_MAX) return 8;
	if (value <= UINT16_MAX) return 16;
#ifdef LISP_64BIT
	if (value <= UINT32_MAX) return 32;
	return 64;
#else
	return 32;
#endif
}

static int upgraded_array_signed(int sign, bigtype value)
{
	if (IsPlus(sign)) {
		if (value <= INT8_MAX) return 8;
		if (value <= INT16_MAX) return 16;
		if (value <= INT32_MAX) return 32;
#ifdef LISP_64BIT
		if (value <= INT64_MAX) return 64;
#endif
	}
	else {
		if (value <= (bigtype)(INT8_MAX) + 1UL) return 8;
		if (value <= (bigtype)(INT16_MAX) + 1UL) return 16;
		if (value <= (bigtype)(INT32_MAX) + 1UL) return 32;
#ifdef LISP_64BIT
		if (value <= (bigtype)(INT64_MAX) + 1ULL) return 64;
#endif
	}
	return 0;
}

static enum ARRAY_TYPE upgraded_array_integer(addr type, int *size)
{
	int sign1, sign2, size1, size2;
	addr left1, left2, right1, right2;
	bigtype value1, value2;

	/* asterisk check */
	GetArrayType(type, 0, &left1);
	if (asterisk_p(left1)) return ARRAY_TYPE_T;
	GetArrayType(type, 2, &right1);
	if (asterisk_p(right1)) return ARRAY_TYPE_T;

	/* left */
	GetArrayType(type, 1, &left2);
	if (castfixed_integer(left2, &sign1, &value1))
		return ARRAY_TYPE_T;
	if (left1 == T)
		value1++;

	/* right */
	GetArrayType(type, 3, &right2);
	if (castfixed_integer(right2, &sign2, &value2))
		return ARRAY_TYPE_T;
	if (right1 == T)
		value2--;

	/* value */
	if (IsPlus(sign1)) {
		if (value1 == 0 && value2 == 1) return ARRAY_TYPE_BIT;
		size1 = upgraded_array_unsigned(value1);
		size2 = upgraded_array_unsigned(value2);
		*size = (size1 < size2)? size2: size1;
		return ARRAY_TYPE_UNSIGNED;
	}
	else {
		size1 = upgraded_array_signed(sign1, value1);
		size2 = upgraded_array_signed(sign2, value2);
		if (size1 == 0 || size2 == 0) return ARRAY_TYPE_T;
		*size = (size1 < size2)? size2: size1;
		return ARRAY_TYPE_SIGNED;
	}
}

static enum ARRAY_TYPE upgraded_array_inplace(addr type, int *size)
{
	/* not */
	if (RefNotDecl(type)) {
		return ARRAY_TYPE_T;
	}
	/* upgraded */
	switch (RefLispDecl(type)) {
		case LISPDECL_CHARACTER:
		case LISPDECL_BASE_CHAR:
		case LISPDECL_STANDARD_CHAR:
			return ARRAY_TYPE_CHARACTER;

		case LISPDECL_INTEGER:
			return upgraded_array_integer(type, size);

		case LISPDECL_SINGLE_FLOAT:
			return ARRAY_TYPE_SINGLE_FLOAT;

		case LISPDECL_DOUBLE_FLOAT:
			return ARRAY_TYPE_DOUBLE_FLOAT;

		case LISPDECL_LONG_FLOAT:
			return ARRAY_TYPE_LONG_FLOAT;

		default:
			break;
	}

	return ARRAY_TYPE_T;
}

enum ARRAY_TYPE upgraded_array_direct(addr type, int *size)
{
	enum ARRAY_TYPE ret;
	LocalRoot local;
	LocalStack stack;

	/* local */
	local = Local_Thread;
	push_local(local, &stack);
	/* upgraded-array */
	type_throw_local(local, &type, type);
	type_optimize_local(local, &type, type);
	Check(! type_optimized_p(type), "optimize error");
	get_type_optimized(&type, type);
	ret = upgraded_array_inplace(type, size);
	/* free */
	rollback_local(local, stack);

	return ret;
}

void type_signed_byte(LocalRoot local, addr *ret, int value)
{
	Check(value <= 0, "size error");
	type_object1(local, LISPDECL_SIGNED_BYTE, fixnum_allocr(local, value), ret);
}

void type_unsigned_byte(LocalRoot local, addr *ret, int value)
{
	Check(value <= 0, "size error");
	type_object1(local, LISPDECL_UNSIGNED_BYTE, fixnum_allocr(local, value), ret);
}

static void make_upgraded_array_type_alloc(LocalRoot local,
		addr *ret, enum ARRAY_TYPE type, int size)
{
	switch (type) {
		case ARRAY_TYPE_BIT:
			type_empty(local, LISPDECL_BIT, ret);
			break;

		case ARRAY_TYPE_CHARACTER:
			type_empty(local, LISPDECL_CHARACTER, ret);
			break;

		case ARRAY_TYPE_SIGNED:
			type_signed_byte(local, ret, size);
			break;

		case ARRAY_TYPE_UNSIGNED:
			type_unsigned_byte(local, ret, size);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			type_aster4(local, LISPDECL_SINGLE_FLOAT, ret);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			type_aster4(local, LISPDECL_DOUBLE_FLOAT, ret);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			type_aster4(local, LISPDECL_LONG_FLOAT, ret);
			break;

		default:
			type_t_alloc(local, ret);
			break;
	}
}

void upgraded_array_type_alloc(LocalRoot local, addr *ret, addr type)
{
	enum ARRAY_TYPE value;
	int size;

	size = 0;
	value = upgraded_array_direct(type, &size);
	make_upgraded_array_type_alloc(local, ret, value, size);
}

void make_upgraded_array_object_heap(addr *ret, enum ARRAY_TYPE type, int size)
{
	addr pos;

	switch (type) {
		case ARRAY_TYPE_BIT:
			GetConst(COMMON_BIT, ret);
			break;

		case ARRAY_TYPE_CHARACTER:
			GetConst(COMMON_CHARACTER, ret);
			break;

		case ARRAY_TYPE_SIGNED:
			GetConst(COMMON_SIGNED_BYTE, &pos);
			list_heap(ret, pos, fixnumh((fixnum)size), NULL);
			break;

		case ARRAY_TYPE_UNSIGNED:
			GetConst(COMMON_UNSIGNED_BYTE, &pos);
			list_heap(ret, pos, fixnumh((fixnum)size), NULL);
			break;

		case ARRAY_TYPE_SINGLE_FLOAT:
			GetConst(COMMON_SINGLE_FLOAT, ret);
			break;

		case ARRAY_TYPE_DOUBLE_FLOAT:
			GetConst(COMMON_DOUBLE_FLOAT, ret);
			break;

		case ARRAY_TYPE_LONG_FLOAT:
			GetConst(COMMON_LONG_FLOAT, ret);
			break;

		default:
			*ret = T;
			break;
	}
}

void upgraded_array_object_heap(addr *ret, addr type)
{
	enum ARRAY_TYPE value;
	int size;

	size = 0;
	value = upgraded_array_direct(type, &size);
	make_upgraded_array_object_heap(ret, value, size);
}

void type_object2_array(LocalRoot local,
		enum LISPDECL type, addr pos1, addr pos2, addr *ret)
{
	if (! asterisk_p(pos1))
		upgraded_array_type_alloc(local, &pos1, pos1);
	type_object2(local, type, pos1, pos2, ret);
}

int equal_array_type(addr left, addr right)
{
	enum LISPDECL decl;

	GetLispDecl(left, &decl);
	if (decl != RefLispDecl(right)) return 0;
	if (decl == LISPDECL_SIGNED_BYTE || decl == LISPDECL_UNSIGNED_BYTE) {
		GetArrayType(left, 0, &left);
		GetArrayType(right, 0, &right);
		Check(GetType(left) != LISPTYPE_FIXNUM, "type left error");
		Check(GetType(right) != LISPTYPE_FIXNUM, "type right error");
		return RefFixnum(left) == RefFixnum(right);
	}

	return 1;
}

typedef void (*call_typesymbol)(LocalRoot, enum LISPDECL, addr *);
struct type_symbol {
	constindex name;
	call_typesymbol call;
	enum LISPDECL type;
};
static struct type_symbol TypeSymbol[] = {
	{ CONSTANT_COMMON_ASTERISK,           type_empty,   LISPDECL_ASTERISK            },
	{ CONSTANT_COMMON_NIL,                type_empty,   LISPDECL_NIL                 },
	{ CONSTANT_COMMON_T,                  type_empty,   LISPDECL_T                   },
	{ CONSTANT_COMMON_ATOM,               type_empty,   LISPDECL_ATOM                },
	{ CONSTANT_COMMON_LIST,               type_empty,   LISPDECL_LIST                },
	{ CONSTANT_COMMON_NULL,               type_empty,   LISPDECL_NULL                },
	{ CONSTANT_COMMON_BOOLEAN,            type_empty,   LISPDECL_BOOLEAN             },
	{ CONSTANT_COMMON_HASH_TABLE,         type_empty,   LISPDECL_HASH_TABLE          },
	{ CONSTANT_COMMON_SYMBOL,             type_empty,   LISPDECL_SYMBOL              },
	{ CONSTANT_COMMON_KEYWORD,            type_empty,   LISPDECL_KEYWORD             },
	{ CONSTANT_COMMON_PACKAGE,            type_empty,   LISPDECL_PACKAGE             },
	{ CONSTANT_COMMON_RANDOM_STATE,       type_empty,   LISPDECL_RANDOM_STATE        },
	{ CONSTANT_COMMON_READTABLE,          type_empty,   LISPDECL_READTABLE           },
	{ CONSTANT_COMMON_PATHNAME,           type_empty,   LISPDECL_PATHNAME            },
	{ CONSTANT_COMMON_LOGICAL_PATHNAME,   type_empty,   LISPDECL_LOGICAL_PATHNAME    },
	{ CONSTANT_COMMON_SEQUENCE,           type_empty,   LISPDECL_SEQUENCE            },
	{ CONSTANT_COMMON_CHARACTER,          type_empty,   LISPDECL_CHARACTER           },
	{ CONSTANT_COMMON_BASE_CHAR,          type_empty,   LISPDECL_BASE_CHAR           },
	{ CONSTANT_COMMON_EXTENDED_CHAR,      type_empty,   LISPDECL_EXTENDED_CHAR       },
	{ CONSTANT_COMMON_STANDARD_CHAR,      type_empty,   LISPDECL_STANDARD_CHAR       },
	{ CONSTANT_COMMON_NUMBER,             type_empty,   LISPDECL_NUMBER              },
	{ CONSTANT_COMMON_RATIO,              type_empty,   LISPDECL_RATIO               },
	{ CONSTANT_COMMON_BIT,                type_empty,   LISPDECL_BIT                 },
	{ CONSTANT_COMMON_FIXNUM,             type_empty,   LISPDECL_FIXNUM              },
	{ CONSTANT_COMMON_BIGNUM,             type_empty,   LISPDECL_BIGNUM              },
	{ CONSTANT_COMMON_RESTART,            type_empty,   LISPDECL_RESTART             },
	{ CONSTANT_SYSTEM_ENVIRONMENT,        type_empty,   LISPDECL_ENVIRONMENT         },
	{ CONSTANT_COMMON_STREAM,             type_empty,   LISPDECL_STREAM              },
	{ CONSTANT_COMMON_BROADCAST_STREAM,   type_empty,   LISPDECL_BROADCAST_STREAM    },
	{ CONSTANT_COMMON_CONCATENATED_STREAM,type_empty,   LISPDECL_CONCATENATED_STREAM },
	{ CONSTANT_COMMON_ECHO_STREAM,        type_empty,   LISPDECL_ECHO_STREAM         },
	{ CONSTANT_COMMON_FILE_STREAM,        type_empty,   LISPDECL_FILE_STREAM         },
	{ CONSTANT_COMMON_STRING_STREAM,      type_empty,   LISPDECL_STRING_STREAM       },
	{ CONSTANT_COMMON_SYNONYM_STREAM,     type_empty,   LISPDECL_SYNONYM_STREAM      },
	{ CONSTANT_COMMON_TWO_WAY_STREAM,     type_empty,   LISPDECL_TWO_WAY_STREAM      },
	{ CONSTANT_SYSTEM_BYTESPEC,           type_empty,   LISPDECL_BYTESPEC            },
	{ CONSTANT_COMMON_FUNCTION,           type_aster3,  LISPDECL_FUNCTION            },
	{ CONSTANT_COMMON_COMPILED_FUNCTION,  type_aster3,  LISPDECL_COMPILED_FUNCTION   },
	{ CONSTANT_COMMON_CONS,               type_aster2,  LISPDECL_CONS                },
	{ CONSTANT_COMMON_ARRAY,              type_aster2,  LISPDECL_ARRAY               },
	{ CONSTANT_COMMON_SIMPLE_ARRAY,       type_aster2,  LISPDECL_SIMPLE_ARRAY        },
	{ CONSTANT_COMMON_VECTOR,             type_aster2,  LISPDECL_VECTOR              },
	{ CONSTANT_COMMON_SIMPLE_VECTOR,      type_aster1,  LISPDECL_SIMPLE_VECTOR       },
	{ CONSTANT_COMMON_BIT_VECTOR,         type_aster1,  LISPDECL_BIT_VECTOR          },
	{ CONSTANT_COMMON_SIMPLE_BIT_VECTOR,  type_aster1,  LISPDECL_SIMPLE_BIT_VECTOR   },
	{ CONSTANT_COMMON_STRING,             type_aster1,  LISPDECL_STRING              },
	{ CONSTANT_COMMON_BASE_STRING,        type_aster1,  LISPDECL_BASE_STRING         },
	{ CONSTANT_COMMON_SIMPLE_STRING,      type_aster1,  LISPDECL_SIMPLE_STRING       },
	{ CONSTANT_COMMON_SIMPLE_BASE_STRING, type_aster1,  LISPDECL_SIMPLE_BASE_STRING  },
	{ CONSTANT_COMMON_REAL,               type_aster4,  LISPDECL_REAL                },
	{ CONSTANT_COMMON_RATIONAL,           type_aster4,  LISPDECL_RATIONAL            },
	{ CONSTANT_COMMON_INTEGER,            type_aster4,  LISPDECL_INTEGER             },
	{ CONSTANT_COMMON_SIGNED_BYTE,        type_aster1,  LISPDECL_SIGNED_BYTE         },
	{ CONSTANT_COMMON_UNSIGNED_BYTE,      type_aster1,  LISPDECL_UNSIGNED_BYTE       },
	{ CONSTANT_COMMON_COMPLEX,            type_aster1,  LISPDECL_COMPLEX             },
	{ CONSTANT_COMMON_FLOAT,              type_aster4,  LISPDECL_FLOAT               },
	{ CONSTANT_COMMON_SHORT_FLOAT,        type_aster4,  LISPDECL_SHORT_FLOAT         },
	{ CONSTANT_COMMON_SINGLE_FLOAT,       type_aster4,  LISPDECL_SINGLE_FLOAT        },
	{ CONSTANT_COMMON_DOUBLE_FLOAT,       type_aster4,  LISPDECL_DOUBLE_FLOAT        },
	{ CONSTANT_COMMON_LONG_FLOAT,         type_aster4,  LISPDECL_LONG_FLOAT          },
	{ CONSTANT_EMPTY,                     NULL,         LISPDECL_EMPTY               }
};

static void build_type_symbol(void)
{
	addr table, value, symbol, cons;
	struct type_symbol *str;
	fixnum i;

	/* build-type */
	hashtable_full_heap(&table, HASHTABLE_TEST_EQ, LISPDECL_SIZE,
			1.0, HASHTABLE_REHASH_THRESHOLD_DEFAULT);
	for (i = 0; ; i++) {
		str = &TypeSymbol[i];
		if (str->name == CONSTANT_EMPTY) break;
		GetConstant(str->name, &symbol);
		intern_hashheap(table, symbol, &cons);
		fixnum_heap(&value, i);
		SetCdr(cons, value);
	}
	Root(LISPINDEX_TYPE_SYMBOL) = table;
}


/*
 *  cons-type
 */
static void type_array4(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	addr aster, check, array;
	size_t size;

	GetConst(COMMON_ASTERISK, &aster);
	for (size = 0, check = right; check != Nil; size++) {
		if (! IsCons(check))
			fmte("Invalid of and form.", NULL);
		GetCons(check, &left, &check);
		if (left == aster)
			fmte("The and type don't use a asterisk.", NULL);
	}
	if (0xFFFFFFFFUL < size)
		fmte("type arguments too long.", NULL);
	vector4_alloc(local, &array, size);
	for (size = 0; right != Nil; size++) {
		GetCons(right, &left, &right);
		parse_type_alloc(local, &left, left);
		SetArrayA4(array, size, left);
	}
	type_object1(local, type, array, ret);
}

static void type_eql(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	if (! IsCons(right))
		fmte("Invalid of the eql form.", NULL);
	GetCons(right, &left, &right);
	if (right != Nil)
		fmte("The eql type must be a one argument.", NULL);
	type_object1(local, type, left, ret);
}

static void type_member(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	size_t size;
	addr array;

	for (size = 0, left = right; left != Nil; size++) {
		if (! IsCons(left))
			fmte("Invalid of the member form.", NULL);
		GetCdr(left, &left);
	}
	if (0xFFFFFFFFUL < size)
		fmte("type arguments too long.", NULL);
	vector4_alloc(local, &array, size);
	for (size = 0; right != Nil; size++) {
		GetCons(right, &left, &right);
		SetArrayA4(array, size, left);
	}
	type_object1(local, type, array, ret);
}

static void type_mod(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	if (! IsCons(right))
		fmte("Invalid of the mod form.", NULL);
	GetCons(right, &left, &right);
	if (right != Nil)
		fmte("The mod type must have a one integer.", NULL);
	if (! integerp(left))
		fmte("The mod type must have an integer type.", NULL);
	if (! plusp_integer(left))
		fmte("The mod type must have a plus integer.", NULL);
	type_object1(local, type, left, ret);
}

static void type_not(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	addr check;

	if (! IsCons(right))
		fmte("Invalid of the not form.", NULL);
	GetCons(right, &left, &right);
	if (right != Nil)
		fmte("The not type must have a one argument.", NULL);
	GetConst(COMMON_ASTERISK, &check);
	if (left == check)
		fmte("The not type don't have a *.", NULL);
	parse_type_alloc(local, &left, left);
	type_object1(local, type, left, ret);
}

static void type_satisfies(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	if (! IsCons(right))
		fmte("Invalid of the satisfies form.", NULL);
	GetCons(right, &left, &right);
	if (right != Nil)
		fmte("The satisfies type must have a one symbol.", NULL);
	if (GetType(left) != LISPTYPE_SYMBOL)
		fmte("The satisfies type must have a symbol type.", NULL);
	type_object1(local, type, left, ret);
}

/*
 *  typespec* [&optional typespec*] [&rest typespec]
 *  typespec* [&optional typespec*] [&rest typespec] [&allow-other-keys]
 */
#undef VALUES_ALLOW_ENABLE
static void value_typespec(LocalRoot local,
		addr cons, addr *retvar, addr *retopt, addr *retrest, addr *retallow)
{
	addr var, vars, opt, rest, allow;
	addr const_opt, const_rest;
#ifdef VALUES_ALLOW_ENABLE
	addr const_allow;
#endif

	GetConst(AMPERSAND_OPTIONAL, &const_opt);
	GetConst(AMPERSAND_REST, &const_rest);
#ifdef VALUES_ALLOW_ENABLE
	GetConst(AMPERSAND_ALLOW, &const_allow);
#endif
	vars = opt = rest = allow = Nil;

var_label:
	if (cons == Nil) goto final;
	getcons(cons, &var, &cons);
	if (var == const_opt) goto optional_label;
	if (var == const_rest) goto rest_label;
#ifdef VALUES_ALLOW_ENABLE
	if (var == const_allow) goto allow_label;
#endif
	parse_type_alloc(local, &var, var);
	cons_alloc(local, &vars, var, vars);
	goto var_label;

optional_label:
	if (cons == Nil) goto final;
	getcons(cons, &var, &cons);
	if (var == const_rest) goto rest_label;
#ifdef VALUES_ALLOW_ENABLE
	if (var == const_allow) goto allow_label;
#endif
	parse_type_alloc(local, &var, var);
	cons_alloc(local, &opt, var, opt);
	goto optional_label;

rest_label:
	if (cons == Nil)
		fmte("After &rest argument must be a type.", NULL);
	getcons(cons, &var, &cons);
	if (var == const_opt || var == const_rest)
		fmte("After &rest argument must be a type.", NULL);
#ifdef VALUES_ALLOW_ENABLE
	if (var == const_allow)
		fmte("After &rest argument must be a type.", NULL);
#endif
	parse_type_alloc(local, &rest, var);
	if (cons == Nil) goto final;
	getcons(cons, &var, &cons);
#ifdef VALUES_ALLOW_ENABLE
	if (var == const_allow) goto allow_label;
#endif
	fmte("Invalid values form.", NULL);

#ifdef VALUES_ALLOW_ENABLE
allow_label:
	allow = T;
	if (cons != Nil)
		fmte("After &allow-other-keys must be nil.", NULL);
	goto final;
#endif

final:
	nreverse_list_unsafe(retvar, vars);
	nreverse_list_unsafe(retopt, opt);
	*retrest = rest;
	*retallow = allow;
	return;
}

static void type_values(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	addr var, opt, rest, allow;
	value_typespec(local, right, &var, &opt, &rest, &allow);
	if (rest == Nil)
		type_empty(local, LISPDECL_T, &rest);
	type_object4(local, type, var, opt, rest, allow, ret);
}

static void type_cons(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	addr car, cdr, aster;

	/* no arguments */
	if (right == Nil) goto asterisk;

	/* one argument */
	if (! IsCons(right))
		fmte("Invalid of the cons type.", NULL);
	GetCons(right, &car, &right);
	GetConst(COMMON_ASTERISK, &aster);
	if (right == Nil) {
		if (car == aster) goto asterisk;
		parse_type_alloc(local, &car, car);
		type_asterisk_alloc(local, &cdr);
		type_object2(local, type, car, cdr, ret);
		return;
	}

	/* two arguments */
	if (! IsCons(right))
		fmte("Invalid of the cons type.", NULL);
	GetCons(right, &cdr, &right);
	if (right != Nil)
		fmte("The cons type must have at most 2 arguments.", NULL);
	if (car == aster && cdr == aster) goto asterisk;
	parse_type_alloc(local, &car, car);
	parse_type_alloc(local, &cdr, cdr);
	type_object2(local, type, car, cdr, ret);
	return;

asterisk:
	parse_symbol(local, ret, left);
}

static void type_function_lambda(LocalRoot local, addr *ret, addr cons)
{
	addr const_opt, const_rest, const_key;
	addr var, opt, rest, key, one, name, type;

	GetConst(AMPERSAND_OPTIONAL, &const_opt);
	GetConst(AMPERSAND_REST, &const_rest);
	GetConst(AMPERSAND_KEY, &const_key);
	var = opt = rest = key = one = Nil;

var_label:
	if (cons == Nil) goto final;
	getcons(cons, &one, &cons);
	if (one == const_opt) goto opt_label;
	if (one == const_rest) goto rest_label;
	if (one == const_key) goto key_label;
	parse_type_alloc(local, &one, one);
	cons_alloc(local, &var, one, var);
	goto var_label;

opt_label:
	if (cons == Nil) goto final;
	getcons(cons, &one, &cons);
	if (one == const_opt)
		fmte("&optional parameter don't allow this place.", NULL);
	if (one == const_rest) goto rest_label;
	if (one == const_key) goto key_label;
	parse_type_alloc(local, &one, one);
	cons_alloc(local, &opt, one, opt);
	goto opt_label;

rest_label:
	if (cons == Nil)
		fmte("After &rest parameter must be have a typespec.", NULL);
	getcons(cons, &one, &cons);
	if (one == const_opt || one == const_rest || one == const_key)
		fmte("After &rest parameter don't allow to be a &-symbol.", NULL);
	parse_type_alloc(local, &rest, one);
	if (cons == Nil) goto final;
	getcons(cons, &one, &cons);
	if (one != const_key)
		fmte("After &rest argument don't allow to be a type.", NULL);
	goto key_label;

key_label:
	if (cons == Nil) goto final;
	getcons(cons, &one, &cons);
	if (one == const_opt || one == const_rest || one == const_key)
		fmte("After &key parameter don't allow to be a &-symbol.", NULL);
	if (! IsCons(one))
		fmte("After &key parameter must be a cons.", NULL);
	getcons(one, &name, &one);
	getcons(one, &type, &one);
	if (one != Nil)
		fmte("&key parameter must be a (key type) list.", NULL);
	parse_type_alloc(local, &type, type);
	cons_alloc(local, &one, name, type);
	cons_alloc(local, &key, one, key);
	goto key_label;

final:
	vector2_alloc(local, &one, 4);
	SetArrayA2(one, 0, nreverse_list_unsafe_inplace(var));
	SetArrayA2(one, 1, nreverse_list_unsafe_inplace(opt));
	SetArrayA2(one, 2, rest);
	SetArrayA2(one, 3, nreverse_list_unsafe_inplace(key));
	*ret = one;
}

static void type_function_cons(LocalRoot local, addr *ret, addr right)
{
	addr aster;

	GetConst(COMMON_ASTERISK, &aster);
	Check(GetType(aster) != LISPTYPE_SYMBOL, "type error");
	if (right == aster) {
		type_asterisk_alloc(local, ret);
		return;
	}
	type_function_lambda(local, ret, right);
}

static void type_function(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	addr aster, first, second;

	/* no arguments */
	if (right == Nil) goto asterisk;

	/* one argument */
	if (! IsCons(right))
		fmte("Invalid of the function type.", NULL);
	GetCons(right, &first, &right);
	GetConst(COMMON_ASTERISK, &aster);
	if (right == Nil) {
		if (first == aster) goto asterisk;
		type_function_cons(local, &first, first);
		type_asterisk_alloc(local, &second);
		type_object3(local, type, first, second, Nil, ret);
		return;
	}

	/* two arguments */
	if (! IsCons(right))
		fmte("Invalid of the function type.", NULL);
	GetCons(right, &second, &right);
	if (right != Nil)
		fmte("The function type must have at most 2 argument.", NULL);
	if (first == aster && second == aster) goto asterisk;
	type_function_cons(local, &first, first);
	parse_type_values_alloc(local, &second, second);
	type_object3(local, type, first, second, Nil, ret);
	return;

asterisk:
	type_asterisk_alloc(local, &aster);
	type_object3(local, type, aster, aster, Nil, ret);
}

static int asterisk_length(addr right, size_t *ret)
{
	addr aster, left;
	size_t size;

	GetConst(COMMON_ASTERISK, &aster);
	for (size = 0; right != Nil; size++) {
		if (! IsCons(right))
			fmte("The dimension parameter must be a list.", NULL);
		GetCons(right, &left, &right);
		if (left != aster) return 0;
	}
	*ret = size;

	return 1;
}

static void fixnum_check(addr pos)
{
	if (GetType(pos) != LISPTYPE_FIXNUM || RefFixnum(pos) < 0)
		fmte("The dimension value must be a non-negative fixnum.", NULL);
}

static void dimension_array(LocalRoot local, addr *ret, addr right)
{
	addr aster, left, array;
	size_t size;

	/* length */
	for (size = 0, left = right; left != Nil; size++) {
		if (! IsCons(left))
			fmte("The dimension parameter must be a list.", NULL);
		GetCdr(left, &left);
	}

	/* make vector */
	GetConst(COMMON_ASTERISK, &aster);
	vector4_alloc(local, &array, size);
	for (size = 0; right != Nil; size++) {
		GetCons(right, &left, &right);
		if (left == aster) {
			type_asterisk_alloc(local, &left);
		}
		else {
			fixnum_check(left);
		}
		SetArrayA4(array, size, left);
	}
	*ret = array;
}

static void parse_typearray(LocalRoot local, addr *ret, addr right)
{
	addr aster;
	size_t size;

	GetConst(COMMON_ASTERISK, &aster);
	if (right == Nil) {
		/* dimension arguments, 0 */
		fixnum_alloc(local, ret, 0);
	}
	else if (IsCons(right)) {
		/* dimension arguments */
		if (asterisk_length(right, &size)) {
			if (FIXNUM_MAX < size)
				fmte("size overflow.", NULL);
			fixnum_alloc(local, ret, (fixnum)size);
		}
		else {
			dimension_array(local, ret, right);
		}
	}
	else {
		/* finxum arguments */
		fixnum_check(right);
		*ret = right;
	}
}

static void type_array(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	addr aster, first, second;

	/* no arguments */
	if (right == Nil) goto asterisk;

	/* one argument */
	if (! IsCons(right))
		fmte("Invalid of the array type.", NULL);
	GetCons(right, &first, &right);
	GetConst(COMMON_ASTERISK, &aster);
	if (right == Nil) {
		if (first == aster) goto asterisk;
		parse_type_alloc(local, &first, first);
		type_asterisk_alloc(local, &second);
		type_object2_array(local, type, first, second, ret);
		return;
	}

	/* two arguments */
	if (! IsCons(right))
		fmte("Invalid of the array type.", NULL);
	GetCons(right, &second, &right);
	if (right != Nil)
		fmte("The array type arguments too long.", NULL);
	if (first == aster && second == aster)
		goto asterisk;
	if (first == aster)
		type_asterisk_alloc(local, &first);
	else
		parse_type_alloc(local, &first, first);
	if (second == aster)
		type_asterisk_alloc(local, &second);
	else
		parse_typearray(local, &second, second);
	type_object2_array(local, type, first, second, ret);
	return;

asterisk:
	parse_symbol(local, ret, left);
}

static void type_vector(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	addr aster, first, second;

	/* no arguments */
	if (right == Nil) goto asterisk;

	/* one argument */
	if (! IsCons(right))
		fmte("Invalid of the vector type.", NULL);
	GetCons(right, &first, &right);
	GetConst(COMMON_ASTERISK, &aster);
	if (right == Nil) {
		if (first == aster) goto asterisk;
		parse_type_alloc(local, &first, first);
		type_asterisk_alloc(local, &second);
		type_object2_array(local, type, first, second, ret);
		return;
	}

	/* two arguments */
	if (! IsCons(right))
		fmte("Invalid of the vector type.", NULL);
	GetCons(right, &second, &right);
	if (right != Nil)
		fmte("The vector type arguments too long.", NULL);
	if (first == aster && second == aster)
		goto asterisk;
	if (first == aster)
		type_asterisk_alloc(local, &first);
	else
		parse_type_alloc(local, &first, first);
	if (second == aster)
		type_asterisk_alloc(local, &second);
	else
		fixnum_check(second);
	type_object2_array(local, type, first, second, ret);
	return;

asterisk:
	parse_symbol(local, ret, left);
}

static void type_size(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	addr aster, first;

	/* no arguments */
	if (right == Nil) goto asterisk;

	/* one argument */
	if (! IsCons(right))
		fmte("Invalid of the type form.", NULL);
	GetCons(right, &first, &right);
	if (right != Nil)
		fmte("The type arguments too long.", NULL);
	GetConst(COMMON_ASTERISK, &aster);
	if (first == aster) goto asterisk;
	fixnum_check(first);
	type_object1(local, type, first, ret);
	return;

asterisk:
	parse_symbol(local, ret, left);
}

static void range_element(void (*call)(addr), addr *ret1, addr *ret2, addr pos)
{
	addr left, right;

	if (IsCons(pos)) {
		GetCons(pos, &left, &right);
		if (right != Nil)
			fmte("The type ~S must be a number or single cons", pos, NULL);
		(*call)(left);
		*ret1 = T;
		*ret2 = left;
	}
	else {
		(*call)(pos);
		*ret1 = Nil;
		*ret2 = pos;
	}
}

/* (integer 10 (20))  -> (integer nil 10 t 20) */
static void type_range_call(LocalRoot local,
		enum LISPDECL type, addr left, addr right, void (*call)(addr), addr *ret)
{
	addr aster, first1, first2, second1, second2;

	/* no arguments */
	if (right == Nil) goto asterisk;

	/* one argument */
	if (! IsCons(right))
		fmte("Invalid of the type form.", NULL);
	GetCons(right, &first1, &right);
	GetConst(COMMON_ASTERISK, &aster);
	if (right == Nil) {
		if (first1 == aster) goto asterisk;
		range_element(call, &first1, &first2, first1);
		type_asterisk_alloc(local, &second1);
		type_object4(local, type, first1, first2, second1, second1, ret);
		return;
	}

	/* two arguments */
	if (! IsCons(right))
		fmte("Invalid of the type form.", NULL);
	GetCons(right, &second1, &right);
	if (right != Nil)
		fmte("The type arguments too long.", NULL);
	if (first1 == aster && second1 == aster) goto asterisk;
	if (first1 == aster) {
		type_asterisk_alloc(local, &first1);
		first2 = first1;
	}
	else {
		range_element(call, &first1, &first2, first1);
	}
	if (second1 == aster) {
		type_asterisk_alloc(local, &second1);
		second2 = second1;
	}
	else {
		range_element(call, &second1, &second2, second1);
	}
	type_object4(local, type, first1, first2, second1, second2, ret);
	return;

asterisk:
	parse_symbol(local, ret, left);
}

static void realp_type(addr pos)
{
	if (! realp(pos))
		fmte("The type argument must be a real type.", NULL);
}
static void type_real(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	type_range_call(local, type, left, right, realp_type, ret);
}

static void rationalp_type(addr pos)
{
	if (! rationalp(pos))
		fmte("The type argument must be a rational type.", NULL);
}
static void type_rational(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	type_range_call(local, type, left, right, rationalp_type, ret);
}

static void integerp_type(addr pos)
{
	if (! integerp(pos))
		fmte("The type argument must be an integer type.", NULL);
}
static void type_integer(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	type_range_call(local, type, left, right, integerp_type, ret);
}

static void integer_check(addr pos)
{
	if (GetType(pos) != LISPTYPE_FIXNUM || (! plusp_integer(pos)))
		fmte("Dimension value must be a positive integer.", NULL);
}

static void type_byte(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	addr aster, first;

	/* no arguments */
	if (right == Nil) goto asterisk;

	/* one argument */
	if (! IsCons(right))
		fmte("Invalid of the byte type.", NULL);
	GetCons(right, &first, &right);
	if (right != Nil)
		fmte("The type arguments too long.", NULL);
	GetConst(COMMON_ASTERISK, &aster);
	if (first == aster) goto asterisk;
	integer_check(first);
	type_object1(local, type, first, ret);
	return;

asterisk:
	parse_symbol(local, ret, left);
}

static void type_complex(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	addr aster, first;

	/* no arguments */
	if (right == Nil) goto asterisk;

	/* one argument */
	if (! IsCons(right))
		fmte("Invalid of the complex type.", NULL);
	GetCons(right, &first, &right);
	if (right != Nil)
		fmte("type arguments too long.", NULL);
	GetConst(COMMON_ASTERISK, &aster);
	if (first == aster) goto asterisk;
	parse_type_alloc(local, &first, first);
	type_object1(local, type, first, ret);
	return;

asterisk:
	parse_symbol(local, ret, left);
}

static void floatp_type(addr pos)
{
	if (! floatp(pos))
		fmte("Arguments of type specification must be a float type.", NULL);
}
static void type_float(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	type_range_call(local, type, left, right, floatp_type, ret);
}

static void short_floatp_type(addr pos)
{
	if (GetType(pos) != LISPTYPE_SHORT_FLOAT)
		fmte("Arguments of type specification must be a short-float type.", NULL);
}
static void type_short(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	type_range_call(local, type, left, right, short_floatp_type, ret);
}

static void single_floatp_type(addr pos)
{
	if (GetType(pos) != LISPTYPE_SINGLE_FLOAT)
		fmte("Arguments of type specification must be a single-float type.", NULL);
}
static void type_single(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	type_range_call(local, type, left, right, single_floatp_type, ret);
}

static void double_floatp_type(addr pos)
{
	if (GetType(pos) != LISPTYPE_DOUBLE_FLOAT)
		fmte("Arguments of type specification must be a double-float type.", NULL);
}
static void type_double(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	type_range_call(local, type, left, right, double_floatp_type, ret);
}

static void long_floatp_type(addr pos)
{
	if (GetType(pos) != LISPTYPE_LONG_FLOAT)
		fmte("Arguments of type specification must be a long-float type.", NULL);
}
static void type_long(LocalRoot local,
		enum LISPDECL type, addr left, addr right, addr *ret)
{
	type_range_call(local, type, left, right, long_floatp_type, ret);
}

typedef void (*call_typeparam)(LocalRoot, enum LISPDECL, addr, addr, addr *);
struct type_param {
	constindex name;
	call_typeparam call;
	enum LISPDECL type;
};
static struct type_param TypeParam[] = {
	/* Compound-type */
	{ CONSTANT_COMMON_AND,                type_array4,   LISPDECL_AND        },
	{ CONSTANT_COMMON_OR,                 type_array4,   LISPDECL_OR         },
	{ CONSTANT_COMMON_EQL,                type_eql,      LISPDECL_EQL        },
	{ CONSTANT_COMMON_MEMBER,             type_member,   LISPDECL_MEMBER     },
	{ CONSTANT_COMMON_MOD,                type_mod,      LISPDECL_MOD        },
	{ CONSTANT_COMMON_NOT,                type_not,      LISPDECL_NOT        },
	{ CONSTANT_COMMON_SATISFIES,          type_satisfies, LISPDECL_SATISFIES },
	/*{ CONSTANT_COMMON_VALUES,           type_values,   LISPDECL_VALUES     },*/
	/* Atomic-type */
	{ CONSTANT_COMMON_CONS,               type_cons,     LISPDECL_CONS              },
	{ CONSTANT_COMMON_FUNCTION,           type_function, LISPDECL_FUNCTION          },
	{ CONSTANT_COMMON_COMPILED_FUNCTION,  type_function, LISPDECL_COMPILED_FUNCTION },
	{ CONSTANT_COMMON_ARRAY,              type_array,    LISPDECL_ARRAY             },
	{ CONSTANT_COMMON_SIMPLE_ARRAY,       type_array,    LISPDECL_SIMPLE_ARRAY      },
	{ CONSTANT_COMMON_VECTOR,             type_vector,   LISPDECL_VECTOR            },
	{ CONSTANT_COMMON_SIMPLE_VECTOR,      type_size,     LISPDECL_SIMPLE_VECTOR     },
	{ CONSTANT_COMMON_BIT_VECTOR,         type_size,     LISPDECL_BIT_VECTOR        },
	{ CONSTANT_COMMON_SIMPLE_BIT_VECTOR,  type_size,     LISPDECL_SIMPLE_BIT_VECTOR },
	{ CONSTANT_COMMON_STRING,             type_size,     LISPDECL_STRING            },
	{ CONSTANT_COMMON_BASE_STRING,        type_size,     LISPDECL_BASE_STRING       },
	{ CONSTANT_COMMON_SIMPLE_STRING,      type_size,     LISPDECL_SIMPLE_STRING     },
	{ CONSTANT_COMMON_SIMPLE_BASE_STRING, type_size,    LISPDECL_SIMPLE_BASE_STRING },
	{ CONSTANT_COMMON_REAL,               type_real,     LISPDECL_REAL              },
	{ CONSTANT_COMMON_RATIONAL,           type_rational, LISPDECL_RATIONAL          },
	{ CONSTANT_COMMON_INTEGER,            type_integer,  LISPDECL_INTEGER           },
	{ CONSTANT_COMMON_SIGNED_BYTE,        type_byte,     LISPDECL_SIGNED_BYTE       },
	{ CONSTANT_COMMON_UNSIGNED_BYTE,      type_byte,     LISPDECL_UNSIGNED_BYTE     },
	{ CONSTANT_COMMON_COMPLEX,            type_complex,  LISPDECL_COMPLEX           },
	{ CONSTANT_COMMON_FLOAT,              type_float,    LISPDECL_FLOAT             },
	{ CONSTANT_COMMON_SHORT_FLOAT,        type_short,    LISPDECL_SHORT_FLOAT       },
	{ CONSTANT_COMMON_SINGLE_FLOAT,       type_single,   LISPDECL_SINGLE_FLOAT      },
	{ CONSTANT_COMMON_DOUBLE_FLOAT,       type_double,   LISPDECL_DOUBLE_FLOAT      },
	{ CONSTANT_COMMON_LONG_FLOAT,         type_long,     LISPDECL_LONG_FLOAT        },
	/* END */
	{ CONSTANT_EMPTY,                     NULL,          LISPDECL_EMPTY             }
};

static void build_type_param(void)
{
	addr table, value, symbol, cons;
	fixnum i;
	struct type_param *str;

	hashtable_full_heap(&table, HASHTABLE_TEST_EQ, 30,
			1.0, HASHTABLE_REHASH_THRESHOLD_DEFAULT);
	for (i = 0; ; i++) {
		str = &TypeParam[i];
		if (str->name == CONSTANT_EMPTY) break;
		GetConstant(str->name, &symbol);
		intern_hashheap(table, symbol, &cons);
		fixnum_heap(&value, i);
		SetCdr(cons, value);
	}
	Root(LISPINDEX_TYPE_PARAM) = table;
}

static void type_clos_local(LocalRoot local, addr pos, addr *ret)
{
	pos = find_class_nil(pos);
	if (pos == Nil) {
		*ret = Nil;
		return;
	}
	type_object1(local, LISPDECL_CLOS, pos, ret);
}

static void parse_symbol(LocalRoot local, addr *ret, addr type)
{
	addr table, pos;
	struct type_symbol *str;

	table = Root(LISPINDEX_TYPE_SYMBOL);
	Check(GetType(table) != LISPTYPE_HASHTABLE, "type symbol-table error.");
	if (findvalue_hashtable(table, type, &pos)) {
		type_clos_local(local, type, &pos);
		if (pos == Nil)
			fmte("type error.", NULL);
	}
	else {
		str = &TypeSymbol[(int)RefFixnum(pos)];
		(str->call)(local, str->type, &pos);
	}
	CheckType(pos, LISPTYPE_TYPE);
	*ret = pos;
}

static void parse_param(LocalRoot local, addr *ret, addr left, addr right)
{
	addr table, pos;
	fixnum index;
	struct type_param *str;
	call_typeparam call;

	table = Root(LISPINDEX_TYPE_PARAM);
	Check(GetType(table) != LISPTYPE_HASHTABLE, "type error.");
	if (findvalue_hashtable(table, left, &pos))
		fmte("type error.", NULL);
	Check(GetType(pos) != LISPTYPE_FIXNUM, "type fixnum error.");
	GetFixnum(pos, &index);
	str = &TypeParam[index];
	call = str->call;
	Check(call == NULL, "TypeParam error.");
	(*call)(local, str->type, left, right, ret);
}

/* parse_type */
void parse_type_values_alloc(LocalRoot local, addr *ret, addr type)
{
	addr left, check;

	if (IsCons(type)) {
		getcons(type, &left, &type);
		GetConst(COMMON_VALUES, &check);
		if (check == left)
			type_values(local, LISPDECL_VALUES, left, type, ret);
		else
			parse_param(local, ret, left, type);
	}
	else {
		parse_symbol(local, ret, type);
	}
}
void parse_type_values_local(LocalRoot local, addr *ret, addr type)
{
	Check(local == NULL, "local error");
	parse_type_values_alloc(local, ret, type);
}
void parse_type_values_heap(addr *ret, addr type)
{
	parse_type_values_alloc(NULL, ret, type);
}

void parse_type_alloc(LocalRoot local, addr *ret, addr type)
{
	addr left;

	if (IsCons(type)) {
		getcons(type, &left, &type);
		parse_param(local, ret, left, type);
	}
	else {
		parse_symbol(local, ret, type);
	}
}
void parse_type_local(LocalRoot local, addr *ret, addr type)
{
	Check(local == NULL, "local error");
	parse_type_alloc(local, ret, type);
}
void parse_type_heap(addr *ret, addr type)
{
	parse_type_alloc(NULL, ret, type);
}

void parse_type_not_alloc(LocalRoot local, addr *ret, addr type)
{
	parse_type_alloc(local, ret, type);
	reversenotdecl(*ret);
}
void parse_type_not_local(LocalRoot local, addr *ret, addr type)
{
	Check(local == NULL, "local error");
	parse_type_not_alloc(local, ret, type);
}
void parse_type_not_heap(addr *ret, addr type)
{
	parse_type_not_alloc(NULL, ret, type);
}

void parse_type_no_asterisk_alloc(LocalRoot local, addr *ret, addr type)
{
	addr asterisk;

	GetConst(COMMON_ASTERISK, &asterisk);
	if (type == asterisk)
		fmte("Don't allow to use asterisk type.", NULL);
	parse_type_alloc(local, ret, type);
}
void parse_type_no_asterisk_local(LocalRoot local, addr *ret, addr type)
{
	Check(local == NULL, "local error");
	parse_type_no_asterisk_alloc(local, ret, type);
}
void parse_type_no_asterisk_heap(addr *ret, addr type)
{
	parse_type_no_asterisk_alloc(NULL, ret, type);
}

/* type_throw */
void type_throw_alloc(LocalRoot local, addr *ret, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_CONS:
		case LISPTYPE_SYMBOL:
			parse_type_alloc(local, ret, pos);
			break;

		case LISPTYPE_TYPE:
			*ret = pos;
			break;

		default:
			fmte("Invalid of type argument ~S.", pos, NULL);
			break;
	}
}
void type_throw_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	type_throw_alloc(local, ret, pos);
}
void type_throw_heap(addr *ret, addr pos)
{
	type_throw_alloc(NULL, ret, pos);
}


/*****************************************************************************
 *  typedecl-classname
 *****************************************************************************/
enum CONSTANT_INDEX TypeDeclClass[LISPDECL_SIZE];

enum CONSTANT_INDEX typedecl_classname(enum LISPDECL type)
{
	Check(LISPDECL_SIZE <= type, "index error");
	return TypeDeclClass[type];
}

struct type_class {
	enum LISPDECL decl;
	enum CONSTANT_INDEX index;
};

static struct type_class TypeClass[] = {
	{ LISPDECL_EMPTY,               CONSTANT_EMPTY                      },
	{ LISPDECL_OPTIMIZED,           CONSTANT_EMPTY                      },
	{ LISPDECL_SUBTYPEP,            CONSTANT_EMPTY                      },
	{ LISPDECL_TYPE,                CONSTANT_EMPTY                      },
	{ LISPDECL_CLOS,                CONSTANT_EMPTY                      },
	{ LISPDECL_ASTERISK,            CONSTANT_COMMON_ASTERISK            },
	/* Compound-type */
	{ LISPDECL_AND,                 CONSTANT_EMPTY                      },
	{ LISPDECL_EQL,                 CONSTANT_EMPTY                      },
	{ LISPDECL_MEMBER,              CONSTANT_EMPTY                      },
	{ LISPDECL_MOD,                 CONSTANT_EMPTY                      },
	{ LISPDECL_NOT,                 CONSTANT_EMPTY                      },
	{ LISPDECL_OR,                  CONSTANT_EMPTY                      },
	{ LISPDECL_SATISFIES,           CONSTANT_EMPTY                      },
	{ LISPDECL_VALUES,              CONSTANT_EMPTY                      },
	/* Extract-type */
	{ LISPDECL_ATOM,                CONSTANT_COMMON_ATOM                },
	{ LISPDECL_LIST,                CONSTANT_COMMON_LIST                },
	{ LISPDECL_BOOLEAN,             CONSTANT_COMMON_BOOLEAN             },
	{ LISPDECL_VECTOR,              CONSTANT_COMMON_VECTOR              },
	{ LISPDECL_SIMPLE_VECTOR,       CONSTANT_COMMON_SIMPLE_VECTOR       },
	{ LISPDECL_BIT_VECTOR,          CONSTANT_COMMON_BIT_VECTOR          },
	{ LISPDECL_SIMPLE_BIT_VECTOR,   CONSTANT_COMMON_SIMPLE_BIT_VECTOR   },
	{ LISPDECL_EXTENDED_CHAR,       CONSTANT_COMMON_EXTENDED_CHAR       },
	{ LISPDECL_STRING,              CONSTANT_COMMON_STRING              },
	{ LISPDECL_BASE_STRING,         CONSTANT_COMMON_BASE_STRING         },
	{ LISPDECL_SIMPLE_STRING,       CONSTANT_COMMON_SIMPLE_STRING       },
	{ LISPDECL_SIMPLE_BASE_STRING,  CONSTANT_COMMON_SIMPLE_BASE_STRING  },
	{ LISPDECL_SIGNED_BYTE,         CONSTANT_COMMON_SIGNED_BYTE         },
	{ LISPDECL_UNSIGNED_BYTE,       CONSTANT_COMMON_UNSIGNED_BYTE       },
	{ LISPDECL_BIT,                 CONSTANT_COMMON_BIT                 },
	{ LISPDECL_FIXNUM,              CONSTANT_COMMON_FIXNUM              },
	{ LISPDECL_BIGNUM,              CONSTANT_COMMON_BIGNUM              },
	/* Atomic-type */
	{ LISPDECL_NIL,                 CONSTANT_COMMON_NIL                 },
	{ LISPDECL_T,                   CONSTANT_COMMON_T                   },
	{ LISPDECL_NULL,                CONSTANT_COMMON_NULL                },
	{ LISPDECL_CONS,                CONSTANT_COMMON_CONS                },
	{ LISPDECL_HASH_TABLE,          CONSTANT_COMMON_HASH_TABLE          },
	{ LISPDECL_SYMBOL,              CONSTANT_COMMON_SYMBOL              },
	{ LISPDECL_KEYWORD,             CONSTANT_COMMON_KEYWORD             },
	{ LISPDECL_PACKAGE,             CONSTANT_COMMON_PACKAGE             },
	{ LISPDECL_RANDOM_STATE,        CONSTANT_COMMON_RANDOM_STATE        },
	{ LISPDECL_READTABLE,           CONSTANT_COMMON_READTABLE           },
	{ LISPDECL_FUNCTION,            CONSTANT_COMMON_FUNCTION            },
	{ LISPDECL_COMPILED_FUNCTION,   CONSTANT_COMMON_COMPILED_FUNCTION   },
	{ LISPDECL_PATHNAME,            CONSTANT_COMMON_PATHNAME            },
	{ LISPDECL_LOGICAL_PATHNAME,    CONSTANT_COMMON_LOGICAL_PATHNAME    },
	{ LISPDECL_SEQUENCE,            CONSTANT_COMMON_SEQUENCE            },
	{ LISPDECL_ARRAY,               CONSTANT_COMMON_ARRAY               },
	{ LISPDECL_SIMPLE_ARRAY,        CONSTANT_COMMON_SIMPLE_ARRAY        },
	{ LISPDECL_CHARACTER,           CONSTANT_COMMON_CHARACTER           },
	{ LISPDECL_BASE_CHAR,           CONSTANT_COMMON_BASE_CHAR           },
	{ LISPDECL_STANDARD_CHAR,       CONSTANT_COMMON_STANDARD_CHAR       },
	{ LISPDECL_NUMBER,              CONSTANT_COMMON_NUMBER              },
	{ LISPDECL_REAL,                CONSTANT_COMMON_REAL                },
	{ LISPDECL_RATIONAL,            CONSTANT_COMMON_RATIONAL            },
	{ LISPDECL_RATIO,               CONSTANT_COMMON_RATIO               },
	{ LISPDECL_INTEGER,             CONSTANT_COMMON_INTEGER             },
	{ LISPDECL_COMPLEX,             CONSTANT_COMMON_COMPLEX             },
	{ LISPDECL_FLOAT,               CONSTANT_COMMON_FLOAT               },
	{ LISPDECL_SHORT_FLOAT,         CONSTANT_COMMON_SHORT_FLOAT         },
	{ LISPDECL_SINGLE_FLOAT,        CONSTANT_COMMON_SINGLE_FLOAT        },
	{ LISPDECL_DOUBLE_FLOAT,        CONSTANT_COMMON_DOUBLE_FLOAT        },
	{ LISPDECL_LONG_FLOAT,          CONSTANT_COMMON_LONG_FLOAT          },
	{ LISPDECL_RESTART,             CONSTANT_COMMON_RESTART             },
	{ LISPDECL_ENVIRONMENT,         CONSTANT_SYSTEM_ENVIRONMENT         },
	{ LISPDECL_STREAM,              CONSTANT_COMMON_STREAM              },
	{ LISPDECL_BROADCAST_STREAM,    CONSTANT_COMMON_BROADCAST_STREAM    },
	{ LISPDECL_CONCATENATED_STREAM, CONSTANT_COMMON_CONCATENATED_STREAM },
	{ LISPDECL_ECHO_STREAM,         CONSTANT_COMMON_ECHO_STREAM         },
	{ LISPDECL_FILE_STREAM,         CONSTANT_COMMON_FILE_STREAM         },
	{ LISPDECL_STRING_STREAM,       CONSTANT_COMMON_STRING_STREAM       },
	{ LISPDECL_SYNONYM_STREAM,      CONSTANT_COMMON_SYNONYM_STREAM      },
	{ LISPDECL_TWO_WAY_STREAM,      CONSTANT_COMMON_TWO_WAY_STREAM      },
	{ LISPDECL_QUOTE,               CONSTANT_COMMON_QUOTE               },
	{ LISPDECL_BYTESPEC,            CONSTANT_SYSTEM_BYTESPEC            },
	{ LISPDECL_SIZE,                CONSTANT_EMPTY                      },
};

void init_type_class(void)
{
	int i;
	struct type_class *table;
	enum LISPDECL decl;

	for (i = 0; ; i++) {
		table = &TypeClass[i];
		decl = table->decl;
		if (decl == LISPDECL_SIZE) break;
		TypeDeclClass[(int)decl] = table->index;
	}
}


/*****************************************************************************
 *  built-type
 *****************************************************************************/
void build_type(void)
{
	build_type_symbol();
	build_type_param();
}

int type_symbol_p(addr symbol)
{
	addr table, check;

	/* symbol check */
	if (! IsSymbol(symbol)) return 0;

	/* type check */
	table = Root(LISPINDEX_TYPE_SYMBOL);
	findcons_hashtable(table, symbol, &check);
	if (check != Nil) return 1;

	/* clos (structure) check */
	check = find_class_nil(symbol);
	return check != Nil;
}

