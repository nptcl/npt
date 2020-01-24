#include "clos_class.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "copy.h"
#include "sequence.h"
#include "type.h"
#include "type_number.h"
#include "type_object.h"
#include "type_optimize.h"
#include "type_symbol.h"

typedef void (*type_object_call)(addr *, addr);
static type_object_call TypeObjectTable[LISPTYPE_SIZE];

static void type_object_error(addr *ret, addr pos)
{
	infobit(pos);
	fmte("Invalid type.", NULL);
}

static void type_object_name(addr *ret, addr pos)
{
	constindex index = getdeclname(RefLispDecl(pos));
	GetConstant(index, ret);
}

static void type_object_optimized(addr *ret, addr pos)
{
	get_type_optimized(&pos, pos);
	type_object(ret, pos);
}

static void type_object_subtypep(addr *ret, addr pos)
{
	get_type_subtypep(&pos, pos);
	type_object(ret, pos);
}

static void type_object_type(addr *ret, addr pos)
{
	GetConst(SYSTEM_TYPE, ret);
}

static void type_object_clos(addr *ret, addr pos)
{
	GetArrayType(pos, 0, &pos);
	if (type_asterisk_p(pos)) {
		GetConst(COMMON_CLASS, ret);
	}
	else {
		clos_class_of(pos, &pos);
		stdget_class_name(pos, ret);
	}
}

static void type_object_vectortype(addr *ret, addr name, addr pos)
{
	addr array, root, temp;
	size_t size;

	GetArrayType(pos, 0, &array);
	LenArrayA4(array, &size);
	for (root = Nil; size; ) {
		size--;
		GetArrayA4(array, size, &temp);
		type_object(&temp, temp);
		cons_heap(&root, temp, root);
	}
	cons_heap(ret, name, root);
}

static void type_object_and(addr *ret, addr pos)
{
	addr name;
	GetConst(COMMON_AND, &name);
	type_object_vectortype(ret, name, pos);
}

static void type_object_or(addr *ret, addr pos)
{
	addr name;
	GetConst(COMMON_OR, &name);
	type_object_vectortype(ret, name, pos);
}

static void type_object_operator1(addr *ret, constindex index, addr pos)
{
	addr name;
	GetConstant(index, &name);
	GetArrayType(pos, 0, &pos);
	copyheap(&pos, pos);
	list_heap(ret, name, pos, NULL);
}

static void type_object_eql(addr *ret, addr pos)
{
	type_object_operator1(ret, CONSTANT_COMMON_EQL, pos);
}

static void type_object_member(addr *ret, addr pos)
{
	addr array, root, temp;
	size_t size;

	GetArrayType(pos, 0, &array);
	LenArrayA4(array, &size);
	for (root = Nil; size; ) {
		size--;
		GetArrayA4(array, size, &temp);
		copyheap(&temp, temp);
		cons_heap(&root, temp, root);
	}
	GetConst(COMMON_MEMBER, &temp);
	cons_heap(ret, temp, root);
}

static void type_object_mod(addr *ret, addr pos)
{
	type_object_operator1(ret, CONSTANT_COMMON_MOD, pos);
}

static void type_object_not(addr *ret, addr pos)
{
	addr name;

	GetConst(COMMON_NOT, &name);
	GetArrayType(pos, 0, &pos);
	type_object(&pos, pos);
	list_heap(ret, name, pos, NULL);
}

static void type_object_satisfies(addr *ret, addr pos)
{
	type_object_operator1(ret, CONSTANT_COMMON_SATISFIES, pos);
}

static void type_object_values(addr *ret, addr pos)
{
	addr root, list, value;

	/* first */
	GetConst(COMMON_VALUES, &value);
	conscar_heap(&root, value);

	/* variable */
	GetArrayType(pos, 0, &list);
	while (list != Nil) {
		GetCons(list, &value, &list);
		type_object(&value, value);
		cons_heap(&root, value, root);
	}

	/* &optional */
	GetArrayType(pos, 1, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_OPTIONAL, &value);
		cons_heap(&root, value, root);
	}
	while (list != Nil) {
		GetCons(list, &value, &list);
		type_object(&value, value);
		cons_heap(&root, value, root);
	}

	/* &rest */
	GetArrayType(pos, 2, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_REST, &value);
		cons_heap(&root, value, root);
		type_object(&list, list);
		cons_heap(&root, list, root);
	}

	/* &allow-other-keys (always nil) */
	GetArrayType(pos, 3, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_ALLOW, &value);
		cons_heap(&root, value, root);
	}

	/* result */
	nreverse_list_unsafe(ret, root);
}

static void type_object_vector(addr *ret, addr pos)
{
	addr name, type;

	GetConst(COMMON_VECTOR, &name);
	GetArrayType(pos, 0, &type);
	GetArrayType(pos, 1, &pos);
	if (type_asterisk_p(type) && type_asterisk_p(pos)) {
		*ret = name;
		return;
	}
	type_object(&type, type);
	if (type_asterisk_p(pos))
		GetConst(COMMON_ASTERISK, &pos);
	list_heap(ret, name, type, pos, NULL);
}

static void type_object_size(addr *ret, constindex index, addr pos)
{
	addr name;

	GetConstant(index, &name);
	GetArrayType(pos, 0, &pos);
	if (type_asterisk_p(pos)) {
		*ret = name;
		return;
	}
	list_heap(ret, name, pos, NULL);
}

static void type_object_simple_vector(addr *ret, addr pos)
{
	type_object_size(ret, CONSTANT_COMMON_SIMPLE_VECTOR, pos);
}

static void type_object_bit_vector(addr *ret, addr pos)
{
	type_object_size(ret, CONSTANT_COMMON_BIT_VECTOR, pos);
}

static void type_object_simple_bit_vector(addr *ret, addr pos)
{
	type_object_size(ret, CONSTANT_COMMON_SIMPLE_BIT_VECTOR, pos);
}

static void type_object_string(addr *ret, addr pos)
{
	type_object_size(ret, CONSTANT_COMMON_STRING, pos);
}

static void type_object_base_string(addr *ret, addr pos)
{
	type_object_size(ret, CONSTANT_COMMON_BASE_STRING, pos);
}

static void type_object_simple_string(addr *ret, addr pos)
{
	type_object_size(ret, CONSTANT_COMMON_SIMPLE_STRING, pos);
}

static void type_object_simple_base_string(addr *ret, addr pos)
{
	type_object_size(ret, CONSTANT_COMMON_SIMPLE_BASE_STRING, pos);
}

static void type_object_signed_byte(addr *ret, addr pos)
{
	type_object_size(ret, CONSTANT_COMMON_SIGNED_BYTE, pos);
}

static void type_object_unsigned_byte(addr *ret, addr pos)
{
	type_object_size(ret, CONSTANT_COMMON_UNSIGNED_BYTE, pos);
}

static void type_object_cons(addr *ret, addr pos)
{
	addr name, car, cdr;

	GetConst(COMMON_CONS, &name);
	GetArrayType(pos, 0, &car);
	GetArrayType(pos, 1, &cdr);
	if (type_asterisk_p(car) && type_asterisk_p(cdr)) {
		*ret = name;
		return;
	}
	type_object(&car, car);
	type_object(&cdr, cdr);
	list_heap(ret, name, car, cdr, NULL);
}

static void type_object_function_args(addr *ret, addr type)
{
	addr root, list, pos, value;

	CheckType(type, LISPTYPE_VECTOR);
	root = Nil;
	/* var */
	GetArrayA2(type, 0, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		type_object(&pos, pos);
		cons_heap(&root, pos, root);
	}

	/* opt */
	GetArrayA2(type, 1, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_OPTIONAL, &pos);
		cons_heap(&root, pos, root);
		while (list != Nil) {
			GetCons(list, &pos, &list);
			type_object(&pos, pos);
			cons_heap(&root, pos, root);
		}
	}

	/* rest */
	GetArrayA2(type, 2, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_REST, &pos);
		cons_heap(&root, pos, root);
		type_object(&list, list);
		cons_heap(&root, list, root);
	}

	/* key */
	GetArrayA2(type, 3, &list);
	if (list != Nil) {
		GetConst(AMPERSAND_KEY, &pos);
		cons_heap(&root, pos, root);
		while (list != Nil) {
			GetCons(list, &pos, &list);
			GetCons(pos, &pos, &value);
			type_object(&value, value);
			list_heap(&pos, pos, value, NULL);
			cons_heap(&root, pos, root);
		}
	}

	/* result */
	nreverse_list_unsafe(ret, root);
}

static void type_object_functiontype(addr *ret, constindex index, addr pos)
{
	addr name, type;

	/* name */
	GetConstant(index, &name);
	/* argument */
	GetArrayType(pos, 0, &type);
	GetArrayType(pos, 1, &pos);
	if (type_asterisk_p(type) && type_asterisk_p(pos)) {
		*ret = name;
		return;
	}
	if (type_asterisk_p(type))
		GetConst(COMMON_ASTERISK, &type);
	else
		type_object_function_args(&type, type);
	/* values */
	if (type_asterisk_p(pos))
		GetConst(COMMON_ASTERISK, &pos);
	else
		type_object(&pos, pos);
	/* result */
	list_heap(ret, name, type, pos, NULL);
}

static void type_object_function(addr *ret, addr pos)
{
	type_object_functiontype(ret, CONSTANT_COMMON_FUNCTION, pos);
}

static void type_object_compiled_function(addr *ret, addr pos)
{
	type_object_functiontype(ret, CONSTANT_COMMON_COMPILED_FUNCTION, pos);
}

static void type_object_arraydimension(addr pos, addr *ret)
{
	addr root, temp;
	size_t size;

	if (GetType(pos) == LISPTYPE_FIXNUM) {
		copyheap(ret, pos);
		return;
	}
	Check(GetType(pos) != LISPTYPE_VECTOR, "type error");
	LenArrayA4(pos, &size);
	for (root = Nil; size; ) {
		size--;
		GetArrayA4(pos, size, &temp);
		copyheap(&temp, temp);
		cons_heap(&root, temp, root);
	}
	*ret = root;
}

static void type_object_arraytype(addr *ret, constindex index, addr pos)
{
	addr name, type;

	/* name */
	GetConstant(index, &name);
	/* type */
	GetArrayType(pos, 0, &type);
	GetArrayType(pos, 1, &pos);
	if (type_asterisk_p(type) && type_asterisk_p(pos)) {
		copyheap(ret, name);
		return;
	}
	if (type_asterisk_p(type))
		GetConst(COMMON_ASTERISK, &type);
	else
		type_object(&type, type);
	/* dimension */
	if (type_asterisk_p(pos))
		GetConst(COMMON_ASTERISK, &pos);
	else
		type_object_arraydimension(pos, &pos);
	/* result */
	list_heap(ret, name, type, pos, NULL);
}

static void type_object_array(addr *ret, addr pos)
{
	type_object_arraytype(ret, CONSTANT_COMMON_ARRAY, pos);
}

static void type_object_simple_array(addr *ret, addr pos)
{
	type_object_arraytype(ret, CONSTANT_COMMON_SIMPLE_ARRAY, pos);
}

static void type_object_realtype(addr *ret, constindex index, addr pos)
{
	addr name, left1, left2, right1, right2;

	/* name */
	GetConstant(index, &name);
	/* argument */
	GetArrayType(pos, 0, &left1);
	GetArrayType(pos, 1, &left2);
	GetArrayType(pos, 2, &right1);
	GetArrayType(pos, 3, &right2);
	if (type_asterisk_p(left1) && type_asterisk_p(right1)) {
		*ret = name;
		return;
	}
	/* left */
	if (type_asterisk_p(left1))
		GetConst(COMMON_ASTERISK, &left2);
	else if (left1 == T)
		conscar_heap(&left2, left2);
	/* right */
	if (type_asterisk_p(right1))
		GetConst(COMMON_ASTERISK, &right2);
	else if (right1 == T)
		conscar_heap(&right2, right2);
	/* result */
	list_heap(ret, name, left2, right2, NULL);
}

static void type_object_real(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_REAL, pos);
}

static void type_object_rational(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_RATIONAL, pos);
}

static void type_object_integer(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_INTEGER, pos);
}

static void type_object_float(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_FLOAT, pos);
}

static void type_object_short_float(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_SHORT_FLOAT, pos);
}

static void type_object_single_float(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_SINGLE_FLOAT, pos);
}

static void type_object_double_float(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_DOUBLE_FLOAT, pos);
}

static void type_object_long_float(addr *ret, addr pos)
{
	type_object_realtype(ret, CONSTANT_COMMON_LONG_FLOAT, pos);
}

static void type_object_complex(addr *ret, addr pos)
{
	addr name;

	GetConst(COMMON_COMPLEX, &name);
	GetArrayType(pos, 0, &pos);
	if (type_asterisk_p(pos)) {
		*ret = name;
		return;
	}
	type_object(&pos, pos);
	list_heap(ret, name, pos, NULL);
}

_g void type_object(addr *ret, addr pos)
{
	type_object_call call;
	addr result, notp;

	Check(GetType(pos) != LISPTYPE_TYPE, "type error");
	call = TypeObjectTable[(int)RefLispDecl(pos)];
	call(&result, pos);
	if (RefNotDecl(pos)) {
		GetConst(COMMON_NOT, &notp);
		list_heap(ret, notp, result, NULL);
	}
	else {
		*ret = result;
	}
	Check(*ret == Unbound, "unbound error");
}

_g void init_type_object(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeObjectTable[i] = type_object_name;

	/* object */
	TypeObjectTable[LISPDECL_EMPTY] = type_object_error;
	TypeObjectTable[LISPDECL_OPTIMIZED] = type_object_optimized;
	TypeObjectTable[LISPDECL_SUBTYPEP] = type_object_subtypep;
	TypeObjectTable[LISPDECL_TYPE] = type_object_type;
	TypeObjectTable[LISPDECL_CLOS] = type_object_clos;
	TypeObjectTable[LISPDECL_ASTERISK] = type_object_name;
	/* Compound-type */
	TypeObjectTable[LISPDECL_AND] = type_object_and;
	TypeObjectTable[LISPDECL_OR] = type_object_or;
	TypeObjectTable[LISPDECL_EQL] = type_object_eql;
	TypeObjectTable[LISPDECL_MEMBER] = type_object_member;
	TypeObjectTable[LISPDECL_MOD] = type_object_mod;
	TypeObjectTable[LISPDECL_NOT] = type_object_not;
	TypeObjectTable[LISPDECL_SATISFIES] = type_object_satisfies;
	TypeObjectTable[LISPDECL_VALUES] = type_object_values;
	/* Extract-type */
	TypeObjectTable[LISPDECL_ATOM] = type_object_name;
	TypeObjectTable[LISPDECL_LIST] = type_object_name;
	TypeObjectTable[LISPDECL_BOOLEAN] = type_object_name;
	TypeObjectTable[LISPDECL_VECTOR] = type_object_vector;
	TypeObjectTable[LISPDECL_SIMPLE_VECTOR] = type_object_simple_vector;
	TypeObjectTable[LISPDECL_BIT_VECTOR] = type_object_bit_vector;
	TypeObjectTable[LISPDECL_SIMPLE_BIT_VECTOR] = type_object_simple_bit_vector;
	TypeObjectTable[LISPDECL_EXTENDED_CHAR] = type_object_name;
	TypeObjectTable[LISPDECL_STRING] = type_object_string;
	TypeObjectTable[LISPDECL_BASE_STRING] = type_object_base_string;
	TypeObjectTable[LISPDECL_SIMPLE_STRING] = type_object_simple_string;
	TypeObjectTable[LISPDECL_SIMPLE_BASE_STRING] = type_object_simple_base_string;
	TypeObjectTable[LISPDECL_SIGNED_BYTE] = type_object_signed_byte;
	TypeObjectTable[LISPDECL_UNSIGNED_BYTE] = type_object_unsigned_byte;
	TypeObjectTable[LISPDECL_BIT] = type_object_name;
	TypeObjectTable[LISPDECL_FIXNUM] = type_object_name;
	TypeObjectTable[LISPDECL_BIGNUM] = type_object_name;
	/* Atomic-type */
	TypeObjectTable[LISPDECL_NIL] = type_object_name;
	TypeObjectTable[LISPDECL_T] = type_object_name;
	TypeObjectTable[LISPDECL_NULL] = type_object_name;
	TypeObjectTable[LISPDECL_CONS] = type_object_cons;
	TypeObjectTable[LISPDECL_HASH_TABLE] = type_object_name;
	TypeObjectTable[LISPDECL_SYMBOL] = type_object_name;
	TypeObjectTable[LISPDECL_KEYWORD] = type_object_name;
	TypeObjectTable[LISPDECL_PACKAGE] = type_object_name;
	TypeObjectTable[LISPDECL_RANDOM_STATE] = type_object_name;
	TypeObjectTable[LISPDECL_READTABLE] = type_object_name;
	TypeObjectTable[LISPDECL_FUNCTION] = type_object_function;
	TypeObjectTable[LISPDECL_COMPILED_FUNCTION] = type_object_compiled_function;
	TypeObjectTable[LISPDECL_PATHNAME] = type_object_name;
	TypeObjectTable[LISPDECL_LOGICAL_PATHNAME] = type_object_name;
	TypeObjectTable[LISPDECL_SEQUENCE] = type_object_name;
	TypeObjectTable[LISPDECL_ARRAY] = type_object_array;
	TypeObjectTable[LISPDECL_SIMPLE_ARRAY] = type_object_simple_array;
	TypeObjectTable[LISPDECL_CHARACTER] = type_object_name;
	TypeObjectTable[LISPDECL_BASE_CHAR] = type_object_name;
	TypeObjectTable[LISPDECL_STANDARD_CHAR] = type_object_name;
	TypeObjectTable[LISPDECL_NUMBER] = type_object_name;
	TypeObjectTable[LISPDECL_REAL] = type_object_real;
	TypeObjectTable[LISPDECL_RATIONAL] = type_object_rational;
	TypeObjectTable[LISPDECL_RATIO] = type_object_name;
	TypeObjectTable[LISPDECL_INTEGER] = type_object_integer;
	TypeObjectTable[LISPDECL_COMPLEX] = type_object_complex;
	TypeObjectTable[LISPDECL_FLOAT] = type_object_float;
	TypeObjectTable[LISPDECL_SHORT_FLOAT] = type_object_short_float;
	TypeObjectTable[LISPDECL_SINGLE_FLOAT] = type_object_single_float;
	TypeObjectTable[LISPDECL_DOUBLE_FLOAT] = type_object_double_float;
	TypeObjectTable[LISPDECL_LONG_FLOAT] = type_object_long_float;
	TypeObjectTable[LISPDECL_RESTART] = type_object_name;
	TypeObjectTable[LISPDECL_ENVIRONMENT] = type_object_name;
	TypeObjectTable[LISPDECL_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_BROADCAST_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_CONCATENATED_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_ECHO_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_FILE_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_STRING_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_SYNONYM_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_TWO_WAY_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_PROMPT_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_PRETTY_STREAM] = type_object_name;
	TypeObjectTable[LISPDECL_QUOTE] = type_object_error;
	TypeObjectTable[LISPDECL_BYTESPEC] = type_object_name;
	TypeObjectTable[LISPDECL_PRINT_DISPATCH] = type_object_name;
}

