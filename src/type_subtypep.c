#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "equal.h"
#include "gc.h"
#include "integer.h"
#include "object.h"
#include "sequence.h"
#include "rational.h"
#include "real.h"
#include "symbol.h"
#include "type.h"
#include "type_copy.h"
#include "type_parse.h"
#include "type_number.h"
#include "type_range.h"
#include "type_subtypep.h"
#include "type_table.h"
#include "type_typep.h"
#include "type_upgraded.h"

typedef SubtypepResult (*call_type_subtypep)(addr left, addr right);
static call_type_subtypep TypeSubtypep[LISPDECL_SIZE];
static SubtypepResult subtypep_call(addr left, addr right, int asterisk);

#define ReturnInvalid { \
	return SUBTYPEP_INVALID; \
}
#define ReturnTrue { \
	return SUBTYPEP_INCLUDE; \
}
#define ReturnFalse { \
	return SUBTYPEP_FALSE; \
}
#define ReturnExclude { \
	return SUBTYPEP_EXCLUDE; \
}
#define ReturnBool(p) { \
	if (p) return SUBTYPEP_INCLUDE; \
	else return SUBTYPEP_FALSE; \
}
#define ReturnThrowBool(v) { \
	switch (v) { \
		case SUBTYPEP_INVALID: ReturnInvalid; \
		case SUBTYPEP_INCLUDE: break; \
		default: ReturnFalse; \
	} \
}
#define ReturnTrueNil(v) { \
	switch (v) { \
		case SUBTYPEP_INVALID: ReturnInvalid; \
		case SUBTYPEP_INCLUDE: ReturnTrue; \
		default: ReturnFalse; \
	} \
}


/*
 *  subtypep-table
 */
static SubtypepResult subtypep_type(addr left, addr right)
{
	if (RefLispDecl(left) != LISPDECL_TYPE) ReturnExclude;
	ReturnBool(RefLispDecl(left) == RefLispDecl(right));
}

static SubtypepResult subtypep_clos(addr left, addr right)
{
	if (RefLispDecl(left) != LISPDECL_CLOS) ReturnExclude;
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	if (type_asterisk_p(right))
		ReturnTrue;
	if (type_asterisk_p(left))
		ReturnFalse;

	/*
	 *  TODO: error
	 *  (defclass aaa () ())
	 *  (defclass bbb () ())
	 *  (subtypep 'aaa 'bbb) -> nil; t
	 *  (subtypep 'aaa '(not bbb))
	 *    -> nil; nil  (correct)
	 *    -> t; t  (incorrect, this implementation.)
	 */
	ReturnBool(clos_subclass_p(left, right));
}

static SubtypepResult subtypep_error(addr left, addr right)
{
	infoprint(left);
	infoprint(right);
	fmte("Invalid subtypep argument.", NULL);
	ReturnInvalid;
}

static SubtypepResult subtypep_equaltype(addr left, addr right)
{
	if (RefLispDecl(left) == RefLispDecl(right)) ReturnTrue;
	ReturnExclude;
}

static SubtypepResult subtypep_nil(addr left, addr right)
{
	ReturnExclude;
}

static SubtypepResult subtypep_t(addr left, addr right)
{
	ReturnTrue;
}

static int asterisk_or_t(addr pos)
{
	enum LISPDECL decl;
	GetLispDecl(pos, &decl);
	return decl == LISPDECL_ASTERISK || decl == LISPDECL_T;
}

static SubtypepResult subtypep_asterisk_or_t(addr left, addr right)
{
	if (asterisk_or_t(right)) ReturnTrue;
	if (asterisk_or_t(left)) ReturnFalse;
	return subtypep_result(left, right, 0);
}

static SubtypepResult subtypep_cons(addr left, addr right)
{
	addr car1, car2, cdr1, cdr2;

	if (RefLispDecl(left) != LISPDECL_CONS) ReturnExclude;
	GetArrayType(left, 0, &car1);
	GetArrayType(left, 1, &cdr1);
	GetArrayType(right, 0, &car2);
	GetArrayType(right, 1, &cdr2);

	ReturnThrowBool(subtypep_asterisk_or_t(car1, car2));
	ReturnTrueNil(subtypep_asterisk_or_t(cdr1, cdr2));
}

static SubtypepResult subtypep_symbol(addr left, addr right)
{
	enum LISPDECL type = RefLispDecl(left);
	if (type == LISPDECL_NULL
			|| type == LISPDECL_SYMBOL
			|| type == LISPDECL_KEYWORD) {
		ReturnTrue;
	}
	ReturnExclude;
}

static SubtypepResult subtypep_keyword(addr left, addr right)
{
	enum LISPDECL type = RefLispDecl(left);
	if (type == LISPDECL_KEYWORD) ReturnTrue;
	if (type == LISPDECL_SYMBOL) ReturnFalse;
	ReturnExclude;
}

static SubtypepResult subtypep_pathname(addr left, addr right)
{
	enum LISPDECL type = RefLispDecl(left);
	if (type == LISPDECL_PATHNAME || type == LISPDECL_LOGICAL_PATHNAME) ReturnTrue;
	ReturnExclude;
}

static SubtypepResult subtypep_logical_pathname(addr left, addr right)
{
	enum LISPDECL type = RefLispDecl(left);
	if (type == LISPDECL_LOGICAL_PATHNAME) ReturnTrue;
	if (type == LISPDECL_PATHNAME) ReturnFalse;
	ReturnExclude;
}

static int subtypep_array_sequence(addr left)
{
	enum LISPTYPE type;
	size_t size;

	GetArrayType(left, 1, &left);
	if (type_asterisk_p(left)) {
		return 0;
	}
	type = GetType(left);
	if (type == LISPTYPE_FIXNUM) {
		return RefFixnum(left) == 1;
	}
	if (type == LISPTYPE_VECTOR) {
		LenArrayA4(left, &size);
		return size == 1;
	}

	return 0;
}

static SubtypepResult subtypep_sequence(addr left, addr right)
{
	enum LISPDECL type = RefLispDecl(left);

	if (type == LISPDECL_ARRAY || type == LISPDECL_SIMPLE_ARRAY) {
		ReturnBool(subtypep_array_sequence(left));
	}
	if (type == LISPDECL_SEQUENCE
			|| type == LISPDECL_NULL
			|| type == LISPDECL_CONS) {
		ReturnTrue;
	}

	ReturnExclude;
}

static SubtypepResult array_array_integer(addr left, addr right)
{
	size_t size;

	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			ReturnBool(RefFixnum(left) == RefFixnum(right));

		case LISPTYPE_VECTOR:
			Check(GetStatusSize(left) != LISPSIZE_ARRAY4, "size left error");
			LenArrayA4(left, &size);
			ReturnBool(((fixnum)size) == RefFixnum(right));

		default:
			break;
	}
	ReturnFalse;
}

static SubtypepResult array_array_vector(addr left, addr right)
{
	addr check1, check2;
	size_t i, size;

	if (GetType(left) != LISPTYPE_VECTOR) ReturnFalse;
	Check(GetStatusSize(right) != LISPSIZE_ARRAY4, "size right error");
	Check(GetStatusSize(left) != LISPSIZE_ARRAY4, "size left error");
	LenArrayA4(right, &i);
	LenArrayA4(left, &size);
	if (size != i) ReturnFalse;
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &check2);
		GetArrayA4(left, i, &check1);
		if (type_asterisk_p(check2)) continue;
		if (type_asterisk_p(check1)) ReturnFalse;
		Check(GetType(check2) != LISPTYPE_FIXNUM, "fixnum right error");
		Check(GetType(check1) != LISPTYPE_FIXNUM, "fixnum left error");
		if (RefFixnum(check1) != RefFixnum(check2)) ReturnFalse;
	}
	ReturnTrue;
}

static SubtypepResult subtypep_array_array_dimension(addr left, addr right)
{
	if (type_asterisk_p(right)) ReturnTrue;
	if (type_asterisk_p(left)) ReturnFalse;
	if (GetType(right) == LISPTYPE_FIXNUM)
		return array_array_integer(left, right);
	if (GetType(right) == LISPTYPE_VECTOR)
		return array_array_vector(left, right);
	Abort("type error");
	ReturnInvalid;
}

static int equal_array_type_asterisk(addr left, addr right)
{
	if (type_asterisk_p(right)) return 1;
	if (type_asterisk_p(left)) return 0;
	return upgraded_array0_equal(left, right);
}

static SubtypepResult subtypep_array_array(addr left, addr right)
{
	addr check1, check2;

	/* type */
	GetArrayType(left, 0, &check1);
	GetArrayType(right, 0, &check2);
	if (! equal_array_type_asterisk(check1, check2)) ReturnFalse;

	/* dimension */
	GetArrayType(left, 1, &left);
	GetArrayType(right, 1, &right);
	ReturnTrueNil(subtypep_array_array_dimension(left, right));
}

static SubtypepResult subtypep_array(addr left, addr right)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_ARRAY:
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_array_array(left, right);

		case LISPDECL_SEQUENCE:
			ReturnFalse;

		default:
			break;
	}

	ReturnExclude;
}

static SubtypepResult subtypep_simple_array(addr left, addr right)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_array_array(left, right);

		case LISPDECL_ARRAY:
		case LISPDECL_SEQUENCE:
			ReturnFalse;

		default:
			break;
	}

	ReturnExclude;
}

static SubtypepResult subtypep_character(addr left, addr right)
{
	enum LISPDECL type = RefLispDecl(left);
	if (type == LISPDECL_CHARACTER ||
			type == LISPDECL_BASE_CHAR ||
			type == LISPDECL_STANDARD_CHAR) {
		ReturnTrue;
	}
	ReturnExclude;
}

static SubtypepResult subtypep_base_char(addr left, addr right)
{
	enum LISPDECL decl = RefLispDecl(left);
	if (decl == LISPDECL_BASE_CHAR || decl == LISPDECL_STANDARD_CHAR) ReturnTrue;
	if (decl == LISPDECL_CHARACTER) ReturnFalse;
	ReturnExclude;
}

static SubtypepResult subtypep_standard_char(addr left, addr right)
{
	enum LISPDECL decl = RefLispDecl(left);
	if (decl == LISPDECL_STANDARD_CHAR) ReturnTrue;
	if (decl == LISPDECL_BASE_CHAR || decl == LISPDECL_CHARACTER) ReturnFalse;
	ReturnExclude;
}

static int subtypep_real_less(addr left, addr right)
{
	return range_any_right_p(left) && range_right_right_less_equal(left, right);
}

static int subtypep_real_greater(addr left, addr right)
{
	return range_left_any_p(left) && range_left_left_greater_equal(left, right);
}

static int subtypep_real_range(addr left, addr right)
{
	return range_between_p(left) && range_in_between(left, right);
}

static int subtypep_realcheck(addr left, addr right)
{
	addr check1, check2;

	if (range_asterisk_p(right)) return 1;
	if (range_asterisk_p(left))  return 0;
	GetArrayType(right, 0, &check1);
	GetArrayType(right, 2, &check2);
	if (type_asterisk_p(check1))
		return subtypep_real_less(left, right);
	if (type_asterisk_p(check2))
		return subtypep_real_greater(left, right);
	else
		return subtypep_real_range(left, right);
}

static int realexclude_left(addr left, addr right)
{
	return range_any_right_p(left) &&
		range_left_any_p(right) &&
		range_right_left_less(left, right);
}

static int realexclude_right(addr left, addr right)
{
	return range_left_any_p(left) &&
		range_any_right_p(right) &&
		range_left_right_greater(left, right);
}

static int subtypep_realexlucde(addr left, addr right)
{
	return
		realexclude_left(left, right) ||
		realexclude_right(left, right);
}

static SubtypepResult subtypep_realparameter(addr left, addr right)
{
	if (subtypep_realcheck(left, right)) ReturnTrue;
	if (subtypep_realexlucde(left, right)) ReturnExclude;
	ReturnFalse;
}

static SubtypepResult subtypep_integer(addr left, addr right)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	if (type == LISPDECL_RATIONAL ||
			type == LISPDECL_REAL ||
			type == LISPDECL_NUMBER) {
		ReturnFalse;
	}
	if (type == LISPDECL_INTEGER) {
		return subtypep_realparameter(left, right);
	}
	ReturnExclude;
}

static SubtypepResult subtypep_rational(addr left, addr right)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	if (type == LISPDECL_REAL || type == LISPDECL_NUMBER) {
		ReturnFalse;
	}
	if (type == LISPDECL_RATIO) {
		ReturnBool(range_asterisk_p(right));
	}
	if (type == LISPDECL_RATIONAL || type == LISPDECL_INTEGER) {
		return subtypep_realparameter(left, right);
	}
	ReturnExclude;
}

static SubtypepResult subtypep_real(addr left, addr right)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	if (type == LISPDECL_NUMBER) {
		ReturnFalse;
	}
	if (type == LISPDECL_RATIO) {
		ReturnBool(range_asterisk_p(right));
	}
	if (decl_range_p(type)) {
		return subtypep_realparameter(left, right);
	}
	ReturnExclude;
}

static SubtypepResult subtypep_number(addr left, addr right)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	if (type == LISPDECL_NUMBER ||
			type == LISPDECL_COMPLEX ||
			type == LISPDECL_RATIO ||
			decl_range_p(type)) {
		ReturnTrue;
	}
	ReturnExclude;
}

static SubtypepResult subtypep_float(addr left, addr right)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	if (type == LISPDECL_NUMBER || type == LISPDECL_REAL) {
		ReturnFalse;
	}
	if (decl_float_p(type)) {
		return subtypep_realparameter(left, right);
	}
	ReturnExclude;
}

static SubtypepResult subtypep_float_type(addr left, addr right, enum LISPDECL check)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	if (type == LISPDECL_NUMBER || type == LISPDECL_REAL || type == LISPDECL_FLOAT) {
		ReturnFalse;
	}
	if (type == check) {
		return subtypep_realparameter(left, right);
	}
	ReturnExclude;
}
static SubtypepResult subtypep_short_float(addr left, addr right)
{
	return subtypep_float_type(left, right, LISPDECL_SHORT_FLOAT);
}
static SubtypepResult subtypep_single_float(addr left, addr right)
{
	return subtypep_float_type(left, right, LISPDECL_SINGLE_FLOAT);
}
static SubtypepResult subtypep_double_float(addr left, addr right)
{
	return subtypep_float_type(left, right, LISPDECL_DOUBLE_FLOAT);
}
static SubtypepResult subtypep_long_float(addr left, addr right)
{
	return subtypep_float_type(left, right, LISPDECL_LONG_FLOAT);
}

static SubtypepResult subtypep_ratio(addr left, addr right)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	if (type == LISPDECL_NUMBER ||
			type == LISPDECL_REAL ||
			type == LISPDECL_RATIONAL) {
		ReturnFalse;
	}
	if (type == LISPDECL_RATIO) {
		ReturnTrue;
	}
	ReturnExclude;
}

static SubtypepResult subtypep_complex_value(addr left, addr right)
{
	int check;

	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	check = RefLispDecl(left) == RefLispDecl(right) &&
		RefNotDecl(left) == RefNotDecl(right);
	ReturnBool(check);
}

static SubtypepResult subtypep_complex(addr left, addr right)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	if (type == LISPDECL_NUMBER) {
		ReturnFalse;
	}
	if (type == LISPDECL_COMPLEX) {
		return subtypep_complex_value(left, right);
	}
	ReturnExclude;
}

static SubtypepResult subtypep_table(addr left, addr right)
{
	call_type_subtypep call;

	call = TypeSubtypep[(int)RefLispDecl(right)];
	Check(call == NULL, "call error");
	return call(left, right);
}


/*
 *  function
 */
struct ordinary_args {
	addr var, opt, rest, key;
	size_t size, size_var, size_opt, size_key, pos_rest;
};
typedef struct ordinary_args ordargs;

static void make_function_ordinary(ordargs *ptr, addr pos)
{
	int check1, check2;

	clearpoint(ptr);
	GetArrayA2(pos, 0, &(ptr->var));
	GetArrayA2(pos, 1, &(ptr->opt));
	GetArrayA2(pos, 2, &(ptr->rest));
	GetArrayA2(pos, 3, &(ptr->key));
	ptr->size_var = length_list_unsafe(ptr->var);
	ptr->size_opt = length_list_unsafe(ptr->opt);
	ptr->size_key = (ptr->key == T)? 0: length_list_unsafe(ptr->key);
	ptr->pos_rest = ptr->size = ptr->size_var + ptr->size_opt;
	check1 = (ptr->rest != Nil);
	check2 = (ptr->key != Nil);
	if (check2) /* (rest key) or (key) */
		ptr->size += 2;
	else if (check1) /* (rest) */
		ptr->size++;
}

struct ordinary_type {
	addr type;
	unsigned nil : 1;
	unsigned var : 1;
	unsigned rest : 1;
	unsigned key : 1;
	unsigned value : 1;
};
typedef struct ordinary_type ordtype;

static void gettype_ordinary(const ordargs *ptr, size_t index, ordtype *ret)
{
	int check1, check2;

	memset(ret, 0, sizeoft(ordtype));
	ret->type = Nil;
	/* var */
	if (index < ptr->size_var) {
		getnth(ptr->var, index, &(ret->type));
		ret->var = 1;
		return;
	}
	index -= ptr->size_var;
	/* opt */
	if (index < ptr->size_opt) {
		getnth(ptr->opt, index, &(ret->type));
		ret->var = 1;
		return;
	}
	index -= ptr->size_opt;
	/* rest */
	check1 = (ptr->rest == Nil);
	check2 = (ptr->key == Nil);
	if (check1 && check2) {
		ret->nil = 1;
		return;
	}
	if (! check1) {
		ret->type = ptr->rest;
		ret->var = 1;
		ret->rest = 1;
	}
	/* key */
	if (! check2) {
		if ((index % 2) == 0) {
			ret->key = 1;
			ret->value = 0;
		}
		else {
			ret->key = 0;
			ret->value = 1;
		}
	}
}

static int ordargs_simple_p(const ordargs *ptr)
{
	return ptr->rest == Nil && ptr->key == Nil;
}

static void ordinary_keytype(LocalRoot local, addr *ret, const ordargs *ptr)
{
	addr cons, array, pos;
	size_t size, i;

	/* &allow-other-keys */
	if (ptr->key == T) {
		GetTypeTable(ret, Symbol);
		return;
	}

	/* (eql key) */
	cons = ptr->key;
	if (singlep(cons)) {
		GetCar(cons, &pos);
		GetCar(pos, &pos);
		type_eql_local(local, pos, ret);
		return;
	}

	/* (or (eql key1) (eql key2) ...) */
	size = length_list_unsafe(cons);
	vector4_alloc(local, &array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		GetCar(pos, &pos);
		type_eql_local(local, pos, &pos);
		SetArrayA4(array, i, pos);
	}
	type1_local(local, LISPDECL_OR, array, ret);
}

static void ordinary_valuetype(LocalRoot local, addr *ret, const ordargs *ptr)
{
	addr cons, array, pos;
	size_t size, i;

	/* &allow-other-keys */
	if (ptr->key == T) {
		GetTypeTable(ret, T);
		return;
	}

	/* type */
	cons = ptr->key;
	if (singlep(cons)) {
		GetCar(cons, &pos);
		GetCdr(pos, ret);
		return;
	}

	/* (or type1 type2 ...) */
	size = length_list_unsafe(cons);
	vector4_alloc(local, &array, size);
	for (i = 0; i < size; i++) {
		GetCons(cons, &pos, &cons);
		GetCdr(pos, &pos);
		SetArrayA4(array, i, pos);
	}
	type1_local(local, LISPDECL_OR, array, ret);
}

static void make_ordinary_type(LocalRoot local, addr *ret,
		const ordargs *ptr, const ordtype *type)
{
	addr pos;

	/* var        -> type */
	/* rest       -> type */
	/* key        -> (and key1 key2 ...) */
	/* rest + key -> (or rest (and key1 key2 ...)) */
	/* &allow-other-keys -> (symbol t) */
	Check(type->nil, "nil error");

	/* var only */
	if (type->var && (! type->rest)) {
		*ret = type->type;
		return;
	}

	/* rest only */
	if (type->rest && (! type->key) && (! type->value)) {
		*ret = type->type;
		return;
	}

	/* key */
	if (type->key) {
		ordinary_keytype(local, &pos, ptr);
		if (type->rest)
			type2and_local(local, type->type, pos, &pos);
		*ret = pos;
		return;
	}

	/* value */
	if (type->value) {
		ordinary_valuetype(local, &pos, ptr);
		if (type->rest)
			type2and_local(local, type->type, pos, &pos);
		*ret = pos;
		return;
	}

	/* error */
	*ret = 0;
	Abort("type error");
}

static int ordinary_subtypep(
		const ordargs *ptr1, const ordtype *type1,
		const ordargs *ptr2, const ordtype *type2)
{
	SubtypepResult result;
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	make_ordinary_type(local, &left, ptr1, type1);
	make_ordinary_type(local, &right, ptr2, type2);
	result = subtypep_result(left, right, 1);
	rollback_local(local, stack);

	return result == SUBTYPEP_INCLUDE;
}

static int ordinary_size(const ordargs *ptr1, const ordargs *ptr2, size_t size)
{
	size_t i;
	ordtype type1, type2;

	for (i = 0; i < size; i++) {
		gettype_ordinary(ptr1, i, &type1);
		gettype_ordinary(ptr2, i, &type2);
		if (type1.nil) break;
		if (type2.nil) return 0;
		if (! ordinary_subtypep(ptr1, &type1, ptr2, &type2))
			return 0;
	}

	return 1;
}

static int ordinary_simple(const ordargs *ptr1, const ordargs *ptr2)
{
	if (ptr1->size > ptr2->size) return 0;
	/* short size */
	return ordinary_size(ptr1, ptr2, ptr1->size);
}

static int ordinary_simple_left(const ordargs *ptr1, const ordargs *ptr2)
{
	/* short size */
	return ordinary_size(ptr1, ptr2, ptr1->size);
}

static int ordinary_check(const ordargs *ptr1, const ordargs *ptr2)
{
	/* long size */
	return ordinary_size(ptr1, ptr2,
			(ptr1->size > ptr2->size? ptr1->size: ptr2->size));
}

static int subtypep_function_ordinary(addr left, addr right)
{
	int check1, check2;
	ordargs ptr1, ptr2;

	/* asterisk */
	if (type_asterisk_p(right)) return 1;
	if (type_asterisk_p(left)) return 0;

	/* list */
	make_function_ordinary(&ptr1, left);
	make_function_ordinary(&ptr2, right);
	if (ptr1.size_var < ptr2.size_var) return 0;

	check1 = ordargs_simple_p(&ptr1);
	check2 = ordargs_simple_p(&ptr2);
	if (check1 && check2)
		return ordinary_simple(&ptr1, &ptr2);
	if (check1)
		return ordinary_simple_left(&ptr1, &ptr2);
	if (check2)
		return 0;
	else
		return ordinary_check(&ptr1, &ptr2);
}

static SubtypepResult subtypep_function_check(addr left, addr right)
{
	addr check1, check2;

	/* lambda-list */
	GetArrayType(left, 0, &check1);
	GetArrayType(right, 0, &check2);
	if (! subtypep_function_ordinary(check1, check2)) ReturnFalse;

	/* values */
	GetArrayType(left, 1, &left);
	GetArrayType(right, 1, &right);
	ReturnTrueNil(subtypep_result(left, right, 1));
}

static SubtypepResult subtypep_function(addr left, addr right)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	if (type != LISPDECL_FUNCTION && type != LISPDECL_COMPILED_FUNCTION) {
		ReturnExclude;
	}

	return subtypep_function_check(left, right);
}

static SubtypepResult subtypep_compiled_function(addr left, addr right)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	if (type == LISPDECL_FUNCTION) {
		ReturnFalse;
	}
	if (type != LISPDECL_COMPILED_FUNCTION) {
		ReturnExclude;
	}

	return subtypep_function_check(left, right);
}

static SubtypepResult subtypep_stream(addr left, addr right)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_STREAM:
		case LISPDECL_BROADCAST_STREAM:
		case LISPDECL_CONCATENATED_STREAM:
		case LISPDECL_ECHO_STREAM:
		case LISPDECL_FILE_STREAM:
		case LISPDECL_STRING_STREAM:
		case LISPDECL_SYNONYM_STREAM:
		case LISPDECL_TWO_WAY_STREAM:
		case LISPDECL_PROMPT_STREAM:
		case LISPDECL_PRETTY_STREAM:
			ReturnTrue;
			break;

		default:
			ReturnExclude;
			break;
	}
}

static SubtypepResult subtypep_stream_child(addr left, addr right)
{
	enum LISPDECL type1, type2;

	GetLispDecl(left, &type1);
	GetLispDecl(right, &type2);
	if (type1 == type2) {
		ReturnTrue;
	}
	if (type1 == LISPDECL_STREAM) {
		ReturnFalse;
	}

	ReturnExclude;
}


/*
 *  subtypep_lisptype
 */
static SubtypepResult subtypep_lisptype_normal(addr left, addr right,
		call_type_subtypep call)
{
	int not2;
	SubtypepResult result;

	result = call(left, right);
	GetNotDecl(right, &not2);
	switch (result) {
		case SUBTYPEP_INCLUDE:
			return not2? SUBTYPEP_EXCLUDE: SUBTYPEP_INCLUDE;

		case SUBTYPEP_EXCLUDE:
			return not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE;

		default:
			return result;
	}
}

static SubtypepResult subtypep_lisptype_not(addr left, addr right,
		call_type_subtypep call)
{
	int not2;
	SubtypepResult result;

	result = call(right, left);  /* reverse */
	GetNotDecl(right, &not2);
	switch (result) {
		case SUBTYPEP_INCLUDE:
			return not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE;

		case SUBTYPEP_EXCLUDE:
			return SUBTYPEP_FALSE;

		default:
			return result;
	}
}

static SubtypepResult subtypep_lisptype(addr left, addr right,
		call_type_subtypep call)
{
	if (RefNotDecl(left))
		return subtypep_lisptype_not(left, right, call);
	else
		return subtypep_lisptype_normal(left, right, call);
}


/*
 *  subtypep_eql
 */
static SubtypepResult subtypep_eql_eql(addr left, addr right)
{
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	if (eql_function(left, right)) ReturnTrue;
	ReturnExclude;
}

static SubtypepResult subtypep_eql_type(addr left, addr right)
{
	int check;

	/* (subtypep '(eql x) '(satisfies y)) */
	type_getvalues1(right, &right);
	if (RefLispDecl(right) == LISPDECL_SATISFIES) {
		ReturnInvalid;
	}

	/* (subtypep '(eql x) right) */
	GetArrayType(left, 0, &left);
	if (typep_table(NULL, left, right, &check)) {
		ReturnInvalid;
	}
	if (check) {
		ReturnTrue;
	}
	else {
		ReturnExclude;
	}
}

static SubtypepResult subtypep_type_eql(addr left, addr right)
{
	int check;

	/* (subtypep '(satisfies x) '(eql y)) */
	type_getvalues1(left, &left);
	if (RefLispDecl(left) == LISPDECL_SATISFIES) {
		ReturnInvalid;
	}

	/* (subtypep left '(eql x)) */
	GetArrayType(right, 0, &right);
	if (typep_table(NULL, right, left, &check)) {
		ReturnInvalid;
	}
	if (check) {
		ReturnFalse;
	}
	else {
		ReturnExclude;
	}
}

static SubtypepResult subtypep_eql_call(addr left, addr right)
{
	int check1, check2;

	check1 = (RefLispDecl(left) == LISPDECL_EQL);
	check2 = (RefLispDecl(right) == LISPDECL_EQL);
	if (check1 && check2)
		return subtypep_eql_eql(left, right);
	if (check1)
		return subtypep_eql_type(left, right);
	if (check2)
		return subtypep_type_eql(left, right);
	Abort("type error");
	ReturnInvalid;
}

static SubtypepResult subtypep_eql(addr left, addr right)
{
	return subtypep_lisptype(left, right, subtypep_eql_call);
}


/*
 *  subtypep_values
 */
static size_t getsize_values(addr pos)
{
	addr check;
	size_t size;

	/* var */
	GetArrayType(pos, 0, &check);
	size = length_list_unsafe(check);
	/* opt */
	GetArrayType(pos, 1, &check);
	size += length_list_unsafe(check);

	return size;
}

static void gettype_values(addr pos, size_t index, addr *ret)
{
	addr check;
	size_t size;

	/* var */
	GetArrayType(pos, 0, &check);
	size = length_list_unsafe(check);
	if (index < size) {
		getnth(check, index, ret);
		return;
	}
	index -= size;

	/* opt */
	GetArrayType(pos, 1, &check);
	size = length_list_unsafe(check);
	if (index < size) {
		getnth(check, index, ret);
		return;
	}

	/* rest */
	GetArrayType(pos, 2, ret);
}

static int subtypep_boolean(addr left, addr right)
{
	return subtypep_result(left, right, 1) == SUBTYPEP_INCLUDE;
}

static int subtypep_values_values(addr left, addr right)
{
	addr check1, check2;
	size_t size1, size2, size, i;

	Check(RefLispDecl(left) != LISPDECL_VALUES, "decl left error");
	Check(RefLispDecl(right) != LISPDECL_VALUES, "decl right error");
	Check(RefNotDecl(left), "left not error");
	Check(RefNotDecl(right), "right not error");

	/* size */
	size1 = getsize_values(left);
	size2 = getsize_values(right);
	size = (size1 > size2)? size1: size2;
	size++; /* &rest check */

	/* check */
	for (i = 0; i < size; i++) {
		gettype_values(left, i, &check1);
		gettype_values(right, i, &check2);
		if (! subtypep_boolean(check1, check2))
			return 0;
	}

	return 1;
}

static int subtypep_values_type(addr left, addr right)
{
	Check(RefLispDecl(left) != LISPDECL_VALUES, "decl left error");
	Check(RefNotDecl(left), "left not error");
	type_getvalues1(left, &left);
	return subtypep_boolean(left, right);
}

static int subtypep_type_values(addr left, addr right)
{
	Check(RefLispDecl(right) != LISPDECL_VALUES, "decl right error");
	Check(RefNotDecl(right), "right not error");
	type_getvalues1(right, &right);
	return subtypep_boolean(left, right);
}

static int subtypep_values_call(addr left, addr right)
{
	int check1, check2;

	check1 = (RefLispDecl(left) == LISPDECL_VALUES);
	check2 = (RefLispDecl(right) == LISPDECL_VALUES);
	if (check1 && check2)
		return subtypep_values_values(left, right);
	if (check1)
		return subtypep_values_type(left, right);
	if (check2)
		return subtypep_type_values(left, right);
	Abort("type error");
	ReturnInvalid;
}

static SubtypepResult subtypep_values(addr left, addr right)
{
	int result;

	/*
	 *  typespec values cannot recognize subtypep-exclude.
	 *  result is include or false.
	 */
	result = subtypep_values_call(left, right);
	return result? SUBTYPEP_INCLUDE: SUBTYPEP_FALSE;
}


/*
 *  subtypep_call
 */
static SubtypepResult subtypep_leftright(addr left, addr right)
{
	return subtypep_lisptype(left, right, subtypep_table);
}


/* left */
static SubtypepResult subtypep_and_left(addr left, addr right)
{
	int exclude;
	SubtypepResult result;
	addr check;
	size_t size, i;

	GetArrayType(left, 0, &left);
	LenArrayA4(left, &size);
	exclude = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(left, i, &check);
		result = subtypep_call(check, right, 0);
		if (result == SUBTYPEP_INCLUDE || result == SUBTYPEP_INVALID) return result;
		if (result == SUBTYPEP_EXCLUDE) exclude = 1;
	}
	if (exclude) ReturnExclude;
	ReturnFalse;
}

static SubtypepResult subtypep_or_left(addr left, addr right)
{
	int include, exclude;
	SubtypepResult result;
	addr check;
	size_t size, i;

	GetArrayType(left, 0, &left);
	LenArrayA4(left, &size);
	include = 1;
	exclude = 1;
	for (i = 0; i < size; i++) {
		GetArrayA4(left, i, &check);
		result = subtypep_call(check, right, 0);
		if (result == SUBTYPEP_INVALID) return result;
		if (result != SUBTYPEP_INCLUDE) include = 0;
		if (result != SUBTYPEP_EXCLUDE) exclude = 0;
	}
	if (include) ReturnTrue;
	if (exclude) ReturnExclude;
	ReturnFalse;
}

static SubtypepResult subtypep_satisfies_left(addr left, addr right)
{
	if (RefLispDecl(right) == LISPDECL_T) ReturnTrue;
	ReturnInvalid;
}

static SubtypepResult subtypep_left(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_TYPE, "type left error");
	switch (RefLispDecl(left)) {
		case LISPDECL_AND:
			return subtypep_and_left(left, right);

		case LISPDECL_OR:
			return subtypep_or_left(left, right);

		case LISPDECL_EQL:
			return subtypep_eql(left, right);

		case LISPDECL_MEMBER:
			fmte("The member type illegal in this context.", NULL);
			break;

		case LISPDECL_NOT:
			fmte("The not type illegal in this context.", NULL);
			break;

		case LISPDECL_VALUES:
			return subtypep_values(left, right);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_left(left, right);

		case LISPDECL_NIL:
			ReturnTrue;

		case LISPDECL_T:
			ReturnFalse;

		default:
			return subtypep_leftright(left, right);
	}

	ReturnInvalid;
}

/* right */
static SubtypepResult subtypep_andargs_right(addr left, addr right)
{
	int include, exclude;
	SubtypepResult result;
	addr check;
	size_t size, i;

	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	include = 1;
	exclude = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &check);
		result = subtypep_call(left, check, 0);
		if (result == SUBTYPEP_INVALID) return result;
		if (result != SUBTYPEP_INCLUDE) include = 0;
		if (result == SUBTYPEP_EXCLUDE) exclude = 1;
	}
	if (include) ReturnTrue;
	if (exclude) ReturnExclude;
	ReturnFalse;
}

static SubtypepResult subtypep_orargs_right(addr left, addr right)
{
	int exclude;
	SubtypepResult result;
	addr check;
	size_t size, i;

	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	exclude = 1;
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &check);
		result = subtypep_call(left, check, 0);
		if (result == SUBTYPEP_INCLUDE || result == SUBTYPEP_INVALID) return result;
		if (result != SUBTYPEP_EXCLUDE) exclude = 0;
	}
	if (exclude) ReturnExclude;
	ReturnFalse;
}

static SubtypepResult subtypep_and_right(addr left, addr right)
{
	if (RefLispDecl(left) == LISPDECL_OR)
		return subtypep_or_left(left, right);
	else
		return subtypep_andargs_right(left, right);
}

static SubtypepResult subtypep_or_right(addr left, addr right)
{
	if (RefLispDecl(left) == LISPDECL_OR)
		return subtypep_or_left(left, right);
	else
		return subtypep_orargs_right(left, right);
}

static SubtypepResult subtypep_satisfies_right(addr left, addr right)
{
	if (RefLispDecl(left) == LISPDECL_NIL) ReturnTrue;
	ReturnInvalid;
}

static SubtypepResult subtypep_nil_right(addr left)
{
	if (RefLispDecl(left) == LISPDECL_NIL) ReturnTrue;
	ReturnExclude;
}

static SubtypepResult subtypep_right(addr left, addr right)
{
	Check(GetType(right) != LISPTYPE_TYPE, "type right error");
	switch (RefLispDecl(right)) {
		case LISPDECL_AND:
			return subtypep_and_right(left, right);

		case LISPDECL_OR:
			return subtypep_or_right(left, right);

		case LISPDECL_EQL:
			return subtypep_eql(left, right);

		case LISPDECL_MEMBER:
			fmte("The member type illegal in this context.", NULL);
			break;

		case LISPDECL_NOT:
			fmte("The not type illegal in this context.", NULL);
			break;

		case LISPDECL_VALUES:
			return subtypep_values(left, right);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_right(left, right);

		case LISPDECL_NIL:
			return subtypep_nil_right(left);

		case LISPDECL_T:
			ReturnTrue;

		default:
			return subtypep_left(left, right);
	}

	ReturnInvalid;
}


/*
 *  subtypep_clang
 */
_g void init_type_subtypep(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeSubtypep[i] = subtypep_error;

	TypeSubtypep[LISPDECL_TYPE] = subtypep_type;
	TypeSubtypep[LISPDECL_CLOS] = subtypep_clos;
	TypeSubtypep[LISPDECL_ASTERISK] = subtypep_error;
	TypeSubtypep[LISPDECL_NIL] = subtypep_nil;
	TypeSubtypep[LISPDECL_T] = subtypep_t;
	TypeSubtypep[LISPDECL_NULL] = subtypep_equaltype;
	TypeSubtypep[LISPDECL_CONS] = subtypep_cons;
	TypeSubtypep[LISPDECL_HASH_TABLE] = subtypep_equaltype;
	TypeSubtypep[LISPDECL_SYMBOL] = subtypep_symbol;
	TypeSubtypep[LISPDECL_KEYWORD] = subtypep_keyword;
	TypeSubtypep[LISPDECL_PACKAGE] = subtypep_equaltype;
	TypeSubtypep[LISPDECL_RANDOM_STATE] = subtypep_equaltype;
	TypeSubtypep[LISPDECL_READTABLE] = subtypep_equaltype;
	TypeSubtypep[LISPDECL_FUNCTION] = subtypep_function;
	TypeSubtypep[LISPDECL_COMPILED_FUNCTION] = subtypep_compiled_function;
	TypeSubtypep[LISPDECL_PATHNAME] = subtypep_pathname;
	TypeSubtypep[LISPDECL_LOGICAL_PATHNAME] = subtypep_logical_pathname;
	TypeSubtypep[LISPDECL_SEQUENCE] = subtypep_sequence;
	TypeSubtypep[LISPDECL_ARRAY] = subtypep_array;
	TypeSubtypep[LISPDECL_SIMPLE_ARRAY] = subtypep_simple_array;
	TypeSubtypep[LISPDECL_CHARACTER] = subtypep_character;
	TypeSubtypep[LISPDECL_BASE_CHAR] = subtypep_base_char;
	TypeSubtypep[LISPDECL_STANDARD_CHAR] = subtypep_standard_char;
	TypeSubtypep[LISPDECL_INTEGER] = subtypep_integer;
	TypeSubtypep[LISPDECL_RATIONAL] = subtypep_rational;
	TypeSubtypep[LISPDECL_REAL] = subtypep_real;
	TypeSubtypep[LISPDECL_NUMBER] = subtypep_number;
	TypeSubtypep[LISPDECL_FLOAT] = subtypep_float;
	TypeSubtypep[LISPDECL_SHORT_FLOAT] = subtypep_short_float;
	TypeSubtypep[LISPDECL_SINGLE_FLOAT] = subtypep_single_float;
	TypeSubtypep[LISPDECL_DOUBLE_FLOAT] = subtypep_double_float;
	TypeSubtypep[LISPDECL_LONG_FLOAT] = subtypep_long_float;
	TypeSubtypep[LISPDECL_RATIO] = subtypep_ratio;
	TypeSubtypep[LISPDECL_COMPLEX] = subtypep_complex;
	TypeSubtypep[LISPDECL_RESTART] = subtypep_equaltype;
	TypeSubtypep[LISPDECL_ENVIRONMENT] = subtypep_equaltype;
	TypeSubtypep[LISPDECL_STREAM] = subtypep_stream;
	TypeSubtypep[LISPDECL_BROADCAST_STREAM] = subtypep_stream_child;
	TypeSubtypep[LISPDECL_CONCATENATED_STREAM] = subtypep_stream_child;
	TypeSubtypep[LISPDECL_ECHO_STREAM] = subtypep_stream_child;
	TypeSubtypep[LISPDECL_FILE_STREAM] = subtypep_stream_child;
	TypeSubtypep[LISPDECL_STRING_STREAM] = subtypep_stream_child;
	TypeSubtypep[LISPDECL_SYNONYM_STREAM] = subtypep_stream_child;
	TypeSubtypep[LISPDECL_TWO_WAY_STREAM] = subtypep_stream_child;
	TypeSubtypep[LISPDECL_PROMPT_STREAM] = subtypep_stream_child;
	TypeSubtypep[LISPDECL_PRETTY_STREAM] = subtypep_stream_child;
	TypeSubtypep[LISPDECL_BYTESPEC] = subtypep_equaltype;
	TypeSubtypep[LISPDECL_PRINT_DISPATCH] = subtypep_equaltype;
}

static SubtypepResult subtypep_call_asterisk(addr left, addr right)
{
	if (type_asterisk_p(right)) {
		if (RefNotDecl(right))
			fmte("Don't allow to use (not *).", NULL);
		return SUBTYPEP_INCLUDE;
	}
	if (type_asterisk_p(left)) {
		if (RefNotDecl(left))
			fmte("Don't allow to use (not *).", NULL);
		return SUBTYPEP_FALSE;
	}
	return subtypep_right(left, right);
}

static SubtypepResult subtypep_call_normal(addr left, addr right)
{
	if (type_asterisk_p(left) || type_asterisk_p(right))
		fmte("Don't allow to use asterisk.", NULL);
	return subtypep_right(left, right);
}

static SubtypepResult subtypep_call(addr left, addr right, int asterisk)
{
	if (asterisk)
		return subtypep_call_asterisk(left, right);
	else
		return subtypep_call_normal(left, right);
}

static void real_extract_subtypep(LocalRoot local, addr *ret, addr type)
{
	type_copy_local(local, &type, type);
	real_extract_local(local, &type, type);
	get_type_subtypep(ret, type);
}

_g SubtypepResult subtypep_result(addr left, addr right, int asterisk)
{
	SubtypepResult result;
	LocalRoot local;
	LocalStack stack;

	CheckType(left, LISPTYPE_TYPE);
	CheckType(right, LISPTYPE_TYPE);
	local = Local_Thread;
	push_local(local, &stack);
	real_extract_subtypep(local, &left, left);
	real_extract_subtypep(local, &right, right);
	result = subtypep_call(left, right, asterisk);
	rollback_local(local, stack);

	return result;
}

static int subtypep_execute(addr left, addr right, int asterisk, int *validp)
{
	switch (subtypep_result(left, right, asterisk)) {
		case SUBTYPEP_INCLUDE:
			*validp = 1;
			return 1;

		case SUBTYPEP_FALSE:
		case SUBTYPEP_EXCLUDE:
			*validp = 1;
			return 0;

		case SUBTYPEP_INVALID:
		default:
			break;
	}
	*validp = 0;
	return 0;
}

_g int subtypep_asterisk_clang(addr left, addr right, int *validp)
{
	CheckType(left, LISPTYPE_TYPE);
	CheckType(right, LISPTYPE_TYPE);
	return subtypep_execute(left, right, 1, validp);
}

_g int subtypep_clang(addr left, addr right, int *validp)
{
	CheckType(left, LISPTYPE_TYPE);
	CheckType(right, LISPTYPE_TYPE);
	return subtypep_execute(left, right, 0, validp);
}


/*
 *  common
 */
static int subtypep_symbol_clos_p(addr x, addr *r)
{
	if (! symbolp(x)) return 0;
	clos_find_class_nil(x, &x);
	if (x == Nil) return 0;
	*r = x;
	return 1;
}

static int subtypep_clos_p(addr x, addr y, addr *r1, addr *r2)
{
	int a, b;

	/* clos */
	a = closp(x);
	b = closp(y);
	if (a && b) {
		type_clos_heap(x, r1);
		type_clos_heap(y, r2);
		return 1;
	}
	if (a) {
		if (! subtypep_symbol_clos_p(y, &y)) return 0;
		type_clos_heap(x, r1);
		type_clos_heap(y, r2);
		return 1;
	}
	if (b) {
		if (! subtypep_symbol_clos_p(x, &x)) return 0;
		type_clos_heap(x, r1);
		type_clos_heap(y, r2);
		return 1;
	}

	if (! subtypep_symbol_clos_p(x, &x)) return 0;
	if (! subtypep_symbol_clos_p(y, &y)) return 0;
	if (clos_built_p(x) && clos_built_p(y)) return 0;
	type_clos_heap(x, r1);
	type_clos_heap(y, r2);
	return 1;
}

_g int subtypep_common(Execute ptr, addr x, addr y, addr env, addr *v1, addr *v2)
{
	int result, invalid;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, x, y, env, NULL);
	if (! subtypep_clos_p(x, y, &x, &y)) {
		if (parse_type(ptr, &x, x, env)) return 1;
		localhold_push(hold, x);
		if (parse_type(ptr, &y, y, env)) return 1;
		localhold_push(hold, y);
	}
	result = subtypep_clang(x, y, &invalid);
	*v1 = result? T: Nil;
	*v2 = invalid? T: Nil;
	localhold_end(hold);

	return 0;
}

