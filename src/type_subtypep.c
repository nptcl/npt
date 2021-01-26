#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "equal.h"
#include "format.h"
#include "hold.h"
#include "integer.h"
#include "object.h"
#include "sequence.h"
#include "rational.h"
#include "real.h"
#include "symbol.h"
#include "type.h"
#include "type_copy.h"
#include "type_number.h"
#include "type_object.h"
#include "type_parse.h"
#include "type_range.h"
#include "type_subtypep.h"
#include "type_table.h"
#include "type_typep.h"
#include "type_upgraded.h"

typedef int (*call_type_subtypep)(addr left, addr right, SubtypepResult *ret);
static call_type_subtypep TypeSubtypep[LISPDECL_SIZE];
static int subtypep_call_(addr left, addr right, int aster, SubtypepResult *ret);

#define ReturnInvalid(ret) Result(ret, SUBTYPEP_INVALID)
#define ReturnInclude(ret) Result(ret, SUBTYPEP_INCLUDE)
#define ReturnFalse(ret) Result(ret, SUBTYPEP_FALSE)
#define ReturnExclude(ret) Result(ret, SUBTYPEP_EXCLUDE)
#define ReturnBool(ret, p) Result(ret, (p)? SUBTYPEP_INCLUDE: SUBTYPEP_FALSE)

#define SwitchInclude(ret, v) { \
	switch (v) { \
		case SUBTYPEP_INVALID: return ReturnInvalid(ret); \
		case SUBTYPEP_INCLUDE: break; \
		default: return ReturnFalse(ret); \
	} \
}


/*
 *  subtypep-table
 */
static int subtypep_type_(addr left, addr right, SubtypepResult *ret)
{
	if (RefLispDecl(left) != LISPDECL_TYPE)
		return Result(ret, SUBTYPEP_EXCLUDE);
	else
		return ReturnBool(ret, RefLispDecl(left) == RefLispDecl(right));
}

static int subtypep_clos_(addr left, addr right, SubtypepResult *ret)
{
	int check;

	if (RefLispDecl(left) != LISPDECL_CLOS)
		return ReturnExclude(ret);
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	if (type_asterisk_p(right))
		return ReturnInclude(ret);
	if (type_asterisk_p(left))
		return ReturnFalse(ret);

	Return(clos_subclass_p_(left, right, &check));
	return ReturnBool(ret, check);
}

static int subtypep_error_(addr left, addr right, SubtypepResult *ret)
{
	infoprint(left);
	infoprint(right);
	*ret = SUBTYPEP_INVALID;
	return fmte_("Invalid subtypep argument.", NULL);
}

static int subtypep_equaltype_(addr left, addr right, SubtypepResult *ret)
{
	if (RefLispDecl(left) == RefLispDecl(right))
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_nil_(addr left, addr right, SubtypepResult *ret)
{
	return ReturnExclude(ret);
}

static int subtypep_t_(addr left, addr right, SubtypepResult *ret)
{
	return ReturnInclude(ret);
}

static int asterisk_or_t(addr pos)
{
	enum LISPDECL decl;
	GetLispDecl(pos, &decl);
	return decl == LISPDECL_ASTERISK || decl == LISPDECL_T;
}

static int subtypep_asterisk_or_t_(addr left, addr right, SubtypepResult *ret)
{
	if (asterisk_or_t(right))
		return ReturnInclude(ret);
	else if (asterisk_or_t(left))
		return ReturnFalse(ret);
	else
		return subtypep_result_(left, right, 0, ret);
}

static int subtypep_null_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_NULL:
			return ReturnInclude(ret);

		case LISPDECL_CONS:
		case LISPDECL_SEQUENCE:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_cons_(addr left, addr right, SubtypepResult *ret)
{
	enum LISPDECL decl;
	SubtypepResult value;
	addr car1, car2, cdr1, cdr2;

	GetLispDecl(left, &decl);
	if (decl == LISPDECL_SEQUENCE)
		return ReturnFalse(ret);
	if (decl != LISPDECL_CONS)
		return ReturnExclude(ret);

	GetArrayType(left, 0, &car1);
	GetArrayType(left, 1, &cdr1);
	GetArrayType(right, 0, &car2);
	GetArrayType(right, 1, &cdr2);

	Return(subtypep_asterisk_or_t_(car1, car2, &value));
	SwitchInclude(ret, value);
	Return(subtypep_asterisk_or_t_(cdr1, cdr2, &value));
	SwitchInclude(ret, value);

	return ReturnInclude(ret);
}

static int subtypep_symbol_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_NULL:
		case LISPDECL_SYMBOL:
		case LISPDECL_KEYWORD:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_keyword_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_KEYWORD:
			return ReturnInclude(ret);

		case LISPDECL_SYMBOL:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_pathname_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_PATHNAME:
		case LISPDECL_LOGICAL_PATHNAME:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_logical_pathname_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_LOGICAL_PATHNAME:
			return ReturnInclude(ret);

		case LISPDECL_PATHNAME:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
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

static int subtypep_sequence_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_ARRAY:
		case LISPDECL_SIMPLE_ARRAY:
			return ReturnBool(ret, subtypep_array_sequence(left));

		case LISPDECL_SEQUENCE:
		case LISPDECL_NULL:
		case LISPDECL_CONS:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int array_array_integer_(addr left, addr right, SubtypepResult *ret)
{
	size_t size;

	switch (GetType(left)) {
		case LISPTYPE_FIXNUM:
			return ReturnBool(ret, fixnumequal(left, right));

		case LISPTYPE_VECTOR:
			Check(GetStatusSize(left) != LISPSIZE_ARRAY4, "size left error");
			LenArrayA4(left, &size);
			return ReturnBool(ret, ((fixnum)size) == RefFixnum(right));

		default:
			return ReturnFalse(ret);
	}
}

static int array_array_vector_(addr left, addr right, SubtypepResult *ret)
{
	addr check1, check2;
	size_t i, size;

	if (GetType(left) != LISPTYPE_VECTOR)
		return ReturnFalse(ret);
	Check(GetStatusSize(right) != LISPSIZE_ARRAY4, "size right error");
	Check(GetStatusSize(left) != LISPSIZE_ARRAY4, "size left error");
	LenArrayA4(right, &i);
	LenArrayA4(left, &size);
	if (size != i)
		return ReturnFalse(ret);
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &check2);
		GetArrayA4(left, i, &check1);
		if (type_asterisk_p(check2))
			continue;
		if (type_asterisk_p(check1))
			return ReturnFalse(ret);
		Check(GetType(check2) != LISPTYPE_FIXNUM, "fixnum right error");
		Check(GetType(check1) != LISPTYPE_FIXNUM, "fixnum left error");
		if (! fixnumequal(check1, check2))
			return ReturnFalse(ret);
	}

	return ReturnInclude(ret);
}

static int subtypep_array_array_dimension_(addr left, addr right, SubtypepResult *ret)
{
	if (type_asterisk_p(right))
		return ReturnInclude(ret);
	if (type_asterisk_p(left))
		return ReturnFalse(ret);
	if (GetType(right) == LISPTYPE_FIXNUM)
		return array_array_integer_(left, right, ret);
	if (GetType(right) == LISPTYPE_VECTOR)
		return array_array_vector_(left, right, ret);
	Abort("type error");
	return ReturnInvalid(ret);
}

static int equal_array_type_asterisk(addr left, addr right)
{
	if (type_asterisk_p(right))
		return 1;
	if (type_asterisk_p(left))
		return 0;
	return upgraded_array0_equal(left, right);
}

static int subtypep_array_array_(addr left, addr right, SubtypepResult *ret)
{
	addr check1, check2;

	/* type */
	GetArrayType(left, 0, &check1);
	GetArrayType(right, 0, &check2);
	if (! equal_array_type_asterisk(check1, check2))
		return ReturnFalse(ret);

	/* dimension */
	GetArrayType(left, 1, &left);
	GetArrayType(right, 1, &right);
	return subtypep_array_array_dimension_(left, right, ret);
}

static int subtypep_array_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_ARRAY:
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_array_array_(left, right, ret);

		case LISPDECL_SEQUENCE:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_simple_array_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_SIMPLE_ARRAY:
			return subtypep_array_array_(left, right, ret);

		case LISPDECL_ARRAY:
		case LISPDECL_SEQUENCE:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_character_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_CHARACTER:
		case LISPDECL_BASE_CHAR:
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_base_char_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_BASE_CHAR:
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		case LISPDECL_CHARACTER:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_standard_char_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_STANDARD_CHAR:
			return ReturnInclude(ret);

		case LISPDECL_BASE_CHAR:
		case LISPDECL_CHARACTER:
			return ReturnFalse(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_real_less_(addr left, addr right, int *ret)
{
	if (! range_any_right_p(left))
		return Result(ret, 0);

	return range_right_right_less_equal_(left, right, ret);
}

static int subtypep_real_greater_(addr left, addr right, int *ret)
{
	if (! range_left_any_p(left))
		return Result(ret, 0);

	return range_left_left_greater_equal_(left, right, ret);
}

static int subtypep_real_range_(addr left, addr right, int *ret)
{
	if (! range_between_p(left))
		return Result(ret, 0);

	return range_in_between_(left, right, ret);
}

static int subtypep_realcheck_(addr left, addr right, int *ret)
{
	addr check1, check2;

	if (range_asterisk_p(right))
		return Result(ret, 1);
	if (range_asterisk_p(left))
		return Result(ret, 0);
	GetArrayType(right, 0, &check1);
	GetArrayType(right, 2, &check2);
	if (type_asterisk_p(check1))
		return subtypep_real_less_(left, right, ret);
	if (type_asterisk_p(check2))
		return subtypep_real_greater_(left, right, ret);
	else
		return subtypep_real_range_(left, right, ret);
}

static int realexclude_left_(addr left, addr right, int *ret)
{
	if (! range_any_right_p(left))
		return Result(ret, 0);
	if (! range_left_any_p(right))
		return Result(ret, 0);

	return range_right_left_less_(left, right, ret);
}

static int realexclude_right_(addr left, addr right, int *ret)
{
	if (! range_left_any_p(left))
		return Result(ret, 0);
	if (! range_any_right_p(right))
		return Result(ret, 0);

	return range_left_right_greater_(left, right, ret);
}

static int subtypep_realexlucde_(addr left, addr right, int *ret)
{
	int check;

	Return(realexclude_left_(left, right, &check));
	if (check)
		return Result(ret, 1);

	return realexclude_right_(left, right, ret);
}

static int subtypep_realparameter_(addr left, addr right, SubtypepResult *ret)
{
	int check;

	Return(subtypep_realcheck_(left, right, &check));
	if (check)
		return ReturnInclude(ret);

	Return(subtypep_realexlucde_(left, right, &check));
	if (check)
		return ReturnExclude(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_integer_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_RATIONAL:
		case LISPDECL_REAL:
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_INTEGER:
			return subtypep_realparameter_(left, right, ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_rational_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_REAL:
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return ReturnBool(ret, range_asterisk_p(right));

		case LISPDECL_RATIONAL:
		case LISPDECL_INTEGER:
			return subtypep_realparameter_(left, right, ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_real_(addr left, addr right, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	switch (type) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return ReturnBool(ret, range_asterisk_p(right));

		default:
			break;
	}

	if (decl_range_p(type))
		return subtypep_realparameter_(left, right, ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_number_(addr left, addr right, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	switch (type) {
		case LISPDECL_NUMBER:
		case LISPDECL_COMPLEX:
		case LISPDECL_RATIO:
			return ReturnInclude(ret);

		default:
			break;
	}

	if (decl_range_p(type))
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_float_(addr left, addr right, SubtypepResult *ret)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	switch (type) {
		case LISPDECL_NUMBER:
		case LISPDECL_REAL:
			return ReturnFalse(ret);

		default:
			break;
	}

	if (decl_float_p(type))
		return subtypep_realparameter_(left, right, ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_float_type_(addr left, addr right,
		SubtypepResult *ret, enum LISPDECL check)
{
	enum LISPDECL type;

	GetLispDecl(left, &type);
	switch (type) {
		case LISPDECL_NUMBER:
		case LISPDECL_REAL:
		case LISPDECL_FLOAT:
			return ReturnFalse(ret);

		default:
			break;
	}

	if (type == check)
		return subtypep_realparameter_(left, right, ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_short_float_(addr left, addr right, SubtypepResult *ret)
{
	return subtypep_float_type_(left, right, ret, LISPDECL_SHORT_FLOAT);
}

static int subtypep_single_float_(addr left, addr right, SubtypepResult *ret)
{
	return subtypep_float_type_(left, right, ret, LISPDECL_SINGLE_FLOAT);
}

static int subtypep_double_float_(addr left, addr right, SubtypepResult *ret)
{
	return subtypep_float_type_(left, right, ret, LISPDECL_DOUBLE_FLOAT);
}

static int subtypep_long_float_(addr left, addr right, SubtypepResult *ret)
{
	return subtypep_float_type_(left, right, ret, LISPDECL_LONG_FLOAT);
}

static int subtypep_ratio_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_NUMBER:
		case LISPDECL_REAL:
		case LISPDECL_RATIONAL:
			return ReturnFalse(ret);

		case LISPDECL_RATIO:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_complex_value_(addr left, addr right, SubtypepResult *ret)
{
	int check;

	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	check = RefLispDecl(left) == RefLispDecl(right)
		&& RefNotDecl(left) == RefNotDecl(right);
	return ReturnBool(ret, check);
}

static int subtypep_complex_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_NUMBER:
			return ReturnFalse(ret);

		case LISPDECL_COMPLEX:
			return subtypep_complex_value_(left, right, ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_table_(addr left, addr right, SubtypepResult *ret)
{
	call_type_subtypep call;

	call = TypeSubtypep[(int)RefLispDecl(right)];
	Check(call == NULL, "call error");
	return (*call)(left, right, ret);
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

static int gettype_ordinary_(const ordargs *ptr, size_t index, ordtype *ret)
{
	int check1, check2;

	memset(ret, 0, sizeoft(ordtype));
	ret->type = Nil;
	/* var */
	if (index < ptr->size_var) {
		Return(getnth_(ptr->var, index, &(ret->type)));
		ret->var = 1;
		return 0;
	}
	index -= ptr->size_var;
	/* opt */
	if (index < ptr->size_opt) {
		Return(getnth_(ptr->opt, index, &(ret->type)));
		ret->var = 1;
		return 0;
	}
	index -= ptr->size_opt;
	/* rest */
	check1 = (ptr->rest == Nil);
	check2 = (ptr->key == Nil);
	if (check1 && check2) {
		ret->nil = 1;
		return 0;
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

	return 0;
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

static int ordinary_subtypep_(
		const ordargs *ptr1, const ordtype *type1,
		const ordargs *ptr2, const ordtype *type2,
		int *ret)
{
	SubtypepResult value;
	addr left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	make_ordinary_type(local, &left, ptr1, type1);
	make_ordinary_type(local, &right, ptr2, type2);
	Return(subtypep_result_(left, right, 1, &value));
	rollback_local(local, stack);

	return Result(ret, value == SUBTYPEP_INCLUDE);
}

static int ordinary_size_(const ordargs *ptr1, const ordargs *ptr2,
		size_t size, int *ret)
{
	int check;
	size_t i;
	ordtype type1, type2;

	for (i = 0; i < size; i++) {
		Return(gettype_ordinary_(ptr1, i, &type1));
		Return(gettype_ordinary_(ptr2, i, &type2));
		if (type1.nil)
			break;
		if (type2.nil)
			return Result(ret, 0);
		Return(ordinary_subtypep_(ptr1, &type1, ptr2, &type2, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int ordinary_simple_(const ordargs *ptr1, const ordargs *ptr2, int *ret)
{
	if (ptr1->size > ptr2->size)
		return Result(ret, 0);
	/* short size */
	return ordinary_size_(ptr1, ptr2, ptr1->size, ret);
}

static int ordinary_simple_left_(const ordargs *ptr1, const ordargs *ptr2, int *ret)
{
	/* short size */
	return ordinary_size_(ptr1, ptr2, ptr1->size, ret);
}

static int ordinary_check_(const ordargs *ptr1, const ordargs *ptr2, int *ret)
{
	/* long size */
	return ordinary_size_(ptr1, ptr2,
			(ptr1->size > ptr2->size? ptr1->size: ptr2->size),
			ret);
}

static int subtypep_function_ordinary_(addr left, addr right, int *ret)
{
	int check1, check2;
	ordargs ptr1, ptr2;

	/* asterisk */
	if (type_asterisk_p(right))
		return Result(ret, 1);
	if (type_asterisk_p(left))
		return Result(ret, 0);

	/* list */
	make_function_ordinary(&ptr1, left);
	make_function_ordinary(&ptr2, right);
	if (ptr1.size_var < ptr2.size_var)
		return Result(ret, 0);

	check1 = ordargs_simple_p(&ptr1);
	check2 = ordargs_simple_p(&ptr2);
	if (check1 && check2)
		return ordinary_simple_(&ptr1, &ptr2, ret);
	if (check1)
		return ordinary_simple_left_(&ptr1, &ptr2, ret);
	if (check2)
		return Result(ret, 0);
	else
		return ordinary_check_(&ptr1, &ptr2, ret);
}

static int subtypep_function_check_(addr left, addr right, SubtypepResult *ret)
{
	int check;
	SubtypepResult value;
	addr check1, check2;

	/* lambda-list */
	GetArrayType(left, 0, &check1);
	GetArrayType(right, 0, &check2);
	Return(subtypep_function_ordinary_(check1, check2, &check));
	if (! check)
		return ReturnFalse(ret);

	/* values */
	GetArrayType(left, 1, &left);
	GetArrayType(right, 1, &right);
	Return(subtypep_result_(left, right, 1, &value));
	SwitchInclude(ret, value);

	return ReturnInclude(ret);
}

static int subtypep_function_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_FUNCTION:
		case LISPDECL_COMPILED_FUNCTION:
			return subtypep_function_check_(left, right, ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_compiled_function_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_FUNCTION:
			return ReturnFalse(ret);

		case LISPDECL_COMPILED_FUNCTION:
			return subtypep_function_check_(left, right, ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_stream_(addr left, addr right, SubtypepResult *ret)
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
		case LISPDECL_MEMORY_STREAM:
			return ReturnInclude(ret);

		default:
			return ReturnExclude(ret);
	}
}

static int subtypep_stream_child_(addr left, addr right, SubtypepResult *ret)
{
	enum LISPDECL type1, type2;

	GetLispDecl(left, &type1);
	GetLispDecl(right, &type2);
	if (type1 == type2)
		return ReturnInclude(ret);
	else if (type1 == LISPDECL_STREAM)
		return ReturnFalse(ret);
	else
		return ReturnExclude(ret);
}


/*
 *  subtypep_lisptype_
 */
static int subtypep_lisptype_normal_(addr left, addr right, SubtypepResult *ret,
		call_type_subtypep call)
{
	int not2;
	SubtypepResult value;

	Return((*call)(left, right, &value));
	GetNotDecl(right, &not2);
	switch (value) {
		case SUBTYPEP_INCLUDE:
			return Result(ret, not2? SUBTYPEP_EXCLUDE: SUBTYPEP_INCLUDE);

		case SUBTYPEP_EXCLUDE:
			return Result(ret, not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE);

		default:
			return Result(ret, value);
	}
}

static int subtypep_lisptype_not_(addr left, addr right, SubtypepResult *ret,
		call_type_subtypep call)
{
	int not2;
	SubtypepResult value;

	Return((*call)(right, left, &value));  /* reverse */
	GetNotDecl(right, &not2);
	switch (value) {
		case SUBTYPEP_INCLUDE:
			return Result(ret, not2? SUBTYPEP_INCLUDE: SUBTYPEP_EXCLUDE);

		case SUBTYPEP_EXCLUDE:
			return Result(ret, SUBTYPEP_FALSE);

		default:
			return Result(ret, value);
	}
}

static int subtypep_lisptype_(addr left, addr right, SubtypepResult *ret,
		call_type_subtypep call)
{
	if (RefNotDecl(left))
		return subtypep_lisptype_not_(left, right, ret, call);
	else
		return subtypep_lisptype_normal_(left, right, ret, call);
}


/*
 *  subtypep_eql
 */
static int subtypep_eql_eql_(addr left, addr right, SubtypepResult *ret)
{
	GetArrayType(left, 0, &left);
	GetArrayType(right, 0, &right);
	if (eql_function(left, right))
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_eql_type_(addr left, addr right, SubtypepResult *ret)
{
	int check;

	/* (subtypep '(eql x) '(satisfies y)) */
	type_getvalues1(right, &right);
	if (RefLispDecl(right) == LISPDECL_SATISFIES)
		return ReturnInvalid(ret);

	/* (subtypep '(eql x) right) */
	GetArrayType(left, 0, &left);
	Return(typep_table_(Execute_Thread, left, right, &check));
	if (check)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_type_eql_(addr left, addr right, SubtypepResult *ret)
{
	int check;

	/* (subtypep '(satisfies x) '(eql y)) */
	type_getvalues1(left, &left);
	if (RefLispDecl(left) == LISPDECL_SATISFIES)
		return ReturnInvalid(ret);

	/* (subtypep left '(eql x)) */
	GetArrayType(right, 0, &right);
	Return(typep_table_(Execute_Thread, right, left, &check));
	if (check)
		return ReturnFalse(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_eql_call_(addr left, addr right, SubtypepResult *ret)
{
	int check1, check2;

	check1 = (RefLispDecl(left) == LISPDECL_EQL);
	check2 = (RefLispDecl(right) == LISPDECL_EQL);
	if (check1 && check2)
		return subtypep_eql_eql_(left, right, ret);
	if (check1)
		return subtypep_eql_type_(left, right, ret);
	if (check2)
		return subtypep_type_eql_(left, right, ret);
	Abort("type error");
	return ReturnInvalid(ret);
}

static int subtypep_eql_(addr left, addr right, SubtypepResult *ret)
{
	return subtypep_lisptype_(left, right, ret, subtypep_eql_call_);
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

static int gettype_values_(addr pos, size_t index, addr *ret)
{
	addr check;
	size_t size;

	/* var */
	GetArrayType(pos, 0, &check);
	size = length_list_unsafe(check);
	if (index < size)
		return getnth_(check, index, ret);
	index -= size;

	/* opt */
	GetArrayType(pos, 1, &check);
	size = length_list_unsafe(check);
	if (index < size)
		return getnth_(check, index, ret);

	/* rest */
	GetArrayType(pos, 2, ret);

	return 0;
}

static int subtypep_boolean_(addr left, addr right, int *ret)
{
	SubtypepResult value;
	Return(subtypep_result_(left, right, 1, &value));
	return Result(ret, value == SUBTYPEP_INCLUDE);
}

static int subtypep_values_values_(addr left, addr right, int *ret)
{
	int check;
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
		Return(gettype_values_(left, i, &check1));
		Return(gettype_values_(right, i, &check2));
		Return(subtypep_boolean_(check1, check2, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static void subtypep_values_local(addr pos, addr *ret)
{
	LocalRoot local;
	addr rest;

	local = Local_Thread;
	conscar_local(local, &pos, pos);
	GetTypeTable(&rest, Null);
	type_values_local(local, pos, Nil, rest, Nil, ret);
}

static int subtypep_values_type_(addr left, addr right, int *ret)
{
	Check(RefLispDecl(left) != LISPDECL_VALUES, "decl left error");
	Check(RefNotDecl(left), "left not error");
	subtypep_values_local(right, &right);
	return subtypep_values_values_(left, right, ret);
}

static int subtypep_type_values_(addr left, addr right, int *ret)
{
	Check(RefLispDecl(right) != LISPDECL_VALUES, "decl right error");
	Check(RefNotDecl(right), "right not error");
	subtypep_values_local(left, &left);
	return subtypep_values_values_(left, right, ret);
}

static int subtypep_values_call_(addr left, addr right, int *ret)
{
	int check1, check2;

	check1 = (RefLispDecl(left) == LISPDECL_VALUES);
	check2 = (RefLispDecl(right) == LISPDECL_VALUES);
	if (check1 && check2)
		return subtypep_values_values_(left, right, ret);
	if (check1)
		return subtypep_values_type_(left, right, ret);
	if (check2)
		return subtypep_type_values_(left, right, ret);
	Abort("type error");
	return Result(ret, 0);
}

static int subtypep_values_(addr left, addr right, SubtypepResult *ret)
{
	int value;
	LocalRoot local;
	LocalStack stack;

	/*  typespec values cannot recognize subtypep-exclude.
	 *  result is include or false.
	 */
	local = Local_Thread;
	push_local(local, &stack);
	Return(subtypep_values_call_(left, right, &value));
	rollback_local(local, stack);
	return Result(ret, value? SUBTYPEP_INCLUDE: SUBTYPEP_FALSE);
}


/*
 *  subtypep_call
 */
static int subtypep_leftright_(addr left, addr right, SubtypepResult *ret)
{
	return subtypep_lisptype_(left, right, ret, subtypep_table_);
}

static int subtypep_clos_left_(addr left, addr right, SubtypepResult *ret)
{
	int not1;

	GetNotDecl(right, &not1);
	if (not1)
		return Result(ret, SUBTYPEP_INVALID);
	else
		return subtypep_leftright_(left, right, ret);
}


/* and/or reduce */
static int subtypep_reduce_(addr pos, addr *value, int *ret);

static int subtypep_reduce_vector_(LocalRoot local, addr pos, addr *value, int *ret)
{
	int result, check;
	addr src, dst, reduce;
	size_t size, i;

	GetArrayType(pos, 0, &src);
	LenArrayA4(src, &size);
	vector4_local(local, &dst, size);
	result = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(src, i, &reduce);
		Return(subtypep_reduce_(reduce, &reduce, &check));
		result |= check;
		SetArrayA4(dst, i, reduce);
	}
	*value = result? dst: src;
	return Result(ret, result);
}

/* (and) -> t */
static size_t subtypep_reduce_size(addr vector)
{
	size_t size;
	LenArrayA4(vector, &size);
	return size;
}

static int subtypep_reduce_and1(addr vector, addr *value)
{
	if (subtypep_reduce_size(vector) != 0)
		return 0;
	GetTypeTable(value, T);
	return 1;
}

/* (and type) -> type */
static int subtypep_reduce_and2(addr vector, addr *value)
{
	if (subtypep_reduce_size(vector) != 1)
		return 0;
	GetArrayA4(vector, 0, value);
	return 1;
}

/* (and ... nil ...) -> nil */
static int subtypep_reduce_and3(addr vector, addr *value)
{
	addr check;
	size_t size, i;

	LenArrayA4(vector, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(vector, i, &check);
		if (RefLispDecl(check) == LISPDECL_NIL) {
			GetTypeTable(value, Nil);
			return 1;
		}
	}

	return 0;
}

/* (and ... t ...) -> (and ...)  remove t */
static int subtypep_reduce_remove(LocalRoot local, addr vector, addr *value,
		enum LISPDECL equal, enum LISPDECL make)
{
	int exist;
	addr check, dst;
	size_t size, count, i;

	/* length */
	LenArrayA4(vector, &size);
	count = 0;
	exist = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(vector, i, &check);
		if (RefLispDecl(check) == equal)
			exist = 1;
		else
			count++;
	}
	if (exist == 0)
		return 0;

	/* replace */
	vector4_local(local, &dst, count);
	count = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(vector, i, &check);
		if (RefLispDecl(check) == equal)
			continue;
		SetArrayA4(dst, count++, check);
	}
	type1_local(local, make, dst, value);

	return 1;
}

static int subtypep_reduce_and4(LocalRoot local, addr vector, addr *value)
{
	return subtypep_reduce_remove(local, vector, value, LISPDECL_T, LISPDECL_AND);
}

/* (and [exclude]) -> nil */
static int subtypep_reduce_and5_(addr vector, addr *value, int *ret)
{
	SubtypepResult check;
	addr left, right;
	size_t size, x, y;

	LenArrayA4(vector, &size);
	for (x = 0; x < size; x++) {
		GetArrayA4(vector, x, &left);
		for (y = x + 1; y < size; y++) {
			GetArrayA4(vector, y, &right);
			Return(subtypep_call_(left, right, 0, &check));
			if (check == SUBTYPEP_EXCLUDE) {
				GetTypeTable(value, Nil);
				return Result(ret, 1);
			}
		}
	}

	return Result(ret, 0);
}

static int subtypep_reduce_and_(LocalRoot local, addr pos, addr *value, int *ret)
{
	int result, check;
	addr vector;

	result = 0;
first:
	Return(subtypep_reduce_vector_(local, pos, &vector, &check));
	result |= check;
	if (subtypep_reduce_and1(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_and2(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_and3(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_and4(local, vector, &pos)) {
		result = 1;
		goto first;
	}
	Return(subtypep_reduce_and5_(vector, value, &check));
	if (check)
		return Result(ret, 1);
	if (result) {
		type1_local(local, LISPDECL_AND, vector, value);
		return Result(ret, 1);
	}

	return Result(ret, 0);
}

/* (or) -> nil */
static int subtypep_reduce_or1(addr vector, addr *value)
{
	if (subtypep_reduce_size(vector) != 0)
		return 0;
	GetTypeTable(value, Nil);
	return 1;
}

/* (or type) -> type */
static int subtypep_reduce_or2(addr vector, addr *value)
{
	return subtypep_reduce_and2(vector, value);
}

/* (or ... t ...) -> t */
static int subtypep_reduce_or3(addr vector, addr *value)
{
	addr check;
	size_t size, i;

	LenArrayA4(vector, &size);
	for (i = 0; i < size; i++) {
		GetArrayA4(vector, i, &check);
		if (RefLispDecl(check) == LISPDECL_T) {
			GetTypeTable(value, T);
			return 1;
		}
	}

	return 0;
}

/* (or ... nil ...) -> (or ...)  remove nil */
static int subtypep_reduce_or4(LocalRoot local, addr vector, addr *value)
{
	return subtypep_reduce_remove(local, vector, value, LISPDECL_NIL, LISPDECL_OR);
}

static int subtypep_reduce_or_(LocalRoot local, addr pos, addr *value, int *ret)
{
	int result, check;
	addr vector;

	result = 0;
first:
	Return(subtypep_reduce_vector_(local, pos, &vector, &check));
	result |= check;
	if (subtypep_reduce_or1(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_or2(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_or3(vector, value))
		return Result(ret, 1);
	if (subtypep_reduce_or4(local, vector, &pos)) {
		result = 1;
		goto first;
	}
	if (result) {
		type1_local(local, LISPDECL_OR, vector, value);
		return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int subtypep_reduce_(addr pos, addr *value, int *ret)
{
	LocalRoot local;

	local = Local_Thread;
	switch (RefLispDecl(pos)) {
		case LISPDECL_AND:
			return subtypep_reduce_and_(local, pos, value, ret);

		case LISPDECL_OR:
			return subtypep_reduce_or_(local, pos, value, ret);

		default:
			*value = pos;
			return Result(ret, 0);
	}
}


/* and/or */
static int subtypep_and_right_(addr left, addr right, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(right, &right, &include));
	if (include)
		return subtypep_call_(left, right, 0, ret);
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	include = 1;
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &check);
		Return(subtypep_call_(left, check, 0, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result == SUBTYPEP_EXCLUDE)
			exclude = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	if (include)
		return ReturnInclude(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_or_right_(addr left, addr right, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(right, &right, &include));
	if (include)
		return subtypep_call_(left, right, 0, ret);
	GetArrayType(right, 0, &right);
	LenArrayA4(right, &size);
	include = 0;
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(right, i, &check);
		Return(subtypep_call_(left, check, 0, &result));
		if (result == SUBTYPEP_INCLUDE)
			include = 1;
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	if (exclude)
		return ReturnExclude(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_and_left_(addr left, addr right, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(left, &left, &include));
	if (include)
		return subtypep_call_(left, right, 0, ret);
	GetArrayType(left, 0, &left);
	LenArrayA4(left, &size);
	include = 0;
	exclude = 0;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(left, i, &check);
		Return(subtypep_call_(check, right, 0, &result));
		if (result == SUBTYPEP_INCLUDE)
			include = 1;
		if (result == SUBTYPEP_EXCLUDE)
			exclude = 1;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

static int subtypep_or_left_(addr left, addr right, SubtypepResult *ret)
{
	int include, exclude, invalid;
	SubtypepResult result;
	addr check;
	size_t size, i;

	Return(subtypep_reduce_(left, &left, &include));
	if (include)
		return subtypep_call_(left, right, 0, ret);
	GetArrayType(left, 0, &left);
	LenArrayA4(left, &size);
	include = 1;
	exclude = 1;
	invalid = 0;
	for (i = 0; i < size; i++) {
		GetArrayA4(left, i, &check);
		Return(subtypep_call_(check, right, 0, &result));
		if (result != SUBTYPEP_INCLUDE)
			include = 0;
		if (result != SUBTYPEP_EXCLUDE)
			exclude = 0;
		if (result == SUBTYPEP_INVALID)
			invalid = 1;
	}
	if (include)
		return ReturnInclude(ret);
	if (exclude)
		return ReturnExclude(ret);
	if (invalid)
		return ReturnInvalid(ret);
	else
		return ReturnFalse(ret);
}

#define LISP_DEBUG_SUBTYPEP
#undef LISP_DEBUG_SUBTYPEP

#ifdef LISP_DEBUG_SUBTYPEP
static int infotype_(addr pos)
{
	Return(type_object_(&pos, pos));
	infoprint(pos);
}
static void infosubtypep(SubtypepResult value)
{
	switch (value) {
		case SUBTYPEP_INCLUDE:
			info("subtypep: include");
			break;

		case SUBTYPEP_EXCLUDE:
			info("subtypep: exclude");
			break;

		case SUBTYPEP_FALSE:
			info("subtypep: false");
			break;

		case SUBTYPEP_INVALID:
			info("subtypep: invalid");
			break;

		default:
			info("subtypep: error");
			break;
	}
}
static int subtypep_andargs_right_(addr left, addr right, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_and_right_(left, right, &value));
	info("[and-right]");
	Return(infotype_(left));
	Return(infotype_(right));
	infosubtypep(value);
	return Result(ret, value);
}
static int subtypep_orargs_right_(addr left, addr right, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_or_right_(left, right, &value));
	info("[or-right]");
	Return(infotype_(left));
	Return(infotype_(right));
	infosubtypep(value);
	return Result(ret, value);
}
static int subtypep_andargs_left_(addr left, addr right, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_and_left_(left, right, &value));
	info("[and-left]");
	Return(infotype_(left));
	Return(infotype_(right));
	infosubtypep(value);
	return Result(ret, value);
}
static int subtypep_orargs_left_(addr left, addr right, SubtypepResult *ret)
{
	SubtypepResult value;
	Return(subtypep_or_left_(left, right, &value));
	info("[or-left]");
	Return(infotype_(left));
	Return(infotype_(right));
	infosubtypep(value);
	return Result(ret, value);
}
#else
#define subtypep_orargs_left_ subtypep_or_left_
#define subtypep_andargs_left_ subtypep_and_left_
#define subtypep_orargs_right_ subtypep_or_right_
#define subtypep_andargs_right_ subtypep_and_right_
#endif

static int subtypep_or_right_switch_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_AND:
			return subtypep_andargs_left_(left, right, ret);

		case LISPDECL_OR:
			return subtypep_orargs_left_(left, right, ret);

		default:
			return subtypep_orargs_right_(left, right, ret);
	}
}

static int subtypep_and_right_switch_(addr left, addr right, SubtypepResult *ret)
{
	switch (RefLispDecl(left)) {
		case LISPDECL_AND:
			return subtypep_andargs_left_(left, right, ret);

		case LISPDECL_OR:
			return subtypep_orargs_left_(left, right, ret);

		default:
			return subtypep_andargs_right_(left, right, ret);
	}
}


/* left */
static int subtypep_satisfies_left_(addr left, addr right, SubtypepResult *ret)
{
	if (RefLispDecl(right) == LISPDECL_T)
		return ReturnInclude(ret);
	else
		return ReturnInvalid(ret);
}

static int subtypep_left_(addr left, addr right, SubtypepResult *ret)
{
	Check(GetType(left) != LISPTYPE_TYPE, "type left error");
	switch (RefLispDecl(left)) {
		case LISPDECL_AND:
			return subtypep_andargs_left_(left, right, ret);

		case LISPDECL_OR:
			return subtypep_orargs_left_(left, right, ret);

		case LISPDECL_EQL:
			return subtypep_eql_(left, right, ret);

		case LISPDECL_MEMBER:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The member type illegal in this context.", NULL);

		case LISPDECL_NOT:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The not type illegal in this context.", NULL);

		case LISPDECL_VALUES:
			return subtypep_values_(left, right, ret);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_left_(left, right, ret);

		case LISPDECL_NIL:
			return ReturnInclude(ret);

		case LISPDECL_T:
			return ReturnFalse(ret);

		case LISPDECL_CLOS:
			return subtypep_clos_left_(left, right, ret);

		default:
			return subtypep_leftright_(left, right, ret);
	}
}

/* right */
static int subtypep_satisfies_right_(addr left, addr right, SubtypepResult *ret)
{
	if (RefLispDecl(left) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnInvalid(ret);
}

static int subtypep_nil_right_(addr left, SubtypepResult *ret)
{
	if (RefLispDecl(left) == LISPDECL_NIL)
		return ReturnInclude(ret);
	else
		return ReturnExclude(ret);
}

static int subtypep_right_(addr left, addr right, SubtypepResult *ret)
{
	Check(GetType(right) != LISPTYPE_TYPE, "type right error");
	switch (RefLispDecl(right)) {
		case LISPDECL_AND:
			return subtypep_and_right_switch_(left, right, ret);

		case LISPDECL_OR:
			return subtypep_or_right_switch_(left, right, ret);

		case LISPDECL_EQL:
			return subtypep_eql_(left, right, ret);

		case LISPDECL_MEMBER:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The member type illegal in this context.", NULL);

		case LISPDECL_NOT:
			*ret = SUBTYPEP_INVALID;
			return fmte_("The not type illegal in this context.", NULL);

		case LISPDECL_VALUES:
			return subtypep_values_(left, right, ret);

		case LISPDECL_SATISFIES:
			return subtypep_satisfies_right_(left, right, ret);

		case LISPDECL_NIL:
			return subtypep_nil_right_(left, ret);

		case LISPDECL_T:
			return ReturnInclude(ret);

		default:
			return subtypep_left_(left, right, ret);
	}
}


/*
 *  subtypep_clang
 */
void init_type_subtypep(void)
{
	int i;

	for (i = 0; i < LISPDECL_SIZE; i++)
		TypeSubtypep[i] = subtypep_error_;

	TypeSubtypep[LISPDECL_TYPE] = subtypep_type_;
	TypeSubtypep[LISPDECL_CLOS] = subtypep_clos_;
	TypeSubtypep[LISPDECL_ASTERISK] = subtypep_error_;
	TypeSubtypep[LISPDECL_NIL] = subtypep_nil_;
	TypeSubtypep[LISPDECL_T] = subtypep_t_;
	TypeSubtypep[LISPDECL_NULL] = subtypep_null_;
	TypeSubtypep[LISPDECL_CONS] = subtypep_cons_;
	TypeSubtypep[LISPDECL_HASH_TABLE] = subtypep_equaltype_;
	TypeSubtypep[LISPDECL_SYMBOL] = subtypep_symbol_;
	TypeSubtypep[LISPDECL_KEYWORD] = subtypep_keyword_;
	TypeSubtypep[LISPDECL_PACKAGE] = subtypep_equaltype_;
	TypeSubtypep[LISPDECL_RANDOM_STATE] = subtypep_equaltype_;
	TypeSubtypep[LISPDECL_READTABLE] = subtypep_equaltype_;
	TypeSubtypep[LISPDECL_FUNCTION] = subtypep_function_;
	TypeSubtypep[LISPDECL_COMPILED_FUNCTION] = subtypep_compiled_function_;
	TypeSubtypep[LISPDECL_PATHNAME] = subtypep_pathname_;
	TypeSubtypep[LISPDECL_LOGICAL_PATHNAME] = subtypep_logical_pathname_;
	TypeSubtypep[LISPDECL_SEQUENCE] = subtypep_sequence_;
	TypeSubtypep[LISPDECL_ARRAY] = subtypep_array_;
	TypeSubtypep[LISPDECL_SIMPLE_ARRAY] = subtypep_simple_array_;
	TypeSubtypep[LISPDECL_CHARACTER] = subtypep_character_;
	TypeSubtypep[LISPDECL_BASE_CHAR] = subtypep_base_char_;
	TypeSubtypep[LISPDECL_STANDARD_CHAR] = subtypep_standard_char_;
	TypeSubtypep[LISPDECL_INTEGER] = subtypep_integer_;
	TypeSubtypep[LISPDECL_RATIONAL] = subtypep_rational_;
	TypeSubtypep[LISPDECL_REAL] = subtypep_real_;
	TypeSubtypep[LISPDECL_NUMBER] = subtypep_number_;
	TypeSubtypep[LISPDECL_FLOAT] = subtypep_float_;
	TypeSubtypep[LISPDECL_SHORT_FLOAT] = subtypep_short_float_;
	TypeSubtypep[LISPDECL_SINGLE_FLOAT] = subtypep_single_float_;
	TypeSubtypep[LISPDECL_DOUBLE_FLOAT] = subtypep_double_float_;
	TypeSubtypep[LISPDECL_LONG_FLOAT] = subtypep_long_float_;
	TypeSubtypep[LISPDECL_RATIO] = subtypep_ratio_;
	TypeSubtypep[LISPDECL_COMPLEX] = subtypep_complex_;
	TypeSubtypep[LISPDECL_RESTART] = subtypep_equaltype_;
	TypeSubtypep[LISPDECL_ENVIRONMENT] = subtypep_equaltype_;
	TypeSubtypep[LISPDECL_STREAM] = subtypep_stream_;
	TypeSubtypep[LISPDECL_BROADCAST_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_CONCATENATED_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_ECHO_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_FILE_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_STRING_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_SYNONYM_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_TWO_WAY_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_PROMPT_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_PRETTY_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_MEMORY_STREAM] = subtypep_stream_child_;
	TypeSubtypep[LISPDECL_BYTESPEC] = subtypep_equaltype_;
	TypeSubtypep[LISPDECL_PRINT_DISPATCH] = subtypep_equaltype_;
	TypeSubtypep[LISPDECL_EVAL] = subtypep_equaltype_;
}

static int subtypep_call_asterisk_(addr left, addr right, SubtypepResult *ret)
{
	if (type_asterisk_p(right)) {
		if (RefNotDecl(right)) {
			*ret = SUBTYPEP_INVALID;
			return fmte_("Don't allow to use (not *).", NULL);
		}
		else {
			return ReturnInclude(ret);
		}
	}
	if (type_asterisk_p(left)) {
		if (RefNotDecl(left)) {
			*ret = SUBTYPEP_INVALID;
			return fmte_("Don't allow to use (not *).", NULL);
		}
		else {
			return ReturnFalse(ret);
		}
	}
	return subtypep_right_(left, right, ret);
}

static int subtypep_call_normal_(addr left, addr right, SubtypepResult *ret)
{
	if (type_asterisk_p(left) || type_asterisk_p(right)) {
		*ret = SUBTYPEP_INVALID;
		return fmte_("Don't allow to use asterisk.", NULL);
	}
	else {
		return subtypep_right_(left, right, ret);
	}
}

static int subtypep_call_(addr left, addr right, int aster, SubtypepResult *ret)
{
	if (aster)
		return subtypep_call_asterisk_(left, right, ret);
	else
		return subtypep_call_normal_(left, right, ret);
}

static int real_extract_subtypep_(LocalRoot local, addr *ret, addr type)
{
	type_copy_local(local, &type, type);
	Return(real_extract_local_(local, &type, type));
	get_type_subtypep(ret, type);

	return 0;
}

int subtypep_result_(addr left, addr right, int aster, SubtypepResult *ret)
{
	SubtypepResult result;
	LocalRoot local;
	LocalStack stack;

	CheckType(left, LISPTYPE_TYPE);
	CheckType(right, LISPTYPE_TYPE);
	local = Local_Thread;
	push_local(local, &stack);
	Return(real_extract_subtypep_(local, &left, left));
	Return(real_extract_subtypep_(local, &right, right));
	Return(subtypep_call_(left, right, aster, &result));
	rollback_local(local, stack);

	return Result(ret, result);
}

static int subtypep_execute_(addr left, addr right, int aster, int *ret, int *validp)
{
	SubtypepResult value;

	Return(subtypep_result_(left, right, aster, &value));
	switch (value) {
		case SUBTYPEP_INCLUDE:
			if (validp)
				*validp = 1;
			return Result(ret, 1);

		case SUBTYPEP_FALSE:
		case SUBTYPEP_EXCLUDE:
			if (validp)
				*validp = 1;
			return Result(ret, 0);

		case SUBTYPEP_INVALID:
		default:
			break;
	}
	if (validp)
		*validp = 0;
	return Result(ret, 0);
}

int subtypep_asterisk_clang_(addr left, addr right, int *ret, int *validp)
{
	CheckType(left, LISPTYPE_TYPE);
	CheckType(right, LISPTYPE_TYPE);
	return subtypep_execute_(left, right, 1, ret, validp);
}

int subtypep_clang_(addr left, addr right, int *ret, int *validp)
{
	CheckType(left, LISPTYPE_TYPE);
	CheckType(right, LISPTYPE_TYPE);
	return subtypep_execute_(left, right, 0, ret, validp);
}


/*
 *  common
 */
static int subtypep_symbol_clos_p(addr x, addr *r)
{
	if (! symbolp(x))
		return 0;
	clos_find_class_nil(x, &x);
	if (x == Nil)
		return 0;
	*r = x;
	return 1;
}

static int subtypep_clos_p_(addr x, addr y, addr *r1, addr *r2, int *ret)
{
	int a, b;

	/* clos */
	a = closp(x);
	b = closp(y);
	if (a && b) {
		type_clos_heap(x, r1);
		type_clos_heap(y, r2);
		return Result(ret, 1);
	}
	if (a) {
		if (! subtypep_symbol_clos_p(y, &y))
			return Result(ret, 0);
		type_clos_heap(x, r1);
		type_clos_heap(y, r2);
		return Result(ret, 1);
	}
	if (b) {
		if (! subtypep_symbol_clos_p(x, &x))
			return Result(ret, 0);
		type_clos_heap(x, r1);
		type_clos_heap(y, r2);
		return Result(ret, 1);
	}

	if (! subtypep_symbol_clos_p(x, &x))
		return Result(ret, 0);
	if (! subtypep_symbol_clos_p(y, &y))
		return Result(ret, 0);

	Return(clos_built_p_(x, &a));
	Return(clos_built_p_(x, &b));
	if (a && b)
		return Result(ret, 0);
	type_clos_heap(x, r1);
	type_clos_heap(y, r2);
	return Result(ret, 1);
}

int subtypep_common(Execute ptr, addr x, addr y, addr env, addr *v1, addr *v2)
{
	int result, invalid;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva_force(hold, x, y, env, NULL);
	Return(subtypep_clos_p_(x, y, &x, &y, &result));
	if (! result) {
		Return(parse_type(ptr, &x, x, env));
		localhold_push(hold, x);
		Return(parse_type(ptr, &y, y, env));
		localhold_push(hold, y);
	}
	Return(subtypep_clang_(x, y, &result, &invalid));
	*v1 = result? T: Nil;
	*v2 = invalid? T: Nil;
	localhold_end(hold);

	return 0;
}

int subtypep_result_syscall(Execute ptr, addr left, addr right, addr *ret)
{
	SubtypepResult value;

	Return(parse_type(ptr, &left, left, Nil));
	Return(parse_type(ptr, &right, right, Nil));
	Return(subtypep_result_(left, right, 1, &value));
	switch (value) {
		case SUBTYPEP_INCLUDE:
			GetConst(SYSTEM_INCLUDE, ret);
			break;

		case SUBTYPEP_EXCLUDE:
			GetConst(SYSTEM_EXCLUDE, ret);
			break;

		case SUBTYPEP_FALSE:
			GetConst(SYSTEM_FALSE, ret);
			break;

		case SUBTYPEP_INVALID:
		default:
			GetConst(SYSTEM_INVALID, ret);
			break;
	}

	return 0;
}

