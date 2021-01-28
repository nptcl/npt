#include "condition.h"
#include "cons_list.h"
#include "local.h"
#include "type_copy.h"
#include "type_function.h"
#include "type_number.h"
#include "type_optimize.h"
#include "type_table.h"

void make_ordargs(ordargs *ptr, addr pos)
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

int gettype_ordargs_(const ordargs *ptr, size_t index, ordtype *ret)
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

int simple_p_ordargs(const ordargs *ptr)
{
	return ptr->rest == Nil && ptr->key == Nil;
}

static void merge_key_ordargs(LocalRoot local, addr *ret, const ordargs *ptr)
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

static void merge_value_ordargs(LocalRoot local, addr *ret, const ordargs *ptr)
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

void merge_ordargs(LocalRoot local, addr *ret, const ordargs *ptr, const ordtype *type)
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
		merge_key_ordargs(local, &pos, ptr);
		if (type->rest)
			type2and_local(local, type->type, pos, &pos);
		*ret = pos;
		return;
	}

	/* value */
	if (type->value) {
		merge_value_ordargs(local, &pos, ptr);
		if (type->rest)
			type2and_local(local, type->type, pos, &pos);
		*ret = pos;
		return;
	}

	/* error */
	*ret = 0;
	Abort("type error");
}


/*
 *  size_check
 */
static int size_check_recursive_ordcall_(addr pos, int value, size_t size, int *ret);

static int size_check_optimized_ordcall_(addr pos, int value, size_t size, int *ret)
{
	get_type_optimized(&pos, pos);
	return size_check_recursive_ordcall_(pos, value, size, ret);
}

static int size_check_subtypep_ordcall_(addr pos, int value, size_t size, int *ret)
{
	get_type_subtypep(&pos, pos);
	return size_check_recursive_ordcall_(pos, value, size, ret);
}

static int size_check_t_ordcall_(addr pos, int value, size_t size, int *ret)
{
	return Result(ret, value);
}

static int size_check_nil_ordcall_(addr pos, int value, size_t size, int *ret)
{
	return Result(ret, ! value);
}

static int size_check_not_ordcall_(addr pos, int value, size_t size, int *ret)
{
	GetArrayType(pos, 0, &pos);
	return size_check_recursive_ordcall_(pos, ! value, size, ret);
}

static int size_check_function_ordcall_(addr pos, int value, size_t size, int *ret)
{
	size_t check;
	addr x;

	Check(! type_function_p(pos), "type error");
	GetArrayType(pos, 0, &pos); /* args */

	/* asterisk */
	if (type_asterisk_p(pos))
		return Result(ret, value);

	/* var */
	GetArrayA2(pos, 0, &x);
	check = length_list_unsafe(x);
	if (size == check)
		return Result(ret, value);
	if (size < check)
		goto error;

	/* opt */
	GetArrayA2(pos, 1, &x);
	check += length_list_unsafe(x);
	if (size <= check)
		return Result(ret, value);

	/* key */
	GetArrayA2(pos, 3, &x);
	if (x != Nil) {
		check -= size;
		if (check % 2)
			return fmte_("There is no value in &key argument.", NULL);
		return Result(ret, value);
	}

	/* rest */
	GetArrayA2(pos, 2, &x);
	if (x != Nil)
		return Result(ret, value);

error:
	return Result(ret, ! value);
}

static int size_check_and_ordcall_(addr pos, int value, size_t size, int *ret)
{
	int loop_check, check;
	addr x;
	size_t loop, i;

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &loop);
	loop_check = 1;
	for (i = 0; i < loop; i++) {
		GetArrayA4(pos, i, &x);
		Return(size_check_recursive_ordcall_(x, 1, size, &check));
		if (! check)
			loop_check = 0;
	}
	if (! loop_check)
		value = ! value;

	return Result(ret, value);
}

static int size_check_or_ordcall_(addr pos, int value, size_t size, int *ret)
{
	int loop_check, check;
	addr x;
	size_t loop, i;

	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &loop);
	loop_check = 0;
	for (i = 0; i < loop; i++) {
		GetArrayA4(pos, i, &x);
		Return(size_check_recursive_ordcall_(x, 1, size, &check));
		if (check)
			loop_check = 1;
	}
	if (! loop_check)
		value = ! value;

	return Result(ret, value);
}

static int size_check_recursive_ordcall_(addr pos, int value, size_t size, int *ret)
{
	int notp;
	enum LISPDECL decl;

	CheckType(pos, LISPTYPE_TYPE);
	GetLispDecl(pos, &decl);
	GetNotDecl(pos, &notp);
	if (notp)
		value = ! value;

	switch (decl) {
		case LISPDECL_OPTIMIZED:
			return size_check_optimized_ordcall_(pos, value, size, ret);

		case LISPDECL_SUBTYPEP:
			return size_check_subtypep_ordcall_(pos, value, size, ret);

		case LISPDECL_ASTERISK:
		case LISPDECL_T:
			return size_check_t_ordcall_(pos, value, size, ret);

		case LISPDECL_NIL:
			return size_check_nil_ordcall_(pos, value, size, ret);

		case LISPDECL_NOT:
			return size_check_not_ordcall_(pos, value, size, ret);

		case LISPDECL_FUNCTION:
		case LISPDECL_COMPILED_FUNCTION:
			return size_check_function_ordcall_(pos, value, size, ret);

		case LISPDECL_AND:
			return size_check_and_ordcall_(pos, value, size, ret);

		case LISPDECL_OR:
			return size_check_or_ordcall_(pos, value, size, ret);

		default:
			return fmte_("Invalid type-specifier ~S.", pos, NULL);
	}
}

int size_check_ordcall_(addr pos, size_t size, int *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	*ret = 0;
	return size_check_recursive_ordcall_(pos, 1, size, ret);
}


/*
 *  gettype_ordcall
 */
static void reverse_ordcall(addr pos, int notp)
{
	CheckType(pos, LISPTYPE_TYPE);
	if (notp == 0)
		return;
	GetNotDecl(pos, &notp);
	notp = ! notp;
	SetNotDecl(pos, notp);
}

static int gettype_optimized_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	get_type_optimized(&pos, pos);
	Return(gettype_ordcall_(pos, i, &pos));
	reverse_ordcall(pos, notp);

	return Result(ret, pos);
}

static int gettype_subtypep_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	get_type_subtypep(&pos, pos);
	Return(gettype_ordcall_(pos, i, &pos));
	reverse_ordcall(pos, notp);

	return Result(ret, pos);
}

static void gettype_bool_ordcall(int value, addr *ret)
{
	if (value)
		type0_heap(LISPDECL_T, ret);
	else
		type0_heap(LISPDECL_NIL, ret);
}

static int gettype_t_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	gettype_bool_ordcall(! notp, ret);

	return 0;
}

static int gettype_nil_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	gettype_bool_ordcall(notp, ret);

	return 0;
}

static int gettype_not_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	notp = ! notp;
	GetArrayType(pos, 0, &pos);
	get_type_optimized(&pos, pos);
	Return(gettype_ordcall_(pos, i, &pos));
	reverse_ordcall(pos, notp);

	return Result(ret, pos);
}

static int gettype_function_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;
	ordargs str;
	ordtype type;
	LocalRoot local;
	LocalStack stack;

	Check(! type_function_p(pos), "type error");
	GetNotDecl(pos, &notp);
	GetArrayType(pos, 0, &pos); /* args */

	/* asterisk */
	if (type_asterisk_p(pos)) {
		gettype_bool_ordcall(! notp, ret);
		return 0;
	}

	/* ordargs */
	make_ordargs(&str, pos);
	Return(gettype_ordargs_(&str, i, &type));
	if (type.nil) {
		gettype_bool_ordcall(notp, ret);
		return 0;
	}

	local = Local_Thread;
	push_local(local, &stack);
	merge_ordargs(local, &pos, &str, &type);
	type_copy_heap(&pos, pos);
	reverse_ordcall(pos, notp);
	rollback_local(local, stack);

	return Result(ret, pos);
}

static int gettype_vector_ordcall_(enum LISPDECL decl, addr pos, size_t i, addr *ret)
{
	int notp;
	addr type, vector, x;
	size_t size, index;

	GetNotDecl(pos, &notp);
	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);

	vector4_heap(&vector, size);
	type1_heap(decl, vector, &type);
	SetNotDecl(type, notp);

	for (index = 0; index < size; index++) {
		GetArrayA4(pos, index, &x);
		Return(gettype_ordcall_(x, index, &x));
		SetArrayA4(vector, index, x);
	}

	return Result(ret, type);
}

static int gettype_and_ordcall_(addr pos, size_t i, addr *ret)
{
	return gettype_vector_ordcall_(LISPDECL_OR, pos, i, ret);
}

static int gettype_or_ordcall_(addr pos, size_t i, addr *ret)
{
	return gettype_vector_ordcall_(LISPDECL_AND, pos, i, ret);
}

static int gettype_error_ordcall_(addr pos, size_t i, addr *ret)
{
	int notp;

	GetNotDecl(pos, &notp);
	if (notp) {
		type0_heap(LISPDECL_T, ret);
		return 0;
	}

	*ret = Nil;
	return fmte_("Invalid type-specifier ~S in function type.", pos, NULL);
}

int gettype_ordcall_(addr pos, size_t i, addr *ret)
{
	CheckType(pos, LISPTYPE_TYPE);
	switch (RefLispDecl(pos)) {
		case LISPDECL_OPTIMIZED:
			return gettype_optimized_ordcall_(pos, i, ret);

		case LISPDECL_SUBTYPEP:
			return gettype_subtypep_ordcall_(pos, i, ret);

		case LISPDECL_ASTERISK:
		case LISPDECL_T:
			return gettype_t_ordcall_(pos, i, ret);

		case LISPDECL_NIL:
			return gettype_nil_ordcall_(pos, i, ret);

		case LISPDECL_NOT:
			return gettype_not_ordcall_(pos, i, ret);

		case LISPDECL_FUNCTION:
		case LISPDECL_COMPILED_FUNCTION:
			return gettype_function_ordcall_(pos, i, ret);

		case LISPDECL_AND:
			return gettype_and_ordcall_(pos, i, ret);

		case LISPDECL_OR:
			return gettype_or_ordcall_(pos, i, ret);

		default:
			return gettype_error_ordcall_(pos, i, ret);
	}
}


/*
 *  function values
 */
enum OrdValues_Type {
	OrdValues_Var,
	OrdValues_Opt,
	OrdValues_Rest
};
typedef enum OrdValues_Type ordvalues;

static void make_recursive_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype);

static void make_optimized_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;

	GetNotDecl(pos, &notp);
	get_type_optimized(&pos, pos);
	make_recursive_ordvalues(pos, i, &pos, rtype);
	reverse_ordcall(pos, notp);
	*ret = pos;
}

static void make_subtypep_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;

	GetNotDecl(pos, &notp);
	get_type_subtypep(&pos, pos);
	make_recursive_ordvalues(pos, i, &pos, rtype);
	reverse_ordcall(pos, notp);
	*ret = pos;
}

static void make_t_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;

	GetNotDecl(pos, &notp);
	gettype_bool_ordcall(! notp, ret);
	*rtype = (i == 0)? OrdValues_Var: OrdValues_Rest;
}

static void make_nil_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;

	GetNotDecl(pos, &notp);
	gettype_bool_ordcall(notp, ret);
	*rtype = (i == 0)? OrdValues_Var: OrdValues_Rest;
}

static void make_not_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;

	GetNotDecl(pos, &notp);
	notp = ! notp;
	GetArrayType(pos, 0, &pos);
	get_type_optimized(&pos, pos);
	make_recursive_ordvalues(pos, i, &pos, rtype);
	reverse_ordcall(pos, notp);
	*ret = pos;
}

static void make_function_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	int notp;
	addr x;
	size_t size;

	Check(! type_function_p(pos), "type error");
	GetNotDecl(pos, &notp);
	GetArrayType(pos, 1, &pos); /* values */

	/* asterisk */
	if (RefLispDecl(pos) != LISPDECL_VALUES) {
		make_recursive_ordvalues(pos, i, ret, rtype);
		return;
	}

	/* var */
	GetArrayA2(pos, 0, &x);
	size = length_list_unsafe(x);
	if (i < size) {
		getnth_unsafe(x, i, &x);
		*rtype = OrdValues_Var;
		goto result;
	}

	/* opt */
	GetArrayA2(pos, 1, &x);
	i -= size;
	size = length_list_unsafe(x);
	if (i < size) {
		getnth_unsafe(x, i, &x);
		*rtype = OrdValues_Opt;
		goto result;
	}

	/* rest */
	GetArrayA2(pos, 2, &x);
	*rtype = OrdValues_Rest;

result:
	type_copy_heap(&x, x);
	reverse_ordcall(x, notp);
	*ret = x;
}

static void make_vector_ordvalues(enum LISPDECL decl,
		addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	ordvalues v, check;
	int notp;
	addr type, vector, x;
	size_t size, index;

	GetNotDecl(pos, &notp);
	GetArrayType(pos, 0, &pos);
	LenArrayA4(pos, &size);

	vector4_heap(&vector, size);
	type1_heap(decl, vector, &type);
	SetNotDecl(type, notp);

	v = OrdValues_Rest;
	for (index = 0; index < size; index++) {
		GetArrayA4(pos, index, &x);
		make_recursive_ordvalues(x, i, &x, &check);
		SetArrayA4(vector, index, x);

		/* valeus check */
		if (check == OrdValues_Var)
			v = OrdValues_Var;
		else if (check == OrdValues_Opt && v == OrdValues_Rest)
			v = OrdValues_Opt;
	}

	*rtype = v;
	*ret = type;
}

static void make_and_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	make_vector_ordvalues(LISPDECL_AND, pos, i, ret, rtype);
}

static void make_or_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	make_vector_ordvalues(LISPDECL_OR, pos, i, ret, rtype);
}

static void make_type_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	if (i == 0) {
		type_copy_heap(ret, pos);
		*rtype = OrdValues_Var;
	}
	else {
		make_t_ordvalues(pos, i, ret, rtype);
	}
}

static void make_recursive_ordvalues(addr pos, size_t i, addr *ret, ordvalues *rtype)
{
	CheckType(pos, LISPTYPE_TYPE);
	switch (RefLispDecl(pos)) {
		case LISPDECL_OPTIMIZED:
			make_optimized_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_SUBTYPEP:
			make_subtypep_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_ASTERISK:
		case LISPDECL_T:
			make_t_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_NIL:
			make_nil_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_NOT:
			make_not_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_FUNCTION:
		case LISPDECL_COMPILED_FUNCTION:
			make_function_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_AND:
			make_and_ordvalues(pos, i, ret, rtype);
			break;

		case LISPDECL_OR:
			make_or_ordvalues(pos, i, ret, rtype);
			break;

		default:
			make_type_ordvalues(pos, i, ret, rtype);
			break;
	}
}

void make_ordvalues_heap(addr pos, addr *ret)
{
	ordvalues type;
	addr x, var, opt, rest;
	size_t i;

	var = opt = rest = Nil;
	for (i = 0; ; i++) {
		make_recursive_ordvalues(pos, i, &x, &type);
		/* var */
		if (type == OrdValues_Var) {
			cons_heap(&var, x, var);
			continue;
		}
		/* opt */
		if (type == OrdValues_Opt) {
			cons_heap(&opt, x, opt);
			continue;
		}
		/* rest */
		rest = x;
		break;
	}
	nreverse(&var, var);
	nreverse(&opt, opt);

	/* type */
	type4_heap(LISPDECL_VALUES, var, opt, rest, Nil, ret);
}

