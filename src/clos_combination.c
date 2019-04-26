#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "eval.h"
#include "lambda.h"
#include "object.h"
#include "package.h"
#include "sequence.h"
#include "symbol.h"
#include <stdarg.h>

/*
 *  long-method-combination
 */
static void stdget_longcomb_constant(addr pos, addr *ret,
		enum Clos_longcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_LONG_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_checkelt(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		clos_check(pos, check, ret);
	}
}

static void stdset_longcomb_constant(addr pos, addr value,
		enum Clos_longcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_LONG_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
	}
	else {
		GetConstant(index2, &check);
		clos_set(pos, check, value);
	}
}
#define StdGetLongCombination(p,r,a,b) \
	stdget_longcomb_constant((p), (r), Clos_longcomb_##a, CONSTANT_CLOSKEY_##b)
#define StdSetLongCombination(p,r,a,b) \
	stdset_longcomb_constant((p), (r), Clos_longcomb_##a, CONSTANT_CLOSKEY_##b)

void stdget_longcomb_name(addr pos, addr *ret)
{
	StdGetLongCombination(pos, ret, name, NAME);
}
void stdset_longcomb_name(addr pos, addr value)
{
	StdSetLongCombination(pos, value, name, NAME);
}

void stdget_longcomb_document(addr pos, addr *ret)
{
	StdGetLongCombination(pos, ret, document, DOCUMENT);
}
void stdset_longcomb_document(addr pos, addr value)
{
	StdSetLongCombination(pos, value, document, DOCUMENT);
}

void stdget_longcomb_lambda_list(addr pos, addr *ret)
{
	StdGetLongCombination(pos, ret, lambda_list, LAMBDA_LIST);
}
void stdset_longcomb_lambda_list(addr pos, addr value)
{
	StdSetLongCombination(pos, value, lambda_list, LAMBDA_LIST);
}

void stdget_longcomb_binding(addr pos, addr *ret)
{
	StdGetLongCombination(pos, ret, binding, BINDING);
}
void stdset_longcomb_binding(addr pos, addr value)
{
	StdSetLongCombination(pos, value, binding, BINDING);
}

void stdget_longcomb_qualifiers(addr pos, addr *ret)
{
	StdGetLongCombination(pos, ret, qualifiers, QUALIFIERS);
}
void stdset_longcomb_qualifiers(addr pos, addr value)
{
	StdSetLongCombination(pos, value, qualifiers, QUALIFIERS);
}

void stdget_longcomb_arguments(addr pos, addr *ret)
{
	StdGetLongCombination(pos, ret, arguments, ARGUMENTS);
}
void stdset_longcomb_arguments(addr pos, addr value)
{
	StdSetLongCombination(pos, value, arguments, ARGUMENTS);
}

void stdget_longcomb_generic(addr pos, addr *ret)
{
	StdGetLongCombination(pos, ret, generic, GENERIC);
}
void stdset_longcomb_generic(addr pos, addr value)
{
	StdSetLongCombination(pos, value, generic, GENERIC);
}

void stdget_longcomb_form(addr pos, addr *ret)
{
	StdGetLongCombination(pos, ret, form, FORM);
}
void stdset_longcomb_form(addr pos, addr value)
{
	StdSetLongCombination(pos, value, form, FORM);
}

void stdget_longcomb_function(addr pos, addr *ret)
{
	StdGetLongCombination(pos, ret, function, FUNCTION);
}
void stdset_longcomb_function(addr pos, addr value)
{
	StdSetLongCombination(pos, value, function, FUNCTION);
}


/*
 *  short-method-combination
 */
static void stdget_shortcomb_constant(addr pos, addr *ret,
		enum Clos_shortcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_SHORT_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_checkelt(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		clos_check(pos, check, ret);
	}
}

static void stdset_shortcomb_constant(addr pos, addr value,
		enum Clos_shortcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_SHORT_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
	}
	else {
		GetConstant(index2, &check);
		clos_set(pos, check, value);
	}
}
#define StdGetShortCombination(p,r,a,b) \
	stdget_shortcomb_constant((p), (r), Clos_shortcomb_##a, CONSTANT_CLOSKEY_##b)
#define StdSetShortCombination(p,r,a,b) \
	stdset_shortcomb_constant((p), (r), Clos_shortcomb_##a, CONSTANT_CLOSKEY_##b)

void stdget_shortcomb_name(addr pos, addr *ret)
{
	StdGetShortCombination(pos, ret, name, NAME);
}
void stdset_shortcomb_name(addr pos, addr value)
{
	StdSetShortCombination(pos, value, name, NAME);
}

void stdget_shortcomb_document(addr pos, addr *ret)
{
	StdGetShortCombination(pos, ret, document, DOCUMENT);
}
void stdset_shortcomb_document(addr pos, addr value)
{
	StdSetShortCombination(pos, value, document, DOCUMENT);
}

void stdget_shortcomb_identity(addr pos, addr *ret)
{
	StdGetShortCombination(pos, ret, identity, IDENTITY);
}
void stdset_shortcomb_identity(addr pos, addr value)
{
	StdSetShortCombination(pos, value, identity, IDENTITY);
}

void stdget_shortcomb_operator(addr pos, addr *ret)
{
	StdGetShortCombination(pos, ret, operator, OPERATOR);
}
void stdset_shortcomb_operator(addr pos, addr value)
{
	StdSetShortCombination(pos, value, operator, OPERATOR);
}

void stdget_shortcomb_order(addr pos, addr *ret)
{
	StdGetShortCombination(pos, ret, order, ORDER);
}
void stdset_shortcomb_order(addr pos, addr value)
{
	StdSetShortCombination(pos, value, order, ORDER);
}


/*
 *  define-long-method-combination
 */
static void stdget_longdef_constant(addr pos, addr *ret,
		enum Clos_longdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_checkelt(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		clos_check(pos, check, ret);
	}
}

static void stdset_longdef_constant(addr pos, addr value,
		enum Clos_longdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
	}
	else {
		GetConstant(index2, &check);
		clos_set(pos, check, value);
	}
}
#define StdGetLongDefineCombination(p,r,a,b) \
	stdget_longdef_constant((p), (r), Clos_longdef_##a, CONSTANT_CLOSKEY_##b)
#define StdSetLongDefineCombination(p,r,a,b) \
	stdset_longdef_constant((p), (r), Clos_longdef_##a, CONSTANT_CLOSKEY_##b)

void stdget_longdef_name(addr pos, addr *ret)
{
	StdGetLongDefineCombination(pos, ret, name, NAME);
}
void stdset_longdef_name(addr pos, addr value)
{
	StdSetLongDefineCombination(pos, value, name, NAME);
}

void stdget_longdef_document(addr pos, addr *ret)
{
	StdGetLongDefineCombination(pos, ret, document, DOCUMENT);
}
void stdset_longdef_document(addr pos, addr value)
{
	StdSetLongDefineCombination(pos, value, document, DOCUMENT);
}

void stdget_longdef_lambda_list(addr pos, addr *ret)
{
	StdGetLongDefineCombination(pos, ret, lambda_list, LAMBDA_LIST);
}
void stdset_longdef_lambda_list(addr pos, addr value)
{
	StdSetLongDefineCombination(pos, value, lambda_list, LAMBDA_LIST);
}

void stdget_longdef_qualifiers(addr pos, addr *ret)
{
	StdGetLongDefineCombination(pos, ret, qualifiers, QUALIFIERS);
}
void stdset_longdef_qualifiers(addr pos, addr value)
{
	StdSetLongDefineCombination(pos, value, qualifiers, QUALIFIERS);
}

void stdget_longdef_arguments(addr pos, addr *ret)
{
	StdGetLongDefineCombination(pos, ret, arguments, ARGUMENTS);
}
void stdset_longdef_arguments(addr pos, addr value)
{
	StdSetLongDefineCombination(pos, value, arguments, ARGUMENTS);
}

void stdget_longdef_generic(addr pos, addr *ret)
{
	StdGetLongDefineCombination(pos, ret, generic, GENERIC);
}
void stdset_longdef_generic(addr pos, addr value)
{
	StdSetLongDefineCombination(pos, value, generic, GENERIC);
}

void stdget_longdef_form(addr pos, addr *ret)
{
	StdGetLongDefineCombination(pos, ret, form, FORM);
}
void stdset_longdef_form(addr pos, addr value)
{
	StdSetLongDefineCombination(pos, value, form, FORM);
}


/*
 *  define-short-method-combination
 */
static void stdget_shortdef_constant(addr pos, addr *ret,
		enum Clos_shortdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_checkelt(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		clos_check(pos, check, ret);
	}
}

static void stdset_shortdef_constant(addr pos, addr value,
		enum Clos_shortdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
	}
	else {
		GetConstant(index2, &check);
		clos_set(pos, check, value);
	}
}
#define StdGetShortDefineCombination(p,r,a,b) \
	stdget_shortdef_constant((p), (r), Clos_shortdef_##a, CONSTANT_CLOSKEY_##b)
#define StdSetShortDefineCombination(p,r,a,b) \
	stdset_shortdef_constant((p), (r), Clos_shortdef_##a, CONSTANT_CLOSKEY_##b)

void stdget_shortdef_name(addr pos, addr *ret)
{
	StdGetShortDefineCombination(pos, ret, name, NAME);
}
void stdset_shortdef_name(addr pos, addr value)
{
	StdSetShortDefineCombination(pos, value, name, NAME);
}

void stdget_shortdef_document(addr pos, addr *ret)
{
	StdGetShortDefineCombination(pos, ret, document, DOCUMENT);
}
void stdset_shortdef_document(addr pos, addr value)
{
	StdSetShortDefineCombination(pos, value, document, DOCUMENT);
}

void stdget_shortdef_identity(addr pos, addr *ret)
{
	StdGetShortDefineCombination(pos, ret, identity, IDENTITY);
}
void stdset_shortdef_identity(addr pos, addr value)
{
	StdSetShortDefineCombination(pos, value, identity, IDENTITY);
}

void stdget_shortdef_operator(addr pos, addr *ret)
{
	StdGetShortDefineCombination(pos, ret, operator, OPERATOR);
}
void stdset_shortdef_operator(addr pos, addr value)
{
	StdSetShortDefineCombination(pos, value, operator, OPERATOR);
}


/*****************************************************************************
 *  check-qualifiers
 *****************************************************************************/
static int qualifiers_equal_list(addr left, addr right)
{
	addr left2, right2, asterisk;

	if (left == Nil && right == Nil) return 1;
	if (! consp(left)) return 0;
	if (! consp(right)) return 0;
	GetCons(left, &left, &left2);
	GetCons(right, &right, &right2);
	GetConst(COMMON_ASTERISK, &asterisk);
	if ((right != asterisk) && (left != right)) return 0;
	if (right2 == asterisk) return 1;
	return qualifiers_equal_list(left2, right2);
}

static int qualifiers_equal_symbol(Execute ptr, addr left, addr right)
{
	int result;
	addr control, call;

	push_close_control(ptr, &control);
	conscar_local(ptr->local, &left, left);
	getfunctioncheck_local(ptr, right, &call);
	if (apply_control(ptr, call, left))
		goto error;
	getresult_control(ptr, &left);
	result = left != Nil;
	if (free_control(ptr, control))
		goto error;

	return result;

error:
	fmte("The predicate ~S don't execute jump operation.", right, NULL);
	return 0;
}

static int qualifiers_equal(Execute ptr, addr left, addr right)
{
	addr aster;

	GetConst(COMMON_ASTERISK, &aster);
	if (right == aster)
		return 1;
	if (listp(right))
		return qualifiers_equal_list(left, right);
	if (symbolp(right))
		return qualifiers_equal_symbol(ptr, left, right);

	/* error */
	fmte("Invalid method-combination-eualifiers ~S.", right, NULL);
	return 0;
}

static int check_qualifiers_equal_long(Execute ptr, addr comb, addr qua)
{
	addr cons, list;

	stdget_longcomb_qualifiers(comb, &cons);
	while (cons != Nil) {
		GetCons(cons, &list, &cons);
		/* cadr */
		GetCdr(list, &list);
		GetCar(list, &list);
		/* check */
		if (qualifiers_equal(ptr, qua, list)) return 1;
	}

	return 0;
}

static int check_qualifiers_equal_short(addr comb, addr qua)
{
	addr name, check;

	if (! consp(qua)) return 0;
	GetCons(qua, &qua, &check);
	if (check != Nil) return 0;
	stdget_shortcomb_name(comb, &name);
	if (qua == name) return 1;
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check) return 1;

	return 0;
}

static int check_qualifiers_equal_standard(addr qua)
{
	addr check;

	/* primary */
	if (qua == Nil) return 1;
	if (! consp(qua)) return 0;
	GetCons(qua, &qua, &check);
	if (check != Nil) return 0;
	/* around, before, after */
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check) return 1;
	GetConst(KEYWORD_BEFORE, &check);
	if (qua == check) return 1;
	GetConst(KEYWORD_AFTER, &check);
	if (qua == check) return 1;

	return 0;
}

int check_qualifiers_equal(Execute ptr, addr comb, addr qua)
{
	if (comb == Nil)
		return check_qualifiers_equal_standard(qua);
	if (clos_long_combination_p(comb))
		return check_qualifiers_equal_long(ptr, comb, qua);
	if (clos_short_combination_p(comb))
		return check_qualifiers_equal_short(comb, qua);
	/* error */
	fmte("Invalid method-combination instance ~S.", comb, NULL);
	return 0;
}


/*****************************************************************************
 *  qualifiers-position
 *****************************************************************************/
void method_combination_qualifiers_count(addr comb, size_t *ret)
{
	if (comb == Nil) {
		*ret = Clos_standard_size;
		return;
	}
	if (clos_long_combination_p(comb)) {
		stdget_longcomb_qualifiers(comb, &comb);
		*ret = length_list_unsafe(comb);
		return;
	}
	if (clos_short_combination_p(comb)) {
		*ret = Clos_short_size;
		return;
	}
	/* error */
	fmte("Invalid method-combination instance ~S.", comb, NULL);
}

static int qualifiers_position_standard_nil(addr qua, size_t *ret)
{
	addr check;

	/* primary */
	if (qua == Nil) {
		*ret = Clos_standard_primary;
		return 0;
	}
	if (! consp(qua))
		return 1;
	GetCons(qua, &qua, &check);
	if (check != Nil)
		return 1;
	/* around, before, after */
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check) {
		*ret = Clos_standard_around;
		return 0;
	}
	GetConst(KEYWORD_BEFORE, &check);
	if (qua == check) {
		*ret = Clos_standard_before;
		return 0;
	}
	GetConst(KEYWORD_AFTER, &check);
	if (qua == check) {
		*ret = Clos_standard_after;
		return 0;
	}

	return 1;
}

static int qualifiers_position_long_nil(Execute ptr, addr qua, addr comb, size_t *ret)
{
	addr cons, list;
	size_t index;

	stdget_longcomb_qualifiers(comb, &cons);
	for (index = 0; cons != Nil; index++) {
		GetCons(cons, &list, &cons);
		/* cadr */
		GetCdr(list, &list);
		GetCar(list, &list);
		/* check */
		if (qualifiers_equal(ptr, qua, list)) {
			*ret = index;
			return 0;
		}
	}

	return 1;
}

static int qualifiers_position_short_nil(addr qua, addr comb, size_t *ret)
{
	addr name, check;

	if (! consp(qua))
		return 1;
	GetCons(qua, &qua, &check);
	if (check != Nil)
		return 1;
	stdget_shortcomb_name(comb, &name);
	if (qua == name) {
		*ret = Clos_short_primary;
		return 0;
	}
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check) {
		*ret = Clos_short_around;
		return 0;
	}

	return 1;
}

int qualifiers_position_nil(Execute ptr, addr qua, addr comb, size_t *ret)
{
	Check(clos_define_combination_p(comb), "type error");
	if (comb == Nil)
		return qualifiers_position_standard_nil(qua, ret);
	if (clos_long_combination_p(comb))
		return qualifiers_position_long_nil(ptr, qua, comb, ret);
	if (clos_short_combination_p(comb))
		return qualifiers_position_short_nil(qua, comb, ret);
	/* error */
	fmte("Invalid method-combination type ~S.", comb, NULL);
	return 0;
}

void qualifiers_position(Execute ptr, addr qua, addr comb, size_t *ret)
{
	if (qualifiers_position_nil(ptr, qua, comb, ret))
		fmte("The qualifiers ~S is not found.", qua, NULL);
}


/*****************************************************************************
 *  standard method-combination
 *****************************************************************************/
static void build_clos_method_combination_standard(void)
{
	addr clos, inst, name;

	GetConst(CLOS_LONG_METHOD_COMBINATION, &clos);
	clos_instance_heap(clos, &inst);
	GetConst(COMMON_STANDARD, &name);
	stdset_longcomb_name(inst, name);
	SetConst(CLOS_COMBINATION_STANDARD, inst);
}

static void build_clos_method_combination_short(constindex n, int ident)
{
	addr clos, inst, name;

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &clos);
	clos_instance_heap(clos, &inst);
	GetConstant(n, &name);
	stdset_shortdef_name(inst, name);
	stdset_shortdef_document(inst, Nil);
	stdset_shortdef_identity(inst, ident? T: Nil);
	stdset_shortdef_operator(inst, name);
	clos_define_combination(name, inst);
}

void build_clos_combination(void)
{
	build_clos_method_combination_standard();
	build_clos_method_combination_short(CONSTANT_COMMON_PLUS, 1);
	build_clos_method_combination_short(CONSTANT_COMMON_AND, 1);
	build_clos_method_combination_short(CONSTANT_COMMON_APPEND, 1);
	build_clos_method_combination_short(CONSTANT_COMMON_LIST, 0);
	build_clos_method_combination_short(CONSTANT_COMMON_MAX, 1);
	build_clos_method_combination_short(CONSTANT_COMMON_MIN, 1);
	build_clos_method_combination_short(CONSTANT_COMMON_NCONC, 1);
	build_clos_method_combination_short(CONSTANT_COMMON_OR, 1);
	build_clos_method_combination_short(CONSTANT_COMMON_PROGN, 1);
}


/*
 *  generic-function
 */
static int clos_method_combination_standard_p(addr pos)
{
	addr check;
	GetConst(COMMON_STANDARD, &check);
	return pos == check;
}

static void clos_method_combination_standard(addr comb, addr list, addr *ret)
{
	if (list != Nil)
		fmte("Invalid STANDARD method-combination arguments ~S.", list, NULL);
	*ret = Nil;
}

static void clos_method_combination_long(addr comb, addr list, addr *ret)
{
	addr pos, value;

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	clos_instance_heap(pos, &pos);
	/* copy */
	stdget_longdef_name(comb, &value);
	stdset_longcomb_name(pos, value);
	stdget_longdef_document(comb, &value);
	stdset_longcomb_document(pos, value);
	stdget_longdef_lambda_list(comb, &value);
	stdset_longcomb_lambda_list(pos, value);
	stdget_longdef_qualifiers(comb, &value);
	stdset_longcomb_qualifiers(pos, value);
	stdget_longdef_arguments(comb, &value);
	stdset_longcomb_arguments(pos, value);
	stdget_longdef_generic(comb, &value);
	stdset_longcomb_generic(pos, value);
	stdget_longdef_form(comb, &value);
	stdset_longcomb_form(pos, value);
	/* binding */
	stdset_longcomb_binding(pos, list);
	/* result */
	*ret = pos;
}

static void clos_method_combination_short_arguments(addr list, addr *ret)
{
	addr check, first, last;

	GetConst(KEYWORD_MOST_SPECIFIC_FIRST, &first);
	GetConst(KEYWORD_MOST_SPECIFIC_LAST, &last);
	if (list == Nil) {
		*ret = first;
		return;
	}
	if (singlep(list)) {
		GetCar(list, &check);
		if (check != first && check != last)
			goto error;
		*ret = check;
		return;
	}
	/* error */
error:
	fmte("METHOD-COMBINATION ~S argument must be a "
			":most-specific-first or :most-specific-last.", list, NULL);
	*ret = 0;
}

static void clos_method_combination_short(addr comb, addr list, addr *ret)
{
	addr pos, value, order;

	/* (&optional argument-precedence-order) */
	clos_method_combination_short_arguments(list, &order);
	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	clos_instance_heap(pos, &pos);
	/* copy */
	stdget_shortdef_name(comb, &value);
	stdset_shortcomb_name(pos, value);
	stdget_shortdef_document(comb, &value);
	stdset_shortcomb_document(pos, value);
	stdget_shortdef_identity(comb, &value);
	stdset_shortcomb_identity(pos, value);
	stdget_shortdef_operator(comb, &value);
	stdset_shortcomb_operator(pos, value);
	/* argument-precedence-order */
	stdset_shortcomb_order(pos, order);
	/* result */
	*ret = pos;
}

void clos_find_method_combination(addr gen, addr list, addr *ret)
{
	addr pos, tail;

	Check(! clos_generic_p(gen), "type error");
	Check(! consp(list), "type error");
	GetCons(list, &pos, &tail);

	/* standard */
	if (clos_method_combination_standard_p(pos)) {
		clos_method_combination_standard(pos, tail, ret);
		return;
	}

	/* long form */
	clos_find_combination(pos, &pos);
	if (clos_define_long_combination_p(pos)) {
		clos_method_combination_long(pos, tail, ret);
		return;
	}

	/* short form */
	if (clos_define_short_combination_p(pos)) {
		clos_method_combination_short(pos, tail, ret);
		return;
	}

	/* error */
	fmte("Invalid method-combination instance ~S.", list, NULL);
}


/*
 *  ensure-define-combination
 */
void ensure_define_combination_short_common(addr name, addr doc, addr ident, addr oper)
{
	addr pos;

	/* instance */
	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	clos_instance_heap(pos, &pos);
	stdset_shortdef_name(pos, name);
	stdset_shortdef_document(pos, doc);
	stdset_shortdef_identity(pos, ident);
	stdset_shortdef_operator(pos, oper);
	/* define-combination */
	clos_define_combination(name, pos);
}

void ensure_define_combination_long_common(addr name, addr lambda, addr spec,
		addr args, addr gen, addr doc, addr form)
{
	addr pos;

	/* instance */
	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	clos_instance_heap(pos, &pos);
	stdset_longdef_name(pos, name);
	stdset_longdef_lambda_list(pos, lambda);
	stdset_longdef_qualifiers(pos, spec);
	stdset_longdef_arguments(pos, args);
	stdset_longdef_generic(pos, gen);
	stdset_longdef_document(pos, doc);
	stdset_longdef_form(pos, form);
	/* define-combination */
	clos_define_combination(name, pos);
}


/*
 *  long form
 */
static void comb_longmacro_lambda(addr *ret, addr args,
		addr gen, addr inst, addr array, addr decl, addr form)
{
	/* `(lambda (generic inst array)
	 *    (declare (ignorable generic inst array))
	 *    (destructuring-bind ,args (combination-binding inst)
	 *      ,declarations
	 *      ,form))
	 * 
	 * `(lambda (generic inst array)
	 *    (declare (ignorable generic inst array))
	 *    ,declarations
	 *    ,form)
	 */
	addr root, pos;
	addr lambda, declare, ignorable, dbind, call;

	/* form */
	for (root = Nil; decl != Nil; ) {
		GetCons(decl, &pos, &decl);
		cons_heap(&root, pos, root);
	}
	cons_heap(&root, form, root);
	nreverse_list_unsafe(&form, root);

	/* destructuring-bind */
	if (args != Nil) {
		GetConst(COMMON_DESTRUCTURING_BIND, &dbind);
		GetConst(CLOSNAME_COMBINATION_BINDING, &call);
		list_heap(&call, call, inst, NULL);
		lista_heap(&form, dbind, args, call, form, NULL);
	}

	/* lambda */
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	list_heap(&ignorable, ignorable, gen, inst, array, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&gen, gen, inst, array, NULL);
	if (args != Nil)
		list_heap(ret, lambda, gen, declare, form, NULL);
	else
		lista_heap(ret, lambda, gen, declare, form, NULL);
}

static void comb_longmacro_variables(addr args, addr *ret)
{
	addr root, list, pos;

	Check(ArgumentStruct(args)->type != ArgumentType_combination, "type error");
	root = Nil;
	/* var */
	GetArgument(args, ArgumentIndex_var, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		cons_heap(&root, pos, root);
	}
	/* opt */
	GetArgument(args, ArgumentIndex_opt, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);
		cons_heap(&root, pos, root);
	}
	/* rest */
	GetArgument(args, ArgumentIndex_rest, &pos);
	if (pos != Nil)
		cons_heap(&root, pos, root);
	/* key */
	GetArgument(args, ArgumentIndex_key, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);
		cons_heap(&root, pos, root);
	}
	/* aux */
	GetArgument(args, ArgumentIndex_aux, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);
		cons_heap(&root, pos, root);
	}
	/* whole */
	GetArgument(args, ArgumentIndex_whole, &pos);
	if (pos != Nil)
		cons_heap(&root, pos, root);
	/* result */
	nreverse_list_unsafe(ret, root);
}

static void comb_longmacro_arguments(addr *ret, addr args, addr form)
{
	/* `(let ((var1 'var1)
	 *        (var2 'var2)
	 *        ...
	 *        (auxN 'auxN)
	 *        (whole 'whole))
	 *    (declare (ignorable ...))
	 *    ,form)
	 */
	addr root, vars, list, pos, value;
	addr quote, declare, ignorable, let;

	/* no :arguments */
	if (args == Nil) {
		*ret = form;
		return;
	}

	/* args */
	GetConst(COMMON_QUOTE, &quote);
	comb_longmacro_variables(args, &vars);
	root = Nil;
	list = vars;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		list_heap(&value, quote, pos, NULL);
		list_heap(&pos, pos, value, NULL);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(&root, root);
	/* declare */
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, list);
	list_heap(&declare, declare, ignorable, NULL);
	/* let */
	GetConst(COMMON_LET, &let);
	list_heap(ret, let, root, declare, form, NULL);
}

static void comb_longmacro_form(addr *ret,
		addr spec, addr gens, addr gen, addr array, addr form)
{
	/* `(let* ((,spec0 (qualifiers-elt 'spec0 ,array 0 order0 required0))
	 *         (,spec1 (qualifiers-elt 'spec1 ,array 1 order1 required1))
	 *         (,gens ,gen))
	 *    (declare (ignorable spec0 ... ,gens))
	 *    ,@form)
	 */
	addr args, vars, pos, temp;
	addr name, order, req, nameq;
	addr elt, quote, declare, ignorable, leta;
	fixnum i;

	args = vars = Nil;
	/* specializers */
	GetConst(CLOSNAME_QUALIFIERS_ELT, &elt);
	GetConst(COMMON_QUOTE, &quote);
	for (i = 0; spec != Nil; i++) {
		getcons(spec, &pos, &spec);
		list_bind(pos, &name, &temp, &order, &req, &temp, NULL);
		fixnum_heap(&pos, i);
		list_heap(&nameq, quote, name, NULL);
		list_heap(&pos, elt, nameq, array, pos, order, req, NULL);
		list_heap(&pos, name, pos, NULL);
		cons_heap(&vars, name, vars);
		cons_heap(&args, pos, args);
	}
	/* :generic-function */
	if (gens != Unbound) {
		list_heap(&pos, gens, gen, NULL);
		cons_heap(&args, pos, args);
		cons_heap(&vars, gens, vars);
	}
	/* ignorable */
	GetConst(COMMON_IGNORABLE, &ignorable);
	nreverse_list_unsafe(&vars, vars);
	cons_heap(&vars, ignorable, vars);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, vars, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	nreverse_list_unsafe(&args, args);
	lista_heap(ret, leta, args, declare, form, NULL);
}

void comb_longmacro(addr *ret,
		addr lambda, addr spec, addr args, addr gens, addr decl, addr form)
{
	addr gen, inst, array;

	make_symbolchar(&gen, "GENERIC");
	make_symbolchar(&inst, "COMBINATION");
	make_symbolchar(&array, "QUALIFIERS");

	comb_longmacro_form(&form, spec, gens, gen, array, form);
	comb_longmacro_arguments(&form, args, form);
	comb_longmacro_lambda(ret, lambda, gen, inst, array, decl, form);
}

static void comb_longform_macrolet(addr *ret, addr args, addr gen, addr form)
{
	/*  `(lambda (&rest ,args)
	 *     (declare (ignorable ,args))
	 *     (macrolet ((make-method (#:expr)
	 *                  (macro-make-method ,gen #:expr))
	 *                (call-method (#:car &optional #:cdr)
	 *                  (macro-call-method #:car #:cdr (quote ,args))))
	 *       ,form))
	 */
	addr lambda, declare, ignorable, macrolet, quote, rest, optional;
	addr make, call, mmake, mcall;
	addr expr, car, cdr;

	/* constant */
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_MAKE_METHOD, &make);
	GetConst(COMMON_CALL_METHOD, &call);
	GetConst(AMPERSAND_REST, &rest);
	GetConst(AMPERSAND_OPTIONAL, &optional);
	GetConst(CLOSNAME_MACRO_MAKE_METHOD, &mmake);
	GetConst(CLOSNAME_MACRO_CALL_METHOD, &mcall);
	make_symbolchar(&expr, "EXPR");
	make_symbolchar(&car, "CAR");
	make_symbolchar(&cdr, "CDR");
	/* macro */
	list_heap(&quote, quote, args, NULL);
	list_heap(&mcall, mcall, car, cdr, quote, NULL);
	list_heap(&car, car, optional, cdr, NULL);
	list_heap(&call, call, car, mcall, NULL);
	list_heap(&mmake, mmake, gen, expr, NULL);
	list_heap(&expr, expr, NULL);
	list_heap(&make, make, expr, mmake, NULL);
	list_heap(&make, make, call, NULL);
	list_heap(&macrolet, macrolet, make, form, NULL);
	list_heap(&ignorable, ignorable, args, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&rest, rest, args, NULL);
	list_heap(ret, lambda, rest, declare, macrolet, NULL);
}

static void comb_longmacro_lambda_list(addr args, addr *ret)
{
	addr root, var, list, a, b;
	struct argument_struct *str;


	str = ArgumentStruct(args);
	Check(str->type != ArgumentType_combination, "type error");

	root = Nil;
	/* whole */
	if (str->whole) {
		GetConst(AMPERSAND_WHOLE, &var);
		cons_heap(&root, var, root);
		GetArgument(args, ArgumentIndex_whole, &var);
		cons_heap(&root, var, root);
	}

	/* var & opt */
	if (str->var || str->opt) {
		GetConst(AMPERSAND_OPTIONAL, &var);
		cons_heap(&root, var, root);
	}

	/* var */
	GetArgument(args, ArgumentIndex_var, &list);
	while (list != Nil) {
		GetCons(list, &var, &list);
		cons_heap(&root, var, root);
	}

	/* opt */
	GetArgument(args, ArgumentIndex_opt, &list);
	while (list != Nil) {
		GetCons(list, &var, &list);
		cons_heap(&root, var, root);
	}

	/* rest */
	if (str->rest) {
		GetConst(AMPERSAND_REST, &var);
		cons_heap(&root, var, root);
		GetArgument(args, ArgumentIndex_rest, &var);
		cons_heap(&root, var, root);
	}

	/* key */
	if (str->keyp || str->key) {
		GetConst(AMPERSAND_KEY, &var);
		cons_heap(&root, var, root);
	}
	GetArgument(args, ArgumentIndex_key, &list);
	while (list != Nil) {
		GetCons(list, &var, &list);
		list_bind(var, &a, &b, NULL);
		if (b == Nil) {
			cons_heap(&root, a, root);
		}
		else {
			list_heap(&var, b, a, NULL);
			cons_heap(&root, var, root);
		}
	}

	/* key */
	if (str->allow) {
		GetConst(AMPERSAND_ALLOW, &var);
		cons_heap(&root, var, root);
	}

	/* result */
	nreverse_list_unsafe(ret, root);
}

static void comb_longform_arguments(addr *ret, addr args, addr comb, addr form)
{
	addr list, lambda;
	addr dbind, declare, ignorable;

	/* no :arguments */
	stdget_longcomb_arguments(comb, &comb);
	if (comb == Nil) {
		*ret = form;
		return;
	}

	/* (destructuring-bind ,arguments ,args
	 *   (declare (ignorable ...))
	 *   ,form)
	 */
	comb_longmacro_variables(comb, &list);
	comb_longmacro_lambda_list(comb, &lambda);
	/* declare */
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, list);
	list_heap(&declare, declare, ignorable, NULL);
	/* result */
	GetConst(COMMON_DESTRUCTURING_BIND, &dbind);
	list_heap(ret, dbind, lambda, args, declare, form, NULL);
}

int comb_longform(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr control, pos, args;

	/* execute */
	stdget_longcomb_form(comb, &pos);
	push_close_control(ptr, &control);
	if (funcall_control(ptr, pos, gen, comb, data, NULL))
		return runcode_free_control(ptr, control);
	getresult_control(ptr, &pos);
	if (free_control(ptr, control))
		return 1;

	/* make-form */
	make_symbolchar(&args, "ARGS");
	comb_longform_arguments(&pos, args, comb, pos);
	comb_longform_macrolet(&pos, args, gen, pos);

	/* eval */
	push_close_control(ptr, &control);
	if (eval_execute(ptr, pos))
		return runcode_free_control(ptr, control);
	getresult_control(ptr, ret);
	return free_control(ptr, control);
}

static void comb_shortform_primary(addr *ret, addr comb, addr list)
{
	addr check, call, root;

	GetConst(COMMON_CALL_METHOD, &call);
	stdget_shortcomb_identity(comb, &check);
	if (check != Nil && singlep(list)) {
		GetCar(list, &list);
		list_heap(ret, call, list, NULL);
	}
	else {
		stdget_shortcomb_operator(comb, &check);
		conscar_heap(&root, check);
		while (list != Nil) {
			getcons(list, &check, &list);
			list_heap(&check, call, check, NULL);
			cons_heap(&root, check, root);
		}
		nreverse_list_unsafe(ret, root);
	}
}

static void comb_shortform_around(addr *ret, addr comb, addr list, addr form)
{
	addr pos, root, car, cdr;

	if (list == Nil) {
		*ret = form;
		return;
	}

	getcons(list, &car, &cdr);
	/* ,@(cdr around) */
	for (root = Nil; cdr != Nil; ) {
		getcons(cdr, &pos, &cdr);
		cons_heap(&root, pos, root);
	}
	/* (make-method ,form) */
	GetConst(COMMON_MAKE_METHOD, &pos);
	list_heap(&pos, pos, form, NULL);
	cons_heap(&root, pos, root);
	nreverse_list_unsafe(&root, root);
	/* call-methd */
	GetConst(COMMON_CALL_METHOD, &pos);
	list_heap(ret, pos, car, root, NULL);
}

static void comb_shortform_make(addr *ret, addr comb, addr data)
{
	addr around, primary, order, check, form;

	Check(lenarrayr(data) != Clos_short_size, "size error");
	/* method */
	getarray(data, Clos_short_around, &around);
	getarray(data, Clos_short_primary, &primary);
	/* required */
	if (primary == Nil) {
		stdget_shortcomb_name(comb, &primary);
		fmte("The qualifier ~S must be at least one method.", primary, NULL);
		return;
	}
	/* order */
	stdget_shortcomb_order(comb, &order);
	GetConst(KEYWORD_MOST_SPECIFIC_LAST, &check);
	if (order == check)
		reverse_list_heap_safe(&primary, primary);
	/* form */
	comb_shortform_primary(&form, comb, primary);
	comb_shortform_around(ret, comb, around, form);
}

int comb_shortform(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr control, pos, args;

	/* make-form */
	make_symbolchar(&args, "ARGS");
	comb_shortform_make(&pos, comb, data);
	comb_longform_macrolet(&pos, args, gen, pos);

	/* eval */
	push_close_control(ptr, &control);
	if (eval_execute(ptr, pos))
		return runcode_free_control(ptr, control);
	getresult_control(ptr, ret);
	return free_control(ptr, control);
}

