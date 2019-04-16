#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
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
	StdGetLongCombination(pos, ret, binding, LAMBDA_LIST);
}
void stdset_longcomb_binding(addr pos, addr value)
{
	StdSetLongCombination(pos, value, binding, LAMBDA_LIST);
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
		*ret = 2;
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
		*ret = 1;
		return 0;
	}
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check) {
		*ret = 0;
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
static void clos_define_combination_short(constindex n, constindex c, int ident)
{
	addr name, clos;

	GetConstant(n, &name);
	GetConstant(c, &clos);
	Check(! symbolp(name), "type error");
	CheckType(clos, LISPTYPE_CLOS);

	stdset_shortdef_name(clos, name);
	stdset_shortdef_document(clos, Nil);
	stdset_shortdef_identity(clos, ident? T: Nil);
	stdset_shortdef_operator(clos, name);
	clos_define_combination(name, clos);
}
#define ClosDefineCombinationShort(x,y) \
	clos_define_combination_short(CONSTANT_COMMON_##x, CONSTANT_COMBINATION_##x, (y));

void build_clos_combination(void)
{
	ClosDefineCombinationShort(PLUS, 1);
	ClosDefineCombinationShort(AND, 1);
	ClosDefineCombinationShort(APPEND, 1);
	ClosDefineCombinationShort(LIST, 0);
	ClosDefineCombinationShort(MAX, 1);
	ClosDefineCombinationShort(MIN, 1);
	ClosDefineCombinationShort(NCONC, 1);
	ClosDefineCombinationShort(PROGN, 1);
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
	if (clos_define_long_combination_p(pos)) {
		clos_find_combination(pos, &pos);
		clos_method_combination_long(pos, tail, ret);
		return;
	}

	/* short form */
	if (clos_define_short_combination_p(pos)) {
		clos_find_combination(pos, &pos);
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

