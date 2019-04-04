#include "clos.h"
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
 *  access
 */
static void stdget_combination_constant(addr pos, addr *ret,
		enum Clos_combination_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_combination_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_checkelt(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		clos_check(pos, check, ret);
	}
}

static void stdset_combination_constant(addr pos, addr value,
		enum Clos_combination_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_combination_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_METHOD_COMBINATION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
	}
	else {
		GetConstant(index2, &check);
		clos_set(pos, check, value);
	}
}
#define StdGetCombination(p,r,a,b) \
	stdget_combination_constant((p), (r), Clos_combination_##a, CONSTANT_CLOSKEY_##b)
#define StdSetCombination(p,r,a,b) \
	stdset_combination_constant((p), (r), Clos_combination_##a, CONSTANT_CLOSKEY_##b)

void stdget_combination_name(addr pos, addr *ret)
{
	StdGetCombination(pos, ret, name, NAME);
}
void stdset_combination_name(addr pos, addr value)
{
	StdSetCombination(pos, value, name, NAME);
}

void stdget_combination_long_p(addr pos, addr *ret)
{
	StdGetCombination(pos, ret, long_p, LONG_P);
}
void stdset_combination_long_p(addr pos, addr value)
{
	StdSetCombination(pos, value, long_p, LONG_P);
}

void stdget_combination_document(addr pos, addr *ret)
{
	StdGetCombination(pos, ret, document, DOCUMENT);
}
void stdset_combination_document(addr pos, addr value)
{
	StdSetCombination(pos, value, document, DOCUMENT);
}

void stdget_combination_identity(addr pos, addr *ret)
{
	StdGetCombination(pos, ret, identity, IDENTITY);
}
void stdset_combination_identity(addr pos, addr value)
{
	StdSetCombination(pos, value, identity, IDENTITY);
}

void stdget_combination_operator(addr pos, addr *ret)
{
	StdGetCombination(pos, ret, operator, OPERATOR);
}
void stdset_combination_operator(addr pos, addr value)
{
	StdSetCombination(pos, value, operator, OPERATOR);
}

void stdget_combination_lambda_list(addr pos, addr *ret)
{
	StdGetCombination(pos, ret, lambda_list, LAMBDA_LIST);
}
void stdset_combination_lambda_list(addr pos, addr value)
{
	StdSetCombination(pos, value, lambda_list, LAMBDA_LIST);
}

void stdget_combination_qualifiers(addr pos, addr *ret)
{
	StdGetCombination(pos, ret, qualifiers, QUALIFIERS);
}
void stdset_combination_qualifiers(addr pos, addr value)
{
	StdSetCombination(pos, value, qualifiers, QUALIFIERS);
}

void stdget_combination_arguments(addr pos, addr *ret)
{
	StdGetCombination(pos, ret, arguments, ARGUMENTS);
}
void stdset_combination_arguments(addr pos, addr value)
{
	StdSetCombination(pos, value, arguments, ARGUMENTS);
}

void stdget_combination_generic(addr pos, addr *ret)
{
	StdGetCombination(pos, ret, generic, GENERIC);
}
void stdset_combination_generic(addr pos, addr value)
{
	StdSetCombination(pos, value, generic, GENERIC);
}

void stdget_combination_form(addr pos, addr *ret)
{
	StdGetCombination(pos, ret, form, FORM);
}
void stdset_combination_form(addr pos, addr value)
{
	StdSetCombination(pos, value, form, FORM);
}

void stdget_combination_function(addr pos, addr *ret)
{
	StdGetCombination(pos, ret, function, FUNCTION);
}
void stdset_combination_function(addr pos, addr value)
{
	StdSetCombination(pos, value, function, FUNCTION);
}


/*****************************************************************************
 *  check-qualifiers
 *****************************************************************************/
static int qualifiers_equal_list(addr left, addr right)
{
	addr left2, right2, asterisk;

	if (left == Nil && right == Nil) return 1;
	if (GetType(left) != LISPTYPE_CONS) return 0;
	if (GetType(right) != LISPTYPE_CONS) return 0;
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
	enum LISPTYPE type;
	addr asterisk;

	GetConst(COMMON_ASTERISK, &asterisk);
	if (right == asterisk) return 1;
	type = GetType(right);
	if (right == Nil || type == LISPTYPE_CONS)
		return qualifiers_equal_list(left, right);
	if (type == LISPTYPE_SYMBOL)
		return qualifiers_equal_symbol(ptr, left, right);
	fmte("Invalid method-combination-eualifiers ~S.", right, NULL);

	return 0;
}

static int check_qualifiers_equal_long(Execute ptr, addr combination, addr qualifiers)
{
	addr cons, qua;

	stdget_combination_qualifiers(combination, &cons);
	while (cons != Nil) {
		GetCons(cons, &qua, &cons);
		/* cadr */
		GetCdr(qua, &qua);
		GetCar(qua, &qua);
		/* check */
		if (qualifiers_equal(ptr, qualifiers, qua)) return 1;
	}

	return 0;
}

static int check_qualifiers_equal_short(addr combination, addr qualifiers)
{
	addr name, check;

	if (GetType(qualifiers) != LISPTYPE_CONS) return 0;
	GetCons(qualifiers, &qualifiers, &check);
	if (check != Nil) return 0;
	stdget_combination_name(combination, &name);
	if (qualifiers == name) return 1;
	GetConst(KEYWORD_AROUND, &check);
	if (qualifiers == check) return 1;

	return 0;
}

int check_qualifiers_equal(Execute ptr, addr combination, addr qualifiers)
{
	addr long_p;

	stdget_combination_long_p(combination, &long_p);
	if (long_p != Nil)
		return check_qualifiers_equal_long(ptr, combination, qualifiers);
	else
		return check_qualifiers_equal_short(combination, qualifiers);
}


/*****************************************************************************
 *  qualifiers-position
 *****************************************************************************/
void method_combination_qualifiers_count(addr combination, size_t *ret)
{
	addr check;
	stdget_combination_long_p(combination, &check);
	if (check != Nil) {
		stdget_combination_qualifiers(combination, &check);
		*ret = length_list_unsafe(check);
	}
	else {
		*ret = 2;
	}
}

static int qualifiers_position_short_nil(addr qualifiers, addr combination, size_t *ret)
{
	addr name, check;

	if (GetType(qualifiers) != LISPTYPE_CONS) return 1;
	GetCons(qualifiers, &qualifiers, &check);
	if (check != Nil) return 1;
	stdget_combination_name(combination, &name);
	if (qualifiers == name) {
		*ret = 1;
		return 0;
	}
	GetConst(KEYWORD_AROUND, &check);
	if (qualifiers == check) {
		*ret = 0;
		return 0;
	}

	return 1;
}

static int qualifiers_position_long_nil(Execute ptr,
		addr qualifiers, addr combination, size_t *ret)
{
	addr cons, qua;
	size_t index;

	stdget_combination_qualifiers(combination, &cons);
	for (index = 0; cons != Nil; index++) {
		GetCons(cons, &qua, &cons);
		/* cadr */
		GetCdr(qua, &qua);
		GetCar(qua, &qua);
		/* check */
		if (qualifiers_equal(ptr, qualifiers, qua)) {
			*ret = index;
			return 0;
		}
	}

	return 1;
}

int qualifiers_position_nil(Execute ptr,
		addr qualifiers, addr combination, size_t *ret)
{
	addr long_p;

	stdget_combination_long_p(combination, &long_p);
	if (long_p != Nil)
		return qualifiers_position_long_nil(ptr, qualifiers, combination, ret);
	else
		return qualifiers_position_short_nil(qualifiers, combination, ret);
}

void qualifiers_position(Execute ptr,
		addr qualifiers, addr combination, size_t *ret)
{
	if (qualifiers_position_nil(ptr, qualifiers, combination, ret))
		fmte("The qualifiers ~S is not found.", qualifiers, NULL);
}


/*****************************************************************************
 *  standard method-combination
 *****************************************************************************/
static void clos_define_combination_qualifiers(addr *ret)
{
	/*
	 *  ((around  (:around))
	 *   (before  (:before))
	 *   (primary ()       )
	 *   (after   (:after) ))
	 */
	addr around, primary, before, after;

	/* (nil (:around)) */
	GetConst(KEYWORD_AROUND, &around);
	conscar_heap(&around, around);
	list_heap(&around, Nil, around, NULL);
	/* (nil (:before)) */
	GetConst(KEYWORD_BEFORE, &before);
	conscar_heap(&before, before);
	list_heap(&before, Nil, before, NULL);
	/* (nil nil) */
	list_heap(&primary, Nil, Nil, NULL);
	/* (nil (:after)) */
	GetConst(KEYWORD_AFTER, &after);
	conscar_heap(&after, after);
	list_heap(&after, Nil, after, NULL);
	/* result */
	list_heap(ret, around, before, primary, after, NULL);
}

static void clos_define_combination_standard(void)
{
	addr name, clos, qualifiers;

	GetConst(COMMON_STANDARD, &name);
	GetConst(COMBINATION_STANDARD, &clos);
	Check(! symbolp(name), "type error");
	CheckType(clos, LISPTYPE_CLOS);
	clos_define_combination_qualifiers(&qualifiers);

	stdset_combination_name(clos, name);
	stdset_combination_long_p(clos, T);
	stdset_combination_lambda_list(clos, Nil);
	stdset_combination_qualifiers(clos, qualifiers);
	stdset_combination_arguments(clos, Nil);
	stdset_combination_generic(clos, Nil);
	stdset_combination_form(clos, Nil);
	stdset_combination_function(clos, Nil);
	clos_define_combination(name, clos);
}

static void clos_define_combination_short(constindex n, constindex c, int ident)
{
	addr name, clos;

	GetConstant(n, &name);
	GetConstant(c, &clos);
	Check(! symbolp(name), "type error");
	CheckType(clos, LISPTYPE_CLOS);

	stdset_combination_name(clos, name);
	stdset_combination_long_p(clos, Nil);
	stdset_combination_document(clos, Nil);
	stdset_combination_identity(clos, ident? T: Nil);
	stdset_combination_operator(clos, name);
	clos_define_combination(name, clos);
}
#define ClosDefineCombinationShort(x,y) \
	clos_define_combination_short(CONSTANT_COMMON_##x, CONSTANT_COMBINATION_##x, (y));

void build_clos_combination(void)
{
	clos_define_combination_standard();
	ClosDefineCombinationShort(PLUS, 1);
	ClosDefineCombinationShort(AND, 1);
	ClosDefineCombinationShort(APPEND, 1);
	ClosDefineCombinationShort(LIST, 0);
	ClosDefineCombinationShort(MAX, 1);
	ClosDefineCombinationShort(MIN, 1);
	ClosDefineCombinationShort(NCONC, 1);
	ClosDefineCombinationShort(PROGN, 1);
}

