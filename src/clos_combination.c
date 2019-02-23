#include "clos.h"
#include "clos_object.h"
#include "clos_standard.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "object.h"
#include "package.h"
#include "sequence.h"
#include "symbol.h"
#include <stdarg.h>


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
	addr control;

	push_close_control(ptr, &control);
	conscar_local(ptr->local, &left, left);
	getfunctioncheck_local(ptr, right, &right);
	apply_control(ptr, right, left);
	getresult_control(ptr, &left);
	result = left != Nil;
	free_control(ptr, control);

	return result;
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

static int check_qualifiers_equal_long(Execute ptr,
		addr combination, addr qualifiers)
{
	addr cons, qua;

	clos_elt(combination, Clos_combination_qualifiers, &cons);
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
	clos_elt(combination, Clos_combination_name, &name);
	if (qualifiers == name) return 1;
	GetConst(KEYWORD_AROUND, &check);
	if (qualifiers == check) return 1;

	return 0;
}

int check_qualifiers_equal(Execute ptr, addr combination, addr qualifiers)
{
	addr long_p;

	clos_elt(combination, Clos_combination_long_p, &long_p);
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
	clos_elt(combination, Clos_combination_long_p, &check);
	if (check != Nil) {
		clos_elt(combination, Clos_combination_qualifiers, &check);
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
	clos_elt(combination, Clos_combination_name, &name);
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

	clos_elt(combination, Clos_combination_qualifiers, &cons);
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

	clos_elt(combination, Clos_combination_long_p, &long_p);
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
static void make_instance_method_combination(addr *ret, addr clos)
{
	addr pos;

	if (clos == Nil)
		GetConst(CLOS_METHOD_COMBINATION, &clos);
	Check(clos == Nil, "method-combination error");
	make_instance_restrict_heap(clos, &pos);
	std_update_class_of(pos, clos);
	*ret = pos;
}

static void define_method_combination_constant(enum CONSTANT_INDEX index, int ident)
{
	addr clos, name;

	make_instance_method_combination(&clos, Nil);
	GetConstant(index, &name);
	Check(! IsSymbol(name), "type name error");
	setf_clos_elt(clos, Clos_combination_name, name);
	setf_clos_elt(clos, Clos_combination_long_p, Nil);
	setf_clos_elt(clos, Clos_combination_document, Nil);
	setf_clos_elt(clos, Clos_combination_identity, ident? T: Nil);
	setf_clos_elt(clos, Clos_combination_operator, name);
	setf_find_method_combination(name, clos);
}

static void combination_standard_qualifiers(addr *ret)
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

static void define_method_combination_standard(void)
{
	addr clos, name, qualifiers;

	make_instance_method_combination(&clos, Nil);
	GetConst(COMMON_STANDARD, &name);
	Check(! IsSymbol(name), "type name error");
	combination_standard_qualifiers(&qualifiers);

	setf_clos_elt(clos, Clos_combination_name, name);
	setf_clos_elt(clos, Clos_combination_long_p, T);
	setf_clos_elt(clos, Clos_combination_lambda_list, Nil);
	setf_clos_elt(clos, Clos_combination_qualifiers, qualifiers);
	setf_clos_elt(clos, Clos_combination_arguments, Nil);
	setf_clos_elt(clos, Clos_combination_generic, Nil);
	setf_clos_elt(clos, Clos_combination_form, Nil);
	setf_clos_elt(clos, Clos_combination_function, Nil);
	setf_find_method_combination(name, clos);
}

void build_clos_combination(Execute ptr)
{
	define_method_combination_standard();
	define_method_combination_constant(CONSTANT_COMMON_PLUS, 1);
	define_method_combination_constant(CONSTANT_COMMON_AND, 1);
	define_method_combination_constant(CONSTANT_COMMON_APPEND, 1);
	define_method_combination_constant(CONSTANT_COMMON_LIST, 0);
	define_method_combination_constant(CONSTANT_COMMON_MAX, 1);
	define_method_combination_constant(CONSTANT_COMMON_MIN, 1);
	define_method_combination_constant(CONSTANT_COMMON_NCONC, 1);
	define_method_combination_constant(CONSTANT_COMMON_PROGN, 1);
}

