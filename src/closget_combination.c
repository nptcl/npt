#include "condition.h"
#include "clos_object.h"
#include "clos_variable.h"
#include "closget.h"
#include "closget_combination.h"
#include "typedef.h"

/*
 *  slot-value
 */
/* long-method-combination */
static int stdget_longcomb_constant_(addr pos, addr *ret,
		enum Clos_longcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_long_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}
#define StdGetLongCombination_(p,r,a,b) \
	stdget_longcomb_constant_((p), (r), Clos_longcomb_##a, CONSTANT_CLOSNAME_##b)

int stdget_longcomb_name_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongCombination_(pos, ret, name, NAME);
}

int stdget_longcomb_documentation_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongCombination_(pos, ret, documentation, DOCUMENTATION);
}

int stdget_longcomb_lambda_list_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongCombination_(pos, ret, lambda_list, LAMBDA_LIST);
}

int stdget_longcomb_binding_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongCombination_(pos, ret, binding, BINDING);
}

int stdget_longcomb_qualifiers_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongCombination_(pos, ret, qualifiers, QUALIFIERS);
}

int stdget_longcomb_arguments_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongCombination_(pos, ret, arguments, ARGUMENTS);
}

int stdget_longcomb_generic_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongCombination_(pos, ret, generic, GENERIC);
}

int stdget_longcomb_form_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongCombination_(pos, ret, form, FORM);
}


/* short-method-combination */
static int stdget_shortcomb_constant_(addr pos, addr *ret,
		enum Clos_shortcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_short_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}
#define StdGetShortCombination_(p,r,a,b) \
	stdget_shortcomb_constant_((p), (r), Clos_shortcomb_##a, CONSTANT_CLOSNAME_##b)

int stdget_shortcomb_name_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetShortCombination_(pos, ret, name, NAME);
}

int stdget_shortcomb_documentation_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetShortCombination_(pos, ret, documentation, DOCUMENTATION);
}

int stdget_shortcomb_identity_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetShortCombination_(pos, ret, identity, IDENTITY);
}

int stdget_shortcomb_operator_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetShortCombination_(pos, ret, operator, OPERATOR);
}

int stdget_shortcomb_order_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetShortCombination_(pos, ret, order, ORDER);
}


/* define-long-method-combination */
static int stdget_longdef_constant_(addr pos, addr *ret,
		enum Clos_longdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_define_long_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}
#define StdGetLongDefineCombination_(p,r,a,b) \
	stdget_longdef_constant_((p), (r), Clos_longdef_##a, CONSTANT_CLOSNAME_##b)

int stdget_longdef_name_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongDefineCombination_(pos, ret, name, NAME);
}

int stdget_longdef_documentation_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongDefineCombination_(pos, ret, documentation, DOCUMENTATION);
}

int stdget_longdef_lambda_list_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongDefineCombination_(pos, ret, lambda_list, LAMBDA_LIST);
}

int stdget_longdef_qualifiers_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongDefineCombination_(pos, ret, qualifiers, QUALIFIERS);
}

int stdget_longdef_arguments_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongDefineCombination_(pos, ret, arguments, ARGUMENTS);
}

int stdget_longdef_generic_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongDefineCombination_(pos, ret, generic, GENERIC);
}

int stdget_longdef_form_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetLongDefineCombination_(pos, ret, form, FORM);
}


/* define-short-method-combination */
static int stdget_shortdef_constant_(addr pos, addr *ret,
		enum Clos_shortdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_define_short_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}
#define StdGetShortDefineCombination_(p,r,a,b) \
	stdget_shortdef_constant_((p), (r), Clos_shortdef_##a, CONSTANT_CLOSNAME_##b)

int stdget_shortdef_name_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetShortDefineCombination_(pos, ret, name, NAME);
}

int stdget_shortdef_documentation_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetShortDefineCombination_(pos, ret, documentation, DOCUMENTATION);
}

int stdget_shortdef_identity_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetShortDefineCombination_(pos, ret, identity, IDENTITY);
}

int stdget_shortdef_operator_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetShortDefineCombination_(pos, ret, operator, OPERATOR);
}


/*
 *  (setf slot-value)
 */
/* long-method-combination */
static int stdset_longcomb_constant_(addr pos, addr value,
		enum Clos_longcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_long_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdSetLongCombination_(p,r,a,b) \
	stdset_longcomb_constant_((p), (r), Clos_longcomb_##a, CONSTANT_CLOSNAME_##b)

int stdset_longcomb_name_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongCombination_(pos, value, name, NAME);
}

int stdset_longcomb_documentation_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongCombination_(pos, value, documentation, DOCUMENTATION);
}

int stdset_longcomb_lambda_list_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongCombination_(pos, value, lambda_list, LAMBDA_LIST);
}

int stdset_longcomb_binding_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongCombination_(pos, value, binding, BINDING);
}

int stdset_longcomb_qualifiers_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongCombination_(pos, value, qualifiers, QUALIFIERS);
}

int stdset_longcomb_arguments_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongCombination_(pos, value, arguments, ARGUMENTS);
}

int stdset_longcomb_generic_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongCombination_(pos, value, generic, GENERIC);
}

int stdset_longcomb_form_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongCombination_(pos, value, form, FORM);
}


/* short-method-combination */
static int stdset_shortcomb_constant_(addr pos, addr value,
		enum Clos_shortcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_short_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdSetShortCombination_(p,r,a,b) \
	stdset_shortcomb_constant_((p), (r), Clos_shortcomb_##a, CONSTANT_CLOSNAME_##b)

int stdset_shortcomb_name_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetShortCombination_(pos, value, name, NAME);
}

int stdset_shortcomb_documentation_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetShortCombination_(pos, value, documentation, DOCUMENTATION);
}

int stdset_shortcomb_identity_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetShortCombination_(pos, value, identity, IDENTITY);
}

int stdset_shortcomb_operator_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetShortCombination_(pos, value, operator, OPERATOR);
}

int stdset_shortcomb_order_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetShortCombination_(pos, value, order, ORDER);
}


/* define-long-method-combination */
static int stdset_longdef_constant_(addr pos, addr value,
		enum Clos_longdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_define_long_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdSetLongDefineCombination_(p,r,a,b) \
	stdset_longdef_constant_((p), (r), Clos_longdef_##a, CONSTANT_CLOSNAME_##b)

int stdset_longdef_name_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongDefineCombination_(pos, value, name, NAME);
}

int stdset_longdef_documentation_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongDefineCombination_(pos, value, documentation, DOCUMENTATION);
}

int stdset_longdef_lambda_list_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongDefineCombination_(pos, value, lambda_list, LAMBDA_LIST);
}

int stdset_longdef_qualifiers_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongDefineCombination_(pos, value, qualifiers, QUALIFIERS);
}

int stdset_longdef_arguments_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongDefineCombination_(pos, value, arguments, ARGUMENTS);
}

int stdset_longdef_generic_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongDefineCombination_(pos, value, generic, GENERIC);
}

int stdset_longdef_form_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetLongDefineCombination_(pos, value, form, FORM);
}


/* define-short-method-combination */
static int stdset_shortdef_constant_(addr pos, addr value,
		enum Clos_shortdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_define_short_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdSetShortDefineCombination_(p,r,a,b) \
	stdset_shortdef_constant_((p), (r), Clos_shortdef_##a, CONSTANT_CLOSNAME_##b)

int stdset_shortdef_name_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetShortDefineCombination_(pos, value, name, NAME);
}

int stdset_shortdef_documentation_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetShortDefineCombination_(pos, value, documentation, DOCUMENTATION);
}

int stdset_shortdef_identity_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetShortDefineCombination_(pos, value, identity, IDENTITY);
}

int stdset_shortdef_operator_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetShortDefineCombination_(pos, value, operator, OPERATOR);
}


/*
 *  slot-boundp
 */
/* long-method-combination */
static int stdboundp_longcomb_constant_(addr pos, int *ret,
		enum Clos_longcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_long_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_getelt(pos, (size_t)index1, &pos);
	}
	else {
		GetConstant(index2, &check);
		Return(clos_get_(pos, check, &pos));
	}

	return Result(ret, pos != Unbound);
}
#define StdBoundpLongCombination_(p,r,a,b) \
	stdboundp_longcomb_constant_((p), (r), Clos_longcomb_##a, CONSTANT_CLOSNAME_##b)

int stdboundp_longcomb_name_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongCombination_(pos, ret, name, NAME);
}

int stdboundp_longcomb_documentation_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongCombination_(pos, ret, documentation, DOCUMENTATION);
}

int stdboundp_longcomb_lambda_list_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongCombination_(pos, ret, lambda_list, LAMBDA_LIST);
}

int stdboundp_longcomb_binding_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongCombination_(pos, ret, binding, BINDING);
}

int stdboundp_longcomb_qualifiers_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongCombination_(pos, ret, qualifiers, QUALIFIERS);
}

int stdboundp_longcomb_arguments_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongCombination_(pos, ret, arguments, ARGUMENTS);
}

int stdboundp_longcomb_generic_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongCombination_(pos, ret, generic, GENERIC);
}

int stdboundp_longcomb_form_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongCombination_(pos, ret, form, FORM);
}


/* short-method-combination */
static int stdboundp_shortcomb_constant_(addr pos, int *ret,
		enum Clos_shortcomb_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortcomb_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_short_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_getelt(pos, (size_t)index1, &pos);
	}
	else {
		GetConstant(index2, &check);
		Return(clos_get_(pos, check, &pos));
	}

	return Result(ret, pos != Unbound);
}
#define StdBoundpShortCombination_(p,r,a,b) \
	stdboundp_shortcomb_constant_((p), (r), Clos_shortcomb_##a, CONSTANT_CLOSNAME_##b)

int stdboundp_shortcomb_name_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpShortCombination_(pos, ret, name, NAME);
}

int stdboundp_shortcomb_documentation_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpShortCombination_(pos, ret, documentation, DOCUMENTATION);
}

int stdboundp_shortcomb_identity_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpShortCombination_(pos, ret, identity, IDENTITY);
}

int stdboundp_shortcomb_operator_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpShortCombination_(pos, ret, operator, OPERATOR);
}

int stdboundp_shortcomb_order_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpShortCombination_(pos, ret, order, ORDER);
}


/* define-long-method-combination */
static int stdboundp_longdef_constant_(addr pos, int *ret,
		enum Clos_longdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_longdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_define_long_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_getelt(pos, (size_t)index1, &pos);
	}
	else {
		GetConstant(index2, &check);
		Return(clos_get_(pos, check, &pos));
	}

	return Result(ret, pos != Unbound);
}
#define StdBoundpLongDefineCombination_(p,r,a,b) \
	stdboundp_longdef_constant_((p), (r), Clos_longdef_##a, CONSTANT_CLOSNAME_##b)

int stdboundp_longdef_name_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongDefineCombination_(pos, ret, name, NAME);
}

int stdboundp_longdef_documentation_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongDefineCombination_(pos, ret, documentation, DOCUMENTATION);
}

int stdboundp_longdef_lambda_list_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongDefineCombination_(pos, ret, lambda_list, LAMBDA_LIST);
}

int stdboundp_longdef_qualifiers_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongDefineCombination_(pos, ret, qualifiers, QUALIFIERS);
}

int stdboundp_longdef_arguments_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongDefineCombination_(pos, ret, arguments, ARGUMENTS);
}

int stdboundp_longdef_generic_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongDefineCombination_(pos, ret, generic, GENERIC);
}

int stdboundp_longdef_form_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpLongDefineCombination_(pos, ret, form, FORM);
}


/* define-short-method-combination */
static int stdboundp_shortdef_constant_(addr pos, int *ret,
		enum Clos_shortdef_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_shortdef_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_define_short_method_combination) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_getelt(pos, (size_t)index1, &pos);
	}
	else {
		GetConstant(index2, &check);
		Return(clos_get_(pos, check, &pos));
	}

	return Result(ret, pos != Unbound);
}
#define StdBoundpShortDefineCombination_(p,r,a,b) \
	stdboundp_shortdef_constant_((p), (r), Clos_shortdef_##a, CONSTANT_CLOSNAME_##b)

int stdboundp_shortdef_name_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpShortDefineCombination_(pos, ret, name, NAME);
}

int stdboundp_shortdef_documentation_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpShortDefineCombination_(pos, ret, documentation, DOCUMENTATION);
}

int stdboundp_shortdef_identity_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpShortDefineCombination_(pos, ret, identity, IDENTITY);
}

int stdboundp_shortdef_operator_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpShortDefineCombination_(pos, ret, operator, OPERATOR);
}

