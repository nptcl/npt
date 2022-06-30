#include "clos.h"
#include "clos_combination.h"
#include "clos_instance.h"
#include "clos_object.h"
#include "closget_combination.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_execute.h"
#include "hold.h"
#include "lambda.h"
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

	if (left == Nil && right == Nil)
		return 1;
	if (! consp(left))
		return 0;
	if (! consp(right))
		return 0;
	GetCons(left, &left, &left2);
	GetCons(right, &right, &right2);
	GetConst(COMMON_ASTERISK, &asterisk);
	if ((right != asterisk) && (left != right))
		return 0;
	if (right2 == asterisk)
		return 1;

	return qualifiers_equal_list(left2, right2);
}

static int qualifiers_equal_symbol_call_(Execute ptr, addr left, addr right, int *ret)
{
	addr call;

	conscar_local(ptr->local, &left, left);
	Return(getfunction_global_(right, &call));
	Return(apply_control_(ptr, call, left));
	getresult_control(ptr, &left);
	*ret = left != Nil;

	return 0;
}

static int qualifiers_equal_symbol_(Execute ptr, addr left, addr right, int *ret)
{
	addr control;
	push_control(ptr, &control);
	(void)qualifiers_equal_symbol_call_(ptr, left, right, ret);
	return pop_control_(ptr, control);
}

static int qualifiers_equal_(Execute ptr, addr left, addr right, int *ret)
{
	addr aster;

	*ret = 0;
	GetConst(COMMON_ASTERISK, &aster);
	if (right == aster)
		return Result(ret, 1);
	if (listp(right))
		return Result(ret, qualifiers_equal_list(left, right));
	if (symbolp(right))
		return qualifiers_equal_symbol_(ptr, left, right, ret);

	/* error */
	return fmte_("Invalid method-combination-eualifiers ~S.", right, NULL);
}

static int check_qualifiers_equal_long_(Execute ptr, addr comb, addr qua, int *ret)
{
	int check, size;
	addr cons, list;

	Return(stdget_longcomb_qualifiers_(ptr, comb, &cons));
	size = 0;
	while (cons != Nil) {
		GetCons(cons, &list, &cons);
		/* cadr */
		GetCdr(list, &list);
		GetCar(list, &list);
		/* check */
		Return(qualifiers_equal_(ptr, qua, list, &check));
		if (check)
			size++;
	}

	/* equal */
	if (size == 1)
		return Result(ret, 1);
	/* not equal */
	if (size == 0)
		return Result(ret, 0);

	/* error */
	*ret = 0;
	return fmte_("Qualifiers ~S must match a only specializer, "
			"but match multiple specializers.", qua, NULL);
}

static int check_qualifiers_equal_short_(Execute ptr, addr comb, addr qua, int *ret)
{
	addr name, check;

	if (! consp(qua))
		return Result(ret, 0);
	GetCons(qua, &qua, &check);
	if (check != Nil)
		return Result(ret, 0);
	Return(stdget_shortcomb_name_(ptr, comb, &name));
	if (qua == name)
		return Result(ret, 1);
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check)
		return Result(ret, 1);

	return Result(ret, 0);
}

static int check_qualifiers_equal_standard(addr qua)
{
	addr check;

	/* primary */
	if (qua == Nil)
		return 1;
	if (! consp(qua))
		return 0;
	GetCons(qua, &qua, &check);
	if (check != Nil)
		return 0;
	/* around, before, after */
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check)
		return 1;
	GetConst(KEYWORD_BEFORE, &check);
	if (qua == check)
		return 1;
	GetConst(KEYWORD_AFTER, &check);
	if (qua == check)
		return 1;

	return 0;
}

int check_qualifiers_equal_(Execute ptr, addr comb, addr qua, int *ret)
{
	int check;

	if (comb == Nil)
		return Result(ret, check_qualifiers_equal_standard(qua));
	Return(clos_long_combination_p_(ptr, comb, &check));
	if (check)
		return check_qualifiers_equal_long_(ptr, comb, qua, ret);
	Return(clos_short_combination_p_(ptr, comb, &check));
	if (check)
		return check_qualifiers_equal_short_(ptr, comb, qua, ret);
	/* error */
	*ret = 0;
	return fmte_("Invalid method-combination instance ~S.", comb, NULL);
}


/*****************************************************************************
 *  qualifiers-position
 *****************************************************************************/
int method_combination_qualifiers_count_(Execute ptr, addr comb, size_t *ret)
{
	int check;

	if (comb == Nil)
		return Result(ret, Clos_standard_size);
	Return(clos_long_combination_p_(ptr, comb, &check));
	if (check) {
		Return(stdget_longcomb_qualifiers_(ptr, comb, &comb));
		return Result(ret, length_list_unsafe(comb));
	}
	Return(clos_short_combination_p_(ptr, comb, &check));
	if (check)
		return Result(ret, Clos_short_size);
	/* error */
	*ret = 0;
	return fmte_("Invalid method-combination instance ~S.", comb, NULL);
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

static int qualifiers_position_long_nil_(Execute ptr, addr qua, addr comb,
		size_t *rsize, int *ret)
{
	int check;
	addr cons, list;
	size_t index;

	Return(stdget_longcomb_qualifiers_(ptr, comb, &cons));
	for (index = 0; cons != Nil; index++) {
		GetCons(cons, &list, &cons);
		/* cadr */
		GetCdr(list, &list);
		GetCar(list, &list);
		/* check */
		Return(qualifiers_equal_(ptr, qua, list, &check));
		if (check) {
			*rsize = index;
			return Result(ret, 0);
		}
	}

	return Result(ret, 1);
}

static int qualifiers_position_short_nil_(Execute ptr,
		addr qua, addr comb, size_t *value, int *ret)
{
	addr name, check;

	if (! consp(qua))
		return Result(ret, 1);
	GetCons(qua, &qua, &check);
	if (check != Nil)
		return Result(ret, 1);
	Return(stdget_shortcomb_name_(ptr, comb, &name));
	if (qua == name) {
		*value = Clos_short_primary;
		return Result(ret, 0);
	}
	GetConst(KEYWORD_AROUND, &check);
	if (qua == check) {
		*value = Clos_short_around;
		return Result(ret, 0);
	}

	return Result(ret, 1);
}

int qualifiers_position_nil_(Execute ptr, addr qua, addr comb,
		size_t *rsize, int *ret)
{
	int check;

	Check(clos_define_combination_p_debug(comb), "type error");
	if (comb == Nil)
		return Result(ret, qualifiers_position_standard_nil(qua, rsize));
	Return(clos_long_combination_p_(ptr, comb, &check));
	if (check)
		return qualifiers_position_long_nil_(ptr, qua, comb, rsize, ret);
	Return(clos_short_combination_p_(ptr, comb, &check));
	if (check)
		return qualifiers_position_short_nil_(ptr, qua, comb, rsize, ret);
	/* error */
	*rsize = 0;
	*ret = 0;
	return fmte_("Invalid method-combination type ~S.", comb, NULL);
}

int qualifiers_position_(Execute ptr, addr qua, addr comb, size_t *rsize)
{
	int check;

	Return(qualifiers_position_nil_(ptr, qua, comb, rsize, &check));
	if (check) {
		*rsize = 0;
		return fmte_("The qualifiers ~S is not found.", qua, NULL);
	}

	return 0;
}


/*****************************************************************************
 *  standard method-combination
 *****************************************************************************/
static int build_clos_method_combination_standard_(Execute ptr)
{
	addr clos, inst, name;

	GetConst(CLOS_LONG_METHOD_COMBINATION, &clos);
	Return(clos_instance_heap_(ptr, clos, &inst));
	GetConst(COMMON_STANDARD, &name);
	Return(stdset_longcomb_name_(ptr, inst, name));
	SetConst(CLOS_COMBINATION_STANDARD, inst);

	return 0;
}

static int build_clos_method_combination_short_(Execute ptr, constindex n, int ident)
{
	addr clos, inst, name;

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &clos);
	Return(clos_instance_heap_(ptr, clos, &inst));
	GetConstant(n, &name);
	Return(stdset_shortdef_name_(ptr, inst, name));
	Return(stdset_shortdef_documentation_(ptr, inst, Nil));
	Return(stdset_shortdef_identity_(ptr, inst, ident? T: Nil));
	Return(stdset_shortdef_operator_(ptr, inst, name));
	clos_define_combination(name, inst);

	return 0;
}

static int build_clos_combination_call_(Execute ptr)
{
	Return(build_clos_method_combination_standard_(ptr));
	Return(build_clos_method_combination_short_(ptr, CONSTANT_COMMON_PLUS, 1));
	Return(build_clos_method_combination_short_(ptr, CONSTANT_COMMON_AND, 1));
	Return(build_clos_method_combination_short_(ptr, CONSTANT_COMMON_APPEND, 1));
	Return(build_clos_method_combination_short_(ptr, CONSTANT_COMMON_LIST, 0));
	Return(build_clos_method_combination_short_(ptr, CONSTANT_COMMON_MAX, 1));
	Return(build_clos_method_combination_short_(ptr, CONSTANT_COMMON_MIN, 1));
	Return(build_clos_method_combination_short_(ptr, CONSTANT_COMMON_NCONC, 1));
	Return(build_clos_method_combination_short_(ptr, CONSTANT_COMMON_OR, 1));
	Return(build_clos_method_combination_short_(ptr, CONSTANT_COMMON_PROGN, 1));

	return 0;
}

void build_clos_combination(Execute ptr)
{
	Error(build_clos_combination_call_(ptr));
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

static int clos_method_combination_standard_(addr comb, addr list, addr *ret)
{
	if (list != Nil) {
		*ret = Nil;
		return fmte_("Invalid STANDARD method-combination arguments ~S.", list, NULL);
	}

	return Result(ret, Nil);
}

static int clos_method_combination_long_(Execute ptr, addr comb, addr list, addr *ret)
{
	addr pos, value;

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	Return(clos_instance_heap_(ptr, pos, &pos));
	/* copy */
	Return(stdget_longdef_name_(ptr, comb, &value));
	Return(stdset_longcomb_name_(ptr, pos, value));
	Return(stdget_longdef_documentation_(ptr, comb, &value));
	Return(stdset_longcomb_documentation_(ptr, pos, value));
	Return(stdget_longdef_lambda_list_(ptr, comb, &value));
	Return(stdset_longcomb_lambda_list_(ptr, pos, value));
	Return(stdget_longdef_qualifiers_(ptr, comb, &value));
	Return(stdset_longcomb_qualifiers_(ptr, pos, value));
	Return(stdget_longdef_arguments_(ptr, comb, &value));
	Return(stdset_longcomb_arguments_(ptr, pos, value));
	Return(stdget_longdef_generic_(ptr, comb, &value));
	Return(stdset_longcomb_generic_(ptr, pos, value));
	Return(stdget_longdef_form_(ptr, comb, &value));
	Return(stdset_longcomb_form_(ptr, pos, value));
	/* binding */
	Return(stdset_longcomb_binding_(ptr, pos, list));
	/* result */
	return Result(ret, pos);
}

static int clos_method_combination_short_arguments_(addr list, addr *ret)
{
	addr check, first, last;

	GetConst(KEYWORD_MOST_SPECIFIC_FIRST, &first);
	GetConst(KEYWORD_MOST_SPECIFIC_LAST, &last);
	if (list == Nil)
		return Result(ret, first);
	if (singlep(list)) {
		GetCar(list, &check);
		if (check != first && check != last)
			goto error;
		return Result(ret, check);
	}
	/* error */
error:
	*ret = Nil;
	return fmte_("METHOD-COMBINATION ~S argument must be a "
			":most-specific-first or :most-specific-last.", list, NULL);
}

static int clos_method_combination_short_(Execute ptr, addr comb, addr list, addr *ret)
{
	addr pos, value, order;

	/* (&optional argument-precedence-order) */
	Return(clos_method_combination_short_arguments_(list, &order));
	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	Return(clos_instance_heap_(ptr, pos, &pos));
	/* copy */
	Return(stdget_shortdef_name_(ptr, comb, &value));
	Return(stdset_shortcomb_name_(ptr, pos, value));
	Return(stdget_shortdef_documentation_(ptr, comb, &value));
	Return(stdset_shortcomb_documentation_(ptr, pos, value));
	Return(stdget_shortdef_identity_(ptr, comb, &value));
	Return(stdset_shortcomb_identity_(ptr, pos, value));
	Return(stdget_shortdef_operator_(ptr, comb, &value));
	Return(stdset_shortcomb_operator_(ptr, pos, value));
	/* argument-precedence-order */
	Return(stdset_shortcomb_order_(ptr, pos, order));
	/* result */
	return Result(ret, pos);
}

static int find_method_combination_(Execute ptr, addr symbol, addr list, addr *ret)
{
	int check;
	addr pos;

	Check(! symbolp(symbol), "type error");
	Check(! listp(list), "type error");

	/* standard */
	if (clos_method_combination_standard_p(symbol))
		return clos_method_combination_standard_(symbol, list, ret);

	/* long form */
	Return(clos_find_combination_(symbol, &pos));
	Return(clos_define_long_combination_p_(ptr, pos, &check));
	if (check)
		return clos_method_combination_long_(ptr, pos, list, ret);

	/* short form */
	Return(clos_define_short_combination_p_(ptr, pos, &check));
	if (check)
		return clos_method_combination_short_(ptr, pos, list, ret);

	/* error */
	*ret = Nil;
	return fmte_("Invalid method-combination instance ~S, ~S.", pos, list, NULL);
}

int mop_find_method_combination_(Execute ptr, addr symbol, addr list, addr *ret)
{
	addr pos;

	Return(find_method_combination_(ptr, symbol, list, &pos));
	if (pos == Nil) {
		GetConst(CLOS_COMBINATION_STANDARD, &pos);
	}

	return Result(ret, pos);
}

int clos_find_method_combination_(Execute ptr, addr list, addr *ret)
{
	addr pos, tail;

	if (! consp_getcons(list, &pos, &tail)) {
		*ret = Nil;
		return fmte_("Invalid method-combination instance ~S.", list, NULL);
	}

	return find_method_combination_(ptr, pos, tail, ret);
}


/*
 *  ensure-define-combination
 */
int ensure_define_combination_short_common_(Execute ptr,
		addr name, addr doc, addr ident, addr oper)
{
	addr pos;

	/* instance */
	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	Return(clos_instance_heap_(ptr, pos, &pos));
	Return(stdset_shortdef_name_(ptr, pos, name));
	Return(stdset_shortdef_documentation_(ptr, pos, doc));
	Return(stdset_shortdef_identity_(ptr, pos, ident));
	Return(stdset_shortdef_operator_(ptr, pos, oper));
	/* define-combination */
	clos_define_combination(name, pos);

	return 0;
}

int ensure_define_combination_long_common_(Execute ptr,
		addr name, addr lambda, addr spec,
		addr args, addr gen, addr doc, addr form, addr decl)
{
	addr pos;

	/* instance */
	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	Return(clos_instance_heap_(ptr, pos, &pos));
	Return(stdset_longdef_name_(ptr, pos, name));
	Return(stdset_longdef_lambda_list_(ptr, pos, lambda));
	Return(stdset_longdef_qualifiers_(ptr, pos, spec));
	Return(stdset_longdef_arguments_(ptr, pos, args));
	Return(stdset_longdef_generic_(ptr, pos, gen));
	Return(stdset_longdef_documentation_(ptr, pos, doc));
	Return(stdset_longdef_form_(ptr, pos, form));
	/* define-combination */
	clos_define_combination(name, pos);

	return 0;
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
	addr pos;
	addr lambda, declare, ignorable, dbind, call;

	/* form */
	GetConst(COMMON_DECLARE, &declare);
	cons_heap(&pos, declare, decl);
	list_heap(&form, pos, form, NULL);

	/* destructuring-bind */
	if (args != Nil) {
		GetConst(COMMON_DESTRUCTURING_BIND, &dbind);
		GetConst(CLOSNAME_COMBINATION_BINDING, &call);
		list_heap(&call, call, inst, NULL);
		lista_heap(&form, dbind, args, call, form, NULL);
	}

	/* lambda */
	GetConst(COMMON_LAMBDA, &lambda);
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
	nreverse(ret, root);
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
	nreverse(&root, root);
	/* declare */
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, vars);
	list_heap(&declare, declare, ignorable, NULL);
	/* let */
	GetConst(COMMON_LET, &let);
	list_heap(ret, let, root, declare, form, NULL);
}

static int comb_longmacro_form_(addr *ret,
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
		Return_getcons(spec, &pos, &spec);
		Return(list_bind_(pos, &name, &temp, &order, &req, &temp, NULL));
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
	nreverse(&vars, vars);
	cons_heap(&vars, ignorable, vars);
	GetConst(COMMON_DECLARE, &declare);
	list_heap(&declare, declare, vars, NULL);
	/* let* */
	GetConst(COMMON_LETA, &leta);
	nreverse(&args, args);
	lista_heap(ret, leta, args, declare, form, NULL);

	return 0;
}

int comb_longmacro_(addr *ret,
		addr lambda, addr spec, addr args, addr gens, addr decl, addr form)
{
	addr gen, inst, array;

	make_symbolchar(&gen, "GENERIC");
	make_symbolchar(&inst, "COMBINATION");
	make_symbolchar(&array, "QUALIFIERS");

	Return(comb_longmacro_form_(&form, spec, gens, gen, array, form));
	comb_longmacro_arguments(&form, args, form);
	comb_longmacro_lambda(ret, lambda, gen, inst, array, decl, form);

	return 0;
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

static int comb_longmacro_lambda_list_(addr args, addr *ret)
{
	addr root, var, list, a, b, c, d;
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
		Return(list_bind_(var, &a, &b, &c, NULL));
		if (c == Nil)
			list_heap(&var, a, b, NULL);
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
		Return(list_bind_(var, &a, &b, &c, &d, NULL));
		list_heap(&a, b, a, NULL);
		if (d == Nil)
			list_heap(&var, a, c, NULL);
		else
			list_heap(&var, a, c, d, NULL);
		cons_heap(&root, var, root);
	}

	/* key */
	if (str->allow) {
		GetConst(AMPERSAND_ALLOW, &var);
		cons_heap(&root, var, root);
	}

	/* result */
	nreverse(ret, root);

	return 0;
}

static int comb_longform_arguments_(Execute ptr,
		addr *ret, addr args, addr comb, addr form)
{
	addr list, lambda;
	addr dbind, declare, ignorable;

	/* no :arguments */
	Return(stdget_longcomb_arguments_(ptr, comb, &comb));
	if (comb == Nil)
		return Result(ret, form);

	/* (destructuring-bind ,arguments ,args
	 *   (declare (ignorable ...))
	 *   ,form)
	 */
	comb_longmacro_variables(comb, &list);
	Return(comb_longmacro_lambda_list_(comb, &lambda));
	/* declare */
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	cons_heap(&ignorable, ignorable, list);
	list_heap(&declare, declare, ignorable, NULL);
	/* result */
	GetConst(COMMON_DESTRUCTURING_BIND, &dbind);
	list_heap(ret, dbind, lambda, args, declare, form, NULL);

	return 0;
}

static int comb_longform_call_(Execute ptr, LocalHold hold,
		addr gen, addr comb, addr data, addr *ret)
{
	addr pos;

	Return(stdget_longcomb_form_(ptr, comb, &pos));
	Return(funcall_control_(ptr, pos, gen, comb, data, NULL));
	getresult_control(ptr, &pos);
	localhold_set(hold, 0, pos);

	return Result(ret, pos);
}

static int comb_longform_push_(Execute ptr, addr gen, addr comb, addr data, addr *ret)
{
	addr control, pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	pos = Nil;
	(void)comb_longform_call_(ptr, hold, gen, comb, data, &pos);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return Result(ret, pos);
}

int comb_longform_(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr pos, args;

	/* execute */
	Return(comb_longform_push_(ptr, gen, comb, data, &pos));

	/* make-form */
	make_symbolchar(&args, "ARGS");
	Return(comb_longform_arguments_(ptr, &pos, args, comb, pos));
	comb_longform_macrolet(&pos, args, gen, pos);

	/* eval */
	return eval_result_partial_(ptr, pos, ret);
}

static int comb_shortform_primary_(Execute ptr, addr *ret, addr comb, addr list)
{
	addr check, call, root;

	GetConst(COMMON_CALL_METHOD, &call);
	Return(stdget_shortcomb_identity_(ptr, comb, &check));
	if (check != Nil && singlep(list)) {
		GetCar(list, &list);
		list_heap(ret, call, list, NULL);
	}
	else {
		Return(stdget_shortcomb_operator_(ptr, comb, &check));
		conscar_heap(&root, check);
		while (list != Nil) {
			Return_getcons(list, &check, &list);
			list_heap(&check, call, check, NULL);
			cons_heap(&root, check, root);
		}
		nreverse(ret, root);
	}

	return 0;
}

static int comb_shortform_around_(addr *ret, addr comb, addr list, addr form)
{
	addr pos, root, car, cdr;

	if (list == Nil)
		return Result(ret, form);

	Return_getcons(list, &car, &cdr);
	/* ,@(cdr around) */
	for (root = Nil; cdr != Nil; ) {
		Return_getcons(cdr, &pos, &cdr);
		cons_heap(&root, pos, root);
	}
	/* (make-method ,form) */
	GetConst(COMMON_MAKE_METHOD, &pos);
	list_heap(&pos, pos, form, NULL);
	cons_heap(&root, pos, root);
	nreverse(&root, root);
	/* call-methd */
	GetConst(COMMON_CALL_METHOD, &pos);
	list_heap(ret, pos, car, root, NULL);

	return 0;
}

static int comb_shortform_make_(Execute ptr, addr *ret, addr comb, addr data)
{
	addr around, primary, order, check, form;

	Check(lenarrayr(data) != Clos_short_size, "size error");
	/* method */
	getarray(data, Clos_short_around, &around);
	getarray(data, Clos_short_primary, &primary);
	/* required */
	if (primary == Nil) {
		Return(stdget_shortcomb_name_(ptr, comb, &primary));
		*ret = Nil;
		return fmte_("The qualifier ~S must be at least one method.", primary, NULL);
	}
	/* order */
	Return(stdget_shortcomb_order_(ptr, comb, &order));
	GetConst(KEYWORD_MOST_SPECIFIC_LAST, &check);
	if (order == check) {
		Return(reverse_list_heap_safe_(&primary, primary));
	}
	/* form */
	Return(comb_shortform_primary_(ptr, &form, comb, primary));
	return comb_shortform_around_(ret, comb, around, form);
}

int comb_shortform_(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr pos, args;

	/* make-form */
	make_symbolchar(&args, "ARGS");
	Return(comb_shortform_make_(ptr, &pos, comb, data));
	comb_longform_macrolet(&pos, args, gen, pos);

	/* eval */
	return eval_result_partial_(ptr, pos, ret);
}

