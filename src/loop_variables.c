#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "hold.h"
#include "loop.h"
#include "loop_bind.h"
#include "loop_parse.h"
#include "loop_special.h"
#include "loop_symbol.h"
#include "loop_variables.h"
#include "object.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  with
 */
static int loop_with_list_bind_(Execute ptr,
		addr list, addr *var, addr *type, addr *value)
{
	Return(list_bind_(list, var, type, value, NULL));
	if (*value == Unbound)
		return loop_bind_initial_list_(ptr, *var, *type, value);

	return 0;
}

static void loop_with_single_bind(addr *ret, addr var, addr type, addr value)
{
	addr qvar, dbind, bind, let, declare, dtype;

	Check(value == Unbound, "unbound error");
	if (! consp(var)) {
		/* `(let ((,var ,value))
		 *    (declare (type ,var ,type))
		 *    ,form)
		 */
		GetConst(COMMON_LET, &let);
		if (value == Nil) {
			list_heap(&value, var, NULL);
		}
		else {
			list_heap(&value, var, value, NULL);
			list_heap(&value, value, NULL);
		}
		if (type == Unbound) {
			list_heap(ret, let, value, NULL);
		}
		else {
			GetConst(COMMON_DECLARE, &declare);
			GetConst(COMMON_TYPE, &dtype);
			list_heap(&dtype, dtype, type, var, NULL);
			list_heap(&declare, declare, dtype, NULL);
			list_heap(ret, let, value, declare, NULL);
		}
	}
	else {
		/* `(destructuring-bind ,var (lisp-system::loop-bind ,var ,type ,value)
		 *    ,form)
		 */
		if (type == Unbound)
			type = T;
		GetConst(COMMON_DESTRUCTURING_BIND, &dbind);
		GetConst(SYSTEM_LOOP_BIND, &bind);
		quotelist_heap(&qvar, var);
		quotelist_heap(&type, type);
		list_heap(&bind, bind, qvar, type, value, NULL);
		list_heap(ret, dbind, var, bind, NULL);
	}
}

static int loop_with_single_(Execute ptr, addr list)
{
	addr var, type, value;

	Return(loop_with_list_bind_(ptr, list, &var, &type, &value));
	loop_with_single_bind(&var, var, type, value);

	return push_let_loop_(ptr, var);
}

static int loop_with_all_variable(addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCar(pos, &pos);
		if (consp(pos))
			return 0;
	}

	return 1;
}

static int loop_with_let_(Execute ptr, addr list)
{
	/* `(let ((,var1 ,value1)
	 *        (,var2 ,value2))
	 *    (declare (type ,var1 ,type1))
	 *    (declare (type ,var2 ,type2))
	 *    ,form)
	 */
	addr args, var, type, value, pos, right, root;
	addr let, declare, dtype;

	/* args */
	args = Nil;
	for (right = list; right != Nil; ) {
		GetCons(right, &pos, &right);
		Return(loop_with_list_bind_(ptr, pos, &var, &type, &value));
		Check(value == Unbound, "unbound error");
		list_heap(&pos, var, value, NULL);
		cons_heap(&args, pos, args);
	}
	/* let */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_TYPE, &dtype);
	conscar_heap(&root, let);
	cons_heap(&root, args, root);
	for (right = list; right != Nil; ) {
		GetCons(right, &pos, &right);
		Return(loop_with_list_bind_(ptr, pos, &var, &type, &value));
		if (type != Unbound) {
			list_heap(&var, dtype, var, type, NULL);
			list_heap(&var, declare, var, NULL);
			cons_heap(&root, var, root);
		}
	}
	nreverse(&root, root);

	return push_let_loop_(ptr, root);
}

static int loop_with_gensym_form_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(loop_with_single_(ptr, pos));
	}

	return 0;
}

static int loop_with_gensym_(Execute ptr, addr list)
{
	/* `(let ((#:g1 ,value1)
	 *        (#:g2 ,value2))
	 *    (let ((,var1 #:g1))
	 *      (declare ...)
	 *      ...
	 *  [or]
	 *    (destructuring-bind ,var1 (lisp-system::loop-bind ,var1 ,type1 ,#g1)
	 *      ...
	 */
	addr args, pos, var, type, value, right, letlist;
	addr g, glist, let;

	/* args */
	args = glist = letlist = Nil;
	for (right = list; right != Nil; ) {
		GetCons(right, &pos, &right);
		Return(loop_with_list_bind_(ptr, pos, &var, &type, &value));
		Check(value == Unbound, "unbound error");
		Return(make_gensym_(ptr, &g));
		/* let args */
		list_heap(&pos, g, value, NULL);
		cons_heap(&letlist, pos, letlist);
		/* gensym list */
		list_heap(&value, g, value, NULL);
		cons_heap(&args, value, args);
		list_heap(&pos, var, type, g, NULL);
		cons_heap(&glist, pos, glist);
	}
	nreverse(&letlist, letlist);
	nreverse(&args, args);
	nreverse(&glist, glist);
	/* expand */
	GetConst(COMMON_LET, &let);
	list_heap(&let, let, letlist, NULL);
	Return(push_let_loop_(ptr, let));
	Return(loop_with_gensym_form_(ptr, glist));

	return 0;
}

static int loop_with_multiple_(Execute ptr, addr list)
{
	if (loop_with_all_variable(list))
		return loop_with_let_(ptr, list);
	else
		return loop_with_gensym_(ptr, list);
}

static int loop_variables_with_(Execute ptr, addr pos)
{
	if (singlep(pos)) {
		/* let */
		GetCar(pos, &pos);
		return loop_with_single_(ptr, pos);
	}
	else {
		/* and */
		return loop_with_multiple_(ptr, pos);
	}
}


/*
 *  for-as-up
 */
static int loop_variables_for_as_up_form_(Execute ptr, addr list)
{
	/* `(if ,var
	 *    (incf ,var ,by)
	 *    (setq ,var ,a2))
	 * `(unless (,less ,var ,g1)
	 *    (go end-loop))
	 */
	int check;
	addr var, a1, a2, b1, b2, by, g1, g2, pos;
	addr if_symbol, incf, setq, unless, less, go, end_loop;
	addr x, y;

	Return(list_bind_(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL));
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_INCF, &incf);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* increment */
	if (a1 == Unbound)
		fixnum_heap(&a2, 0);
	if (by == Unbound)
		fixnum_heap(&by, 1);
	list_heap(&x, incf, var, by, NULL);
	list_heap(&y, setq, var, a2, NULL);
	list_heap(&x, if_symbol, var, x, y, NULL);
	Return(push_form_loop_(ptr, x));

	/* loop */
	if (b1 != Unbound) {
		Return(loop_symbol_below_p_(b1, &check));
		if (check) {
			/* `(unless (< ,var ,g1) (go end-loop)) */
			GetConst(COMMON_NUMBER_LESS, &less);
		}
		else {
			/* `(unless (<= ,var ,g1) (go end-loop)) */
			GetConst(COMMON_NUMBER_LESS_EQUAL, &less);
		}
		list_heap(&less, less, var, g1, NULL);
		list_heap(&go, go, end_loop, NULL);
		list_heap(&pos, unless, less, go, NULL);
		Return(push_form_loop_(ptr, pos));
	}

	return 0;
}

static int loop_variables_for_as_up_init_(Execute ptr, addr list)
{
	/* `(let (,var ,g1)
	 *    ...)
	 * `(setq ,g1 ,b2)
	 */
	addr var, a1, a2, b1, b2, by, g1, g2, x;

	/* let */
	Return(list_bind_(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL));
	Return(push_vars_loop_(ptr, var));
	Return(push_vars_loop_(ptr, g1));

	/* setq */
	if (b1 != Unbound) {
		GetConst(COMMON_SETQ, &x);
		list_heap(&x, x, g1, b2, NULL);
		Return(push_init_loop_(ptr, x));
	}

	return 0;
}

static int loop_variables_for_as_up_(Execute ptr, addr list)
{
	Return(loop_variables_for_as_up_init_(ptr, list));
	Return(loop_variables_for_as_up_form_(ptr, list));

	return 0;
}


/*
 *  for-as down
 */
static int loop_variables_for_as_down_form_(Execute ptr, addr list)
{
	/* `(if ,var
	 *    (defc ,var ,by)
	 *    (setq ,var ,a2))
	 * `(unless (,greater ,var ,g1)
	 *    (go end-loop))
	 */
	int check;
	addr var, a1, a2, b1, b2, by, g1, g2, pos;
	addr if_symbol, decf, setq, unless, greater, go, end_loop;
	addr x, y;

	Return(list_bind_(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL));
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_DECF, &decf);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* increment */
	if (a1 == Unbound)
		fixnum_heap(&a2, 0);
	if (by == Unbound)
		fixnum_heap(&by, 1);
	list_heap(&x, decf, var, by, NULL);
	list_heap(&y, setq, var, a2, NULL);
	list_heap(&x, if_symbol, var, x, y, NULL);
	Return(push_form_loop_(ptr, x));

	/* loop */
	if (b1 != Unbound) {
		Return(loop_symbol_above_p_(b1, &check));
		if (check) {
			/* `(unless (< ,var ,g1) (go end-loop)) */
			GetConst(COMMON_NUMBER_GREATER, &greater);
		}
		else {
			/* `(unless (<= ,var ,g1) (go end-loop)) */
			GetConst(COMMON_NUMBER_GREATER_EQUAL, &greater);
		}
		list_heap(&greater, greater, var, g1, NULL);
		list_heap(&go, go, end_loop, NULL);
		list_heap(&pos, unless, greater, go, NULL);
		Return(push_form_loop_(ptr, pos));
	}

	return 0;
}

static int loop_variables_for_as_down_(Execute ptr, addr list)
{
	Return(loop_variables_for_as_up_init_(ptr, list));
	Return(loop_variables_for_as_down_form_(ptr, list));

	return 0;
}


/*
 *  in-list
 */
static int loop_destructuring_bind_tree_(Execute ptr,
		addr var, addr *gtree, addr *glist)
{
	addr g, a, b, x, y;

	if (var == Nil) {
		*gtree = Nil;
	}
	else if (! consp(var)) {
		Check(! symbolp(var), "type error");
		Return(make_gensym_(ptr, &g));
		*gtree = g;
		cons_heap(glist, var, *glist);
		cons_heap(glist, g, *glist);
	}
	else {
		GetCons(var, &a, &b);
		Return(loop_destructuring_bind_tree_(ptr, a, &x, glist));
		Return(loop_destructuring_bind_tree_(ptr, b, &y, glist));
		cons_heap(gtree, x, y);
	}

	return 0;
}

static int loop_destructuring_bind_(Execute ptr,
		addr var, addr type, addr value, addr *ret)
{
	/* `(destructuring-bind ,glist (loop-bind ',var ',type ,value)
	 *    (setq ,var1 ,g2 ,var2 ,g2 ...))
	 */
	addr dbind, lbind, setq, gtree, glist;

	GetConst(COMMON_DESTRUCTURING_BIND, &dbind);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_LOOP_BIND, &lbind);

	gtree = glist = Nil;
	Return(loop_destructuring_bind_tree_(ptr, var, &gtree, &glist));
	nreverse(&glist, glist);
	if (type == Unbound)
		type = T;
	quotelist_heap(&var, var);
	quotelist_heap(&type, type);
	list_heap(&lbind, lbind, var, type, value, NULL);
	cons_heap(&setq, setq, glist);
	list_heap(ret, dbind, gtree, lbind, setq, NULL);

	return 0;
}

static int loop_destructuring_setq_value_(Execute ptr,
		addr var, addr type, addr value, addr *ret1, addr *ret2)
{
	addr pos;

	/* destructuring-bind */
	if (consp(var)) {
		Return(loop_destructuring_bind_(ptr, var, type, value, ret1));
		*ret2 = Nil;
		return 0;
	}

	/* setq */
	GetConst(COMMON_SETQ, &pos);
	list_heap(ret1, pos, var, value, NULL);

	/* `(check-type ,var ,type) */
	if (type != Unbound) {
		GetConst(COMMON_CHECK_TYPE, &pos);
		list_heap(ret2, pos, var, type, NULL);
	}
	else {
		*ret2 = Nil;
	}

	return 0;
}

static int loop_destructuring_setq_(Execute ptr, addr var, addr type, addr value)
{
	addr x, y;

	Return(loop_destructuring_setq_value_(ptr, var, type, value, &x, &y));
	Return(push_form_loop_(ptr, x));
	if (y != Nil) {
		Return(push_form_loop_(ptr, y));
	}

	return 0;
}

static int loop_destructuring_setq_expr_(Execute ptr,
		addr var, addr type, addr value, addr *ret)
{
	addr x, y, progn;

	Return(loop_destructuring_setq_value_(ptr, var, type, value, &x, &y));
	if (y == Nil)
		return Result(ret, x);

	/* progn */
	GetConst(COMMON_PROGN, &progn);
	list_heap(ret, progn, x, y, NULL);

	return 0;
}

static int loop_variables_for_as_in_list_form_(Execute ptr,
		addr var, addr type, addr step, addr g)
{
	/* `(unless ,g
	 *    (go end-loop))
	 *  (for-setq ,var ,type (car ,g))
	 *  (setq ,g (funcall ,step ,g))
	 *  or
	 *  (setq ,g (cdr ,g))
	 */
	addr unless, go, end_loop, setq, funcall, car, cdr;
	addr x;

	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* `(unless ,g (go end-loop)) */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&x, unless, g, go, NULL);
	Return(push_form_loop_(ptr, x));

	/* (SETQ ,var ,type (car ,g)) */
	GetConst(COMMON_CAR, &car);
	list_heap(&x, car, g, NULL);
	Return(loop_destructuring_setq_(ptr, var, type, x));

	/* update */
	if (step != Unbound) {
		/* `(setq ,g (funcall ,step ,g)) */
		GetConst(COMMON_FUNCALL, &funcall);
		list_heap(&funcall, funcall, step, g, NULL);
		list_heap(&x, setq, g, funcall, NULL);
	}
	else {
		/* `(setq ,g (cdr ,g)) */
		GetConst(COMMON_CDR, &cdr);
		list_heap(&cdr, cdr, g, NULL);
		list_heap(&x, setq, g, cdr, NULL);
	}
	Return(push_form_loop_(ptr, x));

	return 0;
}

static int loop_initialy_setq_(Execute ptr, addr var, addr value)
{
	/* (setq ,var ,value) */
	addr pos;

	GetConst(COMMON_SETQ, &pos);
	list_heap(&pos, pos, var, value, NULL);
	return push_init_loop_(ptr, pos);
}

static int push_vars_tree_loop_(Execute ptr, addr tree)
{
	addr car, cdr;

	if (tree == Nil)
		return 0;
	if (! consp(tree))
		return push_vars_loop_(ptr, tree);
	GetCons(tree, &car, &cdr);
	Return(push_vars_tree_loop_(ptr, car));
	Return(push_vars_tree_loop_(ptr, cdr));

	return 0;
}

static int loop_variables_for_as_in_list_(Execute ptr, addr list)
{
	addr var, type, value, step, g;

	Return(list_bind_(list, &var, &type, &value, &step, &g, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, g));
	Return(loop_initialy_setq_(ptr, g, value));
	Return(loop_variables_for_as_in_list_form_(ptr, var, type, step, g));

	return 0;
}


/*
 *  on-list
 */
static int loop_variables_for_as_on_list_form_(Execute ptr,
		addr var, addr type, addr step, addr g)
{
	/* `(unless ,g
	 *    (go end-loop))
	 *  (for-setq ,var ,type ,g)
	 *  (setq ,g (funcall ,step ,g))
	 *  or
	 *  (setq ,g (cdr ,g))
	 */
	addr unless, go, end_loop, setq, funcall, cdr;
	addr x;

	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* `(unless ,g (go end-loop)) */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&x, unless, g, go, NULL);
	Return(push_form_loop_(ptr, x));

	/* (SETQ ,var ,type ,g) */
	Return(loop_destructuring_setq_(ptr, var, type, g));

	/* update */
	if (step != Unbound) {
		/* `(setq ,g (funcall ,step ,g)) */
		GetConst(COMMON_FUNCALL, &funcall);
		list_heap(&funcall, funcall, step, g, NULL);
		list_heap(&x, setq, g, funcall, NULL);
	}
	else {
		/* `(setq ,g (cdr ,g)) */
		GetConst(COMMON_CDR, &cdr);
		list_heap(&cdr, cdr, g, NULL);
		list_heap(&x, setq, g, cdr, NULL);
	}
	Return(push_form_loop_(ptr, x));

	return 0;
}

static int loop_variables_for_as_on_list_(Execute ptr, addr list)
{
	addr var, type, value, step, g;

	Return(list_bind_(list, &var, &type, &value, &step, &g, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, g));
	Return(loop_initialy_setq_(ptr, g, value));
	Return(loop_variables_for_as_on_list_form_(ptr, var, type, step, g));

	return 0;
}


/*
 *  equals-then
 */
static int loop_variables_for_as_equals_then_form_(Execute ptr,
		addr var, addr type, addr value, addr then, addr g)
{
	/* `(if ,g
	 *    (for-setq ,var ,type ,then)
	 *    (for-setq ,var ,type ,value))
	 *  (setq ,g t)
	 */
	addr if_symbol, setq, pos;

	if (then == Unbound)
		return loop_destructuring_setq_(ptr, var, type, value);

	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_SETQ, &setq);

	Return(loop_destructuring_setq_expr_(ptr, var, type, then, &then));
	Return(loop_destructuring_setq_expr_(ptr, var, type, value, &value));
	list_heap(&pos, if_symbol, g, then, value, NULL);
	Return(push_form_loop_(ptr, pos));
	list_heap(&pos, setq, g, T, NULL);
	Return(push_form_loop_(ptr, pos));

	return 0;
}

static int loop_variables_for_as_equals_then_(Execute ptr, addr list)
{
	addr var, type, value, then, g;

	Return(list_bind_(list, &var, &type, &value, &then, &g, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, g));
	Return(loop_variables_for_as_equals_then_form_(ptr, var, type, value, then, g));

	return 0;
}


/*
 *  across
 */
static int loop_variables_for_as_across_form_(Execute ptr, addr list)
{
	/* `(when ,a
	 *    (go across-update))
	 *  (setq ,a ,array)
	 *  (setq ,b 0)
	 *  (setq ,c (length ,a))
	 *  across-update
	 *  (unless (< ,b ,c)
	 *    (go end-loop))
	 *  (for-setq ,var ,type (elt ,a ,b))
	 *  (incf ,b)
	 */
	addr var, type, array, a, b, c, x;
	addr when, setq, length, unless, less, go, end_loop, elt, incf, label;

	Return(list_bind_(list, &var, &type, &array, &a, &b, &c, NULL));
	GetConst(COMMON_WHEN, &when);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_LENGTH, &length);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_NUMBER_LESS, &less);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_ELT, &elt);
	GetConst(COMMON_INCF, &incf);
	GetConst(SYSTEM_END_LOOP, &end_loop);
	make_symbolchar(&label, "ACROSS-UPDATE");

	/* when */
	list_heap(&x, go, label, NULL);
	list_heap(&x, when, a, x, NULL);
	Return(push_form_loop_(ptr, x));

	/* setq */
	list_heap(&x, setq, a, array, NULL);
	Return(push_form_loop_(ptr, x));
	fixnum_heap(&x, 0);
	list_heap(&x, setq, b, x, NULL);
	Return(push_form_loop_(ptr, x));
	list_heap(&x, length, a, NULL);
	list_heap(&x, setq, c, x, NULL);
	Return(push_form_loop_(ptr, x));

	/* label */
	Return(push_form_loop_(ptr, label));

	/* (unless (< b c) (go end-loop)) */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&less, less, b, c, NULL);
	list_heap(&x, unless, less, go, NULL);
	Return(push_form_loop_(ptr, x));

	/* (for-setq ,var ,type `(elt ,a ,b)) */
	list_heap(&x, elt, a, b, NULL);
	Return(loop_destructuring_setq_(ptr, var, type, x));

	/* expr2: `(incf ,b) */
	list_heap(&x, incf, b, NULL);
	Return(push_form_loop_(ptr, x));

	return 0;
}

static int loop_variables_for_as_across_(Execute ptr, addr list)
{
	addr var, type, array, a, b, c;

	Return(list_bind_(list, &var, &type, &array, &a, &b, &c, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, a));
	Return(push_vars_tree_loop_(ptr, b));
	Return(push_vars_tree_loop_(ptr, c));
	return loop_variables_for_as_across_form_(ptr, list);

}


/*
 *  hash
 */
static int loop_variables_for_as_hash_form_(Execute ptr, addr list)
{
	/* `(unless ,g
	 *    (setq ,g (lisp-system::make-hash-iterator ,table)))
	 *  (multiple-value-bind (,check ,key ,value) (lisp-system::next-hash-iterator ,g)
	 *    (declare (ignorable key value))
	 *    (unless ,check (go end-loop))
	 *    (for-setq ,var ,type ,key)
	 *    (setq ,use ,value))
	 */
	addr var, type, keyp, table, use, g, x, y, z, a, b;
	addr key, value, check;
	addr unless, setq, make, mvbind, next, declare, ignorable, go, end_loop;

	Return(list_bind_(list, &var, &type, &keyp, &table, &use, &g, NULL));

	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_MAKE_HASH_ITERATOR, &make);

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(SYSTEM_NEXT_HASH_ITERATOR, &next);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* (unless ...) */
	list_heap(&x, make, table, NULL);
	list_heap(&x, setq, g, x, NULL);
	list_heap(&x, unless, g, x, NULL);
	Return(push_form_loop_(ptr, x));

	/* (multiple-value-bind ...) */
	Return(make_gensym_(ptr, &check));
	Return(make_gensym_(ptr, &key));
	Return(make_gensym_(ptr, &value));
	list_heap(&z, check, key, value, NULL);
	list_heap(&next, next, g, NULL);
	list_heap(&ignorable, ignorable, key, value, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&go, go, end_loop, NULL);
	list_heap(&unless, unless, check, go, NULL);
	if (keyp != Nil) {
		a = key;
		b = value;
	}
	else {
		a = value;
		b = key;
	}
	Return(loop_destructuring_setq_expr_(ptr, var, type, a, &x));
	if (use != Unbound) {
		list_heap(&y, setq, use, b, NULL);
		list_heap(&x, mvbind, z, next, declare, unless, x, y, NULL);
	}
	else {
		list_heap(&x, mvbind, z, next, declare, unless, x, NULL);
	}

	return push_form_loop_(ptr, x);
}

static int loop_variables_for_as_hash_(Execute ptr, addr list)
{
	addr var, type, keyp, table, use, g;

	Return(list_bind_(list, &var, &type, &keyp, &table, &use, &g, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, g));
	return loop_variables_for_as_hash_form_(ptr, list);
}


/*
 *  package
 */
static int loop_variables_for_as_package_init_(Execute ptr, addr pos, addr list)
{
	/* `(unless ,g
	 *    (setq ,g (lisp-system::make-hash-iterator
	 *               (find-package ,package)
	 *               ,internal ,external ,inherited)))
	 */
	addr var, type, package, g, x;
	addr unless, setq, make, find, internal, external, inherited;

	Return(list_bind_(list, &var, &type, &package, &g, NULL));
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_MAKE_PACKAGE_ITERATOR, &make);
	GetConst(COMMON_FIND_PACKAGE, &find);

	internal = external = inherited = Nil;
	if (loop_symbol_for_as_package_present_p(pos)) {
		internal = external = T;
		inherited = Nil;
	}
	if (loop_symbol_for_as_package_symbol_p(pos)) {
		internal = external = inherited = T;
	}
	if (loop_symbol_for_as_package_external_p(pos)) {
		external = T;
		internal = inherited = Nil;
	}

	/* (unless ...) */
	list_heap(&x, find, package, NULL);
	list_heap(&x, make, x, internal, external, inherited, NULL);
	list_heap(&x, setq, g, x, NULL);
	list_heap(&x, unless, g, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_variables_for_as_package_form_(Execute ptr, addr list)
{
	/* `(multiple-value-bind (,check ,symbol)
	 *    (lisp-system::next-package-iterator ,g)
	 *    (unless ,check (go end-loop))
	 *    (for-setq ,var ,type ,symbol))
	 */
	addr var, type, package, g, x, y, symbol, check;
	addr mvbind, next, unless, go, end_loop;

	Return(list_bind_(list, &var, &type, &package, &g, NULL));
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(SYSTEM_NEXT_PACKAGE_ITERATOR, &next);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* (multiple-value-bind ...) */
	Return(make_gensym_(ptr, &check));
	Return(make_gensym_(ptr, &symbol));
	list_heap(&y, check, symbol, NULL);
	list_heap(&next, next, g, NULL);
	list_heap(&go, go, end_loop, NULL);
	list_heap(&unless, unless, check, go, NULL);
	Return(loop_destructuring_setq_expr_(ptr, var, type, symbol, &x));
	list_heap(&x, mvbind, y, next, unless, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_variables_for_as_package_(Execute ptr, addr pos, addr list)
{
	addr var, type, package, g;

	Return(list_bind_(list, &var, &type, &package, &g, NULL));
	Return(push_vars_tree_loop_(ptr, var));
	Return(push_vars_tree_loop_(ptr, g));
	Return(loop_variables_for_as_package_init_(ptr, pos, list));
	Return(loop_variables_for_as_package_form_(ptr, list));

	return 0;
}


/*
 *  initially
 */
static int loop_variables_initially_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(push_init_loop_(ptr, pos));
	}

	return 0;
}


/*
 *  finally
 */
static int loop_variables_finally_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(push_final_loop_(ptr, pos));
	}

	return 0;
}


/*
 *  interface
 */
static int loop_variables_for_as_type_(Execute ptr, addr list)
{
	addr pos;

	GetCons(list, &pos, &list);
	/* up */
	if (loop_symbol_for_as_arithmetic_up_p(pos))
		return loop_variables_for_as_up_(ptr, list);
	/* downto */
	if (loop_symbol_for_as_arithmetic_downto_p(pos))
		return loop_variables_for_as_down_(ptr, list);
	/* downfrom */
	if (loop_symbol_for_as_arithmetic_downfrom_p(pos))
		return loop_variables_for_as_down_(ptr, list);
	/* in-list */
	if (loop_symbol_for_as_in_list_p(pos))
		return loop_variables_for_as_in_list_(ptr, list);
	/* on-list */
	if (loop_symbol_for_as_on_list_p(pos))
		return loop_variables_for_as_on_list_(ptr, list);
	/* equals-then */
	if (loop_symbol_for_as_equals_then_p(pos))
		return loop_variables_for_as_equals_then_(ptr, list);
	/* across */
	if (loop_symbol_for_as_across_p(pos))
		return loop_variables_for_as_across_(ptr, list);
	/* hash */
	if (loop_symbol_for_as_hash_p(pos))
		return loop_variables_for_as_hash_(ptr, list);
	/* package */
	if (loop_symbol_for_as_package_symbol_p(pos))
		return loop_variables_for_as_package_(ptr, pos, list);
	if (loop_symbol_for_as_package_present_p(pos))
		return loop_variables_for_as_package_(ptr, pos, list);
	if (loop_symbol_for_as_package_external_p(pos))
		return loop_variables_for_as_package_(ptr, pos, list);

	/* error */
	return fmte_("Invalid variables-clause ~S.", pos, NULL);
}

static int loop_variables_for_as_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(loop_variables_for_as_type_(ptr, pos));
	}

	return 0;
}

static int loop_variables_car_(Execute ptr, addr list)
{
	addr pos;

	GetCons(list, &pos, &list);
	/* with */
	if (loop_symbol_with_p(pos))
		return loop_variables_with_(ptr, list);
	/* for-as */
	if (loop_symbol_for_as_p(pos))
		return loop_variables_for_as_(ptr, list);
	/* initially */
	if (loop_symbol_initially_p(pos))
		return loop_variables_initially_(ptr, list);
	/* finally */
	if (loop_symbol_finally_p(pos))
		return loop_variables_finally_(ptr, list);

	/* error */
	return fmte_("Invalid variables-clause ~S.", pos, NULL);
}

int loop_variables_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(loop_variables_car_(ptr, pos));
	}

	return 0;
}

