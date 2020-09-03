#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "hold.h"
#include "loop.h"
#include "loop_bind.h"
#include "loop_parse.h"
#include "loop_symbol.h"
#include "loop_variables.h"
#include "object.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  variables
 */
static void loop_filter_index(addr *form, addr *list, constindex index)
{
	addr root, x, y, z;

	GetConstant(index, &z);
	for (root = Nil; *form != Nil; ) {
		GetCons(*form, &x, form);
		Check(! consp(x), "type error");
		GetCar(x, &y);
		if (y != z) {
			cons_heap(&root, x, root);
		}
		else {
			GetCdr(x, &x);
			while (x != Nil) {
				GetCons(x, &y, &x);
				cons_heap(list, y, *list);
			}
		}
	}
	nreverse(form, root);
}

_g void loop_filter_initially(addr *form, addr *list)
{
	loop_filter_index(form, list, CONSTANT_SYSTEM_LOOP_INITIALLY);
}

_g void loop_filter_finally(addr *form, addr *list)
{
	loop_filter_index(form, list, CONSTANT_SYSTEM_LOOP_FINALLY);
}

static int loop_filter_with_default_(Execute ptr, addr list, addr *ret)
{
	addr root, pos, var, type, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		Return(list_bind_(pos, &var, &type, &value, NULL));
		if (value == Unbound) {
			Return(loop_bind_initial_list_(ptr, var, type, &value));
			list_heap(&pos, var, type, value, NULL);
		}
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse(ret, root);

	return 0;
}

_g int loop_filter_with_(Execute ptr, addr *form, addr *list)
{
	addr root, x, y, z;
	LocalHold hold;

	GetConst(SYSTEM_LOOP_WITH, &z);
	hold = LocalHold_array(ptr, 2);
	for (root = Nil; *form != Nil; ) {
		GetCons(*form, &x, form);
		Check(! consp(x), "type error");
		GetCar(x, &y);
		if (y != z) {
			cons_heap(&root, x, root);
			localhold_set(hold, 0, root);
		}
		else {
			GetCdr(x, &x);
			Return(loop_filter_with_default_(ptr, x, &x));
			cons_heap(list, x, *list);
			localhold_set(hold, 1, *list);
		}
	}
	localhold_end(hold);
	nreverse(form, root);

	return 0;
}


/*
 *  with
 */
static void loop_with_single_bind(addr *form, addr var, addr type, addr value)
{
	addr qvar, dbind, bind, let, declare, dtype;

	Check(value == Unbound, "unbound error");
	if (! consp(var)) {
		/* `(let ((,var ,value))
		 *    (declare (type ,var ,type))
		 *    ,form)
		 */
		GetConst(COMMON_LET, &let);
		list_heap(&value, var, value, NULL);
		list_heap(&value, value, NULL);
		if (type == Unbound) {
			list_heap(form, let, value, *form, NULL);
		}
		else {
			GetConst(COMMON_DECLARE, &declare);
			GetConst(COMMON_TYPE, &dtype);
			list_heap(&dtype, dtype, type, var, NULL);
			list_heap(&declare, declare, dtype, NULL);
			list_heap(form, let, value, declare, *form, NULL);
		}
	}
	else {
		/* `(destructuring-bind ,var (lisp-system::loop-bind ,var ,type ,value)
		 *    ,form)
		 */
		if (type == Unbound)
			GetTypeTable(&type, T);
		GetConst(COMMON_DESTRUCTURING_BIND, &dbind);
		GetConst(SYSTEM_LOOP_BIND, &bind);
		quotelist_heap(&qvar, var);
		quotelist_heap(&type, type);
		list_heap(&bind, bind, qvar, type, value, NULL);
		list_heap(form, dbind, var, bind, *form, NULL);
	}
}

static int loop_with_single_(addr *form, addr list)
{
	addr var, type, value;
	Return(list_bind_(list, &var, &type, &value, NULL));
	loop_with_single_bind(form, var, type, value);

	return 0;
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

static int loop_with_let_(addr *form, addr list)
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
		Return(list_bind_(pos, &var, &type, &value, NULL));
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
		Return(list_bind_(pos, &var, &type, &value, NULL));
		if (type != Unbound) {
			list_heap(&var, dtype, var, type, NULL);
			list_heap(&var, declare, var, NULL);
			cons_heap(&root, var, root);
		}
	}
	cons_heap(&root, *form, root);
	nreverse(form, root);

	return 0;
}

static int loop_with_gensym_form_(addr *form, addr list)
{
	addr pos;

	if (list == Nil)
		return 0;
	GetCons(list, &pos, &list);
	Return(loop_with_gensym_form_(form, list));
	return loop_with_single_(form, pos);
}

static int loop_with_gensym_(Execute ptr, addr *form, addr list)
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
		Return(list_bind_(pos, &var, &type, &value, NULL));
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
	Return(loop_with_gensym_form_(form, glist));
	GetConst(COMMON_LET, &let);
	list_heap(form, let, letlist, *form, NULL);

	return 0;
}

static int loop_with_multiple_(Execute ptr, addr *form, addr list)
{
	if (loop_with_all_variable(list))
		return loop_with_let_(form, list);
	else
		return loop_with_gensym_(ptr, form, list);
}

_g int loop_variables_with_(Execute ptr, addr *form, addr list)
{
	addr pos;

	/* recursive */
	if (list == Nil)
		return 0;
	GetCons(list, &pos, &list);
	/* next */
	Return(loop_variables_with_(ptr, form, list));
	/* bind */
	if (singlep(pos)) {
		/* let */
		GetCar(pos, &pos);
		return loop_with_single_(form, pos);
	}
	else {
		/* and */
		return loop_with_multiple_(ptr, form, pos);
	}
}


/*
 *  for-as
 */
static int loop_push_for_as_up_(addr *expr1, addr *expr2, addr list)
{
	int check;
	addr var, a1, a2, b1, b2, by, g1, g2;
	addr pos, unless, less, go, loop, incf;

	Return(list_bind_(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL));
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_INCF, &incf);
	GetConst(SYSTEM_END_LOOP, &loop);
	if (b1 == Unbound) {
		less = Unbound;
		goto next;
	}
	Return(loop_symbol_below_p_(b1, &check));
	if (check) {
		/* expr1: `(unless (< ,var ,g1) (go end-loop)) */
		GetConst(COMMON_NUMBER_LESS, &less);
		goto next;
	}
	/* expr1: `(unless (<= ,var ,g1) (go end-loop)) */
	GetConst(COMMON_NUMBER_LESS_EQUAL, &less);
next:
	if (less != Unbound) {
		list_heap(&less, less, var, g1, NULL);
		list_heap(&go, go, loop, NULL);
		list_heap(&pos, unless, less, go, NULL);
		cons_heap(expr1, pos, *expr1);
	}

	/* expr2: `(incf ,var ,g2) */
	list_heap(&pos, incf, var, g2, NULL);
	cons_heap(expr2, pos, *expr2);

	return 0;
}
static int loop_variables_for_as_up_(addr *form, addr list)
{
	/* `(let ((,var ,a2)
	 *        (,g1 ,b2)
	 *        (,g2 ,by))
	 *    ,form)
	 */
	addr var, a1, a2, b1, b2, by, g1, g2;
	addr args, let;

	Return(list_bind_(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL));
	args = Nil;
	/* from */
	if (a1 == Unbound)
		fixnum_heap(&a2, 0);
	list_heap(&var, var, a2, NULL);
	cons_heap(&args, var, args);
	/* to */
	if (b1 != Unbound) {
		list_heap(&g1, g1, b2, NULL);
		cons_heap(&args, g1, args);
	}
	/* by */
	if (by == Unbound)
		fixnum_heap(&by, 1);
	list_heap(&g2, g2, by, NULL);
	cons_heap(&args, g2, args);
	/* let */
	nreverse(&args, args);
	GetConst(COMMON_LET, &let);
	list_heap(form, let, args, *form, NULL);

	return 0;
}

static int loop_push_for_as_down_(addr *expr1, addr *expr2, addr list)
{
	int check;
	addr var, a1, a2, b1, b2, by, g1, g2;
	addr pos, unless, greater, go, loop, decf;

	Return(list_bind_(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL));
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_DECF, &decf);
	GetConst(SYSTEM_END_LOOP, &loop);
	if (b1 == Unbound) {
		greater = Unbound;
		goto next;
	}
	Return(loop_symbol_above_p_(b1, &check));
	if (check) {
		/* expr1: `(unless (> ,var ,g1) (go end-loop)) */
		GetConst(COMMON_NUMBER_GREATER, &greater);
		goto next;
	}
	/* expr1: `(unless (>= ,var ,g1) (go end-loop)) */
	GetConst(COMMON_NUMBER_GREATER_EQUAL, &greater);
next:
	if (greater != Unbound) {
		list_heap(&greater, greater, var, g1, NULL);
		list_heap(&go, go, loop, NULL);
		list_heap(&pos, unless, greater, go, NULL);
		cons_heap(expr1, pos, *expr1);
	}

	/* expr2: `(decf ,var ,g2) */
	list_heap(&pos, decf, var, g2, NULL);
	cons_heap(expr2, pos, *expr2);

	return 0;
}

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
		GetTypeTable(&type, T);
	quotelist_heap(&var, var);
	quotelist_heap(&type, type);
	list_heap(&lbind, lbind, var, type, value, NULL);
	cons_heap(&setq, setq, glist);
	list_heap(ret, dbind, gtree, lbind, setq, NULL);

	return 0;
}

static int loop_destructuring_setq_(Execute ptr,
		addr var, addr type, addr value, addr *ret)
{
	addr setq;

	if (consp(var)) {
		Return(loop_destructuring_bind_(ptr, var, type, value, ret));
	}
	else {
		GetConst(COMMON_SETQ, &setq);
		list_heap(ret, setq, var, value, NULL);
	}

	return 0;
}

static int loop_push_for_as_in_list_(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr var, type, value, step, g, x;
	addr unless, go, end_loop, setq, funcall, car, cdr;

	Return(list_bind_(list, &var, &type, &value, &step, &g, NULL));
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_END_LOOP, &end_loop);
	/* expr1: `(unless ,g (go end-loop)) */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&unless, unless, g, go, NULL);
	cons_heap(expr1, unless, *expr1);
	/* expr1: (SETQ ,var ,type (car ,g)) */
	GetConst(COMMON_CAR, &car);
	list_heap(&car, car, g, NULL);
	Return(loop_destructuring_setq_(ptr, var, type, car, &x));
	cons_heap(expr1, x, *expr1);
	/* expr2 */
	if (step != Unbound) {
		/* expr2: `(setq ,g (funcall ,step ,g)) */
		GetConst(COMMON_FUNCALL, &funcall);
		list_heap(&funcall, funcall, step, g, NULL);
		list_heap(&x, setq, g, funcall, NULL);
		cons_heap(expr2, x, *expr2);
	}
	else {
		/* expr2: `(setq ,g (cdr ,g)) */
		GetConst(COMMON_CDR, &cdr);
		list_heap(&cdr, cdr, g, NULL);
		list_heap(&x, setq, g, cdr, NULL);
		cons_heap(expr2, x, *expr2);
	}

	return 0;
}

static void loop_let_variables_recursive(addr var, addr *list)
{
	addr a, b;

	if (var == Nil) {
		return;
	}
	else if (! consp(var)) {
		Check(! symbolp(var), "type error");
		cons_heap(list, var, *list);
	}
	else {
		GetCons(var, &a, &b);
		loop_let_variables_recursive(a, list);
		loop_let_variables_recursive(b, list);
	}
}

static void loop_let_variables(addr *form, addr var)
{
	addr list, let, x;

	if (! consp(var)) {
		/* `(let ((,var)) ,form) */
		GetConst(COMMON_LET, &let);
		list_heap(&x, var, NULL);
		list_heap(&x, x, NULL);
		list_heap(form, let, x, *form, NULL);
		return;
	}
	list = Nil;
	loop_let_variables_recursive(var, &list);
	nreverse(&list, list);
	/* (let ,list ,form) */
	GetConst(COMMON_LET, &let);
	list_heap(form, let, list, *form, NULL);
}

static int loop_variables_for_as_in_list_(addr *form, addr list)
{
	addr var, type, value, step, g;

	Return(list_bind_(list, &var, &type, &value, &step, &g, NULL));
	loop_with_single_bind(form, g, Unbound, value);
	loop_let_variables(form, var);

	return 0;
}

static int loop_push_for_as_on_list_(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr var, type, value, step, g, x;
	addr unless, go, end_loop, setq, funcall, cdr;

	Return(list_bind_(list, &var, &type, &value, &step, &g, NULL));
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_END_LOOP, &end_loop);
	if (! consp(var))
		g = var;
	/* expr1: `(unless ,g (go end-loop)) */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&unless, unless, g, go, NULL);
	cons_heap(expr1, unless, *expr1);
	/* destructuring-bind */
	if (consp(var)) {
		/* expr1: `(destructuring-bind ,glist (loop-bind ',var ',type ,g)
		 *      :    (setq ,var1 ,g2 ,var2 ,g2 ...))
		 */
		Return(loop_destructuring_bind_(ptr, var, type, g, &x));
		cons_heap(expr1, x, *expr1);
	}
	if (step != Unbound) {
		/* expr2: `(setq ,g (funcall ,step ,g)) */
		GetConst(COMMON_FUNCALL, &funcall);
		list_heap(&funcall, funcall, step, g, NULL);
		list_heap(&setq, setq, g, funcall, NULL);
		cons_heap(expr2, setq, *expr2);
	}
	else {
		/* expr2: `(setq ,g (cdr ,g)) */
		GetConst(COMMON_CDR, &cdr);
		list_heap(&cdr, cdr, g, NULL);
		list_heap(&setq, setq, g, cdr, NULL);
		cons_heap(expr2, setq, *expr2);
	}

	return 0;
}
static int loop_variables_for_as_on_list_(addr *form, addr list)
{
	addr var, type, value, step, g;

	Return(list_bind_(list, &var, &type, &value, &step, &g, NULL));
	if (consp(var)) {
		loop_with_single_bind(form, g, Unbound, value);
		loop_let_variables(form, var);
	}
	else {
		loop_with_single_bind(form, var, Unbound, value);
	}

	return 0;
}

static int loop_push_for_as_equals_then_(Execute ptr,
		addr *expr1, addr *expr2, addr list)
{
	addr var, type, value, then, g, x;

	Return(list_bind_(list, &var, &type, &value, &then, &g, NULL));
	if (then == Unbound)
		then = value;
	Return(loop_destructuring_setq_(ptr, var, type, then, &x));
	cons_heap(expr2, x, *expr2);

	return 0;
}
static int loop_variables_for_as_equals_then_(addr *form, addr list)
{
	addr var, type, value, step, g;
	Return(list_bind_(list, &var, &type, &value, &step, &g, NULL));
	loop_with_single_bind(form, var, type, value);

	return 0;
}

static int loop_push_for_as_across_(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr var, type, vector, g1, g2, g3, x;
	addr unless, less, go, end_loop, elt, incf;

	Return(list_bind_(list, &var, &type, &vector, &g1, &g2, &g3, NULL));
	/* expr1: (unless (< g2 g3) (go end-loop)) */
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_NUMBER_LESS, &less);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);
	list_heap(&go, go, end_loop, NULL);
	list_heap(&less, less, g2, g3, NULL);
	list_heap(&unless, unless, less, go, NULL);
	cons_heap(expr1, unless, *expr1);
	/* expr1: (SETQ ,var ,type `(elt ,g1 ,g2)) */
	GetConst(COMMON_ELT, &elt);
	list_heap(&elt, elt, g1, g2, NULL);
	Return(loop_destructuring_setq_(ptr, var, type, elt, &x));
	cons_heap(expr1, x, *expr1);
	/* expr2: `(incf ,g2) */
	GetConst(COMMON_INCF, &incf);
	list_heap(&x, incf, g2, NULL);
	cons_heap(expr2, x, *expr2);

	return 0;
}
static int loop_variables_for_as_across_(addr *form, addr list)
{
	addr var, type, vector, g1, g2, g3, let, length, zero;

	Return(list_bind_(list, &var, &type, &vector, &g1, &g2, &g3, NULL));
	loop_let_variables(form, var);

	/* `(let* ((,g1 ,vector)
	 *         (,g2 0)
	 *         (,g3 (length ,g1)))
	 *    ,form)
	 */
	GetConst(COMMON_LETA, &let);
	GetConst(COMMON_LENGTH, &length);
	fixnum_heap(&zero, 0);
	list_heap(&length, length, g1, NULL);
	list_heap(&g1, g1, vector, NULL);
	list_heap(&g2, g2, zero, NULL);
	list_heap(&g3, g3, length, NULL);
	list_heap(&g1, g1, g2, g3, NULL);
	list_heap(form, let, g1, *form, NULL);

	return 0;
}

static int loop_push_for_as_hash_(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr var, type, keyp, table, use, g, x, y, z;
	addr key, value, check;
	addr mvbind, next, declare, ignorable, unless, go, end_loop, setq;

	Return(make_gensym_(ptr, &check));
	Return(make_gensym_(ptr, &key));
	Return(make_gensym_(ptr, &value));
	Return(list_bind_(list, &var, &type, &keyp, &table, &use, &g, NULL));
	/* expr1: `(multiple-value-bind (,check ,key ,value)
	 *             (lisp-system::next-hash-iterator ,g)
	 *           (declare (ignorable key value))
	 *           (unless ,check (go end-loop))
	 *           (SETQ ,var ,type ,key)
	 *           (setq ,use ,value))
	 */
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(SYSTEM_NEXT_HASH_ITERATOR, &next);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);
	GetConst(COMMON_SETQ, &setq);
	list_heap(&z, check, key, value, NULL);
	list_heap(&next, next, g, NULL);
	list_heap(&ignorable, ignorable, key, value, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	list_heap(&go, go, end_loop, NULL);
	list_heap(&unless, unless, check, go, NULL);
	y = Nil;
	if (keyp != Nil) {
		Return(loop_destructuring_setq_(ptr, var, type, key, &x));
		if (use != Unbound)
			list_heap(&y, setq, use, value, NULL);
	}
	else {
		Return(loop_destructuring_setq_(ptr, var, type, value, &x));
		if (use != Unbound)
			list_heap(&y, setq, use, key, NULL);
	}
	if (use != Unbound)
		list_heap(&x, mvbind, z, next, declare, unless, x, y, NULL);
	else
		list_heap(&x, mvbind, z, next, declare, unless, x, NULL);
	cons_heap(expr1, x, *expr1);

	return 0;
}
static int loop_variables_for_as_hash_(addr *form, addr list)
{
	addr var, type, keyp, table, use, g, let, hash;

	Return(list_bind_(list, &var, &type, &keyp, &table, &use, &g, NULL));
	loop_let_variables(form, var);

	/* `(let ((,g (lisp-system::make-hash-iterator ,table)))
	 *    ,form)
	 */
	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_MAKE_HASH_ITERATOR, &hash);
	list_heap(&hash, hash, table, NULL);
	list_heap(&g, g, hash, NULL);
	list_heap(&g, g, NULL);
	list_heap(form, let, g, *form, NULL);

	return 0;
}

static int loop_push_for_as_package_(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr var, type, package, g, x, y;
	addr symbol, check;
	addr mvbind, next, unless, go, end_loop;

	Return(make_gensym_(ptr, &check));
	Return(make_gensym_(ptr, &symbol));
	Return(list_bind_(list, &var, &type, &package, &g, NULL));
	/* expr1: `(multiple-value-bind (,check ,symbol)
	 *             (lisp-system::next-package-iterator ,g)
	 *           (unless ,check (go end-loop))
	 *           (SETQ ,var ,type ,symbol))
	 */
	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(SYSTEM_NEXT_PACKAGE_ITERATOR, &next);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);
	list_heap(&y, check, symbol, NULL);
	list_heap(&next, next, g, NULL);
	list_heap(&go, go, end_loop, NULL);
	list_heap(&unless, unless, check, go, NULL);
	Return(loop_destructuring_setq_(ptr, var, type, symbol, &x));
	list_heap(&x, mvbind, y, next, unless, x, NULL);
	cons_heap(expr1, x, *expr1);

	return 0;
}

static int loop_variables_for_as_package_(addr *form, addr pos, addr list)
{
	addr var, type, package, g, let, hash, find, check;
	addr internal, external, inherited;

	Return(list_bind_(list, &var, &type, &package, &g, NULL));
	loop_let_variables(form, var);

	internal = external = inherited = Nil;
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT, &check);
	if (pos == check) {
		internal = external = T;
		inherited = Nil;
	}
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL, &check);
	if (pos == check) {
		internal = external = inherited = T;
	}
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL, &check);
	if (pos == check) {
		external = T;
		internal = inherited = Nil;
	}

	/* `(let ((,g (lisp-system::make-hash-iterator
	 *              (find-package ,package)
	 *              ,internal ,external ,inherited)))
	 *    ,form)
	 */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_FIND_PACKAGE, &find);
	GetConst(SYSTEM_MAKE_PACKAGE_ITERATOR, &hash);
	list_heap(&find, find, package, NULL);
	list_heap(&hash, hash, find, internal, external, inherited, NULL);
	list_heap(&g, g, hash, NULL);
	list_heap(&g, g, NULL);
	list_heap(form, let, g, *form, NULL);

	return 0;
}

static int loop_push_for_as_list_(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr pos, a, b, check;

	if (list == Nil)
		return 0;
	GetCons(list, &pos, &list);
	/* next */
	Return(loop_push_for_as_list_(ptr, expr1, expr2, list));
	/* up */
	GetCons(pos, &a, &b);
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP, &check);
	if (a == check)
		return loop_push_for_as_up_(expr1, expr2, b);
	/* downto */
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO, &check);
	if (a == check)
		return loop_push_for_as_down_(expr1, expr2, b);
	/* downfrom */
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM, &check);
	if (a == check)
		return loop_push_for_as_down_(expr1, expr2, b);
	/* in-list */
	GetConst(SYSTEM_LOOP_FOR_AS_IN_LIST, &check);
	if (a == check)
		return loop_push_for_as_in_list_(ptr, expr1, expr2, b);
	/* on-list */
	GetConst(SYSTEM_LOOP_FOR_AS_ON_LIST, &check);
	if (a == check)
		return loop_push_for_as_on_list_(ptr, expr1, expr2, b);
	/* equals-then */
	GetConst(SYSTEM_LOOP_FOR_AS_EQUALS_THEN, &check);
	if (a == check)
		return loop_push_for_as_equals_then_(ptr, expr1, expr2, b);
	/* across */
	GetConst(SYSTEM_LOOP_FOR_AS_ACROSS, &check);
	if (a == check)
		return loop_push_for_as_across_(ptr, expr1, expr2, b);
	/* hash */
	GetConst(SYSTEM_LOOP_FOR_AS_HASH, &check);
	if (a == check)
		return loop_push_for_as_hash_(ptr, expr1, expr2, b);
	/* package */
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL, &check);
	if (a == check)
		return loop_push_for_as_package_(ptr, expr1, expr2, b);
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT, &check);
	if (a == check)
		return loop_push_for_as_package_(ptr, expr1, expr2, b);
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL, &check);
	if (a == check)
		return loop_push_for_as_package_(ptr, expr1, expr2, b);
	/* error */
	return fmte_("Invalid variables-clause ~S.", a, NULL);
}

static int loop_variables_for_as_list_(addr *form, addr list)
{
	addr pos, a, b, check;

	if (list == Nil)
		return 0;
	GetCons(list, &pos, &list);
	/* next */
	Return(loop_variables_for_as_list_(form, list));
	/* up */
	GetCons(pos, &a, &b);
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP, &check);
	if (a == check)
		return loop_variables_for_as_up_(form, b);
	/* downto */
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO, &check);
	if (a == check)
		return loop_variables_for_as_up_(form, b);
	/* downfrom */
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM, &check);
	if (a == check)
		return loop_variables_for_as_up_(form, b);
	/* in-list */
	GetConst(SYSTEM_LOOP_FOR_AS_IN_LIST, &check);
	if (a == check)
		return loop_variables_for_as_in_list_(form, b);
	/* on-list */
	GetConst(SYSTEM_LOOP_FOR_AS_ON_LIST, &check);
	if (a == check)
		return loop_variables_for_as_on_list_(form, b);
	/* equals-then */
	GetConst(SYSTEM_LOOP_FOR_AS_EQUALS_THEN, &check);
	if (a == check)
		return loop_variables_for_as_equals_then_(form, b);
	/* across */
	GetConst(SYSTEM_LOOP_FOR_AS_ACROSS, &check);
	if (a == check)
		return loop_variables_for_as_across_(form, b);
	/* hash */
	GetConst(SYSTEM_LOOP_FOR_AS_HASH, &check);
	if (a == check)
		return loop_variables_for_as_hash_(form, b);
	/* package */
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL, &check);
	if (a == check)
		return loop_variables_for_as_package_(form, a, b);
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT, &check);
	if (a == check)
		return loop_variables_for_as_package_(form, a, b);
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL, &check);
	if (a == check)
		return loop_variables_for_as_package_(form, a, b);
	/* error */
	return fmte_("Invalid variables-clause ~S.", a, NULL);
}

_g int loop_push_for_as_(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr pos, check, next;

	/* loop */
	if (list == Nil)
		return 0;
	GetCons(list, &next, &list);
	Return(loop_push_for_as_(ptr, expr1, expr2, list));
	/* for-as */
	if (! consp_getcons(next, &pos, &list))
		goto error;
	GetConst(SYSTEM_LOOP_FOR_AS, &check);
	if (pos == check)
		return loop_push_for_as_list_(ptr, expr1, expr2, list);
	return 0;

	/* error */
error:
	return fmte_("Invalid loop for-as form ~S.", next, NULL);
}

_g int loop_variables_for_as_(addr *form, addr list)
{
	addr pos, check, next;

	/* loop */
	if (list == Nil)
		return 0;
	GetCons(list, &next, &list);
	Return(loop_variables_for_as_(form, list));
	/* for-as */
	if (! consp_getcons(next, &pos, &list))
		goto error;
	GetConst(SYSTEM_LOOP_FOR_AS, &check);
	if (pos == check)
		return loop_variables_for_as_list_(form, list);
	return 0;

	/* error */
error:
	return fmte_("Invalid loop for-as form ~S.", next, NULL);
}

