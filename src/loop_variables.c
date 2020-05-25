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
	nreverse_list_unsafe(form, root);
}

_g void loop_filter_initially(addr *form, addr *list)
{
	loop_filter_index(form, list, CONSTANT_SYSTEM_LOOP_INITIALLY);
}

_g void loop_filter_finally(addr *form, addr *list)
{
	loop_filter_index(form, list, CONSTANT_SYSTEM_LOOP_FINALLY);
}

static int loop_filter_with_default(Execute ptr, addr list, addr *ret)
{
	addr root, pos, var, type, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		list_bind(pos, &var, &type, &value, NULL);
		if (value == Unbound) {
			if (loop_bind_initial_list(ptr, var, type, &value))
				return 1;
			list_heap(&pos, var, type, value, NULL);
		}
		cons_heap(&root, pos, root);
		localhold_set(hold, 0, root);
	}
	localhold_end(hold);
	nreverse_list_unsafe(ret, root);

	return 0;
}

_g int loop_filter_with(Execute ptr, addr *form, addr *list)
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
			if (loop_filter_with_default(ptr, x, &x))
				return 1;
			cons_heap(list, x, *list);
			localhold_set(hold, 1, *list);
		}
	}
	localhold_end(hold);
	nreverse_list_unsafe(form, root);

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

static void loop_with_single(addr *form, addr list)
{
	addr var, type, value;
	list_bind(list, &var, &type, &value, NULL);
	loop_with_single_bind(form, var, type, value);
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

static void loop_with_let(addr *form, addr list)
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
		list_bind(pos, &var, &type, &value, NULL);
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
		list_bind(pos, &var, &type, &value, NULL);
		if (type != Unbound) {
			list_heap(&var, dtype, var, type, NULL);
			list_heap(&var, declare, var, NULL);
			cons_heap(&root, var, root);
		}
	}
	cons_heap(&root, *form, root);
	nreverse_list_unsafe(form, root);
}

static void loop_with_gensym_form(addr *form, addr list)
{
	addr pos;

	if (list == Nil)
		return;
	GetCons(list, &pos, &list);
	loop_with_gensym_form(form, list);
	loop_with_single(form, pos);
}

static void loop_with_gensym(Execute ptr, addr *form, addr list)
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
		list_bind(pos, &var, &type, &value, NULL);
		Check(value == Unbound, "unbound error");
		make_gensym(ptr, &g);
		/* let args */
		list_heap(&pos, g, value, NULL);
		cons_heap(&letlist, pos, letlist);
		/* gensym list */
		list_heap(&value, g, value, NULL);
		cons_heap(&args, value, args);
		list_heap(&pos, var, type, g, NULL);
		cons_heap(&glist, pos, glist);
	}
	nreverse_list_unsafe(&letlist, letlist);
	nreverse_list_unsafe(&args, args);
	nreverse_list_unsafe(&glist, glist);
	/* expand */
	loop_with_gensym_form(form, glist);
	GetConst(COMMON_LET, &let);
	list_heap(form, let, letlist, *form, NULL);
}

static void loop_with_multiple(Execute ptr, addr *form, addr list)
{
	if (loop_with_all_variable(list))
		loop_with_let(form, list);
	else
		loop_with_gensym(ptr, form, list);
}

_g void loop_variables_with(Execute ptr, addr *form, addr list)
{
	addr pos;

	/* recursive */
	if (list == Nil)
		return;
	GetCons(list, &pos, &list);
	/* next */
	loop_variables_with(ptr, form, list);
	/* bind */
	if (singlep(pos)) {
		/* let */
		GetCar(pos, &pos);
		loop_with_single(form, pos);
	}
	else {
		/* and */
		loop_with_multiple(ptr, form, pos);
	}
}


/*
 *  for-as
 */
static void loop_push_for_as_up(addr *expr1, addr *expr2, addr list)
{
	addr var, a1, a2, b1, b2, by, g1, g2;
	addr pos, unless, less, go, loop, incf;

	list_bind(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_INCF, &incf);
	GetConst(SYSTEM_END_LOOP, &loop);
	if (b1 == Unbound) {
		less = Unbound;
	}
	else if (loop_symbol_below_p(b1)) {
		/* expr1: `(unless (< ,var ,g1) (go end-loop)) */
		GetConst(COMMON_NUMBER_LESS, &less);
	}
	else {
		/* expr1: `(unless (<= ,var ,g1) (go end-loop)) */
		GetConst(COMMON_NUMBER_LESS_EQUAL, &less);
	}
	if (less != Unbound) {
		list_heap(&less, less, var, g1, NULL);
		list_heap(&go, go, loop, NULL);
		list_heap(&pos, unless, less, go, NULL);
		cons_heap(expr1, pos, *expr1);
	}

	/* expr2: `(incf ,var ,g2) */
	list_heap(&pos, incf, var, g2, NULL);
	cons_heap(expr2, pos, *expr2);
}
static void loop_variables_for_as_up(addr *form, addr list)
{
	/* `(let ((,var ,a2)
	 *        (,g1 ,b2)
	 *        (,g2 ,by))
	 *    ,form)
	 */
	addr var, a1, a2, b1, b2, by, g1, g2;
	addr args, let;

	list_bind(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL);
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
	nreverse_list_unsafe(&args, args);
	GetConst(COMMON_LET, &let);
	list_heap(form, let, args, *form, NULL);
}

static void loop_push_for_as_down(addr *expr1, addr *expr2, addr list)
{
	addr var, a1, a2, b1, b2, by, g1, g2;
	addr pos, unless, greater, go, loop, decf;

	list_bind(list, &var, &a1, &a2, &b1, &b2, &by, &g1, &g2, NULL);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_DECF, &decf);
	GetConst(SYSTEM_END_LOOP, &loop);
	if (b1 == Unbound) {
		greater = Unbound;
	}
	else if (loop_symbol_above_p(b1)) {
		/* expr1: `(unless (> ,var ,g1) (go end-loop)) */
		GetConst(COMMON_NUMBER_GREATER, &greater);
	}
	else {
		/* expr1: `(unless (>= ,var ,g1) (go end-loop)) */
		GetConst(COMMON_NUMBER_GREATER_EQUAL, &greater);
	}
	if (greater != Unbound) {
		list_heap(&greater, greater, var, g1, NULL);
		list_heap(&go, go, loop, NULL);
		list_heap(&pos, unless, greater, go, NULL);
		cons_heap(expr1, pos, *expr1);
	}

	/* expr2: `(decf ,var ,g2) */
	list_heap(&pos, decf, var, g2, NULL);
	cons_heap(expr2, pos, *expr2);
}

static void loop_destructuring_bind_tree(Execute ptr,
		addr var, addr *gtree, addr *glist)
{
	addr g, a, b, x, y;

	if (var == Nil) {
		*gtree = Nil;
	}
	else if (! consp(var)) {
		Check(! symbolp(var), "type error");
		make_gensym(ptr, &g);
		*gtree = g;
		cons_heap(glist, var, *glist);
		cons_heap(glist, g, *glist);
	}
	else {
		GetCons(var, &a, &b);
		loop_destructuring_bind_tree(ptr, a, &x, glist);
		loop_destructuring_bind_tree(ptr, b, &y, glist);
		cons_heap(gtree, x, y);
	}
}

static void loop_destructuring_bind(Execute ptr,
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
	loop_destructuring_bind_tree(ptr, var, &gtree, &glist);
	nreverse_list_unsafe(&glist, glist);
	if (type == Unbound)
		GetTypeTable(&type, T);
	quotelist_heap(&var, var);
	quotelist_heap(&type, type);
	list_heap(&lbind, lbind, var, type, value, NULL);
	cons_heap(&setq, setq, glist);
	list_heap(ret, dbind, gtree, lbind, setq, NULL);
}

static void loop_destructuring_setq(Execute ptr,
		addr var, addr type, addr value, addr *ret)
{
	addr setq;

	if (consp(var)) {
		loop_destructuring_bind(ptr, var, type, value, ret);
	}
	else {
		GetConst(COMMON_SETQ, &setq);
		list_heap(ret, setq, var, value, NULL);
	}
}

static void loop_push_for_as_in_list(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr var, type, value, step, g, x;
	addr unless, go, end_loop, setq, funcall, car, cdr;

	list_bind(list, &var, &type, &value, &step, &g, NULL);
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
	loop_destructuring_setq(ptr, var, type, car, &x);
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
	nreverse_list_unsafe(&list, list);
	/* (let ,list ,form) */
	GetConst(COMMON_LET, &let);
	list_heap(form, let, list, *form, NULL);
}

static void loop_variables_for_as_in_list(addr *form, addr list)
{
	addr var, type, value, step, g;

	list_bind(list, &var, &type, &value, &step, &g, NULL);
	loop_with_single_bind(form, g, Unbound, value);
	loop_let_variables(form, var);
}

static void loop_push_for_as_on_list(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr var, type, value, step, g, x;
	addr unless, go, end_loop, setq, funcall, cdr;

	list_bind(list, &var, &type, &value, &step, &g, NULL);
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
		loop_destructuring_bind(ptr, var, type, g, &x);
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
}
static void loop_variables_for_as_on_list(addr *form, addr list)
{
	addr var, type, value, step, g;

	list_bind(list, &var, &type, &value, &step, &g, NULL);
	if (consp(var)) {
		loop_with_single_bind(form, g, Unbound, value);
		loop_let_variables(form, var);
	}
	else {
		loop_with_single_bind(form, var, Unbound, value);
	}
}

static void loop_push_for_as_equals_then(Execute ptr,
		addr *expr1, addr *expr2, addr list)
{
	addr var, type, value, then, g, x;

	list_bind(list, &var, &type, &value, &then, &g, NULL);
	if (then == Unbound)
		then = value;
	loop_destructuring_setq(ptr, var, type, then, &x);
	cons_heap(expr2, x, *expr2);
}
static void loop_variables_for_as_equals_then(addr *form, addr list)
{
	addr var, type, value, step, g;
	list_bind(list, &var, &type, &value, &step, &g, NULL);
	loop_with_single_bind(form, var, type, value);
}

static void loop_push_for_as_across(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr var, type, vector, g1, g2, g3, x;
	addr unless, less, go, end_loop, elt, incf;

	list_bind(list, &var, &type, &vector, &g1, &g2, &g3, NULL);
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
	loop_destructuring_setq(ptr, var, type, elt, &x);
	cons_heap(expr1, x, *expr1);
	/* expr2: `(incf ,g2) */
	GetConst(COMMON_INCF, &incf);
	list_heap(&x, incf, g2, NULL);
	cons_heap(expr2, x, *expr2);
}
static void loop_variables_for_as_across(addr *form, addr list)
{
	addr var, type, vector, g1, g2, g3, let, length, zero;

	list_bind(list, &var, &type, &vector, &g1, &g2, &g3, NULL);
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
}

static void loop_push_for_as_hash(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr var, type, keyp, table, use, g, x, y, z;
	addr key, value, check;
	addr mvbind, next, declare, ignorable, unless, go, end_loop, setq;

	make_gensym(ptr, &check);
	make_gensym(ptr, &key);
	make_gensym(ptr, &value);
	list_bind(list, &var, &type, &keyp, &table, &use, &g, NULL);
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
	if (keyp != Nil) {
		loop_destructuring_setq(ptr, var, type, key, &x);
		if (use != Unbound)
			list_heap(&y, setq, use, value, NULL);
	}
	else {
		loop_destructuring_setq(ptr, var, type, value, &x);
		if (use != Unbound)
			list_heap(&y, setq, use, key, NULL);
	}
	if (use != Unbound)
		list_heap(&x, mvbind, z, next, declare, unless, x, y, NULL);
	else
		list_heap(&x, mvbind, z, next, declare, unless, x, NULL);
	cons_heap(expr1, x, *expr1);
}
static void loop_variables_for_as_hash(addr *form, addr list)
{
	addr var, type, keyp, table, use, g, let, hash;

	list_bind(list, &var, &type, &keyp, &table, &use, &g, NULL);
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
}

static void loop_push_for_as_package(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr var, type, package, g, x, y;
	addr symbol, check;
	addr mvbind, next, unless, go, end_loop;

	make_gensym(ptr, &check);
	make_gensym(ptr, &symbol);
	list_bind(list, &var, &type, &package, &g, NULL);
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
	loop_destructuring_setq(ptr, var, type, symbol, &x);
	list_heap(&x, mvbind, y, next, unless, x, NULL);
	cons_heap(expr1, x, *expr1);
}
static void loop_variables_for_as_package(addr *form, addr pos, addr list)
{
	addr var, type, package, g, let, hash, find, check;
	addr internal, external, inherited;

	list_bind(list, &var, &type, &package, &g, NULL);
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
}

static void loop_push_for_as_list(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr pos, a, b, check;

	if (list == Nil)
		return;
	GetCons(list, &pos, &list);
	/* next */
	loop_push_for_as_list(ptr, expr1, expr2, list);
	/* up */
	GetCons(pos, &a, &b);
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP, &check);
	if (a == check) {
		loop_push_for_as_up(expr1, expr2, b);
		return;
	}
	/* downto */
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO, &check);
	if (a == check) {
		loop_push_for_as_down(expr1, expr2, b);
		return;
	}
	/* downfrom */
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM, &check);
	if (a == check) {
		loop_push_for_as_down(expr1, expr2, b);
		return;
	}
	/* in-list */
	GetConst(SYSTEM_LOOP_FOR_AS_IN_LIST, &check);
	if (a == check) {
		loop_push_for_as_in_list(ptr, expr1, expr2, b);
		return;
	}
	/* on-list */
	GetConst(SYSTEM_LOOP_FOR_AS_ON_LIST, &check);
	if (a == check) {
		loop_push_for_as_on_list(ptr, expr1, expr2, b);
		return;
	}
	/* equals-then */
	GetConst(SYSTEM_LOOP_FOR_AS_EQUALS_THEN, &check);
	if (a == check) {
		loop_push_for_as_equals_then(ptr, expr1, expr2, b);
		return;
	}
	/* across */
	GetConst(SYSTEM_LOOP_FOR_AS_ACROSS, &check);
	if (a == check) {
		loop_push_for_as_across(ptr, expr1, expr2, b);
		return;
	}
	/* hash */
	GetConst(SYSTEM_LOOP_FOR_AS_HASH, &check);
	if (a == check) {
		loop_push_for_as_hash(ptr, expr1, expr2, b);
		return;
	}
	/* package */
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL, &check);
	if (a == check) {
		loop_push_for_as_package(ptr, expr1, expr2, b);
		return;
	}
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT, &check);
	if (a == check) {
		loop_push_for_as_package(ptr, expr1, expr2, b);
		return;
	}
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL, &check);
	if (a == check) {
		loop_push_for_as_package(ptr, expr1, expr2, b);
		return;
	}
	/* error */
	fmte("Invalid variables-clause ~S.", a, NULL);
}

static void loop_variables_for_as_list(addr *form, addr list)
{
	addr pos, a, b, check;

	if (list == Nil)
		return;
	GetCons(list, &pos, &list);
	/* next */
	loop_variables_for_as_list(form, list);
	/* up */
	GetCons(pos, &a, &b);
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_UP, &check);
	if (a == check) {
		loop_variables_for_as_up(form, b);
		return;
	}
	/* downto */
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNTO, &check);
	if (a == check) {
		loop_variables_for_as_up(form, b);
		return;
	}
	/* downfrom */
	GetConst(SYSTEM_LOOP_FOR_AS_ARITHMETIC_DOWNFROM, &check);
	if (a == check) {
		loop_variables_for_as_up(form, b);
		return;
	}
	/* in-list */
	GetConst(SYSTEM_LOOP_FOR_AS_IN_LIST, &check);
	if (a == check) {
		loop_variables_for_as_in_list(form, b);
		return;
	}
	/* on-list */
	GetConst(SYSTEM_LOOP_FOR_AS_ON_LIST, &check);
	if (a == check) {
		loop_variables_for_as_on_list(form, b);
		return;
	}
	/* equals-then */
	GetConst(SYSTEM_LOOP_FOR_AS_EQUALS_THEN, &check);
	if (a == check) {
		loop_variables_for_as_equals_then(form, b);
		return;
	}
	/* across */
	GetConst(SYSTEM_LOOP_FOR_AS_ACROSS, &check);
	if (a == check) {
		loop_variables_for_as_across(form, b);
		return;
	}
	/* hash */
	GetConst(SYSTEM_LOOP_FOR_AS_HASH, &check);
	if (a == check) {
		loop_variables_for_as_hash(form, b);
		return;
	}
	/* package */
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_SYMBOL, &check);
	if (a == check) {
		loop_variables_for_as_package(form, a, b);
		return;
	}
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_PRESENT, &check);
	if (a == check) {
		loop_variables_for_as_package(form, a, b);
		return;
	}
	GetConst(SYSTEM_LOOP_FOR_AS_PACKAGE_EXTERNAL, &check);
	if (a == check) {
		loop_variables_for_as_package(form, a, b);
		return;
	}
	/* error */
	fmte("Invalid variables-clause ~S.", a, NULL);
}

_g void loop_push_for_as(Execute ptr, addr *expr1, addr *expr2, addr list)
{
	addr pos, check, next;

	/* loop */
	if (list == Nil)
		return;
	GetCons(list, &next, &list);
	loop_push_for_as(ptr, expr1, expr2, list);
	/* for-as */
	if (! consp_getcons(next, &pos, &list))
		goto error;
	GetConst(SYSTEM_LOOP_FOR_AS, &check);
	if (pos == check) {
		loop_push_for_as_list(ptr, expr1, expr2, list);
		return;
	}

	/* error */
error:
	fmte("Invalid loop for-as form ~S.", next, NULL);
}

_g void loop_variables_for_as(addr *form, addr list)
{
	addr pos, check, next;

	/* loop */
	if (list == Nil)
		return;
	GetCons(list, &next, &list);
	loop_variables_for_as(form, list);
	/* for-as */
	if (! consp_getcons(next, &pos, &list))
		goto error;
	GetConst(SYSTEM_LOOP_FOR_AS, &check);
	if (pos == check) {
		loop_variables_for_as_list(form, list);
		return;
	}

	/* error */
error:
	fmte("Invalid loop for-as form ~S.", next, NULL);
}

