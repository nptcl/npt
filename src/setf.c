#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "hold.h"
#include "parse_macro.h"
#include "setf.h"
#include "symbol.h"

static int setf_atom(addr pos)
{
	if (keywordp(pos))
		return 1;
	if (symbolp(pos))
		return 0;
	return ! consp(pos);
}


/*
 *  setf-the
 *
 *  (define-setf-expander the (type expr &environment env)
 *    (multiple-value-bind (a b g w r) (get-setf-expansion expr env)
 *      (values a b g
 *        `(multiple-value-bind ,g (the ,type (values ,@g)) ,w)
 *        `(the ,type ,r))))
 */
int function_setf_the(Execute ptr, addr form, addr env)
{
	addr args, type, expr, a, b, g, w, r;
	addr mvbind, the, values;

	Return_getcdr(form, &args);
	a = b = g = w = r = Nil;

	if (! consp_getcons(args, &type, &args))
		goto error;
	if (! consp_getcons(args, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;

	GetConst(COMMON_MULTIPLE_VALUE_BIND, &mvbind);
	GetConst(COMMON_THE, &the);
	GetConst(COMMON_VALUES, &values);
	Return(get_setf_expansion_(ptr, expr, env, &a, &b, &g, &w, &r));
	/* read */
	list_heap(&r, the, type, r, NULL);
	/* write */
	cons_heap(&values, values, g);
	list_heap(&the, the, type, values, NULL);
	list_heap(&w, mvbind, g, the, w, NULL);
	setvalues_control(ptr, a, b, g, w, r, NULL);
	return 0;

error:
	return fmte_("The form ~S must be a (the type expr) form.", form, NULL);
}


/*
 *  setf-values
 *
 *  (define-setf-expander values (&rest form &environment env) ...)
 */
int function_setf_values(Execute ptr, addr form, addr env)
{
	addr args, a, b, g, w, r, v, car, cdr, values;
	addr a1, b1, g1, w1, r1;

	Return_getcdr(form, &args);
	a = b = g = w = r = Nil;
	while (args != Nil) {
		Return_getcons(args, &v, &args);
		Return(get_setf_expansion_(ptr, v, env, &a1, &b1, &g1, &w1, &r1));
		/* vars */
		while (a1 != Nil) {
			Return_getcons(a1, &v, &a1);
			cons_heap(&a, v, a);
		}
		/* vals */
		while (b1 != Nil) {
			Return_getcons(b1, &v, &b1);
			cons_heap(&b, v, b);
		}
		/* store */
		if (g1 != Nil) {
			Return_getcons(g1, &car, &cdr);
			cons_heap(&g, car, g);
			while (cdr != Nil) {
				Return_getcons(cdr, &v, &cdr);
				cons_heap(&a, v, a);
				cons_heap(&b, Nil, b);
			}
		}
		/* writer */
		cons_heap(&w, w1, w);
		/* reader */
		cons_heap(&r, r1, r);
	}

	/* result */
	GetConst(COMMON_VALUES, &values);
	nreverse(&a, a);
	nreverse(&b, b);
	nreverse(&g, g);
	nreverse(&w, w);
	nreverse(&r, r);
	cons_heap(&w, values, w);
	cons_heap(&r, values, r);
	setvalues_control(ptr, a, b, g, w, r, NULL);

	return 0;
}


/*
 *  setf-getf
 *
 *  (define-setf-expander getf (place indicator &optional default) ...)
 *  (get-setf-expansion '(getf x y))
 *    (g2)  ;; (indicator)
 *    (y)
 *    (g1)
 *    (let ((g3 (system::setplist g2 g1 r)))  ;; key, value, plist
 *      (setq x g3)
 *      g1)
 *    (getf r g2 g4)
 */
int function_setf_getf(Execute ptr, addr form, addr env)
{
	addr args, place, indicator, value;
	addr a, b, g, w, r, g1, g2, g3, g4;
	addr let, setplist, getf;

	/* arguments */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &place, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &indicator, &args);
	if (args == Nil)
		value = Nil;
	else {
		if (! consp(args))
			goto error;
		GetCons(args, &value, &args);
		if (args != Nil)
			goto error;
	}

	/* expander */
	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	Return(make_gensym_(ptr, &g1)); /* store */
	Return(make_gensym_(ptr, &g2)); /* indicator */
	Return_getcar(g, &g3);			/* temporary */
	Return(make_gensym_(ptr, &g4)); /* default */
	/* `(,g2 ,g4 ,@a) */
	cons_heap(&a, g4, a);
	cons_heap(&a, g2, a);
	/* `(,indicator ,default ,@b) */
	cons_heap(&b, value, b);
	cons_heap(&b, indicator, b);
	/* `(,g1) */
	conscar_heap(&g, g1);
	/* `(let ((,g3 (system::setplist ,g2 ,g1 ,r))) ,w ,g1) */
	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_SETPLIST, &setplist);
	list_heap(&setplist, setplist, g2, g1, r, NULL);
	list_heap(&setplist, g3, setplist, NULL);
	conscar_heap(&setplist, setplist);
	list_heap(&w, let, setplist, w, g1, NULL);
	/* `(getf ,r ,g2 ,g4) */
	GetConst(COMMON_GETF, &getf);
	list_heap(&r, getf, r, g2, g4, NULL);
	/* result */
	setvalues_control(ptr, a, b, g, w, r, NULL);
	return 0;

error:
	return fmte_("(setf getf) argument ~S must be "
			"(place indicator &optional default) form.", form, NULL);
}


/*
 *  setf-apply
 *
 *  (define-setf-expander apply (call &rest args) ...)
 *  (get-setf-expansion '(apply call x y z))
 *    (g1 g2 g3)
 *    (x y z)
 *    (g)
 *    (apply (function (setf call)) g g1 g2 g3)
 *    (apply (function call) g1 g2 g3)
 */
int function_setf_apply(Execute ptr, addr form, addr env)
{
	addr args, call, pos, list, a, b, g, w, r;
	addr apply, funct, setf;

	GetConst(COMMON_APPLY, &apply);
	GetConst(COMMON_FUNCTION, &funct);
	GetConst(COMMON_SETF, &setf);

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &call, &args))
		goto error;

	/* call check */
	if (! consp_getcons(call, &call, &list))
		goto error_call;
	if (call != funct)
		goto error_call;
	if (! consp_getcons(list, &call, &list))
		goto error_call;
	if (! symbolp(call))
		goto error_call;
	if (list != Nil)
		goto error_call;

	/* gensym */
	Return(make_gensym_(ptr, &g));
	/* a */
	a = Nil;
	list = args;
	while (list != Nil) {
		Return_getcdr(list, &list);
		Return(make_gensym_(ptr, &pos));
		cons_heap(&a, pos, a);
	}
	/* b */
	b = args;
	/* w */
	list_heap(&pos, setf, call, NULL);
	list_heap(&pos, funct, pos, NULL);
	lista_heap(&w, apply, pos, g, a, NULL);
	/* r */
	list_heap(&pos, funct, call, NULL);
	list_heap(&r, apply, pos, a, NULL);
	/* g */
	conscar_heap(&g, g);
	/* result */
	setvalues_control(ptr, a, b, g, w, r, NULL);
	return 0;

error:
	return fmte_("(setf apply) argument ~S "
			"must be (call &rest args) form.", form, NULL);

error_call:
	return fmte_("APPLY argument ~S must be a (FUNCTION symbol) form.", call, NULL);
}


/*
 *  setf-symbol
 *
 *  nil nil (#:g) (setq x #:g) x
 */
static int setf_symbol_(Execute ptr, addr form,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	addr gensym, setq;

	Return(make_gensym_(ptr, &gensym));
	*vars = *vals = Nil;
	conscar_heap(store, gensym);
	GetConst(COMMON_SETQ, &setq);
	list_heap(writer, setq, form, gensym, NULL);
	*reader = form;

	return 0;
}


/*
 *  setf-function
 *    (aaa x y z) ->
 *      (#:x #:y #:z)
 *      (x y z)
 *      (#:g)
 *      (funcall #'(setf aaa) #:g #:x #:y #:z)
 *      (aaa #:x #:y #:z)
 */
static int setf_function_(Execute ptr, addr symbol, addr args,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	addr a, b, c, d, e, funcall, function, setf, var, gen;

	/* (#:g) */
	Return(make_gensym_(ptr, &c));
	conscar_heap(store, c);
	/* (function #'(setf aaa)) */
	GetConst(COMMON_FUNCALL, &funcall);
	GetConst(COMMON_FUNCTION, &function);
	GetConst(COMMON_SETF, &setf);
	conscar_heap(&d, funcall);
	list_heap(&setf, setf, symbol, NULL);
	list_heap(&function, function, setf, NULL);
	cons_heap(&d, function, d);
	cons_heap(&d, c, d);
	/* (aaa) */
	conscar_heap(&e, symbol);
	/* loop */
	for (a = b = Nil; args != Nil; ) {
		Return_getcons(args, &var, &args);
		if (setf_atom(var)) {
			cons_heap(&d, var, d);
			cons_heap(&e, var, e);
		}
		else {
			Return(make_gensym_(ptr, &gen));
			cons_heap(&a, gen, a);
			cons_heap(&b, var, b);
			cons_heap(&d, gen, d);
			cons_heap(&e, gen, e);
		}
	}
	/* result */
	nreverse(vars, a);
	nreverse(vals, b);
	nreverse(writer, d);
	nreverse(reader, e);

	return 0;
}


/*
 *  get-setf-expansion
 */
static void getvalues_nil_control(Execute ptr, size_t index, addr *ret)
{
	getvalues_control(ptr, index, ret);
	if (*ret == Unbound)
		*ret = Nil;
}

static int setf_expander_call_(Execute ptr, LocalHold hold,
		addr call, addr form, addr env,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	Return(funcall_control_(ptr, call, form, env, NULL));
	getvalues_nil_control(ptr, 0, vars);
	getvalues_nil_control(ptr, 1, vals);
	getvalues_nil_control(ptr, 2, store);
	getvalues_nil_control(ptr, 3, writer);
	getvalues_nil_control(ptr, 4, reader);
	localhold_set(hold, 0, *vars);
	localhold_set(hold, 1, *vals);
	localhold_set(hold, 2, *store);
	localhold_set(hold, 3, *writer);
	localhold_set(hold, 4, *reader);

	return 0;
}

static int setf_expander(Execute ptr, addr call, addr form, addr env,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 5);
	push_control(ptr, &control);
	(void)setf_expander_call_(ptr, hold,
			call, form, env, vars, vals, store, writer, reader);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

int get_setf_expansion_(Execute ptr, addr form, addr env,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	int result;
	addr pos, symbol, args, check;

	/* macroexpand */
	Return(macroexpand_(ptr, &pos, form, env, &result));
	if (result)
		form = pos;

	/* symbol */
	if (symbolp(form))
		return setf_symbol_(ptr, form, vars, vals, store, writer, reader);

	/* define-setf-expander */
	if (! consp(form))
		goto error;
	GetCons(form, &symbol, &args);
	if (! symbolp(symbol))
		goto error;
	getsetfmacro_symbol(symbol, &check);
	if (check != Unbound) {
		return setf_expander(ptr, check, form, env, vars, vals, store, writer, reader);
	}

	/* #'(setf form) */
	return setf_function_(ptr, symbol, args, vars, vals, store, writer, reader);

error:
	return fmte_("The form ~S is not setf place.", form, NULL);
}

