#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "eval_parse.h"
#include "setf.h"
#include "symbol.h"

static int setf_atom(addr pos)
{
	if (keywordp(pos)) return 1;
	if (symbolp(pos)) return 0;
	return ! consp(pos);
}


/*
 *  setf-values
 *  (define-setf-expander values (&rest form &environment env) ...)
 */
void function_setf_values(Execute ptr, addr form, addr env)
{
	addr args, a, b, g, w, r, v, car, cdr, values;
	addr a1, b1, g1, w1, r1;

	getcdr(form, &args);
	a = b = g = w = r = Nil;
	while (args != Nil) {
		getcons(args, &v, &args);
		if (get_setf_expansion(ptr, v, env, &a1, &b1, &g1, &w1, &r1))
			return;
		/* vars */
		while (a1 != Nil) {
			getcons(a1, &v, &a1);
			cons_heap(&a, v, a);
		}
		/* vals */
		while (b1 != Nil) {
			getcons(b1, &v, &b1);
			cons_heap(&b, v, b);
		}
		/* store */
		if (g1 != Nil) {
			getcons(g1, &car, &cdr);
			cons_heap(&g, car, g);
			while (cdr != Nil) {
				getcons(cdr, &v, &cdr);
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
	nreverse_list_unsafe(&a, a);
	nreverse_list_unsafe(&b, b);
	nreverse_list_unsafe(&g, g);
	nreverse_list_unsafe(&w, w);
	nreverse_list_unsafe(&r, r);
	cons_heap(&w, values, w);
	cons_heap(&r, values, r);
	setvalues_control(ptr, a, b, g, w, r, NULL);
}


/*
 *  setf-getf
 *  (define-setf-expander getf (place indicator &optional default) ...)
 *  (get-setf-expansion '(getf x y))
 *    (g2)  ;; (indicator)
 *    (y)
 *    (g1)
 *    (let ((g3 (system::setplist g2 g1 r)))  ;; key, value, plist
 *      (setq x g3)
 *      g1)
 *    (getf x g2)
 */
void function_setf_getf(Execute ptr, addr form, addr env)
{
	addr args, place, indicator, value;
	addr a, b, g, w, r, g1, g2, g3, g4;
	addr let, setplist, getf;

	/* arguments */
	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &place, &args);
	if (! consp(args)) goto error;
	GetCons(args, &indicator, &args);
	if (args == Nil)
		value = Nil;
	else {
		if (! consp(args)) goto error;
		GetCons(args, &value, &args);
		if (args != Nil) goto error;
		if (setf_atom(value)) value = Nil;
	}

	/* expander */
	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return;
	make_gensym(ptr, &g1);  /* store */
	make_gensym(ptr, &g2);  /* indicator */
	getcar(g, &g3);			/* temporary */
	if (value != Nil) {
		make_symbolchar(&g4, "IG");
		cons_heap(&a, g4, a);
		cons_heap(&b, value, b);
	}
	/* `(,g2 ,@a) */
	cons_heap(&a, g2, a);
	/* `(,indicator ,@b) */
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
	/* `(getf ,place ,g2) */
	GetConst(COMMON_GETF, &getf);
	list_heap(&r, getf, place, g2, NULL);
	/* result */
	setvalues_control(ptr, a, b, g, w, r, NULL);
	return;

error:
	fmte("(setf getf) argument ~S must be "
			"(place indicator &optional default) form.", form, NULL);
}


/*
 *  setf-symbol
 *    nil nil (#:g) (setq x #:g) x
 */
static void setf_symbol(Execute ptr, addr form,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	addr gensym, setq;

	make_gensym(ptr, &gensym);
	*vars = *vals = Nil;
	conscar_heap(store, gensym);
	GetConst(COMMON_SETQ, &setq);
	list_heap(writer, setq, form, gensym, NULL);
	*reader = form;
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
static void setf_function(Execute ptr, addr symbol, addr args,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	addr a, b, c, d, e, funcall, function, setf, var, gen;

	/* (#:g) */
	make_gensym(ptr, &c);
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
		getcons(args, &var, &args);
		if (setf_atom(var)) {
			cons_heap(&d, var, d);
			cons_heap(&e, var, e);
		}
		else {
			make_gensym(ptr, &gen);
			cons_heap(&a, gen, a);
			cons_heap(&b, var, b);
			cons_heap(&d, gen, d);
			cons_heap(&e, gen, e);
		}
	}
	/* result */
	nreverse_list_unsafe(vars, a);
	nreverse_list_unsafe(vals, b);
	nreverse_list_unsafe(writer, d);
	nreverse_list_unsafe(reader, e);
}


/*
 *  get-setf-expansion
 */
static int setf_expander(Execute ptr, addr call, addr form, addr env,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	addr control;

	/* push */
	push_close_control(ptr, &control);
	/* code */
	if (funcall_control(ptr, call, form, env, NULL))
		return runcode_free_control(ptr, control);
	getvalues_nil_control(ptr, 0, vars);
	getvalues_nil_control(ptr, 1, vals);
	getvalues_nil_control(ptr, 2, store);
	getvalues_nil_control(ptr, 3, writer);
	getvalues_nil_control(ptr, 4, reader);
	/* free */
	return free_control(ptr, control);
}

int get_setf_expansion(Execute ptr, addr form, addr env,
		addr *vars, addr *vals, addr *store, addr *writer, addr *reader)
{
	int result;
	addr pos, symbol, args, check;

	/* macroexpand */
	if (macroexpand(&pos, form, env, &result))
		return 1;
	if (result)
		form = pos;

	/* symbol */
	if (symbolp(form)) {
		setf_symbol(ptr, form, vars, vals, store, writer, reader);
		return 0;
	}

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
	setf_function(ptr, symbol, args, vars, vals, store, writer, reader);
	return 0;

error:
	fmte("The form ~S is not setf place.", form, NULL);
	return 1;
}

