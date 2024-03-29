#include "call_iteration.h"
#include "constant.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "declare.h"
#include "symbol.h"

/*
 *  do / do*
 */
static int do_expand_common_(addr *ret, constindex let_const, constindex setq_const,
		addr var, addr end, addr result, addr decl, addr body)
{
	/*  (let ((var1 init1) ;; do* -> let*
	 *        (var2 init2)
	 *        ...)
	 *    (declare ...)
	 *    (block nil
	 *      (tagbody
	 *        #:do-loop
	 *        (if end
	 *          (return-from nil (progn . result)))
	 *        body
	 *        (psetq var1 update1 var2 update2 ...) ;; do* -> setq
	 *        (go #:do-loop)))))
	 */
	addr let, setq, block, tagbody, ifsym, retsym, progn, go, gloop;
	addr root, car, cdr, a, b;

	GetConstant(let_const, &let);
	GetConstant(setq_const, &setq);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_IF, &ifsym);
	GetConst(COMMON_RETURN_FROM, &retsym);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_GO, &go);
	make_symbolchar(&gloop, "DO-LOOP");

	/* (go #:do-loop) */
	list_heap(&go, go, gloop, NULL);
	/* (psetq ...) */
	root = Nil;
	for (cdr = var; cdr != Nil; ) {
		GetCons(cdr, &car, &cdr);
		if (symbolp(car))
			continue;
		GetCons(car, &a, &car);
		if (car == Nil)
			continue;
		GetCdr(car, &car);
		if (car == Nil)
			continue;
		GetCar(car, &b);
		cons_heap(&root, a, root);
		cons_heap(&root, b, root);
	}
	if (root == Nil)
		setq = Nil;
	else {
		nreverse(&root, root);
		cons_heap(&setq, setq, root);
	}
	/* (if ...) */
	cons_heap(&progn, progn, result);
	list_heap(&retsym, retsym, Nil, progn, NULL);
	list_heap(&ifsym, ifsym, end, retsym, NULL);
	/* (tagbody ...) */
	conscar_heap(&root, tagbody);
	cons_heap(&root, gloop, root);
	cons_heap(&root, ifsym, root);
	while (body != Nil) {
		Return_getcons(body, &car, &body);
		cons_heap(&root, car, root);
	}
	if (setq != Nil)
		cons_heap(&root, setq, root);
	cons_heap(&root, go, root);
	nreverse(&root, root);
	/* (block nil ...) */
	list_heap(&block, block, Nil, root, NULL);
	/* (let args ...) */
	root = Nil;
	while (var != Nil) {
		GetCons(var, &car, &var);
		if (symbolp(car)) {
			cons_heap(&root, car, root);
			continue;
		}
		GetCons(car, &a, &cdr);
		if (cdr == Nil) {
			cons_heap(&root, car, root);
			continue;
		}
		GetCons(cdr, &b, &cdr);
		if (cdr == Nil) {
			cons_heap(&root, car, root);
			continue;
		}
		list_heap(&car, a, b, NULL);
		cons_heap(&root, car, root);
	}
	nreverse(&var, root);
	/* (let ...) */
	conscar_heap(&root, let);
	cons_heap(&root, var, root);
	while (decl != Nil) {
		GetCons(decl, &car, &decl);
		cons_heap(&root, car, root);
	}
	cons_heap(&root, block, root);
	nreverse(ret, root);

	return 0;
}

static int do_constant_common_(addr form, addr *ret,
		constindex do_constant,
		constindex let,
		constindex setq)
{
	addr args, name, var, end, result, decl, car, cdr;

	GetConstant(do_constant, &name);
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &var, &args))
		goto error;
	for (cdr = var; cdr != Nil; ) { /* check only */
		if (! consp_getcons(cdr, &car, &cdr))
			return fmte_("~S variable argument ~S must be a list.", name, cdr, NULL);
		if (symbolp(car))
			continue;
		if (! consp_getcdr(car, &car)) /* var */
			goto error_var;
		if (car == Nil)
			continue;
		if (! consp_getcdr(car, &car)) /* init */
			goto error_var;
		if (car == Nil)
			continue;
		if (! consp_getcdr(car, &car))
			goto error_var;
		if (car != Nil)
			goto error_var;
	}
	if (! consp_getcons(args, &end, &args))
		goto error;
	if (! consp_getcons(end, &end, &result))
		goto error;
	Return(declare_body_form_(args, &decl, &args));
	return do_expand_common_(ret, let, setq, var, end, result, decl, args);

error:
	return fmte_("~S arguemnt ~S must be ((var ...) "
			"(test &body result) &body code).", name, form, NULL);
error_var:
	return fmte_("~S variable argument ~S must be "
			"(symbol &optional initial udpate).", name, cdr, NULL);
}

int do_common_(addr form, addr env, addr *ret)
{
	return do_constant_common_(form, ret,
			CONSTANT_COMMON_DO,
			CONSTANT_COMMON_LET,
			CONSTANT_COMMON_PSETQ);
}

int doa_common_(addr form, addr env, addr *ret)
{
	return do_constant_common_(form, ret,
			CONSTANT_COMMON_DOA,
			CONSTANT_COMMON_LETA,
			CONSTANT_COMMON_SETQ);
}


/*
 *  dotimes
 */
static void dotimes_expand_common(addr *ret,
		addr var, addr count, addr result, addr body)
{
	/*  `(do ((,var 0 (1+ ,var)))
	 *    ((<= ,count ,var) ,result)
	 *      ,@body))
	 */
	addr dosym, oneplus, lessequal, form;

	GetConst(COMMON_DO, &dosym);
	GetConst(COMMON_ONE_PLUS, &oneplus);
	GetConst(COMMON_NUMBER_LESS_EQUAL, &lessequal);

	list_heap(&form, lessequal, count, var, NULL);
	list_heap(&form, form, result, NULL);
	list_heap(&oneplus, oneplus, var, NULL);
	list_heap(&var, var, fixnumh(0), oneplus, NULL);
	conscar_heap(&var, var);
	lista_heap(ret, dosym, var, form, body, NULL);
}

int dotimes_common_(addr form, addr env, addr *ret)
{
	addr args, var, count, result, check;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(var, &var, &count))
		goto error;
	if (! consp_getcons(count, &count, &result))
		goto error;
	if (result != Nil) {
		if (! consp_getcons(result, &result, &check))
			goto error;
		if (check != Nil)
			goto error;
	}
	dotimes_expand_common(ret, var, count, result, args);
	return 0;

error:
	return fmte_("DOTIMES arguemnt ~S must be "
			"((var count &optional result) &body body) form.", form, NULL);
}


/*
 *  dolist
 */
static int dolist_expand_common_(Execute ptr, addr *ret,
		addr var, addr list, addr result, addr decl, addr body)
{
	/* `(prog* ((,g ,list)
	 *          (,var (car ,g)))
	 *    ,@declare
	 *    (unless ,g
	 *      (go #:finish))
	 *    #:loop
	 *    ,@body
	 *    (setq ,g (cdr ,g))
	 *    (unless ,g
	 *      (go #:finish))
	 *    (setq ,var (car ,g))
	 *    (go #:loop)
	 *    #:finish
	 *    (return ,result))
	 */
	addr proga, car, cdr, unless, go, setq, retsym;
	addr g, loop, finish, x, setq1, setq2, root;

	GetConst(COMMON_PROGA, &proga);
	GetConst(COMMON_CAR, &car);
	GetConst(COMMON_CDR, &cdr);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_GO, &go);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_RETURN, &retsym);
	Return(make_gensym_(ptr, &g));
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&finish, "FINISH");

	/* result */
	list_heap(&retsym, retsym, result, NULL);
	/* (unless ...) */
	list_heap(&x, go, finish, NULL);
	list_heap(&unless, unless, g, x, NULL);
	/* (go loop) */
	list_heap(&go, go, loop, NULL);
	/* (,var ...) */
	list_heap(&car, car, g, NULL);
	list_heap(&var, var, car, NULL);
	/* (setq ,g ...) */
	list_heap(&cdr, cdr, g, NULL);
	list_heap(&setq1, setq, g, cdr, NULL);
	/* (setq ,var ...) */
	lista_heap(&setq2, setq, var, NULL);
	/* ((,g ...) ...) */
	list_heap(&g, g, list, NULL);
	list_heap(&g, g, var, NULL);
	/* (prog* ...) */
	conscar_heap(&root, proga);
	cons_heap(&root, g, root);
	while (decl != Nil) {
		Return_getcons(decl, &g, &decl);
		cons_heap(&root, g, root);
	}
	cons_heap(&root, unless, root);
	cons_heap(&root, loop, root);
	while (body != Nil) {
		Return_getcons(body, &g, &body);
		cons_heap(&root, g, root);
	}
	cons_heap(&root, setq1, root);
	cons_heap(&root, unless, root);
	cons_heap(&root, setq2, root);
	cons_heap(&root, go, root);
	cons_heap(&root, finish, root);
	cons_heap(&root, retsym, root);
	nreverse(ret, root);

	return 0;
}

int dolist_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, var, list, result, decl, check;

	Return_getcdr(form, &args);
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(var, &var, &list))
		goto error;
	if (! consp_getcons(list, &list, &result))
		goto error;
	if (result != Nil) {
		if (! consp_getcons(result, &result, &check))
			goto error;
		if (check != Nil)
			goto error;
	}
	Return(declare_body_form_(args, &decl, &args));
	return dolist_expand_common_(ptr, ret, var, list, result, decl, args);

error:
	return fmte_("DOLIST arguemnt ~S must be "
			"((var list &optional result) &body body) form.", form, NULL);
}

