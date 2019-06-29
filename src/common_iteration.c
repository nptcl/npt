/*
 *  ANSI COMMON LISP: 6. Iteration
 */
#include "common_header.h"
#include "cons.h"
#include "eval_declare.h"
#include "type_parse.h"

/* (defmacro do/do* (var end [declaration] [tag-statement]) ...) -> result */
static void expand_do(addr *ret, constindex let_const, constindex setq_const,
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
		if (symbolp(car)) continue;
		GetCons(car, &a, &car);
		if (car == Nil) continue;
		GetCdr(car, &car);
		if (car == Nil) continue;
		GetCar(car, &b);
		cons_heap(&root, a, root);
		cons_heap(&root, b, root);
	}
	if (root == Nil)
		setq = Nil;
	else {
		nreverse_list_unsafe(&root, root);
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
		getcons(body, &car, &body);
		cons_heap(&root, car, root);
	}
	if (setq != Nil)
		cons_heap(&root, setq, root);
	cons_heap(&root, go, root);
	nreverse_list_unsafe(&root, root);
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
	nreverse_list_unsafe(&var, root);
	/* (let ...) */
	conscar_heap(&root, let);
	cons_heap(&root, var, root);
	while (decl != Nil) {
		GetCons(decl, &car, &decl);
		cons_heap(&root, car, root);
	}
	cons_heap(&root, block, root);
	nreverse_list_unsafe(ret, root);
}

static void function_do_constant(Execute ptr, addr form,
		constindex do_constant,
		constindex let_constant,
		constindex setq_constant)
{
	addr args, name, var, end, result, decl, car, cdr;

	GetConstant(do_constant, &name);
	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &var, &args);
	for (cdr = var; cdr != Nil; ) { /* check only */
		if (! consp(cdr))
			fmte("~S variable argument ~S must be a list.", name, cdr, NULL);
		GetCons(cdr, &car, &cdr);
		if (symbolp(car)) continue;
		if (! consp(car)) goto error_var;
		GetCdr(car, &car); /* var */
		if (car == Nil) continue;
		if (! consp(car)) goto error_var;
		GetCdr(car, &car); /* init */
		if (car == Nil) continue;
		if (! consp(car)) goto error_var;
		GetCdr(car, &car);
		if (car != Nil) goto error_var;
	}
	if (! consp(args)) goto error;
	GetCons(args, &end, &args);
	if (! consp(end)) goto error;
	GetCons(end, &end, &result);
	declare_body_form(args, &decl, &args);
	expand_do(&args, let_constant, setq_constant,
			var, end, result, decl, args);
	setresult_control(ptr, args);
	return;

error:
	fmte("~S arguemnt ~S must be ((var ...) "
			"(test &body result) &body code).", name, form, NULL);
	return;
error_var:
	fmte("~S variable argument ~S must be "
			"(symbol &optional initial udpate).", name, cdr, NULL);
	return;
}

static void function_do(Execute ptr, addr form, addr env)
{
	function_do_constant(ptr, form,
			CONSTANT_COMMON_DO, CONSTANT_COMMON_LET, CONSTANT_COMMON_PSETQ);
}

static void defmacro_do(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}

static void function_doa(Execute ptr, addr form, addr env)
{
	function_do_constant(ptr, form,
			CONSTANT_COMMON_DOA, CONSTANT_COMMON_LETA, CONSTANT_COMMON_SETQ);
}

static void defmacro_doa(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DOA, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_doa);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro dotimes (var count &optional result) [declaration] [body]) -> result */
static void expand_dotimes(addr *ret, addr var, addr count, addr result, addr body)
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

static void function_dotimes(Execute ptr, addr form, addr env)
{
	addr args, var, count, result, check;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &var, &args);
	if (! consp(var)) goto error;
	GetCons(var, &var, &count);
	if (! consp(count)) goto error;
	GetCons(count, &count, &result);
	if (result != Nil) {
		if (! consp(result)) goto error;
		GetCons(result, &result, &check);
		if (check != Nil) goto error;
	}
	expand_dotimes(&args, var, count, result, args);
	setresult_control(ptr, args);
	return;

error:
	fmte("DOTIMES arguemnt ~S must be "
			"((var count &optional result) &body body) form.", form, NULL);
}

static void defmacro_dotimes(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DOTIMES, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_dotimes);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro dolist (var value &optional result) [declaration] [body]) -> result */
static void expand_dolist(Execute ptr, addr *ret,
		addr var, addr list, addr result, addr decl, addr body)
{
	/*  (let ((g (gensym)))
	 *    `(do (,var (,g ,value (cdr ,g)))
	 *      ((null ,g) ,result)
	 *      ,@decl
	 *      (setq ,var (car ,g))
	 *      ,@body)))
	 */
	addr let, dosym, car, cdr, null, setq, g, root;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DO, &dosym);
	GetConst(COMMON_CAR, &car);
	GetConst(COMMON_CDR, &cdr);
	GetConst(COMMON_NULL, &null);
	GetConst(COMMON_SETQ, &setq);
	make_gensym(ptr, &g);
	/* (setq ...) */
	list_heap(&car, car, g, NULL);
	list_heap(&setq, setq, var, car, NULL);
	/* ((null ...) ...) */
	list_heap(&null, null, g, NULL);
	list_heap(&null, null, result, NULL);
	/* (,var ...) */
	list_heap(&cdr, cdr, g, NULL);
	list_heap(&g, g, list, cdr, NULL);
	list_heap(&var, var, g, NULL);
	/* (do ...) */
	conscar_heap(&root, dosym);
	cons_heap(&root, var, root);
	cons_heap(&root, null, root);
	while (decl != Nil) {
		GetCons(decl, &g, &decl);
		cons_heap(&root, g, root);
	}
	cons_heap(&root, setq, root);
	while (body != Nil) {
		GetCons(body, &g, &body);
		cons_heap(&root, g, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void function_dolist(Execute ptr, addr form, addr env)
{
	addr args, var, list, result, decl, check;

	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &var, &args);
	if (! consp(var)) goto error;
	GetCons(var, &var, &list);
	if (! consp(list)) goto error;
	GetCons(list, &list, &result);
	if (result != Nil) {
		if (! consp(result)) goto error;
		GetCons(result, &result, &check);
		if (check != Nil) goto error;
	}
	declare_body_form(args, &decl, &args);
	expand_dolist(ptr, &args, var, list, result, decl, args);
	setresult_control(ptr, args);
	return;

error:
	fmte("DOLIST arguemnt ~S must be "
			"((var list &optional result) &body body) form.", form, NULL);
}

static void defmacro_dolist(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DOLIST, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_dolist);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*
 *  function
 */
_g void init_common_iteration(void)
{
	SetPointerCall(defmacro,  macro,  do);
	SetPointerCall(defmacro,  macro,  doa);
	SetPointerCall(defmacro,  macro,  dotimes);
	SetPointerCall(defmacro,  macro,  dolist);
}

_g void build_common_iteration(void)
{
	defmacro_do();
	defmacro_doa();
	defmacro_dotimes();
	defmacro_dolist();
}

