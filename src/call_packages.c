#include "call_packages.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "declare.h"
#include "eval_execute.h"
#include "integer.h"
#include "package.h"
#include "package_designer.h"
#include "package_export.h"
#include "package_make.h"
#include "package_symbol.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  export
 */
_g int export_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return export_package_(pg, symbols);
}


/*
 *  find-package
 */
_g int find_symbol_common_(Execute ptr, addr name, addr pg, addr *ret, addr *state)
{
	enum PACKAGE_TYPE type;

	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}
	Return(find_symbol_package_(pg, name, &name, &type));
	if (name == Nil) {
		*ret = Nil;
		*state = Nil;
	}
	else {
		*ret = name;
		keyword_packagetype(type, state);
	}

	return 0;
}


/*
 *  import
 */
_g int import_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}
	return import_package_(pg, symbols);
}


/*
 *  rename-package
 */
_g int rename_package_common_(Execute ptr, addr pg, addr name, addr names, addr *ret)
{
	if (names == Unbound)
		names = Nil;

	return rename_package_(pg, name, names, ret);
}


/*
 *  shadow
 */
_g int shadow_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return shadow_package_(pg, symbols);
}


/*
 *  shadowing-import
 */
_g int shadowing_import_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return shadowing_import_package_(pg, symbols);
}


/*
 *  make-package
 */
_g int make_package_common_(Execute ptr, addr name, addr rest, addr *ret)
{
	addr nicknames, use;

	/* &key */
	if (GetKeyArgs(rest, KEYWORD_NICKNAMES, &nicknames)) {
		nicknames = Nil;
	}
	if (GetKeyArgs(rest, KEYWORD_USE, &use)) {
		GetConst(PACKAGE_DEFAULT_USE, &use);
	}
	/* make-package */
	return make_package_(ptr, name, nicknames, use, ret);
}


/*
 *  with-package-iterator
 */
static int with_package_iterator_expand_common_(Execute ptr,
		addr name, addr table, int inter, int exter, int inherit,
		addr body, addr *ret)
{
	/* (let ((inst (make-package-iterator table inter exter inherit)))
	 *   (declare (ignorable inst))
	 *   (macrolet ((name () (list (quote next-package-iterator) (quote inst))))
	 *     (declare (ignorable name))
	 *     . body))
	 */
	addr let, declare, ignorable, macrolet, list, quote, make, next;
	addr inst, a, b, c, let1, let2, let3;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_MACROLET, &macrolet);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_MAKE_PACKAGE_ITERATOR, &make);
	GetConst(SYSTEM_NEXT_PACKAGE_ITERATOR, &next);
	Return(make_gensym_(ptr, &inst));

	a = inter? T: Nil;
	b = exter? T: Nil;
	c = inherit? T: Nil;
	list_heap(&let1, make, table, a, b, c, NULL);
	list_heap(&let1, inst, let1, NULL);
	conscar_heap(&let1, let1);
	list_heap(&let2, ignorable, inst, NULL);
	list_heap(&let2, declare, let2, NULL);
	list_heap(&let3, quote, inst, NULL);
	list_heap(&next, quote, next, NULL);
	list_heap(&let3, list, next, let3, NULL);
	list_heap(&let3, name, Nil, let3, NULL);
	conscar_heap(&let3, let3);
	list_heap(&name, ignorable, name, NULL);
	list_heap(&name, declare, name, NULL);
	lista_heap(&let3, macrolet, let3, name, body, NULL);
	list_heap(ret, let, let1, let2, let3, NULL);

	return 0;
}

static int with_package_iterator_check_common(addr rest,
		int *inter, int *exter, int *inherit)
{
	int a, b, c;
	addr list, type, key1, key2, key3;

	a = b = c = 0;
	GetConst(KEYWORD_INTERNAL, &key1);
	GetConst(KEYWORD_EXTERNAL, &key2);
	GetConst(KEYWORD_INHERITED, &key3);
	for (list = rest; list != Nil; ) {
		if (! consp(list)) {
			return call_simple_package_error_va_(NULL,
					"with-package-iterator symbol-types ~S must be "
					":internal, :external, :inherit list.", rest, NULL);
		}
		GetCons(list, &type, &list);
		if (type == key1)
			a = 1;
		else if (type == key2)
			b = 1;
		else if (type == key3)
			c = 1;
		else {
			return call_simple_package_error_va_(NULL,
					"with-package-iterator symbol-type ~S must be a "
					":internal, :external or :inherit value.", type, NULL);
		}
	}
	if (a == 0 && b == 0 && c == 0) {
		return call_simple_package_error_va_(NULL,
				"with-package-iterator symbol-types ~S must be at least "
				":internal, :external :inherit value.", rest, NULL);
	}
	*inter = a;
	*exter = b;
	*inherit = c;

	return 0;
}

_g int with_package_iterator_common(Execute ptr, addr form, addr env, addr *ret)
{
	int inter, exter, inherit;
	addr args, name, list, check;

	/* args */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &name, &args);
	if (! consp(name))
		goto error;
	GetCons(name, &name, &list);
	if (! consp(list))
		goto error;
	GetCons(list, &list, &check);
	inter = exter = inherit = 0;
	Return(with_package_iterator_check_common(check, &inter, &exter, &inherit));
	/* ((name list &rest ...) . args) */
	return with_package_iterator_expand_common_(ptr,
			name, list, inter, exter, inherit, args, ret);

error:
	return fmte_("with-package-iterator form ~S must be "
			"((name package &rest type) &body body)" , form, NULL);
}


/*
 *  unexport
 */
_g int unexport_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return unexport_package_(pg, symbols);
}


/*
 *  unintern
 */
_g int unintern_common_(Execute ptr, addr symbol, addr pg, addr *ret)
{
	int check;

	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}
	Return(unintern_package_(pg, symbol, &check));
	return Result(ret, check? Nil: T);
}


/*
 *  in-package
 */
_g int in_package_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, name, symbol, quote;

	/* argument */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &name, &args);
	if (args != Nil)
		goto error;
	if (! string_designer_p(name))
		goto error;

	/* toplevel */
	if (toplevelp_eval(ptr)) {
		Return(in_package_(ptr, name, NULL));
	}

	/* in-package `(lisp-system::in-package ',name) */
	GetConst(SYSTEM_IN_PACKAGE, &symbol);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&name, quote, name, NULL);
	list_heap(ret, symbol, name, NULL);
	return 0;

error:
	return fmte_("in-package ~S must be (string-designer) form.", form, NULL);
}


/*
 *  unuse-package
 */
_g int unuse_package_common_(Execute ptr, addr unuse, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return unuse_package_(pg, unuse);
}


/*
 *  use-package
 */
_g int use_package_common_(Execute ptr, addr use, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return use_package_(pg, use);
}


/*
 *  do-symbols
 */
static int do_symbols_const_common(addr form, addr *ret, constindex index)
{
	/* `(progn
	 *    (system::do-symbols
	 *      (lambda (,var)
	 *        ,@decl
	 *        (prog () ,@body))
	 *      ,package)
	 *    ,result)
	 */
	addr check, var, package, result, decl, body, lambda, prog, progn;

	Return_getcdr(form, &form);
	if (! consp(form))
		goto error;
	GetCons(form, &check, &body);
	if (! consp(check))
		goto error;
	GetCons(check, &var, &check);
	if (check == Nil) {
		GetConst(SPECIAL_PACKAGE, &package);
		result = Nil;
		goto expand;
	}
	if (! consp(check))
		goto error;
	GetCons(check, &package, &check);
	if (check == Nil) {
		result = Nil;
		goto expand;
	}
	if (! consp(check))
		goto error;
	GetCons(check, &result, &check);
	if (check != Nil)
		goto error;
expand:
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&var, var, NULL);
	GetConst(COMMON_PROG, &prog);
	lista_heap(&prog, prog, Nil, body, NULL);
	list_heap(&prog, prog, NULL);
	Return(nconc2_safe_(decl, prog, &prog));
	GetConst(COMMON_LAMBDA, &lambda);
	lista_heap(&lambda, lambda, var, prog, NULL);
	GetConstant(index, &check);
	list_heap(&check, check, lambda, package, NULL);
	GetConst(COMMON_PROGN, &progn);
	list_heap(ret, progn, check, result, NULL);
	return 0;

error:
	GetConstant(index, &check);
	return fmte_("The ~A ~S must be "
			"((var &optional package result) &rest body) form.", check, form, NULL);
}

_g int do_symbols_common(addr form, addr env, addr *ret)
{
	return do_symbols_const_common(form, ret, CONSTANT_SYSTEM_DO_SYMBOLS);
}


/*
 *  do-external-symbols
 */
_g int do_external_symbols_common(addr form, addr env, addr *ret)
{
	return do_symbols_const_common(form, ret, CONSTANT_SYSTEM_DO_EXTERNAL_SYMBOLS);
}


/*
 *  do-all-symbols
 */
_g int do_all_symbols_common(addr form, addr env, addr *ret)
{
	/* `(progn
	 *    (system::do-all-symbols
	 *      (lambda (,var)
	 *        ,@decl
	 *        (prog () ,@body)))
	 *    ,result)
	 */
	addr check, var, result, decl, body, lambda, prog, progn;

	Return_getcdr(form, &form);
	if (! consp(form))
		goto error;
	GetCons(form, &check, &body);
	if (! consp(check))
		goto error;
	GetCons(check, &var, &check);
	if (check == Nil) {
		result = Nil;
		goto expand;
	}
	if (! consp(check))
		goto error;
	GetCons(check, &result, &check);
	if (check == Nil) {
		goto expand;
	}
	if (check != Nil)
		goto error;
expand:
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&var, var, NULL);
	GetConst(COMMON_PROG, &prog);
	lista_heap(&prog, prog, Nil, body, NULL);
	list_heap(&prog, prog, NULL);
	Return(nconc2_safe_(decl, prog, &prog));
	GetConst(COMMON_LAMBDA, &lambda);
	lista_heap(&lambda, lambda, var, prog, NULL);
	GetConst(SYSTEM_DO_ALL_SYMBOLS, &check);
	list_heap(&check, check, lambda, NULL);
	GetConst(COMMON_PROGN, &progn);
	list_heap(ret, progn, check, result, NULL);
	return 0;

error:
	GetConst(SYSTEM_DO_ALL_SYMBOLS, &check);
	return fmte_("The ~A ~S must be "
			"((var &optional package result) &rest body) form.", check, form, NULL);
}


/*
 *  intern
 */
_g int intern_common_(Execute ptr, addr name, addr pg, addr *ret, addr *sec)
{
	enum PACKAGE_TYPE type;

	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}
	Return(intern_package_(pg, name, &name, &type));
	if (name == Nil) {
		*ret = Nil;
		*sec = Nil;
	}
	else {
		*ret = name;
		keyword_packagetype(type, sec);
	}

	return 0;
}

