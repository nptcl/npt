#include "call_packages.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "declare.h"
#include "eval_value.h"
#include "integer.h"
#include "package.h"
#include "package_designator.h"
#include "package_export.h"
#include "package_import.h"
#include "package_intern.h"
#include "package_make.h"
#include "package_shadow.h"
#include "package_use.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  export
 */
int export_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return export_package_(pg, symbols);
}


/*
 *  find-package
 */
int find_symbol_common_(Execute ptr, addr name, addr pg, addr *ret, addr *state)
{
	enum PACKAGE_TYPE type;

	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}
	Return(find_symbol_package_(pg, name, &name, &type));
	if (type == PACKAGE_TYPE_NIL) {
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
int import_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}
	return import_package_(pg, symbols);
}


/*
 *  rename-package
 */
int rename_package_common_(Execute ptr, addr pg, addr name, addr names, addr *ret)
{
	if (names == Unbound)
		names = Nil;

	return rename_package_(pg, name, names, ret);
}


/*
 *  shadow
 */
int shadow_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return shadow_package_(pg, symbols);
}


/*
 *  shadowing-import
 */
int shadowing_import_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return shadowing_import_package_(pg, symbols);
}


/*
 *  make-package
 */
int make_package_common_(Execute ptr, addr name, addr rest, addr *ret)
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

static int with_package_iterator_check_common_(addr rest,
		int *inter, int *exter, int *inherit)
{
	int a, b, c;
	addr list, type, key1, key2, key3;

	a = b = c = 0;
	GetConst(KEYWORD_INTERNAL, &key1);
	GetConst(KEYWORD_EXTERNAL, &key2);
	GetConst(KEYWORD_INHERITED, &key3);
	for (list = rest; list != Nil; ) {
		if (! consp_getcons(list, &type, &list)) {
			return call_simple_package_error_va_(NULL,
					"with-package-iterator symbol-types ~S must be "
					":internal, :external, :inherit list.", rest, NULL);
		}
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

int with_package_iterator_common_(Execute ptr, addr form, addr env, addr *ret)
{
	int inter, exter, inherit;
	addr args, name, list, check;

	/* args */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (! consp_getcons(name, &name, &list))
		goto error;
	if (! consp_getcons(list, &list, &check))
		goto error;
	inter = exter = inherit = 0;
	Return(with_package_iterator_check_common_(check, &inter, &exter, &inherit));
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
int unexport_common_(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return unexport_package_(pg, symbols);
}


/*
 *  unintern
 */
int unintern_common_(Execute ptr, addr symbol, addr pg, addr *ret)
{
	int check;

	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}
	Return(unintern_package_(pg, symbol, &check));
	return Result(ret, check? T: Nil);
}


/*
 *  in-package
 */
int in_package_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, name, symbol, quote;

	/* argument */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (args != Nil)
		goto error;
	if (! string_designator_p(name))
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
	return fmte_("in-package ~S must be (string-designator) form.", form, NULL);
}


/*
 *  unuse-package
 */
int unuse_package_common_(Execute ptr, addr unuse, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return unuse_package_(pg, unuse);
}


/*
 *  use-package
 */
int use_package_common_(Execute ptr, addr use, addr pg)
{
	if (pg == Unbound) {
		Return(getpackage_(ptr, &pg));
	}

	return use_package_(pg, use);
}


/*
 *  do-symbols
 */
static int do_symbols_const_common_(addr form, addr *ret, constindex index)
{
	/* `(block nil
	 *    (system::do-symbols
	 *      (lambda (,var)
	 *        (declare (ignorable ,var))
	 *        ,@decl
	 *        (tagbody ,@body))
	 *      ,package)
	 *    (let (,var)
	 *      (declare (ignorable ,var)
	 *      ,result)))
	 */
	addr check, var, list, package, result, decl, body;
	addr declare, ignorable, lambda, tagbody, block, let;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &check, &body))
		goto error;
	if (! consp_getcons(check, &var, &check))
		goto error;
	if (check == Nil) {
		GetConst(SPECIAL_PACKAGE, &package);
		result = Nil;
		goto expand;
	}
	if (! consp_getcons(check, &package, &check))
		goto error;
	if (check == Nil) {
		result = Nil;
		goto expand;
	}
	if (! consp_getcons(check, &result, &check))
		goto error;
	if (check != Nil)
		goto error;

expand:
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_LET, &let);
	list_heap(&ignorable, ignorable, var, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	Return(declare_body_form_(body, &decl, &body));
	cons_heap(&tagbody, tagbody, body);
	conscar_heap(&tagbody, tagbody);
	Return(nconc2_safe_(decl, tagbody, &tagbody));
	conscar_heap(&list, var);
	lista_heap(&lambda, lambda, list, declare, tagbody, NULL);
	GetConstant(index, &check);
	list_heap(&check, check, lambda, package, NULL);
	list_heap(&result, let, list, declare, result, NULL);
	list_heap(ret, block, Nil, check, result, NULL);
	return 0;

error:
	GetConstant(index, &check);
	return fmte_("The ~A ~S must be "
			"((var &optional package result) &rest body) form.", check, form, NULL);
}

int do_symbols_common_(addr form, addr env, addr *ret)
{
	return do_symbols_const_common_(form, ret, CONSTANT_SYSTEM_DO_SYMBOLS);
}


/*
 *  do-external-symbols
 */
int do_external_symbols_common_(addr form, addr env, addr *ret)
{
	return do_symbols_const_common_(form, ret, CONSTANT_SYSTEM_DO_EXTERNAL_SYMBOLS);
}


/*
 *  do-all-symbols
 */
int do_all_symbols_common_(addr form, addr env, addr *ret)
{
	/* `(block nil
	 *    (system::do-all-symbols
	 *      (lambda (,var)
	 *        (declare (ignorable ,var))
	 *        ,@decl
	 *        (tagbody ,@body)))
	 *    (let (,var)
	 *      (declare (ignorable ,var)
	 *      ,result)))
	 */
	addr check, var, list, result, decl, body;
	addr declare, ignorable, lambda, tagbody, block, let;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &check, &body))
		goto error;
	if (! consp_getcons(check, &var, &check))
		goto error;
	if (check == Nil) {
		result = Nil;
		goto expand;
	}
	if (! consp_getcons(check, &result, &check))
		goto error;
	if (check == Nil) {
		goto expand;
	}
	if (check != Nil)
		goto error;
expand:
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_BLOCK, &block);
	GetConst(COMMON_LET, &let);
	list_heap(&ignorable, ignorable, var, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	Return(declare_body_form_(body, &decl, &body));
	cons_heap(&tagbody, tagbody, body);
	conscar_heap(&tagbody, tagbody);
	Return(nconc2_safe_(decl, tagbody, &tagbody));
	conscar_heap(&list, var);
	lista_heap(&lambda, lambda, list, declare, tagbody, NULL);
	GetConst(SYSTEM_DO_ALL_SYMBOLS, &check);
	list_heap(&check, check, lambda, NULL);
	list_heap(&result, let, list, declare, result, NULL);
	list_heap(ret, block, Nil, check, result, NULL);
	return 0;

error:
	GetConst(SYSTEM_DO_ALL_SYMBOLS, &check);
	return fmte_("The ~A ~S must be "
			"((var &optional package result) &rest body) form.", check, form, NULL);
}


/*
 *  intern
 */
int intern_common_(Execute ptr, addr name, addr pg, addr *ret, addr *sec)
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

