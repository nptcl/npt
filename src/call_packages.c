#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "declare.h"
#include "eval_execute.h"
#include "integer.h"
#include "package.h"
#include "package_symbol.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  export
 */
_g void export_common(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound)
		getpackage(ptr, &pg);
	export_package(pg, symbols);
}


/*
 *  find-package
 */
_g void find_symbol_common(Execute ptr, addr name, addr pg, addr *ret, addr *state)
{
	enum PACKAGE_TYPE type;

	if (pg == Unbound)
		getpackage(ptr, &pg);
	type = find_symbol_package(pg, name, &name);
	if (name == Nil) {
		*ret = Nil;
		*state = Nil;
	}
	else {
		*ret = name;
		keyword_packagetype(type, state);
	}
}


/*
 *  import
 */
_g void import_common(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound)
		getpackage(ptr, &pg);
	import_package(pg, symbols);
}


/*
 *  rename-package
 */
_g void rename_package_common(Execute ptr, addr pg, addr name, addr names, addr *ret)
{
	if (names == Unbound)
		names = Nil;
	rename_package(pg, name, names, ret);
}


/*
 *  shadow
 */
_g void shadow_common(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound)
		getpackage(ptr, &pg);
	shadow_package(pg, symbols);
}


/*
 *  shadowing-import
 */
_g void shadowing_import_common(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound)
		getpackage(ptr, &pg);
	shadowing_import_package(pg, symbols);
}


/*
 *  make-package
 */
_g void make_package_common(Execute ptr, addr name, addr rest, addr *ret)
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
	make_package(name, nicknames, use, ret);
}


/*
 *  with-package-iterator
 */
static void with_package_iterator_expand_common(Execute ptr,
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
	make_gensym(ptr, &inst);

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
			return fmte_("with-package-iterator symbol-types ~S must be "
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
			return fmte_("with-package-iterator symbol-type ~S must be a "
					":internal, :external or :inherit value.", type, NULL);
		}
	}
	if (a == 0 && b == 0 && c == 0) {
		return fmte_("with-package-iterator symbol-types ~S must be at least "
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
	with_package_iterator_expand_common(ptr,
			name, list, inter, exter, inherit, args, ret);
	return 0;

error:
	return fmte_("with-package-iterator form ~S must be "
			"((name package &rest type) &body body)" , form, NULL);
}


/*
 *  unexport
 */
_g void unexport_common(Execute ptr, addr symbols, addr pg)
{
	if (pg == Unbound)
		getpackage(ptr, &pg);
	unexport_package(pg, symbols);
}


/*
 *  unintern
 */
_g void unintern_common(Execute ptr, addr symbol, addr pg, addr *ret)
{
	if (pg == Unbound)
		getpackage(ptr, &pg);
	*ret = unintern_package(pg, symbol)? Nil: T;
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
	if (toplevelp_eval(ptr))
		in_package(ptr, name, NULL);

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
_g void unuse_package_common(Execute ptr, addr unuse, addr pg)
{
	if (pg == Unbound)
		getpackage(ptr, &pg);
	unuse_package(pg, unuse);
}


/*
 *  use-package
 */
_g void use_package_common(Execute ptr, addr use, addr pg)
{
	if (pg == Unbound)
		getpackage(ptr, &pg);
	use_package(pg, use);
}


/*
 *  defpackage
 */
static int defpackage_nicknames_common(addr *ret, addr info, addr root)
{
	addr list, pos;

	/* ignore */
	if (root == Nil)
		return Result(ret, info);

	/* push */
	for (list = root; list != Nil; ) {
		if (! consp(list))
			return fmte_(":nickname option ~S don't allow a dotted list.", root, NULL);
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos))
			return fmte_(":nickname ~S must be a string-designer.", pos, NULL);
	}
	cons_heap(ret, root, info);

	return 0;
}

static int defpackage_documentation_common(addr *ret, addr info, addr list)
{
	addr doc, check;

	if (! consp(list))
		return fmte_(":documentation option ~S don't allow a dotted list.", list, NULL);
	GetCons(list, &doc, &check);
	if (! stringp(doc))
		return fmte_(":documentation ~S must be a string.", doc, NULL);
	if (check != Nil)
		return fmte_(":documentation argument ~S must be a single list.", list, NULL);
	if (info != Nil) {
		return fmte_(":documentation option don't accept "
				"multiple defines ~S and ~S.", info, doc, NULL);
	}

	return Result(ret, doc);
}

static int defpackage_use_common(addr *ret, addr info, addr root)
{
	addr list, pos;

	for (list = root; list != Nil; ) {
		if (! consp(list))
			return fmte_(":use option ~S don't allow a dotted list.", root, NULL);
		GetCons(list, &pos, &list);
		if (! package_designer_p(pos))
			return fmte_(":use ~S must be a package-designer.", pos, NULL);
	}
	cons_heap(ret, root, info);

	return 0;
}

static int defpackage_find_common(addr left, addr root)
{
	addr list, right;

	while (root != Nil) {
		GetCons(root, &list, &root);
		while (list != Nil) {
			GetCons(list, &right, &list);
			if (package_designer_equal(left, right))
				return 1;
		}
	}

	return 0;
}

static int defpackage_shadow_common(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr root)
{
	addr list, pos;

	for (list = root; list != Nil; ) {
		if (! consp(list))
			return fmte_(":shadow option ~S don't allow a dotted list.", root, NULL);
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos))
			return fmte_(":shadow ~S must be a string-designer.", pos, NULL);
		if (defpackage_find_common(pos, shadowing)) {
			return fmte_(":shadow ~S "
					"already exists in :shadowing-import-from.", pos, NULL);
		}
		if (defpackage_find_common(pos, import))
			return fmte_(":shadow ~S already exists in :import-from.", pos, NULL);
		if (defpackage_find_common(pos, intern))
			return fmte_(":shadow ~S already exists in :intern.", pos, NULL);
	}
	cons_heap(ret, root, shadow);

	return 0;
}

static int defpackage_shadowing_common(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr root)
{
	addr list, pos, package;

	/* package */
	if (! consp(root)) {
		return fmte_(":shadowing-import-from option ~S "
				"must be a (packgage &rest symbol) form.", root, NULL);
	}
	GetCons(root, &package, &list);
	if (! package_designer_p(package)) {
		return fmte_(":shadowing-import-from first argument ~S "
				"must be a package-designer.", package, NULL);
	}

	/* symbols */
	while (list != Nil) {
		if (! consp(list)) {
			return fmte_(":shadowing-import-from option ~S "
					"don't allow a dotted list.", root, NULL);
		}
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos)) {
			return fmte_(":shadowing-import-from ~S "
					"must be a string-designer.", pos, NULL);
		}
		if (defpackage_find_common(pos, shadow)) {
			return fmte_(":shadowing-import-from ~S "
					"already exists in :shadow.", pos, NULL);
		}
		if (defpackage_find_common(pos, import)) {
			return fmte_(":shadowing-import-from ~S "
					"already exists in :import-from.", pos, NULL);
		}
		if (defpackage_find_common(pos, intern)) {
			return fmte_(":shadowing-import-from ~S "
					"already exists in :intern.", pos, NULL);
		}
	}
	cons_heap(ret, root, shadowing);

	return 0;
}

static int defpackage_import_common(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr root)
{
	addr list, pos, package;

	/* package */
	if (! consp(root)) {
		return fmte_(":import-from option ~S "
				"must be a (packgage &rest symbol) form.", root, NULL);
	}
	GetCons(root, &package, &list);
	if (! package_designer_p(package)) {
		return fmte_(":import-from first argument ~S "
				"must be a package-designer.", package, NULL);
	}

	/* symbols */
	while (list != Nil) {
		if (! consp(list)) {
			return fmte_(":import-from option ~S "
					"don't allow a dotted list.", root, NULL);
		}
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos))
			return fmte_(":import-from ~S must be a string-designer.", pos, NULL);
		if (defpackage_find_common(pos, shadow))
			return fmte_(":import-from ~S already exists in :shadow.", pos, NULL);
		if (defpackage_find_common(pos, shadowing)) {
			return fmte_(":import-from ~S "
					"already exists in :shadowing-import-from.", pos, NULL);
		}
		if (defpackage_find_common(pos, intern))
			return fmte_(":import-from ~S already exists in :intern.", pos, NULL);
	}
	cons_heap(ret, root, import);

	return 0;
}

static int defpackage_export_common(addr *ret, addr expt, addr intern, addr root)
{
	addr list, pos;

	for (list = root; list != Nil; ) {
		if (! consp(list))
			return fmte_(":export option ~S don't allow a dotted list.", root, NULL);
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos))
			return fmte_(":export ~S must be a string-designer.", pos, NULL);
		if (defpackage_find_common(pos, intern))
			return fmte_(":export ~S already exists in :intern.", pos, NULL);
	}
	cons_heap(ret, root, expt);

	return 0;
}

static int defpackage_intern_common(addr *ret, addr expt, addr intern, addr root)
{
	addr list, pos;

	for (list = root; list != Nil; ) {
		if (! consp(list))
			return fmte_(":intern option ~S don't allow a dotted list.", root, NULL);
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos))
			return fmte_(":intern ~S must be a string-designer.", pos, NULL);
		if (defpackage_find_common(pos, expt))
			return fmte_(":intern ~S already exists in :export.", pos, NULL);
	}
	cons_heap(ret, root, intern);

	return 0;
}

static int defpackage_size_common(addr *ret, addr info, addr list)
{
	addr size, check;

	if (! consp(list))
		return fmte_(":size option ~S don't allow a dotted list.", list, NULL);
	GetCons(list, &size, &check);
	if (! integerp(size))
		return fmte_(":size ~S must be a string.", size, NULL);
	if (minusp_integer(size))
		return fmte_(":size ~S must be a positive integer.", size, NULL);
	if (check != Nil)
		return fmte_(":size argument ~S must be a single list.", list, NULL);
	if (info != Nil) {
		return fmte_(":size option don't accept "
				"multiple defines ~S and ~S.", info, size, NULL);
	}

	return Result(ret, size);
}

static int defpackage_expand_common(addr name, addr form, addr *ret)
{
	addr args, key, list;
	addr knick, kdoc, kuse, kshadow, kshadowing, kimport, kexport, kintern, ksize;
	addr nicknames, doc, use, shadow, shadowing, import, expt, intern, size;

	GetConst(KEYWORD_NICKNAMES, &knick);
	GetConst(KEYWORD_DOCUMENTATION, &kdoc);
	GetConst(KEYWORD_USE, &kuse);
	GetConst(KEYWORD_SHADOW, &kshadow);
	GetConst(KEYWORD_SHADOWING_IMPORT_FROM, &kshadowing);
	GetConst(KEYWORD_IMPORT_FROM, &kimport);
	GetConst(KEYWORD_EXPORT, &kexport);
	GetConst(KEYWORD_INTERN, &kintern);
	GetConst(KEYWORD_SIZE, &ksize);

	nicknames = doc = use = shadow = shadowing = import = expt = intern = size = Nil;
	for (args = form; args != Nil; ) {
		if (! consp(args)) {
			return fmte_("The defpackage option ~S "
					"don't allow a dotted list.", form, NULL);
		}
		GetCons(args, &list, &args);
		if (! consp(list))
			return fmte_("The defpackage option ~S must be a cons.", list, NULL);
		GetCons(list, &key, &list);
		if (key == knick) {
			Return(defpackage_nicknames_common(&nicknames, nicknames, list));
		}
		else if (key == kdoc) {
			Return(defpackage_documentation_common(&doc, doc, list));
		}
		else if (key == kuse) {
			Return(defpackage_use_common(&use, use, list));
		}
		else if (key == kshadow) {
			Return(defpackage_shadow_common(&shadow,
						shadow, shadowing, import, intern, list));
		}
		else if (key == kshadowing) {
			Return(defpackage_shadowing_common(&shadowing,
						shadow, shadowing, import, intern, list));
		}
		else if (key == kimport) {
			Return(defpackage_import_common(&shadowing,
						shadow, shadowing, import, intern, list));
		}
		else if (key == kexport) {
			Return(defpackage_export_common(&expt, expt, intern, list));
		}
		else if (key == kintern) {
			Return(defpackage_intern_common(&intern, expt, intern, list));
		}
		else if (key == ksize) {
			Return(defpackage_size_common(&size, size, list));
		}
		else {
			return fmte_("Invalid defpackage option ~S.", key, NULL);
		}
	}

	/* lisp-system::defpackage */
	nreverse(&nicknames, nicknames);
	nreverse(&use, use);
	nreverse(&shadow, shadow);
	nreverse(&shadowing, shadowing);
	nreverse(&import, import);
	nreverse(&intern, intern);
	GetConst(SYSTEM_DEFPACKAGE, &form);
	GetConst(COMMON_QUOTE, &key);
	list_heap(&name, key, name, NULL);
	if (nicknames != Nil)
		list_heap(&nicknames, key, nicknames, NULL);
	if (use != Nil)
		list_heap(&use, key, use, NULL);
	if (shadow != Nil)
		list_heap(&shadow, key, shadow, NULL);
	if (shadowing != Nil)
		list_heap(&shadowing, key, shadowing, NULL);
	if (import != Nil)
		list_heap(&import, key, import, NULL);
	if (expt != Nil)
		list_heap(&expt, key, expt, NULL);
	if (intern != Nil)
		list_heap(&intern, key, intern, NULL);
	list_heap(ret, form, name, size, doc, nicknames, use,
			shadow, shadowing, import, expt, intern, NULL);

	return 0;
}

_g int defpackage_common(addr form, addr env, addr *ret)
{
	addr name;

	Return_getcdr(form, &form);
	if (! consp(form))
		return fmte_("DEFPACKAGE argument ~S must be (name &rest options).", form, NULL);
	GetCons(form, &name, &form);
	if (! string_designer_p(name))
		return fmte_("DEFPACKAGE name ~S must be a string-designer.", name, NULL);
	else
		return defpackage_expand_common(name, form, ret);
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
	declare_body_form(body, &decl, &body);
	list_heap(&var, var, NULL);
	GetConst(COMMON_PROG, &prog);
	lista_heap(&prog, prog, Nil, body, NULL);
	list_heap(&prog, prog, NULL);
	nconc2_safe(decl, prog, &prog);
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
	declare_body_form(body, &decl, &body);
	list_heap(&var, var, NULL);
	GetConst(COMMON_PROG, &prog);
	lista_heap(&prog, prog, Nil, body, NULL);
	list_heap(&prog, prog, NULL);
	nconc2_safe(decl, prog, &prog);
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
_g void intern_common(Execute ptr, addr name, addr pg, addr *ret, addr *sec)
{
	enum PACKAGE_TYPE type;

	if (pg == Unbound)
		getpackage(ptr, &pg);
	type = intern_package(pg, name, &name);
	if (name == Nil) {
		*ret = Nil;
		*sec = Nil;
	}
	else {
		*ret = name;
		keyword_packagetype(type, sec);
	}
}

