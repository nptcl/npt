/*
 *  ANSI COMMON LISP: 11. Packages
 */
#include "common_header.h"
#include "cons.h"
#include "eval.h"
#include "integer.h"
#include "number.h"
#include "package.h"
#include "strtype.h"
#include "type_parse.h"

/* (defun export (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  (or string character symbol package) ;; package-designer
 */
static void function_export(Execute ptr, addr symbols, addr package)
{
	if (package == Unbound)
		getpackage(ptr, &package);
	export_package(package, symbols);
	setresult_control(ptr, T);
}

static void defun_export(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_EXPORT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_export);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find-symbol (string &optional package) ...) -> symbol, status
 *   package  package-designer
 *   status   (member :inherited :external :interal nil)
 */
static void function_find_symbol(Execute ptr, addr name, addr package)
{
	addr second;
	enum PACKAGE_TYPE type;

	if (package == Unbound)
		getpackage(ptr, &package);
	type = find_symbol_package(package, name, &name);
	if (name == Nil) {
		setvalues_control(ptr, Nil, Nil, NULL);
	}
	else {
		keyword_packagetype(type, &second);
		setvalues_control(ptr, name, second, NULL);
	}
}

static void defun_find_symbol(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_SYMBOL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_find_symbol);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intern);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find-package (name) ...) -> package */
static void function_find_package(Execute ptr, addr name)
{
	find_package(name, &name);
	setresult_control(ptr, name);
}

static void type_find_package(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, StringDesigner);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, PackageNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_find_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_PACKAGE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_find_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun find-all-symbols (string) ...) -> symbols
 *   string   string-designer
 *   symbols  list
 */
static void function_find_all_symbols(Execute ptr, addr name)
{
	find_allsymbols_package(name, &name);
	setresult_control(ptr, name);
}

static void type_find_all_symbols(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, StringDesigner);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_find_all_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_ALL_SYMBOLS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_find_all_symbols);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_all_symbols(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun import (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  package-designer
 */
static void function_import(Execute ptr, addr symbols, addr package)
{
	if (package == Unbound)
		getpackage(ptr, &package);
	import_package(package, symbols);
	setresult_control(ptr, T);
}

static void defun_import(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_IMPORT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_import);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun list-all-packages () ...) -> list */
static void function_list_all_packages(Execute ptr)
{
	addr list;
	list_all_packages(&list);
	setresult_control(ptr, list);
}

static void type_list_all_packages(addr *ret)
{
	addr arg, values;

	typeargs_empty(&arg);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_list_all_packages(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LIST_ALL_PACKAGES, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_empty(pos, p_defun_list_all_packages);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_list_all_packages(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun rename-package (package name &optional nicknames) ...) -> value
 *   package    package-designer
 *   name       package-designer
 *   nicknames  list
 *   value      package
 */
static void function_rename_package(Execute ptr,
		addr package, addr name, addr nicknames)
{
	if (nicknames == Unbound)
		nicknames = Nil;
	rename_package(package, name, nicknames, &package);
	setresult_control(ptr, package);
}

static void type_rename_package(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, PackageDesigner);
	GetTypeTable(&values, List);
	typeargs_var2opt1(&arg, arg, arg, values);
	GetTypeValues(&values, Package);
	type_compiled_heap(arg, values, ret);
}

static void defun_rename_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RENAME_PACKAGE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2opt1(pos, p_defun_rename_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_rename_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun shadow (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list string-designer)
 *   package  package-designer
 */
static void function_shadow(Execute ptr, addr symbols, addr package)
{
	if (package == Unbound)
		getpackage(ptr, &package);
	shadow_package(package, symbols);
	setresult_control(ptr, T);
}

static void type_shadow(addr *ret)
{
	addr arg, values, type1, type2;

	GetTypeTable(&type1, List);
	GetTypeTable(&type2, StringDesigner);
	type2or_heap(type1, type2, &arg);
	GetTypeTable(&values, PackageDesigner);
	typeargs_var1opt1(&arg, arg, values);
	GetTypeValues(&values, EqlT);
	type_compiled_heap(arg, values, ret);
}

static void defun_shadow(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SHADOW, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_shadow);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_shadow(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun shadowing-import (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  package-designer
 */
static void function_shadowing_import(Execute ptr, addr symbols, addr package)
{
	if (package == Unbound)
		getpackage(ptr, &package);
	shadowing_import_package(package, symbols);
	setresult_control(ptr, T);
}

static void defun_shadowing_import(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SHADOWING_IMPORT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_shadowing_import);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun delete-package (pacakge) ...) -> booelan
 *   package  package-designer
 */
static void function_delete_package(Execute ptr, addr package)
{
	setbool_control(ptr, ! delete_package(package));
}

static void type_delete_package(addr *ret)
{
	addr arg, values;

	GetTypeArgs(&arg, PackageDesigner);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_delete_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_DELETE_PACKAGE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_delete_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_delete_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-package (name &key nicknames use) ...) -> package
 *   name       string-designer
 *   nicknames  list
 *   use        list
 *   package    package
 */
static void function_make_package(Execute ptr, addr name, addr rest)
{
	addr nicknames, use;

	/* &key */
	if (getkeyargs(rest, KEYWORD_NICKNAMES, &nicknames)) {
		nicknames = Nil;
	}
	if (getkeyargs(rest, KEYWORD_USE, &use)) {
		GetConst(PACKAGE_DEFAULT_USE, &use);
	}
	/* make-package */
	make_package(name, nicknames, use, &name);
	setresult_control(ptr, name);
}

static void type_make_package(addr *ret)
{
	addr arg, values, type1, type2, symbol, type, key;

	/* arg */
	GetTypeTable(&arg, StringDesigner);
	GetTypeTable(&type, List);
	GetConst(KEYWORD_NICKNAMES, &symbol);
	cons_heap(&type1, symbol, type);
	GetConst(KEYWORD_USE, &symbol);
	cons_heap(&type2, symbol, type);
	list_heap(&key, type1, type2, NULL);
	typeargs_var1key(&arg, arg, key);
	/* values */
	GetTypeValues(&values, Package);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_PACKAGE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-package-iterator ((name list &rest types) &body body) ...) */
static void expand_with_package_iterator(Execute ptr,
		addr name, addr table, int inter, int exter, int inherit, addr body)
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
	list_heap(&let, let, let1, let2, let3, NULL);
	setresult_control(ptr, let);
}

static void check_package_iterator(addr rest, int *inter, int *exter, int *inherit)
{
	int a, b, c;
	addr list, type, key1, key2, key3;

	a = b = c = 0;
	GetConst(KEYWORD_INTERNAL, &key1);
	GetConst(KEYWORD_EXTERNAL, &key2);
	GetConst(KEYWORD_INHERITED, &key3);
	for (list = rest; list != Nil; ) {
		if (! consp(list)) {
			fmte("with-package-iterator symbol-types ~S must be "
					":internal, :external, :inherit list.", rest, NULL);
		}
		GetCons(list, &type, &list);
		if (type == key1) a = 1;
		else if (type == key2) b = 1;
		else if (type == key3) c = 1;
		else {
			fmte("with-package-iterator symbol-type ~S must be a "
					":internal, :external or :inherit value.", type, NULL);
		}
	}
	if (a == 0 && b == 0 && c == 0) {
		fmte("with-package-iterator symbol-types ~S must be at least "
				":internal, :external :inherit value.", rest, NULL);
	}
	*inter = a;
	*exter = b;
	*inherit = c;
}

static void function_with_package_iterator(Execute ptr, addr form, addr env)
{
	int inter, exter, inherit;
	addr args, name, list, check;

	/* args */
	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &name, &args);
	if (! consp(name)) goto error;
	GetCons(name, &name, &list);
	if (! consp(list)) goto error;
	GetCons(list, &list, &check);
	check_package_iterator(check, &inter, &exter, &inherit);
	/* ((name list &rest ...) . args) */
	expand_with_package_iterator(ptr, name, list, inter, exter, inherit, args);
	return;

error:
	fmte("with-package-iterator form ~S must be "
			"((name package &rest type) &body body)" , form, NULL);
}

static void defmacro_with_package_iterator(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_PACKAGE_ITERATOR, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_package_iterator);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun unexport (symbols &optional package) ...) -> (eql t)
 *   symbols  (or list symbol)
 *   package  (or string character symbol package) ;; package-designer
 */
static void function_unexport(Execute ptr, addr symbols, addr package)
{
	if (package == Unbound)
		getpackage(ptr, &package);
	unexport_package(package, symbols);
	setresult_control(ptr, T);
}

static void defun_unexport(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNEXPORT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unexport);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Export);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun unintern (symbol &optional package) ...) -> boolean
 *   package  package-designer
 */
static void function_unintern(Execute ptr, addr symbol, addr package)
{
	if (package == Unbound)
		getpackage(ptr, &package);
	setbool_control(ptr, ! unintern_package(package, symbol));
}

static void type_unintern(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Symbol);
	GetTypeTable(&values, PackageDesigner);
	typeargs_var1opt1(&arg, arg, values);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_unintern(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNINTERN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unintern);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_unintern(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro in-package (name) ...) */
static void function_in_package(Execute ptr, addr form, addr env)
{
	addr args, name, symbol, quote, list;

	/* argument */
	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &name, &args);
	if (args != Nil) goto error;
	if (! string_designer_p(name)) goto error;

	/* toplevel */
	if (toplevelp_eval(ptr))
		in_package(ptr, name, NULL);

	/* in-package `(lisp-system::in-package ',name) */
	GetConst(SYSTEM_IN_PACKAGE, &symbol);
	GetConst(COMMON_QUOTE, &quote);
	list_heap(&name, quote, name, NULL);
	list_heap(&list, symbol, name, NULL);
	setresult_control(ptr, list);
	return;

error:
	fmte("in-package ~S must be (string-designer) form.", form, NULL);
}

static void defmacro_in_package(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_IN_PACKAGE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_in_package);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun unuse-package (list &optional package) ...) -> t
 *    list     (or package-designer list)
 *    package  package-designer
 */
static void function_unuse_package(Execute ptr, addr unuse, addr package)
{
	if (package == Unbound)
		getpackage(ptr, &package);
	unuse_package(package, unuse);
	setresult_control(ptr, T);
}

static void defun_unuse_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNUSE_PACKAGE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unuse_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, UsePackage);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun use-package (list &optional package) ...) -> t
 *    list     (or package-designer list)
 *    package  package-designer
 */
static void function_use_package(Execute ptr, addr use, addr package)
{
	if (package == Unbound)
		getpackage(ptr, &package);
	use_package(package, use);
	setresult_control(ptr, T);
}

static void defun_use_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_USE_PACKAGE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_use_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, UsePackage);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro defpackage (name &rest options) ...) -> package
 *   options ::= (:nicknames nickname*)* |
 *               (:documentation string) |
 *               (:use package-name*)* |
 *               (:shadow {symbol-name}*)* |
 *               (:shadowing-import-from package-name {symbol-name}*)* |
 *               (:import-from package-name {symbol-name}*)* |
 *               (:export {symbol-name}*)* |
 *               (:intern {symbol-name}*)* |
 *               (:size integer)
 */
static void defpackage_nicknames(addr *ret, addr info, addr root)
{
	addr list, pos;

	/* ignore */
	if (root == Nil) {
		*ret = info;
		return;
	}

	/* push */
	for (list = root; list != Nil; ) {
		if (! consp(list))
			fmte(":nickname option ~S don't allow a dotted list.", root, NULL);
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos))
			fmte(":nickname ~S must be a string-designer.", pos, NULL);
	}
	cons_heap(ret, root, info);
}

static void defpackage_documentation(addr *ret, addr info, addr list)
{
	addr doc, check;

	if (! consp(list))
		fmte(":documentation option ~S don't allow a dotted list.", list, NULL);
	GetCons(list, &doc, &check);
	if (! stringp(doc))
		fmte(":documentation ~S must be a string.", doc, NULL);
	if (check != Nil)
		fmte(":documentation argument ~S must be a single list.", list, NULL);
	if (info != Nil) {
		fmte(":documentation option don't accept "
				"multiple defines ~S and ~S.", info, doc, NULL);
	}
	*ret = doc;
}

static void defpackage_use(addr *ret, addr info, addr root)
{
	addr list, pos;

	for (list = root; list != Nil; ) {
		if (! consp(list))
			fmte(":use option ~S don't allow a dotted list.", root, NULL);
		GetCons(list, &pos, &list);
		if (! package_designer_p(pos))
			fmte(":use ~S must be a package-designer.", pos, NULL);
	}
	cons_heap(ret, root, info);
}

static int defpackage_find(addr left, addr root)
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

static void defpackage_shadow(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr root)
{
	addr list, pos;

	for (list = root; list != Nil; ) {
		if (! consp(list))
			fmte(":shadow option ~S don't allow a dotted list.", root, NULL);
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos))
			fmte(":shadow ~S must be a string-designer.", pos, NULL);
		if (defpackage_find(pos, shadowing))
			fmte(":shadow ~S already exists in :shadowing-import-from.", pos, NULL);
		if (defpackage_find(pos, import))
			fmte(":shadow ~S already exists in :import-from.", pos, NULL);
		if (defpackage_find(pos, intern))
			fmte(":shadow ~S already exists in :intern.", pos, NULL);
	}
	cons_heap(ret, root, shadow);
}

static void defpackage_shadowing(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr root)
{
	addr list, pos, package;

	/* package */
	if (! consp(root)) {
		fmte(":shadowing-import-from option ~S "
				"must be a (packgage &rest symbol) form.", root, NULL);
	}
	GetCons(root, &package, &list);
	if (! package_designer_p(package)) {
		fmte(":shadowing-import-from first argument ~S "
				"must be a package-designer.", package, NULL);
	}

	/* symbols */
	while (list != Nil) {
		if (! consp(list)) {
			fmte(":shadowing-import-from option ~S "
					"don't allow a dotted list.", root, NULL);
		}
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos))
			fmte(":shadowing-import-from ~S must be a string-designer.", pos, NULL);
		if (defpackage_find(pos, shadow))
			fmte(":shadowing-import-from ~S already exists in :shadow.", pos, NULL);
		if (defpackage_find(pos, import))
			fmte(":shadowing-import-from ~S "
					"already exists in :import-from.", pos, NULL);
		if (defpackage_find(pos, intern))
			fmte(":shadowing-import-from ~S already exists in :intern.", pos, NULL);
	}
	cons_heap(ret, root, shadowing);
}

static void defpackage_import(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr root)
{
	addr list, pos, package;

	/* package */
	if (! consp(root)) {
		fmte(":import-from option ~S "
				"must be a (packgage &rest symbol) form.", root, NULL);
	}
	GetCons(root, &package, &list);
	if (! package_designer_p(package)) {
		fmte(":import-from first argument ~S "
				"must be a package-designer.", package, NULL);
	}

	/* symbols */
	while (list != Nil) {
		if (! consp(list))
			fmte(":import-from option ~S don't allow a dotted list.", root, NULL);
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos))
			fmte(":import-from ~S must be a string-designer.", pos, NULL);
		if (defpackage_find(pos, shadow))
			fmte(":import-from ~S already exists in :shadow.", pos, NULL);
		if (defpackage_find(pos, shadowing))
			fmte(":import-from ~S "
					"already exists in :shadowing-import-from.", pos, NULL);
		if (defpackage_find(pos, intern))
			fmte(":import-from ~S already exists in :intern.", pos, NULL);
	}
	cons_heap(ret, root, import);
}

static void defpackage_export(addr *ret, addr expt, addr intern, addr root)
{
	addr list, pos;

	for (list = root; list != Nil; ) {
		if (! consp(list))
			fmte(":export option ~S don't allow a dotted list.", root, NULL);
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos))
			fmte(":export ~S must be a string-designer.", pos, NULL);
		if (defpackage_find(pos, intern))
			fmte(":export ~S already exists in :intern.", pos, NULL);
	}
	cons_heap(ret, root, expt);
}

static void defpackage_intern(addr *ret, addr expt, addr intern, addr root)
{
	addr list, pos;

	for (list = root; list != Nil; ) {
		if (! consp(list))
			fmte(":intern option ~S don't allow a dotted list.", root, NULL);
		GetCons(list, &pos, &list);
		if (! string_designer_p(pos))
			fmte(":intern ~S must be a string-designer.", pos, NULL);
		if (defpackage_find(pos, expt))
			fmte(":intern ~S already exists in :export.", pos, NULL);
	}
	cons_heap(ret, root, intern);
}

static void defpackage_size(addr *ret, addr info, addr list)
{
	addr size, check;

	if (! consp(list))
		fmte(":size option ~S don't allow a dotted list.", list, NULL);
	GetCons(list, &size, &check);
	if (! integerp(size))
		fmte(":size ~S must be a string.", size, NULL);
	if (minusp_integer(size))
		fmte(":size ~S must be a positive integer.", size, NULL);
	if (check != Nil)
		fmte(":size argument ~S must be a single list.", list, NULL);
	if (info != Nil) {
		fmte(":size option don't accept "
				"multiple defines ~S and ~S.", info, size, NULL);
	}
	*ret = size;
}

static void defpackage_expand(addr name, addr form, addr *ret)
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
		if (! consp(args))
			fmte("The defpackage option ~S don't allow a dotted list.", form, NULL);
		GetCons(args, &list, &args);
		if (! consp(list))
			fmte("The defpackage option ~S must be a cons.", list, NULL);
		GetCons(list, &key, &list);
		if (key == knick)
			defpackage_nicknames(&nicknames, nicknames, list);
		else if (key == kdoc)
			defpackage_documentation(&doc, doc, list);
		else if (key == kuse)
			defpackage_use(&use, use, list);
		else if (key == kshadow)
			defpackage_shadow(&shadow, shadow, shadowing, import, intern, list);
		else if (key == kshadowing)
			defpackage_shadowing(&shadowing, shadow, shadowing, import, intern, list);
		else if (key == kimport)
			defpackage_import(&shadowing, shadow, shadowing, import, intern, list);
		else if (key == kexport)
			defpackage_export(&expt, expt, intern, list);
		else if (key == kintern)
			defpackage_intern(&intern, expt, intern, list);
		else if (key == ksize)
			defpackage_size(&size, size, list);
		else
			fmte("Invalid defpackage option ~S.", key, NULL);
	}

	/* lisp-system::defpackage */
	nreverse_list_unsafe(&nicknames, nicknames);
	nreverse_list_unsafe(&use, use);
	nreverse_list_unsafe(&shadow, shadow);
	nreverse_list_unsafe(&shadowing, shadowing);
	nreverse_list_unsafe(&import, import);
	nreverse_list_unsafe(&intern, intern);
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
}

static void function_defpackage(Execute ptr, addr form, addr env)
{
	addr name;

	getcdr(form, &form);
	if (! consp(form))
		fmte("DEFPACKAGE argument ~S must be (name &rest options).", form, NULL);
	GetCons(form, &name, &form);
	if (! string_designer_p(name))
		fmte("DEFPACKAGE name ~S must be a string-designer.", name, NULL);
	defpackage_expand(name, form, &form);
	setresult_control(ptr, form);
}

static void defmacro_defpackage(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFPACKAGE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_defpackage);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro do-symbols (var &optional package result) . tagbody) */
static void function_do_symbols_constant(Execute ptr, addr form, constindex index)
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

	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &check, &body);
	if (! consp(check)) goto error;
	GetCons(check, &var, &check);
	if (check == Nil) {
		GetConst(SPECIAL_PACKAGE, &package);
		result = Nil;
		goto expand;
	}
	if (! consp(check)) goto error;
	GetCons(check, &package, &check);
	if (check == Nil) {
		result = Nil;
		goto expand;
	}
	if (! consp(check)) goto error;
	GetCons(check, &result, &check);
	if (check != Nil) goto error;
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
	list_heap(&check, progn, check, result, NULL);
	setresult_control(ptr, check);
	return;

error:
	GetConstant(index, &check);
	fmte("The ~A ~S must be "
			"((var &optional package result) &rest body) form.", check, form, NULL);
}

static void function_do_symbols(Execute ptr, addr form, addr env)
{
	function_do_symbols_constant(ptr, form, CONSTANT_SYSTEM_DO_SYMBOLS);
}

static void defmacro_do_symbols(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO_SYMBOLS, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do_symbols);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro do-external-symbols (var &optional package result) . tagbody) */
static void function_do_external_symbols(Execute ptr, addr form, addr env)
{
	function_do_symbols_constant(ptr, form, CONSTANT_SYSTEM_DO_EXTERNAL_SYMBOLS);
}

static void defmacro_do_external_symbols(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO_EXTERNAL_SYMBOLS, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do_external_symbols);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro do-all-symbols (var &optional result) . tagbody) */
static void function_do_all_symbols(Execute ptr, addr form, addr env)
{
	/* `(progn
	 *    (system::do-all-symbols
	 *      (lambda (,var)
	 *        ,@decl
	 *        (prog () ,@body)))
	 *    ,result)
	 */
	addr check, var, result, decl, body, lambda, prog, progn;

	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &check, &body);
	if (! consp(check)) goto error;
	GetCons(check, &var, &check);
	if (check == Nil) {
		result = Nil;
		goto expand;
	}
	if (! consp(check)) goto error;
	GetCons(check, &result, &check);
	if (check == Nil) {
		goto expand;
	}
	if (check != Nil) goto error;
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
	list_heap(&check, progn, check, result, NULL);
	setresult_control(ptr, check);
	return;

error:
	GetConst(SYSTEM_DO_ALL_SYMBOLS, &check);
	fmte("The ~A ~S must be "
			"((var &optional package result) &rest body) form.", check, form, NULL);
}

static void defmacro_do_all_symbols(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DO_ALL_SYMBOLS, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_do_all_symbols);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun intern (string &optional package) ...) -> symbol, status
 *   package  package-designer
 *   status   (member :inherited :external :interal nil)
 */
static void function_intern(Execute ptr, addr name, addr package)
{
	addr second;
	enum PACKAGE_TYPE type;

	if (package == Unbound)
		getpackage(ptr, &package);
	type = intern_package(package, name, &name);
	if (name == Nil) {
		setvalues_control(ptr, Nil, Nil, NULL);
	}
	else {
		keyword_packagetype(type, &second);
		setvalues_control(ptr, name, second, NULL);
	}
}

static void defun_intern(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTERN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_intern);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Intern);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-name (package) ...) -> name
 *   package  package-designer
 *   name     (or string null)
 */
static void function_package_name(Execute ptr, addr package)
{
	getname_package(package, &package);
	setresult_control(ptr, package);
}

static void type_package_name(addr *ret)
{
	addr arg, values;

	GetTypeArgs(&arg, PackageDesigner);
	GetTypeValues(&values, StringNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_package_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_NAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_package_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-nicknames (package) ...) -> list */
static void function_package_nicknames(Execute ptr, addr package)
{
	getnickname_package(package, &package);
	setresult_control(ptr, package);
}

static void defun_package_nicknames(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_NICKNAMES, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_nicknames);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-shadowing-symbols (package) ...) -> list */
static void function_package_shadowing_symbols(Execute ptr, addr package)
{
	getshadow_package(package, &package);
	setresult_control(ptr, package);
}

static void defun_package_shadowing_symbols(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_SHADOWING_SYMBOLS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_shadowing_symbols);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-use-list (package) ...) -> list */
static void function_package_use_list(Execute ptr, addr package)
{
	getuselist_package(package, &package);
	setresult_control(ptr, package);
}

static void defun_package_use_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_USE_LIST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_use_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun package-used-by-list (package) ...) -> list */
static void function_package_used_by_list(Execute ptr, addr package)
{
	getusedbylist_package(package, &package);
	setresult_control(ptr, package);
}

static void defun_package_used_by_list(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_USED_BY_LIST, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_used_by_list);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, PackageNicknames);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun packagep (object) ...) -> boolean */
static void function_packagep(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_PACKAGE);
}

static void defun_packagep(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGEP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_packagep);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar *pacakge*) */
static void defvar_package(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_PACKAGE, &symbol);
	setspecial_symbol(symbol);

	/* type */
	GetTypeTable(&type, Package);
	settype_value_symbol(symbol, type);
}


/* (defun package-error-package (package-error) ...) -> package-designer */
static void function_package_error_package(Execute ptr, addr var)
{
	package_error_package(&var, var);
	setresult_control(ptr, var);
}

static void type_package_error_package(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, PackageError);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, PackageDesigner);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_package_error_package(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PACKAGE_ERROR_PACKAGE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_package_error_package);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_package_error_package(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
void init_common_packages(void)
{
	SetPointerCall(defun, var1opt1, export);
	SetPointerCall(defun, var1opt1, find_symbol);
	SetPointerCall(defun, var1, find_package);
	SetPointerCall(defun, var1, find_all_symbols);
	SetPointerCall(defun, var1opt1, import);
	SetPointerCall(defun, empty, list_all_packages);
	SetPointerCall(defun, var2opt1, rename_package);
	SetPointerCall(defun, var1opt1, shadow);
	SetPointerCall(defun, var1opt1, shadowing_import);
	SetPointerCall(defun, var1, delete_package);
	SetPointerCall(defun, var1dynamic, make_package);
	SetPointerCall(defmacro, macro, with_package_iterator);
	SetPointerCall(defun, var1opt1, unexport);
	SetPointerCall(defun, var1opt1, unintern);
	SetPointerCall(defmacro, macro, in_package);
	SetPointerCall(defun, var1opt1, unuse_package);
	SetPointerCall(defun, var1opt1, use_package);
	SetPointerCall(defmacro, macro, defpackage);
	SetPointerCall(defmacro, macro, do_symbols);
	SetPointerCall(defmacro, macro, do_external_symbols);
	SetPointerCall(defmacro, macro, do_all_symbols);
	SetPointerCall(defun, var1opt1, intern);
	SetPointerCall(defun, var1, package_name);
	SetPointerCall(defun, var1, package_nicknames);
	SetPointerCall(defun, var1, package_shadowing_symbols);
	SetPointerCall(defun, var1, package_use_list);
	SetPointerCall(defun, var1, package_used_by_list);
	SetPointerCall(defun, var1, packagep);
	SetPointerCall(defun, var1, package_error_package);
}

void build_common_packages(void)
{
	defun_export();
	defun_find_symbol();
	defun_find_package();
	defun_find_all_symbols();
	defun_import();
	defun_list_all_packages();
	defun_rename_package();
	defun_shadow();
	defun_shadowing_import();
	defun_delete_package();
	defun_make_package();
	defmacro_with_package_iterator();
	defun_unexport();
	defun_unintern();
	defmacro_in_package();
	defun_unuse_package();
	defun_use_package();
	defmacro_defpackage();
	defmacro_do_symbols();
	defmacro_do_external_symbols();
	defmacro_do_all_symbols();
	defun_intern();
	defun_package_name();
	defun_package_nicknames();
	defun_package_shadowing_symbols();
	defun_package_use_list();
	defun_package_used_by_list();
	defun_packagep();
	defvar_package();
	defun_package_error_package();
}

