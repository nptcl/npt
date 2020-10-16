#include "character.h"
#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "document.h"
#include "execute_object.h"
#include "function.h"
#include "hashtable.h"
#include "integer.h"
#include "package.h"
#include "package_defpackage.h"
#include "package_delete.h"
#include "package_designer.h"
#include "package_export.h"
#include "package_import.h"
#include "package_intern.h"
#include "package_make.h"
#include "package_object.h"
#include "package_shadow.h"
#include "package_use.h"
#include "pointer.h"
#include "strtype.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  lisp-system:defpackage execute
 */
static int defpackage_update_nicknames_(addr pos, addr names)
{
	addr table, list, name;

	PackageTable(&table);

	/* delete nicknames */
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(delete_renameone_package_(table, name));
	}
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, Nil);

	/* append nicknames */
	return append_nicknames_package_(pos, names);
}

static int defpackage_update_shadowing_(addr pos, addr list)
{
	addr child, package, key;

	while (list != Nil) {
		GetCons(list, &child, &list);
		GetCons(child, &package, &child);
		Return(package_designer_(package, &package));
		while (child != Nil) {
			GetCons(child, &key, &child);
			Return(string_designer_heap_(&key, key, NULL));
			Return(intern_package_(package, key, &key, NULL));
			Return(shadowing_import_package_(pos, key));
		}
	}

	return 0;
}

static int defpackage_update_import_(LocalRoot local, addr pos, addr list)
{
	addr child, package, args, symbol;
	LocalStack stack;

	while (list != Nil) {
		GetCons(list, &child, &list);
		GetCons(child, &package, &child);
		Return(package_designer_(package, &package));
		push_local(local, &stack);
		for (args = Nil; child != Nil; ) {
			GetCons(child, &symbol, &child);
			Return(string_designer_heap_(&symbol, symbol, NULL));
			Return(intern_package_(package, symbol, &symbol, NULL));
			cons_local(local, &args, symbol, args);
		}
		nreverse(&args, args);
		Return(import_package_(pos, args));
		rollback_local(local, stack);
	}

	return 0;
}

static int defpackage_update_intern_(addr pos, addr list)
{
	addr name;

	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(intern_package_table_(pos, name, &name, NULL));
	}

	return 0;
}

static int defpackage_export_(addr pos, addr list)
{
	enum PACKAGE_TYPE type;
	addr name, root, symbol;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &name, &list);
		Return(intern_package_(pos, name, &symbol, &type));
		cons_heap(&root, symbol, root);
	}

	return export_package_(pos, root);
}

static int defpackage_update_(Execute ptr, addr pos, addr rest)
{
	addr names, use, shadow, shadowing, import, expt, intern;
	LocalRoot local;

	if (GetKeyArgs(rest, KEYWORD_NICKNAMES, &names))
		names = Nil;
	if (GetKeyArgs(rest, KEYWORD_USE, &use))
		use = Nil;
	if (GetKeyArgs(rest, KEYWORD_SHADOW, &shadow))
		shadow = Nil;
	if (GetKeyArgs(rest, KEYWORD_SHADOWING_IMPORT_FROM, &shadowing))
		shadowing = Nil;
	if (GetKeyArgs(rest, KEYWORD_IMPORT_FROM, &import))
		import = Nil;
	if (GetKeyArgs(rest, KEYWORD_EXPORT, &expt))
		expt = Nil;
	if (GetKeyArgs(rest, KEYWORD_INTERN, &intern))
		intern = Nil;
	local = ptr->local;

	/* nicknames */
	Return(nicknames_make_package_(ptr, names, &names));
	Return(defpackage_update_nicknames_(pos, names));
	/* shadow, shadowing-symbols */
	if (shadow != Nil) {
		Return(shadow_package_(pos, shadow));
	}
	if (shadowing != Nil) {
		Return(defpackage_update_shadowing_(pos, shadowing));
	}
	/* use */
	if (use != Nil) {
		Return(use_package_(pos, use));
	}
	/* import-from */
	if (import != Nil) {
		Return(defpackage_update_import_(local, pos, import));
	}
	/* intern */
	if (intern != Nil) {
		Return(defpackage_update_intern_(pos, intern));
	}
	/* export */
	if (expt != Nil) {
		Return(defpackage_export_(pos, expt));
	}

	return 0;
}

static int defpackage_make(Execute ptr, addr pos, addr rest)
{
	int check;
	addr control, save;

    if (! defpackage_update_(ptr, pos, rest))
		return 0;

	/* escape */
	push_control(ptr, &control);
	save_execute_control(ptr, &save);
	normal_throw_control(ptr);
	if (delete_package_(pos, &check))
		goto escape;
	restore_execute_control(ptr, save);
escape:
	return pop_control_(ptr, control);
}

static int defpackage_resize_package_(addr pos, size_t size)
{
	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	return force_resize_hashtable_(pos, size);
}

_g int defpackage_execute(Execute ptr, addr var, addr rest, addr *ret)
{
	int sizep;
	addr size, doc, pos;
	size_t sizet;

	Check(! stringp(var), "type error");

	/* size */
	if (GetKeyArgs(rest, KEYWORD_SIZE, &size))
		size = Nil;
	sizep = (size != Nil);
	if (sizep) {
		if (GetIndex_integer(size, &sizet))
			return fmte_(":size ~S is too large.", size, NULL);
	}

	/* package */
	Return(find_package_(var, &pos));
	if (pos == Nil) {
		if (sizep) {
			Return(package_size_heap_(&pos, var, sizet));
		}
		else {
			Return(package_heap_(&pos, var));
		}
		Return(defpackage_make(ptr, pos, rest));
	}
	else {
		if (sizep) {
			Return(defpackage_resize_package_(pos, sizet));
		}
		Return(defpackage_update_(ptr, pos, rest));
	}

	/* documentation */
	if (GetKeyArgs(rest, KEYWORD_DOCUMENTATION, &doc))
		doc = Nil;
	setdocument_package(pos, doc);

	/* result */
	return Result(ret, pos);
}


/*
 *  common-lisp:defpackage macro
 */
static int package_designer_string_(addr *value, addr pos, int *ret)
{
	if (packagep(pos)) {
		Return(getname_package_(pos, &pos));
	}
	return string_designer_heap_(value, pos, ret);
}

static int defpackage_nicknames_common(addr *ret, addr info, addr list)
{
	int check;
	addr pos, type;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(string_designer_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesigner);
			return call_type_error_va_(NULL, pos, type,
					":nickname ~S must be a string-designer.", pos, NULL);
		}
		cons_heap(&info, pos, info);
	}

	return Result(ret, info);
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
		return call_simple_program_error_va_(NULL,
				":documentation option don't accept "
				"multiple defines ~S and ~S.", info, doc, NULL);
	}

	return Result(ret, doc);
}

static int defpackage_use_common(addr *ret, addr info, addr list)
{
	int check;
	addr pos, type;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(package_designer_string_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, PackageDesigner);
			return call_type_error_va_(NULL, pos, type,
					":use ~S must be a package-designer.", pos, NULL);
		}
		cons_heap(&info, pos, info);
	}

	return Result(ret, info);
}

static int defpackage_find_list_(addr x, addr list, int *ret)
{
	int check;
	addr y;

	while (list != Nil) {
		GetCons(list, &y, &list);
		Return(string_equal_(x, y, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int defpackage_find_import_(addr x, addr list, int *ret)
{
	int check;
	addr row, y;

	while (list != Nil) {
		GetCons(list, &row, &list);
		GetCdr(row, &row); /* package-name */
		while (row != Nil) {
			GetCons(row, &y, &row);
			Return(string_equal_(x, y, &check));
			if (check)
				return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int defpackage_shadow_common(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr list)
{
	int check;
	addr pos, type;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designer_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesigner);
			return call_type_error_va_(NULL, pos, type,
					":shadow ~S must be a string-designer.", pos, NULL);
		}
		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":shadow ~S already exists in :shadowing-import-from.",
					pos, NULL);
		}
		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":shadow ~S already exists in :import-from.", pos, NULL);
		}
		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":shadow ~S already exists in :intern.", pos, NULL);
		}
		/* push */
		cons_heap(&shadow, pos, shadow);
	}

	return Result(ret, shadow);
}

static int defpackage_shadowing_common(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr list)
{
	int check;
	addr pos, type, row;

	/* package name */
	Return_getcons(list, &pos, &list);
	Return(package_designer_string_(&pos, pos, &check));
	if (! check) {
		GetTypeTable(&type, PackageDesigner);
		return call_type_error_va_(NULL, pos, type,
				":shadowing-import-from first argument ~S "
				"must be a package-designer.", pos, NULL);
	}
	conscar_heap(&row, pos);

	/* symbols */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designer_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesigner);
			return call_type_error_va_(NULL, pos, type,
					":shadowing-import-from ~S "
					"must be a string-designer.", pos, NULL);
		}
		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":shadowing-import-from ~S "
					"already exists in :shadow.", pos, NULL);
		}
		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":shadowing-import-from ~S "
					"already exists in :import-from.", pos, NULL);
		}
		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":shadowing-import-from ~S "
					"already exists in :intern.", pos, NULL);
		}
		/* push */
		cons_heap(&row, pos, row);
	}

	nreverse(&row, row);
	cons_heap(ret, row, shadowing);
	return 0;
}

static int defpackage_import_common(addr *ret,
		addr shadow, addr shadowing, addr import, addr intern, addr list)
{
	int check;
	addr pos, type, row;

	/* package name */
	Return_getcons(list, &pos, &list);
	Return(package_designer_string_(&pos, pos, &check));
	if (! check) {
		GetTypeTable(&type, PackageDesigner);
		return call_type_error_va_(NULL, pos, type,
				":import-from first argument ~S "
				"must be a package-designer.", pos, NULL);
	}
	conscar_heap(&row, pos);

	/* symbols */
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designer_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesigner);
			return call_type_error_va_(NULL, pos, type,
					":import-from ~S must be a string-designer.", pos, NULL);
		}
		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":import-from ~S already exists in :shadow.", pos, NULL);
		}
		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":import-from ~S already exists in :shadowing-import-from.",
					pos, NULL);
		}
		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":import-from ~S already exists in :intern.", pos, NULL);
		}
		/* push */
		cons_heap(&row, pos, row);
	}

	nreverse(&row, row);
	cons_heap(ret, row, import);
	return 0;
}

static int defpackage_export_common(addr *ret, addr expt, addr intern, addr list)
{
	int check;
	addr pos, type;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designer_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesigner);
			return call_type_error_va_(NULL, pos, type,
					":export ~S must be a string-designer.", pos, NULL);
		}
		/* intern check */
		Return(defpackage_find_list_(pos, intern, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":export ~S already exists in :intern.", pos, NULL);
		}
		/* push */
		cons_heap(&expt, pos, expt);
	}

	return Result(ret, expt);
}

static int defpackage_intern_common(addr *ret,
		addr shadow, addr shadowing, addr import, addr expt, addr intern, addr list)
{
	int check;
	addr pos, type;

	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		/* type check */
		Return(string_designer_heap_(&pos, pos, &check));
		if (! check) {
			GetTypeTable(&type, StringDesigner);
			return call_type_error_va_(NULL, pos, type,
					":intern ~S must be a string-designer.", pos, NULL);
		}
		/* shadow check */
		Return(defpackage_find_list_(pos, shadow, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":intern ~S already exists in :shadow.", pos, NULL);
		}
		/* shadowing-import-from check */
		Return(defpackage_find_import_(pos, shadowing, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":intern ~S already exists in :shadowing-import-from.",
					pos, NULL);
		}
		/* import-from check */
		Return(defpackage_find_import_(pos, import, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":intern~S already exists in :import-from.", pos, NULL);
		}
		/* export check */
		Return(defpackage_find_list_(pos, expt, &check));
		if (check) {
			return call_simple_program_error_va_(NULL,
					":intern ~S already exists in :export.", pos, NULL);
		}
		/* push */
		cons_heap(&intern, pos, intern);
	}

	return Result(ret, intern);
}

static int defpackage_size_common(addr *ret, addr info, addr list)
{
	int check;
	addr size, value;

	if (! consp(list))
		return fmte_(":size option ~S don't allow a dotted list.", list, NULL);
	GetCons(list, &size, &value);
	if (! integerp(size))
		return fmte_(":size ~S must be a string.", size, NULL);
	Return(minusp_integer_(size, &check));
	if (check)
		return fmte_(":size ~S must be a positive integer.", size, NULL);
	if (value != Nil)
		return fmte_(":size argument ~S must be a single list.", list, NULL);
	if (info != Nil) {
		return call_simple_program_error_va_(NULL,
				":size option don't accept "
				"multiple defines ~S and ~S.", info, size, NULL);
	}

	return Result(ret, size);
}

static int defpackage_expand_common(addr name, addr form, addr *ret)
{
	addr args, key, list, quote;
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
		if (! consp(list)) {
			return call_simple_program_error_va_(NULL,
					"The defpackage option ~S must be a cons.", list, NULL);
		}
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
			Return(defpackage_intern_common(&intern,
						shadow, shadowing, import, expt, intern, list));
		}
		else if (key == ksize) {
			Return(defpackage_size_common(&size, size, list));
		}
		else {
			return call_simple_program_error_va_(NULL,
					"Invalid defpackage option ~S.", key, NULL);
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
	GetConst(COMMON_QUOTE, &quote);
	/* (lisp-system::defpackage name ...) */
	list = Nil;
	cons_heap(&list, form, list);
	cons_heap(&list, name, list);
	/* :size */
	if (size != Nil) {
		cons_heap(&list, ksize, list);
		cons_heap(&list, size, list);
	}
	/* :documentation */
	if (doc != Nil) {
		cons_heap(&list, kdoc, list);
		cons_heap(&list, doc, list);
	}
	/* :nicknames */
	if (nicknames != Nil) {
		cons_heap(&list, knick, list);
		list_heap(&nicknames, quote, nicknames, NULL);
		cons_heap(&list, nicknames, list);
	}
	/* :use */
	if (use != Nil) {
		cons_heap(&list, kuse, list);
		list_heap(&use, quote, use, NULL);
		cons_heap(&list, use, list);
	}
	/* :shadow */
	if (shadow != Nil) {
		cons_heap(&list, kshadow, list);
		list_heap(&shadow, quote, shadow, NULL);
		cons_heap(&list, shadow, list);
	}
	/* :shadowing-import-from */
	if (shadowing != Nil) {
		cons_heap(&list, kshadowing, list);
		list_heap(&shadowing, quote, shadowing, NULL);
		cons_heap(&list, shadowing, list);
	}
	/* :import-from */
	if (import != Nil) {
		cons_heap(&list, kimport, list);
		list_heap(&import, quote, import, NULL);
		cons_heap(&list, import, list);
	}
	/* :export */
	if (expt != Nil) {
		cons_heap(&list, kexport, list);
		list_heap(&expt, quote, expt, NULL);
		cons_heap(&list, expt, list);
	}
	/* :intern */
	if (intern != Nil) {
		cons_heap(&list, kintern, list);
		list_heap(&intern, quote, intern, NULL);
		cons_heap(&list, intern, list);
	}
	/* result */
	nreverse(ret, list);
	return 0;
}

_g int defpackage_common(addr form, addr env, addr *ret)
{
	int check;
	addr name;

	Return_getcdr(form, &form);
	if (! consp(form))
		return fmte_("DEFPACKAGE argument ~S must be (name &rest options).", form, NULL);
	GetCons(form, &name, &form);
	Return(string_designer_heap_(&name, name, &check));
	if (! check)
		return fmte_("DEFPACKAGE name ~S must be a string-designer.", name, NULL);
	else
		return defpackage_expand_common(name, form, ret);
}

