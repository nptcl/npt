#include "bignum_output.h"
#include "character.h"
#include "character_queue.h"
#include "condition.h"
#include "constant.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "function.h"
#include "hashtable.h"
#include "integer.h"
#include "package.h"
#include "package_bittype.h"
#include "package_object.h"
#include "package_symbol.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  gentemp
 */
_g int make_gentemp_(Execute ptr, addr prefix, addr package, addr *ret)
{
	enum PACKAGE_TYPE type;
	int keyword;
	addr value, queue, name, gentemp;
	LocalRoot local;
	LocalStack stack;

	/* package check */
	if (package == NULL)
		getpackage(ptr, &package);
	else
		package_designer(package, &package);
	GetConst(PACKAGE_KEYWORD, &value);
	keyword = (value == package);

	/* symbol-name */
	GetConst(PACKAGE_GENTEMP, &value);
	Check(! integerp(value), "type error");

	local = ptr->local;
	for (;;) {
		/* make symbol-name */
		push_local(local, &stack);
		charqueue_local(local, &queue, 1 + 16);
		if (prefix == NULL)
			pushchar_charqueue_local(local, queue, "T");
		else
			pushstring_charqueue_local(local, queue, prefix);
		Return(decimal_charqueue_integer_local_(local, value, queue));
		make_charqueue_local(local, queue, &name);
		type = find_symbol_package(package, name, &gentemp);
		if (type == PACKAGE_TYPE_NIL)
			make_charqueue_heap(queue, &name);
		rollback_local(local, stack);

		/* (1+ *gentemp-counter*) */
		oneplus_integer_common(local, value, &value);
		SetConst(PACKAGE_GENTEMP, value);

		/* check intern */
		if (type == PACKAGE_TYPE_NIL)
			break;
	}

	/* gentemp */
	intern_package(package, name, &gentemp);
	if (keyword)
		export_package(package, gentemp);
	
	return Result(ret, gentemp);
}


/*
 *  defpackage
 */
static void defpackage_findcons(addr table, addr key, addr *ret)
{
	if (stringp(key)) {
		findcons_hashtable(table, key, ret);
		return;
	}
	if (symbolp(key)) {
		GetNameSymbol(key, &key);
		findcons_hashtable(table, key, ret);
		return;
	}
	if (characterp(key)) {
		findcons_unicode_hashtable(table, RefCharacter(key), ret);
		return;
	}
	*ret = Nil;
}

static void defpackage_make_nicknames(LocalRoot local, addr *ret, addr list)
{
	addr root, child, pos;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &child, &list);
		while (child != Nil) {
			GetCons(child, &pos, &child);
			cons_local(local, &root, pos, root);
		}
	}
	nreverse(ret, root);
}

static void defpackage_check_nicknames(addr pos, addr names)
{
	addr table, list, name, check;

	PackageTable(&table);
	for (list = names; list != Nil; ) {
		GetCons(list, &name, &list);
		defpackage_findcons(table, name, &check);
		if (check != Nil) {
			GetCdr(check, &check);
			if (pos != check)
				fmte("nickname ~A is already exists.", name, NULL);
		}
	}
}

static void defpackage_update_nicknames(addr pos, addr names)
{
	addr table, list, name;

	PackageTable(&table);

	/* delete nicknames */
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, &list);
	while (list != Nil) {
		GetCons(list, &name, &list);
		delete_renameone_package(table, name);
	}
	SetPackage(pos, PACKAGE_INDEX_NICKNAME, Nil);

	/* append nicknames */
	append_nicknames_package(pos, names);
}

static void defpackage_update_shadow(LocalRoot local, addr pos, addr list)
{
	defpackage_make_nicknames(local, &list, list);
	shadow_list_package(pos, list);
}

static void defpackage_update_shadowing(addr pos, addr list)
{
	addr child, package, key;

	while (list != Nil) {
		GetCons(list, &child, &list);
		GetCons(child, &package, &child);
		package_designer(package, &package);
		while (child != Nil) {
			GetCons(child, &key, &child);
			string_designer_heap(&key, key);
			intern_package(package, key, &key);
			shadowing_import_symbol_package(pos, key);
		}
	}
}

static void defpackage_update_use(LocalRoot local, addr pos, addr list)
{
	defpackage_make_nicknames(local, &list, list);
	use_package_list_package(pos, list);
}

static void defpackage_update_import(LocalRoot local, addr pos, addr list)
{
	addr child, package, args, symbol;
	LocalStack stack;

	while (list != Nil) {
		GetCons(list, &child, &list);
		GetCons(child, &package, &child);
		package_designer(package, &package);
		push_local(local, &stack);
		for (args = Nil; child != Nil; ) {
			GetCons(child, &symbol, &child);
			string_designer_heap(&symbol, symbol);
			intern_package(package, symbol, &symbol);
			cons_local(local, &args, symbol, args);
		}
		nreverse(&args, args);
		import_package(pos, args);
		rollback_local(local, stack);
	}
}

static void defpackage_update_intern(addr pos, addr list)
{
	addr child, name;

	while (list != Nil) {
		GetCons(list, &child, &list);
		while (child != Nil) {
			GetCons(child, &name, &child);
			string_designer_heap(&name, name);
			intern_package_table(pos, name, &name);
		}
	}
}

static void defpackage_update_export(LocalRoot local, addr pos, addr list)
{
	addr root, child, name;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &child, &list);
		while (child != Nil) {
			GetCons(child, &name, &child);
			string_designer_heap(&name, name);
			intern_package_table(pos, name, &name);
			cons_local(local, &root, name, root);
		}
	}
	nreverse(&root, root);
	export_list_package(pos, root);
}

static void defpackage_update(LocalRoot local, addr pos, addr rest)
{
	addr nicknames, use, shadow, shadowing, import, expt, intern;

	List_bind(rest, &nicknames, &use, &shadow, &shadowing,
			&import, &expt, &intern, NULL);
	/* nicknames */
	defpackage_make_nicknames(local, &nicknames, nicknames);
	defpackage_check_nicknames(pos, nicknames);
	defpackage_update_nicknames(pos, nicknames);
	/* shadow, shadowing-symbols */
	defpackage_update_shadow(local, pos, shadow);
	defpackage_update_shadowing(pos, shadowing);
	/* use */
	defpackage_update_use(local, pos, use);
	/* import-from, intern */
	defpackage_update_import(local, pos, import);
	defpackage_update_intern(pos, intern);
	/* export */
	defpackage_update_export(local, pos, expt);
}

static int function_defpackage_make(Execute ptr, addr condition)
{
	addr pos;

	/* delete */
	getdata_control(ptr, &pos);
	delete_package(pos);
	/* throw */
	error_function(condition);

	return 0;
}

static int defpackage_make(Execute ptr, addr pos, addr rest)
{
	addr control, symbol, call;

	/* push */
	push_new_control(ptr, &control);
	/* handler-case */
	GetConst(COMMON_ERROR, &symbol);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_defpackage_make);
	SetDataFunction(call, pos);
	pushhandler_common(ptr, symbol, call, 0);
	/* code */
	defpackage_update(ptr->local, pos, rest);
	/* free */
	return free_control_(ptr, control);
}

static void resize_pacakge(addr pos, size_t size)
{
	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	force_resize_hashtable(pos, size);
}

_g int defpackage_execute(Execute ptr, addr rest, addr *ret)
{
	int sizep;
	addr name, pos, size, doc;
	size_t value;

	GetCons(rest, &name, &rest);
	GetCons(rest, &size, &rest);

	/* size */
	sizep = (size != Nil);
	if (sizep) {
		if (GetIndex_integer(size, &value))
			fmte(":size ~S is too large.", size, NULL);
	}

	/* package */
	GetCons(rest, &doc, &rest);
	find_package(name, &pos);
	if (pos == Nil) {
		if (sizep)
			package_size_heap(&pos, name, value);
		else
			package_heap(&pos, name);
		Return(defpackage_make(ptr, pos, rest));
	}
	else {
		if (sizep)
			resize_pacakge(pos, value);
		defpackage_update(ptr->local, pos, rest);
	}

	/* documentation */
	setdocument_package(pos, doc);

	/* result */
	*ret = pos;

	return 0;
}


/*
 *  iterator
 */
static int syscall_do_symbols_check(Execute ptr, addr call, addr package)
{
	addr table, list, bit;
	size_t size, i;

	package_designer(package, &package);
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetTableHash(table, &table);
	LenArrayHash(table, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &bit, &list);
			GetCdr(bit, &bit);
			GetBitTypeSymbol(bit, &bit);
			if (callclang_funcall(ptr, &bit, call, bit, NULL))
				return 1;
		}
	}

	return 0;
}

_g int do_symbols_package(Execute ptr, addr call, addr package)
{
	return syscall_do_symbols_check(ptr, call, package);
}

_g int do_external_symbols_package(Execute ptr, addr call, addr package)
{
	addr table, list, bit;
	size_t size, i;

	package_designer(package, &package);
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetTableHash(table, &table);
	LenArrayHash(table, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &bit, &list);
			GetCdr(bit, &bit);
			if (StructBitType(bit)->intern == PACKAGE_TYPE_EXTERNAL) {
				GetBitTypeSymbol(bit, &bit);
				if (callclang_funcall(ptr, &bit, call, bit, NULL))
					return 1;
			}
		}
	}

	return 0;
}

_g int do_all_symbols_package(Execute ptr, addr call)
{
	addr array, left, right, key, check;
	size_t i, size;

	PackageTable(&array);
	GetTableHash(array, &array);
	LenArrayHash(array, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(array, i, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCons(left, &key, &left);
			GetPackage(left, PACKAGE_INDEX_NAME, &check);
			if (string_equal(key, check)) {
				if (syscall_do_symbols_check(ptr, call, left))
					return 1;
			}
		}
	}

	return 0;
}

_g void all_symbols_package(addr package, addr *ret)
{
	addr table, list, bit, root;
	size_t size, i;

	package_designer(package, &package);
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetTableHash(table, &table);
	LenArrayHash(table, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &bit, &list);
			GetCdr(bit, &bit);
			GetBitTypeSymbol(bit, &bit);
			cons_heap(&root, bit, root);
		}
	}
	*ret = root;
}


/*
 *  initialize
 */
_g void init_package_common(void)
{
	SetPointerCall(defun, var1, defpackage_make);
}

