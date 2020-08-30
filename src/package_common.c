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
#include "package_common.h"
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
	if (package == NULL) {
		Return(getpackage_(ptr, &package));
	}
	else {
		Return(package_designer_(package, &package));
	}
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
		if (prefix == NULL) {
			Return(pushchar_charqueue_local_(local, queue, "T"));
		}
		else {
			Return(pushstring_charqueue_local_(local, queue, prefix));
		}
		Return(decimal_charqueue_integer_local_(local, value, queue));
		make_charqueue_local(local, queue, &name);
		Return(find_symbol_package_(package, name, &gentemp, &type));
		if (type == PACKAGE_TYPE_NIL)
			make_charqueue_heap(queue, &name);
		rollback_local(local, stack);

		/* (1+ *gentemp-counter*) */
		Return(oneplus_integer_common_(local, value, &value));
		SetConst(PACKAGE_GENTEMP, value);

		/* check intern */
		if (type == PACKAGE_TYPE_NIL)
			break;
	}

	/* gentemp */
	Return(intern_package_(package, name, &gentemp, NULL));
	if (keyword) {
		Return(export_package_(package, gentemp));
	}
	
	return Result(ret, gentemp);
}


/*
 *  defpackage
 */
static int defpackage_findcons_(addr table, addr key, addr *ret)
{
	if (stringp(key))
		return findcons_hashtable_(table, key, ret);

	if (symbolp(key)) {
		GetNameSymbol(key, &key);
		return findcons_hashtable_(table, key, ret);
	}

	if (characterp(key))
		return findcons_unicode_hashtable_(table, RefCharacter(key), ret);

	return Result(ret, Nil);
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

static int defpackage_check_nicknames_(addr pos, addr names)
{
	addr table, list, name, check;

	PackageTable(&table);
	for (list = names; list != Nil; ) {
		GetCons(list, &name, &list);
		Return(defpackage_findcons_(table, name, &check));
		if (check != Nil) {
			GetCdr(check, &check);
			if (pos != check)
				return fmte_("nickname ~A is already exists.", name, NULL);
		}
	}

	return 0;
}

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

static int defpackage_update_shadow_(LocalRoot local, addr pos, addr list)
{
	defpackage_make_nicknames(local, &list, list);
	return shadow_list_package_(pos, list);
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
			Return(shadowing_import_symbol_package_(pos, key));
		}
	}

	return 0;
}

static int defpackage_update_use_(LocalRoot local, addr pos, addr list)
{
	defpackage_make_nicknames(local, &list, list);
	return use_package_list_package_(pos, list);
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
	addr child, name;

	while (list != Nil) {
		GetCons(list, &child, &list);
		while (child != Nil) {
			GetCons(child, &name, &child);
			Return(string_designer_heap_(&name, name, NULL));
			Return(intern_package_table_(pos, name, &name, NULL));
		}
	}

	return 0;
}

static int defpackage_update_export_(LocalRoot local, addr pos, addr list)
{
	addr root, child, name;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &child, &list);
		while (child != Nil) {
			GetCons(child, &name, &child);
			Return(string_designer_heap_(&name, name, NULL));
			Return(intern_package_table_(pos, name, &name, NULL));
			cons_local(local, &root, name, root);
		}
	}
	nreverse(&root, root);

	return export_list_package_(pos, root);
}

static int defpackage_update_(LocalRoot local, addr pos, addr rest)
{
	addr nicknames, use, shadow, shadowing, import, expt, intern;

	List_bind(rest, &nicknames, &use, &shadow, &shadowing,
			&import, &expt, &intern, NULL);
	/* nicknames */
	defpackage_make_nicknames(local, &nicknames, nicknames);
	Return(defpackage_check_nicknames_(pos, nicknames));
	Return(defpackage_update_nicknames_(pos, nicknames));
	/* shadow, shadowing-symbols */
	Return(defpackage_update_shadow_(local, pos, shadow));
	Return(defpackage_update_shadowing_(pos, shadowing));
	/* use */
	Return(defpackage_update_use_(local, pos, use));
	/* import-from, intern */
	Return(defpackage_update_import_(local, pos, import));
	Return(defpackage_update_intern_(pos, intern));
	/* export */
	Return(defpackage_update_export_(local, pos, expt));

	return 0;
}

static int function_defpackage_make(Execute ptr, addr condition)
{
	int check;
	addr pos;

	/* delete */
	getdata_control(ptr, &pos);
	Return(delete_package_(pos, &check));
	/* throw */
	return error_function_(ptr, condition);
}

static int defpackage_make_call_(Execute ptr, addr pos, addr rest)
{
	addr symbol, call;

	/* handler-case */
	GetConst(COMMON_ERROR, &symbol);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_defpackage_make);
	SetDataFunction(call, pos);
	Return(pushhandler_common_(ptr, symbol, call, 0));
	/* code */
	return defpackage_update_(ptr->local, pos, rest);
}

static int defpackage_make(Execute ptr, addr pos, addr rest)
{
	addr control;

	push_control(ptr, &control);
	(void)defpackage_make_call_(ptr, pos, rest);
	return pop_control_(ptr, control);
}

static int resize_pacakge_(addr pos, size_t size)
{
	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	return force_resize_hashtable_(pos, size);
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
			return fmte_(":size ~S is too large.", size, NULL);
	}

	/* package */
	GetCons(rest, &doc, &rest);
	Return(find_package_(name, &pos));
	if (pos == Nil) {
		if (sizep) {
			Return(package_size_heap_(&pos, name, value));
		}
		else {
			Return(package_heap_(&pos, name));
		}
		Return(defpackage_make(ptr, pos, rest));
	}
	else {
		if (sizep) {
			Return(resize_pacakge_(pos, value));
		}
		Return(defpackage_update_(ptr->local, pos, rest));
	}

	/* documentation */
	setdocument_package(pos, doc);

	/* result */
	return Result(ret, pos);
}


/*
 *  iterator
 */
static int syscall_do_symbols_check(Execute ptr, addr call, addr package)
{
	addr table, list, bit;
	size_t size, i;

	Return(package_designer_(package, &package));
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	GetTableHash(table, &table);
	LenArrayHash(table, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &bit, &list);
			GetCdr(bit, &bit);
			GetBitTypeSymbol(bit, &bit);
			Return(callclang_funcall(ptr, &bit, call, bit, NULL));
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

	Return(package_designer_(package, &package));
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
				Return(callclang_funcall(ptr, &bit, call, bit, NULL));
			}
		}
	}

	return 0;
}

_g int do_all_symbols_package_(Execute ptr, addr call)
{
	int check;
	addr array, left, right, key, value;
	size_t i, size;

	PackageTable(&array);
	GetTableHash(array, &array);
	LenArrayHash(array, &size);
	for (i = 0; i < size; i++) {
		GetArrayHash(array, i, &right);
		while (right != Nil) {
			GetCons(right, &left, &right);
			GetCons(left, &key, &left);
			GetPackage(left, PACKAGE_INDEX_NAME, &value);
			Return(string_equal_(key, value, &check));
			if (check) {
				Return(syscall_do_symbols_check(ptr, call, left));
			}
		}
	}

	return 0;
}

_g int all_symbols_package_(addr package, addr *ret)
{
	addr table, list, bit, root;
	size_t size, i;

	Return(package_designer_(package, &package));
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

	return Result(ret, root);
}


/*
 *  initialize
 */
_g void init_package_common(void)
{
	SetPointerCall(defun, var1, defpackage_make);
}

