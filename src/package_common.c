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
#include "integer_calc.h"
#include "package.h"
#include "package_bittype.h"
#include "package_common.h"
#include "package_delete.h"
#include "package_designer.h"
#include "package_export.h"
#include "package_intern.h"
#include "package_object.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  gentemp
 */
int make_gentemp_(Execute ptr, addr prefix, addr package, addr *ret)
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
			Return(funcall1_control_(ptr, &bit, call, bit, NULL));
		}
	}

	return 0;
}

int do_symbols_package(Execute ptr, addr call, addr package)
{
	return syscall_do_symbols_check(ptr, call, package);
}

int do_external_symbols_package(Execute ptr, addr call, addr package)
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
				Return(funcall1_control_(ptr, &bit, call, bit, NULL));
			}
		}
	}

	return 0;
}

int do_all_symbols_package_(Execute ptr, addr call)
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

int all_symbols_package_(addr package, addr *ret)
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

