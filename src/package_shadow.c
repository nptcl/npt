#include "condition.h"
#include "condition_define.h"
#include "cons.h"
#include "hashtable.h"
#include "package.h"
#include "package_bittype.h"
#include "package_designer.h"
#include "package_import.h"
#include "package_shadow.h"
#include "strtype.h"
#include "symbol.h"
#include "type_table.h"
#include "typedef.h"

/****************************************************************************
 *  Function SHADOW
 ****************************************************************************/
static int symbol_shadow_package_(addr package, addr pos)
{
	int check;
	addr bit;
	struct bittype_struct *str;

	Return(string_designer_heap_(&pos, pos, NULL));
	Return(intern_bitpackage_(package, pos, &bit, &check));
	str = StructBitType(bit);
	if (str->inherit) {
		/* change type to intern from inherit. */
		shadowintern_bitpackage(bit, pos, package);
	}
	if (! str->shadow) {
		GetBitTypeSymbol(bit, &pos);
		Return(push_list_shadow_package_(package, pos));
		SetBitTypeShadow(bit, 1);
	}

	return 0;
}

static int list_shadow_package_(addr package, addr args)
{
	addr list, pos, type;

	/* type check */
	Check(! listp(args), "type error");
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! string_designer_p(pos)) {
			GetTypeTable(&type, StringDesigner);
			return call_type_error_va_(NULL, pos, type,
					"SHADOW ~S must be a string-designer.", pos, NULL);
		}
	}

	/* shadow */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(symbol_shadow_package_(package, pos));
	}

	return 0;
}

int shadow_package_(addr package, addr pos)
{
	addr type;

	Return(package_designer_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_CHARACTER:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_STRING:
		case LISPTYPE_ARRAY:
			return symbol_shadow_package_(package, pos);

		case LISPTYPE_CONS:
			return list_shadow_package_(package, pos);

		default:
			GetTypeTable(&type, StringDesignerList);
			return call_type_error_va_(NULL, pos, type,
					"SHADOW ~S must be a string-designer or list.", pos, NULL);
	}
}


/****************************************************************************
 *  Function SHADOWING-IMPORT
 ****************************************************************************/
static int export_shadowing_import_package_(addr pos, addr sym, addr sym0)
{
	addr bit, check;

	CheckType(pos, LISPTYPE_PACKAGE);
	Check(! symbolp(sym), "type error");
	Check(! symbolp(sym0), "type error");

	GetPackage(pos, PACKAGE_INDEX_TABLE, &pos);
	GetNameSymbol(sym, &bit);
	Return(findnil_hashtable_(pos, bit, &bit));
	if (bit == Nil)
		return 0;
	if (StructBitType(bit)->expt == 0)
		return 0;
	GetBitTypeSymbol(bit, &check);
	if (check != sym0)
		return 0;
	SetBitTypeSymbol(bit, sym);

	return 0;
}

static int inherited_shadowing_import_package_(addr package, addr sym, addr sym0)
{
	addr list, pos;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(sym), "type error");
	Check(! symbolp(sym0), "type error");

	GetPackage(package, PACKAGE_INDEX_USED, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(export_shadowing_import_package_(pos, sym, sym0));
	}

	return 0;
}

static int symbol_shadowing_import_package_(addr package, addr symbol)
{
	int check;
	addr hash, name, bit, pos;
	struct bittype_struct *str;

	CheckType(package, LISPTYPE_PACKAGE);
	Check(! symbolp(symbol), "type error");

	GetPackage(package, PACKAGE_INDEX_TABLE, &hash);
	GetNameSymbol(symbol, &name);
	Return(findnil_hashtable_(hash, name, &bit));

	/* Import symbol. */
	if (bit == Nil) {
		Return(import_bitpackage_(package, symbol, &bit, &check));
		Return(push_list_shadow_package_(package, symbol));
		SetBitTypeShadow(bit, 1);
		return 0;
	}

	/* Push into shadowing-symbols */
	str = StructBitType(bit);
	if (str->shadow == 0) {
		Return(push_list_shadow_package_(package, symbol));
		str->shadow = 1;
	}

	/* If already intern the same symbol. */
	GetBitTypeSymbol(bit, &pos);
	if (pos == symbol)
		return 0;

	/* Change the non-inherited symbol to the new symbol. */
	if (str->expt == 0) {
		Return(delete_hashtable_(hash, name, &check));
		return import_bitpackage_(package, symbol, &bit, &check);
	}

	/* Inherited symbol. */
	SetBitTypeSymbol(bit, symbol);
	return inherited_shadowing_import_package_(package, symbol, pos);
}

static int list_shadowing_import_package_(addr package, addr args)
{
	addr list, pos, type;

	/* type check */
	Check(! listp(args), "type error");
	list = args;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		if (! symbolp(pos)) {
			GetTypeTable(&type, StringDesigner);
			return call_type_error_va_(NULL, args, type,
					"SHADOWING-IMPORT ~S must be a symbol.", pos, NULL);
		}
	}

	/* shadowing-import */
	list = args;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(symbol_shadowing_import_package_(package, pos));
	}

	return 0;
}

int shadowing_import_package_(addr package, addr pos)
{
	addr type;

	Return(package_designer_(package, &package));
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			return symbol_shadowing_import_package_(package, pos);

		case LISPTYPE_CONS:
			return list_shadowing_import_package_(package, pos);

		default:
			GetTypeTable(&type, StringList);
			return call_type_error_va_(NULL, pos, type,
					"SHADOWING-IMPORT ~S must be a symbol or list.", pos, NULL);
	}
}

