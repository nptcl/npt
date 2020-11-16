#include "cons.h"
#include "hashtable.h"
#include "package.h"
#include "package_bittype.h"
#include "package_designer.h"
#include "package_iterator.h"
#include "package_object.h"
#include "typedef.h"

int package_iterator_alloc_(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited)
{
	addr pos, package, table;
	struct StructPackageIterator *str;

	/* object */
	alloc_smallsize(local, &pos,
			LISPSYSTEM_PACKAGEITERATOR,
			PackageIterator_Size,
			sizeoft(struct StructPackageIterator));
	str = PtrStructPackageIterator(pos);
	clearpoint(str);
	str->internal = (internal != 0);
	str->external = (external != 0);
	str->inherited = (inherited != 0);
	str->finish = 0;

	/* no-package */
	if (list == Nil) {
		str->finish = 1;
		return Result(ret, pos);
	}

	/* package or list */
	if (listp(list)) {
		Return_getcons(list, &package, &list);
	}
	else {
		package = list;
		list = Nil;
	}

	/* package -> hash-iterator */
	Return(package_designer_(package, &package));
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	hash_iterator_alloc(local, &table, table);

	/* result */
	SetPackageIterator(pos, PackageIterator_List, list);
	SetPackageIterator(pos, PackageIterator_Table, table);
	SetPackageIterator(pos, PackageIterator_Package, package);

	return Result(ret, pos);
}

int package_iterator_local_(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited)
{
	Check(local == NULL, "local error");
	return package_iterator_alloc_(local, ret, list, internal, external, inherited);
}

int package_iterator_heap_(addr *ret,
		addr list, int internal, int external, int inherited)
{
	return package_iterator_alloc_(NULL, ret, list, internal, external, inherited);
}

static enum PACKAGE_TYPE hash_package_iterator(addr pos, addr *rets, addr *retp)
{
	enum PACKAGE_TYPE type;
	addr table, key, value;
	struct StructPackageIterator *str;
	struct bittype_struct *bit;

	str = PtrStructPackageIterator(pos);
	GetPackageIterator(pos, PackageIterator_Table, &table);
	while (next_hash_iterator(table, &key, &value)) {
		bit = StructBitType(value);
		type = bit->intern;
		if ((str->internal && type == PACKAGE_TYPE_INTERNAL) ||
				(str->external && type == PACKAGE_TYPE_EXTERNAL) ||
				(str->inherited && type == PACKAGE_TYPE_INHERITED)) {
			GetBitTypeSymbol(value, rets);
			GetPackageIterator(pos, PackageIterator_Package, retp);
			return type;
		}
	}

	return PACKAGE_TYPE_NIL;
}

static int forward_package_iterator_(addr pos)
{
	addr list, raw, table, package;

	GetPackageIterator(pos, PackageIterator_List, &list);
	if (list == Nil) {
		PtrStructPackageIterator(pos)->finish = 1;
		return 0;
	}
	Return_getcons(list, &package, &list);
	Return(package_designer_(package, &package));
	GetPackage(package, PACKAGE_INDEX_TABLE, &raw);
	GetPackageIterator(pos, PackageIterator_Table, &table);
	set_hash_iterator(table, raw);
	SetPackageIterator(pos, PackageIterator_List, list);
	SetPackageIterator(pos, PackageIterator_Package, package);

	return 0;
}

int next_package_iterator_(addr pos, addr *rets, addr *retp, enum PACKAGE_TYPE *ret)
{
	enum PACKAGE_TYPE type;
	struct StructPackageIterator *str;

	CheckType(pos, LISPSYSTEM_PACKAGEITERATOR);
	str = PtrStructPackageIterator(pos);
	while (! str->finish) {
		type = hash_package_iterator(pos, rets, retp);
		if (type != PACKAGE_TYPE_NIL)
			return Result(ret, type);
		Return(forward_package_iterator_(pos));
	}

	return Result(ret, PACKAGE_TYPE_NIL);
}

