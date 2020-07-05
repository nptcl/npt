#include "cons.h"
#include "hashtable.h"
#include "package.h"
#include "package_bittype.h"
#include "package_iterator.h"
#include "package_object.h"
#include "typedef.h"

_g void package_iterator_alloc(LocalRoot local, addr *ret,
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
		*ret = pos;
		return;
	}

	/* package or list */
	if (listp(list)) {
		getcons(list, &package, &list);
	}
	else {
		package = list;
		list = Nil;
	}

	/* package -> hash-iterator */
	package_designer(package, &package);
	GetPackage(package, PACKAGE_INDEX_TABLE, &table);
	hash_iterator_alloc(local, &table, table);

	/* result */
	SetPackageIterator(pos, PackageIterator_List, list);
	SetPackageIterator(pos, PackageIterator_Table, table);
	SetPackageIterator(pos, PackageIterator_Package, package);
	*ret = pos;
}

_g void package_iterator_local(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited)
{
	Check(local == NULL, "local error");
	package_iterator_alloc(local, ret, list, internal, external, inherited);
}

_g void package_iterator_heap(addr *ret,
		addr list, int internal, int external, int inherited)
{
	package_iterator_alloc(NULL, ret, list, internal, external, inherited);
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

static void forward_package_iterator(addr pos)
{
	addr list, raw, table, package;

	GetPackageIterator(pos, PackageIterator_List, &list);
	if (list == Nil) {
		PtrStructPackageIterator(pos)->finish = 1;
		return;
	}
	getcons(list, &package, &list);
	package_designer(package, &package);
	GetPackage(package, PACKAGE_INDEX_TABLE, &raw);
	GetPackageIterator(pos, PackageIterator_Table, &table);
	set_hash_iterator(table, raw);
	SetPackageIterator(pos, PackageIterator_List, list);
	SetPackageIterator(pos, PackageIterator_Package, package);
}

_g enum PACKAGE_TYPE next_package_iterator(addr pos, addr *rets, addr *retp)
{
	enum PACKAGE_TYPE type;
	struct StructPackageIterator *str;

	CheckType(pos, LISPSYSTEM_PACKAGEITERATOR);
	str = PtrStructPackageIterator(pos);
	while (! str->finish) {
		type = hash_package_iterator(pos, rets, retp);
		if (type != PACKAGE_TYPE_NIL)
			return type;
		forward_package_iterator(pos);
	}

	return PACKAGE_TYPE_NIL;
}

