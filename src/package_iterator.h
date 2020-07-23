#ifndef __PACKAGE_ITERATOR_HEADER__
#define __PACKAGE_ITERATOR_HEADER__

#include "local.h"
#include "typedef.h"

struct StructPackageIterator {
	unsigned internal : 1;
	unsigned external : 1;
	unsigned inherited : 1;
	unsigned finish : 1;
};

enum PackageIterator {
	PackageIterator_List,
	PackageIterator_Table,
	PackageIterator_Package,
	PackageIterator_Size
};

#define PtrPackageIterator(x) PtrBodySSa(x, PackageIterator_Size)
#define PtrStructPackageIterator(x) \
	((struct StructPackageIterator *)PtrPackageIterator(x))
#define GetIndexPackageIterator(x) (PtrStructPackagetable(x)->index)
#define RetPackageIterator RetArraySS
#define GetPackageIterator GetArraySS
#define SetPackageIterator SetArraySS

_g int package_iterator_alloc_(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited);
_g int package_iterator_local_(LocalRoot local, addr *ret,
		addr list, int internal, int external, int inherited);
_g int package_iterator_heap_(addr *ret,
		addr list, int internal, int external, int inherited);
_g int next_package_iterator_(addr pos, addr *rets, addr *retp, enum PACKAGE_TYPE *ret);

#endif

