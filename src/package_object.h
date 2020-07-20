#ifndef __PACKAGE_OBJECT_HEADER__
#define __PACKAGE_OBJECT_HEADER__

#include "memory.h"
#include "typedef.h"

enum PACKAGE_INDEX {
	PACKAGE_INDEX_TABLE = 0,
	PACKAGE_INDEX_NAME,
	PACKAGE_INDEX_NICKNAME,
	PACKAGE_INDEX_USE,
	PACKAGE_INDEX_USED,
	PACKAGE_INDEX_SHADOW,
	PACKAGE_INDEX_EXPORT,
	PACKAGE_INDEX_DOCUMENT,
	PACKAGE_INDEX_SIZE
};

enum PACKAGE_TYPE {
	PACKAGE_TYPE_NIL = 0,
	PACKAGE_TYPE_INTERNAL,
	PACKAGE_TYPE_EXTERNAL,
	PACKAGE_TYPE_INHERITED,
	PACKAGE_TYPE_SIZE
};

#define GetPackage      GetArrayA2
#define SetPackage      SetArrayA2

_g int packagep(addr pos);
_g int package_designer_p(addr pos);
_g int package_designer_equal(addr left, addr right);

_g void getname_package(addr pos, addr *ret);
_g void getnickname_package(addr pos, addr *ret);
_g void getuselist_package(addr pos, addr *ret);
_g void getusedbylist_package(addr pos, addr *ret);
_g void getshadow_package(addr pos, addr *ret);
_g void getdocument_package(addr pos, addr *ret);
_g void setdocument_package(addr pos, addr value);

#endif
