#include "package.h"
#include "package_object.h"
#include "strtype.h"
#include "typedef.h"

_g int packagep(addr pos)
{
	return GetType(pos) == LISPTYPE_PACKAGE;
}

_g int package_designer_p(addr pos)
{
	return packagep(pos) || string_designer_p(pos);
}

_g int package_designer_equal(addr left, addr right)
{
	if (packagep(left))
		GetPackage(left, PACKAGE_INDEX_NAME, &left);
	if (packagep(right))
		GetPackage(right, PACKAGE_INDEX_NAME, &right);

	return string_designer_equal(left, right);
}


/*
 *  package function
 */
_g void getname_package(addr pos, addr *ret)
{
	package_designer(pos, &pos);
	GetPackage(pos, PACKAGE_INDEX_NAME, ret);
}

_g void getnickname_package(addr pos, addr *ret)
{
	package_designer(pos, &pos);
	GetPackage(pos, PACKAGE_INDEX_NICKNAME, ret);
}

_g void getuselist_package(addr pos, addr *ret)
{
	package_designer(pos, &pos);
	GetPackage(pos, PACKAGE_INDEX_USE, ret);
}

_g void getusedbylist_package(addr pos, addr *ret)
{
	package_designer(pos, &pos);
	GetPackage(pos, PACKAGE_INDEX_USED, ret);
}

_g void getshadow_package(addr pos, addr *ret)
{
	package_designer(pos, &pos);
	GetPackage(pos, PACKAGE_INDEX_SHADOW, ret);
}

_g void getdocument_package(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	GetPackage(pos, PACKAGE_INDEX_DOCUMENT, ret);
}

_g void setdocument_package(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	SetPackage(pos, PACKAGE_INDEX_DOCUMENT, value);
}
