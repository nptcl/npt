#include "character_check.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "copy.h"
#include "pathname_object.h"
#include "strtype.h"
#include "strvect.h"
#include "typedef.h"

/*
 *  access
 */
_g void getarray_pathname(addr pos, enum PATHNAME_INDEX index, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetArrayPathname_Low(pos, index, ret);
}

_g void setarray_pathname(addr pos, enum PATHNAME_INDEX index, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArrayPathname_Low(pos, index, value);
}

_g int reflogical_pathname(addr pos)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	return RefLogicalPathname_Low(pos);
}

_g void getlogical_pathname(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetLogicalPathname_Low(pos, ret);
}

_g void setlogical_pathname(addr pos, int value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetLogicalPathname_Low(pos, value);
}

_g void gethost_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetHostPathname_Low(pos, ret);
}

_g void sethost_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetHostPathname_Low(pos, value);
}

_g void getdevice_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetDevicePathname_Low(pos, ret);
}

_g void setdevice_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDevicePathname_Low(pos, value);
}

_g void getdirectory_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetDirectoryPathname_Low(pos, ret);
}

_g void setdirectory_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDirectoryPathname_Low(pos, value);
}

_g void getname_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetNamePathname_Low(pos, ret);
}

_g void setname_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetNamePathname_Low(pos, value);
}

_g void gettype_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetTypePathname_Low(pos, ret);
}

_g void settype_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTypePathname_Low(pos, value);
}

_g void getversion_pathname(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	GetVersionPathname_Low(pos, ret);
}

_g void setversion_pathname(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_PATHNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetVersionPathname_Low(pos, value);
}


/*
 *  pathname object
 */
static void directory_pathname_alloc(LocalRoot local, addr pos, addr *ret)
{
	addr absolute, wild, wild_inferiors;

	/* "string" -> (:absolute "string") */
	if (stringp(pos)) {
		GetConst(KEYWORD_ABSOLUTE, &absolute);
		list_alloc(local, ret, absolute, pos, NULL);
		return;
	}

	/* :wild -> (:absolute :wild-inferiors) */
	GetConst(KEYWORD_WILD, &wild);
	if (pos == wild) {
		GetConst(KEYWORD_ABSOLUTE, &absolute);
		GetConst(KEYWORD_WILD_INFERIORS, &wild_inferiors);
		list_alloc(local, ret, absolute, wild_inferiors, NULL);
		return;
	}

	/* :wild-inferiors -> (:absolute :wild-inferiors) */
	GetConst(KEYWORD_WILD_INFERIORS, &wild_inferiors);
	if (pos == wild_inferiors) {
		GetConst(KEYWORD_ABSOLUTE, &absolute);
		list_alloc(local, ret, absolute, wild_inferiors, NULL);
		return;
	}

	/* others */
	*ret = pos;
}

_g void make_pathname_alloc(LocalRoot local, addr *ret, int logical)
{
	alloc_array2(local, ret, LISPTYPE_PATHNAME, PATHNAME_INDEX_SIZE);
	SetLogicalPathname(*ret, logical);
}

static void setpathname_alloc(LocalRoot local, addr pos, addr host, addr device,
		addr directory, addr name, addr type, addr version)
{
	/* directory */
	directory_pathname_alloc(local, directory, &directory);

	/* set value */
	SetHostPathname(pos, host);
	SetDevicePathname(pos, device);
	SetDirectoryPathname(pos, directory);
	SetNamePathname(pos, name);
	SetTypePathname(pos, type);
	SetVersionPathname(pos, version);
}

_g void pathname_alloc(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type)
{
	addr version;

	make_pathname_alloc(local, ret, 0);
	GetConst(KEYWORD_UNSPECIFIC, &version);
	setpathname_alloc(local, *ret, host, device, directory, name, type, version);
}
_g void pathname_local(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type)
{
	Check(local == NULL, "local error");
	pathname_alloc(local, ret, host, device, directory, name, type);
}
_g void pathname_heap(addr *ret,
		addr host, addr device, addr directory, addr name, addr type)
{
	pathname_alloc(NULL, ret, host, device, directory, name, type);
}

_g void logical_pathname_alloc(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version)
{
	addr device;

	GetConst(KEYWORD_UNSPECIFIC, &device);
	make_pathname_alloc(local, ret, 1);
	setpathname_alloc(local, *ret, host, device, directory, name, type, version);
}
_g void logical_pathname_local(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version)
{
	Check(local == NULL, "local error");
	logical_pathname_alloc(local, ret, host, directory, name, type, version);
}
_g void logical_pathname_heap(addr *ret, addr host,
		addr directory, addr name, addr type, addr version)
{
	logical_pathname_alloc(NULL, ret, host, directory, name, type, version);
}

_g int pathnamep(addr pos)
{
	return GetType(pos) == LISPTYPE_PATHNAME;
}

_g int pathname_pathname_p(addr pos)
{
	return pathnamep(pos) && RefLogicalPathname(pos) == 0;
}

_g int pathname_logical_p(addr pos)
{
	return pathnamep(pos) && RefLogicalPathname(pos) != 0;
}

_g int pathname_file_p(addr pos)
{
	addr check;

	if (! pathnamep(pos))
		return 0;
	GetNamePathname(pos, &check);
	return check != Nil;
}

_g int pathname_directory_p(addr pos)
{
	addr name, type;

	if (! pathnamep(pos))
		return 0;
	GetNamePathname(pos, &name);
	GetTypePathname(pos, &type);

	return name == Nil && type == Nil;
}

_g void copylocal_pathname_array(LocalRoot local, addr a, int i, addr b)
{
	addr value;

	GetArrayPathname(a, (enum PATHNAME_INDEX)i, &value);
	copylocal_object(local, &value, value);
	SetArrayPathname(b, (enum PATHNAME_INDEX)i, value);
}

_g void copy_pathname_alloc(LocalRoot local, addr *ret, addr pos)
{
	int i;
	addr one;

	make_pathname_alloc(local, &one, RefLogicalPathname(pos));
	for (i = 0; i < PATHNAME_INDEX_SIZE; i++)
		copylocal_pathname_array(local, pos, i, one);
	*ret = one;
}

enum PathnameType {
	PathnameType_Unix,
	PathnameType_Windows,
	PathnameType_Logical,
	PathnameType_Error
};
static void make_pathname_environment(addr host, enum PathnameType *ret)
{
	addr check;

	/* unix */
	GetConst(SYSTEM_UNIX, &check);
	if (host == check) {
		*ret = PathnameType_Unix;
		return;
	}

	/* windows */
	GetConst(SYSTEM_WINDOWS, &check);
	if (host == check) {
		*ret = PathnameType_Windows;
		return;
	}

	/* logical */
	if (stringp(host)) {
		*ret = PathnameType_Logical;
		return;
	}

	/* error */
	*ret = PathnameType_Error;
}

_g lisp_equal_calltype pathname_equal_function(addr pos)
{
	enum PathnameType type;

	Check(! pathnamep(pos), "type left error");
	GetHostPathname(pos, &pos);
	make_pathname_environment(pos, &type);
	switch (type) {
		case PathnameType_Windows:
		case PathnameType_Logical:
			return equalp_function_;

		default:
			return equal_function_;
	}
}

static int make_pathname_environment_(addr host, enum PathnameType *ret)
{
	make_pathname_environment(host, ret);
	if (*ret == PathnameType_Error)
		return fmte_("Invalid host value ~S.", host, NULL);
	return 0;
}

_g int pathname_equal_(addr a, addr b, int *ret)
{
	int check;
	addr x, y;
	int (*equal)(addr, addr, int *);

	Check(! pathnamep(a), "type left error");
	Check(! pathnamep(b), "type right error");

	if (RefLogicalPathname(a) != RefLogicalPathname(b))
		return Result(ret, 0);

	/* host */
	GetHostPathname(a, &x);
	GetHostPathname(b, &y);
	Return(equalp_function_(x, y, &check));
	if (! check)
		return Result(ret, 0);
	equal = pathname_equal_function(a);

	/* device */
	GetDevicePathname(a, &x);
	GetDevicePathname(b, &y);
	Return((*equal)(x, y, &check));
	if (! check)
		return Result(ret, 0);

	/* directory */
	GetDirectoryPathname(a, &x);
	GetDirectoryPathname(b, &y);
	Return((*equal)(x, y, &check));
	if (! check)
		return Result(ret, 0);

	/* name */
	GetNamePathname(a, &x);
	GetNamePathname(b, &y);
	Return((*equal)(x, y, &check));
	if (! check)
		return Result(ret, 0);

	/* type */
	GetTypePathname(a, &x);
	GetTypePathname(b, &y);
	Return((*equal)(x, y, &check));
	if (! check)
		return Result(ret, 0);

	/* version */
	pathname_version(a, &x);
	pathname_version(b, &y);
	if (! eql_function(x, y))
		return Result(ret, 0);

	return Result(ret, 1);
}


/* make-pathname */
static int make_pathname_directory_keyword_p_(addr pos, int *ret)
{
	if (! stringp(pos))
		return Result(ret, 0);

	Return(string_equal_char_(pos, "*", ret));
	if (*ret)
		return 0;

	Return(string_equal_char_(pos, "**", ret));
	if (*ret)
		return 0;

	return string_equal_char_(pos, "..", ret);
}

static int make_pathname_directory_(addr *ret, addr list)
{
	int keywordp, check;
	addr root, pos, absolute, relative, wild, wildi, up;

	GetConst(KEYWORD_ABSOLUTE, &absolute);
	GetConst(KEYWORD_RELATIVE, &relative);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wildi);

	/* value */
	if (list == Nil || list == wild || list == wildi || stringp(list))
		return Result(ret, list);

	/* cons */
	if (! consp(list)) {
		*ret = Nil;
		return fmte_(":directory ~S must be a list or string type.", list, NULL);
	}
	GetConst(KEYWORD_UP, &up);
	Return_getcons(list, &pos, &root);
	if (pos != absolute && pos != relative) {
		*ret = Nil;
		return fmte_("The first argument of :directory ~S must be ~S or ~S.",
				pos, absolute, relative, NULL);
	}
	for (keywordp = 1; root != Nil; ) {
		Return_getcons(root, &pos, &root);
		Return(make_pathname_directory_keyword_p_(pos, &check));
		if (check)
			keywordp = 0;
		if (! stringp(pos) && pos != wild && pos != wildi && pos != up) {
			*ret = Nil;
			return fmte_("Invalid :directory argument ~S.", pos, NULL);
		}
	}
	if (keywordp)
		return Result(ret, list);

	/* rebuild */
	GetCons(list, &pos, &list);
	conscar_heap(&root, pos);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		/* "*" */
		Return(stringp_equal_char_(pos, "*", &check));
		if (check) {
			cons_heap(&root, wild, root);
			continue;
		}
		/* "**" */
		Return(stringp_equal_char_(pos, "**", &check));
		if (check) {
			cons_heap(&root, wildi, root);
			continue;
		}
		/* ".." */
		Return(stringp_equal_char_(pos, "..", &check));
		if (check) {
			cons_heap(&root, up, root);
			continue;
		}
		/* else */
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static int make_pathname_string_(addr *ret)
{
	int check;
	addr pos;

	pos = *ret;
	if (! stringp(pos))
		return Result(ret, pos);

	Return(string_upper_p_(pos, &check));
	if (check)
		return string_lower_heap_(pos, ret);

	Return(string_lower_p_(pos, &check));
	if (check)
		return string_upper_heap_(pos, ret);

	return  Result(ret, pos);
}

static int make_pathname_check_(addr pos, int *ret)
{
	int check;

	if (! stringp(pos))
		return Result(ret, 0);
	Return(string_upper_p_(pos, &check));
	if (check)
		return Result(ret, 1);

	return string_lower_p_(pos, ret);
}

static int make_pathname_list_p_(addr list, int *ret)
{
	int check;
	addr pos;

	/* string */
	if (stringp(list))
		return make_pathname_check_(list, ret);

	/* others */
	while (consp_getcons(list, &pos, &list)) {
		Return(make_pathname_check_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int make_pathname_list_(addr *ret, addr list)
{
	int check;
	addr root, pos;

	/* string */
	if (stringp(list)) {
		Return(make_pathname_string_(&list));
		return Result(ret, list);
	}

	/* atom */
	if (! consp(list))
		return Result(ret, list);

	/* cons */
	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(make_pathname_check_(pos, &check));
		if (check) {
			Return(make_pathname_string_(&pos));
		}
		cons_heap(&root, pos, root);
	}
	nreverse(ret, root);

	return 0;
}

static int make_pathname_case_(addr *directory, addr *name, addr *type)
{
	int check;

	/* directory */
	Return(make_pathname_list_p_(*directory, &check));
	if (check) {
		Return(make_pathname_list_(directory, *directory));
	}
	/* name, type */
	Return(make_pathname_string_(name));
	Return(make_pathname_string_(type));

	return 0;
}

static int pathname_type_lower_p(enum PathnameType ptype, int localp)
{
	return localp == 0 && ptype == PathnameType_Unix;
}

static int pathname_host_lower_p(addr host, int localp)
{
	enum PathnameType ptype;

	if (localp)
		return 0;
	Return(make_pathname_environment_(host, &ptype));
	return ptype == PathnameType_Unix;
}

static int make_pathname_logical_(addr *ret,
		addr host, addr device, addr directory,
		addr name, addr type, addr version)
{
	int check;
	addr unspec;

	/* host */
	Return(string_upper_p_(host, &check));
	if (! check) {
		Return(string_upper_heap_(host, &host));
	}

	/* defaults */
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	if (version == unspec)
		version = Nil;

	/* make */
	logical_pathname_heap(ret, host, directory, name, type, version);
	return 0;
}

_g int make_pathname_heap_(addr *ret,
		addr host, addr device, addr directory,
		addr name, addr type, addr version, int localp)
{
	enum PathnameType ptype;

	/* format */
	Return(make_pathname_directory_(&directory, directory));

	/* case */
	Return(make_pathname_environment_(host, &ptype));
	if (pathname_type_lower_p(ptype, localp)) {
		Return(make_pathname_case_(&directory, &name, &type));
	}

	/* pathname */
	if (ptype == PathnameType_Logical) {
		Return(make_pathname_logical_(ret,
					host, device, directory, name, type, version));
	}
	else {
		pathname_heap(ret, host, device, directory, name, type);
	}

	return 0;
}


/*
 *  pathname accessor
 */
_g int pathname_host_(addr pos, addr *ret, int ignore_localp)
{
	GetHostPathname(pos, &pos);
	return Result(ret, pos);
}

_g int pathname_device_(addr pos, addr *ret, int ignore_localp)
{
	GetDevicePathname(pos, &pos);
	return Result(ret, pos);
}

_g int pathname_directory_(addr pos, addr *ret, int localp)
{
	int check;
	addr host;

	GetHostPathname(pos, &host);
	GetDirectoryPathname(pos, &pos);
	if (pathname_host_lower_p(host, localp)) {
		Return(make_pathname_list_p_(pos, &check));
		if (check) {
			Return(make_pathname_list_(&pos, pos));
		}
	}

	return Result(ret, pos);
}

_g int pathname_name_(addr pos, addr *ret, int localp)
{
	addr host;

	GetHostPathname(pos, &host);
	GetNamePathname(pos, &pos);
	if (pathname_host_lower_p(host, localp)) {
		Return(make_pathname_string_(&pos));
	}

	return Result(ret, pos);
}

_g int pathname_type_(addr pos, addr *ret, int localp)
{
	addr host;

	GetHostPathname(pos, &host);
	GetTypePathname(pos, &pos);
	if (pathname_host_lower_p(host, localp)) {
		Return(make_pathname_string_(&pos));
	}

	return Result(ret, pos);
}

_g void pathname_version(addr pos, addr *ret)
{
	addr version;

	GetVersionPathname(pos, &version);
	if (RefLogicalPathname(pos) != 0 && version == Nil) {
		GetConst(KEYWORD_NEWEST, ret);
	}
	else {
		*ret = version;
	}
}

