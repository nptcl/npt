#include "constant.h"
#include "cons.h"
#include "copy.h"
#include "define.h"
#include "pathname_localp.h"
#include "pathname_object.h"
#include "pathname.h"
#include "strtype.h"

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
_g void make_pathname_alloc(LocalRoot local, addr *ret, int logical)
{
	alloc_array2(local, ret, LISPTYPE_PATHNAME, PATHNAME_INDEX_SIZE);
	SetLogicalPathname(*ret, logical);
}

static void setpathname(LocalRoot local, addr pos, addr host, addr device,
		addr directory, addr name, addr type, addr version)
{
	addr symbol;

	/* directory */
	if (directory == Nil) {
		GetConst(KEYWORD_RELATIVE, &symbol);
		conscar_alloc(local, &directory, symbol);
	}

	/* set */
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
	setpathname(local, *ret, host, device, directory, name, type, version);
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
	setpathname(local, *ret, host, device, directory, name, type, version);
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

_g int pathname_equal(addr a, addr b)
{
	addr check1, check2;

	Check(! pathnamep(a), "type left error");
	Check(! pathnamep(b), "type right error");

	if (RefLogicalPathname(a) != RefLogicalPathname(b))
		return 0;
	/* host */
	GetHostPathname(a, &check1);
	GetHostPathname(b, &check2);
	if (! equalp_function(check1, check2))
		return 0;
	/* device */
	GetDevicePathname(a, &check1);
	GetDevicePathname(b, &check2);
	if (! LispPathnameEqual(check1, check2))
		return 0;
	/* directory */
	GetDirectoryPathname(a, &check1);
	GetDirectoryPathname(b, &check2);
	if (! LispPathnameEqual(check1, check2))
		return 0;
	/* name */
	GetNamePathname(a, &check1);
	GetNamePathname(b, &check2);
	if (! LispPathnameEqual(check1, check2))
		return 0;
	/* type */
	GetTypePathname(a, &check1);
	GetTypePathname(b, &check2);
	if (! LispPathnameEqual(check1, check2))
		return 0;
	/* version */
	GetVersionPathname(a, &check1);
	GetVersionPathname(b, &check2);
	if (! eql_function(check1, check2))
		return 0;

	return 1;
}


/*
 *  wild_pathname_boolean
 */
static int wild_pathname_string(addr pos)
{
	unicode c;
	size_t size, i;

	if (! stringp(pos))
		return 0;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (c == '*' || c == '?')
			return 1;
	}

	return 0;
}

_g int wild_pathname_boolean(addr file, addr field)
{
	addr check, pos, wild1, wild2, value;

	Check(! pathnamep(file), "type error");
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);

	/* skip host*/
	/* skip device */

	/* directory */
	GetConst(KEYWORD_DIRECTORY, &check);
	if (field == check || field == Nil) {
		GetDirectoryPathname(file, &pos);
		if (pos == wild1)
			return 1;
		while (consp(pos)) {
			GetCons(pos, &value, &pos);
			if (value == wild1)
				return 1;
			if (value == wild2)
				return 1;
			if (wild_pathname_string(value))
				return 1;
		}
	}

	/* name */
	GetConst(KEYWORD_NAME, &check);
	if (field == check || field == Nil) {
		GetNamePathname(file, &pos);
		if (pos == wild1)
			return 1;
		if (wild_pathname_string(pos))
			return 1;
	}

	/* type */
	GetConst(KEYWORD_TYPE, &check);
	if (field == check || field == Nil) {
		GetTypePathname(file, &pos);
		if (pos == wild1)
			return 1;
		if (wild_pathname_string(pos))
			return 1;
	}

	/* version */
	GetConst(KEYWORD_VERSION, &check);
	if (field == check || field == Nil) {
		GetVersionPathname(file, &pos);
		if (pos == wild1)
			return 1;
	}

	return 0;
}


/*
 *  wildcard-pathname-p
 */
static int wildcard_character_pathname(
		addr p1, size_t n1, size_t s1,
		addr p2, size_t n2, size_t s2)
{
	unicode c1, c2;
	size_t i;

	if (n1 == s1 && n2 == s2)
		return 1;
	if (n1 == s1 || n2 == s2)
		return 0;
	string_getc(p1, n1, &c1);
	string_getc(p2, n2, &c2);
	/* (a ?) -> next */
	if (c2 == '?')
		return wildcard_character_pathname(p1,n1+1,s1,  p2,n2+1,s2);
	/* (a a) -> next, (a b) -> false */
	if (c2 != '*') {
		if (c1 != c2)
			return 0;
		return wildcard_character_pathname(p1,n1+1,s1,  p2,n2+1,s2);
	}
	/* (a *) */
	n2++;
	for (i = n1; i <= s1; i++) {
		if (wildcard_character_pathname(p1,i,s1,  p2,n2,s2))
			return 1;
	}
	return 0;
}

static int wildcard_string_p(addr pos)
{
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (c == '*' || c == '?')
			return 1;
	}

	return 0;
}

_g int wildcard_stringp_p(addr pos)
{
	return stringp(pos) && wildcard_string_p(pos);
}

_g int wildcard_string_pathname(addr a, addr b)
{
	int check1, check2;
	addr wild;
	size_t s1, s2;

	GetConst(KEYWORD_WILD, &wild);
	if (a == wild && b == wild)
		return 1;
	check1 = stringp(a);
	check2 = stringp(b);
	if (check1 && b == wild)
		return 1;
	if ((! check1) || (! check2))
		return 0;
	if (LispPathnameEqual(a, b))
		return 1;
	if (wildcard_string_p(a))
		return 0;
	string_length(a, &s1);
	string_length(b, &s2);
	return wildcard_character_pathname(a, 0, s1, b, 0, s2);
}

_g int wildcard_eq_pathname(addr a, addr b)
{
	return (a == b) || wildcard_string_pathname(a, b);
}

static int wildcard_nil_pathname(addr a, addr b, int wildp)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	if (wildp && a == Nil)
		a = wild;
	if (wildp && b == Nil)
		b = wild;

	return wildcard_eq_pathname(a, b);
}

static int wildcard_list_pathname(addr a, addr b)
{
	addr a1, b1, pos1, pos2, wild, wilds;

	if (a == Nil && b == Nil)
		return 1;
	if (a != Nil && b == Nil)
		return 0;
	GetConst(KEYWORD_WILD_INFERIORS, &wilds);
	if (a == Nil && b != Nil) {
		while (b != Nil) {
			getcons(b, &pos2, &b);
			if (pos2 != wilds)
				return 0;
		}
		return 1;
	}
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wilds);
	getcons(a, &pos1, &a1);
	getcons(b, &pos2, &b1);
	/* ("str" *) -> next */
	if (pos2 == wild)
		return wildcard_list_pathname(a1, b1);
	/* ("str" "str") -> next, ("str" "aaa") -> false */
	if (pos2 != wilds) {
		if (! wildcard_string_pathname(pos1, pos2))
			return 0;
		return wildcard_list_pathname(a1, b1);
	}
	/* ("str" **) */
	for (;;) {
		if (wildcard_list_pathname(a, b1))
			return 1;
		if (a == Nil)
			break;
		getcdr(a, &a);
	}
	return 0;
}

static int wildcard_directory_p(addr pos)
{
	addr check;

	GetConst(KEYWORD_WILD, &check);
	if (pos == check)
		return 1;
	GetConst(KEYWORD_WILD_INFERIORS, &check);
	if (pos == check)
		return 1;

	return wildcard_stringp_p(pos);
}

static int wildcard_directory_pathname(addr a, addr b)
{
	int check1, check2;
	addr car1, car2, cdr1, cdr2;

	cdr1 = a;
	cdr2 = b;
	check1 = 0;
	check2 = 1;
	for (;;) {
		if (cdr1 == Nil && cdr2 == Nil)
			return 1;
		if (cdr1 == Nil || cdr2 == Nil)
			break;
		getcons(cdr1, &car1, &cdr1);
		getcons(cdr2, &car2, &cdr2);
		if (wildcard_directory_p(car1))
			check1 = 1;
		if (wildcard_directory_p(car2))
			check2 = 1;
		if (! LispPathnameEqual(car1, car2))
			break;
	}
	if (check1 || (! check2))
		return 0;
	else {
		getcdr(a, &a);
		getcdr(b, &b);
		return wildcard_list_pathname(a, b);
	}
}

static int wildcard_version_pathname(addr a, addr b)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	if (a == Nil)
		a = wild;
	if (b == Nil)
		b = wild;
	if (eql_function(a, b))
		return 1;

	return b == wild;
}

_g int wildcard_pathname(addr a, addr b, int wild)
{
	addr check1, check2;

	Check(! pathnamep(a), "type left error");
	Check(! pathnamep(b), "type right error");
	if (RefLogicalPathname(a) != RefLogicalPathname(b))
		return 0;
	/* host */
	GetHostPathname(a, &check1);
	GetHostPathname(b, &check2);
	if (! equalp_function(check1, check2))
		return 0;
	/* device */
	GetDevicePathname(a, &check1);
	GetDevicePathname(b, &check2);
	if (! LispPathnameEqual(check1, check2))
		return 0;
	/* directory */
	GetDirectoryPathname(a, &check1);
	GetDirectoryPathname(b, &check2);
	if (! wildcard_directory_pathname(check1, check2))
		return 0;
	/* name */
	GetNamePathname(a, &check1);
	GetNamePathname(b, &check2);
	if (! wildcard_nil_pathname(check1, check2, wild))
		return 0;
	/* type */
	GetTypePathname(a, &check1);
	GetTypePathname(b, &check2);
	if (! wildcard_nil_pathname(check1, check2, wild))
		return 0;
	/* version */
	GetVersionPathname(a, &check1);
	GetVersionPathname(b, &check2);
	if (! wildcard_version_pathname(check1, check2))
		return 0;

	return 1;
}

