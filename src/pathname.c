#include "bignum.h"
#include "charqueue.h"
#include "condition.h"
#include "constant.h"
#include "copy.h"
#include "cons.h"
#include "cons_list.h"
#include "integer.h"
#include "pathname_common.h"
#include "pathname_localp.h"
#include "pathname_object.h"
#include "pathname_table.h"
#include "pathname_translate.h"
#include "pathname.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  parser table
 */
static int pathname_system_unix_p(addr pos)
{
	addr check;
	GetConst(SYSTEM_UNIX, &check);
	return pos == check;
}

static int pathname_system_windows_p(addr pos)
{
	addr check;
	GetConst(SYSTEM_WINDOWS, &check);
	return pos == check;
}


/*
 *  parse-namestring
 */
static void parser_struct_pathname(struct fileparse *pa)
{
	addr host;

#ifdef LISP_DEBUG
	size_t size;
	string_length(pa->thing, &size);
	Check(pa->end < pa->start || size < pa->end, "size error");
#endif

	host = pa->host;
	if (host == Nil)
		GetHostPathname(pa->path, &host);

	/* unix */
	if (pathname_system_unix_p(host)) {
		parser_unix_pathname(pa);
		return;
	}

	/* windows */
	if (pathname_system_windows_p(host)) {
		parser_windows_pathname(pa);
		return;
	}

	/* logical pathname */
	if (stringp(host)) {
		parser_logical_pathname(pa);
		return;
	}

	/* error */
	fmte("Unknown pathname-host ~S.", host, NULL);
}

static void defaults_pathname_alloc(Execute ptr, addr *ret, addr defaults, int localp)
{
	if (defaults == Nil || defaults == Unbound) {
		GetConst(SPECIAL_DEFAULT_PATHNAME_DEFAULTS, &defaults);
		getspecialcheck_local(ptr, defaults, &defaults);
	}
	pathname_designer_alloc(ptr, defaults, ret, localp);
}

_g void defaults_pathname_heap(Execute ptr, addr *ret, addr defaults)
{
	defaults_pathname_alloc(ptr, ret, defaults, 0);
}

static void parse_pathname_full_alloc(Execute ptr,
		addr thing, addr host, addr defaults, size_t start, size_t end, int junk,
		addr *ret, size_t *pos, int localp)
{
	LocalStack stack;
	struct fileparse pa;

	/* argument */
	defaults_pathname_alloc(ptr, &defaults, defaults, localp);
	init_fileparse(&pa, ptr, localp);
	pa.thing = thing;
	pa.path = defaults;
	pa.host = host;
	pa.start = start;
	pa.end = end;
	pa.junk = junk? 1: 0;

	/* execute */
	push_localp(pa.local, &stack);
	parser_struct_pathname(&pa);
	*ret = pa.result;
	*pos = pa.endpos;
	rollback_localp(pa.local, stack);
}

_g void parse_pathname_full_heap(Execute ptr, addr thing, addr host,
		addr defaults, size_t start, size_t end, int junk, addr *ret, size_t *pos)
{
	parse_pathname_full_alloc(ptr,
			thing, host, defaults, start, end, junk, ret, pos, 0);
}

static void parse_pathname_alloc(Execute ptr, addr thing, addr *ret, int localp)
{
	size_t end;
	string_length(thing, &end);
	parse_pathname_full_alloc(ptr, thing, Nil, Nil, 0, end, 0, ret, &end, localp);
}

static void parse_pathname_heap(Execute ptr, addr thing, addr *ret)
{
	size_t end;
	string_length(thing, &end);
	parse_pathname_full_heap(ptr, thing, Nil, Nil, 0, end, 0, ret, &end);
}

_g void parse_pathname_host_heap(Execute ptr, addr thing, addr host, addr *ret)
{
	size_t end;
	string_length(thing, &end);
	parse_pathname_full_heap(ptr, thing, host, Nil, 0, end, 0, ret, &end);
}

_g void parse_pathname_char_heap(Execute ptr, const char *str, addr *ret)
{
	addr thing;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	strvect_char_local(local, &thing, str);
	parse_pathname_heap(ptr, thing, ret);
	rollback_local(local, stack);
}

_g void pathname_designer_alloc(Execute ptr, addr pos, addr *ret, int localp)
{
	addr value, type;
	LocalRoot local;

	/* pathname */
	local = localp? ptr->local: NULL;
	if (pathnamep(pos)) {
		copylocal_object(local, ret, pos);
		return;
	}

	/* stream */
	if (streamp(pos)) {
		GetPathnameStream(pos, &value);
		if (! pathnamep(value)) {
			GetConst(COMMON_PATHNAME, &type);
			type_error_stdarg(pos, type,
					"The stream ~S does not have a pathname object.", pos, NULL);
		}
		copylocal_object(local, ret, value);
		return;
	}

	/* string */
	if (stringp(pos)) {
		parse_pathname_alloc(ptr, pos, &value, localp);
		if (! pathnamep(value)) {
			GetConst(COMMON_PATHNAME, &type);
			type_error_stdarg(pos, type,
					"The string ~S is not pathname format.", pos, NULL);
		}
		*ret = value;
		return;
	}

	/* type-error */
	TypeError(pos, PATHNAME);
}

_g void pathname_designer_heap(Execute ptr, addr pos, addr *ret)
{
	pathname_designer_alloc(ptr, pos, ret, 0);
}

_g void pathname_designer_local(Execute ptr, addr pos, addr *ret)
{
	pathname_designer_alloc(ptr, pos, ret, 1);
}


/*
 *  physical-pathname
 */
_g void physical_pathname_alloc(Execute ptr, addr pos, addr *ret, int localp)
{
	LocalRoot local;
	addr host, list, left, right, value;

	/* physical pathname */
	local = localp? ptr->local: NULL;
	pathname_designer_alloc(ptr, pos, &pos, localp);
	if (! RefLogicalPathname(pos)) {
		copylocal_object(local, ret, pos);
		return;
	}

	/* logical pathname */
	GetHostPathname(pos, &host);
	if (! gethost_logical_pathname(host, &list))
		fmte("The logical-hostname ~S is not exist.", host, NULL);
	while (list != Nil) {
		GetCons(list, &right, &list);
		List_bind(right, &left, &value, NULL);
		if (wildcard_pathname(pos, left, 1)) {
			translate_pathname_alloc(ptr, ret, pos, left, value, localp);
			return;
		}
	}
	fmte("The logical-pathname ~S don't match translate table.", pos, NULL);
}

_g void physical_pathname_heap(Execute ptr, addr pos, addr *ret)
{
	physical_pathname_alloc(ptr, pos, ret, 0);
}

_g void physical_pathname_local(Execute ptr, addr pos, addr *ret)
{
	physical_pathname_alloc(ptr, pos, ret, 1);
}


/*
 *  file-namestring
 */
static void file_namestring_filename(LocalpRoot local, addr pos, addr queue)
{
	LocalRoot alloc;
	addr right, wild;

	/* name */
	alloc = local->local;
	GetConst(KEYWORD_WILD, &wild);
	GetNamePathname(pos, &right);
	if (right != Nil) {
		if (right == wild)
			pushchar_charqueue_local(alloc, queue, "*");
		else if (right != Nil)
			pushstring_charqueue_local(alloc, queue, right);
	}

	/* type */
	GetTypePathname(pos, &right);
	if (right != Nil) {
		push_charqueue_local(alloc, queue, '.');
		if (right == wild)
			pushchar_charqueue_local(alloc, queue, "*");
		else
			pushstring_charqueue_local(alloc, queue, right);
	}
}

static void file_pathname_namestring(LocalpRoot local, addr *ret, addr pos)
{
	addr host, queue;

	GetHostPathname(pos, &host);
	if (! pathname_system_unix_p(host) && ! pathname_system_windows_p(host))
		fmte("Unknown pathname-host ~S.", host, NULL);

	charqueue_local(local->local, &queue, 0);
	file_namestring_filename(local, pos, queue);
	make_charqueue_alloc(localp_alloc(local), queue, ret);
}

static void logical_namestring_version(LocalpRoot local, addr queue, addr right)
{
	LocalRoot alloc;
	addr wild, newest;

	alloc = local->local;
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_NEWEST, &newest);
	if (right == newest)
		return;
	push_charqueue_local(alloc, queue, '.');
	if (right == wild)
		pushchar_charqueue_local(alloc, queue, "*");
	else if (integerp(right))
		decimal_charqueue_integer_local(alloc, right, queue);
	else
		fmte("Invalid version value ~S.", right, NULL);
}

static void file_logical_namestring(LocalpRoot local, addr *ret, addr pos)
{
	addr queue, right;

	charqueue_local(local->local, &queue, 0);
	file_namestring_filename(local, pos, queue);
	GetVersionPathname(pos, &right);
	if (right != Nil)
		logical_namestring_version(local, queue, right);
	make_charqueue_alloc(localp_alloc(local), queue, ret);
}

static void file_name_pathname_alloc(LocalpRoot local, addr pos, addr *ret)
{
	LocalStack stack;

	push_localp(local, &stack);
	if (RefLogicalPathname(pos))
		file_logical_namestring(local, ret, pos);
	else
		file_pathname_namestring(local, ret, pos);
	rollback_localp(local, stack);
}

_g void file_name_pathname_heap(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 0;
	buffer.local = local;
	file_name_pathname_alloc(&buffer, pos, ret);
}

_g void file_name_pathname_local(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 1;
	buffer.local = local;
	file_name_pathname_alloc(&buffer, pos, ret);
}


/*
 *  directory-namestring
 */
static void directory_namestring_filename(LocalpRoot local,
		addr pos, addr queue, unicode split, int logicalp)
{
	LocalRoot alloc;
	addr left, right, absolute, relative, wild, wildi, up;

	alloc = local->local;
	GetConst(KEYWORD_ABSOLUTE, &absolute);
	GetConst(KEYWORD_RELATIVE, &relative);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wildi);
	GetConst(KEYWORD_UP, &up);

	/* directory */
	GetDirectoryPathname(pos, &right);
	if (! consp(right))
		fmte("Invalid directory ~S.", right, NULL);
	GetCons(right, &left, &right);
	if (left == absolute) {
		if (! logicalp)
			push_charqueue_local(alloc, queue, split);
	}
	else if (left == relative) {
		if (logicalp)
			push_charqueue_local(alloc, queue, split);
	}
	else {
		fmte("Invalid directory type ~S.", left, NULL);
	}
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (left == wild)
			pushchar_charqueue_local(alloc, queue, "*");
		else if (left == wildi)
			pushchar_charqueue_local(alloc, queue, "**");
		else if (left == up)
			pushchar_charqueue_local(alloc, queue, "..");
		else
			pushstring_charqueue_local(alloc, queue, left);
		push_charqueue_local(alloc, queue, split);
	}
}

static void directory_pathname_namestring(LocalpRoot local, addr *ret, addr pos)
{
	addr host, queue;

	charqueue_local(local->local, &queue, 0);
	GetHostPathname(pos, &host);
	if (pathname_system_unix_p(host))
		directory_namestring_filename(local, pos, queue, '/', 0);
	else if (pathname_system_windows_p(host))
		directory_namestring_filename(local, pos, queue, '\\', 0);
	else
		fmte("Unknown pathname-host ~S.", host, NULL);
	make_charqueue_alloc(localp_alloc(local), queue, ret);
}

static void directory_logical_namestring(LocalpRoot local, addr *ret, addr pos)
{
	addr queue;

	charqueue_local(local->local, &queue, 0);
	directory_namestring_filename(local, pos, queue, ';', 1);
	make_charqueue_alloc(localp_alloc(local), queue, ret);
}

static void directory_name_pathname_alloc(LocalpRoot local, addr pos, addr *ret)
{
	LocalStack stack;

	push_localp(local, &stack);
	if (RefLogicalPathname(pos))
		directory_logical_namestring(local, ret, pos);
	else
		directory_pathname_namestring(local, ret, pos);
	rollback_localp(local, stack);
}

_g void directory_name_pathname_heap(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 0;
	buffer.local = local;
	directory_name_pathname_alloc(&buffer, pos, ret);
}

_g void directory_name_pathname_local(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 1;
	buffer.local = local;
	directory_name_pathname_alloc(&buffer, pos, ret);
}


/*
 *  namestring
 */
static void namestring_filename(LocalpRoot local,
		addr pos, addr queue, unicode split, int logicalp)
{
	directory_namestring_filename(local, pos, queue, split, logicalp);
	file_namestring_filename(local, pos, queue);
}

static void namestring_unix(LocalpRoot local, addr *ret, addr pos)
{
	addr queue;

	charqueue_local(local->local, &queue, 0);
	namestring_filename(local, pos, queue, '/', 0);
	make_charqueue_alloc(localp_alloc(local), queue, ret);
}

static void namestring_windows(LocalpRoot local, addr *ret, addr pos)
{
	LocalRoot alloc;
	addr device, queue, universal, file, check;

	alloc = local->local;
	GetConst(SYSTEM_UNIVERSAL, &universal);
	GetConst(SYSTEM_DEVICE, &file);
	GetDevicePathname(pos, &device);
	charqueue_local(alloc, &queue, 0);
	if (device == universal) {
		push_charqueue_local(alloc, queue, '\\');
		namestring_filename(local, pos, queue, '\\', 0);
	}
	else if (device == file) {
		push_charqueue_local(alloc, queue, '\\');
		push_charqueue_local(alloc, queue, '\\');
		push_charqueue_local(alloc, queue, '.');
		push_charqueue_local(alloc, queue, '\\');
		GetNamePathname(pos, &check);
		if (check != Nil)
			pushstring_charqueue_local(alloc, queue, check);
	}
	else if (device != Nil) {
		pushstring_charqueue_local(alloc, queue, device);
		push_charqueue_local(alloc, queue, ':');
		namestring_filename(local, pos, queue, '\\', 0);
	}
	else {
		namestring_filename(local, pos, queue, '\\', 0);
	}
	make_charqueue_alloc(localp_alloc(local), queue, ret);
}

static void pathname_namestring(LocalpRoot local, addr *ret, addr pos)
{
	addr host;

	GetHostPathname(pos, &host);
	if (pathname_system_unix_p(host))
		namestring_unix(local, ret, pos);
	else if (pathname_system_windows_p(host))
		namestring_windows(local, ret, pos);
	else
		fmte("Unknown pathname-host ~S.", host, NULL);
}

static void logical_namestring(LocalpRoot local, addr *ret, addr pos)
{
	LocalRoot alloc;
	addr queue, right;

	alloc = local->local;
	charqueue_local(alloc, &queue, 0);
	/* host */
	GetHostPathname(pos, &right);
	pushstring_charqueue_local(alloc, queue, right);
	pushchar_charqueue_local(alloc, queue, ":");
	/* directory, name, type */
	namestring_filename(local, pos, queue, ';', 1);
	/* version */
	GetVersionPathname(pos, &right);
	if (right != Nil)
		logical_namestring_version(local, queue, right);
	/* result */
	make_charqueue_alloc(localp_alloc(local), queue, ret);
}

static void name_pathname_alloc(Execute ptr, LocalpRoot local, addr pos, addr *ret)
{
	LocalStack stack;

	push_localp(local, &stack);
	pathname_designer_alloc(ptr, pos, &pos, local->localp);
	if (RefLogicalPathname(pos))
		logical_namestring(local, ret, pos);
	else
		pathname_namestring(local, ret, pos);
	rollback_localp(local, stack);
}

_g void name_pathname_heap(Execute ptr, addr pos, addr *ret)
{
	struct localp_struct buffer;

	buffer.localp = 0;
	buffer.local = ptr->local;
	name_pathname_alloc(ptr, &buffer, pos, ret);
}

_g void name_pathname_local(Execute ptr, addr pos, addr *ret)
{
	struct localp_struct buffer;

	buffer.localp = 1;
	buffer.local = ptr->local;
	name_pathname_alloc(ptr, &buffer, pos, ret);
}


/*
 *  merge-pathnames
 */
static void list_merge_directory(addr pos, addr defpath, addr *ret)
{
	addr cons, left;

	cons = Nil;
	while (defpath != Nil) {
		GetCons(defpath, &left, &defpath);
		cons_heap(&cons, left, cons);
	}
	while (pos != Nil) {
		GetCons(pos, &left, &pos);
		cons_heap(&cons, left, cons);
	}
	nreverse(ret, cons);
}

static int check_merge_directory(addr pos, addr defpath)
{
	addr relative;

	if (GetType(pos) == LISPTYPE_CONS && GetType(defpath) == LISPTYPE_CONS) {
		GetCar(pos, &pos);
		GetConst(KEYWORD_RELATIVE, &relative);
		return pos == relative;
	}

	return 0;
}

static void merge_directory(addr pos, addr defpath, addr *ret)
{
	if (check_merge_directory(pos, defpath)) {
		GetCdr(pos, &pos);
		list_merge_directory(pos, defpath, ret);
	}
	else if (pos == Nil) {
		*ret = defpath;
	}
	else {
		*ret = pos;
	}
}

static void merge_pathname_array(addr pos,
		enum PATHNAME_INDEX type, addr defpath, addr *ret)
{
	GetArrayPathname(pos, type, &pos);
	if (pos == Nil) {
		GetArrayPathname(defpath, type, ret);
	}
	else {
		*ret = pos;
	}
}

static void merge_pathname_object(addr pos, addr defaults, addr defver, addr *ret)
{
	addr check1, check2;
	addr host, device, directory, name, type, version;

	host = device = directory = name = type = version = Nil;
	/* If pathname does not specify a host, device, directory, name, or type,
	 * each such component is copied from default-pathname.
	 */
	merge_pathname_array(pos, PATHNAME_INDEX_HOST, defaults, &host);
	merge_pathname_array(pos, PATHNAME_INDEX_DEVICE, defaults, &device);
	merge_pathname_array(pos, PATHNAME_INDEX_NAME, defaults, &name);
	merge_pathname_array(pos, PATHNAME_INDEX_TYPE, defaults, &type);

	/* If pathname does not specify a name, then the version, if not provided,
	 * will come from default-pathname, just like the other components.
	 * if (name == Nil && version == Nil)
	 *    the version copied from defvar.
	 */
	GetNamePathname(pos, &check1);
	GetVersionPathname(pos, &check2);
	if (check1 == Nil && check2 == Nil)
		version = defver;

	/* If pathname does specify a name, then the version is not affected
	 * by default-pathname.
	 *
	 * if (name != Nil)
	 *    the version don't copy from default-pathname.
	 */
	if (check1 != Nil)
		GetVersionPathname(pos, &version);

	/* If this process leaves the version missing, the default-version is used.
	 *
	 * Finally,
	 * if (version == Nil)
	 *   the version copy from default-pathname.
	 */
	if (version == Nil)
		version = defver;

	/* directory */
	GetDirectoryPathname(pos, &check1);
	GetDirectoryPathname(defaults, &check2);
	merge_directory(check1, check2, &directory);

	/* make pathname */
	if (RefLogicalPathname(pos))
		logical_pathname_heap(ret, host, directory, name, type, version);
	else
		pathname_heap(ret, host, device, directory, name, type);
}

_g void merge_pathnames_clang(Execute ptr,
		addr pos, addr defaults, addr defver, addr *ret)
{
	addr host;

	/* logical-pathname namestring */
	defaults_pathname_heap(ptr, &defaults, defaults);
	if (stringp(pos) && RefLogicalPathname(defaults)) {
		GetHostPathname(pos, &host);
		parse_pathname_host_heap(ptr, pos, host, &pos);
	}
	pathname_designer_heap(ptr, pos, &pos);

	/* version */
	if (defver == Unbound)
		GetConst(KEYWORD_NEWEST, &defver);

	/* merge */
	merge_pathname_object(pos, defaults, defver, ret);
}


/*
 *  initialize
 */
_g void init_pathname(void)
{
	init_pathname_common();
}

_g void build_pathname(void)
{
	build_pathname_translate();
}

