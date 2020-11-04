#include "bignum_output.h"
#include "character_queue.h"
#include "condition.h"
#include "constant.h"
#include "copy.h"
#include "cons.h"
#include "cons_list.h"
#include "integer.h"
#include "pathname.h"
#include "pathname_localp.h"
#include "pathname_object.h"
#include "pathname_table.h"
#include "pathname_translate.h"
#include "pathname_wildcard.h"
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
static int parser_struct_pathname_(struct fileparse *pa)
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
	if (pathname_system_unix_p(host))
		return parser_unix_pathname_(pa);

	/* windows */
	if (pathname_system_windows_p(host))
		return parser_windows_pathname_(pa);

	/* logical pathname */
	if (stringp(host))
		return parser_logical_pathname_(pa);

	/* error */
	return fmte_("Unknown pathname-host ~S.", host, NULL);
}

static int defaults_pathname_alloc_(Execute ptr, addr *ret, addr defaults, int localp)
{
	if (defaults == Nil || defaults == Unbound) {
		GetConst(SPECIAL_DEFAULT_PATHNAME_DEFAULTS, &defaults);
		Return(getspecialcheck_local_(ptr, defaults, &defaults));
	}
	return pathname_designer_alloc_(ptr, defaults, ret, localp);
}

_g int defaults_pathname_heap_(Execute ptr, addr *ret, addr defaults)
{
	return defaults_pathname_alloc_(ptr, ret, defaults, 0);
}

static int parse_pathname_full_alloc_(Execute ptr,
		addr thing, addr host, addr defaults, size_t start, size_t end, int junk,
		addr *ret, size_t *pos, int localp, int errorp)
{
	LocalStack stack;
	struct fileparse pa;

	/* argument */
	Return(defaults_pathname_alloc_(ptr, &defaults, defaults, localp));
	init_fileparse(&pa, ptr, localp);
	pa.thing = thing;
	pa.path = defaults;
	pa.host = host;
	pa.start = start;
	pa.end = end;
	pa.junk = junk? 1: 0;
	pa.errorp = errorp;

	/* logical-pathname */
	if (host == Nil && pathname_logical_p(defaults)) {
		GetHostPathname(defaults, &host);
		pa.host = host;
	}

	/* execute */
	push_localp(pa.local, &stack);
	Return(parser_struct_pathname_(&pa));
	*ret = pa.result;
	*pos = pa.endpos;
	rollback_localp(pa.local, stack);

	return 0;
}

_g int parse_pathname_full_heap_(Execute ptr, addr thing, addr host,
		addr defaults, size_t start, size_t end, int junk, addr *ret, size_t *pos)
{
	return parse_pathname_full_alloc_(ptr,
			thing, host, defaults, start, end, junk, ret, pos, 0, 1);
}

static int parse_pathname_alloc_(Execute ptr, addr thing, addr *ret, int localp)
{
	size_t end;
	string_length(thing, &end);
	return parse_pathname_full_alloc_(ptr,
			thing, Nil, Nil, 0, end, 0, ret, &end, localp, 1);
}

static int parse_pathname_heap_(Execute ptr, addr thing, addr *ret)
{
	size_t end;
	string_length(thing, &end);
	return parse_pathname_full_heap_(ptr, thing, Nil, Nil, 0, end, 0, ret, &end);
}

static int parse_pathname_host_heap_(Execute ptr, addr thing, addr host, addr *ret)
{
	size_t end;
	string_length(thing, &end);
	return parse_pathname_full_heap_(ptr, thing, host, Nil, 0, end, 0, ret, &end);
}

_g int parse_pathname_setf_heap_(Execute ptr, addr thing, addr host, addr *ret)
{
	size_t end;
	string_length(thing, &end);
	return parse_pathname_full_alloc_(ptr,
			thing, host, Nil, 0, end, 0, ret, &end, 0, 0);
}

_g int parse_pathname_char_heap_(Execute ptr, const char *str, addr *ret)
{
	addr thing;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	strvect_char_local(local, &thing, str);
	Return(parse_pathname_heap_(ptr, thing, ret));
	rollback_local(local, stack);

	return 0;
}

_g int pathname_designer_alloc_(Execute ptr, addr pos, addr *ret, int localp)
{
	addr value, type;
	LocalRoot local;

	/* pathname */
	local = localp? ptr->local: NULL;
	if (pathnamep(pos)) {
		copylocal_object(local, ret, pos);
		return 0;
	}

	/* stream */
	if (streamp(pos)) {
		GetPathnameStream(pos, &value);
		if (! pathnamep(value)) {
			GetConst(COMMON_PATHNAME, &type);
			return call_type_error_va_(ptr, pos, type,
					"The stream ~S does not have a pathname object.", pos, NULL);
		}
		copylocal_object(local, ret, value);
		return 0;
	}

	/* string */
	if (stringp(pos)) {
		Return(parse_pathname_alloc_(ptr, pos, &value, localp));
		if (! pathnamep(value)) {
			GetConst(COMMON_PATHNAME, &type);
			return call_type_error_va_(ptr, pos, type,
					"The string ~S is not pathname format.", pos, NULL);
		}
		return Result(ret, value);
	}

	/* type-error */
	return TypeError_(pos, PATHNAME);
}

_g int pathname_designer_heap_(Execute ptr, addr pos, addr *ret)
{
	return pathname_designer_alloc_(ptr, pos, ret, 0);
}

_g int pathname_designer_local_(Execute ptr, addr pos, addr *ret)
{
	return pathname_designer_alloc_(ptr, pos, ret, 1);
}


/*
 *  physical-pathname
 */
_g int physical_pathname_alloc_(Execute ptr, addr pos, addr *ret, int localp)
{
	int check;
	LocalRoot local;
	addr host, list, left, right, value;

	/* physical pathname */
	local = localp? ptr->local: NULL;
	Return(pathname_designer_alloc_(ptr, pos, &pos, localp));
	if (! RefLogicalPathname(pos)) {
		copylocal_object(local, ret, pos);
		return 0;
	}

	/* logical pathname */
	GetHostPathname(pos, &host);
	Return(gethost_logical_pathname_(host, &list));
	if (list == Nil)
		return fmte_("The logical-hostname ~S is not exist.", host, NULL);
	while (list != Nil) {
		GetCons(list, &right, &list);
		List_bind(right, &left, &value, NULL);
		Return(wildcard_pathname_(pos, left, 1, &check));
		if (check)
			return translate_pathname_alloc_(ptr, ret, pos, left, value, localp);
	}
	return fmte_("The logical-pathname ~S don't match translate table.", pos, NULL);
}

_g int physical_pathname_heap_(Execute ptr, addr pos, addr *ret)
{
	return physical_pathname_alloc_(ptr, pos, ret, 0);
}

_g int physical_pathname_local_(Execute ptr, addr pos, addr *ret)
{
	return physical_pathname_alloc_(ptr, pos, ret, 1);
}


/*
 *  file-namestring
 */
static int file_namestring_filename_(LocalpRoot local, addr pos, addr queue)
{
	LocalRoot alloc;
	addr right, wild;

	/* name */
	alloc = local->local;
	GetConst(KEYWORD_WILD, &wild);
	GetNamePathname(pos, &right);
	if (right != Nil) {
		if (right == wild) {
			Return(pushchar_charqueue_local_(alloc, queue, "*"));
		}
		else if (right != Nil) {
			Return(pushstring_charqueue_local_(alloc, queue, right));
		}
	}

	/* type */
	GetTypePathname(pos, &right);
	if (right != Nil) {
		Return(push_charqueue_local_(alloc, queue, '.'));
		if (right == wild) {
			Return(pushchar_charqueue_local_(alloc, queue, "*"));
		}
		else {
			Return(pushstring_charqueue_local_(alloc, queue, right));
		}
	}

	return 0;
}

static int file_pathname_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	addr host, queue;

	GetHostPathname(pos, &host);
	if (! pathname_system_unix_p(host) && ! pathname_system_windows_p(host))
		return fmte_("Unknown pathname-host ~S.", host, NULL);

	charqueue_local(local->local, &queue, 0);
	Return(file_namestring_filename_(local, pos, queue));
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int logical_namestring_version_(LocalpRoot local, addr queue, addr right)
{
	LocalRoot alloc;
	addr wild, newest;

	alloc = local->local;
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_NEWEST, &newest);
	if (right == newest)
		return 0;
	Return(push_charqueue_local_(alloc, queue, '.'));
	if (right == wild)
		return pushchar_charqueue_local_(alloc, queue, "*");
	else if (integerp(right))
		return decimal_charqueue_integer_local_(alloc, right, queue);
	else
		return fmte_("Invalid version value ~S.", right, NULL);
}

static int file_logical_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	addr queue, right;

	charqueue_local(local->local, &queue, 0);
	Return(file_namestring_filename_(local, pos, queue));
	GetVersionPathname(pos, &right);
	if (right != Nil) {
		Return(logical_namestring_version_(local, queue, right));
	}
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int file_name_pathname_alloc_(LocalpRoot local, addr pos, addr *ret)
{
	LocalStack stack;

	push_localp(local, &stack);
	if (RefLogicalPathname(pos)) {
		Return(file_logical_namestring_(local, ret, pos));
	}
	else {
		Return(file_pathname_namestring_(local, ret, pos));
	}
	rollback_localp(local, stack);

	return 0;
}

_g int file_name_pathname_heap_(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 0;
	buffer.local = local;
	return file_name_pathname_alloc_(&buffer, pos, ret);
}

_g int file_name_pathname_local_(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 1;
	buffer.local = local;
	return file_name_pathname_alloc_(&buffer, pos, ret);
}


/*
 *  directory-namestring
 */
static int directory_namestring_filename_(LocalpRoot local,
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
		return fmte_("Invalid directory ~S.", right, NULL);
	GetCons(right, &left, &right);
	if (left == absolute) {
		if (! logicalp) {
			Return(push_charqueue_local_(alloc, queue, split));
		}
	}
	else if (left == relative) {
		if (logicalp) {
			Return(push_charqueue_local_(alloc, queue, split));
		}
	}
	else {
		return fmte_("Invalid directory type ~S.", left, NULL);
	}
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (left == wild) {
			Return(pushchar_charqueue_local_(alloc, queue, "*"));
		}
		else if (left == wildi) {
			Return(pushchar_charqueue_local_(alloc, queue, "**"));
		}
		else if (left == up) {
			Return(pushchar_charqueue_local_(alloc, queue, ".."));
		}
		else {
			Return(pushstring_charqueue_local_(alloc, queue, left));
		}
		Return(push_charqueue_local_(alloc, queue, split));
	}

	return 0;
}

static int directory_pathname_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	addr host, queue;

	charqueue_local(local->local, &queue, 0);
	GetHostPathname(pos, &host);
	if (pathname_system_unix_p(host)) {
		Return(directory_namestring_filename_(local, pos, queue, '/', 0));
	}
	else if (pathname_system_windows_p(host)) {
		Return(directory_namestring_filename_(local, pos, queue, '\\', 0));
	}
	else {
		return fmte_("Unknown pathname-host ~S.", host, NULL);
	}
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int directory_logical_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	addr queue;

	charqueue_local(local->local, &queue, 0);
	Return(directory_namestring_filename_(local, pos, queue, ';', 1));
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int directory_name_pathname_alloc_(LocalpRoot local, addr pos, addr *ret)
{
	LocalStack stack;

	push_localp(local, &stack);
	if (RefLogicalPathname(pos)) {
		Return(directory_logical_namestring_(local, ret, pos));
	}
	else {
		Return(directory_pathname_namestring_(local, ret, pos));
	}
	rollback_localp(local, stack);

	return 0;
}

_g int directory_name_pathname_heap_(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 0;
	buffer.local = local;
	return directory_name_pathname_alloc_(&buffer, pos, ret);
}

_g int directory_name_pathname_local_(LocalRoot local, addr pos, addr *ret)
{
	struct localp_struct buffer;

	Check(local == NULL, "local error");
	buffer.localp = 1;
	buffer.local = local;
	return directory_name_pathname_alloc_(&buffer, pos, ret);
}


/*
 *  namestring
 */
static int namestring_filename_(LocalpRoot local,
		addr pos, addr queue, unicode split, int logicalp)
{
	Return(directory_namestring_filename_(local, pos, queue, split, logicalp));
	return file_namestring_filename_(local, pos, queue);
}

static int namestring_unix_(LocalpRoot local, addr *ret, addr pos)
{
	addr queue;

	charqueue_local(local->local, &queue, 0);
	Return(namestring_filename_(local, pos, queue, '/', 0));
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int namestring_windows_(LocalpRoot local, addr *ret, addr pos)
{
	LocalRoot alloc;
	addr device, queue, universal, file, check;

	alloc = local->local;
	GetConst(SYSTEM_UNIVERSAL, &universal);
	GetConst(SYSTEM_DEVICE, &file);
	GetDevicePathname(pos, &device);
	charqueue_local(alloc, &queue, 0);
	if (device == universal) {
		Return(push_charqueue_local_(alloc, queue, '\\'));
		Return(namestring_filename_(local, pos, queue, '\\', 0));
	}
	else if (device == file) {
		Return(push_charqueue_local_(alloc, queue, '\\'));
		Return(push_charqueue_local_(alloc, queue, '\\'));
		Return(push_charqueue_local_(alloc, queue, '.'));
		Return(push_charqueue_local_(alloc, queue, '\\'));
		GetNamePathname(pos, &check);
		if (check != Nil) {
			Return(pushstring_charqueue_local_(alloc, queue, check));
		}
	}
	else if (device != Nil) {
		Return(pushstring_charqueue_local_(alloc, queue, device));
		Return(push_charqueue_local_(alloc, queue, ':'));
		Return(namestring_filename_(local, pos, queue, '\\', 0));
	}
	else {
		Return(namestring_filename_(local, pos, queue, '\\', 0));
	}
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int pathname_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	addr host;

	GetHostPathname(pos, &host);
	if (pathname_system_unix_p(host))
		return namestring_unix_(local, ret, pos);
	else if (pathname_system_windows_p(host))
		return namestring_windows_(local, ret, pos);
	else
		return fmte_("Unknown pathname-host ~S.", host, NULL);
}

static int logical_namestring_(LocalpRoot local, addr *ret, addr pos)
{
	LocalRoot alloc;
	addr queue, right;

	alloc = local->local;
	charqueue_local(alloc, &queue, 0);
	/* host */
	GetHostPathname(pos, &right);
	Return(pushstring_charqueue_local_(alloc, queue, right));
	Return(pushchar_charqueue_local_(alloc, queue, ":"));
	/* directory, name, type */
	Return(namestring_filename_(local, pos, queue, ';', 1));
	/* version */
	GetVersionPathname(pos, &right);
	if (right != Nil) {
		Return(logical_namestring_version_(local, queue, right));
	}
	/* result */
	make_charqueue_alloc(localp_alloc(local), queue, ret);

	return 0;
}

static int name_pathname_alloc_(Execute ptr, LocalpRoot local, addr pos, addr *ret)
{
	LocalStack stack;

	push_localp(local, &stack);
	Return(pathname_designer_alloc_(ptr, pos, &pos, local->localp));
	if (RefLogicalPathname(pos)) {
		Return(logical_namestring_(local, ret, pos));
	}
	else {
		Return(pathname_namestring_(local, ret, pos));
	}
	rollback_localp(local, stack);

	return 0;
}

_g int name_pathname_heap_(Execute ptr, addr pos, addr *ret)
{
	struct localp_struct buffer;

	buffer.localp = 0;
	buffer.local = ptr->local;
	return name_pathname_alloc_(ptr, &buffer, pos, ret);
}

_g int name_pathname_local_(Execute ptr, addr pos, addr *ret)
{
	struct localp_struct buffer;

	buffer.localp = 1;
	buffer.local = ptr->local;
	return name_pathname_alloc_(ptr, &buffer, pos, ret);
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

_g int merge_pathnames_clang_(Execute ptr,
		addr pos, addr defaults, addr defver, addr *ret)
{
	addr host;

	/* logical-pathname namestring */
	Return(defaults_pathname_heap_(ptr, &defaults, defaults));
	if (stringp(pos) && RefLogicalPathname(defaults)) {
		GetHostPathname(pos, &host);
		Return(parse_pathname_host_heap_(ptr, pos, host, &pos));
	}
	Return(pathname_designer_heap_(ptr, pos, &pos));

	/* version */
	if (defver == Unbound)
		GetConst(KEYWORD_NEWEST, &defver);

	/* merge */
	merge_pathname_object(pos, defaults, defver, ret);

	return 0;
}


/*
 *  initialize
 */
_g void build_pathname(void)
{
	build_pathname_translate();
}

