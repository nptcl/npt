#include "bignum.h"
#include "build.h"
#include "character.h"
#include "charqueue.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "copy.h"
#include "equal.h"
#include "files.h"
#include "function.h"
#include "hashtable.h"
#include "heap.h"
#include "input.h"
#include "integer.h"
#include "local.h"
#include "memory.h"
#include "number.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "pointer.h"
#include "reader.h"
#include "sequence.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "thread.h"

/*  pathname
 *    host -> unix, windows, "logical"
 *    [unix]
 *      device = nil
 *      version = :unspecific
 *    [windows]
 *      device = (universal | device | "[A-Z]" | Nil)
 *      version = :unspecific
 *    [logical]
 *      device = :unspecific
 *      version = (or null integer)
 */

/*
 *  logical-pathname table
 */
_g void build_pathname(void)
{
	addr value, symbol;

	/* logical-pathname */
	hashtable_heap(&value);
	settest_hashtable(value, HASHTABLE_TEST_EQUALP);
	GetConst(SYSTEM_LOGICAL_PATHNAME, &symbol);
	SetValueSymbol(symbol, value);

	/* universal time */
	value = intsizeh((70ULL*365 + 17ULL) * 24 * 60 * 60);
	GetConst(SYSTEM_TIME1970, &symbol);
	SetValueSymbol(symbol, value);
}

static void table_logical_pathname(addr *ret)
{
	addr symbol;
	GetConst(SYSTEM_LOGICAL_PATHNAME, &symbol);
	GetValueSymbol(symbol, ret);
}

_g void sethost_pathname(addr key, addr value)
{
	addr list;

	table_logical_pathname(&list);
	intern_hashheap(list, key, &list);
	SetCdr(list, value);
}

/* found=1, notfound=0 */
_g int gethost_pathname(addr key, addr *ret)
{
	addr list;
	table_logical_pathname(&list);
	return findvalue_hashtable(list, key, ret);
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
	SetPathname(pos, PATHNAME_INDEX_HOST, host);
	SetPathname(pos, PATHNAME_INDEX_DEVICE, device);
	SetPathname(pos, PATHNAME_INDEX_DIRECTORY, directory);
	SetPathname(pos, PATHNAME_INDEX_NAME, name);
	SetPathname(pos, PATHNAME_INDEX_TYPE, type);
	SetPathname(pos, PATHNAME_INDEX_VERSION, version);
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
	GetPathname(pos, PATHNAME_INDEX_NAME, &check);
	return check != Nil;
}

_g int pathname_directory_p(addr pos)
{
	addr name, type;

	if (! pathnamep(pos))
		return 0;
	GetPathname(pos, PATHNAME_INDEX_NAME, &name);
	GetPathname(pos, PATHNAME_INDEX_TYPE, &type);

	return name == Nil && type == Nil;
}

_g void copylocal_pathname_array(LocalRoot local, addr a, int i, addr b)
{
	addr value;

	GetPathname(a, i, &value);
	copylocal_object(local, &value, value);
	SetPathname(b, i, value);
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

#ifdef LISP_PATHNAME_EQUALP
#define LispPathnameEqual equalp_function
#else
#define LispPathnameEqual equal_function
#endif
_g int pathname_equal(addr a, addr b)
{
	addr check1, check2;

	Check(! pathnamep(a), "type left error");
	Check(! pathnamep(b), "type right error");

	if (RefLogicalPathname(a) != RefLogicalPathname(b)) return 0;
	/* host */
	GetPathname(a, PATHNAME_INDEX_HOST, &check1);
	GetPathname(b, PATHNAME_INDEX_HOST, &check2);
	if (! equalp_function(check1, check2)) return 0;
	/* device */
	GetPathname(a, PATHNAME_INDEX_DEVICE, &check1);
	GetPathname(b, PATHNAME_INDEX_DEVICE, &check2);
	if (! LispPathnameEqual(check1, check2)) return 0;
	/* directory */
	GetPathname(a, PATHNAME_INDEX_DIRECTORY, &check1);
	GetPathname(b, PATHNAME_INDEX_DIRECTORY, &check2);
	if (! LispPathnameEqual(check1, check2)) return 0;
	/* name */
	GetPathname(a, PATHNAME_INDEX_NAME, &check1);
	GetPathname(b, PATHNAME_INDEX_NAME, &check2);
	if (! LispPathnameEqual(check1, check2)) return 0;
	/* type */
	GetPathname(a, PATHNAME_INDEX_TYPE, &check1);
	GetPathname(b, PATHNAME_INDEX_TYPE, &check2);
	if (! LispPathnameEqual(check1, check2)) return 0;
	/* version */
	GetPathname(a, PATHNAME_INDEX_VERSION, &check1);
	GetPathname(b, PATHNAME_INDEX_VERSION, &check2);
	if (! eql_function(check1, check2)) return 0;

	return 1;
}


/*
 *  localp
 */
struct localp_struct {
	unsigned localp : 1;
	LocalRoot local;
};
typedef struct localp_struct *LocalpRoot;

static void push_localp(LocalpRoot local, LocalStack *ret)
{
	if (local->localp)
		*ret = NULL;
	else
		push_local(local->local, ret);
}

static void rollback_localp(LocalpRoot local, LocalStack stack)
{
	if (! local->localp)
		rollback_local(local->local, stack);
}

static LocalRoot localp_alloc(LocalpRoot local)
{
	return local->localp? local->local: NULL;
}


/*
 *  parser
 */
struct fileparse {
	unsigned junk : 1;
	unsigned force_host : 1;
	Execute ptr;
	LocalpRoot local;
	addr thing, path, queue, result;
	addr host, device, directory, name, type, version;
	size_t start, end, endpos;
	struct localp_struct local_buffer;
};

static void init_fileparse(struct fileparse *pa, Execute ptr, int localp)
{
	clearpoint(pa);
	pa->thing = pa->path = pa->queue = pa->result =
		pa->host = pa->device = pa->directory =
		pa->name = pa->type = pa->version = Nil;
	pa->ptr = ptr;
	pa->local_buffer.local = ptr->local;
	pa->local_buffer.localp = localp? 1: 0;
	pa->local = &pa->local_buffer;
}

static void pathname_fileparse_alloc(struct fileparse *pa, int logical)
{
	LocalRoot local;

	local = localp_alloc(pa->local);
	if (logical) {
		logical_pathname_alloc(local, &pa->result,
				pa->host, pa->directory, pa->name, pa->type, pa->version);
	}
	else {
		pathname_alloc(local, &pa->result,
				pa->host, pa->device, pa->directory, pa->name, pa->type);
	}
}

static void pushconstant_fileparse(struct fileparse *pa, addr *list, constindex index)
{
	addr value;
	GetConstant(index, &value);
	cons_alloc(localp_alloc(pa->local), list, value, *list);
}

static int stringp_equal_char(addr left, const char *right)
{
	return stringp(left) && string_equal_char(left, right);
}

static int stringp_equalp_char(addr left, const char *right)
{
	return stringp(left) && string_equalp_char(left, right);
}

static void wild_value_pathname(addr input, addr *ret)
{
	if (stringp_equal_char(input, "*")) {
		GetConst(KEYWORD_WILD, ret);
	}
	else {
		*ret = input;
	}
}

static void wild_newest_value_pathname(addr input, addr *ret)
{
	if (stringp_equal_char(input, "*")) {
		GetConst(KEYWORD_WILD, ret);
	}
	else if (stringp_equalp_char(input, "NEWEST")) {
		GetConst(KEYWORD_NEWEST, ret);
	}
	else {
		*ret = input;
	}
}

static void check_asterisk_logical_pathname(addr pos)
{
	unicode a, b;
	size_t size, i;

	if (! stringp(pos)) return;
	string_length(pos, &size);
	a = 0;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &b);
		if (a == '*' && b == '*')
			fmte("Invalid wildcard string ~S.", pos, NULL);
	}
}

static void check_version_logical_pathname(addr pos)
{
	unicode c;
	size_t size, i;

	if (! stringp(pos)) return;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (! isDigitCase(c))
			fmte(":VERSION ~S must be a positive integer.", pos, NULL);
	}
}

static void check_parse_logical_pathname(struct fileparse *pa)
{
	int check;
	addr list, pos;
	size_t size;

	/* host */
	string_length(pa->host, &size);
	if (size == 0)
		fmte("Invalid host name ~S.", pa->host, NULL);

	/* directory */
	for (list = pa->directory; list != Nil; ) {
		GetCons(list, &pos, &list);
		check_asterisk_logical_pathname(pos);
	}
	/* name */
	check_asterisk_logical_pathname(pa->name);
	/* type */
	check_asterisk_logical_pathname(pa->type);
	/* version */
	pos = pa->version;
	check_version_logical_pathname(pos);
	if (stringp(pos)) {
		if (read_from_string(pa->ptr, &check, &pos, pos))
			fmte("Cannot read ~S object.", pa->version, NULL);
		if (check)
			fmte("Cannot read ~S object.", pa->version, NULL);
		if (! integerp(pos))
			fmte("Invalid version object ~S.", pos, NULL);
		pa->version = pos;
	}
}

static void make_parse_logical_pathname(struct fileparse *pa)
{
	GetConst(KEYWORD_UNSPECIFIC, &pa->device);
	wild_value_pathname(pa->name, &pa->name);
	wild_value_pathname(pa->type, &pa->type);
	wild_newest_value_pathname(pa->version, &pa->version);
	if (pa->host == Nil)
		fmte("No logical-pathname host.", NULL);
	check_parse_logical_pathname(pa);
	pathname_fileparse_alloc(pa, 1);
}

static void pushrange_pathname(LocalpRoot local,
		addr queue, const unicode *body, size_t n1, size_t n2)
{
	unicode c;

	for (; n1 < n2; n1++) {
		string_getdirect(body, n1, &c);
		push_charqueue_local(local->local, queue, c);
	}
}

static void make_charqueue_fileparse(struct fileparse *pa, addr queue, addr *ret)
{
	make_charqueue_alloc(localp_alloc(pa->local), queue, ret);
}

static void nametype_pathname(struct fileparse *pa, size_t index)
{
	/*
	 *  index -> 1..size-1
	 *    "a.bc" -> ("a" "bc")
	 *    "abc." -> ("abc" "")   not nil
	 */
	addr pos, queue;
	const unicode *body;
	size_t size;
	LocalpRoot local;

	local = pa->local;
	queue = pa->queue;
	pos = pa->name;
	string_length(pos, &size);
	GetStringUnicode(pos, &body);
	Check(size <= index, "length error");

	/* name */
	clear_charqueue(queue);
	pushrange_pathname(local, queue, body, 0, index);
	make_charqueue_fileparse(pa, queue, &pa->name);
	/* type */
	clear_charqueue(queue);
	pushrange_pathname(local, queue, body, index + 1, size);
	make_charqueue_fileparse(pa, queue, &pa->type);
}

static void pushdirectory_fileparse(struct fileparse *pa, addr *list, addr name)
{
	/*
	 *  ".." -> :UP
	 *  "*"  -> :WILD
	 *  "**" -> :WILD-INFERIORS
	 */
	if (stringp_equal_char(name, "..")) {
		GetConst(KEYWORD_UP, &name);
	}
	else if (stringp_equal_char(name, "*")) {
		GetConst(KEYWORD_WILD, &name);
	}
	else if (stringp_equal_char(name, "**")) {
		GetConst(KEYWORD_WILD_INFERIORS, &name);
	}
	cons_alloc(localp_alloc(pa->local), list, name, *list);
}

/* parser-logical */
static int check_host_logical_pathname(LocalpRoot local, addr queue)
{
	addr key;
	make_charqueue_local(local->local, queue, &key);
	return gethost_pathname(key, &key);
}

static int check_drive_logical_pathname(LocalpRoot local, int drive)
{
	addr key;
	strvect_local(local->local, &key, 1);
	strvect_setc(key, 0, drive);
	return gethost_pathname(key, &key);
}

/* parser-unix */
static void make_unix_pathname(struct fileparse *pa)
{
	GetConst(SYSTEM_UNIX, &pa->host);
	GetPathname(pa->path, PATHNAME_INDEX_DEVICE, &pa->device);
	wild_value_pathname(pa->name, &pa->name);
	wild_value_pathname(pa->type, &pa->type);
	GetPathname(pa->path, PATHNAME_INDEX_VERSION, &pa->version);
	pathname_fileparse_alloc(pa, 0);
}

/* parser-windows */
static void make_windows_pathname(struct fileparse *pa)
{
	GetConst(SYSTEM_WINDOWS, &pa->host);
	wild_value_pathname(pa->name, &pa->name);
	wild_value_pathname(pa->type, &pa->type);
	GetPathname(pa->path, PATHNAME_INDEX_VERSION, &pa->version);
	pathname_fileparse_alloc(pa, 0);
}

static int backslash_backslash_pathname(const unicode *body, size_t size)
{
	unicode c;

	if (size < 2) return 0;
	string_getdirect(body, 0, &c);
	if (c != '\\') return 0;
	string_getdirect(body, 1, &c);

	return (c == '\\');
}

static int charbackslash_pathname(const unicode *body, size_t size, unicode type)
{
	unicode c;

	if (size < 4) return 0;
	string_getdirect(body, 2, &c);
	if (c != type) return 0;
	string_getdirect(body, 3, &c);

	return (c == '\\');
}

static int check_unc_pathname(const unicode *body, size_t size)
{
	unicode c;

	if (size < 2) return 0;
	string_getdirect(body, 2, &c);

	return (c != '\\');
}

static int check_drive_pathname(const unicode *body,
		size_t size, size_t i, unicode *ret)
{
	unicode c, drive;

	if ((size - i) < 2) return 0;
	string_getdirect(body, i, &drive);
	if (! isAlphabetic(drive)) return 0;
	string_getdirect(body, i + 1, &c);
	if (c != ':') return 0;
	*ret = drive;

	return 1;
}

static void make_drive_pathname(struct fileparse *pa, unicode c)
{
	addr pos;
	strvect_alloc(localp_alloc(pa->local), &pos, 1);
	strvect_setc(pos, 0, c);
	pa->device = pos;
}

static void make_device_windows_pathname(struct fileparse *pa,
		const unicode *body, size_t size, size_t i)
{
	addr queue;

	/* drive */
	GetConst(SYSTEM_DEVICE, &pa->device);
	/* name */
	queue = pa->queue;
	pushrange_pathname(pa->local, queue, body, i, size);
	make_charqueue_fileparse(pa, queue, &pa->name);
	clear_charqueue(queue);
}


/*
 *  parser table
 */
static int logical_word_p(unicode c)
{
	return isAlphanumeric(c) || (c == '-');
}

static int logical_wordaster_p(unicode c)
{
	return logical_word_p(c) || (c == '*');
}

static void parser_logical_pathname(struct fileparse *pa)
{
	int absolute, relative, dp1, dp2;
	unicode c;
	LocalpRoot local;
	const unicode *body;
	addr charqueue, queue, thing, temp;
	size_t i, size;

	/* initialize */
	local = pa->local;
	thing = pa->thing;
	if (! stringp(thing))
		TypeError(thing, STRING);
	charqueue_local(local->local, &charqueue, 0);
	pa->queue = charqueue;
	queue = Nil;
	size = pa->end;
	GetStringUnicode(thing, &body);
	absolute = relative = dp1 = dp2 = 0;
	i = pa->start;
	GetConst(KEYWORD_NEWEST, &pa->version);

	/* start */
	if (size == 0) goto finish;
	string_getdirect(body, i++, &c);
	if (logical_wordaster_p(c)) goto next1;
	if (c == ';') {
		relative = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_RELATIVE);
		goto discard;
	}
	if (c == '.') goto dot_type2;
	goto error;

host:
	make_charqueue_fileparse(pa, charqueue, &pa->host);
	clear_charqueue(charqueue);
	if (size <= i) goto finish;
	string_getdirect(body, i++, &c);
	if (logical_wordaster_p(c)) goto next2;
	if (c == ';') {
		relative = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_RELATIVE);
		goto discard;
	}
	if (c == '.') goto dot_type2;
	goto error;

next1:
	push_charqueue_local(local->local, charqueue, c);
	if (size <= i) goto finish_name;
	string_getdirect(body, i++, &c);
	if (logical_wordaster_p(c)) goto next1;
	if (c == ';') goto path;
	if (c == ':') goto host;
	if (c == '.') goto dot_type1;
	goto error;

path:
	if (relative == 0 && absolute == 0) {
		absolute = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_ABSOLUTE);
	}
	make_charqueue_fileparse(pa, charqueue, &temp);
	clear_charqueue(charqueue);
	pushdirectory_fileparse(pa, &queue, temp);

discard:
	if (size <= i) goto finish;
	string_getdirect(body, i++, &c);
	if (logical_wordaster_p(c)) goto next2;
	if (c == ';') goto discard;
	if (c == '.') goto dot_type2;
	goto error;

next2:
	push_charqueue_local(local->local, charqueue, c);
	if (size <= i) goto finish_name;
	string_getdirect(body, i++, &c);
	if (logical_wordaster_p(c)) goto next2;
	if (c == ';') goto path;
	if (c == '.') goto dot_type1;
	goto error;

dot_type1:
	make_charqueue_fileparse(pa, charqueue, &pa->name);
	clear_charqueue(charqueue);

dot_type2:
	if (size <= i) goto finish_type;
	string_getdirect(body, i++, &c);
	if (logical_wordaster_p(c)) {
		push_charqueue_local(local->local, charqueue, c);
		goto dot_type2;
	}
	if (c == '.') goto dot_version1;
	goto error;

dot_version1:
	make_charqueue_fileparse(pa, charqueue, &pa->type);
	clear_charqueue(charqueue);

dot_version2:
	if (size <= i) goto finish_version;
	string_getdirect(body, i++, &c);
	if (logical_wordaster_p(c)) {
		push_charqueue_local(local->local, charqueue, c);
		goto dot_version2;
	}
	goto error;

finish_name:
	make_charqueue_fileparse(pa, charqueue, &pa->name);
	goto finish;

finish_type:
	make_charqueue_fileparse(pa, charqueue, &pa->type);
	goto finish;

finish_version:
	make_charqueue_fileparse(pa, charqueue, &pa->version);
	goto finish;

finish:
	nreverse_list_unsafe(&pa->directory, queue);
	pa->endpos = i;
	make_parse_logical_pathname(pa);
	return;

error:
	fmte("Invalid logical-pathname ~S.", thing, NULL);
}

static void parser_unix_pathname(struct fileparse *pa)
{
	int absolute, relative, logical, dp;
	unicode c;
	LocalpRoot local;
	const unicode *body;
	addr charqueue, queue, thing, temp;
	size_t i, di, ni, size;
	struct fileparse backup;

	/* initialize */
	backup = *pa;
	local = pa->local;
	thing = pa->thing;
	if (! stringp(thing))
		TypeError(thing, STRING);
	charqueue_local(local->local, &charqueue, 0);
	pa->queue = charqueue;
	queue = Nil;
	size = pa->end;
	GetStringUnicode(thing, &body);
	absolute = relative = logical = dp = 0;
	di = ni = 0;
	i = pa->start;

	/* start */
	if (size == 0) goto finish;
	string_getdirect(body, i++, &c);
	if (c == '/') {
		absolute = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_ABSOLUTE);
		goto first;
	}
	if (c == '.') { di = ni; dp = 1; }
	if (c == ':') { logical = 1; }
	push_charqueue_local(local->local, charqueue, c);
	ni++;
	goto next1;

first:
	if (size <= i) goto finish;
	string_getdirect(body, i++, &c);
	if (c == '/') goto first;
	if (c == '.') { di = ni; dp = 1; }
	push_charqueue_local(local->local, charqueue, c);
	ni++;
	goto next1;

next1:
	if (size <= i) goto name_finish;
	string_getdirect(body, i++, &c);
	if (c == '/') goto next2;
	if (c == '.') { di = ni; dp = 1; }
	if (logical == 0 && c == ':') {
		if (check_host_logical_pathname(local, charqueue)) {
			/* parser logical */
			*pa = backup;
			parser_logical_pathname(pa);
			return;
		}
		logical = 1;
	}
	push_charqueue_local(local->local, charqueue, c);
	ni++;
	goto next1;

next2:
	if (absolute == 0 && relative == 0) {
		relative = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_RELATIVE);
	}
	make_charqueue_fileparse(pa, charqueue, &temp);
	clear_charqueue(charqueue);
	pushdirectory_fileparse(pa, &queue, temp);
	ni = di = dp = 0;
	goto first;

name_finish:
	make_charqueue_fileparse(pa, charqueue, &pa->name);
	if (di && dp)
		nametype_pathname(pa, di);
	goto finish;

finish:
	nreverse_list_unsafe(&pa->directory, queue);
	pa->endpos = i;
	make_unix_pathname(pa);
}

#define slashp(x)  ((x) == '/' || (x) == '\\')
static void parser_windows_pathname(struct fileparse *pa)
{
	int absolute, relative, universal, question, win32device, logical, dp;
	unicode c;
	LocalpRoot local;
	const unicode *body;
	addr charqueue, queue, thing, temp;
	size_t i, di, ni, size;
	struct fileparse backup;

	/* initialize */
	backup = *pa;
	local = pa->local;
	thing = pa->thing;
	if (! stringp(thing))
		TypeError(thing, STRING);
	charqueue_local(local->local, &charqueue, 0);
	pa->queue = charqueue;
	queue = Nil;
	size = pa->end;
	GetStringUnicode(thing, &body);
	absolute = relative = universal = question = win32device = logical = dp = 0;
	di = ni = 0;
	i = pa->start;

	/* Windows */
	if (backslash_backslash_pathname(body, size)) {
		logical = 1;
		if (charbackslash_pathname(body, size, '?')) goto quest;
		if (charbackslash_pathname(body, size, '.')) goto dot;
		if (check_unc_pathname(body, size)) goto universal;
	}
start_drive:
	if (check_drive_pathname(body, size, i, &c)) goto drive;
	goto start;

quest: /* ignore */
	question = 1;
	i += 4;
	goto start_drive;

dot:
	win32device = 1;
	make_device_windows_pathname(pa, body, size, 4);
	goto finish;

universal: /* ignore */
	universal = 1;
	i += 1; /* not 2 */
	GetConst(SYSTEM_UNIVERSAL, &pa->device);
	goto start;

drive:
	if (check_drive_logical_pathname(local, (int)c)) {
		/* parser logical */
		*pa = backup;
		parser_logical_pathname(pa);
		return;
	}
	logical = 1;
	make_drive_pathname(pa, c);
	i += 2;
	goto start;

	/* start */
start:
	if (size == 0) goto finish;
	string_getdirect(body, i++, &c);
	if (slashp(c)) {
		absolute = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_ABSOLUTE);
		goto first;
	}
	if (c == '.') { di = ni; dp = 1; }
	push_charqueue_local(local->local, charqueue, c);
	ni++;
	goto next1;

first:
	if (size <= i) goto finish;
	string_getdirect(body, i++, &c);
	if (slashp(c)) goto first;
	if (c == '.') { di = ni; dp = 1; }
	if (c == ':') { logical = 1; }
	push_charqueue_local(local->local, charqueue, c);
	ni++;
	goto next1;

next1:
	if (size <= i) goto name_finish;
	string_getdirect(body, i++, &c);
	if (slashp(c)) goto next2;
	if (c == '.') { di = ni; dp = 1; }
	if (logical == 0 && c == ':') {
		if (check_host_logical_pathname(local, charqueue)) {
			/* parser logical */
			*pa = backup;
			parser_logical_pathname(pa);
			return;
		}
		logical = 1;
	}
	push_charqueue_local(local->local, charqueue, c);
	ni++;
	goto next1;

next2:
	if (absolute == 0 && relative == 0) {
		relative = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_RELATIVE);
	}
	make_charqueue_fileparse(pa, charqueue, &temp);
	clear_charqueue(charqueue);
	pushdirectory_fileparse(pa, &queue, temp);
	ni = di = dp = 0;
	goto first;

name_finish:
	make_charqueue_fileparse(pa, charqueue, &pa->name);
	if (di && dp)
		nametype_pathname(pa, di);
	goto finish;

finish:
	nreverse_list_unsafe(&pa->directory, queue);
	pa->endpos = i;
	make_windows_pathname(pa);
}

static int system_unix_p(addr pos)
{
	addr check;
	GetConst(SYSTEM_UNIX, &check);
	return pos == check;
}

static int system_windows_p(addr pos)
{
	addr check;
	GetConst(SYSTEM_WINDOWS, &check);
	return pos == check;
}

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
		GetPathname(pa->path, PATHNAME_INDEX_HOST, &host);

	/* unix */
	if (system_unix_p(host)) {
		parser_unix_pathname(pa);
		return;
	}

	/* windows */
	if (system_windows_p(host)) {
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


/*
 *  parse-namestring
 */
static void defaults_pathname_alloc(Execute ptr, addr *ret, addr defaults, int localp)
{
	if (defaults == Nil || defaults == Unbound) {
		GetConst(SPECIAL_DEFAULT_PATHNAME_DEFAULTS, &defaults);
		getspecialcheck_local(ptr, defaults, &defaults);
	}
	pathname_designer_alloc(ptr, defaults, ret, localp);
}

static void defaults_pathname_heap(Execute ptr, addr *ret, addr defaults)
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

static void parse_pathname_full_heap(Execute ptr, addr thing, addr host,
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

static void parse_pathname_host_heap(Execute ptr, addr thing, addr host, addr *ret)
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
 *  wildcard-pathname-p
 */
static int wildcard_character_pathname(
		addr p1, size_t n1, size_t s1,
		addr p2, size_t n2, size_t s2)
{
	unicode c1, c2;
	size_t i;

	if (n1 == s1 && n2 == s2) return 1;
	if (n1 == s1 || n2 == s2) return 0;
	string_getc(p1, n1, &c1);
	string_getc(p2, n2, &c2);
	/* (a ?) -> next */
	if (c2 == '?')
		return wildcard_character_pathname(p1,n1+1,s1,  p2,n2+1,s2);
	/* (a a) -> next, (a b) -> false */
	if (c2 != '*') {
		if (c1 != c2) return 0;
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

static int wildcard_stringp_p(addr pos)
{
	return stringp(pos) && wildcard_string_p(pos);
}

static int wildcard_string_pathname(addr a, addr b)
{
	int check1, check2;
	addr wild;
	size_t s1, s2;

	GetConst(KEYWORD_WILD, &wild);
	if (a == wild && b == wild) return 1;
	check1 = stringp(a);
	check2 = stringp(b);
	if (check1 && b == wild) return 1;
	if ((! check1) || (! check2)) return 0;
	if (LispPathnameEqual(a, b)) return 1;
	if (wildcard_string_p(a)) return 0;
	string_length(a, &s1);
	string_length(b, &s2);
	return wildcard_character_pathname(a, 0, s1, b, 0, s2);
}

static int wildcard_eq_pathname(addr a, addr b)
{
	return (a == b) || wildcard_string_pathname(a, b);
}

static int wildcard_nil_pathname(addr a, addr b, int wildp)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	if (wildp && a == Nil) a = wild;
	if (wildp && b == Nil) b = wild;
	return wildcard_eq_pathname(a, b);
}

static int wildcard_list_pathname(addr a, addr b)
{
	addr a1, b1, pos1, pos2, wild, wilds;

	if (a == Nil && b == Nil) return 1;
	if (a != Nil && b == Nil) return 0;
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
		if (! wildcard_string_pathname(pos1, pos2)) return 0;
		return wildcard_list_pathname(a1, b1);
	}
	/* ("str" **) */
	for (;;) {
		if (wildcard_list_pathname(a, b1)) return 1;
		if (a == Nil) break;
		getcdr(a, &a);
	}
	return 0;
}

static int wildcard_directory_p(addr pos)
{
	addr check;

	GetConst(KEYWORD_WILD, &check);
	if (pos == check) return 1;
	GetConst(KEYWORD_WILD_INFERIORS, &check);
	if (pos == check) return 1;
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
		if (cdr1 == Nil && cdr2 == Nil) return 1;
		if (cdr1 == Nil || cdr2 == Nil) break;
		getcons(cdr1, &car1, &cdr1);
		getcons(cdr2, &car2, &cdr2);
		if (wildcard_directory_p(car1)) check1 = 1;
		if (wildcard_directory_p(car2)) check2 = 1;
		if (! LispPathnameEqual(car1, car2)) break;
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
	if (a == Nil) a = wild;
	if (b == Nil) b = wild;
	if (eql_function(a, b)) return 1;
	return b == wild;
}

_g int wildcard_pathname(addr a, addr b, int wild)
{
	addr check1, check2;

	Check(! pathnamep(a), "type left error");
	Check(! pathnamep(b), "type right error");
	if (RefLogicalPathname(a) != RefLogicalPathname(b)) return 0;
	/* host */
	GetPathname(a, PATHNAME_INDEX_HOST, &check1);
	GetPathname(b, PATHNAME_INDEX_HOST, &check2);
	if (! equalp_function(check1, check2)) return 0;
	/* device */
	GetPathname(a, PATHNAME_INDEX_DEVICE, &check1);
	GetPathname(b, PATHNAME_INDEX_DEVICE, &check2);
	if (! LispPathnameEqual(check1, check2)) return 0;
	/* directory */
	GetPathname(a, PATHNAME_INDEX_DIRECTORY, &check1);
	GetPathname(b, PATHNAME_INDEX_DIRECTORY, &check2);
	if (! wildcard_directory_pathname(check1, check2)) return 0;
	/* name */
	GetPathname(a, PATHNAME_INDEX_NAME, &check1);
	GetPathname(b, PATHNAME_INDEX_NAME, &check2);
	if (! wildcard_nil_pathname(check1, check2, wild)) return 0;
	/* type */
	GetPathname(a, PATHNAME_INDEX_TYPE, &check1);
	GetPathname(b, PATHNAME_INDEX_TYPE, &check2);
	if (! wildcard_nil_pathname(check1, check2, wild)) return 0;
	/* version */
	GetPathname(a, PATHNAME_INDEX_VERSION, &check1);
	GetPathname(b, PATHNAME_INDEX_VERSION, &check2);
	if (! wildcard_version_pathname(check1, check2)) return 0;

	return 1;
}


/*
 *  translate-pathname
 */
/* struct */

struct wildcard_position {
	struct wildcard_position *next;
	size_t a, b;
};

struct wildcard_struct {
	struct wildcard_position *root, *tail;
};

static struct wildcard_position *make_wildcard_position(LocalpRoot local,
		size_t a, size_t b)
{
	struct wildcard_position *ptr;

	ptr = (struct wildcard_position *)lowlevel_local(local->local,
			sizeoft(struct wildcard_position));
	ptr->next = NULL;
	ptr->a = a;
	ptr->b = b;

	return ptr;
}

static struct wildcard_struct *make_wildcard_struct(LocalpRoot local)
{
	struct wildcard_struct *ptr;

	ptr = (struct wildcard_struct *)lowlevel_local(local->local,
			sizeoft(struct wildcard_struct));
	ptr->root = ptr->tail = NULL;

	return ptr;
}

static void wildcard_struct_push(LocalpRoot local,
		struct wildcard_struct *ptr, size_t a, size_t b)
{
	struct wildcard_position *pos;

	pos = make_wildcard_position(local, a, b);
	if (ptr->root == NULL) {
		ptr->root = ptr->tail = pos;
	}
	else {
		pos->next = ptr->tail;
		ptr->tail = pos;
	}
}

static int wildcard_push_match_pathname(LocalpRoot local, struct wildcard_struct *ptr,
		addr p1, size_t n1, size_t s1, addr p2, size_t n2, size_t s2)
{
	int check;
	unicode c1, c2;
	size_t i;

	if (n1 == s1 && n2 == s2) return 1;
	if (n1 == s1 || n2 == s2) return 0;
	string_getc(p1, n1, &c1);
	string_getc(p2, n2, &c2);
	/* (a ?) -> next */
	if (c2 == '?') {
		check = wildcard_push_match_pathname(local,ptr,  p1,n1+1,s1,  p2,n2+1,s2);
		if (check)
			wildcard_struct_push(local, ptr, n1, n1+1);
		return check;
	}
	/* (a a) -> next, (a b) -> false */
	if (c2 != '*') {
		if (c1 != c2) return 0;
		return wildcard_push_match_pathname(local,ptr,  p1,n1+1,s1,  p2,n2+1,s2);
	}
	/* (a *) */
	n2++;
	for (i = n1; i <= s1; i++) {
		if (wildcard_push_match_pathname(local,ptr,  p1,i,s1,  p2,n2,s2)) {
			wildcard_struct_push(local, ptr, n1, i);
			return 1;
		}
	}
	return 0;
}

static int wildcard_push_string_pathname(LocalpRoot local,
		struct wildcard_struct *ptr, addr *ret, addr a, addr b)
{
	size_t s1, s2;

	if (! stringp(a)) return 0;
	if (! stringp(b)) return 0;
	string_length(a, &s1);
	string_length(b, &s2);
	return wildcard_push_match_pathname(local, ptr, a, 0, s1, b, 0, s2);
}

static void push_charqueue_wildcard(LocalpRoot local,
		addr queue, addr pos, struct wildcard_position *ptr)
{
	unicode c;
	size_t i;

	for (i = ptr->a; i < ptr->b; i++) {
		string_getc(pos, i, &c);
		push_charqueue_local(local->local, queue, c);
	}
}

static void wildcard_replace_pathname(LocalpRoot local,
		struct wildcard_struct *ptr, addr *ret, addr pos, addr to)
{
	unicode c;
	struct wildcard_position *child;
	addr queue;
	size_t size, i;

	charqueue_local(local->local, &queue, 0);
	string_length(to, &size);
	child = ptr->tail;
	for (i = 0; i < size; i++) {
		string_getc(to, i, &c);
		if (c == '*' || c == '?') {
			if (child) {
				push_charqueue_wildcard(local, queue, pos, child);
				child = child->next;
			}
		}
		else {
			push_charqueue_local(local->local, queue, c);
		}
	}

	/* position check */
	if (child) {
		clear_charqueue(queue);
		push_charqueue_wildcard(local, queue, pos, child);
		make_charqueue_alloc(localp_alloc(local), queue, &pos);
		fmte("Cannot extract ~S pattern.", pos, NULL);
		return;
	}

	/* result */
	make_charqueue_alloc(localp_alloc(local), queue, ret);
}

static void translate_string_pathname(LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	struct wildcard_struct *ptr;
	addr wild, root;
	LocalStack stack;

	/* wildcard */
	GetConst(KEYWORD_WILD, &wild);
	push_localp(local, &stack);
	if (from == wild)
		strvect_char_local(local->local, &from, "*");
	if (to == wild)
		strvect_char_local(local->local, &to, "*");
	ptr = make_wildcard_struct(local);
	if (! wildcard_push_string_pathname(local, ptr, &root, pos, from))
		fmte("The string ~S doesn't match ~S.", pos, from, NULL);

	/* replace */
	wildcard_replace_pathname(local, ptr, ret, pos, to);
	rollback_localp(local, stack);
}

static int translate_list_pathname(LocalpRoot local, addr *ret, addr a, addr b)
{
	int check;
	addr a1, b1, pos1, pos2, wild, wilds;

	if (a == Nil && b == Nil) return 1;
	if (a == Nil || b == Nil) return 0;
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wilds);
	getcons(a, &pos1, &a1);
	getcons(b, &pos2, &b1);
	/* ("str" *) -> next */
	if (pos2 == wild) {
		check = translate_list_pathname(local, ret, a1, b1);
		if (check)
			list_local(local->local, ret, *ret, pos2, pos1, Nil, NULL);
		return check;
	}
	/* ("str" "s*r") -> next, ("str" "a*a") -> false */
	if (wildcard_stringp_p(pos2)) {
		if (! wildcard_string_pathname(pos1, pos2)) return 0;
		list_local(local->local, ret, *ret, pos2, pos1, Nil, NULL);
		return translate_list_pathname(local, ret, a1, b1);
	}
	/* ("str" "str") -> next, ("str" "aaa") -> false */
	if (pos2 != wilds) {
		if (! wildcard_eq_pathname(pos1, pos2)) return 0;
		return translate_list_pathname(local, ret, a1, b1);
	}
	/* ("str" **) */
	a1 = a;
	for (;;) {
		if (translate_list_pathname(local, ret, a, b1)) {
			list_local(local->local, ret, *ret, pos2, a1, a, NULL);
			return 1;
		}
		if (a == Nil) break;
		getcdr(a, &a);
	}
	return 0;
}

static void replace_wild_wilds_pathname(LocalpRoot local, addr *root, addr a, addr b)
{
	addr pos;
	LocalRoot alloc;

	alloc = localp_alloc(local);
	while (a != b) {
		GetCons(a, &pos, &a);
		cons_alloc(alloc, root, pos, *root);
	}
}

static void replace_wild_string_pathname(LocalpRoot local,
		addr *root, addr pos, addr from)
{
	addr to;

	strvect_char_local(local->local, &to, "*");
	translate_string_pathname(local, &pos, pos, from, to);
	cons_alloc(localp_alloc(local), root, pos, *root);
}

static void replace_wild_pathname(LocalpRoot local, addr *root, addr *list)
{
	addr wild1, wild2, next, pos, a, b;

	if (*list == Nil)
		fmte("Don't match wildcard FROM and TO.", NULL);
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	List_bind(*list, &next, &pos, &a, &b, NULL);
	if (pos == wild1)
		cons_alloc(localp_alloc(local), root, a, *root);
	else if (pos == wild2)
		replace_wild_wilds_pathname(local, root, a, b);
	else if (wildcard_stringp_p(pos))
		replace_wild_string_pathname(local, root, a, pos);
	else
		cons_alloc(localp_alloc(local), root, pos, *root);
	*list = next;
}

static void replace_string_wild_pathname(LocalpRoot local,
		addr *root, addr pos, addr to)
{
	addr from;

	strvect_char_local(local->local, &from, "*");
	translate_string_pathname(local, &pos, pos, from, to);
	cons_alloc(localp_alloc(local), root, pos, *root);
}

static void replace_string_string_pathname(LocalpRoot local,
		addr *root, addr pos, addr from, addr to)
{
	translate_string_pathname(local, &pos, pos, from, to);
	cons_alloc(localp_alloc(local), root, pos, *root);
}

static void replace_string_pathname(LocalpRoot local, addr *root, addr *list, addr to)
{
	addr wild1, wild2, next, pos, a, b;

	if (*list == Nil)
		fmte("Don't match wildcard FROM and TO.", NULL);
	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	List_bind(*list, &next, &pos, &a, &b, NULL);
	if (pos == wild1 || pos == wild2)
		replace_string_wild_pathname(local, root, a, to);
	else if (wildcard_stringp_p(pos))
		replace_string_string_pathname(local, root, a, pos, to);
	else
		cons_alloc(localp_alloc(local), root, pos, *root);
	*list = next;
}

static void translate_replace_pathname(LocalpRoot local,
		addr *root, addr *list, addr to)
{
	LocalRoot alloc;
	addr pos, wild1, wild2;

	GetConst(KEYWORD_WILD, &wild1);
	GetConst(KEYWORD_WILD_INFERIORS, &wild2);
	alloc = localp_alloc(local);
	while (to != Nil) {
		getcons(to, &pos, &to);
		if (pos == wild1 || pos == wild2)
			replace_wild_pathname(local, root, list);
		else if (wildcard_stringp_p(pos))
			replace_string_pathname(local, root, list, pos);
		else
			cons_alloc(alloc, root, pos, *root);
	}
	nreverse_list_unsafe(root, *root);
}

static void translate_directory_pathname(LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	addr list;
	LocalStack stack;

	push_localp(local, &stack);
	list = Nil;
	if (! translate_list_pathname(local, &list, pos, from))
		fmte("Cannot translate ~S to ~S.", from, pos, NULL);
	*ret = Nil;
	translate_replace_pathname(local, ret, &list, to);
	rollback_localp(local, stack);
}

static void translate_nil_pathname(LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	addr wild;

	GetConst(KEYWORD_WILD, &wild);
	if (from == Nil) from = wild;
	if (to == Nil) to = wild;
	translate_string_pathname(local, ret, pos, from, to);
}

static void translate_version_pathname(addr *ret, addr pos, addr from, addr to)
{
	addr wild, unspec;

	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_UNSPECIFIC, &unspec);
	if (from == Nil) from = wild;
	if (to == Nil) to = wild;
	/* :unspecific */
	if (to == unspec) {
		*ret = unspec;
		return;
	}

	/* value */
	if (from == wild) {
		if (to == wild) {
			*ret = pos;
			return;
		}
		fmte(":VERSION from-wildcard is *, but to-wildcard ~S is not *.", to, NULL);
	}
	if (! eql_function(pos, to))
		fmte(":VERSION source ~S don't match to-wildcard ~S.", pos, to, NULL);

	/* :unspecific */
	*ret = unspec;
}

static void translate_pathname_localp(Execute ptr, LocalpRoot local,
		addr *ret, addr pos, addr from, addr to)
{
	int localp;
	LocalRoot alloc;
	addr one, a, b, c;

	alloc = localp_alloc(local);
	localp = local->localp;
	/* argument */
	pathname_designer_alloc(ptr, pos, &pos, localp);
	pathname_designer_alloc(ptr, from, &from, localp);
	pathname_designer_alloc(ptr, to, &to, localp);
	/* one */
	make_pathname_alloc(alloc, &one, RefLogicalPathname(to));
	/* host */
	copylocal_pathname_array(alloc, to, PATHNAME_INDEX_HOST, one);
	/* device */
	copylocal_pathname_array(alloc, to, PATHNAME_INDEX_DEVICE, one);
	/* directory */
	GetPathname(pos, PATHNAME_INDEX_DIRECTORY, &a);
	GetPathname(from, PATHNAME_INDEX_DIRECTORY, &b);
	GetPathname(to, PATHNAME_INDEX_DIRECTORY, &c);
	translate_directory_pathname(local, &a, a, b, c);
	copylocal_object(alloc, &a, a);
	SetPathname(one, PATHNAME_INDEX_DIRECTORY, a);
	/* name */
	GetPathname(pos, PATHNAME_INDEX_NAME, &a);
	GetPathname(from, PATHNAME_INDEX_NAME, &b);
	GetPathname(to, PATHNAME_INDEX_NAME, &c);
	translate_nil_pathname(local, &a, a, b, c);
	copylocal_object(alloc, &a, a);
	SetPathname(one, PATHNAME_INDEX_NAME, a);
	/* type */
	GetPathname(pos, PATHNAME_INDEX_TYPE, &a);
	GetPathname(from, PATHNAME_INDEX_TYPE, &b);
	GetPathname(to, PATHNAME_INDEX_TYPE, &c);
	translate_nil_pathname(local, &a, a, b, c);
	copylocal_object(alloc, &a, a);
	SetPathname(one, PATHNAME_INDEX_TYPE, a);
	/* version */
	GetPathname(pos, PATHNAME_INDEX_VERSION, &a);
	GetPathname(from, PATHNAME_INDEX_VERSION, &b);
	GetPathname(to, PATHNAME_INDEX_VERSION, &c);
	translate_version_pathname(&a, a, b, c);
	copylocal_object(alloc, &a, a);
	SetPathname(one, PATHNAME_INDEX_VERSION, a);
	/* result */
	*ret = one;
}

static void translate_pathname_alloc(Execute ptr,
		addr *ret, addr pos, addr from, addr to, int localp)
{
	struct localp_struct buffer;

	buffer.localp = localp;
	buffer.local = ptr->local;
	translate_pathname_localp(ptr, &buffer, ret, pos, from, to);
}

static void translate_pathname_heap(Execute ptr,
		addr *ret, addr pos, addr from, addr to)
{
	translate_pathname_alloc(ptr, ret, pos, from, to, 0);
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
	GetPathname(pos, PATHNAME_INDEX_HOST, &host);
	if (! gethost_pathname(host, &list))
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
	GetPathname(pos, PATHNAME_INDEX_NAME, &right);
	if (right != Nil) {
		if (right == wild)
			pushchar_charqueue_local(alloc, queue, "*");
		else if (right != Nil)
			pushstring_charqueue_local(alloc, queue, right);
	}

	/* type */
	GetPathname(pos, PATHNAME_INDEX_TYPE, &right);
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

	GetPathname(pos, PATHNAME_INDEX_HOST, &host);
	if (! system_unix_p(host) && ! system_windows_p(host))
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
	GetPathname(pos, PATHNAME_INDEX_VERSION, &right);
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
	GetPathname(pos, PATHNAME_INDEX_DIRECTORY, &right);
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
	GetPathname(pos, PATHNAME_INDEX_HOST, &host);
	if (system_unix_p(host))
		directory_namestring_filename(local, pos, queue, '/', 0);
	else if (system_windows_p(host))
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
	GetPathname(pos, PATHNAME_INDEX_DEVICE, &device);
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
		GetPathname(pos, PATHNAME_INDEX_NAME, &check);
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

	GetPathname(pos, PATHNAME_INDEX_HOST, &host);
	if (system_unix_p(host))
		namestring_unix(local, ret, pos);
	else if (system_windows_p(host))
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
	GetPathname(pos, PATHNAME_INDEX_HOST, &right);
	pushstring_charqueue_local(alloc, queue, right);
	pushchar_charqueue_local(alloc, queue, ":");
	/* directory, name, type */
	namestring_filename(local, pos, queue, ';', 1);
	/* version */
	GetPathname(pos, PATHNAME_INDEX_VERSION, &right);
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
static void merge_cons_directory(addr pos, addr defpath, addr *ret)
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
	nreverse_list_unsafe(ret, cons);
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
		merge_cons_directory(pos, defpath, ret);
	}
	else if (pos == Nil) {
		*ret = defpath;
	}
	else {
		*ret = pos;
	}
}

static void mergecopy(addr pos, enum PATHNAME_INDEX type, addr defpath, addr *ret)
{
	GetPathname(pos, type, &pos);
	if (pos == Nil) {
		GetPathname(defpath, type, ret);
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
	mergecopy(pos, PATHNAME_INDEX_HOST, defaults, &host);
	mergecopy(pos, PATHNAME_INDEX_DEVICE, defaults, &device);
	mergecopy(pos, PATHNAME_INDEX_NAME, defaults, &name);
	mergecopy(pos, PATHNAME_INDEX_TYPE, defaults, &type);

	/* If pathname does not specify a name, then the version, if not provided,
	 * will come from default-pathname, just like the other components.
	 * if (name == Nil && version == Nil)
	 *    the version copied from defvar.
	 */
	GetPathname(pos, PATHNAME_INDEX_NAME, &check1);
	GetPathname(pos, PATHNAME_INDEX_VERSION, &check2);
	if (check1 == Nil && check2 == Nil)
		version = defver;

	/* If pathname does specify a name, then the version is not affected
	 * by default-pathname.
	 *
	 * if (name != Nil)
	 *    the version don't copy from default-pathname.
	 */
	if (check1 != Nil)
		GetPathname(pos, PATHNAME_INDEX_VERSION, &version);

	/* If this process leaves the version missing, the default-version is used.
	 *
	 * Finally,
	 * if (version == Nil)
	 *   the version copy from default-pathname.
	 */
	if (version == Nil)
		version = defver;

	/* directory */
	GetPathname(pos, PATHNAME_INDEX_DIRECTORY, &check1);
	GetPathname(defaults, PATHNAME_INDEX_DIRECTORY, &check2);
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
		GetPathname(pos, PATHNAME_INDEX_HOST, &host);
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
 *  common-lisp
 */
static void make_pathname_directory(addr *ret, addr list)
{
	int check;
	addr root, pos, absolute, relative, wild, wildi, up;

	/* :directory "Hello" */
	GetConst(KEYWORD_ABSOLUTE, &absolute);
	if (stringp(list)) {
		list_heap(ret, absolute, list, NULL);
		return;
	}

	/* check */
	if (! consp(list))
		fmte(":directory ~S must be a list or string type.", list, NULL);
	GetConst(KEYWORD_RELATIVE, &relative);
	GetConst(KEYWORD_WILD, &wild);
	GetConst(KEYWORD_WILD_INFERIORS, &wildi);
	GetConst(KEYWORD_UP, &up);
	getcons(list, &pos, &root);
	if (pos != absolute && pos != relative) {
		fmte("The firest argument of :directory ~S must be ~S or ~S.",
				pos, absolute, relative, NULL);
	}
	for (check = 1; root != Nil; ) {
		getcons(root, &pos, &root);
		if (stringp(pos)
				&& stringp_equal_char(pos, "*")
				&& stringp_equal_char(pos, "**")
				&& stringp_equal_char(pos, "..")) {
			check = 0;
		}
		if (! stringp(pos) && pos != wild && pos != wildi && pos != up) {
			fmte("Invalid :directory argument ~S.", pos, NULL);
		}
	}
	if (check) {
		*ret = list;
		return;
	}

	/* rebuild */
	GetCons(list, &pos, &list);
	conscar_heap(&root, pos);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (stringp_equal_char(pos, "*"))
			pos = wild;
		else if (stringp_equal_char(pos, "**"))
			pos = wildi;
		else if (stringp_equal_char(pos, ".."))
			pos = up;
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);
}

enum PathnameType {
	PathnameType_Unix,
	PathnameType_Windows,
	PathnameType_Logical
};
static enum PathnameType pathname_environment(addr host)
{
	addr check;

	/* unix */
	GetConst(SYSTEM_UNIX, &check);
	if (host == check)
		return PathnameType_Unix;

	/* windows */
	GetConst(SYSTEM_WINDOWS, &check);
	if (host == check)
		return PathnameType_Windows;

	/* logical */
	if (stringp(host))
		return PathnameType_Logical;

	/* error */
	fmte("Invalid host value ~S.", host, NULL);

	return PathnameType_Unix;
}

static int make_pathname_upper_p(addr pos)
{
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (! isUpperCase(c)) return 0;
	}

	return 1;
}

static int make_pathname_lower_p(addr pos)
{
	unicode c;
	size_t size, i;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (! isUpperCase(c)) return 0;
	}

	return 1;
}

static void make_pathname_upper(addr *ret, addr pos)
{
	unicode c;
	addr one;
	size_t size, i;

	string_length(pos, &size);
	strvect_heap(&one, size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		strvect_setc(one, toUpperUnicode(c), c);
	}
	*ret = one;
}

static void make_pathname_lower(addr *ret, addr pos)
{
	unicode c;
	addr one;
	size_t size, i;

	string_length(pos, &size);
	strvect_heap(&one, size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		strvect_setc(one, toUpperUnicode(c), c);
	}
	*ret = one;
}

static void make_pathname_string(addr *ret)
{
	addr pos;

	pos = *ret;
	if (stringp(pos)) {
		if (make_pathname_upper_p(pos))
			make_pathname_lower(ret, pos);
		else if (make_pathname_lower_p(pos))
			make_pathname_upper(ret, pos);
	}
}

static int make_pathname_check(addr pos)
{
	return stringp(pos) && (make_pathname_upper_p(pos) || make_pathname_lower_p(pos));
}

static int make_pathname_list_p(addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		if (make_pathname_check(pos))
			return 1;
	}

	return 0;
}

static void make_pathname_list(addr *ret, addr list)
{
	addr root, pos;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		if (make_pathname_check(pos))
			make_pathname_string(&pos);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void make_pathname_case(addr *directory, addr *name, addr *type)
{
	/* directory */
	if (make_pathname_list_p(*directory))
		make_pathname_list(directory, *directory);
	/* name, type */
	make_pathname_string(name);
	make_pathname_string(type);
}

_g void make_pathname(Execute ptr, addr *ret, addr rest)
{

	/* (defun make-pathname (&key host device directory name type version defaults case)
	 *     ...) -> pathname
	 *   host       (or string symbol)
	 *   device     (or string symbol)  ;; (eql :unspecific))
	 *   directory  (or cons (member :wild :unspecific))
	 *   name       (or string cons (member nil :wild))
	 *   type       (or string (member nil :wild :unspecific)))
	 *   version    (or (integer 1 *) (member nil :wild :unspecific :newest))
	 *   defaults   (or pathname null)  ;; default *default-pathname-defaults*
	 *   case       (member :common :local)
	 */
	enum PathnameType ptype;
	addr host, device, directory, name, type, version, defaults, keycase, check;

	if (getkeyargs(rest, KEYWORD_HOST, &host)) host = Unbound;
	if (getkeyargs(rest, KEYWORD_DEVICE, &device)) device = Unbound;
	if (getkeyargs(rest, KEYWORD_DIRECTORY, &directory)) directory = Unbound;
	if (getkeyargs(rest, KEYWORD_NAME, &name)) name = Unbound;
	if (getkeyargs(rest, KEYWORD_TYPE, &type)) type = Unbound;
	if (getkeyargs(rest, KEYWORD_VERSION, &version)) version = Unbound;
	if (getkeyargs(rest, KEYWORD_DEFAULTS, &defaults)) defaults = Unbound;
	if (getkeyargs(rest, KEYWORD_CASE, &keycase)) keycase = Unbound;

	/* *default-pathname-defaults* */
	defaults_pathname_heap(ptr, &defaults, defaults);

	/* default arguments */
	if (host == Unbound)
		GetPathname(defaults, PATHNAME_INDEX_HOST, &host);
	if (device == Unbound)
		GetPathname(defaults, PATHNAME_INDEX_DEVICE, &device);
	if (directory == Unbound)
		GetPathname(defaults, PATHNAME_INDEX_DIRECTORY, &directory);
	if (name == Unbound)
		GetPathname(defaults, PATHNAME_INDEX_NAME, &name);
	if (type == Unbound)
		GetPathname(defaults, PATHNAME_INDEX_TYPE, &type);
	if (version == Unbound)
		GetPathname(defaults, PATHNAME_INDEX_VERSION, &version);

	/* format */
	make_pathname_directory(&directory, directory);

	/* case */
	ptype = pathname_environment(host);
	GetConst(KEYWORD_COMMON, &check);
	if (keycase == check &&
			(ptype == PathnameType_Unix || ptype == PathnameType_Logical)) {
		make_pathname_case(&directory, &name, &type);
	}

	/* pathname */
	if (ptype == PathnameType_Logical) {
		GetConst(KEYWORD_UNSPECIFIC, &check);
		if (version == check)
			GetConst(KEYWORD_NEWEST, &version);
		logical_pathname_heap(ret, host, directory, name, type, version);
	}
	else {
		pathname_heap(ret, host, device, directory, name, type);
	}
}


/*
 *  pathname accessor
 */
_g void pathname_host(addr pos, addr *ret, int localp)
{
	GetPathname(pos, PATHNAME_INDEX_HOST, &pos);
	if (! localp)
		make_pathname_string(&pos);
	*ret = pos;
}

_g void pathname_device(addr pos, addr *ret, int localp)
{
	GetPathname(pos, PATHNAME_INDEX_DEVICE, &pos);
	if (! localp)
		make_pathname_string(&pos);
	*ret = pos;
}

_g void pathname_directory(addr pos, addr *ret, int localp)
{
	GetPathname(pos, PATHNAME_INDEX_DIRECTORY, &pos);
	if (! localp) {
		if (make_pathname_list_p(pos))
			make_pathname_list(&pos, pos);
	}
	*ret = pos;
}

_g void pathname_name(addr pos, addr *ret, int localp)
{
	GetPathname(pos, PATHNAME_INDEX_NAME, &pos);
	if (! localp)
		make_pathname_string(&pos);
	*ret = pos;
}

_g void pathname_type(addr pos, addr *ret, int localp)
{
	GetPathname(pos, PATHNAME_INDEX_TYPE, &pos);
	if (! localp)
		make_pathname_string(&pos);
	*ret = pos;
}

_g void pathname_version(addr pos, addr *ret)
{
	GetPathname(pos, PATHNAME_INDEX_VERSION, ret);
}


/*
 *  logical-pathname-translations
 */
_g void get_logical_pathname_translations(addr host, addr *ret)
{
	gethost_pathname(host, ret);
}

static void list_logical_pathname_translations(Execute ptr,
		addr *ret, addr host, addr list)
{
	addr show, value, root, left, right, check;

	/* parse-list */
	show = list;
	root = Nil;
	while (list != Nil) {
		getcons(list, &right, &list);
		getcons(right, &left, &right);
		getcons(right, &right, &value);
		if (value != Nil)
			fmte("Invalid logical-pathname-translations arguments ~S.", show, NULL);

		/* left */
		if (stringp(left))
			parse_pathname_host_heap(ptr, left, host, &value);
		pathname_designer_heap(ptr, value, &value);
		if (! pathname_logical_p(value))
			fmte("The left argument ~S must be a logical-pathname.", left, NULL);
		GetPathname(value, PATHNAME_INDEX_HOST, &check);
		if (! string_equalp(host, check))
			fmte("The logical-pathname :HOST ~S must be ~S.", check, host, NULL);
		left = value;

		/* right */
		pathname_designer_heap(ptr, right, &value);
		if (! pathname_pathname_p(value))
			fmte("The right argument ~S must be a no-logical-pathname.", right, NULL);
		right = value;

		/* result */
		list_heap(&right, left, right, NULL);
		cons_heap(&root, right, root);
	}
	nreverse_list_unsafe(ret, root);
}

static int function_set_logical_pathname_translations(Execute ptr, addr condition)
{
	addr table, pos;

	/* delete */
	table_logical_pathname(&table);
	getdata_control(ptr, &pos);
	delete_hashtable(table, pos);
	/* throw */
	error_function(condition);

	return 0;
}

static void set_logical_pathname_translations_code(Execute ptr,
		addr host, addr list, addr cons)
{
	list_logical_pathname_translations(ptr, &list, host, list);
	SetCdr(cons, list);
}

static int set_logical_pathname_translations_intern(Execute ptr,
		addr host, addr list, addr table)
{
	addr control, symbol, call, cons;

	/* push */
	push_close_control(ptr, &control);
	/* handler-case */
	GetConst(COMMON_ERROR, &symbol);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_set_logical_pathname_translations);
	pushhandler_control(ptr, symbol, call, 0);
	/* code */
	intern_hashheap(table, host, &cons);
	SetDataFunction(call, host);
	set_logical_pathname_translations_code(ptr, host, list, cons);
	/* free */
	return free_control_(ptr, control);
}

_g int set_logical_pathname_translations(Execute ptr, addr host, addr list)
{
	addr table, cons;

	table_logical_pathname(&table);
	if (findvalue_hashtable(table, host, &cons)) {
		set_logical_pathname_translations_code(ptr, host, list, cons);
	}
	else {
		Return(set_logical_pathname_translations_intern(ptr, host, list, table));
	}

	return 0;
}


/*
 *  logical-pathname
 */
_g void logical_pathname(Execute ptr, addr *ret, addr pos)
{
	addr value, type;

	pathname_designer_heap(ptr, pos, &value);
	if (! pathname_logical_p(value)) {
		GetConst(COMMON_LOGICAL_PATHNAME, &type);
		type_error_stdarg(value, type,
				"The pathname ~S is not logical-pathname.", pos, NULL);
	}
	*ret = value;
}


/*
 *  namestring
 */
_g void namestring_pathname(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	if (stringp(pos)) {
		local = ptr->local;
		push_local(local, &stack);
		pathname_designer_local(ptr, pos, &pos);
		name_pathname_heap(ptr, pos, ret);
		rollback_local(local, stack);
	}
	else {
		pathname_designer_heap(ptr, pos, &pos);
		name_pathname_heap(ptr, pos, ret);
	}
}

_g void file_namestring_pathname(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	if (stringp(pos)) {
		push_local(local, &stack);
		pathname_designer_local(ptr, pos, &pos);
		file_name_pathname_heap(local, pos, ret);
		rollback_local(local, stack);
	}
	else {
		pathname_designer_heap(ptr, pos, &pos);
		file_name_pathname_heap(local, pos, ret);
	}
}

_g void directory_namestring_pathname(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	if (stringp(pos)) {
		push_local(local, &stack);
		pathname_designer_local(ptr, pos, &pos);
		directory_name_pathname_heap(local, pos, ret);
		rollback_local(local, stack);
	}
	else {
		pathname_designer_heap(ptr, pos, &pos);
		directory_name_pathname_heap(local, pos, ret);
	}
}

_g void host_namestring_pathname(Execute ptr, addr *ret, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	if (stringp(pos)) {
		local = ptr->local;
		push_local(local, &stack);
		pathname_designer_local(ptr, pos, &pos);
		GetPathname(pos, PATHNAME_INDEX_HOST, &pos);
		if (! stringp(pos))
			strvect_heap(ret, 0);
		else
			copylocal_object(NULL, ret, pos);
		rollback_local(local, stack);
	}
	else {
		pathname_designer_heap(ptr, pos, &pos);
		GetPathname(pos, PATHNAME_INDEX_HOST, &pos);
		if (! stringp(pos))
			strvect_heap(ret, 0);
		else
			*ret = pos;
	}
}

static void enough_copy_pathname(addr pos, addr cdr, addr *ret)
{
	addr one, value;

	make_pathname_alloc(NULL, &one, RefLogicalPathname(pos));
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_HOST, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_DEVICE, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_NAME, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_TYPE, one);
	copylocal_pathname_array(NULL, pos, PATHNAME_INDEX_VERSION, one);
	/* directory */
	GetPathname(pos, PATHNAME_INDEX_DIRECTORY, &pos);
	GetConst(KEYWORD_RELATIVE, &value);
	copylocal_object(NULL, &cdr, cdr);
	cons_heap(&value, value, cdr);
	SetPathname(one, PATHNAME_INDEX_DIRECTORY, value);
	*ret = one;
}

static int enough_directory_pathname(LocalRoot local,
		addr pos, addr a, addr b, addr *ret)
{
	addr car1, car2, cdr1, cdr2, next;

	/* relative, absolute */
	getcons(a, &car1, &cdr1);
	getcons(b, &car2, &cdr2);
	if (car1 != car2)
		return 0;

	/* directory */
	while (cdr2 != Nil) {
		if (cdr1 == Nil)
			return 0;
		getcons(cdr1, &car1, &next);
		getcons(cdr2, &car2, &cdr2);
		if (! LispPathnameEqual(car1, car2))
			return 0;
		cdr1 = next;
	}

	/* result */
	enough_copy_pathname(pos, cdr1, ret);

	return 1;
}

static int enough_merge_pathname(LocalRoot local, addr a, addr b, addr *ret)
{
	int check1, check2;
	addr pos1, pos2;

	check1 = RefLogicalPathname(a);
	check2 = RefLogicalPathname(b);
	if (check1 != check2) return 0;
	/* host */
	GetPathname(a, PATHNAME_INDEX_HOST, &pos1);
	GetPathname(b, PATHNAME_INDEX_HOST, &pos2);
	if (! equalp_function(pos1, pos2)) return 0;
	/* device */
	GetPathname(a, PATHNAME_INDEX_DEVICE, &pos1);
	GetPathname(b, PATHNAME_INDEX_DEVICE, &pos2);
	if (! LispPathnameEqual(pos1, pos2)) return 0;
	/* directory */
	GetPathname(a, PATHNAME_INDEX_DIRECTORY, &pos1);
	GetPathname(b, PATHNAME_INDEX_DIRECTORY, &pos2);
	return enough_directory_pathname(local, a, pos1, pos2, ret);
}

_g void enough_namestring_pathname(Execute ptr, addr *ret, addr pos, addr defaults)
{
	addr value;

	defaults_pathname_heap(ptr, &defaults, defaults);
	pathname_designer_heap(ptr, pos, &pos);
	if (enough_merge_pathname(ptr->local, pos, defaults, &value))
		pos = value;
	name_pathname_heap(ptr, pos, ret);
}


/*
 *  parse-namestring
 */
_g void parse_namestring(Execute ptr, addr *ret, addr *position,
		addr thing, addr host, addr defaults, addr start, addr end, addr junk)
{
	addr check, type;
	size_t index1, index2;

	/* defaults */
	defaults_pathname_heap(ptr, &defaults, defaults);
	/* stream */
	if (streamp(thing))
		GetPathnameStream(thing, &thing);
	/* pathname */
	if (pathnamep(thing)) {
		GetPathname(thing, PATHNAME_INDEX_HOST, &check);
		if (host != Nil && ! equalp_function(host, check)) {
			GetConst(COMMON_PATHNAME, &type);
			type_error_stdarg(thing, type,
					":HOST ~S does not match a pathname host ~S.",
					host, check, NULL);
		}
		*ret = thing;
		*position = start;
		return;
	}
	/* string */
	if (stringp(thing)) {
		if (GetIndex_integer(start, &index1))
			fmte("Invalid :start value ~S.", start, NULL);
		if (end == Nil)
			string_length(thing, &index2);
		else if (GetIndex_integer(end, &index2))
			fmte("Invalid :start value ~S.", end, NULL);
		parse_pathname_full_heap(ptr, thing, host, defaults,
				index1, index2, junk != Nil, &thing, &index1);
		make_index_integer_alloc(NULL, position, index1);
		/* host check */
		if (host != Nil) {
			GetPathname(thing, PATHNAME_INDEX_HOST, &check);
			if (! equalp_function(host, check))
				fmte(":HOST ~S is not argument host ~S.", check, host, NULL);
		}
		/* result */
		*ret = thing;
		return;
	}

	GetConst(COMMON_PATHNAME, &type);
	type_error_stdarg(thing, type, "~S is not pathname-designer.", thing, NULL);
}


/*
 *  pathname-wild-p
 */
static int wild_pathname_string(addr pos)
{
	unicode c;
	size_t size, i;

	if (! stringp(pos)) return 0;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (c == '*' || c == '?') return 1;
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
		GetPathname(file, PATHNAME_INDEX_DIRECTORY, &pos);
		if (pos == wild1) return 1;
		while (consp(pos)) {
			GetCons(pos, &value, &pos);
			if (value == wild1) return 1;
			if (value == wild2) return 1;
			if (wild_pathname_string(value)) return 1;
		}
	}

	/* name */
	GetConst(KEYWORD_NAME, &check);
	if (field == check || field == Nil) {
		GetPathname(file, PATHNAME_INDEX_NAME, &pos);
		if (pos == wild1) return 1;
		if (wild_pathname_string(pos)) return 1;
	}

	/* type */
	GetConst(KEYWORD_TYPE, &check);
	if (field == check || field == Nil) {
		GetPathname(file, PATHNAME_INDEX_TYPE, &pos);
		if (pos == wild1) return 1;
		if (wild_pathname_string(pos)) return 1;
	}

	/* version */
	GetConst(KEYWORD_VERSION, &check);
	if (field == check || field == Nil) {
		GetPathname(file, PATHNAME_INDEX_VERSION, &pos);
		if (pos == wild1) return 1;
	}

	return 0;
}

_g void wild_pathname_p(Execute ptr, addr *ret, addr file, addr field)
{
	int check;

	pathname_designer_heap(ptr, file, &file);
	if (field == Unbound) field = Nil;
	check = wild_pathname_boolean(file, field);
	*ret = check? T: Nil;
}


/*
 *  pathname-match-p
 */
_g void pathname_match_p(Execute ptr, addr *ret, addr a, addr b)
{
	int check;

	pathname_designer_heap(ptr, a, &a);
	pathname_designer_heap(ptr, b, &b);
	check = wildcard_pathname(a, b, 1);
	*ret = check? T: Nil;
}


/*
 *  translate-pathname
 */
_g void translate_pathname(Execute ptr, addr *ret, addr pos, addr from, addr to)
{
	translate_pathname_heap(ptr, ret, pos, from, to);
}


/*
 *  translate-logical-pathname
 */
_g void translate_logical_pathname(Execute ptr, addr *ret, addr pos)
{
	physical_pathname_heap(ptr, pos, ret);
}


/*
 *  merge-pathnames
 */
_g void merge_pathnames(Execute ptr, addr *ret, addr pos, addr defaults, addr version)
{
	merge_pathnames_clang(ptr, pos, defaults, version, ret);
}


/*
 *  load-logical-pathname-translations
 */
_g int load_logical_pathname_translations_common(Execute ptr, addr host, int *ret)
{
	addr defaults, file, x;

	/* pathname */
	GetConst(SYSTEM_LOAD_LOGICAL_PATHNAME_TRANSLATIONS, &defaults);
	getspecial_local(ptr, defaults, &defaults);
	defaults_pathname_heap(ptr, &defaults, defaults);
	pathname_heap(&file, Nil, Nil, Nil, host, Nil);
	merge_pathnames_clang(ptr, host, defaults, Unbound, &file);
	/* load */
	probe_file_files(ptr, &x, file);
	if (x != T) {
		/* host file is not exist */
		*ret = 0;
		return 0;
	}
	/* read file */
	Return(readlist_input(ptr, file, &x));
	Return(set_logical_pathname_translations(ptr, host, x));
	*ret = 1;

	return 0;
}


/*
 *  initialize
 */
_g void init_pathname(void)
{
	SetPointerCall(defun, var1, set_logical_pathname_translations);
}

