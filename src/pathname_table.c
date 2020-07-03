#include "character_check.h"
#include "charqueue.h"
#include "condition.h"
#include "constant.h"
#include "cons.h"
#include "integer.h"
#include "local.h"
#include "pathname_localp.h"
#include "pathname_table.h"
#include "pathname_translate.h"
#include "pathname_object.h"
#include "reader.h"
#include "strtype.h"
#include "strvect.h"
#include "typedef.h"

_g void init_fileparse(struct fileparse *pa, Execute ptr, int localp)
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

_g void pathname_fileparse_alloc(struct fileparse *pa, int logical)
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

_g void wild_value_pathname(addr input, addr *ret)
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

	if (! stringp(pos))
		return;
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

	if (! stringp(pos))
		return;
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

_g void make_parse_logical_pathname(struct fileparse *pa)
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

_g void pushrange_pathname(LocalpRoot local,
		addr queue, const unicode *body, size_t n1, size_t n2)
{
	unicode c;

	for (; n1 < n2; n1++) {
		string_getdirect(body, n1, &c);
		push_charqueue_local(local->local, queue, c);
	}
}

_g void make_charqueue_fileparse(struct fileparse *pa, addr queue, addr *ret)
{
	make_charqueue_alloc(localp_alloc(pa->local), queue, ret);
}

_g void nametype_pathname(struct fileparse *pa, size_t index)
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

_g void pushdirectory_fileparse(struct fileparse *pa, addr *list, addr name)
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

_g void pushconstant_fileparse(struct fileparse *pa, addr *list, constindex index)
{
	addr value;
	GetConstant(index, &value);
	cons_alloc(localp_alloc(pa->local), list, value, *list);
}

_g int check_host_logical_pathname(LocalpRoot local, addr queue)
{
	addr key;
	make_charqueue_local(local->local, queue, &key);
	return gethost_logical_pathname(key, &key);
}

_g int check_drive_logical_pathname(LocalpRoot local, int drive)
{
	addr key;
	strvect_local(local->local, &key, 1);
	strvect_setc(key, 0, drive);
	return gethost_logical_pathname(key, &key);
}

