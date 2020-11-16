#include "character_check.h"
#include "character_queue.h"
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
#include "type_table.h"
#include "typedef.h"

void init_fileparse(struct fileparse *pa, Execute ptr, int localp)
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

void pathname_fileparse_alloc(struct fileparse *pa, int logical)
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

int wild_value_pathname_(addr input, addr *ret)
{
	int check;

	Return(stringp_equal_char_(input, "*", &check));
	if (check) {
		GetConst(KEYWORD_WILD, ret);
		return 0;
	}

	return Result(ret, input);
}

static int wild_newest_value_pathname_(addr input, addr *ret)
{
	int check;

	Return(stringp_equal_char_(input, "*", &check));
	if (check) {
		GetConst(KEYWORD_WILD, ret);
		return 0;
	}

	Return(stringp_equalp_char_(input, "NEWEST", &check));
	if (check) {
		GetConst(KEYWORD_NEWEST, ret);
		return 0;
	}

	return Result(ret, input);
}

static int check_asterisk_logical_pathname_(addr pos)
{
	unicode a, b;
	size_t size, i;

	if (! stringp(pos))
		return 0;
	string_length(pos, &size);
	a = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &b));
		if (a == '*' && b == '*')
			return fmte_("Invalid wildcard string ~S.", pos, NULL);
	}

	return 0;
}

static int check_version_logical_pathname_(addr pos)
{
	unicode c;
	size_t size, i;

	if (! stringp(pos))
		return 0;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (! isDigitCase(c))
			return fmte_(":VERSION ~S must be a positive integer.", pos, NULL);
	}

	return 0;
}

static int check_parse_logical_pathname_host_(addr host, int errorp)
{
	addr check;

	if (! errorp)
		return 0;
	Return(gethost_logical_pathname_(host, &check));
	if (check == Nil) {
		GetTypeTable(&check, String);
		return call_type_error_va_(NULL,
				host, check, "There is no logical hostname ~S.", host, NULL);
	}

	return 0;
}

static int check_parse_logical_pathname_(struct fileparse *pa)
{
	int check;
	addr list, pos;
	size_t size;

	/* host */
	string_length(pa->host, &size);
	if (size == 0)
		return fmte_("Invalid host name ~S.", pa->host, NULL);
	Return(check_parse_logical_pathname_host_(pa->host, pa->errorp));

	/* directory */
	for (list = pa->directory; list != Nil; ) {
		GetCons(list, &pos, &list);
		Return(check_asterisk_logical_pathname_(pos));
	}
	/* name */
	Return(check_asterisk_logical_pathname_(pa->name));
	/* type */
	Return(check_asterisk_logical_pathname_(pa->type));
	/* version */
	pos = pa->version;
	Return(check_version_logical_pathname_(pos));
	if (stringp(pos)) {
		if (read_from_string(pa->ptr, &check, &pos, pos))
			return fmte_("Cannot read ~S object.", pa->version, NULL);
		if (check)
			return fmte_("Cannot read ~S object.", pa->version, NULL);
		if (! integerp(pos))
			return fmte_("Invalid version object ~S.", pos, NULL);
		pa->version = pos;
	}

	return 0;
}

int make_parse_logical_pathname_(struct fileparse *pa)
{
	GetConst(KEYWORD_UNSPECIFIC, &pa->device);
	Return(wild_value_pathname_(pa->name, &pa->name));
	Return(wild_value_pathname_(pa->type, &pa->type));
	Return(wild_newest_value_pathname_(pa->version, &pa->version));
	if (pa->host == Nil)
		return fmte_("No logical-pathname host.", NULL);
	Return(check_parse_logical_pathname_(pa));
	pathname_fileparse_alloc(pa, 1);

	return 0;
}

int pushrange_pathname_(LocalpRoot local,
		addr queue, addr thing, size_t n1, size_t n2)
{
	unicode c;

	for (; n1 < n2; n1++) {
		Return(string_getc_(thing, n1, &c));
		Return(push_charqueue_local_(local->local, queue, c));
	}

	return 0;
}

void make_charqueue_fileparse(struct fileparse *pa, addr queue, addr *ret)
{
	make_charqueue_alloc(localp_alloc(pa->local), queue, ret);
}

int nametype_pathname_(struct fileparse *pa, size_t index)
{
	/*
	 *  index -> 1..size-1
	 *    "a.bc" -> ("a" "bc")
	 *    "abc." -> ("abc" "")   not nil
	 */
	addr pos, queue;
	size_t size;
	LocalpRoot local;

	local = pa->local;
	queue = pa->queue;
	pos = pa->name;
	string_length(pos, &size);
	Check(size <= index, "length error");

	/* name */
	clear_charqueue(queue);
	Return(pushrange_pathname_(local, queue, pos, 0, index));
	make_charqueue_fileparse(pa, queue, &pa->name);
	/* type */
	clear_charqueue(queue);
	Return(pushrange_pathname_(local, queue, pos, index + 1, size));
	make_charqueue_fileparse(pa, queue, &pa->type);

	return 0;
}

int pushdirectory_fileparse_(struct fileparse *pa, addr *list, addr name)
{
	/*
	 *  ".." -> :UP
	 *  "*"  -> :WILD
	 *  "**" -> :WILD-INFERIORS
	 */
	int check;

	Return(stringp_equal_char_(name, "..", &check));
	if (check) {
		GetConst(KEYWORD_UP, &name);
		goto next;
	}

	Return(stringp_equal_char_(name, "*", &check));
	if (check) {
		GetConst(KEYWORD_WILD, &name);
		goto next;
	}

	Return(stringp_equal_char_(name, "**", &check));
	if (check) {
		GetConst(KEYWORD_WILD_INFERIORS, &name);
		goto next;
	}

next:
	cons_alloc(localp_alloc(pa->local), list, name, *list);
	return 0;
}

void pushconstant_fileparse(struct fileparse *pa, addr *list, constindex index)
{
	addr value;
	GetConstant(index, &value);
	cons_alloc(localp_alloc(pa->local), list, value, *list);
}

int check_host_logical_pathname_(LocalpRoot local, addr queue, int *ret)
{
	addr key;
	make_charqueue_local(local->local, queue, &key);
	Return(gethost_logical_pathname_(key, &key));
	return Result(ret, key != Nil);
}

int check_drive_logical_pathname_(LocalpRoot local, int drive, int *ret)
{
	addr key;
	strvect_local(local->local, &key, 1);
	Return(strvect_setc_(key, 0, drive));
	Return(gethost_logical_pathname_(key, &key));
	return Result(ret, key != Nil);
}

