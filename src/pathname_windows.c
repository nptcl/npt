#include "character_check.h"
#include "character_queue.h"
#include "condition.h"
#include "constant.h"
#include "cons_list.h"
#include "pathname_object.h"
#include "pathname_table.h"
#include "strtype.h"
#include "strvect.h"

/*
 *  windows pathname
 */
static int parser_make_windows_pathname_(struct fileparse *pa)
{
	GetConst(SYSTEM_WINDOWS, &pa->host);
	Return(wild_value_pathname_(pa->name, &pa->name));
	Return(wild_value_pathname_(pa->type, &pa->type));
	GetVersionPathname(pa->path, &pa->version);
	pathname_fileparse_alloc(pa, 0);

	return 0;
}

static int parser_windows_bsbs_p_(addr thing, size_t size, int *ret)
{
	unicode c;

	if (size < 2)
		return Result(ret, 0);
	Return(string_getc_(thing, 0, &c));
	if (c != '\\')
		return Result(ret, 0);
	Return(string_getc_(thing, 1, &c));

	return Result(ret, (c == '\\'));
}

static int parser_windows_bs_p_(addr thing, size_t size, unicode x, int *ret)
{
	unicode c;

	if (size < 4)
		return Result(ret, 0);
	Return(string_getc_(thing, 2, &c));
	if (c != x)
		return Result(ret, 0);
	Return(string_getc_(thing, 3, &c));

	return Result(ret, (c == '\\'));
}

static int parser_windows_unc_p_(addr thing, size_t size, int *ret)
{
	unicode c;

	if (size < 2)
		return Result(ret, 0);
	Return(string_getc_(thing, 2, &c));

	return Result(ret, (c != '\\'));
}

static int parser_windows_drive_p_(addr thing,
		size_t size, size_t i, unicode *value, int *ret)
{
	unicode c, drive;

	if ((size - i) < 2)
		return Result(ret, 0);
	Return(string_getc_(thing, i, &drive));
	if (! isAlphabetic(drive))
		return Result(ret, 0);
	Return(string_getc_(thing, i + 1, &c));
	if (c != ':')
		return Result(ret, 0);
	*value = drive;

	return Result(ret, 1);
}

static int parser_windows_drive_(struct fileparse *pa, unicode c)
{
	addr pos;

	strvect_alloc(localp_alloc(pa->local), &pos, 1);
	Return(strvect_setc_(pos, 0, toUpperUnicode(c)));
	pa->device = pos;

	return 0;
}

static int parser_windows_device_(struct fileparse *pa,
		addr thing, size_t size, size_t i)
{
	addr queue;

	/* drive */
	GetConst(SYSTEM_DEVICE, &pa->device);
	/* name */
	queue = pa->queue;
	Return(pushrange_pathname_(pa->local, queue, thing, i, size));
	make_charqueue_fileparse(pa, queue, &pa->name);
	clear_charqueue(queue);

	return 0;
}

static int parser_windows_slash_p(unicode x)
{
	return x == '/' || x == '\\';
}

int parser_windows_pathname_(struct fileparse *pa)
{
	int absolute, relative, logical, dp, check;
	unicode c;
	LocalpRoot local;
	addr charqueue, queue, thing, temp;
	size_t i, di, ni, size;
	struct fileparse backup;

	/* initialize */
	backup = *pa;
	local = pa->local;
	thing = pa->thing;
	if (! stringp(thing))
		return TypeError_(thing, STRING);
	charqueue_local(local->local, &charqueue, 0);
	pa->queue = charqueue;
	queue = Nil;
	size = pa->end;
	absolute = relative = logical = dp = 0;
	di = ni = 0;
	i = pa->start;

	/* Windows */
	Return(parser_windows_bsbs_p_(thing, size, &check));
	if (check) {
		logical = 1;
		Return(parser_windows_bs_p_(thing, size, '?', &check));
		if (check)
			goto question;
		Return(parser_windows_bs_p_(thing, size, '.', &check));
		if (check)
			goto dot;
		Return(parser_windows_unc_p_(thing, size, &check));
		if (check)
			goto universal;
	}

drive1:
	Return(parser_windows_drive_p_(thing, size, i, &c, &check));
	if (check)
		goto drive2;
	goto start;

question: /* ignore */
	i += 4;
	goto drive1;

dot:
	Return(parser_windows_device_(pa, thing, size, 4));
	goto finish;

universal: /* ignore */
	i += 1; /* not 2 */
	GetConst(SYSTEM_UNIVERSAL, &pa->device);
	goto start;

drive2:
	Return(check_drive_logical_pathname_(local, (int)c, &check));
	if (check) {
		/* parser logical */
		*pa = backup;
		return parser_logical_pathname_(pa);
	}
	logical = 1;
	Return(parser_windows_drive_(pa, c));
	i += 2;
	goto start;

	/* start */
start:
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
	if (parser_windows_slash_p(c)) {
		absolute = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_ABSOLUTE);
		goto first;
	}
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	Return(push_charqueue_local_(local->local, charqueue, c));
	ni++;
	goto next1;

first:
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
	if (parser_windows_slash_p(c))
		goto first;
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	if (c == ':')
		logical = 1;
	Return(push_charqueue_local_(local->local, charqueue, c));
	ni++;
	goto next1;

next1:
	if (size <= i)
		goto finish1;
	Return(string_getc_(thing, i++, &c));
	if (parser_windows_slash_p(c))
		goto next2;
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	if (logical == 0 && c == ':') {
		Return(check_host_logical_pathname_(local, charqueue, &check));
		if (check) {
			/* parser logical */
			*pa = backup;
			return parser_logical_pathname_(pa);
		}
		logical = 1;
	}
	Return(push_charqueue_local_(local->local, charqueue, c));
	ni++;
	goto next1;

next2:
	if (absolute == 0 && relative == 0) {
		relative = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_RELATIVE);
	}
	make_charqueue_fileparse(pa, charqueue, &temp);
	clear_charqueue(charqueue);
	Return(pushdirectory_fileparse_(pa, &queue, temp));
	ni = di = dp = 0;
	goto first;

finish1:
	make_charqueue_fileparse(pa, charqueue, &pa->name);
	if (di && dp) {
		Return(nametype_pathname_(pa, di));
	}
	goto finish;

finish:
	nreverse(&pa->directory, queue);
	pa->endpos = i;
	return parser_make_windows_pathname_(pa);
}

