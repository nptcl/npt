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
static void parser_make_windows_pathname(struct fileparse *pa)
{
	GetConst(SYSTEM_WINDOWS, &pa->host);
	wild_value_pathname(pa->name, &pa->name);
	wild_value_pathname(pa->type, &pa->type);
	GetVersionPathname(pa->path, &pa->version);
	pathname_fileparse_alloc(pa, 0);
}

static int parser_windows_pathname_bsbs_p(const unicode *body, size_t size)
{
	unicode c;

	if (size < 2)
		return 0;
	string_getdirect(body, 0, &c);
	if (c != '\\')
		return 0;
	string_getdirect(body, 1, &c);

	return (c == '\\');
}

static int parser_windows_pathname_bs_p(const unicode *body, size_t size, unicode type)
{
	unicode c;

	if (size < 4)
		return 0;
	string_getdirect(body, 2, &c);
	if (c != type)
		return 0;
	string_getdirect(body, 3, &c);

	return (c == '\\');
}

static int parser_windows_pathname_unc_p(const unicode *body, size_t size)
{
	unicode c;

	if (size < 2)
		return 0;
	string_getdirect(body, 2, &c);

	return (c != '\\');
}

static int parser_windows_pathname_drive_p(const unicode *body,
		size_t size, size_t i, unicode *ret)
{
	unicode c, drive;

	if ((size - i) < 2)
		return 0;
	string_getdirect(body, i, &drive);
	if (! isAlphabetic(drive))
		return 0;
	string_getdirect(body, i + 1, &c);
	if (c != ':')
		return 0;
	*ret = drive;

	return 1;
}

static void parser_windows_pathname_drive(struct fileparse *pa, unicode c)
{
	addr pos;
	strvect_alloc(localp_alloc(pa->local), &pos, 1);
	strvect_setc(pos, 0, c);
	pa->device = pos;
}

static void parser_windows_pathname_device(struct fileparse *pa,
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

static int parser_windows_pathname_slash_p(unicode x)
{
	return x == '/' || x == '\\';
}

_g int parser_windows_pathname_(struct fileparse *pa)
{
	int absolute, relative, universal, question, win32device, logical, dp, check;
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
		return TypeError_(thing, STRING);
	charqueue_local(local->local, &charqueue, 0);
	pa->queue = charqueue;
	queue = Nil;
	size = pa->end;
	GetStringUnicode(thing, &body);
	absolute = relative = universal = question = win32device = logical = dp = 0;
	di = ni = 0;
	i = pa->start;

	/* Windows */
	if (parser_windows_pathname_bsbs_p(body, size)) {
		logical = 1;
		if (parser_windows_pathname_bs_p(body, size, '?'))
			goto quest;
		if (parser_windows_pathname_bs_p(body, size, '.'))
			goto dot;
		if (parser_windows_pathname_unc_p(body, size))
			goto universal;
	}
start_drive:
	if (parser_windows_pathname_drive_p(body, size, i, &c))
		goto drive;
	goto start;

quest: /* ignore */
	question = 1;
	i += 4;
	goto start_drive;

dot:
	win32device = 1;
	parser_windows_pathname_device(pa, body, size, 4);
	goto finish;

universal: /* ignore */
	universal = 1;
	i += 1; /* not 2 */
	GetConst(SYSTEM_UNIVERSAL, &pa->device);
	goto start;

drive:
	Return(check_drive_logical_pathname_(local, (int)c, &check));
	if (check) {
		/* parser logical */
		*pa = backup;
		return parser_logical_pathname_(pa);
	}
	logical = 1;
	parser_windows_pathname_drive(pa, c);
	i += 2;
	goto start;

	/* start */
start:
	if (size == 0)
		goto finish;
	string_getdirect(body, i++, &c);
	if (parser_windows_pathname_slash_p(c)) {
		absolute = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_ABSOLUTE);
		goto first;
	}
	if (c == '.') { di = ni; dp = 1; }
	push_charqueue_local(local->local, charqueue, c);
	ni++;
	goto next1;

first:
	if (size <= i)
		goto finish;
	string_getdirect(body, i++, &c);
	if (parser_windows_pathname_slash_p(c))
		goto first;
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	if (c == ':')
		logical = 1;
	push_charqueue_local(local->local, charqueue, c);
	ni++;
	goto next1;

next1:
	if (size <= i)
		goto name_finish;
	string_getdirect(body, i++, &c);
	if (parser_windows_pathname_slash_p(c))
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
	nreverse(&pa->directory, queue);
	pa->endpos = i;
	parser_make_windows_pathname(pa);
	return 0;
}

