#include "character_queue.h"
#include "condition.h"
#include "constant.h"
#include "cons_list.h"
#include "pathname_object.h"
#include "pathname_table.h"
#include "strtype.h"
#include "strvect.h"

/*
 *  unix pathname
 */
static void parser_make_unix_pathname(struct fileparse *pa)
{
	GetConst(SYSTEM_UNIX, &pa->host);
	GetDevicePathname(pa->path, &pa->device);
	wild_value_pathname(pa->name, &pa->name);
	wild_value_pathname(pa->type, &pa->type);
	GetVersionPathname(pa->path, &pa->version);
	pathname_fileparse_alloc(pa, 0);
}

_g void parser_unix_pathname(struct fileparse *pa)
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
	if (size == 0)
		goto finish;
	string_getdirect(body, i++, &c);
	if (c == '/') {
		absolute = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_ABSOLUTE);
		goto first;
	}
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	if (c == ':')
		logical = 1;
	push_charqueue_local(local->local, charqueue, c);
	ni++;
	goto next1;

first:
	if (size <= i)
		goto finish;
	string_getdirect(body, i++, &c);
	if (c == '/')
		goto first;
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	push_charqueue_local(local->local, charqueue, c);
	ni++;
	goto next1;

next1:
	if (size <= i)
		goto name_finish;
	string_getdirect(body, i++, &c);
	if (c == '/')
		goto next2;
	if (c == '.') {
		di = ni;
		dp = 1;
	}
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
	nreverse(&pa->directory, queue);
	pa->endpos = i;
	parser_make_unix_pathname(pa);
}

