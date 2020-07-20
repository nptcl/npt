#include "character_check.h"
#include "character_queue.h"
#include "condition.h"
#include "cons_list.h"
#include "pathname_table.h"
#include "strtype.h"
#include "strvect.h"

/*
 *  logical-pathname
 */
static int parser_logical_pathname_wordaster_p(unicode c)
{
	return isAlphanumeric(c) || (c == '-') || (c == '*');
}

_g int parser_logical_pathname_(struct fileparse *pa)
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
		return TypeError_(thing, STRING);
	charqueue_local(local->local, &charqueue, 0);
	pa->queue = charqueue;
	queue = Nil;
	size = pa->end;
	GetStringUnicode(thing, &body);
	absolute = relative = dp1 = dp2 = 0;
	i = pa->start;
	GetConst(KEYWORD_NEWEST, &pa->version);

	/* start */
	if (size == 0)
		goto finish;
	string_getdirect(body, i++, &c);
	if (parser_logical_pathname_wordaster_p(c))
		goto next1;
	if (c == ';') {
		relative = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_RELATIVE);
		goto discard;
	}
	if (c == '.')
		goto dot_type2;
	goto error;

host:
	make_charqueue_fileparse(pa, charqueue, &pa->host);
	clear_charqueue(charqueue);
	if (size <= i)
		goto finish;
	string_getdirect(body, i++, &c);
	if (parser_logical_pathname_wordaster_p(c))
		goto next2;
	if (c == ';') {
		relative = 1;
		pushconstant_fileparse(pa, &queue, CONSTANT_KEYWORD_RELATIVE);
		goto discard;
	}
	if (c == '.')
		goto dot_type2;
	goto error;

next1:
	push_charqueue_local(local->local, charqueue, c);
	if (size <= i)
		goto finish_name;
	string_getdirect(body, i++, &c);
	if (parser_logical_pathname_wordaster_p(c))
		goto next1;
	if (c == ';')
		goto path;
	if (c == ':')
		goto host;
	if (c == '.')
		goto dot_type1;
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
	if (size <= i)
		goto finish;
	string_getdirect(body, i++, &c);
	if (parser_logical_pathname_wordaster_p(c))
		goto next2;
	if (c == ';')
		goto discard;
	if (c == '.')
		goto dot_type2;
	goto error;

next2:
	push_charqueue_local(local->local, charqueue, c);
	if (size <= i)
		goto finish_name;
	string_getdirect(body, i++, &c);
	if (parser_logical_pathname_wordaster_p(c))
		goto next2;
	if (c == ';')
		goto path;
	if (c == '.')
		goto dot_type1;
	goto error;

dot_type1:
	make_charqueue_fileparse(pa, charqueue, &pa->name);
	clear_charqueue(charqueue);

dot_type2:
	if (size <= i)
		goto finish_type;
	string_getdirect(body, i++, &c);
	if (parser_logical_pathname_wordaster_p(c)) {
		push_charqueue_local(local->local, charqueue, c);
		goto dot_type2;
	}
	if (c == '.')
		goto dot_version1;
	goto error;

dot_version1:
	make_charqueue_fileparse(pa, charqueue, &pa->type);
	clear_charqueue(charqueue);

dot_version2:
	if (size <= i)
		goto finish_version;
	string_getdirect(body, i++, &c);
	if (parser_logical_pathname_wordaster_p(c)) {
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
	nreverse(&pa->directory, queue);
	pa->endpos = i;
	return make_parse_logical_pathname_(pa);

error:
	return fmte_("Invalid logical-pathname ~S.", thing, NULL);
}

