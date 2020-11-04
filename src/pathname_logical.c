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

static int parser_logical_host_(struct fileparse *pa, addr queue)
{
	int check;
	LocalRoot local;
	addr pos;

	local = localp_alloc(pa->local);
	make_charqueue_alloc(local, queue, &pos);
	Return(string_upper_p_(pos, &check));
	if (! check) {
		Return(string_upper_alloc_(local, pos, &pos));
	}
	pa->host = pos;

	return 0;
}

_g int parser_logical_pathname_(struct fileparse *pa)
{
	int absolute, relative, dp1, dp2;
	unicode c;
	LocalpRoot local;
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
	absolute = relative = dp1 = dp2 = 0;
	i = pa->start;
	GetConst(KEYWORD_NEWEST, &pa->version);

	/* start */
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
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
	Return(parser_logical_host_(pa, charqueue));
	clear_charqueue(charqueue);
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
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
	Return(push_charqueue_local_(local->local, charqueue, c));
	if (size <= i)
		goto finish_name;
	Return(string_getc_(thing, i++, &c));
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
	Return(pushdirectory_fileparse_(pa, &queue, temp));

discard:
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
	if (parser_logical_pathname_wordaster_p(c))
		goto next2;
	if (c == ';')
		goto discard;
	if (c == '.')
		goto dot_type2;
	goto error;

next2:
	Return(push_charqueue_local_(local->local, charqueue, c));
	if (size <= i)
		goto finish_name;
	Return(string_getc_(thing, i++, &c));
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
	Return(string_getc_(thing, i++, &c));
	if (parser_logical_pathname_wordaster_p(c)) {
		Return(push_charqueue_local_(local->local, charqueue, c));
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
	Return(string_getc_(thing, i++, &c));
	if (parser_logical_pathname_wordaster_p(c)) {
		Return(push_charqueue_local_(local->local, charqueue, c));
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
	if (pa->junk) {
		pa->result = Nil;
		pa->endpos = i? i - 1: 0;
		return 0;
	}

	return call_parse_error_(NULL);
}

