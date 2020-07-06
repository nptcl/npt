#include <errno.h>
#include "character_queue.h"
#include "cons.h"
#include "condition.h"
#include "constant.h"
#include "cons_list.h"
#include "encode.h"
#include "hashtable.h"
#include "pathname_localp.h"
#include "pathname_object.h"
#include "pathname_table.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

/*
 *  home directory
 */
static int parser_home_directory_p_pathname(struct fileparse *pa)
{
	addr list, x, check;
	unicode c;
	size_t size;

	/* (:relative "~" ...) */
	/* (:relative "~user" ...) */
	list = pa->directory;
	if (! consp_getcons(list, &x, &list))
		return 0;
	GetConst(KEYWORD_RELATIVE, &check);
	if (x != check)
		return 0;
	if (! consp_getcons(list, &x, &list))
		return 0;
	if (! stringp(x))
		return 0;
	string_length(x, &size);
	if (size == 0)
		return 0;
	string_getc(x, 0, &c);

	return c == '~';
}

#ifdef LISP_POSIX
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>

#ifdef LISP_DEBUG
#define PATHNAME_GETPWNAM_SIZE	(1UL << 1UL)
#else
#define PATHNAME_GETPWNAM_SIZE	(1UL << 16UL)
#endif

static int env_home_pathname(struct fileparse *pa, addr x, addr *ret)
{
	addr pos;

	GetConst(SYSTEM_SPECIAL_ENVIRONMENT, &pos);
	getspecial_local(pa->ptr, pos, &pos);
	if (pos == Unbound)
		return 0;
	if (! hashtablep(pos))
		return 0;

	return findvalue_char_hashtable(pos, "HOME", ret);
}

static int strvect_home_pathname(LocalRoot local, const byte *pw_dir, addr *ret)
{
	addr pos;
	size_t size;
	unicode *body;

	if (UTF8_null_strlen(pw_dir, &size))
		return 0; /* encode error */
	strvect_local(local, &pos, size);
	GetStringUnicode(pos, &body);
	if (UTF8_null_makeunicode(body, pw_dir))
		return 0; /* encode error */
	strvect_update_character_type(pos);
	*ret = pos;

	return 1;
}

static int passwd_home_pathname(
		const char *name, struct fileparse *pa, addr x, addr *ret)
{
	int check;
	char *data;
	struct passwd pwd, *result;
	size_t size;
	LocalRoot local;
	LocalStack stack;

	local = pa->ptr->local;
	size = PATHNAME_GETPWNAM_SIZE;
	for (;;) {
		push_local(local, &stack);
		data = (char *)lowlevel_local(local, size);
		if (name)
			check = getpwnam_r(name, &pwd, data, size, &result);
		else
			check = getpwuid_r(getuid(), &pwd, data, size, &result);
		if (check == 0) {
			/* not found */
			if (result == NULL)
				return 0;
			/* ok */
			break;
		}
		if (check != ERANGE)
			return 0; /* error */

		/* retry */
		rollback_local(local, stack);
		size <<= 1;
		if (size == 0)
			return 0; /* size error */
	}

	return strvect_home_pathname(local, (const byte *)pwd.pw_dir, ret);
}

static int uid_home_pathname(struct fileparse *pa, addr x, addr *ret)
{
	return passwd_home_pathname(NULL, pa, x, ret);
}

static int name_home_pathname(struct fileparse *pa, addr x, addr *ret)
{
	const char *body;
	LocalRoot local;

	/* username */
	local = pa->ptr->local;
	if (UTF8_buffer_clang(local, &x, x))
		return 0; /* encode error */
	posbody(x, (addr *)&body);
	body++; /* ~ */

	return passwd_home_pathname(body, pa, x, ret);
}

static int string_home_pathname(struct fileparse *pa, addr x, addr *ret)
{
	if (string_equal_char(x, "~")) {
		return env_home_pathname(pa, x, ret)
			|| uid_home_pathname(pa, x, ret);
	}
	else {
		return name_home_pathname(pa, x, ret);
	}
}

static void split_home_pathname(LocalpRoot localp, addr x, size_t i, addr *ret)
{
	addr car, cdr;
	unicode c;
	size_t size, a;
	LocalRoot local;

	/* ignore / */
	string_length(x, &size);
	for (;;) {
		/* end */
		if (size <= i) {
			*ret = Nil;
			return;
		}

		/* slash */
		string_getc(x, i, &c);
		if (c != '/')
			break;
		i++;
	}

	/* next */
	a = i;
	for (;;) {
		if (size <= i)
			break;
		string_getc(x, i, &c);
		if (c == '/')
			break;
		i++;
	}
	if (a == i) {
		*ret = Nil;
		return;
	}

	/* string */
	local = localp_alloc(localp);
	strvect_subseq_alloc(local, &car, x, a, i);
	split_home_pathname(localp, x, i, &cdr);
	cons_alloc(local, ret, car, cdr);
}

static int list_home_pathname(struct fileparse *pa, addr x, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	LocalpRoot localp;

	localp = pa->local;
	local = pa->ptr->local;
	push_local(local, &stack);

	/* get home directory */
	if (! string_home_pathname(pa, x, &x)) {
		rollback_local(local, stack);
		return 0;
	}

	/* split / */
	split_home_pathname(localp, x, 0, ret);

	/* heap only */
	if (! localp->localp)
		rollback_local(local, stack);

	/* ok */
	return 1;
}

static int make_home_pathname(struct fileparse *pa, addr list, addr *ret)
{
	addr root, x;

	GetCdr(list, &list); /* :relative */
	GetCons(list, &x, &list); /* "~user" */
	if (list_home_pathname(pa, x, &x) == 0)
		return 0;
	/* (:absolute ,@x . list) */
	GetConst(KEYWORD_ABSOLUTE, &root);
	cons_alloc(localp_alloc(pa->local), &root, root, x);
	nconc2_safe(root, list, ret);
	return 1;
}

static void parser_home_directory_pathname(struct fileparse *pa)
{
	addr list;

	/* (:relative "~user" ...) -> (:absolute home path ...) */
	list = pa->directory;
	if (make_home_pathname(pa, list, &list))
		pa->directory = list;
}
#else
static void parser_home_directory_pathname(struct fileparse *pa)
{
	/* do nothing */
}
#endif


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
	if (parser_home_directory_p_pathname(pa))
		parser_home_directory_pathname(pa);
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

