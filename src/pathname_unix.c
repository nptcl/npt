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
static int parser_home_directory_p_pathname_(struct fileparse *pa, int *ret)
{
	addr list, x, check;
	unicode c;
	size_t size;

	/* (:relative "~" ...) */
	/* (:relative "~user" ...) */
	list = pa->directory;
	if (! consp_getcons(list, &x, &list))
		return Result(ret, 0);
	GetConst(KEYWORD_RELATIVE, &check);
	if (x != check)
		return Result(ret, 0);
	if (! consp_getcons(list, &x, &list))
		return Result(ret, 0);
	if (! stringp(x))
		return Result(ret, 0);
	string_length(x, &size);
	if (size == 0)
		return Result(ret, 0);
	Return(string_getc_(x, 0, &c));

	return Result(ret, c == '~');
}

#ifdef LISP_UNIX
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>

#ifdef LISP_DEBUG
#define PATHNAME_GETPWNAM_SIZE	(1UL << 1UL)
#else
#define PATHNAME_GETPWNAM_SIZE	(1UL << 16UL)
#endif

static int env_home_pathname_(struct fileparse *pa, addr x, addr *ret)
{
	addr pos;

	GetConst(SYSTEM_SPECIAL_ENVIRONMENT, &pos);
	getspecial_local(pa->ptr, pos, &pos);
	if (pos == Unbound)
		return Result(ret, Unbound);
	if (! hashtablep(pos))
		return Result(ret, Unbound);

	return find_char_hashtable_(pos, "HOME", ret);
}

static int strvect_home_pathname_(LocalRoot local, const byte *pw_dir, addr *ret)
{
	addr pos;
	size_t size;
	unicode *body;

	if (UTF8_null_strlen(pw_dir, &size))
		return Result(ret, Unbound); /* encode error */
	strvect_local(local, &pos, size);
	GetStringUnicode(pos, &body);
	if (UTF8_null_makeunicode(body, pw_dir))
		return Result(ret, Unbound); /* encode error */

	return Result(ret, pos);
}

static int passwd_home_pathname_(
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
				return Result(ret, Unbound);
			/* ok */
			break;
		}
		if (check != ERANGE)
			return Result(ret, Unbound); /* error */

		/* retry */
		rollback_local(local, stack);
		size <<= 1;
		if (size == 0)
			return Result(ret, Unbound); /* size error */
	}

	return strvect_home_pathname_(local, (const byte *)pwd.pw_dir, ret);
}

static int uid_home_pathname_(struct fileparse *pa, addr x, addr *ret)
{
	return passwd_home_pathname_(NULL, pa, x, ret);
}

static int name_home_pathname_(struct fileparse *pa, addr x, addr *ret)
{
	const char *body;
	LocalRoot local;

	/* username */
	local = pa->ptr->local;
	Return(UTF8_buffer_clang_(local, &x, x));
	if (x == Unbound)
		return Result(ret, Unbound); /* encode error */
	posbody(x, (addr *)&body);
	body++; /* ~ */

	return passwd_home_pathname_(body, pa, x, ret);
}

static int string_home_pathname_(struct fileparse *pa, addr x, addr *ret)
{
	int check;

	Return(string_equal_char_(x, "~", &check));
	if (! check)
		return name_home_pathname_(pa, x, ret);

	Return(env_home_pathname_(pa, x, &x));
	if (x != Unbound)
		return Result(ret, x);

	return uid_home_pathname_(pa, x, ret);
}

static int split_home_pathname_(LocalpRoot localp, addr x, size_t i, addr *ret)
{
	addr car, cdr;
	unicode c;
	size_t size, a;
	LocalRoot local;

	/* ignore / */
	string_length(x, &size);
	for (;;) {
		/* end */
		if (size <= i)
			return Result(ret, Nil);

		/* slash */
		Return(string_getc_(x, i, &c));
		if (c != '/')
			break;
		i++;
	}

	/* next */
	a = i;
	for (;;) {
		if (size <= i)
			break;
		Return(string_getc_(x, i, &c));
		if (c == '/')
			break;
		i++;
	}
	if (a == i)
		return Result(ret, Nil);

	/* string */
	local = localp_alloc(localp);
	Return(strvect_subseq_alloc_(local, &car, x, a, i));
	Return(split_home_pathname_(localp, x, i, &cdr));
	cons_alloc(local, ret, car, cdr);

	return 0;
}

static int list_home_pathname_(struct fileparse *pa, addr x, addr *ret)
{
	LocalRoot local;
	LocalStack stack;
	LocalpRoot localp;

	localp = pa->local;
	local = pa->ptr->local;
	push_local(local, &stack);

	/* get home directory */
	Return(string_home_pathname_(pa, x, &x));
	if (x == Unbound) {
		rollback_local(local, stack);
		return Result(ret, Unbound);
	}

	/* split / */
	Return(split_home_pathname_(localp, x, 0, ret));

	/* heap only */
	if (! localp->localp)
		rollback_local(local, stack);

	/* ok */
	return 0;
}

static int make_home_pathname_(struct fileparse *pa, addr list, addr *ret)
{
	addr root, x;

	GetCdr(list, &list); /* :relative */
	GetCons(list, &x, &list); /* "~user" */
	Return(list_home_pathname_(pa, x, &x));
	if (x == Unbound)
		return Result(ret, Unbound);
	/* (:absolute ,@x . list) */
	GetConst(KEYWORD_ABSOLUTE, &root);
	cons_alloc(localp_alloc(pa->local), &root, root, x);
	return nconc2_safe_(root, list, ret);
}

static int parser_home_directory_pathname_(struct fileparse *pa)
{
	addr list;

	/* (:relative "~user" ...) -> (:absolute home path ...) */
	list = pa->directory;
	Return(make_home_pathname_(pa, list, &list));
	if (list != Unbound)
		pa->directory = list;

	return 0;
}
#else
static int parser_home_directory_pathname_(struct fileparse *pa)
{
	/* do nothing */
	return 0;
}
#endif


/*
 *  unix pathname
 */
static int parser_make_unix_pathname_(struct fileparse *pa)
{
	int check;

	GetConst(SYSTEM_UNIX, &pa->host);
	GetDevicePathname(pa->path, &pa->device);
	Return(wild_value_pathname_(pa->name, &pa->name));
	Return(wild_value_pathname_(pa->type, &pa->type));
	GetVersionPathname(pa->path, &pa->version);
	Return(parser_home_directory_p_pathname_(pa, &check));
	if (check) {
		Return(parser_home_directory_pathname_(pa));
	}
	pathname_fileparse_alloc(pa, 0);

	return 0;
}

int parser_unix_pathname_(struct fileparse *pa)
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

	/* start */
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
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
	Return(push_charqueue_local_(local->local, charqueue, c));
	ni++;
	goto next1;

first:
	if (size <= i)
		goto finish;
	Return(string_getc_(thing, i++, &c));
	if (c == '/')
		goto first;
	if (c == '.') {
		di = ni;
		dp = 1;
	}
	Return(push_charqueue_local_(local->local, charqueue, c));
	ni++;
	goto next1;

next1:
	if (size <= i)
		goto name_finish;
	Return(string_getc_(thing, i++, &c));
	if (c == '/')
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

name_finish:
	make_charqueue_fileparse(pa, charqueue, &pa->name);
	if (di && dp) {
		Return(nametype_pathname_(pa, di));
	}
	goto finish;

finish:
	nreverse(&pa->directory, queue);
	pa->endpos = i;
	Return(parser_make_unix_pathname_(pa));
	return 0;
}

