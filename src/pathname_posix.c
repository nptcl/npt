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
	strvect_update_character_type(pos);

	return Result(ret, pos);;
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
	if (UTF8_buffer_clang(local, &x, x))
		return Result(ret, Unbound); /* encode error */
	posbody(x, (addr *)&body);
	body++; /* ~ */

	return passwd_home_pathname_(body, pa, x, ret);
}

static int string_home_pathname_(struct fileparse *pa, addr x, addr *ret)
{
	if (! string_equal_char(x, "~"))
		return name_home_pathname_(pa, x, ret);

	Return(env_home_pathname_(pa, x, &x));
	if (x != Unbound)
		return Result(ret, x);

	return uid_home_pathname_(pa, x, ret);
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
	split_home_pathname(localp, x, 0, ret);

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
	nconc2_safe(root, list, ret);
	return 0;
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
	GetConst(SYSTEM_UNIX, &pa->host);
	GetDevicePathname(pa->path, &pa->device);
	wild_value_pathname(pa->name, &pa->name);
	wild_value_pathname(pa->type, &pa->type);
	GetVersionPathname(pa->path, &pa->version);
	if (parser_home_directory_p_pathname(pa)) {
		Return(parser_home_directory_pathname_(pa));
	}
	pathname_fileparse_alloc(pa, 0);

	return 0;
}

_g int parser_unix_pathname_(struct fileparse *pa)
{
	int absolute, relative, logical, dp, check;
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
	Return(parser_make_unix_pathname_(pa));
	return 0;
}

