#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "encode.h"
#include "hashtable.h"
#include "process.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

static int find_environment_char(Execute ptr, const char *key, addr *ret)
{
	addr pos;

	GetConst(SYSTEM_SPECIAL_ENVIRONMENT, &pos);
	getspecial_local(ptr, pos, &pos);
	return pos != Unbound && findvalue_char_hashtable(pos, key, ret);
}


/*
 *  run-process
 */
#if defined(LISP_POSIX)
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

static char *run_process_utf8(LocalRoot local, addr pos)
{
	addr data;
	char *str;

	if (UTF8_buffer_clang(local, &data, pos)) {
		fmte("Invalid UTF8 format ~S.", pos, NULL);
		return NULL;
	}
	posbody(data, (addr *)&str);

	return str;
}

static char **run_process_list_utf8(LocalRoot local, addr var, addr list)
{
	char **array;
	addr pos;
	size_t size, i;

	size = length_list_safe(list) + 1;
	array = (char **)lowlevel_local(local, (size + 1) * sizeoft(char *));
	array[0] = run_process_utf8(local, var);
	for (i = 1; i < size; i++) {
		GetCons(list, &pos, &list);
		array[i] = run_process_utf8(local, pos);
	}
	array[i] = 0;

	return array;
}

static void run_process_posix(LocalRoot local, addr var, addr args, addr *ret)
{
	int status;
	char *name;
	char **list;
	pid_t pid;

	if (! listp(args))
		conscar_local(local, &args, args);
	name = run_process_utf8(local, var);
	list = run_process_list_utf8(local, var, args);
	pid = fork();
	if (pid == -1) {
		fmte("fork error", NULL);
		return;
	}
	if (pid == 0) {
		/* child process */
		(void)execvp(name, list);
		fmte("execvp error", NULL);
		return;
	}

	/* wait */
	waitpid(pid, &status, 0);
	fixnum_heap(ret, (fixnum)status); /* heap */
}

_g void run_process(LocalRoot local, addr var, addr args, addr rest, addr *ret)
{
	/* ignore rest */
	run_process_posix(local, var, args, ret);
}

#elif defined(LISP_WINDOWS)
static void run_process_delimited(LocalRoot local, addr *ret, addr x, unicode z)
{
	size_t size, a, b;
	addr y;
	unicode c;

	string_length(x, &size);
	strvect_local(local, &y, size + 2);
	a = b = 0;
	strvect_setc(y, b++, z);
	while (a < size) {
		string_getc(x, a++, &c);
		strvect_setc(y, b++, c);
	}
	strvect_setc(y, b, z);

	*ret = y;
}

static void run_process_windows_pathname(LocalRoot local,
		addr pos, addr *ret, size_t *rsize)
{
	int space;
	unicode c;
	size_t size, i;

	/* space check */
	string_length(pos, &size);
	space = 0;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		if (c == ' ')
			space = 1;
		if (c == '"') {
			fmte("Don't include character #\\\" in Windows pathname ~S.", pos, NULL);
			return;
		}
	}

	/* encode */
	if (space) {
		run_process_delimited(local, ret, pos, '"');
		*rsize = size + 2;
	}
	else {
		*ret = pos;
		*rsize = size;
	}
}

static wchar_t *run_process_utf16(LocalRoot local, addr pos)
{
	addr data;
	wchar_t *str;

	if (UTF16_buffer_clang(local, &data, pos)) {
		fmte("Invalid UTF16 format ~S.", pos, NULL);
		return NULL;
	}
	posbody(data, (addr *)&str);

	return str;
}

static wchar_t *run_process_list_utf16(LocalRoot local, addr var, addr list)
{
	int first;
	addr root, x, y;
	unicode c;
	size_t size, value, a, b;

	run_process_windows_pathname(local, var, &x, &size);
	conscar_local(local, &root, x);
	while (list != Nil) {
		getcons(list, &x, &list);
		run_process_windows_pathname(local, x, &x, &value);
		size += value + 1;
		cons_local(local, &root, x, root);
	}
	nreverse_list_unsafe(&root, root);

	strvect_local(local, &y, size);
	a = 0;
	for (first = 1; root != Nil; first = 0) {
		GetCons(root, &x, &root);
		if (first == 0)
			strvect_setc(y, a++, ' ');
		strvect_length(x, &value);
		for (b = 0; b < value; b++) {
			strvect_getc(x, b, &c);
			strvect_setc(y, a++, c);
		}
	}

	return run_process_utf16(local, y);
}

static void run_process_windows(LocalRoot local, addr var, addr args, addr *ret)
{
	wchar_t *list;
	STARTUPINFO sinfo;
	PROCESS_INFORMATION pinfo;
	HANDLE child;
	DWORD status;

	if (! listp(args))
		conscar_local(local, &args, args);
	list = run_process_list_utf16(local, var, args);
	cleartype(sinfo);
	cleartype(pinfo);
	if (! CreateProcess(NULL, list, NULL, NULL,
				FALSE, 0, NULL, NULL, &sinfo, &pinfo)) {
		fmte("Cannot run process ~S.", var, NULL);
		return;
	}
	child = pinfo.hProcess;
	if (! CloseHandle(pinfo.hThread)) {
		fmte("CloseHandle error.", NULL);
		return;
	}

	/* wait */
	WaitForSingleObject(child, INFINITE);
	if (! GetExitCodeProcess(child, &status)) {
		fmte("GetExitCodeProcess error.", NULL);
		return;
	}
	fixnum_heap(ret, (fixnum)status); /* heap */
}

_g void run_process(LocalRoot local, addr var, addr args, addr rest, addr *ret)
{
	/* ignore rest */
	run_process_windows(local, var, args, ret);
}

#else
_g void run_process(LocalRoot local, addr var, addr args, addr rest, addr *ret)
{
	fmte("This implementation does not support RUN-PROGRAM.", NULL);
}
#endif


/*
 *  ed-process
 */
static int find_ed_program(Execute ptr, addr *ret)
{
	addr pos;

	/* *ed-program* */
	GetConst(SYSTEM_ED_PROGRAM, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos != Unbound) {
		*ret = pos;
		return 1;
	}

	/* *environment* */
	if (find_environment_char(ptr, "EDITOR", &pos)) {
		*ret = pos;
		return 1;
	}

	/* not found */
	return 0;
}

#if defined(LISP_POSIX)
#define LISP_ED_PROCESS_DEFAULT  "vi"
#elif defined(LISP_WINDOWS)
#define LISP_ED_PROCESS_DEFAULT  "notepad.exe"
#else
#define LISP_ED_PROCESS_DEFAULT  "ed"
#endif

_g void ed_process(Execute ptr, addr file)
{
	addr call, status;

	if (find_ed_program(ptr, &call) == 0)
		strvect_char_heap(&call, LISP_ED_PROCESS_DEFAULT);
	run_process(ptr->local, call, file, Nil, &status);
}

