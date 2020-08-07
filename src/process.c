#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "encode.h"
#include "hashtable.h"
#include "process.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

static int find_environment_char_(Execute ptr, const char *key, addr *ret)
{
	addr pos;

	GetConst(SYSTEM_SPECIAL_ENVIRONMENT, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound)
		return Result(ret, Unbound);
	else
		return find_char_hashtable_(pos, key, ret);
}


/*
 *  run-process
 */
#if defined(LISP_POSIX)
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

static int run_process_utf8_(LocalRoot local, addr pos, char **ret)
{
	addr data;
	char *str;

	Return(UTF8_buffer_clang_(local, &data, pos));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF8 format ~S.", pos, NULL);
	}
	posbody(data, (addr *)&str);

	return Result(ret, str);
}

static int run_process_list_utf8_(LocalRoot local, addr var, addr list, char ***ret)
{
	char **array;
	addr pos;
	size_t size, i;

	Return(length_list_safe_(list, &size));
	size++;
	array = (char **)lowlevel_local(local, (size + 1) * sizeoft(char *));
	Return(run_process_utf8_(local, var, &(array[0])));
	for (i = 1; i < size; i++) {
		GetCons(list, &pos, &list);
		Return(run_process_utf8_(local, pos, &(array[i])));
	}
	array[i] = 0;

	return Result(ret, array);
}

static int run_process_posix_(LocalRoot local, addr var, addr args, addr *ret)
{
	int status;
	char *name;
	char **list;
	pid_t pid;

	if (! listp(args))
		conscar_local(local, &args, args);
	Return(run_process_utf8_(local, var, &name));
	Return(run_process_list_utf8_(local, var, args, &list));
	pid = fork();
	if (pid == -1)
		return fmte_("fork error", NULL);
	if (pid == 0) {
		/* child process */
		(void)execvp(name, list);
		return fmte_("execvp error", NULL);
	}

	/* wait */
	waitpid(pid, &status, 0);
	fixnum_heap(ret, (fixnum)status); /* heap */

	return 0;
}

_g int run_process_(LocalRoot local, addr var, addr args, addr rest, addr *ret)
{
	/* ignore rest */
	return run_process_posix_(local, var, args, ret);
}

#elif defined(LISP_WINDOWS)
static int run_process_delimited_(LocalRoot local, addr *ret, addr x, unicode z)
{
	size_t size, a, b;
	addr y;
	unicode c;

	string_length(x, &size);
	strvect_local(local, &y, size + 2);
	a = b = 0;
	Return(strvect_setc_(y, b++, z));
	while (a < size) {
		Return(string_getc_(x, a++, &c));
		Return(strvect_setc_(y, b++, c));
	}
	Return(strvect_setc_(y, b, z));

	return Result(ret, y);
}

static int run_process_windows_pathname_(LocalRoot local,
		addr pos, addr *ret, size_t *rsize)
{
	int space;
	unicode c;
	size_t size, i;

	/* space check */
	string_length(pos, &size);
	space = 0;
	for (i = 0; i < size; i++) {
		Return(string_getc_(pos, i, &c));
		if (c == ' ')
			space = 1;
		if (c == '"') {
			return fmte_("Don't include character #\\\" "
					"in Windows pathname ~S.", pos, NULL);
		}
	}

	/* encode */
	if (space) {
		Return(run_process_delimited_(local, ret, pos, '"'));
		*rsize = size + 2;
	}
	else {
		*ret = pos;
		*rsize = size;
	}

	return 0;
}

static int run_process_utf16_(LocalRoot local, addr pos, wchar_t **ret)
{
	addr data;
	wchar_t *str;

	Return(UTF16_buffer_clang_(local, &data, pos));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF16 format ~S.", pos, NULL);
	}
	posbody(data, (addr *)&str);

	return Result(ret, str);
}

static int run_process_list_utf16_(LocalRoot local, addr var, addr list, wchar_t **ret)
{
	int first;
	addr root, x, y;
	unicode c;
	size_t size, value, a, b;

	Return(run_process_windows_pathname_(local, var, &x, &size));
	conscar_local(local, &root, x);
	while (list != Nil) {
		Return_getcons(list, &x, &list);
		Return(run_process_windows_pathname_(local, x, &x, &value));
		size += value + 1;
		cons_local(local, &root, x, root);
	}
	nreverse(&root, root);

	strvect_local(local, &y, size);
	a = 0;
	for (first = 1; root != Nil; first = 0) {
		GetCons(root, &x, &root);
		if (first == 0) {
			Return(strvect_setc_(y, a++, ' '));
		}
		strvect_length(x, &value);
		for (b = 0; b < value; b++) {
			strvect_getc(x, b, &c);
			Return(strvect_setc_(y, a++, c));
		}
	}

	return run_process_utf16_(local, y, ret);
}

static int run_process_windows_(LocalRoot local, addr var, addr args, addr *ret)
{
	wchar_t *list;
	STARTUPINFO sinfo;
	PROCESS_INFORMATION pinfo;
	HANDLE child;
	DWORD status;

	if (! listp(args))
		conscar_local(local, &args, args);
	Return(run_process_list_utf16_(local, var, args, &list));
	cleartype(sinfo);
	cleartype(pinfo);
	if (! CreateProcess(NULL, list, NULL, NULL,
				FALSE, 0, NULL, NULL, &sinfo, &pinfo)) {
		return fmte_("Cannot run process ~S.", var, NULL);
	}
	child = pinfo.hProcess;
	if (! CloseHandle(pinfo.hThread))
		return fmte_("CloseHandle error.", NULL);

	/* wait */
	WaitForSingleObject(child, INFINITE);
	if (! GetExitCodeProcess(child, &status))
		return fmte_("GetExitCodeProcess error.", NULL);
	fixnum_heap(ret, (fixnum)status); /* heap */

	return 0;
}

_g int run_process_(LocalRoot local, addr var, addr args, addr rest, addr *ret)
{
	/* ignore rest */
	return run_process_windows_(local, var, args, ret);
}

#else
_g int run_process_(LocalRoot local, addr var, addr args, addr rest, addr *ret)
{
	return fmte_("This implementation does not support RUN-PROGRAM.", NULL);
}
#endif


/*
 *  ed-process
 */
static int find_ed_program_(Execute ptr, addr *ret)
{
	addr pos;

	/* *ed-program* */
	GetConst(SYSTEM_ED_PROGRAM, &pos);
	getspecial_local(ptr, pos, &pos);
	if (pos != Unbound)
		return Result(ret, pos);

	/* *environment* */
	return find_environment_char_(ptr, "EDITOR", ret);
}

#if defined(LISP_POSIX)
#define LISP_ED_PROCESS_DEFAULT  "vi"
#elif defined(LISP_WINDOWS)
#define LISP_ED_PROCESS_DEFAULT  "notepad.exe"
#else
#define LISP_ED_PROCESS_DEFAULT  "ed"
#endif

_g int ed_process_(Execute ptr, addr file)
{
	addr call, status;

	Return(find_ed_program_(ptr, &call));
	if (call == Unbound)
		strvect_char_heap(&call, LISP_ED_PROCESS_DEFAULT);

	return run_process_(ptr->local, call, file, Nil, &status);
}

