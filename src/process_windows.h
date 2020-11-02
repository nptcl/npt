#include "clos.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "execute.h"
#include "encode.h"
#include "local.h"
#include "object.h"
#include "strtype.h"
#include "strvect.h"

#include <windows.h>

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

static int run_process_windows_(Execute ptr, addr var, addr args, addr *ret)
{
	wchar_t *list;
	STARTUPINFOW sinfo;
	PROCESS_INFORMATION pinfo;
	HANDLE child;
	DWORD status;
	LocalRoot local;

	local = ptr->local;
	if (! listp(args))
		conscar_local(local, &args, args);
	Return(run_process_list_utf16_(local, var, args, &list));
	cleartype(sinfo);
	cleartype(pinfo);
	if (! CreateProcessW(NULL, list, NULL, NULL,
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

_g int run_process_arch_(Execute ptr, addr instance, addr *ret)
{
	addr var, args;

	Return(ClosGetConst_(instance, KEYWORD_PROGRAM, &var));
	Return(ClosGetConst_(instance, KEYWORD_ARGS, &args));
	return run_process_windows_(ptr, var, args, ret);
}

