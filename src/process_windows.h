#include "clos.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_callbind.h"
#include "control_object.h"
#include "execute.h"
#include "extern_dlfile.h"
#include "encode.h"
#include "hold.h"
#include "local.h"
#include "paper.h"
#include "pathname.h"
#include "object.h"
#include "paper.h"
#include "pointer_type.h"
#include "strtype.h"
#include "strvect.h"

#include <windows.h>

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
	HANDLE hProcess, hThread;
	DWORD status;
	LocalRoot local;

	local = ptr->local;
	if (! listp(args))
		conscar_local(local, &args, args);
	Return(run_process_list_utf16_(local, var, args, &list));
	cleartype(sinfo);
	cleartype(pinfo);
	sinfo.cb = sizeof(sinfo);
	if (! CreateProcessW(NULL, list, NULL, NULL,
				FALSE, 0, NULL, NULL, &sinfo, &pinfo)) {
		return fmte_("Cannot run process ~S.", var, NULL);
	}
	hProcess = pinfo.hProcess;
	hThread = pinfo.hThread;

	/* wait */
	WaitForSingleObject(hProcess, INFINITE);
	if (! GetExitCodeProcess(hProcess, &status)) {
		(void)CloseHandle(hProcess);
		(void)CloseHandle(hThread);
		return fmte_("GetExitCodeProcess error.", NULL);
	}
	fixnum_heap(ret, (fixnum)status); /* heap */

	/* Close */
	(void)CloseHandle(hProcess);
	(void)CloseHandle(hThread);

	return 0;
}

int run_process_arch_(Execute ptr, addr instance, addr *ret)
{
	addr var, args;

	Return(ClosGetConst_(instance, KEYWORD_PROGRAM, &var));
	Return(ClosGetConst_(instance, KEYWORD_ARGS, &args));
	return run_process_windows_(ptr, var, args, ret);
}


/*
 *  dlfile
 */
#define LISP_PROCESS_FILE		(('D' << 16U) | ('L' << 8U) | 'L')
#define LISP_PROCESS_CALL		(('F' << 16U) | ('A' << 8U) | 'R')

struct dlfile_struct {
	uint32_t magic;
	int openp;
	HMODULE handle;
};

struct dlcall_struct {
	uint32_t magic;
	struct callbind_struct call;
};

int dlfile_check_arch_(addr pos, addr *ret, int *openp)
{
	struct dlfile_struct str;
	size_t size;

	*openp = 0;
	if (! paperp(pos))
		return Result(ret, Nil);
	paper_get_memory(pos, 0, sizeof(struct dlfile_struct), &str, &size);
	if (size != sizeof(struct dlfile_struct))
		return Result(ret, Nil);
	if (str.magic != LISP_PROCESS_FILE)
		return Result(ret, Nil);

	*openp = (str.openp != 0);
	paper_get_array(pos, 1, ret);
	return 0;
}

static HMODULE dlopen_open_handle_(const WCHAR *utf16)
{
	int callp;
	HMODULE handle;
	FARPROC proc;
	lisp_dlfile_array array;
	int (*call)(lisp_dlfile_array);

	/* open */
	handle = LoadLibraryW(utf16);
	if (handle == NULL)
		return NULL;
	callp = 0;
	lisp_dlfile_make(array);

	/* lisp_dlfile_main */
	proc = GetProcAddress(handle, "lisp_dlfile_main");
	if (proc) {
		call = (int (*)(lisp_dlfile_array))proc;
		if ((*call)(array))
			goto error;
		callp = 1;
	}

	/* lisp_dllmain */
	proc = GetProcAddress(handle, "lisp_dllmain");
	if (proc) {
		call = (int (*)(lisp_dlfile_array))proc;
		if ((*call)(array))
			goto error;
		callp = 1;
	}

	/* error check */
	if (callp == 0)
		goto error;
	return handle;

error:
	FreeLibrary(handle);
	return NULL;
}

int dlopen_arch_(Execute ptr, addr pos, addr *ret)
{
	addr file, paper;
	const wchar_t *utf16;
	HMODULE handle;
	LocalRoot local;
	LocalStack stack;
	struct dlfile_struct str;

	/* dlopen */
	local = ptr->local;
	push_local(local, &stack);
	Return(physical_pathname_local_(ptr, pos, &pos));
	Return(name_pathname_heap_(ptr, pos, &file));
	gchold_push_local(local, file);

	Return(run_process_utf16_(local, file, (wchar_t **)&utf16));
	handle = dlopen_open_handle_(utf16);
	rollback_local(local, stack);
	if (handle == NULL) {
		*ret = Nil;
		return fmte_("LoadLibraryW error, ~S.", file, NULL);
	}

	/* paper */
	cleartype(str);
	str.magic = LISP_PROCESS_FILE;
	str.openp = 1;
	str.handle = handle;
	Return(paper_arraybody_heap_(&paper, 2, sizeof(struct dlfile_struct)));
	paper_set_array(paper, 0, file);
	paper_set_array(paper, 1, pos);
	paper_set_memory(paper, 0, sizeof(struct dlfile_struct), &str, NULL);
	return Result(ret, paper);
}

int dlclose_arch_(Execute ptr, addr pos, addr *ret)
{
	addr file;
	struct dlfile_struct *str;

	paper_ptr_body_unsafe(pos, (void **)&str);
	if (! str->openp)
		return Result(ret, Nil); /* already closed */

	if (FreeLibrary(str->handle) == 0) {
		*ret = Nil;
		paper_get_array(pos, 0, &file);
		return fmte_("FreeLibrary error, ~S.", file, NULL);
	}
	str->openp = 0;
	return Result(ret, T);
}

int dlsym_arch_(Execute ptr, addr pos, addr name, enum CallBind_index type, addr *ret)
{
	addr file, paper;
	const char *utf8;
	FARPROC sym;
	LocalRoot local;
	LocalStack stack;
	struct dlfile_struct str;
	struct dlcall_struct call;

	/* dlfile */
	paper_get_memory(pos, 0, sizeof(struct dlfile_struct), &str, NULL);
	if (! str.openp) {
		*ret = Nil;
		paper_get_array(pos, 0, &file);
		return fmte_("dlfile is already closed, ~S.", file, NULL);
	}

	/* pointer */
	local = ptr->local;
	push_local(local, &stack);
	Return(run_process_utf8_(local, name, (char **)&utf8));
	sym = GetProcAddress(str.handle, utf8);
	rollback_local(local, stack);
	if (sym == NULL) {
		*ret = Nil;
		return fmte_("GetProcAddress error, ~S.", name, NULL);
	}

	/* call */
	cleartype(call);
	call.magic = LISP_PROCESS_CALL;
	call.call.type = type;
	call.call.call.pvoid = (void *)sym;
	Return(paper_arraybody_heap_(&paper, 2, sizeof(struct dlcall_struct)));
	paper_set_array(paper, 0, pos);
	paper_set_array(paper, 1, name);
	paper_set_memory(paper, 0, sizeof(struct dlcall_struct), &call, NULL);
	return Result(ret, paper);
}


/*
 *  dlcall
 */
static int dlcall_arch_callbind_(Execute ptr,
	addr name, addr args, struct callbind_struct *bind)
{
	addr control;

	push_control(ptr, &control);
	SetControl(ptr->control, Control_Cons, args);
	SetControl(ptr->control, Control_ConsTail, Nil);
	(void)call_callbind_function_(ptr, name, bind);
	return pop_control_(ptr, control);
}

int dlcall_arch_(Execute ptr, addr pos, addr args)
{
	addr dlfile, name;
	struct dlfile_struct *str;
	struct dlcall_struct *call;

	/* sym */
	paper_ptr_body_unsafe(pos, (void **)&call);
	if (call->magic != LISP_PROCESS_CALL)
		return fmte_("Invalid dlsym object, ~S.", pos, NULL);
	paper_get_array(pos, 0, &dlfile);
	paper_get_array(pos, 1, &name);

	/* dlfile */
	paper_ptr_body_unsafe(dlfile, (void **)&str);
	if (! str->openp)
		return fmte_("dlfile ~S is already closed.", dlfile, NULL);

	/* call */
	return dlcall_arch_callbind_(ptr, name, args, &(call->call));
}
