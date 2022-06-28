#include "closget.h"
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
#include "pointer_type.h"
#include "object.h"

#include <dlfcn.h>
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

static int run_process_unix_(Execute ptr, addr var, addr args, addr *ret)
{
	int status;
	char *name;
	char **list;
	pid_t pid;
	LocalRoot local;

	local = ptr->local;
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

int run_process_arch_(Execute ptr, addr instance, addr *ret)
{
	addr var, args;

	Return(ClosGetConst_(instance, KEYWORD_PROGRAM, &var));
	Return(ClosGetConst_(instance, KEYWORD_ARGS, &args));
	return run_process_unix_(ptr, var, args, ret);
}


/*
 *  dlfile
 */
#define LISP_PROCESS_FILE		(('S' << 8U) | 'O')
#define LISP_PROCESS_CALL		(('S' << 16U) | ('Y' << 8U) | 'M')

struct dlfile_struct {
	uint32_t magic;
	int openp;
	void *handle;
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

static void *dlopen_open_handle_(const char *utf8)
{
	int callp;
	void *handle;
	void *proc;
	lisp_dlfile_array array;
	int (*call)(lisp_dlfile_array);

	/* open */
	handle = dlopen(utf8, RTLD_LAZY);
	if (handle == NULL)
		return NULL;
	callp = 0;
	lisp_dlfile_make(array);

	/* lisp_dlfile_main */
	proc = dlsym(handle, "lisp_dlfile_main");
	if (proc) {
		call = (int (*)(lisp_dlfile_array))proc;
		if ((*call)(array))
			goto error;
		callp = 1;
	}

	/* lisp_somain */
	proc = dlsym(handle, "lisp_somain");
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
	dlclose(handle);
	return NULL;
}

int dlopen_arch_(Execute ptr, addr pos, addr *ret)
{
	addr file, paper;
	const char *utf8;
	void *handle;
	LocalRoot local;
	LocalStack stack;
	struct dlfile_struct str;

	/* dlopen */
	local = ptr->local;
	push_local(local, &stack);
	Return(physical_pathname_local_(ptr, pos, &pos));
	Return(name_pathname_heap_(ptr, pos, &file));
	gchold_push_local(local, file);

	Return(run_process_utf8_(local, file, (char **)&utf8));
	handle = dlopen_open_handle_(utf8);
	rollback_local(local, stack);
	if (handle == NULL) {
		*ret = Nil;
		return fmte_("dlopen error, ~S.", file, NULL);
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

	if (dlclose(str->handle) != 0) {
		*ret = Nil;
		paper_get_array(pos, 0, &file);
		return fmte_("dlclose error, ~S.", file, NULL);
	}
	str->openp = 0;
	return Result(ret, T);
}

int dlsym_arch_(Execute ptr, addr pos, addr name, enum CallBind_index type, addr *ret)
{
	addr file, paper;
	const char *utf8;
	void *sym;
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
	sym = dlsym(str.handle, utf8);
	rollback_local(local, stack);
	if (sym == NULL) {
		*ret = Nil;
		return fmte_("dlsym error, ~S.", name, NULL);
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

