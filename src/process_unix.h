#include "clos.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_callbind.h"
#include "control_object.h"
#include "execute.h"
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


#ifdef LISP_DYNAMIC_LINK
/*
 *  dlfile
 */
#define LISP_PROCESS_FILE		(('S' << 8U) | 'O')
#define LISP_PROCESS_CALL		(('S' << 16U) | ('Y' << 8U) | 'M')

struct dlfile_struct {
	uint32_t magic;
	int closed;
	void *handle;
};

struct dlcall_struct {
	uint32_t magic;
	struct callbind_struct call;
};

int dlfile_check_arch_(addr pos, int *ret)
{
	struct dlfile_struct str;
	size_t size;

	if (! paperp(pos)) {
		*ret = 0;
		return fmte_("Invalid object, ~S.", pos, NULL);
	}
	paper_get_memory(pos, 0, sizeof(struct dlfile_struct), &str, &size);
	if (size != sizeof(struct dlfile_struct))
		return Result(ret, 0);
	if (str.magic != LISP_PROCESS_FILE)
		return Result(ret, 0);

	return Result(ret, 1);
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
	Return(name_physical_heap_(ptr, pos, &file));
	gchold_push_local(local, file);

	Return(run_process_utf8_(local, file, (char **)&utf8));
	handle = dlopen(utf8, RTLD_LAZY);
	rollback_local(local, stack);
	if (handle == NULL) {
		*ret = Nil;
		return fmte_("dlopen error, ~S.", file, NULL);
	}

	/* paper */
	cleartype(str);
	str.magic = LISP_PROCESS_FILE;
	str.closed = 0;
	str.handle = handle;
	Return(paper_arraybody_heap_(&paper, 1, sizeof(struct dlfile_struct)));
	paper_set_array(paper, 0, file);
	paper_set_memory(paper, 0, sizeof(struct dlfile_struct), &str, NULL);
	return Result(ret, paper);
}

int dlclose_arch_(Execute ptr, addr pos, addr *ret)
{
	addr file;
	struct dlfile_struct str;

	paper_get_memory(pos, 0, sizeof(struct dlfile_struct), &str, NULL);
	if (str.closed)
		return Result(ret, Nil); /* already closed */

	if (dlclose(str.handle) != 0) {
		*ret = Nil;
		paper_get_array(pos, 0, &file);
		return fmte_("dlclose error, ~S.", file, NULL);
	}
	str.closed = 1;
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
	if (str.closed) {
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
	if (str->closed)
		return fmte_("dlfile ~S is already closed.", dlfile, NULL);

	/* call */
	return dlcall_arch_callbind_(ptr, name, args, &(call->call));
}

#else
/*
 *  Error
 */
int dlfile_check_arch_(addr pos, int *ret)
{
	*ret = 0;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlopen_arch_(Execute ptr, addr pos, addr *ret)
{
	*ret = Nil;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlclose_arch_(Execute ptr, addr pos, addr *ret)
{
	*ret = Nil;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlsym_arch_(Execute ptr, addr pos, addr name, enum CallBind_index type, addr *ret)
{
	*ret = Nil;
	return fmte_("This implementation does not support DLFILE.", NULL);
}

int dlcall_arch_(Execute ptr, addr pos, addr args)
{
	return fmte_("This implementation does not support DLCALL.", NULL);
}
#endif

