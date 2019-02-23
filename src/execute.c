#include <stdio.h>
#include <stdlib.h>
#include "alloc.h"
#include "lisp.h"
#include "local.h"
#include "execute.h"
#include "thread.h"

#ifdef LISP_DEBUG
#define EXECUTE_SIZE      2
#define EXECUTE_PLUS      3
#define EXECUTE_BODY      4
#else
#define EXECUTE_SIZE      0x010000
#define EXECUTE_PLUS      0x010000
#define EXECUTE_BODY      0x010000
#endif

#define SizeLimit (1024UL * 1024UL)

/*
 *  execute
 */
static struct execute **ExecuteArray = NULL;
static mutexlite Mutex;
static condlite Cond;
static size_t Size;
static size_t Position;

#ifdef LISP_DEGRADE
struct execute ***Degrade_execute_Execute(void) { return &ExecuteArray; }
size_t *Degrade_execute_Size(void) { return &Size; }
size_t *Degrade_execute_Position(void) { return &Position; }
#endif

/* thread local */
threadlocal ThreadLocal_Execute;
threadlocal ThreadLocal_Index;
threadlocal ThreadLocal_Local;


/*
 *  execute
 */
static void init_execute_local(void)
{
	make_threadlocal(&ThreadLocal_Execute);
	make_threadlocal(&ThreadLocal_Index);
	make_threadlocal(&ThreadLocal_Local);
}
static void free_execute_local(void)
{
	destroy_threadlocal(ThreadLocal_Execute);
	destroy_threadlocal(ThreadLocal_Index);
	destroy_threadlocal(ThreadLocal_Local);
}
void set_execute_local(struct execute *ptr)
{
	set_threadlocal(ThreadLocal_Execute, (void *)ptr);
	set_threadlocal(ThreadLocal_Index, (void *)&ptr->index);
	set_threadlocal(ThreadLocal_Local, (void *)ptr->local);
}

static struct execute *allocmembit(size_t index)
{
	struct execute *bit;

	bit = malloctype(struct execute);
	if (bit == NULL) {
		Debug("malloctype error");
		return NULL;
	}
	if (make_mutexlite(&bit->mutex)) {
		Debug("make_mutexlite error");
		free(bit);
		return NULL;
	}
	clearpoint(bit);
	bit->state = ThreadState_Empty;
	bit->index = index;
	bit->property = 0;
	bit->signal = ExecuteControl_Run;
	bit->data = NULL;
	bit->result = 0;

	return bit;
}

static void freemembit(struct execute *bit)
{
	destroy_mutexlite(&bit->mutex);
	free(bit);
}

static struct execute **allocbuffer(size_t size)
{
	size_t i, k;
	struct execute **ptr, *bit;

	ptr = mallocsize(struct execute *, size);
	if (ptr == NULL) {
		Debug("mallocsize error");
		return NULL;
	}
	for (i = 0; i < size; i++) {
		bit = allocmembit(i);
		if (bit == NULL) {
			Debug("allocmembit error");
			for (k = 0; k < i; k++)
				freemembit(ptr[k]);
			free(ptr);
			return NULL;
		}
		ptr[i] = bit;
	}

	return ptr;
}

static void freebuffer(struct execute **ptr)
{
	size_t i;

	if (ptr) {
		for (i = 0; i < Size; i++)
			freemembit(ptr[i]);
		free(ptr);
	}
}

static int init_mainthread(struct execute **ptr, size_t size)
{
	struct execute *bit;
	jmp_buf *exec;
	LocalRoot local;

	exec = malloctype(jmp_buf);
	if (exec == NULL) {
		Debug("malloctype error");
		return 1;
	}

	local = make_local(size);
	if (local == NULL) {
		Debug("make_local error");
		free(exec);
		return 1;
	}

	bit = ptr[0];  /* main thread */
	/* thread info */
	bit->state = ThreadState_Run;
	bit->routine = 0;
	cleartype(bit->handle);
#ifdef LISP_THREAD_WINDOWS
	cleartype(bit->handleid);
#endif

	/* lisp info */
	bit->exec = exec;
	bit->local = local;
	bit->control = Unbound;

	/* threadlocal */
	set_execute_local(bit);

	return 0;
}

int init_execute(size_t size)
{
	struct execute **ptr;

	if (ExecuteArray) {
		Debug("ExecuteArray error.");
		return 1;
	}

	/* size check */
	if (size < SizeLimit)
		size = SizeLimit;

	/* execute structure */
	ptr = allocbuffer(EXECUTE_SIZE);
	if (ptr == NULL) {
		Debug("allocbuffer error");
		return 1;
	}

	/* sync object */
	if (make_mutexlite(&Mutex)) {
		Debug("make_mutexlite error");
		goto error1;
	}
	make_condlite(&Cond);

	/* threadlocal */
	init_execute_local();

	/* thread index 0 */
	if (init_mainthread(ptr, size)) {
		Debug("init_mainthread error");
		goto error3;
	}

	/* Global variables */
	ExecuteArray = ptr;
	Size = EXECUTE_SIZE;
	Position = 1;

	return 0;

error3:
	freebuffer(ptr);
	destroy_condlite(&Cond);
error1:
	destroy_mutexlite(&Mutex);
	return 1;
}

void free_execute(void)
{
	size_t i;
	struct execute *bit;

	if (ExecuteArray == NULL) return;

	/* ExecuteArray */
	for (i = 0; i < Size; i++) {
		bit = ExecuteArray[i];
		if (bit->state != ThreadState_Empty) {
			free_local(bit->local);
			free(bit->exec);
		}
	}

	/* threadlocal */
	free_execute_local();

	/* Global variables */
	destroy_condlite(&Cond);
	destroy_mutexlite(&Mutex);
	freebuffer(ExecuteArray);
	ExecuteArray = 0;
	Size = 0;
}

static int extendmemory(void)
{
	size_t size, i, k;
	struct execute **ptr, *bit;

	size = Size + EXECUTE_PLUS;
	ptr = reallocsize(ExecuteArray, struct execute *, size);
	if (ptr == NULL) {
		Debug("reallocsize error");
		return 1;
	}
	for (i = Size; i < size; i++) {
		bit = allocmembit(i);
		if (bit == NULL) {
			Debug("allocmembit error");
			for (k = Size; k < i; k++)
				freemembit(ptr[k]);
			/* for realloc */
			ExecuteArray = ptr;
			/* try recovery */
			ptr = reallocsize(ExecuteArray, struct execute *, Size);
			if (ptr) ExecuteArray = ptr;
			return 1;
		}
		ptr[i] = bit;
	}
	ExecuteArray = ptr;
	Position = Size;
	Size = size;

	return 0;
}

static int findempty(struct execute **ret)
{
	size_t index;

	/* first try */
	for (index = Position; index < Size; index++) {
		if (ExecuteArray[index]->state == ThreadState_Empty) {
			Position = index;
			*ret = ExecuteArray[index];
			return 1;
		}
	}

	/* second try */
	for (index = 1; index < Position; index++) {
		if (ExecuteArray[index]->state == ThreadState_Empty) {
			Position = index;
			*ret = ExecuteArray[index];
			return 1;
		}
	}

	return 0;
}

static int findstate(struct execute **ptr)
{
	lock_mutexlite(&Mutex);

	/* find state=ThreadState_Empty */
	if (findempty(ptr)) goto finish;

	/* extend memory */
	if (extendmemory()) {
		Debug("expandmemory error");
		unlock_mutexlite(&Mutex);
		return 1;
	}
	*ptr = ExecuteArray[Position];

finish:
	(*ptr)->state = ThreadState_Run;
	unlock_mutexlite(&Mutex);

	return 0;
}

void setstate_execute(struct execute *ptr, enum ThreadState value)
{
	lock_mutexlite(&Mutex);
	ptr->state = value;
	unlock_mutexlite(&Mutex);
}

int make_execute(execfunction proc, struct execute **ret, size_t size)
{
	struct execute *ptr;

	/* size check */
	if (size < SizeLimit)
		size = SizeLimit;

	/* alloc */
	if (findstate(&ptr)) {
		Debug("findstate error");
		return 1;
	}

	/* thread */
	ptr->routine = proc;
	if (create_thread(proc, ptr)) {
		Debug("create_thread error");
		setstate_execute(ptr, ThreadState_Empty);
		return 1;
	}
	if (ret) *ret = ptr;

	return 0;
}

int join_execute(struct execute *ptr)
{
	int result;

	lock_mutexlite(&Mutex);
	switch (ptr->state) {
		case ThreadState_Empty:
			break;

		case ThreadState_Run:
			ptr->state = ThreadState_Join;
			goto join;

		case ThreadState_Finish:
			goto join;

		case ThreadState_Join:
			break;

		default:
			Debug("join_thread  state error");
			goto error;
	}
	unlock_mutexlite(&Mutex);
	return 0;

join:
	unlock_mutexlite(&Mutex);
	result = join_thread(&ptr->handle);
	if (result) {
		Debug("join_thread error");
		return 1;
	}
	setstate_execute(ptr, ThreadState_Empty);
	return 0;

error:
	unlock_mutexlite(&Mutex);
	return 1;
}

size_t count_execute(void)
{
	size_t i, count;

	count = 0;
	for (i = 0; i < Size; i++) {
		if (ExecuteArray[i]->state != ThreadState_Empty)
			count++;
	}

	return count;
}

int joinindex_execute(size_t index)
{
	return join_execute(ExecuteArray[index]);
}

struct execute *getexecute(size_t index)
{
	struct execute *result;

	lock_mutexlite(&Mutex);
	if (Size <= index) {
		unlock_mutexlite(&Mutex);
		Debug("index error");
		return NULL;
	}
	result = ExecuteArray[index];
	unlock_mutexlite(&Mutex);

	return result;
}

void exitexecute(struct execute *ptr, lispcode code)
{
	if (ptr == NULL || (! GetPropertyExecute(ptr, LISPPROP_JUMP))) {
		Debug("exitexecute error");
		exitindex(0, LISPCODE_ABORT);
	}
	exit_code(ptr, code);
}

void exitindex(size_t index, lispcode code)
{
	struct execute *ptr;

	ptr = getexecute(index);
	if (ptr == NULL) {
		Debug("getexecute error");
		abortindex(0);
		return;
	}
	exitexecute(ptr, code);
}

void abortexecute(struct execute *ptr)
{
	if (ptr == NULL || (! GetPropertyExecute(ptr, LISPPROP_JUMP))) {
		if (ptr == NULL || ptr->index == 0) {
			Debug("abort.");
			exit(1);
		}
		else {
			abortindex(0);
		}
	}
	SetPropertyExecute(ptr, LISPPROP_ABORT, 1);
	exitexecute(ptr, LISPCODE_ABORT);
}

void abortindex(size_t index)
{
	struct execute *ptr;

	ptr = getexecute(index);
	if (ptr == NULL) {
		Debug("getexecute error");
		if (index == 0) {
			Debug("abort.");
			exit(1);
		}
		else {
			abortindex(0);
		}
		return;
	}
	abortexecute(ptr);
}


/*
 *  codejump
 */
int begin_code_check(Execute ptr, lispcode *code)
{
	if (GetPropertyExecute(ptr, LISPPROP_JUMP)) {
		*code = LISPCODE_CONFLICT;
		return 0;
	}
	else {
		SetPropertyExecute(ptr, LISPPROP_JUMP, 1);
		return 1;
	}
}

void end_code(Execute ptr)
{
	SetPropertyExecute((ptr), LISPPROP_JUMP, 0);
	ClearJmpBuf(ptr->exec);
}
void end_code_thread(void)
{
	end_code(Execute_Thread);
}

int code_run_p(lispcode code)
{
	return code == LISPCODE_EXECUTE;
}
int code_end_p(lispcode code)
{
	return code == LISPCODE_SUCCESS;
}
int code_error_p(lispcode code)
{
	return code >= LISPCODE_ERROR;
}

void begin_switch_check(Execute ptr, codejump *code)
{
	Check(! GetPropertyExecute(ptr, LISPPROP_JUMP), "begin_switch error");
	code->ptr = ptr;
	CopyJmpBuf(&(code->jump), ptr->exec);
}

void end_switch(codejump *code)
{
	Execute ptr = code->ptr;
	CopyJmpBuf(ptr->exec, &(code->jump));
}

int codejump_run_p(codejump *code)
{
	return code_run_p(code->code);
}
int codejump_end_p(codejump *code)
{
	return code_end_p(code->code);
}
int codejump_error_p(codejump *code)
{
	return code_error_p(code->code);
}

void exit_code(Execute ptr, lispcode code)
{
	longjmp(*(ptr->exec), code);
}
void exit_code_thread(lispcode code)
{
	exit_code(Execute_Thread, code);
}

void break_code(Execute ptr)
{
	exit_code(ptr, LISPCODE_SUCCESS);
}
void break_code_thread(void)
{
	break_code(Execute_Thread);
}

void throw_code(Execute ptr, lispcode code)
{
	if (code_error_p(code))
		exit_code(ptr, code);
}
void throw_code_thread(lispcode code)
{
	throw_code(Execute_Thread, code);
}
void throw_switch(codejump *code)
{
	throw_code(code->ptr, code->code);
}


/*
 *  gc sync
 */
void gcstate_execute(void)
{
	size_t i;
	struct execute *ptr;

	lock_mutexlite(&Mutex);
	for (i = 0; i < Size; i++) {
		ptr = ExecuteArray[i];
		switch (ptr->state) {
			case ThreadState_Empty:
			case ThreadState_Finish:
			case ThreadState_Join:
				continue;

			case ThreadState_Run:
				ptr->state = ThreadState_Signal;
				break;

			default:
				Debug("state error");
				exitthis(LISPCODE_ERROR);
				break;
		}
	}
	broadcast_condlite(&Cond);
	unlock_mutexlite(&Mutex);
}

void gcstart_execute(struct execute *ptr)
{
	size_t i;

	lock_mutexlite(&Mutex);
	ptr->state = ThreadState_GcStart;
loop:
	for (i = 0; i < Size; i++) {
		switch (ExecuteArray[i]->state) {
			case ThreadState_Signal:
				wait_condlite(&Cond, &Mutex);
				goto loop;

			case ThreadState_Run:
				Debug("state error");
				exitthis(LISPCODE_ERROR);
				break;

			default:
				break;
		}
	}
	unlock_mutexlite(&Mutex);
}

void gcwait_execute(struct execute *ptr)
{
	lock_mutexlite(&Mutex);
	while (ptr->state != ThreadState_Run)
		wait_condlite(&Cond, &Mutex);
	unlock_mutexlite(&Mutex);
}

void gcend_execute(void)
{
	size_t i;
	struct execute *ptr;

	lock_mutexlite(&Mutex);
	for (i = 0; i < Size; i++) {
		ptr = ExecuteArray[i];
		if (ptr->state == ThreadState_GcStart)
			ptr->state = ThreadState_Run;
	}
	broadcast_condlite(&Cond);
	unlock_mutexlite(&Mutex);
}

void foreach_execute(void (*call)(struct execute *))
{
	size_t i;
	struct execute *ptr;

	for (i = 0; i < Position; i++) {
		ptr = ExecuteArray[i];
		if (ptr->state != ThreadState_Empty)
			call(ptr);
	}
}


/*
 *  exit
 */
void exit_execute(int value)
{
	Execute ptr;

	ptr = getexecute(0);
	ptr->result = value;
	exitexecute(ptr, LISPCODE_EXIT);
}

