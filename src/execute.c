#include "alloc.h"
#include "build.h"
#include "define.h"
#include "execute.h"
#include "execute_object.h"
#include "local.h"
#include "memory.h"
#include "thread.h"

/* constant */
#ifdef LISP_DEBUG
#define EXECUTE_SIZE      2
#define EXECUTE_PLUS      3
#else
#define EXECUTE_SIZE      0x0100
#define EXECUTE_PLUS      0x0100
#endif

#define SizeLimit (1024UL * 1024UL)

/*
 *  execute
 */
static struct execute **Execute_Array = NULL;
static mutexlite Execute_Mutex;
static condlite Execute_Cond;
static size_t Execute_Size;
static size_t Execute_Position;
static size_t Execute_LocalSize;

#ifdef LISP_DEGRADE
struct execute ***Degrade_execute_Execute(void) { return &Execute_Array; }
size_t *Degrade_execute_Size(void) { return &Execute_Size; }
size_t *Degrade_execute_Position(void) { return &Execute_Position; }
#endif


/*
 *  Thread
 */
static void init_execute_local(void)
{
	lispd_make_threadlocal(&ThreadLocal_Execute);
	lispd_make_threadlocal(&ThreadLocal_Index);
	lispd_make_threadlocal(&ThreadLocal_Local);
}
static void free_execute_local(void)
{
	lispd_destroy_threadlocal(ThreadLocal_Execute);
	lispd_destroy_threadlocal(ThreadLocal_Index);
	lispd_destroy_threadlocal(ThreadLocal_Local);
}
void set_execute_local(struct execute *ptr)
{
	lispd_set_threadlocal(ThreadLocal_Execute, (void *)ptr);
	lispd_set_threadlocal(ThreadLocal_Index, (void *)&ptr->index);
	lispd_set_threadlocal(ThreadLocal_Local, (void *)ptr->local);
}

static struct execute *alloc_membit(size_t index)
{
	struct execute *bit;

	bit = malloctype(struct execute);
	if (bit == NULL) {
		Debug("malloctype error");
		return NULL;
	}
	if (lispd_make_mutexlite(&bit->mutex)) {
		Debug("lispd_make_mutexlite error");
		free(bit);
		return NULL;
	}
	clearpoint(bit);
	bit->state = ThreadState_Empty;
	bit->index = index;
	bit->result = 0;

	return bit;
}

static void free_membit(struct execute *bit)
{
	lispd_destroy_mutexlite(&bit->mutex);
	free(bit);
}

static struct execute **alloc_buffer_membit(size_t size)
{
	size_t i, k;
	struct execute **ptr, *bit;

	ptr = mallocsize(struct execute *, size);
	if (ptr == NULL) {
		Debug("mallocsize error");
		return NULL;
	}
	for (i = 0; i < size; i++) {
		bit = alloc_membit(i);
		if (bit == NULL) {
			Debug("alloc_membit error");
			for (k = 0; k < i; k++)
				free_membit(ptr[k]);
			free(ptr);
			return NULL;
		}
		ptr[i] = bit;
	}

	return ptr;
}

static void free_buffer_membit(struct execute **ptr)
{
	size_t i;

	if (ptr) {
		for (i = 0; i < Execute_Size; i++)
			free_membit(ptr[i]);
		free(ptr);
	}
}


/*
 *  initialize
 */
static int init_execute_main(struct execute **ptr, size_t size)
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

	/* values */
	init_execute_values(bit);

	/* threadlocal */
	set_execute_local(bit);

	return 0;
}

int init_execute(size_t size)
{
	struct execute **ptr;

	if (Execute_Array) {
		Debug("Execute_Array error.");
		return 1;
	}

	/* size check */
	if (size < SizeLimit)
		size = SizeLimit;

	/* execute structure */
	ptr = alloc_buffer_membit(EXECUTE_SIZE);
	if (ptr == NULL) {
		Debug("alloc_buffer_membit error");
		return 1;
	}

	/* sync object */
	if (lispd_make_mutexlite(&Execute_Mutex)) {
		Debug("lispd_make_mutexlite error");
		goto error1;
	}
	lispd_make_condlite(&Execute_Cond);

	/* threadlocal */
	init_execute_local();

	/* thread index 0 */
	if (init_execute_main(ptr, size)) {
		Debug("init_execute_main error");
		goto error3;
	}

	/* Global variables */
	Execute_Array = ptr;
	Execute_Size = EXECUTE_SIZE;
	Execute_Position = 1;
	Execute_LocalSize = size;

	return 0;

error3:
	free_buffer_membit(ptr);
	lispd_destroy_condlite(&Execute_Cond);
error1:
	lispd_destroy_mutexlite(&Execute_Mutex);
	return 1;
}

void free_execute(void)
{
	size_t i;
	struct execute *bit;

	if (Execute_Array == NULL)
		return;

	/* Execute_Array */
	for (i = 0; i < Execute_Size; i++) {
		bit = Execute_Array[i];
		if (bit->state != ThreadState_Empty) {
			free_local(bit->local);
			free(bit->exec);
		}
	}

	/* threadlocal */
	free_execute_local();

	/* Global variables */
	lispd_destroy_condlite(&Execute_Cond);
	lispd_destroy_mutexlite(&Execute_Mutex);
	free_buffer_membit(Execute_Array);
	Execute_Array = 0;
	Execute_Size = 0;
	Execute_Position = 0;
	Execute_LocalSize = 0;
}

int reload_execute(void)
{
	size_t size;

	size = Execute_LocalSize;
	free_execute();
	return init_execute(size);
}

static int extendmemory_execute(void)
{
	size_t size, i, k;
	struct execute **ptr, *bit;

	size = Execute_Size + EXECUTE_PLUS;
	ptr = reallocsize(Execute_Array, struct execute *, size);
	if (ptr == NULL) {
		Debug("reallocsize error");
		return 1;
	}
	for (i = Execute_Size; i < size; i++) {
		bit = alloc_membit(i);
		if (bit == NULL) {
			Debug("alloc_membit error");
			for (k = Execute_Size; k < i; k++)
				free_membit(ptr[k]);
			/* for realloc */
			Execute_Array = ptr;
			/* try recovery */
			ptr = reallocsize(Execute_Array, struct execute *, Execute_Size);
			if (ptr) Execute_Array = ptr;
			return 1;
		}
		ptr[i] = bit;
	}
	Execute_Array = ptr;
	Execute_Position = Execute_Size;
	Execute_Size = size;

	return 0;
}

static int findempty(struct execute **ret)
{
	size_t index;

	/* first try */
	for (index = Execute_Position; index < Execute_Size; index++) {
		if (Execute_Array[index]->state == ThreadState_Empty) {
			Execute_Position = index;
			*ret = Execute_Array[index];
			return 1;
		}
	}

	/* second try */
	for (index = 1; index < Execute_Position; index++) {
		if (Execute_Array[index]->state == ThreadState_Empty) {
			Execute_Position = index;
			*ret = Execute_Array[index];
			return 1;
		}
	}

	return 0;
}

static int findstate_execute(struct execute **ptr)
{
	lispd_lock_mutexlite(&Execute_Mutex);

	/* find state=ThreadState_Empty */
	if (findempty(ptr))
		goto finish;

	/* extend memory */
	if (extendmemory_execute()) {
		Debug("expandmemory error");
		lispd_unlock_mutexlite(&Execute_Mutex);
		return 1;
	}
	*ptr = Execute_Array[Execute_Position];

finish:
	(*ptr)->state = ThreadState_Run;
	lispd_unlock_mutexlite(&Execute_Mutex);

	return 0;
}

void setstate_execute(struct execute *ptr, enum ThreadState value)
{
	lispd_lock_mutexlite(&Execute_Mutex);
	ptr->state = value;
	lispd_unlock_mutexlite(&Execute_Mutex);
}

int make_execute(execfunction proc, struct execute **ret, size_t size)
{
	struct execute *ptr;

	/* size check */
	if (size < SizeLimit)
		size = SizeLimit;

	/* alloc */
	if (findstate_execute(&ptr)) {
		Debug("findstate_execute error");
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

	lispd_lock_mutexlite(&Execute_Mutex);
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
	lispd_unlock_mutexlite(&Execute_Mutex);
	return 0;

join:
	lispd_unlock_mutexlite(&Execute_Mutex);
	result = join_thread(&ptr->handle);
	if (result) {
		Debug("join_thread error");
		return 1;
	}
	setstate_execute(ptr, ThreadState_Empty);
	return 0;

error:
	lispd_unlock_mutexlite(&Execute_Mutex);
	return 1;
}

size_t count_execute(void)
{
	size_t i, count;

	count = 0;
	for (i = 0; i < Execute_Size; i++) {
		if (Execute_Array[i]->state != ThreadState_Empty)
			count++;
	}

	return count;
}

int joinindex_execute(size_t index)
{
	return join_execute(Execute_Array[index]);
}

struct execute *getexecute(size_t index)
{
	struct execute *result;

	lispd_lock_mutexlite(&Execute_Mutex);
	if (Execute_Size <= index) {
		lispd_unlock_mutexlite(&Execute_Mutex);
		Debug("index error");
		return NULL;
	}
	result = Execute_Array[index];
	lispd_unlock_mutexlite(&Execute_Mutex);

	return result;
}

int equal_control_restart(Execute ptr, addr control)
{
	return ptr->throw_value == throw_restart_case
		&& ptr->throw_control == control;
}

int equal_control_catch(Execute ptr, addr symbol)
{
	return ptr->throw_value == throw_catch
		&& ptr->throw_handler == symbol;
}


/*
 *  gc sync
 */
void gcstate_execute(enum GcMode mode)
{
	lisp_gcsync = mode;
}

static int gcstart_execute_check(struct execute *ptr)
{
	size_t i;

	for (i = 0; i < Execute_Size; i++) {
		if (Execute_Array[i]->state == ThreadState_Run)
			return 1;
	}

	return 0;
}

void gcstart_execute(struct execute *ptr)
{
	lispd_lock_mutexlite(&Execute_Mutex);
	ptr->state = ThreadState_GcWait;
	lispd_broadcast_condlite(&Execute_Cond);
	while (gcstart_execute_check(ptr))
		lispd_wait_condlite(&Execute_Cond, &Execute_Mutex);
	lispd_unlock_mutexlite(&Execute_Mutex);
}

void gcwait_execute(struct execute *ptr)
{
	lispd_lock_mutexlite(&Execute_Mutex);
	while (ptr->state != ThreadState_Run)
		lispd_wait_condlite(&Execute_Cond, &Execute_Mutex);
	lispd_unlock_mutexlite(&Execute_Mutex);
}

void gcend_execute(void)
{
	size_t i;
	struct execute *ptr;

	lispd_lock_mutexlite(&Execute_Mutex);
	for (i = 0; i < Execute_Size; i++) {
		ptr = Execute_Array[i];
		if (ptr->state == ThreadState_GcWait)
			ptr->state = ThreadState_Run;
	}
	lispd_broadcast_condlite(&Execute_Cond);
	lispd_unlock_mutexlite(&Execute_Mutex);
}

void foreach_execute(void (*call)(struct execute *))
{
	size_t i;
	struct execute *ptr;

	for (i = 0; i < Execute_Position; i++) {
		ptr = Execute_Array[i];
		if (ptr->state != ThreadState_Empty)
			call(ptr);
	}
}

int foreach_check_execute(int (*call)(struct execute *))
{
	size_t i;
	struct execute *ptr;

	for (i = 0; i < Execute_Position; i++) {
		ptr = Execute_Array[i];
		if (ptr->state != ThreadState_Empty) {
			if (call(ptr))
				return 1;
		}
	}

	return 0;
}

