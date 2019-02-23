#include "thread.h"
#include "execute.h"

#undef USE_CLIB

#ifdef USE_CLIB
#define beginth(sec,size,p1,p2,s,id) _beginthreadex(sec,size,p1,p2,s,id)
#define endth(x) _endthreadex(x)
#else
#define beginth(sec,size,p1,p2,s,id) CreateThread(sec,size,p1,p2,s,id)
#define endth(x) ExitThread(x)
#endif


/*
 *  mutexlite
 */
int make_mutexlite(mutexlite *ptr)
{
	InitializeCriticalSection(ptr);
	return 0;
}

int make_rwlocklite(rwlocklite *ptr)
{
	InitializeSRWLock(ptr);
	return 0;
}


/*
 *  semaphore windows
 */
int trylock_semwindows(semwindows *ptr)
{
	DWORD result = WaitForSingleObject(*ptr, 0);
	if (result == WAIT_TIMEOUT) return 1;
	if (result != WAIT_OBJECT_0) {
		Debug("WaitForSingleObject (trylock semaphore) error");
		threaderror();
	}
	return 0;
}


/*
 *  binary semaphore  [condition variable]
 */
void make_binsemlite(binsemlite *ptr)
{
	make_mutexlite(&ptr->mutex);
	make_condlite(&ptr->cond);
	ptr->value = 1;
}
void destroy_binsemlite(binsemlite *ptr)
{
	destroy_condlite(&ptr->cond);
	destroy_mutexlite(&ptr->mutex);
}
void lock_binsemlite(binsemlite *ptr)
{
	lock_mutexlite(&ptr->mutex);
	while (ptr->value <= 0)
		wait_condlite(&ptr->cond, &ptr->mutex);
	ptr->value--;
	unlock_mutexlite(&ptr->mutex);
}
void unlock_binsemlite(binsemlite *ptr)
{
	lock_mutexlite(&ptr->mutex);
	/* binary semaphore check */
	if (1 <= ptr->value) {
		unlock_mutexlite(&ptr->mutex);
		Debug("unlock_binsemlite error");
		threaderror();
	}
	/* semaphore */
	if (ptr->value <= 0)
		signal_condlite(&ptr->cond);
	ptr->value++;
	unlock_mutexlite(&ptr->mutex);
}
int trylock_binsemlite(binsemlite *ptr)
{
	int result;

	lock_mutexlite(&ptr->mutex);
	if (ptr->value <= 0) {
		result = 1;
	}
	else {
		ptr->value--;
		result = 0;
	}
	unlock_mutexlite(&ptr->mutex);

	return result;
}


/*
 *  thread
 */
static DWORD WINAPI start_routine(LPVOID pvoid)
{
	Execute arg;

	arg = (Execute)pvoid;
	set_execute_local(arg);
	(*arg->routine)(arg);
	setstate_execute(arg, ThreadState_Finish);
	endth(0);

	return 0;
}

int create_thread(execfunction proc, Execute arg)
{
	HANDLE result;

	result = beginth(NULL, 0, start_routine, (LPVOID)arg,
			CREATE_SUSPENDED, &arg->handleid);
	if (result == NULL) {
		fprintf(stderr, "CreateThread error\n");
		return 1;
	}
	arg->handle = result;
	if (ResumeThread(result) < 0) {
		fprintf(stderr, "ResumeThread error\n");
		CloseHandle(result);
		return 1;
	}

	return 0;
}

int join_thread(threadhandle *handle)
{
	if (WaitForSingleObject(*handle, INFINITE) == WAIT_FAILED) {
		fprintf(stderr, "WaitForSingleObject error\n");
		return 1;
	}
	CloseHandle(*handle);

	return 0;
}

