#ifndef __TYPEDEF_THREAD_HEADER__
#define __TYPEDEF_THREAD_HEADER__

#include "define.h"

/* single thread mode */
#ifdef LISP_THREAD_DISABLE
typedef int threadhandle;
typedef int mutexlite;
typedef int rwlocklite;
struct threadlocal_single {
	const void *value;
};
typedef struct threadlocal_single *threadlocal;
typedef int condlite;
typedef int binsem;
#endif

/* pthread mode */
#ifdef LISP_THREAD_POSIX
#include <pthread.h>
#include <semaphore.h>
#include <errno.h>
typedef pthread_t threadhandle;
typedef pthread_mutex_t mutexlite;
typedef pthread_rwlock_t rwlocklite;
typedef pthread_key_t threadlocal;
typedef sem_t semposix;
typedef semposix binsem;
typedef pthread_cond_t condlite;
#endif

/* Windows mode */
#ifdef LISP_THREAD_WINDOWS
#include <windows.h>
typedef HANDLE threadhandle;
typedef DWORD threadid;
typedef CRITICAL_SECTION mutexlite;
/* required Vista */
typedef SRWLOCK rwlocklite;
typedef DWORD threadlocal;
typedef CONDITION_VARIABLE condlite;
typedef HANDLE semwindows;
struct binsemlite_tag {
	mutexlite mutex;
	condlite cond;
	int value;
};
typedef struct binsemlite_tag binsemlite;
typedef binsemlite binsem;
#endif

#endif

