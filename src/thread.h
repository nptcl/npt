#ifndef __THREAD_HEADER__
#define __THREAD_HEADER__

#include "build.h"
#include "typedef.h"
#include "typedef_thread.h"

/*
 *  single
 */
#ifdef LISP_THREAD_SINGLE
/* mutex */
int lispd_make_mutexlite(mutexlite *mutex);
void lispd_destroy_mutexlite(mutexlite *mutex);
void lispd_lock_mutexlite(mutexlite *mutex);
int lispd_trylock_mutexlite(mutexlite *mutex);
void lispd_unlock_mutexlite(mutexlite *mutex);
/* read / write */
int lispd_make_rwlocklite(rwlocklite *lock);
void lispd_destroy_rwlocklite(rwlocklite *lock);
void lispd_rdlock_rwlocklite(rwlocklite *lock);
void lispd_wrlock_rwlocklite(rwlocklite *lock);
int lispd_tryrdlock_rwlocklite(rwlocklite *lock);
int lispd_trywrlock_rwlocklite(rwlocklite *lock);
void lispd_unrdlock_rwlocklite(rwlocklite *lock);
void lispd_unwrlock_rwlocklite(rwlocklite *lock);
/* thread local */
void lispd_make_threadlocal(threadlocal *key);
void lispd_destroy_threadlocal(threadlocal key);
const void *lispd_get_threadlocal(threadlocal key);
void lispd_set_threadlocal(threadlocal key, const void *value);
/* binary semaphore */
void lispd_make_binsem(binsem *x);
void lispd_destroy_binsem(binsem *x);
void lispd_lock_binsem(binsem *x);
int lispd_trylock_binsem(binsem *x);
void lispd_unlock_binsem(binsem *x);
/* condition variable */
void lispd_make_condlite(condlite *x);
void lispd_destroy_condlite(condlite *x);
void lispd_wait_condlite(condlite *x, mutexlite *m);
void lispd_signal_condlite(condlite *x);
void lispd_broadcast_condlite(condlite *x);
#endif


/*
 *  remove
 */
#ifdef LISP_THREAD_REMOVE
/* mutex */
#define lispd_make_mutexlite(mutex) (*(mutex) = 0)
#define lispd_destroy_mutexlite(mutex) ((void)mutex)
#define lispd_lock_mutexlite(mutex) ((void)mutex)
#define lispd_trylock_mutexlite(mutex) 0
#define lispd_unlock_mutexlite(mutex) ((void)mutex)
/* read / write */
#define lispd_make_rwlocklite(rw) (*(rw) = 0)
#define lispd_destroy_rwlocklite(rw) ((void)rw)
#define lispd_rdlock_rwlocklite(rw) ((void)rw)
#define lispd_wrlock_rwlocklite(rw) ((void)rw)
#define lispd_tryrdlock_rwlocklite(rw) 0
#define lispd_trywrlock_rwlocklite(rw) 0
#define lispd_unrdlock_rwlocklite(rw) ((void)rw)
#define lispd_unwrlock_rwlocklite(rw)((void)rw)
/* thread local */
#define lispd_make_threadlocal(key) ((void)key)
#define lispd_destroy_threadlocal(key) ((void)key)
/*static int __get_threadlocal;
#define lispd_get_threadlocal(key) ((void *)&__get_threadlocal)
 */
#define lispd_get_threadlocal(key) ((void *)NULL)
#define lispd_set_threadlocal(key, value) ((void)NULL)
/* binary semaphore */
#define lispd_make_binsem(x) (*(x) = 0)
#define lispd_destroy_binsem(x) ((void)x)
#define lispd_lock_binsem(x) ((void)x)
#define lispd_trylock_binsem(x) 0
#define lispd_unlock_binsem(x) ((void)x)
/* condition variable */
#define lispd_make_condlite(x) (*(x) = 0)
#define lispd_destroy_condlite(x) ((void)x)
#define lispd_wait_condlite(x, m)  ((void)x)
#define lispd_signal_condlite(x)  ((void)x)
#define lispd_broadcast_condlite(x)  ((void)x)
#endif


/*
 *  unix
 */
#ifdef LISP_THREAD_UNIX
/* mutex */
#define lispd_make_mutexlite(mutex) \
	pthread_mutex_init((mutex), NULL)
#define lispd_destroy_mutexlite(mutex) \
	if (pthread_mutex_destroy(mutex)) { \
		Debug("pthread_mutex_destroy error"); \
		lispd_threaderror(); \
	}
#define lispd_lock_mutexlite(mutex) \
	if (pthread_mutex_lock(mutex)) { \
		Debug("pthread_mutex_lock error"); \
		lispd_threaderror(); \
	}
/* trylock_mutelite return zero if success. */
/* pthread_mutex_trylock: return zero if success, else non-zero. */
#define lispd_trylock_mutexlite(mutex) \
	pthread_mutex_trylock(mutex)
#define lispd_unlock_mutexlite(mutex) \
	if (pthread_mutex_unlock(mutex)) { \
		Debug("pthread_mutex_unlock error"); \
		lispd_threaderror(); \
	}

/* read / write */
#define lispd_make_rwlocklite(rw) \
	pthread_rwlock_init((rw), NULL)
#define lispd_destroy_rwlocklite(rw) \
	if (pthread_rwlock_destroy(rw)) { \
		Debug("pthread_rwlock_destroy error"); \
		lispd_threaderror(); \
	}
#define lispd_rdlock_rwlocklite(rw) \
	if (pthread_rwlock_rdlock(rw)) { \
		Debug("pthread_rwlock_rdlock error"); \
		lispd_threaderror(); \
	}
#define lispd_wrlock_rwlocklite(rw) \
	if (pthread_rwlock_wrlock(rw)) { \
		Debug("pthread_rwlock_wrlock error"); \
		lispd_threaderror(); \
	}
/* lispd_tryrdlock_rwlocklite return zero if success. */
/* pthread_rwlock_tryrdlock: return zero if success, else non-zero. */
#define lispd_tryrdlock_rwlocklite(rw) \
	pthread_rwlock_tryrdlock(rw)
/* lispd_trywrlock_rwlocklite return zero if success. */
/* pthread_rwlock_trywrlock: return zero if success, else non-zero. */
#define lispd_trywrlock_rwlocklite(rw) \
	pthread_rwlock_trywrlock(rw)
#define lispd_unrdlock_rwlocklite(rw) \
	if (pthread_rwlock_unlock(rw)) { \
		Debug("pthread_rwlock_unlock [unrdlock] error"); \
		lispd_threaderror(); \
	}
#define lispd_unwrlock_rwlocklite(rw) \
	if (pthread_rwlock_unlock(rw)) { \
		Debug("pthread_rwlock_unlock [unwrlock] error"); \
		lispd_threaderror(); \
	}

#define lispd_make_threadlocal(key) \
	if (pthread_key_create(key, NULL)) { \
		Debug("pthread_key_create error"); \
		lispd_threaderror(); \
	}

/* thread local */
#define lispd_destroy_threadlocal(key) \
	if (pthread_key_delete(key)) { \
		Debug("pthread_key_delete error"); \
		lispd_threaderror(); \
	}
#define lispd_get_threadlocal(key) pthread_getspecific(key)
#define lispd_set_threadlocal(key, value) \
	if (pthread_setspecific(key, value)) { \
		Debug("pthread_setspecific error"); \
		lispd_threaderror(); \
	}

/* Unix semaphore */
#define make_semunix(x, v) { \
	if (sem_init((x), 0, (v))) { \
		Debug("sem_init error"); \
		lispd_threaderror(); \
	} \
}
#define destroy_semunix(x) { \
	if (sem_destroy(x)) { \
		Debug("sem_destroy error"); \
		lispd_threaderror(); \
	} \
}
#define lock_semunix(x) { \
	if (sem_wait(x)) { \
		Debug("sem_destroy error"); \
		lispd_threaderror(); \
	} \
}
int lispd_trylock_semunix(semunix *sem);
#define unlock_semunix(x) { \
	if (sem_post(x)) { \
		Debug("sem_post error"); \
		lispd_threaderror(); \
	} \
}
int lispd_get_semunix(semunix *sem);

/* binary semaphore */
#define lispd_make_binsem(x) make_semunix((x), 1);
#define lispd_destroy_binsem destroy_semunix
#define lispd_lock_binsem lock_semunix
#define lispd_trylock_binsem lispd_trylock_semunix
#define lispd_unlock_binsem(x) { \
	unlock_semunix(x); \
	if (1 < lispd_get_semunix(x)) { \
		Debug("lispd_unlock_binsem error"); \
		lispd_threaderror(); \
	} \
}

/* condition variable */
#define lispd_make_condlite(x) { \
	if (pthread_cond_init(x, NULL)) { \
		Debug("pthread_cond_init error"); \
		lispd_threaderror(); \
	} \
}
#define lispd_destroy_condlite(x) { \
	if (pthread_cond_destroy(x)) { \
		Debug("pthread_cond_destroy error"); \
		lispd_threaderror(); \
	} \
}
#define lispd_wait_condlite(x, m) { \
	if (pthread_cond_wait((x), (m))) { \
		Debug("pthread_cond_wait error"); \
		lispd_threaderror(); \
	} \
}
#define lispd_signal_condlite(x) { \
	if (pthread_cond_signal(x)) { \
		Debug("pthread_cond_signal error"); \
		lispd_threaderror(); \
	} \
}
#define lispd_broadcast_condlite(x) { \
	if (pthread_cond_broadcast(x)) { \
		Debug("pthread_cond_broadcast error"); \
		lispd_threaderror(); \
	} \
}
#endif


/*
 *  Windows
 */
#ifdef LISP_THREAD_WINDOWS
#include <windows.h>
#include <synchapi.h>

/* mutex */
int lispd_make_mutexlite(mutexlite *);
#define lispd_destroy_mutexlite DeleteCriticalSection
#define lispd_lock_mutexlite EnterCriticalSection
/* trylock_mutelite return zero if success. */
/* TryEnterCriticalSection: return non-zero if success, else zero. */
#define lispd_trylock_mutexlite(mutex) (TryEnterCriticalSection(mutex) == 0)
#define lispd_unlock_mutexlite LeaveCriticalSection

/* read / write  [SRWLock Vista module] */
int lispd_make_rwlocklite(rwlocklite *);
#define lispd_destroy_rwlocklite(rw)
#define lispd_rdlock_rwlocklite(rw) AcquireSRWLockShared(rw)
#define lispd_wrlock_rwlocklite(rw) AcquireSRWLockExclusive(rw)
/* lispd_tryrdlock_rwlocklite return zero if success. */
/* TryAcquireSRWLockShared: return non-zero if success, else zero. */
#define lispd_tryrdlock_rwlocklite(rw) (TryAcquireSRWLockShared(rw) == 0)
/* lispd_trywrlock_rwlocklite return zero if success. */
/* TryAcquireSRWLockExclusive: return non-zero if success, else zero. */
#define lispd_trywrlock_rwlocklite(rw) (TryAcquireSRWLockExclusive(rw) == 0)
#define lispd_unrdlock_rwlocklite(rw) ReleaseSRWLockShared(rw)
#define lispd_unwrlock_rwlocklite(rw) ReleaseSRWLockExclusive(rw)

/* thread local */
#define lispd_make_threadlocal(key) \
	if ((*key = TlsAlloc()) == 0xFFFFFFFF) { \
		Debug("TlsAlloc error"); \
		lispd_threaderror(); \
	}
#define lispd_destroy_threadlocal(key) \
	if (TlsFree(key) == 0) { \
		Debug("TlsFree error"); \
		lispd_threaderror(); \
	}
#define lispd_get_threadlocal(key) TlsGetValue(key)
#define lispd_set_threadlocal(key, value) \
	if (TlsSetValue(key, value) == 0) { \
		Debug("TlsSetValue error"); \
		lispd_threaderror(); \
	}

/* windows semaphore */
#define make_semwindows(x, init, maxvalue) { \
	HANDLE __handle = CreateSemaphore(NULL, (init), (maxvalue), NULL); \
	if (__handle == NULL) { \
		Debug("CreateSemaphore error"); \
		lispd_threaderror(); \
	} \
	*(x) = __handle; \
}
#define destroy_semwindows(x) { \
	if (CloseHandle(*(x)) == 0) { \
		Debug("CloseHandle (semaphore) error"); \
		lispd_threaderror(); \
	} \
}
#define lock_semwindows(x) { \
	if (WaitForSingleObject(*(x), INFINITE) != WAIT_OBJECT_0) { \
		Debug("WaitForSingleObject (lock semaphore) error"); \
		lispd_threaderror(); \
	} \
}
int lispd_trylock_semwindows(semwindows *ptr);
#define unlock_semwindows(x) { \
	if (ReleaseSemaphore(*(x), 1, NULL) == 0) { \
		Debug("ReleaseSemaphore (semaphore) error"); \
		lispd_threaderror(); \
	} \
}

#if 0
/* binary semaphore */
#define lispd_make_binsem(x) make_semwindows((x), 1, 1)
#define lispd_destroy_binsem destroy_semwindows
#define lispd_lock_binsem lock_semwindows
#define lispd_trylock_binsem lispd_trylock_semwindows
#define lispd_unlock_binsem unlock_semwindows
#endif
/* binary semaphore  [condition variable] */
#define lispd_make_binsem lispd_make_binsemlite
#define lispd_destroy_binsem lispd_destroy_binsemlite
#define lispd_lock_binsem lispd_lock_binsemlite
#define lispd_unlock_binsem lispd_unlock_binsemlite
#define lispd_trylock_binsem lispd_trylock_binsemlite
void lispd_make_binsemlite(binsemlite *ptr);
void lispd_destroy_binsemlite(binsemlite *ptr);
void lispd_lock_binsemlite(binsemlite *ptr);
void lispd_unlock_binsemlite(binsemlite *ptr);
int lispd_trylock_binsemlite(binsemlite *ptr);

/* condition variable */
#define lispd_make_condlite(x) InitializeConditionVariable(x)
#define lispd_destroy_condlite(x)
#define lispd_wait_condlite(x, m) { \
	if (SleepConditionVariableCS((x), (m), INFINITE) == 0) { \
		Debug("SleepConditionVariableCS error"); \
		lispd_threaderror(); \
	} \
}
#define lispd_signal_condlite(x) WakeConditionVariable(x)
#define lispd_broadcast_condlite(x) WakeAllConditionVariable(x)

#endif


/*
 *  tools
 */
void lispd_threaderror(void);
void lispd_wrlock2_rwlocklite(rwlocklite *, rwlocklite *);
void lispd_unwrlock2_rwlocklite(rwlocklite *, rwlocklite *);
void lispd_wrlock3_rwlocklite(rwlocklite *m1, rwlocklite *m2, rwlocklite *m3);
void lispd_unwrlock3_rwlocklite(rwlocklite *m1, rwlocklite *m2, rwlocklite *m3);

#endif

