#ifndef __THREAD_HEADER__
#define __THREAD_HEADER__

#include "lisp.h"
#include "typedef.h"


/*
 *  single
 */
#ifdef LISP_THREAD_SINGLE
/* mutex */
int make_mutexlite(mutexlite *mutex);
void destroy_mutexlite(mutexlite *mutex);
void lock_mutexlite(mutexlite *mutex);
int trylock_mutexlite(mutexlite *mutex);
void unlock_mutexlite(mutexlite *mutex);
/* read / write */
int make_rwlocklite(rwlocklite *lock);
void destroy_rwlocklite(rwlocklite *lock);
void rdlock_rwlocklite(rwlocklite *lock);
void wrlock_rwlocklite(rwlocklite *lock);
int tryrdlock_rwlocklite(rwlocklite *lock);
int trywrlock_rwlocklite(rwlocklite *lock);
void unrdlock_rwlocklite(rwlocklite *lock);
void unwrlock_rwlocklite(rwlocklite *lock);
/* thread local */
void make_threadlocal(threadlocal *key);
void destroy_threadlocal(threadlocal key);
const void *get_threadlocal(threadlocal key);
void set_threadlocal(threadlocal key, const void *value);
/* binary semaphore */
void make_binsem(binsem *x);
void destroy_binsem(binsem *x);
void lock_binsem(binsem *x);
int trylock_binsem(binsem *x);
void unlock_binsem(binsem *x);
/* condition variable */
void make_condlite(condlite *x);
void destroy_condlite(condlite *x);
void wait_condlite(condlite *x, mutexlite *m);
void signal_condlite(condlite *x);
void broadcast_condlite(condlite *x);
#endif


/*
 *  remove
 */
#ifdef LISP_THREAD_REMOVE
/* mutex */
#define make_mutexlite(mutex) (*(mutex) = 0)
#define destroy_mutexlite(mutex) ((void)mutex)
#define lock_mutexlite(mutex) ((void)mutex)
#define trylock_mutexlite(mutex) 0
#define unlock_mutexlite(mutex) ((void)mutex)
/* read / write */
#define make_rwlocklite(rw) (*(rw) = 0)
#define destroy_rwlocklite(rw) ((void)rw)
#define rdlock_rwlocklite(rw) ((void)rw)
#define wrlock_rwlocklite(rw) ((void)rw)
#define tryrdlock_rwlocklite(rw) 0
#define trywrlock_rwlocklite(rw) 0
#define unrdlock_rwlocklite(rw) ((void)rw)
#define unwrlock_rwlocklite(rw)((void)rw)
/* thread local */
#define make_threadlocal(key) ((void)key)
#define destroy_threadlocal(key) ((void)key)
/*static int __get_threadlocal;
#define get_threadlocal(key) ((void *)&__get_threadlocal)
 */
#define get_threadlocal(key) ((void *)NULL)
#define set_threadlocal(key, value) ((void)NULL)
/* binary semaphore */
#define make_binsem(x) (*(x) = 0)
#define destroy_binsem(x) ((void)x)
#define lock_binsem(x) ((void)x)
#define trylock_binsem(x) 0
#define unlock_binsem(x) ((void)x)
/* condition variable */
#define make_condlite(x) (*(x) = 0)
#define destroy_condlite(x) ((void)x)
#define wait_condlite(x, m)  ((void)x)
#define signal_condlite(x)  ((void)x)
#define broadcast_condlite(x)  ((void)x)
#endif


/*
 *  posix
 */
#ifdef LISP_THREAD_POSIX
/* mutex */
#define make_mutexlite(mutex) \
	pthread_mutex_init((mutex), NULL)
#define destroy_mutexlite(mutex) \
	if (pthread_mutex_destroy(mutex)) { \
		Debug("pthread_mutex_destroy error"); \
		threaderror(); \
	}
#define lock_mutexlite(mutex) \
	if (pthread_mutex_lock(mutex)) { \
		Debug("pthread_mutex_lock error"); \
		threaderror(); \
	}
/* trylock_mutelite return zero if success. */
/* pthread_mutex_trylock: return zero if success, else non-zero. */
#define trylock_mutexlite(mutex) \
	pthread_mutex_trylock(mutex)
#define unlock_mutexlite(mutex) \
	if (pthread_mutex_unlock(mutex)) { \
		Debug("pthread_mutex_unlock error"); \
		threaderror(); \
	}

/* read / write */
#define make_rwlocklite(rw) \
	pthread_rwlock_init((rw), NULL)
#define destroy_rwlocklite(rw) \
	if (pthread_rwlock_destroy(rw)) { \
		Debug("pthread_rwlock_destroy error"); \
		threaderror(); \
	}
#define rdlock_rwlocklite(rw) \
	if (pthread_rwlock_rdlock(rw)) { \
		Debug("pthread_rwlock_rdlock error"); \
		threaderror(); \
	}
#define wrlock_rwlocklite(rw) \
	if (pthread_rwlock_wrlock(rw)) { \
		Debug("pthread_rwlock_wrlock error"); \
		threaderror(); \
	}
/* tryrdlock_rwlocklite return zero if success. */
/* pthread_rwlock_tryrdlock: return zero if success, else non-zero. */
#define tryrdlock_rwlocklite(rw) \
	pthread_rwlock_tryrdlock(rw)
/* trywrlock_rwlocklite return zero if success. */
/* pthread_rwlock_trywrlock: return zero if success, else non-zero. */
#define trywrlock_rwlocklite(rw) \
	pthread_rwlock_trywrlock(rw)
#define unrdlock_rwlocklite(rw) \
	if (pthread_rwlock_unlock(rw)) { \
		Debug("pthread_rwlock_unlock [unrdlock] error"); \
		threaderror(); \
	}
#define unwrlock_rwlocklite(rw) \
	if (pthread_rwlock_unlock(rw)) { \
		Debug("pthread_rwlock_unlock [unwrlock] error"); \
		threaderror(); \
	}

#define make_threadlocal(key) \
	if (pthread_key_create(key, NULL)) { \
		Debug("pthread_key_create error"); \
		threaderror(); \
	}

/* thread local */
#define destroy_threadlocal(key) \
	if (pthread_key_delete(key)) { \
		Debug("pthread_key_delete error"); \
		threaderror(); \
	}
#define get_threadlocal(key) pthread_getspecific(key)
#define set_threadlocal(key, value) \
	if (pthread_setspecific(key, value)) { \
		Debug("pthread_setspecific error"); \
		threaderror(); \
	}

/* posix semaphore */
#define make_semposix(x, v) { \
	if (sem_init((x), 0, (v))) { \
		Debug("sem_init error"); \
		threaderror(); \
	} \
}
#define destroy_semposix(x) { \
	if (sem_destroy(x)) { \
		Debug("sem_destroy error"); \
		threaderror(); \
	} \
}
#define lock_semposix(x) { \
	if (sem_wait(x)) { \
		Debug("sem_destroy error"); \
		threaderror(); \
	} \
}
int trylock_semposix(semposix *sem);
#define unlock_semposix(x) { \
	if (sem_post(x)) { \
		Debug("sem_post error"); \
		threaderror(); \
	} \
}
int get_semposix(semposix *sem);

/* binary semaphore */
#define make_binsem(x) make_semposix((x), 1);
#define destroy_binsem destroy_semposix
#define lock_binsem lock_semposix
#define trylock_binsem trylock_semposix
#define unlock_binsem(x) { \
	unlock_semposix(x); \
	if (1 < get_semposix(x)) { \
		Debug("unlock_binsem error"); \
		threaderror(); \
	} \
}

/* condition variable */
#define make_condlite(x) { \
	if (pthread_cond_init(x, NULL)) { \
		Debug("pthread_cond_init error"); \
		threaderror(); \
	} \
}
#define destroy_condlite(x) { \
	if (pthread_cond_destroy(x)) { \
		Debug("pthread_cond_destroy error"); \
		threaderror(); \
	} \
}
#define wait_condlite(x, m) { \
	if (pthread_cond_wait((x), (m))) { \
		Debug("pthread_cond_wait error"); \
		threaderror(); \
	} \
}
#define signal_condlite(x) { \
	if (pthread_cond_signal(x)) { \
		Debug("pthread_cond_signal error"); \
		threaderror(); \
	} \
}
#define broadcast_condlite(x) { \
	if (pthread_cond_broadcast(x)) { \
		Debug("pthread_cond_broadcast error"); \
		threaderror(); \
	} \
}
#endif


/*
 *  Windows
 */
#ifdef LISP_THREAD_WINDOWS
#include <windows.h>
#include <Synchapi.h>

/* mutex */
int make_mutexlite(mutexlite *);
#define destroy_mutexlite DeleteCriticalSection
#define lock_mutexlite EnterCriticalSection
/* trylock_mutelite return zero if success. */
/* TryEnterCriticalSection: return non-zero if success, else zero. */
#define trylock_mutexlite(mutex) (TryEnterCriticalSection(mutex) == 0)
#define unlock_mutexlite LeaveCriticalSection

/* read / write  [SRWLock Vista module] */
int make_rwlocklite(rwlocklite *);
#define destroy_rwlocklite(rw)
#define rdlock_rwlocklite(rw) AcquireSRWLockShared(rw)
#define wrlock_rwlocklite(rw) AcquireSRWLockExclusive(rw)
/* tryrdlock_rwlocklite return zero if success. */
/* TryAcquireSRWLockShared: return non-zero if success, else zero. */
#define tryrdlock_rwlocklite(rw) (TryAcquireSRWLockShared(rw) == 0)
/* trywrlock_rwlocklite return zero if success. */
/* TryAcquireSRWLockExclusive: return non-zero if success, else zero. */
#define trywrlock_rwlocklite(rw) (TryAcquireSRWLockExclusive(rw) == 0)
#define unrdlock_rwlocklite(rw) ReleaseSRWLockShared(rw)
#define unwrlock_rwlocklite(rw) ReleaseSRWLockExclusive(rw)

/* thread local */
#define make_threadlocal(key) \
	if ((*key = TlsAlloc()) == 0xFFFFFFFF) { \
		Debug("TlsAlloc error"); \
		threaderror(); \
	}
#define destroy_threadlocal(key) \
	if (TlsFree(key) == 0) { \
		Debug("TlsFree error"); \
		threaderror(); \
	}
#define get_threadlocal(key) TlsGetValue(key)
#define set_threadlocal(key, value) \
	if (TlsSetValue(key, value) == 0) { \
		Debug("TlsSetValue error"); \
		threaderror(); \
	}

/* windows semaphore */
#define make_semwindows(x, init, maxvalue) { \
	HANDLE __handle = CreateSemaphore(NULL, (init), (maxvalue), NULL); \
	if (__handle == NULL) { \
		Debug("CreateSemaphore error"); \
		threaderror(); \
	} \
	*(x) = __handle; \
}
#define destroy_semwindows(x) { \
	if (CloseHandle(*(x)) == 0) { \
		Debug("CloseHandle (semaphore) error"); \
		threaderror(); \
	} \
}
#define lock_semwindows(x) { \
	if (WaitForSingleObject(*(x), INFINITE) != WAIT_OBJECT_0) { \
		Debug("WaitForSingleObject (lock semaphore) error"); \
		threaderror(); \
	} \
}
int trylock_semwindows(semwindows *ptr);
#define unlock_semwindows(x) { \
	if (ReleaseSemaphore(*(x), 1, NULL) == 0) { \
		Debug("ReleaseSemaphore (semaphore) error"); \
		threaderror(); \
	} \
}

#if 0
/* binary semaphore */
#define make_binsem(x) make_semwindows((x), 1, 1)
#define destroy_binsem destroy_semwindows
#define lock_binsem lock_semwindows
#define trylock_binsem trylock_semwindows
#define unlock_binsem unlock_semwindows
#endif
/* binary semaphore  [condition variable] */
#define make_binsem make_binsemlite
#define destroy_binsem destroy_binsemlite
#define lock_binsem lock_binsemlite
#define unlock_binsem unlock_binsemlite
#define trylock_binsem trylock_binsemlite
void make_binsemlite(binsemlite *ptr);
void destroy_binsemlite(binsemlite *ptr);
void lock_binsemlite(binsemlite *ptr);
void unlock_binsemlite(binsemlite *ptr);
int trylock_binsemlite(binsemlite *ptr);

/* condition variable */
#define make_condlite(x) InitializeConditionVariable(x)
#define destroy_condlite(x)
#define wait_condlite(x, m) { \
	if (SleepConditionVariableCS((x), (m), INFINITE) == 0) { \
		Debug("SleepConditionVariableCS error"); \
		threaderror(); \
	} \
}
#define signal_condlite(x) WakeConditionVariable(x)
#define broadcast_condlite(x) WakeAllConditionVariable(x)

#endif


/*
 *  tools
 */
void threaderror(void);
void wrlock2_rwlocklite(rwlocklite *, rwlocklite *);
void unwrlock2_rwlocklite(rwlocklite *, rwlocklite *);
void wrlock3_rwlocklite(rwlocklite *m1, rwlocklite *m2, rwlocklite *m3);
void unwrlock3_rwlocklite(rwlocklite *m1, rwlocklite *m2, rwlocklite *m3);

#endif

