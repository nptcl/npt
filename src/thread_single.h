#include "alloc.h"
#include "typedef.h"

/*
 *  mutexlite
 */
int make_mutexlite(mutexlite *mutex)
{
	*mutex = 0;
	return 0;
}

void destroy_mutexlite(mutexlite *mutex)
{
	if (*mutex) {
		Debug("destroy_mutexlite error.");
		threaderror();
	}
}

void lock_mutexlite(mutexlite *mutex)
{
	if (*mutex) {
		Debug("lock_mutexlite error.");
		threaderror();
	}
	*mutex = 1;
}

int trylock_mutexlite(mutexlite *mutex)
{
	if (*mutex) {
		Debug("trylock_mutexlite error.");
		threaderror();
	}

	return 0;
}

void unlock_mutexlite(mutexlite *mutex)
{
	if (*mutex == 0) {
		Debug("unlock_mutexlite error.");
		threaderror();
	}
	*mutex = 0;
}


/*
 *  rwlocklite
 */
int make_rwlocklite(rwlocklite *lock)
{
	*lock = 0;
	return 0;
}

void destroy_rwlocklite(rwlocklite *lock)
{
	if (*lock) {
		Debug("destroy_rwlocklite error.");
		threaderror();
	}
}

void rdlock_rwlocklite(rwlocklite *lock)
{
	if (*lock == 2) {
		Debug("rdlock_rwlocklite error.");
		threaderror();
	}
	*lock = 1;
}

void wrlock_rwlocklite(rwlocklite *lock)
{
	if (*lock) {
		Debug("wrlock_rwlocklite error.");
		threaderror();
	}
	*lock = 2;
}

int tryrdlock_rwlocklite(rwlocklite *lock)
{
	if (*lock == 2) {
		Debug("tryrdlock_rwlocklite error.");
		threaderror();
	}

	return 0;
}

int trywrlock_rwlocklite(rwlocklite *lock)
{
	if (*lock) {
		Debug("trywrlock_rwlocklite error.");
		threaderror();
	}

	return 0;
}

void unrdlock_rwlocklite(rwlocklite *lock)
{
	if (*lock != 1) {
		Debug("unrdlock_rwlocklite error.");
		threaderror();
	}
}

void unwrlock_rwlocklite(rwlocklite *lock)
{
	if (*lock != 2) {
		Debug("unwrlock_rwlocklite error.");
		threaderror();
	}
}


/*
 *  threadlocal
 */
void make_threadlocal(threadlocal *key)
{
	struct threadlocal_single *ptr;
	ptr = malloctype(struct threadlocal_single);
	ptr->value = 0;
	*key = ptr;
}

void destroy_threadlocal(threadlocal key)
{
	if (key == NULL) {
		Debug("destroy_threadlocal error");
		threaderror();
	}
	free(key);
}

const void *get_threadlocal(threadlocal key)
{
	if (key == NULL) {
		Debug("get_threadlocal error");
		threaderror();
	}
	return key->value;
}

void set_threadlocal(threadlocal key, const void *value)
{
	if (key == NULL) {
		Debug("set_threadlocal error");
		threaderror();
	}
	key->value = value;
}


/*
 *  binary semaphore
 */
void make_binsem(binsem *x)
{
	*x = 0;
}

/* ARGSUSED0 */
void destroy_binsem(binsem *x)
{
}

void lock_binsem(binsem *x)
{
	if (*x) {
		Debug("lock_binsem error.");
		threaderror();
	}
	*x = 1;
}

int trylock_binsem(binsem *x)
{
	if (*x) {
		Debug("trylock_binsem error.");
		threaderror();
	}

	return 0;
}

void unlock_binsem(binsem *x)
{
	if (*x == 0) {
		Debug("unlock_mutexlite error.");
		threaderror();
	}
	*x = 0;
}


/*
 *  condition variable
 */
void make_condlite(condlite *x)
{
	*x = 0;
}

/* ARGSUSED0 */
void destroy_condlite(condlite *x)
{
}

/* ARGSUSED0 */
void wait_condlite(condlite *x, mutexlite *m)
{
}

/* ARGSUSED0 */
void signal_condlite(condlite *x)
{
}

/* ARGSUSED0 */
void broadcast_condlite(condlite *x)
{
}

