#include "alloc.h"
#include "typedef.h"

/*
 *  mutexlite
 */
_g int lispd_make_mutexlite(mutexlite *mutex)
{
	*mutex = 0;
	return 0;
}

_g void lispd_destroy_mutexlite(mutexlite *mutex)
{
	if (*mutex) {
		Debug("lispd_destroy_mutexlite error.");
		lispd_threaderror();
	}
}

_g void lispd_lock_mutexlite(mutexlite *mutex)
{
	if (*mutex) {
		Debug("lispd_lock_mutexlite error.");
		lispd_threaderror();
	}
	*mutex = 1;
}

_g int lispd_trylock_mutexlite(mutexlite *mutex)
{
	if (*mutex) {
		Debug("lispd_trylock_mutexlite error.");
		lispd_threaderror();
	}

	return 0;
}

_g void lispd_unlock_mutexlite(mutexlite *mutex)
{
	if (*mutex == 0) {
		Debug("lispd_unlock_mutexlite error.");
		lispd_threaderror();
	}
	*mutex = 0;
}


/*
 *  rwlocklite
 */
_g int lispd_make_rwlocklite(rwlocklite *lock)
{
	*lock = 0;
	return 0;
}

_g void lispd_destroy_rwlocklite(rwlocklite *lock)
{
	if (*lock) {
		Debug("lispd_destroy_rwlocklite error.");
		lispd_threaderror();
	}
}

_g void lispd_rdlock_rwlocklite(rwlocklite *lock)
{
	if (*lock == 2) {
		Debug("lispd_rdlock_rwlocklite error.");
		lispd_threaderror();
	}
	*lock = 1;
}

_g void lispd_wrlock_rwlocklite(rwlocklite *lock)
{
	if (*lock) {
		Debug("lispd_wrlock_rwlocklite error.");
		lispd_threaderror();
	}
	*lock = 2;
}

_g int lispd_tryrdlock_rwlocklite(rwlocklite *lock)
{
	if (*lock == 2) {
		Debug("lispd_tryrdlock_rwlocklite error.");
		lispd_threaderror();
	}

	return 0;
}

_g int lispd_trywrlock_rwlocklite(rwlocklite *lock)
{
	if (*lock) {
		Debug("lispd_trywrlock_rwlocklite error.");
		lispd_threaderror();
	}

	return 0;
}

_g void lispd_unrdlock_rwlocklite(rwlocklite *lock)
{
	if (*lock != 1) {
		Debug("lispd_unrdlock_rwlocklite error.");
		lispd_threaderror();
	}
}

_g void lispd_unwrlock_rwlocklite(rwlocklite *lock)
{
	if (*lock != 2) {
		Debug("lispd_unwrlock_rwlocklite error.");
		lispd_threaderror();
	}
}


/*
 *  threadlocal
 */
_g void lispd_make_threadlocal(threadlocal *key)
{
	struct threadlocal_single *ptr;
	ptr = malloctype(struct threadlocal_single);
	ptr->value = 0;
	*key = ptr;
}

_g void lispd_destroy_threadlocal(threadlocal key)
{
	if (key == NULL) {
		Debug("lispd_destroy_threadlocal error");
		lispd_threaderror();
	}
	free(key);
}

_g const void *lispd_get_threadlocal(threadlocal key)
{
	if (key == NULL) {
		Debug("lispd_get_threadlocal error");
		lispd_threaderror();
	}
	return key->value;
}

_g void lispd_set_threadlocal(threadlocal key, const void *value)
{
	if (key == NULL) {
		Debug("lispd_set_threadlocal error");
		lispd_threaderror();
	}
	key->value = value;
}


/*
 *  binary semaphore
 */
_g void lispd_make_binsem(binsem *x)
{
	*x = 0;
}

_g void lispd_destroy_binsem(binsem *x)
{
}

_g void lispd_lock_binsem(binsem *x)
{
	if (*x) {
		Debug("lispd_lock_binsem error.");
		lispd_threaderror();
	}
	*x = 1;
}

_g int lispd_trylock_binsem(binsem *x)
{
	if (*x) {
		Debug("lispd_trylock_binsem error.");
		lispd_threaderror();
	}

	return 0;
}

_g void lispd_unlock_binsem(binsem *x)
{
	if (*x == 0) {
		Debug("lispd_unlock_mutexlite error.");
		lispd_threaderror();
	}
	*x = 0;
}


/*
 *  condition variable
 */
_g void lispd_make_condlite(condlite *x)
{
	*x = 0;
}

_g void lispd_destroy_condlite(condlite *x)
{
}

_g void lispd_wait_condlite(condlite *x, mutexlite *m)
{
}

_g void lispd_signal_condlite(condlite *x)
{
}

_g void lispd_broadcast_condlite(condlite *x)
{
}

