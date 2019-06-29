#include <stdio.h>
#include <stdlib.h>
#include "build.h"
#include "thread.h"


/*
 *  single
 */
#ifdef LISP_THREAD_SINGLE
#include "thread_single.h"
#endif


/*
 *  disable
 */
#ifdef LISP_THREAD_DISABLE
#include "thread_disable.h"
#endif


/*
 *  posix
 */
#ifdef LISP_THREAD_POSIX
#include "thread_posix.h"
#endif


/*
 *  Windows
 */
#ifdef LISP_THREAD_WINDOWS
#include "thread_windows.h"
#endif


/*
 *  tools
 */
_g void threaderror(void)
{
	Abort("thread error");
}

_g void wrlock2_rwlocklite(rwlocklite *lock1, rwlocklite *lock2)
{
	if (lock1 == lock2) {
		wrlock_rwlocklite(lock1);
		return;
	}
	if (lock2 < lock1)
		goto trylock2;

trylock1:
	wrlock_rwlocklite(lock1);
	if (trywrlock_rwlocklite(lock2) == 0) return;
	unwrlock_rwlocklite(lock1);
trylock2:
	wrlock_rwlocklite(lock2);
	if (trywrlock_rwlocklite(lock1) == 0) return;
	unwrlock_rwlocklite(lock2);
	goto trylock1;
}

_g void unwrlock2_rwlocklite(rwlocklite *lock1, rwlocklite *lock2)
{
	if (lock1 == lock2) {
		unwrlock_rwlocklite(lock1);
	}
	else {
		unwrlock_rwlocklite(lock1);
		unwrlock_rwlocklite(lock2);
	}
}

#define SwapVariable(a,b,temp) { temp = a; a = b; b = temp; }
_g void wrlock3_rwlocklite(rwlocklite *m1, rwlocklite *m2, rwlocklite *m3)
{
	int check1, check2;
	rwlocklite *temp;

	check1 = (m1 == m2);
	check2 = (m2 == m3);
	if (check1 && check2) {
		wrlock_rwlocklite(m1);
	}
	else if (check1) {
		wrlock2_rwlocklite(m2, m3);
	}
	else if (check2) {
		wrlock2_rwlocklite(m3, m1);
	}
	else if (m3 == m1) {
		wrlock2_rwlocklite(m1, m2);
	}
	else {
		if (m2 < m1) SwapVariable(m2, m1, temp);
		if (m3 < m1) SwapVariable(m3, m1, temp);
		if (m3 < m2) SwapVariable(m3, m2, temp);

trylock1: /* m1->m2->m3 */
		wrlock_rwlocklite(m1);
		if (trywrlock_rwlocklite(m2) == 0) {
			if (trywrlock_rwlocklite(m3) == 0) return;
			unwrlock_rwlocklite(m2);
			unwrlock_rwlocklite(m1);
			goto trylock3;
		}
		unwrlock_rwlocklite(m1);

trylock2: /* m2->m3->m1 */
		wrlock_rwlocklite(m2);
		if (trywrlock_rwlocklite(m3) == 0) {
			if (trywrlock_rwlocklite(m1) == 0) return;
			unwrlock_rwlocklite(m3);
			unwrlock_rwlocklite(m2);
			goto trylock1;
		}
		unwrlock_rwlocklite(m2);

trylock3: /* m3->m1->m2 */
		wrlock_rwlocklite(m3);
		if (trywrlock_rwlocklite(m1) == 0) {
			if (trywrlock_rwlocklite(m2) == 0) return;
			unwrlock_rwlocklite(m1);
			unwrlock_rwlocklite(m3);
			goto trylock2;
		}
		unwrlock_rwlocklite(m3);
		goto trylock1;
	}
}

_g void unwrlock3_rwlocklite(rwlocklite *m1, rwlocklite *m2, rwlocklite *m3)
{
	int check1, check2;

	check1 = (m1 == m2);
	check2 = (m2 == m3);
	if (check1 && check2) {
		unwrlock_rwlocklite(m1);
	}
	else if (check1) {
		unwrlock_rwlocklite(m2);
		unwrlock_rwlocklite(m3);
	}
	else if (check2) {
		unwrlock_rwlocklite(m3);
		unwrlock_rwlocklite(m1);
	}
	else if (m3 == m1) {
		unwrlock_rwlocklite(m1);
		unwrlock_rwlocklite(m2);
	}
	else {
		unwrlock_rwlocklite(m1);
		unwrlock_rwlocklite(m2);
		unwrlock_rwlocklite(m3);
	}
}

