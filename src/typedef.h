#ifndef __TYPEDEF_HEADER__
#define __TYPEDEF_HEADER__

#include "c99.h"
#include "define.h"
#include <stdint.h>

/*
 *  Basic type
 */
typedef unsigned char byte;
typedef uint16_t byte16;
typedef uint32_t byte32;
typedef uint64_t byte64;
typedef float short_float;
typedef float single_float;
typedef double double_float;
typedef long double long_float;
typedef uint32_t unicode;
typedef byte *pbyte;
typedef byte *addr;
struct execute;
typedef struct execute *Execute;
typedef void (*calltype)(Execute, addr);
typedef void (*callmaketype)(addr *);

enum CallBind_index {
	CallBind_system,
	CallBind_macro,
	CallBind_type,
	CallBind_none,
	CallBind_any,
	CallBind_empty,
	CallBind_dynamic,
	CallBind_rest,
	CallBind_var1,
	CallBind_var2,
	CallBind_var3,
	CallBind_opt1,
	CallBind_opt2,
	CallBind_opt3,
	CallBind_opt4,
	CallBind_opt5,
	CallBind_var1opt1,
	CallBind_var2opt1,
	CallBind_var3opt1,
	CallBind_var1opt2,
	CallBind_var2opt2,
	CallBind_var1rest,
	CallBind_var2rest,
	CallBind_var1dynamic,
	CallBind_var2dynamic,
	CallBind_var3dynamic,
	CallBind_var4dynamic,
	CallBind_size
};
typedef void *callbind;
typedef void (*callbind_macro)(Execute, addr, addr);
typedef void (*callbind_none)(void);
typedef void (*callbind_any)(Execute);
typedef void (*callbind_empty)(Execute);
typedef void (*callbind_dynamic)(Execute, addr);
typedef void (*callbind_rest)(Execute, addr);
typedef void (*callbind_var1)(Execute, addr);
typedef void (*callbind_var2)(Execute, addr, addr);
typedef void (*callbind_var3)(Execute, addr, addr, addr);
typedef void (*callbind_opt1)(Execute, addr);
typedef void (*callbind_opt2)(Execute, addr, addr);
typedef void (*callbind_opt3)(Execute, addr, addr, addr);
typedef void (*callbind_opt4)(Execute, addr, addr, addr, addr);
typedef void (*callbind_opt5)(Execute, addr, addr, addr, addr, addr);
typedef void (*callbind_var1opt1)(Execute, addr, addr);
typedef void (*callbind_var2opt1)(Execute, addr, addr, addr);
typedef void (*callbind_var3opt1)(Execute, addr, addr, addr, addr);
typedef void (*callbind_var1opt2)(Execute, addr, addr, addr);
typedef void (*callbind_var2opt2)(Execute, addr, addr, addr, addr);
typedef void (*callbind_var1rest)(Execute, addr, addr);
typedef void (*callbind_var2rest)(Execute, addr, addr, addr);
typedef void (*callbind_var1dynamic)(Execute, addr, addr);
typedef void (*callbind_var2dynamic)(Execute, addr, addr, addr);
typedef void (*callbind_var3dynamic)(Execute, addr, addr, addr, addr);
typedef void (*callbind_var4dynamic)(Execute, addr, addr, addr, addr, addr);

struct callbind_struct {
	enum CallBind_index type;
	union {
		calltype system;
		callbind_macro macro;
		callbind_none none;
		callbind_any any;
		callbind_empty empty;
		callbind_dynamic dynamic;
		callbind_rest rest;
		callbind_var1 var1;
		callbind_var2 var2;
		callbind_var3 var3;
		callbind_opt1 opt1;
		callbind_opt2 opt2;
		callbind_opt3 opt3;
		callbind_opt4 opt4;
		callbind_opt5 opt5;
		callbind_var1opt1 var1opt1;
		callbind_var2opt1 var2opt1;
		callbind_var3opt1 var3opt1;
		callbind_var1opt2 var1opt2;
		callbind_var2opt2 var2opt2;
		callbind_var1rest var1rest;
		callbind_var2rest var2rest;
		callbind_var1dynamic var1dynamic;
		callbind_var2dynamic var2dynamic;
		callbind_var3dynamic var3dynamic;
		callbind_var4dynamic var4dynamic;
		callbind *ptr;
	} call;
};


/*
 *  fixnum
 */
#ifdef LISP_64BIT
#define LISP_INFO           "64bit-code"
#define LISP_INTEGER_BIT	64
#define LISP_INTEGER_MASK	UINT64_MAX
#define FIXNUM_MAX			INT64_MAX
#define FIXNUM_MIN			INT64_MIN
typedef int64_t fixnum;
typedef uint64_t fixed;
#define PRIdF PRId64
#define PRIuF PRIu64
#define PRIxF PRIx64
#define PRIXF PRIX64
#else
#define LISP_INFO           "32bit-code"
#define LISP_INTEGER_BIT	32
#define LISP_INTEGER_MASK	UINT32_MAX
#define FIXNUM_MAX			INT32_MAX
#define FIXNUM_MIN			INT32_MIN
typedef int32_t fixnum;
typedef uint32_t fixed;
#define PRIdF PRId32
#define PRIuF PRIu32
#define PRIxF PRIx32
#define PRIXF PRIX32
#endif

typedef fixed bigtype;
#define FIXNUM_UMIN			((bigtype)FIXNUM_MIN)


/*
 *  Thread
 */
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

