#ifndef __EXECUTE_SETJMP__
#define __EXECUTE_SETJMP__

#include "typedef.h"

/* abort */
#ifdef __cplusplus
class Lisp_abort_class {};
#define Lisp_abort_throw()  throw Lisp_abort_class()
#define Lisp_abort_Begin    try
#define Lisp_abort_End      catch (Lisp_abort_class) {}
#else
#include <setjmp.h>
#include <string.h>
__extern jmp_buf Lisp_abort_setjmp;
#define Lisp_abort_throw()  longjmp(Lisp_abort_setjmp, 1)
#define Lisp_abort_Begin    if (setjmp(Lisp_abort_setjmp) == 0)
#define Lisp_abort_End      memset(&Lisp_abort_setjmp, 0, sizeof(Lisp_abort_setjmp))
#endif

__extern lisp_abort_calltype Lisp_abort_handler;
_g void abort_execute(void);
_g lisp_abort_calltype set_abort_handler(lisp_abort_calltype call);
_g lisp_abort_calltype set_abort_setjmp_handler(void);

/* degrade */
#ifdef __cplusplus
class Lisp_degrade_class {};
#define Lisp_degrade_throw()	throw Lisp_degrade_class()
#define Lisp_degrade_Begin		try
#define Lisp_degrade_End		catch (Lisp_degrade_class) {}
#else
#include <setjmp.h>
#include <string.h>
__extern jmp_buf Lisp_degrade_setjmp;
#define Lisp_degrade_throw()	longjmp(Lisp_degrade_setjmp, 1)
#define Lisp_degrade_Begin		if (setjmp(Lisp_degrade_setjmp) == 0)
#define Lisp_degrade_End		memset(&Lisp_degrade_setjmp, 0, sizeof(Lisp_degrade_setjmp))
#endif
_g lisp_abort_calltype set_degrade_setjmp_handler(void);

#endif

