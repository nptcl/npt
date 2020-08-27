#ifndef __DEFINE_SETJMP__
#define __DEFINE_SETJMP__

#include "define.h"

/* abort */
#ifdef LISP_ABORT_SETJMP
#include <setjmp.h>
#include <string.h>
extern jmp_buf Lisp_abort_setjmp;
#define Lisp_abort_throw()  longjmp(Lisp_abort_setjmp, 1)
#define Lisp_abort_Begin    if (setjmp(Lisp_abort_setjmp) == 0)
#define Lisp_abort_End      memset(&Lisp_abort_setjmp, 0, sizeof(Lisp_abort_setjmp))
#else
class Lisp_abort_class {};
#define Lisp_abort_throw()  throw Lisp_abort_class()
#define Lisp_abort_Begin    try
#define Lisp_abort_End      catch (Lisp_abort_class) {}
#endif

/* degrade */
#ifdef LISP_ABORT_SETJMP
#include <setjmp.h>
#include <string.h>
extern jmp_buf Lisp_degrade_setjmp;
#define Lisp_degrade_throw()	longjmp(Lisp_degrade_setjmp, 1)
#define Lisp_degrade_Begin		if (setjmp(Lisp_degrade_setjmp) == 0)
#define Lisp_degrade_End		memset(&Lisp_degrade_setjmp, 0, sizeof(Lisp_degrade_setjmp))
#else
class Lisp_degrade_class {};
#define Lisp_degrade_throw()	throw Lisp_degrade_class()
#define Lisp_degrade_Begin		try
#define Lisp_degrade_End		catch (Lisp_degrade_class) {}
#endif

#endif

