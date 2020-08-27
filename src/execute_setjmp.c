#include <stdio.h>
#include <stdlib.h>
#include "define.h"
#include "execute_setjmp.h"
#include "typedef.h"

#ifdef LISP_ABORT_SETJMP
#include <setjmp.h>
jmp_buf Lisp_abort_setjmp;
jmp_buf Lisp_degrade_setjmp;
#endif

_g lisp_abort_calltype Lisp_abort_handler = NULL;


/*
 *  abort
 */
static void abort_shutdown(void)
{
	exit(1); /* or abort(); */
}

_g void abort_execute(void)
{
	/* handler */
	if (Lisp_abort_handler) {
		(*Lisp_abort_handler)();
	}

	/* default */
	fprintf(stderr, "\n\n");
	fprintf(stderr, "**************\n");
	fprintf(stderr, "  LISP ABORT  \n");
	fprintf(stderr, "**************\n");
	abort_shutdown();
}

_g lisp_abort_calltype set_abort_handler(lisp_abort_calltype call)
{
	lisp_abort_calltype ret;

	ret = Lisp_abort_handler;
	Lisp_abort_handler = call;
	return ret;
}

static void abort_setjmp_handler(void)
{
	Lisp_abort_throw();
}
_g lisp_abort_calltype set_abort_setjmp_handler(void)
{
	return set_abort_handler(abort_setjmp_handler);
}


/*
 *  degrade
 */
static void degrade_setjmp_handler(void)
{
	Lisp_degrade_throw();
}
_g lisp_abort_calltype set_degrade_setjmp_handler(void)
{
	return set_abort_handler(degrade_setjmp_handler);
}

