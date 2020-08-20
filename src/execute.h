#ifndef __EXECUTE_HEADER__
#define __EXECUTE_HEADER__

#include <signal.h>
#include "execute_setjmp.h"
#include "execute_typedef.h"
#include "thread.h"

/* threadlocal */
#ifdef LISP_THREAD_DISABLE
#define Execute_Thread (getexecute(0))
#define Local_Thread (getexecute(0)->local)
#define Index_Thread 0
#else
#define Execute_Thread ((struct execute *)get_threadlocal(ThreadLocal_Execute))
#define Local_Thread ((LocalRoot)get_threadlocal(ThreadLocal_Local))
#define Index_Thread (*(const size_t *)get_threadlocal(ThreadLocal_Index))
#endif


/* function */
_g int init_execute(size_t);
_g void free_execute(void);
_g void set_execute_local(struct execute *ptr);
_g int make_execute(execfunction, struct execute **, size_t);
_g void setstate_execute(struct execute *, enum ThreadState);
_g int join_execute(struct execute *);
_g size_t count_execute(void);

_g int joinindex_execute(size_t);
_g int create_thread(execfunction, struct execute *);
_g int join_thread(threadhandle *);

_g struct execute *getexecute(size_t index);
_g int equal_control_restart(Execute ptr, addr control);
_g int equal_control_catch(Execute ptr, addr symbol);


/* gc sync */
_g void gcstate_execute(enum GcMode mode);
_g void gcstart_execute(struct execute *ptr);
_g void gcwait_execute(struct execute *ptr);
_g void gcend_execute(void);
_g void foreach_execute(void (*call)(struct execute *));
_g int foreach_check_execute(int (*call)(struct execute *));

#endif

