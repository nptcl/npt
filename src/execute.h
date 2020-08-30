#ifndef __EXECUTE_HEADER__
#define __EXECUTE_HEADER__

#include <signal.h>
#include "execute_setjmp.h"
#include "execute_typedef.h"
#include "thread.h"

#define init_execute _n(init_execute)
#define free_execute _n(free_execute)
#define set_execute_local _n(set_execute_local)
#define make_execute _n(make_execute)
#define setstate_execute _n(setstate_execute)
#define join_execute _n(join_execute)
#define count_execute _n(count_execute)
#define joinindex_execute _n(joinindex_execute)
#define create_thread _n(create_thread)
#define join_thread _n(join_thread)
#define getexecute _n(getexecute)
#define equal_control_restart _n(equal_control_restart)
#define equal_control_catch _n(equal_control_catch)
#define gcstate_execute _n(gcstate_execute)
#define gcstart_execute _n(gcstart_execute)
#define gcwait_execute _n(gcwait_execute)
#define gcend_execute _n(gcend_execute)
#define foreach_execute _n(foreach_execute)
#define foreach_check_execute _n(foreach_check_execute)

#define Degrade_execute_Execute _n(Degrade_execute_Execute)
#define Degrade_execute_Size _n(Degrade_execute_Size)
#define Degrade_execute_Position _n(Degrade_execute_Position)

/* threadlocal */
#ifdef LISP_THREAD_DISABLE
#define Execute_Thread (getexecute(0))
#define Local_Thread (getexecute(0)->local)
#define Index_Thread 0
#else
#define Execute_Thread ((struct execute *)lispd_get_threadlocal(ThreadLocal_Execute))
#define Local_Thread ((LocalRoot)lispd_get_threadlocal(ThreadLocal_Local))
#define Index_Thread (*(const size_t *)lispd_get_threadlocal(ThreadLocal_Index))
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

