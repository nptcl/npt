#ifndef __EXECUTE_HEADER__
#define __EXECUTE_HEADER__

#include <setjmp.h>
#include "define.h"
#include "typedef_thread.h"
#include "local.h"

/*
 *  execute
 */
enum ThreadState {
	ThreadState_Empty = 0,
	ThreadState_Run,
	ThreadState_Finish,
	ThreadState_Signal,
	ThreadState_GcStart,
	ThreadState_Join,
	ThreadState_Size
};

enum ExecuteControl {
	ExecuteControl_Run,
	ExecuteControl_End,
	ExecuteControl_Point,
	ExecuteControl_Throw,
	ExecuteControl_Jump,
	ExecuteControl_Size
};

struct execute;
typedef void (*execfunction)(struct execute *);

struct taginfo_struct {
	unsigned open : 1;
	unsigned thr : 1;
	unsigned wake : 1;
	size_t point;
	addr control;
};

struct execute {
	/* lisp info */
	jmp_buf *exec;
	LocalRoot local;
	addr control;
	enum ExecuteControl signal;
	struct taginfo_struct *taginfo;
	int result;

	/* thread info */
	enum ThreadState state;
	size_t index;
	byte property;
	mutexlite mutex;
	execfunction routine;
	threadhandle handle;
#ifdef LISP_THREAD_WINDOWS
	threadid handleid;
#endif
};
typedef struct execute *Execute;

struct CodeJump {
	Execute ptr;
	lispcode code;
	jmp_buf jump;
};
typedef struct CodeJump codejump;


/*
 *  threadlocal
 */
__extern threadlocal ThreadLocal_Execute;
__extern threadlocal ThreadLocal_Index;
__extern threadlocal ThreadLocal_Local;

#ifdef LISP_THREAD_DISABLE
#define Execute_Thread (getexecute(0))
#define Local_Thread (getexecute(0)->local)
#define Index_Thread 0
#else
#define Execute_Thread ((struct execute *)get_threadlocal(ThreadLocal_Execute))
#define Local_Thread ((LocalRoot)get_threadlocal(ThreadLocal_Local))
#define Index_Thread (*(const size_t *)get_threadlocal(ThreadLocal_Index))
#endif


/*
 *  function
 */
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
_g void exitexecute(struct execute *ptr, lispcode code);
_g void exitindex(size_t index, lispcode code);
#define exitthis(code) exitexecute(Execute_Thread, code)
_g void abortexecute(struct execute *ptr);
_g void abortindex(size_t index);
#define abortthis() abortexecute(Execute_Thread)


/*
 *  codejump
 */
#define setjmp_execute(ptr, code) { \
	*(int *)(code) = setjmp(*(ptr)->exec); \
}
#define begin_code(ptr, code) { \
	if (begin_code_check((ptr), (code))) { \
		int __begin_code; \
		setjmp_execute((ptr), &__begin_code); \
		*(code) = (lispcode)__begin_code; \
	} \
}
#define begin_code_thread(code) { \
	Execute __ptr = Execute_Thread; \
	begin_code(__ptr, code); \
}
_g int begin_code_check(Execute ptr, lispcode *code);
_g void end_code(Execute ptr);
_g void end_code_thread(void);
_g int code_run_p(lispcode code);
_g int code_end_p(lispcode code);
_g int code_error_p(lispcode code);

#define begin_switch(ptr, jump) { \
	int __begin_value; \
	begin_switch_check((ptr), (jump)); \
	setjmp_execute((ptr), &__begin_value); \
	(jump)->code = (lispcode)__begin_value; \
}
#define begin_switch_thread(jump) { \
	Execute __ptr = Execute_Thread; \
	begin_switch(__ptr, jump); \
}
_g void begin_switch_check(Execute ptr, codejump *code);
_g void end_switch(codejump *code);
_g int codejump_run_p(codejump *code);
_g int codejump_end_p(codejump *code);
_g int codejump_error_p(codejump *code);
_g int codejump_control_p(codejump *code);

_g void exit_code(Execute ptr, lispcode code);
_g void exit_code_thread(lispcode code);
_g void break_code(Execute ptr);
_g void break_code_thread(void);
_g void throw_code(Execute ptr, lispcode code);
_g void throw_code_thread(lispcode code);
_g void throw_switch(codejump *code);
_g int equal_control_restart(Execute ptr, addr control);


/*
 *  gc sync
 */
_g void gcstate_execute(void);
_g void gcstart_execute(struct execute *ptr);
_g void gcwait_execute(struct execute *ptr);
_g void gcend_execute(void);
_g void foreach_execute(void (*call)(struct execute *));
_g int foreach_check_execute(int (*call)(struct execute *));

/*
 *  exit
 */
_g void exit_execute(int value);

#endif

