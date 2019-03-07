#ifndef __EXECUTE_HEADER__
#define __EXECUTE_HEADER__

#include <setjmp.h>
#include "thread.h"
#include "typedef.h"
#include "lisptype.h"
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
	ExecuteControl_Size
};

struct execute;
typedef void (*execfunction)(struct execute *);

struct execute {
	/* lisp info */
	jmp_buf *exec;
	LocalRoot local;
	addr control;
	enum ExecuteControl signal;
	void *data;
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

struct CodeJump {
	Execute ptr;
	lispcode code;
	jmp_buf jump;
};
typedef struct CodeJump codejump;


/*
 *  threadlocal
 */
extern threadlocal ThreadLocal_Execute;
extern threadlocal ThreadLocal_Index;
extern threadlocal ThreadLocal_Local;

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
int init_execute(size_t);
void free_execute(void);
void set_execute_local(struct execute *ptr);
int make_execute(execfunction, struct execute **, size_t);
void setstate_execute(struct execute *, enum ThreadState);
int join_execute(struct execute *);
size_t count_execute(void);

int joinindex_execute(size_t);
int create_thread(execfunction, struct execute *);
int join_thread(threadhandle *);

struct execute *getexecute(size_t index);
void exitexecute(struct execute *ptr, lispcode code);
void exitindex(size_t index, lispcode code);
#define exitthis(code) exitexecute(Execute_Thread, code)
void abortexecute(struct execute *ptr);
void abortindex(size_t index);
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
int begin_code_check(Execute ptr, lispcode *code);
void end_code(Execute ptr);
void end_code_thread(void);
int code_run_p(lispcode code);
int code_end_p(lispcode code);
int code_error_p(lispcode code);

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
void begin_switch_check(Execute ptr, codejump *code);
void end_switch(codejump *code);
int codejump_run_p(codejump *code);
int codejump_end_p(codejump *code);
int codejump_error_p(codejump *code);

void exit_code(Execute ptr, lispcode code);
void exit_code_thread(lispcode code);
void break_code(Execute ptr);
void break_code_thread(void);
void throw_code(Execute ptr, lispcode code);
void throw_code_thread(lispcode code);
void throw_switch(codejump *code);


/*
 *  gc sync
 */
void gcstate_execute(void);
void gcstart_execute(struct execute *ptr);
void gcwait_execute(struct execute *ptr);
void gcend_execute(void);
void foreach_execute(void (*call)(struct execute *));
int foreach_check_execute(int (*call)(struct execute *));

/*
 *  exit
 */
void exit_execute(int value);

#endif

