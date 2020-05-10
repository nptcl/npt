#ifndef __EXECUTE_TYPEDEF_HEADER__
#define __EXECUTE_TYPEDEF_HEADER__

#include <setjmp.h>
#include "define.h"
#include "local.h"
#include "typedef.h"
#include "typedef_thread.h"

#ifdef LISP_DEBUG
#define EXECUTE_VALUES		2
#else
#define EXECUTE_VALUES		8
#endif

/*
 *  execute
 */
enum ThreadState {
	ThreadState_Empty = 0,
	ThreadState_Run,
	ThreadState_Finish,
	ThreadState_GcWait,
	ThreadState_Join,
	ThreadState_Size
};

struct execute;
typedef void (*execfunction)(struct execute *);

struct execute {
	unsigned disable_copy_p  : 1;
	unsigned throw_point_p   : 1;
	unsigned abort           : 1;
	unsigned jump            : 1;
	/* lisp info */
	jmp_buf *exec;
	LocalRoot local;
	int result;

	/* control */
	addr control;
	addr *values, *values_list;
	size_t sizer;

	/* runcode */
	size_t throw_point;
	addr throw_control;

	/* thread info */
	size_t index;
	enum ThreadState state;
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

#endif

