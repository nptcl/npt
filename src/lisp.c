#include <stdlib.h>
#include <string.h>
#include "array.h"
#include "calltype.h"
#include "character.h"
#include "clos.h"
#include "clos_type.h"
#include "code.h"
#include "common.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "copy.h"
#include "eval_declare.h"
#include "execute.h"
#include "fasl.h"
#include "file.h"
#include "file_memory.h"
#include "format.h"
#include "gc.h"
#include "heap.h"
#include "hashtable.h"
#include "lisp.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "print.h"
#include "random_state.h"
#include "readtable.h"
#include "real_common.h"
#include "rt.h"
#include "stream.h"
#include "sxhash.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "user.h"

#define DEFAULT_MEMORY		(320UL * 1024UL * 1024UL)
#define DEFAULT_STACK		(160UL * 1024UL * 1024UL)


/*
 *  Variables
 */
int      lisp_init          = 0;
addr     lisp_root[LISPINDEX_SIZE];
addr     lisp_nil           = 0;
addr     lisp_t             = 0;
byte32   lisp_property      = 0;
/* for debug */
int      lisp_info_enable	= 1;


/*
 *  property
 */
void setproperty(int index, int value)
{
	Check(lisp_init == 0, "lisp error");
	Check(index < 0 || 32 <= index, "range error");
	SetShiftValue(lisp_property, index, value, 1UL, byte32);
}

int getproperty(int index)
{
	Check(lisp_init == 0, "lisp error");
	Check(index < 0 || 32 <= index, "range error");
	return GetShiftValue(lisp_property, index, 1UL);
}


/*
 *  Initialize
 */
void initlisp(void)
{
	init_class_of();
	init_control();
	init_copy();
	init_fasl();
	init_format();
	init_print();
	init_readtable();
	init_stream();
	init_sxhash();
	init_type();
}

static void clearlisp_force(void)
{
	int i;

	for (i = 0; i < LISPINDEX_SIZE; i++)
		lisp_root[i] = 0;
	lisp_nil           = 0;
	lisp_t             = 0;
	lisp_property      = 0;
}

int alloclisp(size_t heap, size_t stack)
{
	if (lisp_init) {
		Debug("lisp object already allocated.");
		return 1;
	}

	if (heap == 0)
		heap = DEFAULT_MEMORY;
	if (stack == 0)
		stack = DEFAULT_STACK;
	if (heap < 1024UL * 1024UL) {
		Debug("heap size must be greater than 1MByte.");
		return 1;
	}
	if (stack < 1024UL * 1024UL) {
		Debug("stack size must be greater than 1MByte.");
		return 1;
	}
	clearlisp_force();

	/* file */
	if (init_file()) {
		Debug("init_file error.");
		return 1;
	}

	/* heap */
	if (init_heap(heap)) {
		Debug("init_heap error.");
		goto error_file;
	}

	/* execute */
	if (init_execute(stack)) {
		Debug("init_execute error");
		goto error_heap;
	}

	/* symbol */
	if (init_symbol()) {
		Debug("init_symbol error");
		goto error_execute;
	}

	/* random_state */
	if (init_random_state()) {
		Debug("init_random_state error");
		goto error_object;
	}

	/* check */
	lisp_init = 1;

	return 0;

error_object:
	free_symbol();
error_execute:
	free_execute();
error_heap:
	free_heap();
error_file:
	free_file();
	return 1;
}

void freelisp(void)
{
	if (lisp_init) {
		free_random_state();
		free_symbol();
		free_execute();
		free_heap();
		free_file();
		clearlisp_force();
	}
	lisp_init = 0;
}


/*
 *  buildlisp
 */
void build_lisproot(Execute ptr)
{
	size_t i;

	nil_heap();
	t_heap();
	lisp_root[LISPINDEX_NIL] = Nil;
	lisp_root[LISPINDEX_T] = T;
	for (i = 0; i < LISPINDEX_SIZE; i++)
		lisp_root[i] = Nil;
	ptr->control = Nil;
}

static void push_features(const char *name)
{
	addr symbol, keyword, cons;

	GetConst(SPECIAL_FEATURES, &symbol);
	internchar_keyword(name, &keyword);
	GetValueSymbol(symbol, &cons);
	Check(find_list_eq_unsafe(keyword, cons), "push error");
	cons_heap(&cons, keyword, cons);
	SetValueSymbol(symbol, cons);
}

static void intern_features(void)
{
	addr symbol;

	GetConst(SPECIAL_FEATURES, &symbol);
	SetValueSymbol(symbol, Nil);

	push_features("ANSI-CL");
	push_features("COMMON-LISP");
#ifdef LISP_ANSI
	push_features("ANSI-C");
	push_features(LISPNAME "-ANSI-C");
#endif
#ifdef LISP_ANSI_WINDOWS
	push_features("ANSI-C-WINDOWS");
	push_features(LISPNAME "-ANSI-C-WINDOWS");
#endif
#ifdef LISP_FREEBSD
	push_features("UNIX");
	push_features("FREEBSD");
#endif
#ifdef LISP_LINUX
	push_features("UNIX");
	push_features("LINUX");
#endif
#ifdef LISP_WINDOWS
	push_features("WINDOWS");
	push_features("WIN32");
#endif

#ifdef LISP_ARCH_64BIT
	push_features("ARCH-64-BIT");
	push_features("64-BIT");
#endif
#ifdef LISP_ARCH_32BIT
	push_features("ARCH-32-BIT");
	push_features("32-BIT");
#endif

	push_features(LISPNAME);
#ifdef LISP_64BIT
	push_features(LISPNAME "-64-BIT");
#endif
#ifdef LISP_32BIT
	push_features(LISPNAME "-32-BIT");
#endif

#ifdef LISP_DEBUG
	push_features(LISPNAME "-DEBUG");
#endif
#ifdef LISP_DEGRADE
	push_features(LISPNAME "-DEGRADE");
#endif
#ifdef LISP_COMPLEX_INACCURACY
	push_features("MATH-INACCURACY");
#endif
#ifdef __cplusplus
	push_features("CPLUSPLUS");
#else
	push_features("C99");
#endif

#if 0
#ifdef LISP_THREAD_ENABLE
	push_features("THREAD");
#endif
#endif
}

void buildlisp(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_character();
	build_float();
	build_package();
	build_stream();
	build_symbol();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_calltype();
	build_syscall();
	build_common();
	build_readtable();
	build_pathname();
	build_eval_declare();
	build_fasl();
	build_code();
	build_user();
	build_rt();
	intern_features();
	gcexec();
}


/*
 *  core
 */
int save_lisp(struct filememory *fm)
{
	int i;

	/* heap */
	IfDebug(save_heap(fm), "save_heap error.");

	/* lisp.c */
	IfDebug(writecheck_filememory(fm, &lisp_property, sizeoft(lisp_property)),
			"writecheck error: lisp_property.");
	for (i = 0; i < LISPINDEX_SIZE; i++) {
		IfDebug(writeaddr_filememory(fm, lisp_root[i]), "writeaddr error: lisp_root.");
	}

	return 0;
}

int load_lisp(struct filememory *fm)
{
	int i;

	/* heap */
	IfDebug(load_heap(fm), "load_heap error.");

	/* lisp.c */
	IfDebug(readcheck_filememory(fm, &lisp_property, sizeoft(lisp_property)),
			"readcheck error: lisp_property.");
	for (i = 0; i < LISPINDEX_SIZE; i++) {
		IfDebug(readaddr_filememory(fm, &(lisp_root[i])), "readaddr error: lisp_root.");
	}
	lisp_nil = lisp_root[LISPINDEX_NIL];
	lisp_t = lisp_root[LISPINDEX_T];

	return 0;
}

