#include <stdlib.h>
#include <string.h>
#include "array.h"
#include "boole.h"
#include "build.h"
#include "call.h"
#include "character.h"
#include "clos.h"
#include "clos_type.h"
#include "code.h"
#include "common.h"
#include "compile.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control.h"
#include "copy.h"
#include "declare.h"
#include "document.h"
#include "eastasian_unicode.h"
#include "encode.h"
#include "environment.h"
#include "eval.h"
#include "execute.h"
#include "execute_setjmp.h"
#include "extern.h"
#include "file.h"
#include "file_memory.h"
#include "format.h"
#include "function.h"
#include "gc.h"
#include "heap.h"
#include "heap_core.h"
#include "heap_memory.h"
#include "hashtable.h"
#include "localtime.h"
#include "make.h"
#include "object.h"
#include "package.h"
#include "package_intern.h"
#include "pathname.h"
#include "pointer.h"
#include "print.h"
#include "random_state.h"
#include "reader.h"
#include "real.h"
#include "restart.h"
#include "require.h"
#include "rt.h"
#include "scope.h"
#include "stream_init.h"
#include "structure.h"
#include "subtypep.h"
#include "sxhash.h"
#include "symbol.h"
#include "syscall.h"
#include "terme.h"
#include "type.h"
#include "type_table.h"

#ifdef LISP_EXTENSION
#include "ext_main.h"
#endif

#define DEFAULT_MEMORY		(320UL * 1024UL * 1024UL)
#define DEFAULT_STACK		(160UL * 1024UL * 1024UL)

/*
 *  Initialize
 */
void initlisp(void)
{
	clear_pointer();
	init_boole();
	init_call();
	init_clos();
	init_code();
	init_common();
	init_compile();
	init_condition();
	init_control();
	init_copy();
	init_documentation();
	init_eastasian();
	init_encode();
	init_environment();
	init_eval();
	init_extern();
	init_format();
	init_function();
	init_heap();
	init_localtime();
	init_make();
	init_package();
	init_print();
	init_reader();
	init_restart();
	init_rt();
	init_scope();
	init_subtypep();
	init_stream();
	init_structure();
	init_sxhash();
	init_syscall();
	init_terme();
	init_type();
#ifdef LISP_EXTENSION
	lisps_init_extension();
#endif
}

static void clearlisp_force(void)
{
	int i;

	for (i = 0; i < LISPINDEX_SIZE; i++)
		lisp_root[i] = 0;
	lisp_nil_object = 0;
	lisp_t_object = 0;
	lisp_gcsync = GcMode_Off;
	Lisp_abort_handler = NULL;
}

int alloclisp(size_t heap, size_t stack)
{
	if (lisp_initialize) {
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
	if (alloc_heap(heap)) {
		Debug("alloc_heap error.");
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
	lisp_initialize = 1;
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
	if (lisp_initialize) {
		free_random_state();
		free_symbol();
		free_execute();
		free_heap();
		free_file();
		clearlisp_force();
	}
	lisp_initialize = 0;
}


/*
 *  buildlisp
 */
void setlisproot(enum LISPINDEX index, addr value)
{
	SetChain(value, 0xFF);
	lisp_root[index] = value;
}

void build_lisproot(Execute ptr)
{
	size_t i;

	nil_heap();
	t_heap();
	for (i = 0; i < LISPINDEX_SIZE; i++)
		lisp_root[i] = Nil;
	lisp_root[LISPINDEX_NIL] = Nil;
	lisp_root[LISPINDEX_T] = T;
	ptr->control = Nil;
}

static void push_features(const char *name)
{
	addr symbol, keyword, cons;

	GetConst(SPECIAL_FEATURES, &symbol);
	Error(internchar_keyword_(name, &keyword, NULL));
	GetValueSymbol(symbol, &cons);
	Check(find_list_eq_unsafe(keyword, cons), "push error");
	cons_heap(&cons, keyword, cons);
	SetValueSymbol(symbol, cons);
}

static void set_features(void)
{
	addr symbol;

	GetConst(SPECIAL_FEATURES, &symbol);
	SetValueSymbol(symbol, Nil);

	push_features("ANSI-CL");
	push_features("COMMON-LISP");
#ifdef LISP_ANSIC
	push_features("ANSI-C");
	push_features(LISPNAME "-ANSI-C");
#endif
#ifdef LISP_ANSIC_WINDOWS
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
	push_features("CPU-64");
#endif
#ifdef LISP_ARCH_32BIT
	push_features("CPU-32");
#endif
#ifdef LISP_64BIT
	push_features("FIXNUM-64");
#endif
#ifdef LISP_32BIT
	push_features("FIXNUM-32");
#endif

	push_features(LISPNAME);
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

#ifdef LISP_FLOAT_LONG_64
	push_features("LONG-FLOAT-64");
#endif
#ifdef LISP_FLOAT_LONG_80
	push_features("LONG-FLOAT-80");
#endif
#ifdef LISP_FLOAT_LONG_128
	push_features("LONG-FLOAT-128");
#endif

#ifdef LISP_DEBUG
#ifdef LISP_DEBUG_FORCE_GC
	push_features("FORCE-GC");
#endif
#endif

#if 0
#ifdef LISP_THREAD_ENABLE
	push_features("THREAD");
#endif
#endif

#ifdef LISP_EXTENSION
	push_features("EXTENSION");
#endif
}

static void set_pretty_printing(void)
{
	addr symbol;
	GetConst(SPECIAL_PRINT_PRETTY, &symbol);
	SetValueSymbol(symbol, T);
}

void buildlisp(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_function();
	build_character();
	build_real();
	build_package();
	build_stream();
	build_symbol();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_syscall();
	build_terme();
	build_common();
	build_print(ptr);
	build_environment(ptr);
	build_documentation(ptr);
	build_reader();
	build_pathname();
	build_declare();
	build_code();
	build_require();
	build_rt();
	set_features();
	set_pretty_printing();
#ifdef LISP_EXTENSION
	lisps_build_extension(ptr);
#endif
	gcexec(GcMode_Full);
}


/*
 *  core
 */
int save_lisp(filestream fm)
{
	int i;

	/* heap */
	if (save_heap(fm)) {
		Debug("save_heap error.");
		return 1;
	}

	/* build.c */
	for (i = 0; i < LISPINDEX_SIZE; i++) {
		if (writeaddr_filememory(fm, lisp_root[i])) {
			Debug2("writeaddr error: lisp_root[%d].", i);
			return 1;
		}
	}

	return 0;
}

int load_lisp(filestream fm)
{
	int i;

	/* heap */
	if (load_heap(fm)) {
		Debug("load_heap error.");
		return 1;
	}

	/* build.c */
	for (i = 0; i < LISPINDEX_SIZE; i++) {
		if (readaddr_filememory(fm, &(lisp_root[i]))) {
			Debug2("readaddr error: lisp_root[%d].", i);
			return 1;
		}
	}
	lisp_nil_object = lisp_root[LISPINDEX_NIL];
	lisp_t_object = lisp_root[LISPINDEX_T];

	return 0;
}

