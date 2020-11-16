#include "clos.h"
#include "control_object.h"
#include "execute.h"
#include "heap_memory.h"
#include "pointer.h"
#include "pointer_table.h"
#include "pointer_type.h"
#include "typedef.h"
#include "typedef_value.h"
#include "typedef_thread.h"

/*
 *  build
 */
int      lisp_initialize    = 0;
addr     lisp_root[LISPINDEX_SIZE];
addr     lisp_nil_object    = 0;
addr     lisp_t_object      = 0;
int      lisp_info_enable   = 1;
enum GcMode lisp_gcsync     = GcMode_Off;


/*
 *  heap
 */
void   *heap_alloc = 0;
addr    heap_root = 0;
addr    heap_front = 0;
addr    heap_pos = 0;
addr    heap_tail = 0;
addr    heap_range = 0;
size_t  heap_object = 0;
size_t  heap_count = 0;
size_t  heap_gc_count = 0;
size_t  heap_gc_partial = 0;
size_t  heap_gc_full = 0;
size_t  heap_cons_count = 0;
size_t  heap_symbol_count = 0;


/*
 *  control
 */
#ifdef LISP_DEBUG_FORCE_GC
size_t GcCounterForce = 0;
#endif
size_t ControlCounter = 0;


/*
 *  clos
 */
addr Clos_standard_class = 0;
addr Clos_standard_generic = 0;
addr Clos_standard_method = 0;
addr Clos_standard_combination = 0;
addr Clos_standard_specializer = 0;


/*
 *  execute
 */
threadlocal ThreadLocal_Execute;
threadlocal ThreadLocal_Index;
threadlocal ThreadLocal_Local;
lisp_abort_calltype Lisp_abort_handler = NULL;


/*
 *  pointer
 */
struct callbind_struct pointer_table[SizePointer];

