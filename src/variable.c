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
_g int      lisp_initialize    = 0;
_g addr     lisp_root[LISPINDEX_SIZE];
_g addr     lisp_nil_object    = 0;
_g addr     lisp_t_object      = 0;
_g int      lisp_info_enable   = 1;
_g enum GcMode lisp_gcsync     = GcMode_Off;


/*
 *  heap
 */
_g void   *heap_alloc = 0;
_g addr    heap_root = 0;
_g addr    heap_front = 0;
_g addr    heap_pos = 0;
_g addr    heap_tail = 0;
_g addr    heap_range = 0;
_g size_t  heap_object = 0;
_g size_t  heap_count = 0;
_g size_t  heap_gc_count = 0;
_g size_t  heap_gc_partial = 0;
_g size_t  heap_gc_full = 0;
_g size_t  heap_cons_count = 0;
_g size_t  heap_symbol_count = 0;


/*
 *  control
 */
#ifdef LISP_DEBUG_FORCE_GC
_g size_t GcCounterForce = 0;
#endif
_g size_t ControlCounter = 0;


/*
 *  clos
 */
_g addr Clos_standard_class = 0;
_g addr Clos_standard_generic = 0;
_g addr Clos_standard_method = 0;
_g addr Clos_standard_combination = 0;
_g addr Clos_standard_specializer = 0;


/*
 *  execute
 */
_g threadlocal ThreadLocal_Execute;
_g threadlocal ThreadLocal_Index;
_g threadlocal ThreadLocal_Local;
_g lisp_abort_calltype Lisp_abort_handler = NULL;


/*
 *  pointer
 */
_g struct callbind_struct pointer_table[SizePointer];

