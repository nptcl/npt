#include "clos.h"
#include "control_object.h"
#include "execute.h"
#include "heap_memory.h"
#include "pointer.h"
#include "pointer_table.h"
#include "pointer_type.h"
#include "stream.h"
#include "typedef.h"
#include "typedef_value.h"
#include "typedef_stream.h"
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


/*
 *  pointer
 */
_g struct callbind_struct pointer_table[SizePointer];


/*
 *  stream
 */
_g lisp_streamtype_close Stream_close[StreamType_Array];
_g lisp_streamtype_read_binary Stream_read_binary[StreamType_Array];
_g lisp_streamtype_readf_binary Stream_readf_binary[StreamType_Array];
_g lisp_streamtype_read_byte Stream_read_byte[StreamType_Array];
_g lisp_streamtype_unread_byte Stream_unread_byte[StreamType_Array];
_g lisp_streamtype_write_binary Stream_write_binary[StreamType_Array];
_g lisp_streamtype_write_byte Stream_write_byte[StreamType_Array];
_g lisp_streamtype_read_char Stream_read_char[StreamType_Array];
_g lisp_streamtype_read_hang Stream_read_hang[StreamType_Array];
_g lisp_streamtype_unread_char Stream_unread_char[StreamType_Array];
_g lisp_streamtype_write_char Stream_write_char[StreamType_Array];
_g lisp_streamtype_terpri Stream_terpri[StreamType_Array];
_g lisp_streamtype_getleft Stream_getleft[StreamType_Array];
_g lisp_streamtype_setleft Stream_setleft[StreamType_Array];
_g lisp_streamtype_fresh_line Stream_fresh_line[StreamType_Array];
_g lisp_streamtype_clear_input Stream_clear_input[StreamType_Array];
_g lisp_streamtype_inputp Stream_inputp[StreamType_Array];
_g lisp_streamtype_outputp Stream_outputp[StreamType_Array];
_g lisp_streamtype_interactivep Stream_interactivep[StreamType_Array];
_g lisp_streamtype_characterp Stream_characterp[StreamType_Array];
_g lisp_streamtype_binaryp Stream_binaryp[StreamType_Array];
_g lisp_streamtype_element_type Stream_element_type[StreamType_Array];
_g lisp_streamtype_file_length Stream_file_length[StreamType_Array];
_g lisp_streamtype_file_position Stream_file_position[StreamType_Array];
_g lisp_streamtype_file_position_start Stream_file_position_start[StreamType_Array];
_g lisp_streamtype_file_position_end Stream_file_position_end[StreamType_Array];
_g lisp_streamtype_file_position_set Stream_file_position_set[StreamType_Array];
_g lisp_streamtype_file_charlen Stream_file_charlen[StreamType_Array];
_g lisp_streamtype_file_strlen Stream_file_strlen[StreamType_Array];
_g lisp_streamtype_listen Stream_listen[StreamType_Array];
_g lisp_streamtype_finish_output Stream_finish_output[StreamType_Array];
_g lisp_streamtype_force_output Stream_force_output[StreamType_Array];
_g lisp_streamtype_clear_output Stream_clear_output[StreamType_Array];
_g lisp_streamtype_exitpoint Stream_exitpoint[StreamType_Array];
_g lisp_streamtype_termsize Stream_termsize[StreamType_Array];

