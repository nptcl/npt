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


/*
 *  heap
 */
_g void   *heap_alloc = 0;
_g addr    heap_root = 0;
_g addr    heap_front = 0;
_g addr    heap_pos = 0;
_g size_t  heap_object = 0;
_g size_t  heap_count = 0;
_g size_t  heap_gc_count = 0;


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
_g LispStreamTypeArray(close);
_g LispStreamTypeArray(read_binary);
_g LispStreamTypeArray(readforce_binary);
_g LispStreamTypeArray(read_byte);
_g LispStreamTypeArray(unread_byte);
_g LispStreamTypeArray(write_binary);
_g LispStreamTypeArray(write_byte);
_g LispStreamTypeArray(read_char);
_g LispStreamTypeArray(read_hang);
_g LispStreamTypeArray(unread_char);
_g LispStreamTypeArray(write_char);
_g LispStreamTypeArray(terpri);
_g LispStreamTypeArray(getleft);
_g LispStreamTypeArray(setleft);
_g LispStreamTypeArray(fresh_line);
_g LispStreamTypeArray(clear_input);
_g LispStreamTypeArray(inputp);
_g LispStreamTypeArray(outputp);
_g LispStreamTypeArray(interactivep);
_g LispStreamTypeArray(characterp);
_g LispStreamTypeArray(binaryp);
_g LispStreamTypeArray(element_type);
_g LispStreamTypeArray(file_length);
_g LispStreamTypeArray(file_position);
_g LispStreamTypeArray(file_position_start);
_g LispStreamTypeArray(file_position_end);
_g LispStreamTypeArray(file_position_set);
_g LispStreamTypeArray(file_character_length);
_g LispStreamTypeArray(file_string_length);
_g LispStreamTypeArray(listen);
_g LispStreamTypeArray(finish_output);
_g LispStreamTypeArray(force_output);
_g LispStreamTypeArray(clear_output);
_g LispStreamTypeArray(exitpoint);
_g LispStreamTypeArray(terminal_width);

