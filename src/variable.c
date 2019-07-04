#include "typedef.h"
#include "typedef_enum.h"
#include "typedef_thread.h"
#include "pointer_table.h"
#include "pointer_type.h"
#include "stream.h"

/*
 *  build
 */
_g int      lisp_initialize    = 0;
_g addr     lisp_root[LISPINDEX_SIZE];
_g addr     lisp_nil           = 0;
_g addr     lisp_t             = 0;
_g byte32   lisp_property      = 0;
/* for debug */
_g int      lisp_info_enable   = 1;


/*
 *  heap
 */
_g void *heap_alloc = 0;
_g addr  heap_root = 0;
_g addr  heap_front = 0;
_g addr  heap_pos = 0;


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
_g struct callbind_struct pointer_table[p_size];


/*
 *  stream
 */
_g int (*Stream_close[StreamType_Size])(addr, int);
_g int (*Stream_read_binary[StreamType_Size])(addr, void *, size_t, size_t *);
_g int (*Stream_readforce_binary[StreamType_Size])(addr, void *, size_t, size_t *);
_g int (*Stream_read_byte[StreamType_Size])(addr, byte *);
_g int (*Stream_unread_byte[StreamType_Size])(addr, byte);
_g int (*Stream_write_binary[StreamType_Size])(addr, const void *, size_t, size_t *);
_g int (*Stream_write_byte[StreamType_Size])(addr, byte);
_g int (*Stream_read_char[StreamType_Size])(addr, unicode *);
_g int (*Stream_read_hang[StreamType_Size])(addr, unicode *, int *);
_g void (*Stream_unread_char[StreamType_Size])(addr, unicode);
_g void (*Stream_write_char[StreamType_Size])(addr, unicode);
_g int (*Stream_fresh_line[StreamType_Size])(addr);
_g void (*Stream_clear_input[StreamType_Size])(addr);
_g int (*Stream_inputp[StreamType_Size])(addr);
_g int (*Stream_outputp[StreamType_Size])(addr);
_g int (*Stream_interactivep[StreamType_Size])(addr);
_g int (*Stream_characterp[StreamType_Size])(addr);
_g int (*Stream_binaryp[StreamType_Size])(addr);
_g void (*Stream_element_type[StreamType_Size])(addr, addr *);
_g void (*Stream_file_length[StreamType_Size])(addr, addr *);
_g int (*Stream_file_position[StreamType_Size])(addr, size_t *);
_g int (*Stream_file_position_start[StreamType_Size])(addr);
_g int (*Stream_file_position_end[StreamType_Size])(addr);
_g int (*Stream_file_position_set[StreamType_Size])(addr, size_t);
_g int (*Stream_file_character_length[StreamType_Size])(addr, unicode, size_t *);
_g int (*Stream_file_string_length[StreamType_Size])(addr, addr, size_t *);
_g int (*Stream_listen[StreamType_Size])(addr);
_g void (*Stream_finish_output[StreamType_Size])(addr);
_g void (*Stream_force_output[StreamType_Size])(addr);
_g void (*Stream_clear_output[StreamType_Size])(addr);

