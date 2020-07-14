#ifndef __LISP_TYPEDEF_STREAM_HEADER__
#define __LISP_TYPEDEF_STREAM_HEADER__

#include <stddef.h>
#include "typedef_basic.h"

typedef int (*lisp_streamtype_close)(addr, addr *);
typedef int (*lisp_streamtype_read_binary)(addr, void *, size_t, size_t *);
typedef int (*lisp_streamtype_readforce_binary)(addr, void *, size_t, size_t *);
typedef int (*lisp_streamtype_read_byte)(addr, byte *);
typedef int (*lisp_streamtype_unread_byte)(addr, byte);
typedef int (*lisp_streamtype_write_binary)(addr, const void *, size_t, size_t *);
typedef int (*lisp_streamtype_write_byte)(addr, byte);
typedef int (*lisp_streamtype_read_char)(addr, unicode *);
typedef int (*lisp_streamtype_read_hang)(addr, unicode *, int *);
typedef void (*lisp_streamtype_unread_char)(addr, unicode);
typedef void (*lisp_streamtype_write_char)(addr, unicode);
typedef void (*lisp_streamtype_terpri)(addr);
typedef size_t (*lisp_streamtype_getleft)(addr);
typedef void (*lisp_streamtype_setleft)(addr, size_t);
typedef int (*lisp_streamtype_fresh_line)(addr);
typedef void (*lisp_streamtype_clear_input)(addr);
typedef int (*lisp_streamtype_inputp)(addr);
typedef int (*lisp_streamtype_outputp)(addr);
typedef int (*lisp_streamtype_interactivep)(addr);
typedef int (*lisp_streamtype_characterp)(addr);
typedef int (*lisp_streamtype_binaryp)(addr);
typedef void (*lisp_streamtype_element_type)(addr, addr *);
typedef void (*lisp_streamtype_file_length)(addr, addr *);
typedef int (*lisp_streamtype_file_position)(addr, size_t *);
typedef int (*lisp_streamtype_file_position_start)(addr);
typedef int (*lisp_streamtype_file_position_end)(addr);
typedef int (*lisp_streamtype_file_position_set)(addr, size_t);
typedef int (*lisp_streamtype_file_character_length)(addr, unicode, size_t *);
typedef int (*lisp_streamtype_file_string_length)(addr, addr, size_t *);
typedef int (*lisp_streamtype_listen)(addr);
typedef void (*lisp_streamtype_finish_output)(addr);
typedef void (*lisp_streamtype_force_output)(addr);
typedef void (*lisp_streamtype_clear_output)(addr);
typedef void (*lisp_streamtype_exitpoint)(addr);
typedef int (*lisp_streamtype_terminal_width)(addr, size_t *);

#endif

