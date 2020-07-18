#ifndef __FILE_HEADER__
#define __FILE_HEADER__

#include "execute.h"
#include "file_type.h"
#include "stream.h"
#include "typedef.h"

#define PtrFileMemory(stream) ((struct filememory *)PtrDataStream(stream))

/*
 *  initialize
 */
_g int init_file(void);
_g void free_file(void);
_g int consolep_file(void);


/*
 *  Common Function
 */
_g void make_standard_input(addr *stream);
_g void make_standard_output(addr *stream);
_g void make_standard_error(addr *stream);
_g void update_standard_input(addr stream);
_g void update_standard_output(addr stream);
_g void update_standard_error(addr stream);
_g int script_header(addr stream);


/*
 *  stream function
 */
_g void force_close_stream_file(addr stream);
_g int close_stream_file_(addr stream, addr *ret);
_g int read_binary_file_(addr stream, void *pos, size_t size, size_t *ret);
_g int readf_binary_file_(addr stream, void *pos, size_t size, size_t *ret);
_g int read_byte_file_(addr stream, byte *c, int *ret);
_g int unread_byte_file_(addr stream, byte c);
_g int write_binary_file_(addr stream, const void *pos, size_t size, size_t *ret);
_g int write_byte_file_(addr stream, byte c);

_g int read_char_file_(addr stream, unicode *c, int *ret);
_g int read_hang_file_(addr stream, unicode *c, int *hang, int *ret);
_g int write_char_file_(addr stream, unicode c);
_g int file_length_file_(addr file, size_t *value, int *ret);
_g int file_position_file_(addr stream, size_t *value, int *ret);
_g int file_position_start_file_(addr stream, int *ret);
_g int file_position_end_file_(addr stream, int *ret);
_g int file_position_set_file_(addr stream, size_t value, int *ret);
_g int file_charlen_file_(addr stream, unicode u, size_t *value, int *ret);
_g int file_strlen_file_(addr stream, addr pos, size_t *value, int *ret);
_g void external_format_file(addr stream, addr *ret);
_g int listen_file_(addr stream, int *ret);
_g int clear_input_file_(addr stream);
_g int finish_output_file_(addr stream);
_g int force_output_file_(addr stream);
_g int clear_output_file_(addr stream);
_g int exitpoint_file_(addr stream);
_g int termsize_file_(addr stream, size_t *value, int *ret);


/*
 *  core
 */
_g int save_stream_file(addr pos);
_g int save_stream_system(addr pos);

#endif

