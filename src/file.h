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
_g int read_binary_file(addr stream, void *pos, size_t size, size_t *ret);
_g int readforce_binary_file(addr stream, void *pos, size_t size, size_t *ret);
_g int read_byte_file(addr stream, byte *c);
_g int unread_byte_file(addr stream, byte c);
_g int write_binary_file(addr stream, const void *pos, size_t size, size_t *ret);
_g int write_byte_file(addr stream, byte c);

_g int read_char_file(addr stream, unicode *c);
_g int read_hang_file(addr stream, unicode *c, int *hang);
_g void write_char_file(addr stream, unicode c);
_g int file_length_file(addr file, size_t *ret);
_g int file_position_file(addr stream, size_t *ret);
_g int file_position_start_file(addr stream);
_g int file_position_end_file(addr stream);
_g int file_position_set_file(addr stream, size_t pos);
_g int file_character_length_file(addr stream, unicode u, size_t *ret);
_g int file_string_length_file(addr stream, addr pos, size_t *ret);
_g void external_format_file(addr stream, addr *ret);
_g int listen_file(addr stream);
_g void clear_input_file(addr stream);
_g void finish_output_file(addr stream);
_g void force_output_file(addr stream);
_g void clear_output_file(addr stream);
_g void exitpoint_file(addr stream);
_g int terminal_width_file(addr stream, size_t *ret);


/*
 *  core
 */
_g int save_stream_file(addr pos);
_g int save_stream_system(addr pos);

#endif

