#ifndef __STREAM_ERROR_HEADER__
#define __STREAM_ERROR_HEADER__

#include <stddef.h>
#include "typedef.h"

_g int close_stream_error(addr pos, int abort);
_g int read_binary_stream_error(addr stream, void *pos, size_t size, size_t *ret);
_g int readforce_binary_stream_error(addr stream, void *pos, size_t size, size_t *ret);
_g int read_byte_stream_error(addr stream, byte *ret);
_g int unread_byte_stream_error(addr stream, byte c);
_g int write_binary_stream_error(addr stream, const void *pos, size_t size, size_t *ret);
_g int write_byte_stream_error(addr stream, byte c);
_g int read_char_stream_error(addr stream, unicode *ret);
_g int read_hang_stream_error(addr stream, unicode *ret, int *hang);
_g void unread_char_stream_error(addr stream, unicode c);
_g void write_char_stream_error(addr stream, unicode c);
_g int fresh_line_stream_error(addr stream);
_g int inputp_stream_error(addr stream);
_g int outputp_stream_error(addr stream);
_g int interactivep_stream_error(addr stream);
_g int characterp_stream_error(addr stream);
_g int binaryp_stream_error(addr stream);
_g void file_length_stream_error(addr stream, addr *ret);
_g int file_character_length_stream_error(addr stream, unicode u, size_t *ret);
_g int file_string_length_stream_error(addr stream, addr pos, size_t *ret);
_g int listen_stream_error(addr stream);
_g void clear_input_stream_error(addr stream);
_g void finish_output_stream_error(addr stream);
_g void force_output_stream_error(addr stream);
_g void clear_output_stream_error(addr stream);

#endif

