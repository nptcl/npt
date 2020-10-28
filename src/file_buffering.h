#ifndef __FILE_BUFFERING_HEADER__
#define __FILE_BUFFERING_HEADER__

#include "define.h"
#include "file_type.h"
#include "typedef.h"

#define read_low_buffering _n(read_low_buffering)
#define write_low_buffering _n(write_low_buffering)
#define close_low_buffering _n(close_low_buffering)
#define flush_low_buffering _n(flush_low_buffering)
#define read_ready_low_buffering _n(read_ready_low_buffering)
#define file_length_low_buffering _n(file_length_low_buffering)
#define file_position_low_buffering _n(file_position_low_buffering)
#define file_position_start_low_buffering _n(file_position_start_low_buffering)
#define file_position_end_low_buffering _n(file_position_end_low_buffering)
#define file_position_set_low_buffering _n(file_position_set_low_buffering)

#define close_stream_buffering_ _n(close_stream_buffering_)
#define read_binary_buffering_ _n(read_binary_buffering_)
#define read_byte_buffering_ _n(read_byte_buffering_)
#define write_binary_buffering_ _n(write_binary_buffering_)
#define write_byte_buffering_ _n(write_byte_buffering_)
#define read_char_buffering_ _n(read_char_buffering_)
#define read_hang_buffering_ _n(read_hang_buffering_)
#define write_char_buffering_ _n(write_char_buffering_)
#define file_length_buffering_ _n(file_length_buffering_)
#define file_position_buffering_ _n(file_position_buffering_)
#define file_position_start_buffering_ _n(file_position_start_buffering_)
#define file_position_end_buffering_ _n(file_position_end_buffering_)
#define file_position_set_buffering_ _n(file_position_set_buffering_)
#define finish_output_buffering_ _n(finish_output_buffering_)
#define exitpoint_buffering_ _n(exitpoint_buffering_)

_g int read_low_buffering(filestream fm, byte *pos, size_t size, size_t *ret);
_g int write_low_buffering(filestream fm, const byte *pos, size_t size, size_t *ret);
_g int close_low_buffering(filestream fm);
_g int flush_low_buffering(filestream fm);
_g int read_ready_low_buffering(filestream fm);
_g int file_length_low_buffering(filestream fm, size_t *ret);
_g int file_position_low_buffering(filestream fm, size_t *ret);
_g int file_position_start_low_buffering(filestream fm);
_g int file_position_end_low_buffering(filestream fm);
_g int file_position_set_low_buffering(filestream fm, size_t pos);

_g int close_stream_buffering_(addr stream, addr *ret);
_g int read_binary_buffering_(addr stream, void *pos, size_t size, size_t *ret);
_g int read_byte_buffering_(addr stream, addr *value, int *ret);
_g int write_binary_buffering_(addr stream, const void *pos, size_t size, size_t *ret);
_g int write_byte_buffering_(addr stream, addr pos);
_g int read_char_buffering_(addr stream, unicode *c, int *ret);
_g int read_hang_buffering_(addr stream, unicode *c, int *hang, int *ret);
_g int write_char_buffering_(addr stream, unicode c);
_g int file_length_buffering_(addr file, size_t *value, int *ret);
_g int file_position_buffering_(addr file, size_t *value, int *ret);
_g int file_position_start_buffering_(addr stream, int *ret);
_g int file_position_end_buffering_(addr stream, int *ret);
_g int file_position_set_buffering_(addr stream, size_t value, int *ret);
_g int finish_output_buffering_(addr stream);
_g int exitpoint_buffering_(addr stream);

#endif

