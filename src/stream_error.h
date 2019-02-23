#ifndef __STREAM_ERROR_HEADER__
#define __STREAM_ERROR_HEADER__

#include <stddef.h>
#include "typedef.h"

int close_stream_error(addr pos, int abort);
int read_binary_stream_error(addr stream, void *pos, size_t size, size_t *ret);
int readforce_binary_stream_error(addr stream, void *pos, size_t size, size_t *ret);
int read_byte_stream_error(addr stream, byte *ret);
int unread_byte_stream_error(addr stream, byte c);
int write_binary_stream_error(addr stream, const void *pos, size_t size, size_t *ret);
int write_byte_stream_error(addr stream, byte c);
int read_char_stream_error(addr stream, unicode *ret);
int read_hang_stream_error(addr stream, unicode *ret, int *hang);
void unread_char_stream_error(addr stream, unicode c);
void write_char_stream_error(addr stream, unicode c);
int fresh_line_stream_error(addr stream);
int inputp_stream_error(addr stream);
int outputp_stream_error(addr stream);
int interactivep_stream_error(addr stream);
void file_length_stream_error(addr stream, addr *ret);
int file_character_length_stream_error(addr stream, unicode u, size_t *ret);
int file_string_length_stream_error(addr stream, addr pos, size_t *ret);
int listen_stream_error(addr stream);
void clear_input_stream_error(addr stream);
void finish_output_stream_error(addr stream);
void force_output_stream_error(addr stream);
void clear_output_stream_error(addr stream);

#endif

