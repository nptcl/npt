#ifndef __STREAM_FUNCTION_HEADER__
#define __STREAM_FUNCTION_HEADER__

#include "typedef.h"

#define close_stream_ _n(close_stream_)
#define read_binary_stream_ _n(read_binary_stream_)
#define readf_binary_stream_ _n(readf_binary_stream_)
#define read_byte_stream_ _n(read_byte_stream_)
#define unread_byte_stream_ _n(unread_byte_stream_)
#define write_binary_stream_ _n(write_binary_stream_)
#define write_byte_stream_ _n(write_byte_stream_)
#define read_char_stream_ _n(read_char_stream_)
#define read_hang_stream_ _n(read_hang_stream_)
#define unread_char_stream_ _n(unread_char_stream_)
#define write_char_stream_ _n(write_char_stream_)
#define terpri_stream_ _n(terpri_stream_)
#define getleft_stream_ _n(getleft_stream_)
#define setleft_stream_ _n(setleft_stream_)
#define fresh_line_stream_ _n(fresh_line_stream_)
#define clear_input_stream_ _n(clear_input_stream_)
#define inputp_stream_ _n(inputp_stream_)
#define outputp_stream_ _n(outputp_stream_)
#define interactivep_stream_ _n(interactivep_stream_)
#define characterp_stream_ _n(characterp_stream_)
#define binaryp_stream_ _n(binaryp_stream_)
#define element_type_stream_ _n(element_type_stream_)
#define file_length_stream_ _n(file_length_stream_)
#define file_position_stream_ _n(file_position_stream_)
#define file_position_start_stream_ _n(file_position_start_stream_)
#define file_position_end_stream_ _n(file_position_end_stream_)
#define file_position_set_stream_ _n(file_position_set_stream_)
#define file_charlen_stream_ _n(file_charlen_stream_)
#define file_strlen_stream_ _n(file_strlen_stream_)
#define listen_stream_ _n(listen_stream_)
#define finish_output_stream_ _n(finish_output_stream_)
#define force_output_stream_ _n(force_output_stream_)
#define clear_output_stream_ _n(clear_output_stream_)
#define exitpoint_stream_ _n(exitpoint_stream_)
#define termsize_stream_ _n(termsize_stream_)

_g int close_stream_(addr stream, addr *ret);
_g int read_binary_stream_(addr stream, void *pos, size_t size, size_t *ret);
_g int readf_binary_stream_(addr stream, void *pos, size_t size, size_t *ret);
_g int read_byte_stream_(addr stream, addr *pos, int *ret);
_g int unread_byte_stream_(addr stream, byte c);
_g int write_binary_stream_(addr stream, const void *pos, size_t size, size_t *ret);
_g int write_byte_stream_(addr stream, addr pos);
_g int read_char_stream_(addr stream, unicode *c, int *ret);
_g int read_hang_stream_(addr stream, unicode *c, int *hang, int *ret);
_g int unread_char_stream_(addr stream, unicode c);
_g int write_char_stream_(addr stream, unicode c);
_g int terpri_stream_(addr stream);
_g int getleft_stream_(addr stream, size_t *ret);
_g int setleft_stream_(addr stream, size_t value);
_g int fresh_line_stream_(addr stream, int *ret);
_g int clear_input_stream_(addr stream);
_g int inputp_stream_(addr stream, int *ret);
_g int outputp_stream_(addr stream, int *ret);
_g int interactivep_stream_(addr stream, int *ret);
_g int characterp_stream_(addr stream, int *ret);
_g int binaryp_stream_(addr stream, int *ret);
_g int element_type_stream_(addr stream, addr *ret);
_g int file_length_stream_(addr stream, addr *ret);
_g int file_position_stream_(addr stream, size_t *value, int *ret);
_g int file_position_start_stream_(addr stream, int *ret);
_g int file_position_end_stream_(addr stream, int *ret);
_g int file_position_set_stream_(addr stream, size_t value, int *ret);
_g int file_charlen_stream_(addr stream, unicode u, size_t *value, int *ret);
_g int file_strlen_stream_(addr stream, addr pos, size_t *value, int *ret);
_g int listen_stream_(addr stream, int *ret);
_g int finish_output_stream_(addr stream);
_g int force_output_stream_(addr stream);
_g int clear_output_stream_(addr stream);
_g int exitpoint_stream_(addr stream);
_g int termsize_stream_(addr stream, size_t *value, int *ret);

#endif

