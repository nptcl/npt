#ifndef __STREAM_ERROR_HEADER__
#define __STREAM_ERROR_HEADER__

#include <stddef.h>
#include "typedef.h"

#define close_stream_error _n(close_stream_error)
#define read_byte_stream_error _n(read_byte_stream_error)
#define unread_byte_stream_error _n(unread_byte_stream_error)
#define write_byte_stream_error _n(write_byte_stream_error)
#define read_char_stream_error _n(read_char_stream_error)
#define read_hang_stream_error _n(read_hang_stream_error)
#define unread_char_stream_error _n(unread_char_stream_error)
#define write_char_stream_error _n(write_char_stream_error)
#define getleft_stream_error _n(getleft_stream_error)
#define setleft_stream_error _n(setleft_stream_error)
#define inputp_stream_error _n(inputp_stream_error)
#define outputp_stream_error _n(outputp_stream_error)
#define interactivep_stream_error _n(interactivep_stream_error)
#define characterp_stream_error _n(characterp_stream_error)
#define binaryp_stream_error _n(binaryp_stream_error)
#define element_type_stream_error _n(element_type_stream_error)
#define external_format_stream_error _n(external_format_stream_error)
#define file_length_stream_error _n(file_length_stream_error)
#define file_position_stream_error _n(file_position_stream_error)
#define file_position_start_stream_error _n(file_position_start_stream_error)
#define file_position_end_stream_error _n(file_position_end_stream_error)
#define file_position_set_stream_error _n(file_position_set_stream_error)
#define file_charlen_stream_error _n(file_charlen_stream_error)
#define file_strlen_stream_error _n(file_strlen_stream_error)
#define listen_stream_error _n(listen_stream_error)
#define clear_input_stream_error _n(clear_input_stream_error)
#define finish_output_stream_error _n(finish_output_stream_error)
#define force_output_stream_error _n(force_output_stream_error)
#define clear_output_stream_error _n(clear_output_stream_error)
#define exitpoint_stream_error _n(exitpoint_stream_error)
#define termsize_stream_error _n(termsize_stream_error)

int close_stream_error(addr pos, addr *ret);
int read_byte_stream_error(addr stream, addr *value, int *ret);
int unread_byte_stream_error(addr stream, byte c);
int write_byte_stream_error(addr stream, addr pos);
int read_char_stream_error(addr stream, unicode *c, int *ret);
int read_hang_stream_error(addr stream, unicode *c, int *hang, int *ret);
int unread_char_stream_error(addr stream, unicode c);
int write_char_stream_error(addr stream, unicode c);
int getleft_stream_error(addr stream, size_t *ret);
int setleft_stream_error(addr stream, size_t value);
int inputp_stream_error(addr stream, int *ret);
int outputp_stream_error(addr stream, int *ret);
int interactivep_stream_error(addr stream, int *ret);
int characterp_stream_error(addr stream, int *ret);
int binaryp_stream_error(addr stream, int *ret);
int element_type_stream_error(addr stream, addr *ret);
int external_format_stream_error(addr stream, addr *ret);
int file_length_stream_error(addr stream, addr *ret);
int file_position_stream_error(addr stream, size_t *value, int *ret);
int file_position_start_stream_error(addr stream, int *ret);
int file_position_end_stream_error(addr stream, int *ret);
int file_position_set_stream_error(addr stream, size_t value, int *ret);
int file_charlen_stream_error(addr stream, unicode u, size_t *value, int *ret);
int file_strlen_stream_error(addr stream, addr pos, size_t *value, int *ret);
int listen_stream_error(addr stream, int *ret);
int clear_input_stream_error(addr stream);
int finish_output_stream_error(addr stream);
int force_output_stream_error(addr stream);
int clear_output_stream_error(addr stream);
int exitpoint_stream_error(addr stream);
int termsize_stream_error(addr stream, size_t *value, int *ret);

#endif

