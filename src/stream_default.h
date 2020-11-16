#ifndef __STREAM_DEFAULT_HEADER__
#define __STREAM_DEFAULT_HEADER__

#include "typedef.h"

#define close_default_stream _n(close_default_stream)
#define read_char_default_stream _n(read_char_default_stream)
#define read_hang_default_stream _n(read_hang_default_stream)
#define unread_char_default_stream _n(unread_char_default_stream)
#define write_char_default_stream _n(write_char_default_stream)
#define getleft_default_stream _n(getleft_default_stream)
#define setleft_default_stream _n(setleft_default_stream)
#define charleft_default_stream _n(charleft_default_stream)
#define file_length_default_stream _n(file_length_default_stream)
#define file_position_default_stream _n(file_position_default_stream)
#define file_position_start_default_stream _n(file_position_start_default_stream)
#define file_position_end_default_stream _n(file_position_end_default_stream)
#define file_position_set_default_stream _n(file_position_set_default_stream)
#define finish_output_default_stream _n(finish_output_default_stream)
#define force_output_default_stream _n(force_output_default_stream)
#define clear_output_default_stream _n(clear_output_default_stream)
#define exitpoint_default_stream _n(exitpoint_default_stream)
#define termsize_default_stream _n(termsize_default_stream)

#define checkp_true_stream _n(checkp_true_stream)
#define checkp_false_stream _n(checkp_false_stream)
#define element_type_character_stream _n(element_type_character_stream)
#define element_type_io_stream _n(element_type_io_stream)
#define external_format_default_stream _n(external_format_default_stream)

int close_default_stream(addr stream, addr *ret);
int read_char_default_stream(addr stream, unicode *c, int *ret);
int read_hang_default_stream(addr stream, unicode *c, int *hang, int *ret);
int unread_char_default_stream(addr stream, unicode c);
int write_char_default_stream(addr stream, unicode c);
int getleft_default_stream(addr stream, size_t *ret);
int setleft_default_stream(addr stream, size_t value);
void charleft_default_stream(addr stream, unicode c);
int file_length_default_stream(addr stream, addr *ret);
int file_position_default_stream(addr stream, size_t *value, int *ret);
int file_position_start_default_stream(addr stream, int *ret);
int file_position_end_default_stream(addr stream, int *ret);
int file_position_set_default_stream(addr stream, size_t value, int *ret);
int finish_output_default_stream(addr stream);
int force_output_default_stream(addr stream);
int clear_output_default_stream(addr stream);
int exitpoint_default_stream(addr stream);
int termsize_default_stream(addr stream, size_t *value, int *ret);

int checkp_true_stream(addr stream, int *ret);
int checkp_false_stream(addr stream, int *ret);
int element_type_character_stream(addr stream, addr *ret);
int element_type_io_stream(addr stream, addr *ret);
int external_format_default_stream(addr stream, addr *ret);

#endif

