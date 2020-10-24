#ifndef __STREAM_DEFAULT_HEADER__
#define __STREAM_DEFAULT_HEADER__

#include "typedef.h"

#define close_default_stream _n(close_default_stream)
#define read_char_default_stream _n(read_char_default_stream)
#define read_hang_default_stream _n(read_hang_default_stream)
#define unread_char_default_stream _n(unread_char_default_stream)
#define write_char_default_stream _n(write_char_default_stream)
#define terpri_default_stream _n(terpri_default_stream)
#define getleft_default_stream _n(getleft_default_stream)
#define setleft_default_stream _n(setleft_default_stream)
#define charleft_default_stream _n(charleft_default_stream)
#define fresh_line_default_stream _n(fresh_line_default_stream)
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

_g int close_default_stream(addr stream, addr *ret);
_g int read_char_default_stream(addr stream, unicode *c, int *ret);
_g int read_hang_default_stream(addr stream, unicode *c, int *hang, int *ret);
_g int unread_char_default_stream(addr stream, unicode c);
_g int write_char_default_stream(addr stream, unicode c);
_g int terpri_default_stream(addr stream);
_g int getleft_default_stream(addr stream, size_t *ret);
_g int setleft_default_stream(addr stream, size_t value);
_g void charleft_default_stream(addr stream, unicode c);
_g int fresh_line_default_stream(addr stream, int *ret);
_g int file_length_default_stream(addr stream, addr *ret);
_g int file_position_default_stream(addr stream, size_t *value, int *ret);
_g int file_position_start_default_stream(addr stream, int *ret);
_g int file_position_end_default_stream(addr stream, int *ret);
_g int file_position_set_default_stream(addr stream, size_t value, int *ret);
_g int finish_output_default_stream(addr stream);
_g int force_output_default_stream(addr stream);
_g int clear_output_default_stream(addr stream);
_g int exitpoint_default_stream(addr stream);
_g int termsize_default_stream(addr stream, size_t *value, int *ret);

_g int checkp_true_stream(addr stream, int *ret);
_g int checkp_false_stream(addr stream, int *ret);
_g int element_type_character_stream(addr stream, addr *ret);
_g int element_type_io_stream(addr stream, addr *ret);

#endif

