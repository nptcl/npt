#ifndef __STREAM_COMMON_HEADER__
#define __STREAM_COMMON_HEADER__

#include "execute.h"
#include "typedef.h"

_g int read_byte_common(Execute ptr, addr stream, addr errorp, addr value, addr *ret);
_g int write_byte_common(Execute ptr, addr value, addr stream);
_g int peek_char_common(Execute ptr, addr type, addr stream,
		addr errorp, addr value, addr recp, addr *ret);
_g int read_char_common(Execute ptr,
		addr stream, addr errorp, addr value, addr recp, addr *ret);
_g int read_char_no_hang_common(Execute ptr,
		addr stream, addr errorp, addr value, addr recp, addr *ret);
_g int terpri_common(Execute ptr, addr stream);
_g int fresh_line_common(Execute ptr, addr stream, addr *ret);
_g void unread_char_common(Execute ptr, addr pos, addr stream);
_g void write_char_common(Execute ptr, addr pos, addr stream);
_g int read_line_common(Execute ptr,
		addr stream, addr errorp, addr value, addr recp,
		addr *ret, addr *sec);
_g int write_string_common(Execute ptr, addr string, addr rest);
_g int write_line_common(Execute ptr, addr string, addr rest);
_g int read_sequence_common(addr var, addr stream, addr rest, addr *ret);
_g int write_sequence_common(LocalRoot local, addr var, addr stream, addr rest);
_g int file_position_common(Execute ptr, addr stream, addr pos, addr *ret);
_g int file_string_length_common(addr stream, addr pos, addr *ret);
_g int open_common(Execute ptr, addr pos, addr rest, addr *ret);
_g int with_open_file_common(addr form, addr *ret);
_g int close_common(Execute ptr, addr pos, addr rest, addr *ret);
_g int with_open_stream_common(addr form, addr *ret);
_g void listen_common(Execute ptr, addr stream, addr *ret);
_g void clear_input_common(Execute ptr, addr stream);
_g int make_string_input_stream_common(addr var, addr rest, addr *ret);
_g int make_string_output_stream_common(Execute ptr, addr rest, addr *ret);
_g int get_output_stream_string_common(Execute ptr, addr var, addr *ret);
_g int with_input_from_string_common(addr form, addr *ret);
_g int with_output_to_string_common(addr form, addr *ret);

#endif

