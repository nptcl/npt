#ifndef __CALL_STREAMS_HEADER__
#define __CALL_STREAMS_HEADER__

#include "execute.h"
#include "typedef.h"

#define read_byte_common _n(read_byte_common)
#define write_byte_common _n(write_byte_common)
#define peek_char_common _n(peek_char_common)
#define read_char_common _n(read_char_common)
#define read_char_no_hang_common _n(read_char_no_hang_common)
#define terpri_common _n(terpri_common)
#define fresh_line_common _n(fresh_line_common)
#define unread_char_common _n(unread_char_common)
#define write_char_common _n(write_char_common)
#define read_line_common _n(read_line_common)
#define write_string_common _n(write_string_common)
#define write_line_common _n(write_line_common)
#define read_sequence_common _n(read_sequence_common)
#define write_sequence_common _n(write_sequence_common)
#define file_position_common _n(file_position_common)
#define file_string_length_common _n(file_string_length_common)
#define open_common _n(open_common)
#define with_open_file_common _n(with_open_file_common)
#define close_common _n(close_common)
#define with_open_stream_common _n(with_open_stream_common)
#define listen_common _n(listen_common)
#define clear_input_common _n(clear_input_common)
#define make_string_input_stream_common _n(make_string_input_stream_common)
#define make_string_output_stream_common _n(make_string_output_stream_common)
#define get_output_stream_string_common _n(get_output_stream_string_common)
#define with_input_from_string_common _n(with_input_from_string_common)
#define with_output_to_string_common _n(with_output_to_string_common)

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
_g int unread_char_common(Execute ptr, addr pos, addr stream);
_g int write_char_common(Execute ptr, addr pos, addr stream);
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
_g int listen_common(Execute ptr, addr stream, addr *ret);
_g int clear_input_common(Execute ptr, addr stream);
_g int make_string_input_stream_common(addr var, addr rest, addr *ret);
_g int make_string_output_stream_common(Execute ptr, addr rest, addr *ret);
_g int get_output_stream_string_common(Execute ptr, addr var, addr *ret);
_g int with_input_from_string_common(addr form, addr *ret);
_g int with_output_to_string_common(addr form, addr *ret);

#endif

