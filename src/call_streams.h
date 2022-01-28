#ifndef __CALL_STREAMS_HEADER__
#define __CALL_STREAMS_HEADER__

#include "execute.h"
#include "typedef.h"

#define read_byte_common_ _n(read_byte_common_)
#define write_byte_common_ _n(write_byte_common_)
#define peek_char_common_ _n(peek_char_common_)
#define read_char_common_ _n(read_char_common_)
#define read_char_no_hang_common_ _n(read_char_no_hang_common_)
#define terpri_common_ _n(terpri_common_)
#define fresh_line_common_ _n(fresh_line_common_)
#define unread_char_common_ _n(unread_char_common_)
#define write_char_common_ _n(write_char_common_)
#define read_line_common_ _n(read_line_common_)
#define write_string_common_ _n(write_string_common_)
#define write_line_common_ _n(write_line_common_)
#define read_sequence_common_ _n(read_sequence_common_)
#define write_sequence_common_ _n(write_sequence_common_)
#define file_position_common_ _n(file_position_common_)
#define file_string_length_common_ _n(file_string_length_common_)
#define open_common_ _n(open_common_)
#define with_open_file_common_ _n(with_open_file_common_)
#define close_common_ _n(close_common_)
#define with_open_stream_common_ _n(with_open_stream_common_)
#define listen_common_ _n(listen_common_)
#define clear_input_common_ _n(clear_input_common_)
#define finish_output_common_ _n(finish_output_common_)
#define force_output_common_ _n(force_output_common_)
#define clear_output_common_ _n(clear_output_common_)
#define make_string_input_stream_common_ _n(make_string_input_stream_common_)
#define make_string_output_stream_common_ _n(make_string_output_stream_common_)
#define get_output_stream_string_common_ _n(get_output_stream_string_common_)
#define with_input_from_string_common_ _n(with_input_from_string_common_)
#define with_output_to_string_common_ _n(with_output_to_string_common_)

int read_byte_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr *ret);
int write_byte_common_(Execute ptr, addr value, addr stream);
int peek_char_common_(Execute ptr, addr type, addr stream,
		addr errorp, addr value, addr recp, addr *ret);
int read_char_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr recp, addr *ret);
int read_char_no_hang_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr recp, addr *ret);
int terpri_common_(Execute ptr, addr stream);
int fresh_line_common_(Execute ptr, addr stream, addr *ret);
int unread_char_common_(Execute ptr, addr pos, addr stream);
int write_char_common_(Execute ptr, addr pos, addr stream);
int read_line_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr recp,
		addr *ret, addr *sec);
int write_string_common_(Execute ptr, addr string, addr rest);
int write_line_common_(Execute ptr, addr string, addr rest);
int read_sequence_common_(addr var, addr stream, addr rest, addr *ret);
int write_sequence_common_(LocalRoot local, addr var, addr stream, addr rest);
int file_position_common_(Execute ptr, addr stream, addr pos, addr *ret);
int file_string_length_common_(addr stream, addr pos, addr *ret);
int open_common_(Execute ptr, addr pos, addr rest, addr *ret);
int with_open_file_common_(addr form, addr *ret);
int close_common_(Execute ptr, addr pos, addr rest, addr *ret);
int with_open_stream_common_(addr form, addr *ret);
int listen_common_(Execute ptr, addr stream, addr *ret);
int clear_input_common_(Execute ptr, addr stream);
int finish_output_common_(Execute ptr, addr stream);
int force_output_common_(Execute ptr, addr stream);
int clear_output_common_(Execute ptr, addr stream);
int make_string_input_stream_common_(addr var, addr x, addr y, addr *ret);
int make_string_output_stream_common_(Execute ptr, addr rest, addr *ret);
int get_output_stream_string_common_(Execute ptr, addr var, addr *ret);
int with_input_from_string_common_(addr form, addr *ret);
int with_output_to_string_common_(addr form, addr *ret);

#endif

