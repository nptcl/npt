#ifndef __FILE_HEADER__
#define __FILE_HEADER__

#include "execute.h"
#include "file_type.h"
#include "typedef.h"

/*
 *  initialize
 */
_g int init_file(void);
_g void free_file(void);
_g int consolep_file(void);


/*
 *  Common Function
 */
_g void make_standard_input(addr *stream);
_g void make_standard_output(addr *stream);
_g void make_standard_error(addr *stream);
_g void update_standard_input(addr stream);
_g void update_standard_output(addr stream);
_g void update_standard_error(addr stream);
_g int script_header(addr stream);

/* input */
_g int open_input_stream_external(Execute ptr, addr *stream, addr file, addr format);
_g int open_input_stream(Execute ptr, addr *stream, addr file);
_g void open_input_stream_error(Execute ptr, addr *stream, addr file);
_g int open_input_binary_stream(Execute ptr, addr *stream, addr file);
_g int open_input_ascii_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf8_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf8bom_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf16_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf16le_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf16be_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf16lebom_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf16bebom_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf32_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf32le_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf32be_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf32lebom_stream(Execute ptr, addr *stream, addr file);
_g int open_input_utf32bebom_stream(Execute ptr, addr *stream, addr file);

/* output */
_g int open_output_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_output_binary_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_output_ascii_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_output_utf8_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
_g int open_output_utf16le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
_g int open_output_utf16be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
_g int open_output_utf32le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
_g int open_output_utf32be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);

/* io */
_g int open_io_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_binary_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_binary_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_ascii_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf8_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf8bom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf16_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf16le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf16be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf16lebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf16bebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf32_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf32le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf32be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf32lebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
_g int open_io_utf32bebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);


/*
 *  stream function
 */
_g int close_stream_file(addr stream, int abort);
_g int read_binary_file(addr stream, void *pos, size_t size, size_t *ret);
_g int readforce_binary_file(addr stream, void *pos, size_t size, size_t *ret);
_g int read_byte_file(addr stream, byte *c);
_g int unread_byte_file(addr stream, byte c);
_g int write_binary_file(addr stream, const void *pos, size_t size, size_t *ret);
_g int write_byte_file(addr stream, byte c);

_g int read_char_file(addr stream, unicode *c);
_g int read_hang_file(addr stream, unicode *c, int *hang);
_g void write_char_file(addr stream, unicode c);
_g int file_length_file(addr file, size_t *ret);
_g int file_position_file(addr stream, size_t *ret);
_g int file_position_start_file(addr stream);
_g int file_position_end_file(addr stream);
_g int file_position_set_file(addr stream, size_t pos);
_g int file_character_length_file(addr stream, unicode u, size_t *ret);
_g int file_string_length_file(addr stream, addr pos, size_t *ret);
_g void external_format_file(addr stream, addr *ret);
_g int listen_file(addr stream);
_g void clear_input_file(addr stream);
_g void finish_output_file(addr stream);
_g void force_output_file(addr stream);
_g void clear_output_file(addr stream);
_g void exitpoint_file(addr stream);
_g int terminal_width_file(addr stream, size_t *ret);


/*
 *  core
 */
_g int save_stream_file(addr pos);
_g int save_stream_system(addr pos);

#endif

