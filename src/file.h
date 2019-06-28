#ifndef __FILE_HEADER__
#define __FILE_HEADER__

#include "execute.h"
#include "file_type.h"
#include "typedef.h"

/*
 *  initialize
 */
int init_file(void);
void free_file(void);
int consolep_file(void);


/*
 *  Common Function
 */
void make_standard_input(addr *stream);
void make_standard_output(addr *stream);
void make_standard_error(addr *stream);
void update_standard_input(addr stream);
void update_standard_output(addr stream);
void update_standard_error(addr stream);
int script_header(addr stream);

/* input */
int open_input_stream_external(Execute ptr, addr *stream, addr file, addr format);
int open_input_stream(Execute ptr, addr *stream, addr file);
void open_input_stream_error(Execute ptr, addr *stream, addr file);
int open_input_binary_stream(Execute ptr, addr *stream, addr file);
int open_input_ascii_stream(Execute ptr, addr *stream, addr file);
int open_input_utf8_stream(Execute ptr, addr *stream, addr file);
int open_input_utf8bom_stream(Execute ptr, addr *stream, addr file);
int open_input_utf16_stream(Execute ptr, addr *stream, addr file);
int open_input_utf16le_stream(Execute ptr, addr *stream, addr file);
int open_input_utf16be_stream(Execute ptr, addr *stream, addr file);
int open_input_utf16lebom_stream(Execute ptr, addr *stream, addr file);
int open_input_utf16bebom_stream(Execute ptr, addr *stream, addr file);
int open_input_utf32_stream(Execute ptr, addr *stream, addr file);
int open_input_utf32le_stream(Execute ptr, addr *stream, addr file);
int open_input_utf32be_stream(Execute ptr, addr *stream, addr file);
int open_input_utf32lebom_stream(Execute ptr, addr *stream, addr file);
int open_input_utf32bebom_stream(Execute ptr, addr *stream, addr file);

/* output */
int open_output_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_output_binary_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_output_ascii_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_output_utf8_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
int open_output_utf16le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
int open_output_utf16be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
int open_output_utf32le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);
int open_output_utf32be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode, int bomp);

/* io */
int open_io_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_binary_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_binary_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_ascii_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf8_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf8bom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf16_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf16le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf16be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf16lebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf16bebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf32_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf32le_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf32be_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf32lebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);
int open_io_utf32bebom_stream(Execute ptr, addr *stream,
		addr file, enum FileOutput mode);


/*
 *  stream function
 */
int close_stream_file(addr stream, int abort);
int read_binary_file(addr stream, void *pos, size_t size, size_t *ret);
int readforce_binary_file(addr stream, void *pos, size_t size, size_t *ret);
int read_byte_file(addr stream, byte *c);
int unread_byte_file(addr stream, byte c);
int write_binary_file(addr stream, const void *pos, size_t size, size_t *ret);
int write_byte_file(addr stream, byte c);

int read_char_file(addr stream, unicode *c);
int read_hang_file(addr stream, unicode *c, int *hang);
void write_char_file(addr stream, unicode c);
int file_length_file(addr file, size_t *ret);
int file_position_file(addr stream, size_t *ret);
int file_position_start_file(addr stream);
int file_position_end_file(addr stream);
int file_position_set_file(addr stream, size_t pos);
int file_character_length_file(addr stream, unicode u, size_t *ret);
int file_string_length_file(addr stream, addr pos, size_t *ret);
void external_format_file(addr stream, addr *ret);
int listen_file(addr stream);
void clear_input_file(addr stream);
void finish_output_file(addr stream);
void force_output_file(addr stream);
void clear_output_file(addr stream);


/*
 *  core
 */
int save_stream_file(addr pos);
int save_stream_system(addr pos);

#endif

