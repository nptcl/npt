#ifndef __FILE_HEADER__
#define __FILE_HEADER__

#include "execute.h"
#include "file_type.h"
#include "stream.h"
#include "typedef.h"

#define init_file _n(init_file)
#define free_file _n(free_file)
#define consolep_file _n(consolep_file)
#define make_standard_input _n(make_standard_input)
#define make_standard_output _n(make_standard_output)
#define make_standard_error _n(make_standard_error)
#define update_standard_input _n(update_standard_input)
#define update_standard_output _n(update_standard_output)
#define update_standard_error _n(update_standard_error)
#define script_header _n(script_header)
#define force_close_stream_file _n(force_close_stream_file)
#define close_stream_file_ _n(close_stream_file_)
#define read_binary_file_ _n(read_binary_file_)
#define read_byte_file_type _n(read_byte_file_type)
#define read_byte_file_ _n(read_byte_file_)
#define unread_byte_file_ _n(unread_byte_file_)
#define write_binary_file_ _n(write_binary_file_)
#define write_byte_file_type_ _n(write_byte_file_type_)
#define write_byte_file_ _n(write_byte_file_)
#define read_char_file_ _n(read_char_file_)
#define read_hang_file_ _n(read_hang_file_)
#define write_char_file_ _n(write_char_file_)
#define element_type_file_ _n(element_type_file_)
#define external_format_file_ _n(external_format_file_)
#define file_length_file_type_ _n(file_length_file_type_)
#define file_length_file_ _n(file_length_file_)
#define file_position_file_type_ _n(file_position_file_type_)
#define file_position_file_ _n(file_position_file_)
#define file_position_start_file_type_ _n(file_position_start_file_type_)
#define file_position_start_file_ _n(file_position_start_file_)
#define file_position_end_file_type_ _n(file_position_end_file_type_)
#define file_position_end_file_ _n(file_position_end_file_)
#define file_position_set_file_type_ _n(file_position_set_file_type_)
#define file_position_set_file_ _n(file_position_set_file_)
#define file_charlen_file_ _n(file_charlen_file_)
#define file_strlen_file_ _n(file_strlen_file_)
#define listen_file_ _n(listen_file_)
#define clear_input_file_ _n(clear_input_file_)
#define finish_output_file_ _n(finish_output_file_)
#define force_output_file_ _n(force_output_file_)
#define clear_output_file_ _n(clear_output_file_)
#define exitpoint_file_ _n(exitpoint_file_)
#define termsize_file_ _n(termsize_file_)
#define save_stream_file _n(save_stream_file)
#define save_stream_system _n(save_stream_system)

#define CheckFileStream(stream) Check(! file_stream_p(stream), "type error")
#define PtrFileMemory(stream) ((filestream)PtrDataStream(stream))

/*
 *  initialize
 */
int init_file(void);
void free_file(void);
int consolep_file(void);


/*
 *  Common Function
 */
int make_standard_input(addr *stream);
int make_standard_output(addr *stream);
int make_standard_error(addr *stream);
int update_standard_input(addr stream);
int update_standard_output(addr stream);
int update_standard_error(addr stream);
int script_header(addr stream);


/*
 *  stream function
 */
void force_close_stream_file(addr stream);
int close_stream_file_(addr stream, addr *ret);
int read_binary_file_(addr stream, void *pos, size_t size, size_t *ret);
int read_byte_file_type(addr stream, addr *ret);
int read_byte_file_(addr stream, addr *value, int *ret);
int unread_byte_file_(addr stream, byte c);
int write_binary_file_(addr stream, const void *pos, size_t size, size_t *ret);
int write_byte_file_type_(filestream fm, addr pos);
int write_byte_file_(addr stream, addr pos);
int read_char_file_(addr stream, unicode *c, int *ret);
int read_hang_file_(addr stream, unicode *c, int *hang, int *ret);
int write_char_file_(addr stream, unicode c);
int element_type_file_(addr stream, addr *ret);
int external_format_file_(addr stream, addr *ret);
int file_length_file_type_(filestream fm, size_t *value, int *ret);
int file_length_file_(addr file, size_t *value, int *ret);
int file_position_file_type_(addr stream, size_t *value, int *ret);
int file_position_file_(addr stream, size_t *value, int *ret);
int file_position_start_file_type_(addr stream, int *ret);
int file_position_start_file_(addr stream, int *ret);
int file_position_end_file_type_(addr stream, int *ret);
int file_position_end_file_(addr stream, int *ret);
int file_position_set_file_type_(addr stream, size_t value, int *ret);
int file_position_set_file_(addr stream, size_t value, int *ret);
int file_charlen_file_(addr stream, unicode u, size_t *value, int *ret);
int file_strlen_file_(addr stream, addr pos, size_t *value, int *ret);
int listen_file_(addr stream, int *ret);
int clear_input_file_(addr stream);
int finish_output_file_(addr stream);
int force_output_file_(addr stream);
int clear_output_file_(addr stream);
int exitpoint_file_(addr stream);
int termsize_file_(addr stream, size_t *value, int *ret);


/*
 *  core
 */
int save_stream_file(addr pos);
int save_stream_system(addr pos);

#endif

