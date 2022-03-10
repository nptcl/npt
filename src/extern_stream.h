#ifndef __LISP_EXTERN_STREAM_HEADER__
#define __LISP_EXTERN_STREAM_HEADER__

#include <stddef.h>
#include "typedef_basic.h"
#include "typedef_stream.h"

/* stream object */
void lisp0_stream_define(addr *ret, int index, size_t size);
void lisp_stream_define(addr x, int index, size_t size);
void lisp_stream_memory(addr stream, void **ret);
void lisp0_getinfo_stream(addr *ret, addr stream);
void lisp_getinfo_stream(addr x, addr stream);
void lisp_setinfo_stream(addr stream, addr value);

void lisp_stream_calltype_close(int, lisp_streamtype_close);
void lisp_stream_calltype_read_byte(int, lisp_streamtype_read_byte);
void lisp_stream_calltype_unread_byte(int, lisp_streamtype_unread_byte);
void lisp_stream_calltype_write_byte(int, lisp_streamtype_write_byte);
void lisp_stream_calltype_read_char(int, lisp_streamtype_read_char);
void lisp_stream_calltype_read_hang(int, lisp_streamtype_read_hang);
void lisp_stream_calltype_unread_char(int, lisp_streamtype_unread_char);
void lisp_stream_calltype_write_char(int, lisp_streamtype_write_char);
void lisp_stream_calltype_getleft(int, lisp_streamtype_getleft);
void lisp_stream_calltype_setleft(int, lisp_streamtype_setleft);
void lisp_stream_calltype_inputp(int, lisp_streamtype_inputp);
void lisp_stream_calltype_outputp(int, lisp_streamtype_outputp);
void lisp_stream_calltype_interactivep(int, lisp_streamtype_interactivep);
void lisp_stream_calltype_characterp(int, lisp_streamtype_characterp);
void lisp_stream_calltype_binaryp(int, lisp_streamtype_binaryp);
void lisp_stream_calltype_element_type(int, lisp_streamtype_element_type);
void lisp_stream_calltype_external_format(int, lisp_streamtype_external_format);
void lisp_stream_calltype_file_length(int, lisp_streamtype_file_length);
void lisp_stream_calltype_file_position(int, lisp_streamtype_file_position);
void lisp_stream_calltype_file_position_start(int, lisp_streamtype_file_position_start);
void lisp_stream_calltype_file_position_end(int, lisp_streamtype_file_position_end);
void lisp_stream_calltype_file_position_set(int, lisp_streamtype_file_position_set);
void lisp_stream_calltype_file_charlen(int, lisp_streamtype_file_charlen);
void lisp_stream_calltype_file_strlen(int, lisp_streamtype_file_strlen);
void lisp_stream_calltype_listen(int, lisp_streamtype_listen);
void lisp_stream_calltype_clear_input(int, lisp_streamtype_clear_input);
void lisp_stream_calltype_finish_output(int, lisp_streamtype_finish_output);
void lisp_stream_calltype_force_output(int, lisp_streamtype_force_output);
void lisp_stream_calltype_clear_output(int, lisp_streamtype_clear_output);
void lisp_stream_calltype_exitpoint(int, lisp_streamtype_exitpoint);
void lisp_stream_calltype_termsize(int, lisp_streamtype_termsize);

void lisp_stream_calltype_error_close(int);
void lisp_stream_calltype_error_read_byte(int);
void lisp_stream_calltype_error_unread_byte(int);
void lisp_stream_calltype_error_write_byte(int);
void lisp_stream_calltype_error_read_char(int);
void lisp_stream_calltype_error_read_hang(int);
void lisp_stream_calltype_error_unread_char(int);
void lisp_stream_calltype_error_write_char(int);
void lisp_stream_calltype_error_getleft(int);
void lisp_stream_calltype_error_setleft(int);
void lisp_stream_calltype_error_inputp(int);
void lisp_stream_calltype_error_outputp(int);
void lisp_stream_calltype_error_interactivep(int);
void lisp_stream_calltype_error_characterp(int);
void lisp_stream_calltype_error_binaryp(int);
void lisp_stream_calltype_error_element_type(int);
void lisp_stream_calltype_error_external_format(int);
void lisp_stream_calltype_error_file_length(int);
void lisp_stream_calltype_error_file_position(int);
void lisp_stream_calltype_error_file_position_start(int);
void lisp_stream_calltype_error_file_position_end(int);
void lisp_stream_calltype_error_file_position_set(int);
void lisp_stream_calltype_error_file_charlen(int);
void lisp_stream_calltype_error_file_strlen(int);
void lisp_stream_calltype_error_listen(int);
void lisp_stream_calltype_error_clear_input(int);
void lisp_stream_calltype_error_finish_output(int);
void lisp_stream_calltype_error_force_output(int);
void lisp_stream_calltype_error_clear_output(int);
void lisp_stream_calltype_error_exitpoint(int);
void lisp_stream_calltype_error_termsize(int);

#endif

