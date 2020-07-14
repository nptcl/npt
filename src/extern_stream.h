#ifndef __LISP_EXTERN_STREAM_HEADER__
#define __LISP_EXTERN_STREAM_HEADER__

#include <stddef.h>
#include "typedef_basic.h"
#include "typedef_stream.h"

int lisp_stream_define_(addr *ret, int type, size_t size);
int lisp_stream_memory_(addr stream, void **ret);
int lisp_getinfo_stream_(addr stream, addr *ret);
int lisp_setinfo_stream_(addr stream, addr value);

#define LispStreamExtendDeclare(name) \
	int lisp_stream_calltype_##name##_(int, lisp_streamtype_##name)
LispStreamExtendDeclare(close);
LispStreamExtendDeclare(read_binary);
LispStreamExtendDeclare(readforce_binary);
LispStreamExtendDeclare(read_byte);
LispStreamExtendDeclare(unread_byte);
LispStreamExtendDeclare(write_binary);
LispStreamExtendDeclare(write_byte);
LispStreamExtendDeclare(read_char);
LispStreamExtendDeclare(read_hang);
LispStreamExtendDeclare(unread_char);
LispStreamExtendDeclare(write_char);
LispStreamExtendDeclare(terpri);
LispStreamExtendDeclare(getleft);
LispStreamExtendDeclare(setleft);
LispStreamExtendDeclare(fresh_line);
LispStreamExtendDeclare(clear_input);
LispStreamExtendDeclare(inputp);
LispStreamExtendDeclare(outputp);
LispStreamExtendDeclare(interactivep);
LispStreamExtendDeclare(characterp);
LispStreamExtendDeclare(binaryp);
LispStreamExtendDeclare(element_type);
LispStreamExtendDeclare(file_length);
LispStreamExtendDeclare(file_position);
LispStreamExtendDeclare(file_position_start);
LispStreamExtendDeclare(file_position_end);
LispStreamExtendDeclare(file_position_set);
LispStreamExtendDeclare(file_character_length);
LispStreamExtendDeclare(file_string_length);
LispStreamExtendDeclare(listen);
LispStreamExtendDeclare(finish_output);
LispStreamExtendDeclare(force_output);
LispStreamExtendDeclare(clear_output);
LispStreamExtendDeclare(exitpoint);
LispStreamExtendDeclare(terminal_width);

#define LispStreamExtendError(name) \
	int lisp_stream_calltype_error_##name##_(int)
LispStreamExtendError(close);
LispStreamExtendError(read_binary);
LispStreamExtendError(readforce_binary);
LispStreamExtendError(read_byte);
LispStreamExtendError(unread_byte);
LispStreamExtendError(write_binary);
LispStreamExtendError(write_byte);
LispStreamExtendError(read_char);
LispStreamExtendError(read_hang);
LispStreamExtendError(unread_char);
LispStreamExtendError(write_char);
LispStreamExtendError(terpri);
LispStreamExtendError(getleft);
LispStreamExtendError(setleft);
LispStreamExtendError(fresh_line);
LispStreamExtendError(clear_input);
LispStreamExtendError(inputp);
LispStreamExtendError(outputp);
LispStreamExtendError(interactivep);
LispStreamExtendError(characterp);
LispStreamExtendError(binaryp);
LispStreamExtendError(element_type);
LispStreamExtendError(file_length);
LispStreamExtendError(file_position);
LispStreamExtendError(file_position_start);
LispStreamExtendError(file_position_end);
LispStreamExtendError(file_position_set);
LispStreamExtendError(file_character_length);
LispStreamExtendError(file_string_length);
LispStreamExtendError(listen);
LispStreamExtendError(finish_output);
LispStreamExtendError(force_output);
LispStreamExtendError(clear_output);
LispStreamExtendError(exitpoint);
LispStreamExtendError(terminal_width);

#endif

