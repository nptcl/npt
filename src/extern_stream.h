#ifndef __LISP_EXTERN_STREAM_HEADER__
#define __LISP_EXTERN_STREAM_HEADER__

#include <stddef.h>
#include "stream_variable.h"
#include "typedef_basic.h"
#include "typedef_stream.h"

/* stream object */
void lisp0_stream_define(addr *ret, int index, size_t size);
void lisp_stream_define(addr x, int index, size_t size);
void lisp_stream_memory(addr stream, void **ret);
void lisp0_getinfo_stream(addr *ret, addr stream);
void lisp_getinfo_stream(addr x, addr stream);
void lisp_setinfo_stream(addr stream, addr value);

#define LispStreamExtendDeclare(name) \
	void lisp_stream_calltype_##name(int, lisp_streamtype_##name)
LispStreamExtendDeclare(close);
LispStreamExtendDeclare(read_byte);
LispStreamExtendDeclare(unread_byte);
LispStreamExtendDeclare(write_byte);
LispStreamExtendDeclare(read_char);
LispStreamExtendDeclare(read_hang);
LispStreamExtendDeclare(unread_char);
LispStreamExtendDeclare(write_char);
LispStreamExtendDeclare(getleft);
LispStreamExtendDeclare(setleft);
LispStreamExtendDeclare(inputp);
LispStreamExtendDeclare(outputp);
LispStreamExtendDeclare(interactivep);
LispStreamExtendDeclare(characterp);
LispStreamExtendDeclare(binaryp);
LispStreamExtendDeclare(element_type);
LispStreamExtendDeclare(external_format);
LispStreamExtendDeclare(file_length);
LispStreamExtendDeclare(file_position);
LispStreamExtendDeclare(file_position_start);
LispStreamExtendDeclare(file_position_end);
LispStreamExtendDeclare(file_position_set);
LispStreamExtendDeclare(file_charlen);
LispStreamExtendDeclare(file_strlen);
LispStreamExtendDeclare(listen);
LispStreamExtendDeclare(clear_input);
LispStreamExtendDeclare(finish_output);
LispStreamExtendDeclare(force_output);
LispStreamExtendDeclare(clear_output);
LispStreamExtendDeclare(exitpoint);
LispStreamExtendDeclare(termsize);

#define LispStreamExtendError(name) \
	void lisp_stream_calltype_error_##name(int)
LispStreamExtendError(close);
LispStreamExtendError(read_byte);
LispStreamExtendError(unread_byte);
LispStreamExtendError(write_byte);
LispStreamExtendError(read_char);
LispStreamExtendError(read_hang);
LispStreamExtendError(unread_char);
LispStreamExtendError(write_char);
LispStreamExtendError(getleft);
LispStreamExtendError(setleft);
LispStreamExtendError(inputp);
LispStreamExtendError(outputp);
LispStreamExtendError(interactivep);
LispStreamExtendError(characterp);
LispStreamExtendError(binaryp);
LispStreamExtendError(element_type);
LispStreamExtendError(external_format);
LispStreamExtendError(file_length);
LispStreamExtendError(file_position);
LispStreamExtendError(file_position_start);
LispStreamExtendError(file_position_end);
LispStreamExtendError(file_position_set);
LispStreamExtendError(file_charlen);
LispStreamExtendError(file_strlen);
LispStreamExtendError(listen);
LispStreamExtendError(clear_input);
LispStreamExtendError(finish_output);
LispStreamExtendError(force_output);
LispStreamExtendError(clear_output);
LispStreamExtendError(exitpoint);
LispStreamExtendError(termsize);

#endif

