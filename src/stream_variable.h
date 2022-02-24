#ifndef __STREAM_VARIABLE_HEADER__
#define __STREAM_VARIABLE_HEADER__

#include "define.h"
#include "stream_default.h"
#include "stream_object.h"
#include "typedef.h"
#include "typedef_stream.h"

#define Stream_close _n(Stream_close)
#define Stream_read_byte _n(Stream_read_byte)
#define Stream_unread_byte _n(Stream_unread_byte)
#define Stream_write_byte _n(Stream_write_byte)
#define Stream_read_char _n(Stream_read_char)
#define Stream_read_hang _n(Stream_read_hang)
#define Stream_unread_char _n(Stream_unread_char)
#define Stream_write_char _n(Stream_write_char)
#define Stream_getleft _n(Stream_getleft)
#define Stream_setleft _n(Stream_setleft)
#define Stream_inputp _n(Stream_inputp)
#define Stream_outputp _n(Stream_outputp)
#define Stream_interactivep _n(Stream_interactivep)
#define Stream_characterp _n(Stream_characterp)
#define Stream_binaryp _n(Stream_binaryp)
#define Stream_element_type _n(Stream_element_type)
#define Stream_external_format _n(Stream_external_format)
#define Stream_file_length _n(Stream_file_length)
#define Stream_file_position _n(Stream_file_position)
#define Stream_file_position_start _n(Stream_file_position_start)
#define Stream_file_position_end _n(Stream_file_position_end)
#define Stream_file_position_set _n(Stream_file_position_set)
#define Stream_file_charlen _n(Stream_file_charlen)
#define Stream_file_strlen _n(Stream_file_strlen)
#define Stream_listen _n(Stream_listen)
#define Stream_clear_input _n(Stream_clear_input)
#define Stream_finish_output _n(Stream_finish_output)
#define Stream_force_output _n(Stream_force_output)
#define Stream_clear_output _n(Stream_clear_output)
#define Stream_exitpoint _n(Stream_exitpoint)
#define Stream_termsize _n(Stream_termsize)

#define DefineStreamLet(x,y,z) Stream_##y[StreamType_##x] = y##_##z;
#define DefineStreamSet(x,y) Stream_##y[StreamType_##x] = y##_##x;
#define DefineStreamErr(x,y) Stream_##y[StreamType_##x] = y##_stream_error;
#define DefineStream___(x,y) Stream_##y[StreamType_##x] = y##_stream_error;
#define DefineStreamDef(x,y) Stream_##y[StreamType_##x] = y##_default_stream;
#define DefineStreamEql(x,y,z) Stream_##y[StreamType_##x] = z;
#define DefineStreamChk(x,y,z) Stream_##y[StreamType_##x] = checkp_##z##_stream;

#define PipeStreamArray(x,y) Stream_##y[StreamPipe_Index(StreamPipe_##x)]
#define PipeStreamLet(x,y,z) PipeStreamArray(x, y) = y##_##z;
#define PipeStreamSet(x,y) PipeStreamArray(x, y) = y##_##x;
#define PipeStreamErr(x,y) PipeStreamArray(x, y) = y##_stream_error;
#define PipeStream___(x,y) PipeStreamArray(x, y) = y##_stream_error;
#define PipeStreamDef(x,y) PipeStreamArray(x, y) = y##_default_stream;
#define PipeStreamEql(x,y,z) PipeStreamArray(x, y) = z;
#define PipeStreamChk(x,y,z) PipeStreamArray(x, y) = checkp_##z##_stream;

extern lisp_streamtype_close Stream_close[Stream_Size];
extern lisp_streamtype_read_byte Stream_read_byte[Stream_Size];
extern lisp_streamtype_unread_byte Stream_unread_byte[Stream_Size];
extern lisp_streamtype_write_byte Stream_write_byte[Stream_Size];
extern lisp_streamtype_read_char Stream_read_char[Stream_Size];
extern lisp_streamtype_read_hang Stream_read_hang[Stream_Size];
extern lisp_streamtype_unread_char Stream_unread_char[Stream_Size];
extern lisp_streamtype_write_char Stream_write_char[Stream_Size];
extern lisp_streamtype_getleft Stream_getleft[Stream_Size];
extern lisp_streamtype_setleft Stream_setleft[Stream_Size];
extern lisp_streamtype_inputp Stream_inputp[Stream_Size];
extern lisp_streamtype_outputp Stream_outputp[Stream_Size];
extern lisp_streamtype_interactivep Stream_interactivep[Stream_Size];
extern lisp_streamtype_characterp Stream_characterp[Stream_Size];
extern lisp_streamtype_binaryp Stream_binaryp[Stream_Size];
extern lisp_streamtype_element_type Stream_element_type[Stream_Size];
extern lisp_streamtype_external_format Stream_external_format[Stream_Size];
extern lisp_streamtype_file_length Stream_file_length[Stream_Size];
extern lisp_streamtype_file_position Stream_file_position[Stream_Size];
extern lisp_streamtype_file_position_start Stream_file_position_start[Stream_Size];
extern lisp_streamtype_file_position_end Stream_file_position_end[Stream_Size];
extern lisp_streamtype_file_position_set Stream_file_position_set[Stream_Size];
extern lisp_streamtype_file_charlen Stream_file_charlen[Stream_Size];
extern lisp_streamtype_file_strlen Stream_file_strlen[Stream_Size];
extern lisp_streamtype_listen Stream_listen[Stream_Size];
extern lisp_streamtype_clear_input Stream_clear_input[Stream_Size];
extern lisp_streamtype_finish_output Stream_finish_output[Stream_Size];
extern lisp_streamtype_force_output Stream_force_output[Stream_Size];
extern lisp_streamtype_clear_output Stream_clear_output[Stream_Size];
extern lisp_streamtype_exitpoint Stream_exitpoint[Stream_Size];
extern lisp_streamtype_termsize Stream_termsize[Stream_Size];

#endif

