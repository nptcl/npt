#ifndef __STREAM_VARIABLE_HEADER__
#define __STREAM_VARIABLE_HEADER__

#include "define.h"
#include "stream_default.h"
#include "stream_object.h"
#include "typedef.h"
#include "typedef_stream.h"

#define Stream_close _n(Stream_close)
#define Stream_element_type _n(Stream_element_type)
#define Stream_exitpoint _n(Stream_exitpoint)
#define Stream_file_charlen _n(Stream_file_charlen)
#define Stream_file_length _n(Stream_file_length)
#define Stream_file_position _n(Stream_file_position)
#define Stream_file_position_end _n(Stream_file_position_end)
#define Stream_file_position_set _n(Stream_file_position_set)
#define Stream_file_position_start _n(Stream_file_position_start)
#define Stream_file_strlen _n(Stream_file_strlen)
#define Stream_finish_output _n(Stream_finish_output)
#define Stream_force_output _n(Stream_force_output)
#define Stream_fresh_line _n(Stream_fresh_line)
#define Stream_getleft _n(Stream_getleft)
#define Stream_inputp _n(Stream_inputp)
#define Stream_interactivep _n(Stream_interactivep)
#define Stream_listen _n(Stream_listen)
#define Stream_outputp _n(Stream_outputp)
#define Stream_read_binary _n(Stream_read_binary)
#define Stream_read_byte _n(Stream_read_byte)
#define Stream_read_char _n(Stream_read_char)
#define Stream_read_hang _n(Stream_read_hang)
#define Stream_readf_binary _n(Stream_readf_binary)
#define Stream_setleft _n(Stream_setleft)
#define Stream_termsize _n(Stream_termsize)

#define Stream_binaryp _n(Stream_binaryp)
#define Stream_characterp _n(Stream_characterp)
#define Stream_clear_input _n(Stream_clear_input)
#define Stream_clear_output _n(Stream_clear_output)

#define Stream_terpri _n(Stream_terpri)
#define Stream_unread_byte _n(Stream_unread_byte)
#define Stream_unread_char _n(Stream_unread_char)
#define Stream_write_binary _n(Stream_write_binary)
#define Stream_write_byte _n(Stream_write_byte)
#define Stream_write_char _n(Stream_write_char)

#define DefineStreamLet(x,y,z) Stream_##y[StreamType_##x] = y##_##z;
#define DefineStreamSet(x,y) Stream_##y[StreamType_##x] = y##_##x;
#define DefineStreamErr(x,y) Stream_##y[StreamType_##x] = y##_stream_error;
#define DefineStream___(x,y) Stream_##y[StreamType_##x] = y##_stream_error;
#define DefineStreamDef(x,y) Stream_##y[StreamType_##x] = y##_default_stream;
#define DefineStreamEql(x,y,z) Stream_##y[StreamType_##x] = z;
#define DefineStreamChk(x,y,z) Stream_##y[StreamType_##x] = checkp_##z##_stream;

#define StreamType_Array	(StreamType_Size + LISP_STREAM_EXTEND)
__extern lisp_streamtype_close Stream_close[StreamType_Array];
__extern lisp_streamtype_read_binary Stream_read_binary[StreamType_Array];
__extern lisp_streamtype_readf_binary Stream_readf_binary[StreamType_Array];
__extern lisp_streamtype_read_byte Stream_read_byte[StreamType_Array];
__extern lisp_streamtype_unread_byte Stream_unread_byte[StreamType_Array];
__extern lisp_streamtype_write_binary Stream_write_binary[StreamType_Array];
__extern lisp_streamtype_write_byte Stream_write_byte[StreamType_Array];
__extern lisp_streamtype_read_char Stream_read_char[StreamType_Array];
__extern lisp_streamtype_read_hang Stream_read_hang[StreamType_Array];
__extern lisp_streamtype_unread_char Stream_unread_char[StreamType_Array];
__extern lisp_streamtype_write_char Stream_write_char[StreamType_Array];
__extern lisp_streamtype_terpri Stream_terpri[StreamType_Array];
__extern lisp_streamtype_getleft Stream_getleft[StreamType_Array];
__extern lisp_streamtype_setleft Stream_setleft[StreamType_Array];
__extern lisp_streamtype_fresh_line Stream_fresh_line[StreamType_Array];
__extern lisp_streamtype_clear_input Stream_clear_input[StreamType_Array];
__extern lisp_streamtype_inputp Stream_inputp[StreamType_Array];
__extern lisp_streamtype_outputp Stream_outputp[StreamType_Array];
__extern lisp_streamtype_interactivep Stream_interactivep[StreamType_Array];
__extern lisp_streamtype_characterp Stream_characterp[StreamType_Array];
__extern lisp_streamtype_binaryp Stream_binaryp[StreamType_Array];
__extern lisp_streamtype_element_type Stream_element_type[StreamType_Array];
__extern lisp_streamtype_file_length Stream_file_length[StreamType_Array];
__extern lisp_streamtype_file_position Stream_file_position[StreamType_Array];
__extern lisp_streamtype_file_position_start Stream_file_position_start[StreamType_Array];
__extern lisp_streamtype_file_position_end Stream_file_position_end[StreamType_Array];
__extern lisp_streamtype_file_position_set Stream_file_position_set[StreamType_Array];
__extern lisp_streamtype_file_charlen Stream_file_charlen[StreamType_Array];
__extern lisp_streamtype_file_strlen Stream_file_strlen[StreamType_Array];
__extern lisp_streamtype_listen Stream_listen[StreamType_Array];
__extern lisp_streamtype_finish_output Stream_finish_output[StreamType_Array];
__extern lisp_streamtype_force_output Stream_force_output[StreamType_Array];
__extern lisp_streamtype_clear_output Stream_clear_output[StreamType_Array];
__extern lisp_streamtype_exitpoint Stream_exitpoint[StreamType_Array];
__extern lisp_streamtype_termsize Stream_termsize[StreamType_Array];

#endif

