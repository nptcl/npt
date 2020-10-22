#include "condition.h"
#include "file.h"
#include "stream_error.h"
#include "stream_file.h"
#include "stream_variable.h"
#include "stream.h"

_g void init_stream_binary_input(void)
{
	DefineStreamLet(BinaryInput, close, stream_file_);
	DefineStreamLet(BinaryInput, read_binary, file_);
	DefineStreamLet(BinaryInput, readf_binary, file_);
	DefineStreamLet(BinaryInput, read_byte, file_);
	DefineStreamLet(BinaryInput, unread_byte, file_);
	DefineStream___(BinaryInput, write_binary);
	DefineStream___(BinaryInput, write_byte);
	DefineStream___(BinaryInput, read_char);
	DefineStream___(BinaryInput, read_hang);
	DefineStream___(BinaryInput, unread_char);
	DefineStream___(BinaryInput, write_char);
	DefineStream___(BinaryInput, terpri);
	DefineStream___(BinaryInput, getleft);
	DefineStream___(BinaryInput, setleft);
	DefineStream___(BinaryInput, fresh_line);
	DefineStreamChk(BinaryInput, inputp, true);
	DefineStreamChk(BinaryInput, outputp, false);
	DefineStreamChk(BinaryInput, interactivep, false);
	DefineStreamChk(BinaryInput, characterp, false);
	DefineStreamChk(BinaryInput, binaryp, true);
	DefineStreamLet(BinaryInput, element_type, binary_stream);
	DefineStreamDef(BinaryInput, file_length);
	DefineStreamLet(BinaryInput, file_position, file_);
	DefineStreamLet(BinaryInput, file_position_start, file_);
	DefineStreamLet(BinaryInput, file_position_end, file_);
	DefineStreamLet(BinaryInput, file_position_set, file_);
	DefineStream___(BinaryInput, file_charlen);
	DefineStream___(BinaryInput, file_strlen);
	DefineStreamLet(BinaryInput, listen, file_);
	DefineStreamLet(BinaryInput, clear_input, file_);
	DefineStream___(BinaryInput, finish_output);
	DefineStream___(BinaryInput, force_output);
	DefineStream___(BinaryInput, clear_output);
	DefineStreamDef(BinaryInput, exitpoint);
	DefineStream___(BinaryInput, termsize);
}

_g void init_stream_binary_output(void)
{
	DefineStreamLet(BinaryOutput, close, stream_file_);
	DefineStream___(BinaryOutput, read_binary);
	DefineStream___(BinaryOutput, readf_binary);
	DefineStream___(BinaryOutput, read_byte);
	DefineStream___(BinaryOutput, unread_byte);
	DefineStreamLet(BinaryOutput, write_binary, file_);
	DefineStreamLet(BinaryOutput, write_byte, file_);
	DefineStream___(BinaryOutput, read_char);
	DefineStream___(BinaryOutput, read_hang);
	DefineStream___(BinaryOutput, unread_char);
	DefineStream___(BinaryOutput, write_char);
	DefineStream___(BinaryOutput, terpri);
	DefineStream___(BinaryOutput, getleft);
	DefineStream___(BinaryOutput, setleft);
	DefineStream___(BinaryOutput, fresh_line);
	DefineStreamChk(BinaryOutput, inputp, false);
	DefineStreamChk(BinaryOutput, outputp, true);
	DefineStreamChk(BinaryOutput, interactivep, false);
	DefineStreamChk(BinaryOutput, characterp, false);
	DefineStreamChk(BinaryOutput, binaryp, true);
	DefineStreamLet(BinaryOutput, element_type, binary_stream);
	DefineStreamDef(BinaryOutput, file_length);
	DefineStreamLet(BinaryOutput, file_position, file_);
	DefineStreamLet(BinaryOutput, file_position_start, file_);
	DefineStreamLet(BinaryOutput, file_position_end, file_);
	DefineStreamLet(BinaryOutput, file_position_set, file_);
	DefineStreamLet(BinaryOutput, file_charlen, file_);
	DefineStreamLet(BinaryOutput, file_strlen, file_);
	DefineStream___(BinaryOutput, listen);
	DefineStream___(BinaryOutput, clear_input);
	DefineStreamLet(BinaryOutput, finish_output, file_);
	DefineStreamLet(BinaryOutput, force_output, file_);
	DefineStreamLet(BinaryOutput, clear_output, file_);
	DefineStreamDef(BinaryOutput, exitpoint);
	DefineStreamDef(BinaryOutput, termsize);
}

_g void init_stream_binary_io(void)
{
	DefineStreamLet(BinaryIO, close, stream_file_);
	DefineStreamLet(BinaryIO, read_binary, file_);
	DefineStreamLet(BinaryIO, readf_binary, file_);
	DefineStreamLet(BinaryIO, read_byte, file_);
	DefineStreamLet(BinaryIO, unread_byte, file_);
	DefineStreamLet(BinaryIO, write_binary, file_);
	DefineStreamLet(BinaryIO, write_byte, file_);
	DefineStream___(BinaryIO, read_char);
	DefineStream___(BinaryIO, read_hang);
	DefineStream___(BinaryIO, unread_char);
	DefineStream___(BinaryIO, write_char);
	DefineStream___(BinaryIO, terpri);
	DefineStream___(BinaryIO, getleft);
	DefineStream___(BinaryIO, setleft);
	DefineStream___(BinaryIO, fresh_line);
	DefineStreamChk(BinaryIO, inputp, true);
	DefineStreamChk(BinaryIO, outputp, true);
	DefineStreamChk(BinaryIO, interactivep, false);
	DefineStreamChk(BinaryIO, characterp, false);
	DefineStreamChk(BinaryIO, binaryp, true);
	DefineStreamLet(BinaryIO, element_type, binary_stream);
	DefineStreamDef(BinaryIO, file_length);
	DefineStreamLet(BinaryIO, file_position, file_);
	DefineStreamLet(BinaryIO, file_position_start, file_);
	DefineStreamLet(BinaryIO, file_position_end, file_);
	DefineStreamLet(BinaryIO, file_position_set, file_);
	DefineStreamLet(BinaryIO, file_charlen, file_);
	DefineStreamLet(BinaryIO, file_strlen, file_);
	DefineStreamLet(BinaryIO, listen, file_);
	DefineStreamLet(BinaryIO, clear_input, file_);
	DefineStreamLet(BinaryIO, finish_output, file_);
	DefineStreamLet(BinaryIO, force_output, file_);
	DefineStreamLet(BinaryIO, clear_output, file_);
	DefineStreamDef(BinaryIO, exitpoint);
	DefineStreamDef(BinaryIO, termsize);
}

_g void init_stream_character_input(void)
{
	DefineStreamLet(CharacterInput, close, stream_file_);
	DefineStream___(CharacterInput, read_binary);
	DefineStream___(CharacterInput, readf_binary);
	DefineStream___(CharacterInput, read_byte);
	DefineStream___(CharacterInput, unread_byte);
	DefineStream___(CharacterInput, write_binary);
	DefineStream___(CharacterInput, write_byte);
	DefineStreamDef(CharacterInput, read_char);
	DefineStreamDef(CharacterInput, read_hang);
	DefineStreamDef(CharacterInput, unread_char);
	DefineStream___(CharacterInput, write_char);
	DefineStream___(CharacterInput, terpri);
	DefineStream___(CharacterInput, getleft);
	DefineStream___(CharacterInput, setleft);
	DefineStream___(CharacterInput, fresh_line);
	DefineStreamChk(CharacterInput, inputp, true);
	DefineStreamChk(CharacterInput, outputp, false);
	DefineStreamChk(CharacterInput, interactivep, false);
	DefineStreamChk(CharacterInput, characterp, true);
	DefineStreamChk(CharacterInput, binaryp, false);
	DefineStreamLet(CharacterInput, element_type, character_stream);
	DefineStreamDef(CharacterInput, file_length);
	DefineStreamLet(CharacterInput, file_position, file_);
	DefineStreamLet(CharacterInput, file_position_start, file_);
	DefineStreamLet(CharacterInput, file_position_end, file_);
	DefineStreamLet(CharacterInput, file_position_set, file_);
	DefineStream___(CharacterInput, file_charlen);
	DefineStream___(CharacterInput, file_strlen);
	DefineStreamLet(CharacterInput, listen, file_);
	DefineStreamLet(CharacterInput, clear_input, file_);
	DefineStream___(CharacterInput, finish_output);
	DefineStream___(CharacterInput, force_output);
	DefineStream___(CharacterInput, clear_output);
	DefineStreamDef(CharacterInput, exitpoint);
	DefineStream___(CharacterInput, termsize);
}

_g void init_stream_character_output(void)
{
	DefineStreamLet(CharacterOutput, close, stream_file_);
	DefineStream___(CharacterOutput, read_binary);
	DefineStream___(CharacterOutput, readf_binary);
	DefineStream___(CharacterOutput, read_byte);
	DefineStream___(CharacterOutput, unread_byte);
	DefineStream___(CharacterOutput, write_binary);
	DefineStream___(CharacterOutput, write_byte);
	DefineStream___(CharacterOutput, read_char);
	DefineStream___(CharacterOutput, read_hang);
	DefineStream___(CharacterOutput, unread_char);
	DefineStreamDef(CharacterOutput, write_char);
	DefineStreamDef(CharacterOutput, terpri);
	DefineStreamDef(CharacterOutput, getleft);
	DefineStreamDef(CharacterOutput, setleft);
	DefineStreamDef(CharacterOutput, fresh_line);
	DefineStreamChk(CharacterOutput, inputp, false);
	DefineStreamChk(CharacterOutput, outputp, true);
	DefineStreamChk(CharacterOutput, interactivep, false);
	DefineStreamChk(CharacterOutput, characterp, true);
	DefineStreamChk(CharacterOutput, binaryp, false);
	DefineStreamLet(CharacterOutput, element_type, character_stream);
	DefineStreamDef(CharacterOutput, file_length);
	DefineStreamLet(CharacterOutput, file_position, file_);
	DefineStreamLet(CharacterOutput, file_position_start, file_);
	DefineStreamLet(CharacterOutput, file_position_end, file_);
	DefineStreamLet(CharacterOutput, file_position_set, file_);
	DefineStreamLet(CharacterOutput, file_charlen, file_);
	DefineStreamLet(CharacterOutput, file_strlen, file_);
	DefineStream___(CharacterOutput, listen);
	DefineStream___(CharacterOutput, clear_input);
	DefineStreamLet(CharacterOutput, finish_output, file_);
	DefineStreamLet(CharacterOutput, force_output, file_);
	DefineStreamLet(CharacterOutput, clear_output, file_);
	DefineStreamDef(CharacterOutput, exitpoint);
	DefineStreamDef(CharacterOutput, termsize);
}

_g void init_stream_character_io(void)
{
	DefineStreamLet(CharacterIO, close, stream_file_);
	DefineStream___(CharacterIO, read_binary);
	DefineStream___(CharacterIO, readf_binary);
	DefineStream___(CharacterIO, read_byte);
	DefineStream___(CharacterIO, unread_byte);
	DefineStream___(CharacterIO, write_binary);
	DefineStream___(CharacterIO, write_byte);
	DefineStreamDef(CharacterIO, read_char);
	DefineStreamDef(CharacterIO, read_hang);
	DefineStreamDef(CharacterIO, unread_char);
	DefineStreamDef(CharacterIO, write_char);
	DefineStreamDef(CharacterIO, terpri);
	DefineStreamDef(CharacterIO, getleft);
	DefineStreamDef(CharacterIO, setleft);
	DefineStreamDef(CharacterIO, fresh_line);
	DefineStreamChk(CharacterIO, inputp, true);
	DefineStreamChk(CharacterIO, outputp, true);
	DefineStreamChk(CharacterIO, interactivep, false);
	DefineStreamChk(CharacterIO, characterp, true);
	DefineStreamChk(CharacterIO, binaryp, false);
	DefineStreamLet(CharacterIO, element_type, character_stream);
	DefineStreamDef(CharacterIO, file_length);
	DefineStreamLet(CharacterIO, file_position, file_);
	DefineStreamLet(CharacterIO, file_position_start, file_);
	DefineStreamLet(CharacterIO, file_position_end, file_);
	DefineStreamLet(CharacterIO, file_position_set, file_);
	DefineStreamLet(CharacterIO, file_charlen, file_);
	DefineStreamLet(CharacterIO, file_strlen, file_);
	DefineStreamLet(CharacterIO, listen, file_);
	DefineStreamLet(CharacterIO, clear_input, file_);
	DefineStreamLet(CharacterIO, finish_output, file_);
	DefineStreamLet(CharacterIO, force_output, file_);
	DefineStreamLet(CharacterIO, clear_output, file_);
	DefineStreamDef(CharacterIO, exitpoint);
	DefineStreamDef(CharacterIO, termsize);
}

_g void init_stream_binchar_input(void)
{
	DefineStreamLet(BincharInput, close, stream_file_);
	DefineStreamLet(BincharInput, read_binary, file_);
	DefineStreamLet(BincharInput, readf_binary, file_);
	DefineStreamLet(BincharInput, read_byte, file_);
	DefineStreamLet(BincharInput, unread_byte, file_);
	DefineStream___(BincharInput, write_binary);
	DefineStream___(BincharInput, write_byte);
	DefineStreamDef(BincharInput, read_char);
	DefineStreamDef(BincharInput, read_hang);
	DefineStreamDef(BincharInput, unread_char);
	DefineStream___(BincharInput, write_char);
	DefineStream___(BincharInput, terpri);
	DefineStream___(BincharInput, getleft);
	DefineStream___(BincharInput, setleft);
	DefineStream___(BincharInput, fresh_line);
	DefineStreamChk(BincharInput, inputp, true);
	DefineStreamChk(BincharInput, outputp, false);
	DefineStreamChk(BincharInput, interactivep, true);
	DefineStreamChk(BincharInput, characterp, true);
	DefineStreamChk(BincharInput, binaryp, true);
	DefineStreamLet(BincharInput, element_type, character_stream);
	DefineStream___(BincharInput, file_length);
	DefineStreamLet(BincharInput, file_position, file_);
	DefineStreamLet(BincharInput, file_position_start, file_);
	DefineStreamLet(BincharInput, file_position_end, file_);
	DefineStreamLet(BincharInput, file_position_set, file_);
	DefineStream___(BincharInput, file_charlen);
	DefineStream___(BincharInput, file_strlen);
	DefineStreamLet(BincharInput, listen, file_);
	DefineStreamLet(BincharInput, clear_input, file_);
	DefineStream___(BincharInput, finish_output);
	DefineStream___(BincharInput, force_output);
	DefineStream___(BincharInput, clear_output);
	DefineStreamLet(BincharInput, exitpoint, file_);
	DefineStream___(BincharInput, termsize);
}

_g void init_stream_binchar_output(void)
{
	DefineStreamLet(BincharOutput, close, stream_file_);
	DefineStream___(BincharOutput, read_binary);
	DefineStream___(BincharOutput, readf_binary);
	DefineStream___(BincharOutput, read_byte);
	DefineStream___(BincharOutput, unread_byte);
	DefineStreamLet(BincharOutput, write_binary, file_);
	DefineStreamLet(BincharOutput, write_byte, file_);
	DefineStream___(BincharOutput, read_char);
	DefineStream___(BincharOutput, read_hang);
	DefineStream___(BincharOutput, unread_char);
	DefineStreamDef(BincharOutput, write_char);
	DefineStreamDef(BincharOutput, terpri);
	DefineStreamDef(BincharOutput, getleft);
	DefineStreamDef(BincharOutput, setleft);
	DefineStreamDef(BincharOutput, fresh_line);
	DefineStreamChk(BincharOutput, inputp, false);
	DefineStreamChk(BincharOutput, outputp, true);
	DefineStreamChk(BincharOutput, interactivep, true);
	DefineStreamChk(BincharOutput, characterp, true);
	DefineStreamChk(BincharOutput, binaryp, true);
	DefineStreamLet(BincharOutput, element_type, character_stream);
	DefineStream___(BincharOutput, file_length);
	DefineStreamLet(BincharOutput, file_position, file_);
	DefineStreamLet(BincharOutput, file_position_start, file_);
	DefineStreamLet(BincharOutput, file_position_end, file_);
	DefineStreamLet(BincharOutput, file_position_set, file_);
	DefineStreamLet(BincharOutput, file_charlen, file_);
	DefineStreamLet(BincharOutput, file_strlen, file_);
	DefineStream___(BincharOutput, listen);
	DefineStream___(BincharOutput, clear_input);
	DefineStreamLet(BincharOutput, finish_output, file_);
	DefineStreamLet(BincharOutput, force_output, file_);
	DefineStreamLet(BincharOutput, clear_output, file_);
	DefineStreamLet(BincharOutput, exitpoint, file_);
	DefineStreamLet(BincharOutput, termsize, file_);
}

_g void init_stream_binchar_io(void)
{
	DefineStreamLet(BincharIO, close, stream_file_);
	DefineStreamLet(BincharIO, read_binary, file_);
	DefineStreamLet(BincharIO, readf_binary, file_);
	DefineStreamLet(BincharIO, read_byte, file_);
	DefineStreamLet(BincharIO, unread_byte, file_);
	DefineStreamLet(BincharIO, write_binary, file_);
	DefineStreamLet(BincharIO, write_byte, file_);
	DefineStreamDef(BincharIO, read_char);
	DefineStreamDef(BincharIO, read_hang);
	DefineStreamDef(BincharIO, unread_char);
	DefineStreamDef(BincharIO, write_char);
	DefineStreamDef(BincharIO, terpri);
	DefineStreamDef(BincharIO, getleft);
	DefineStreamDef(BincharIO, setleft);
	DefineStreamDef(BincharIO, fresh_line);
	DefineStreamChk(BincharIO, inputp, true);
	DefineStreamChk(BincharIO, outputp, true);
	DefineStreamChk(BincharIO, interactivep, true);
	DefineStreamChk(BincharIO, characterp, true);
	DefineStreamChk(BincharIO, binaryp, true);
	DefineStreamLet(BincharIO, element_type, character_stream);
	DefineStream___(BincharIO, file_length);
	DefineStreamLet(BincharIO, file_position, file_);
	DefineStreamLet(BincharIO, file_position_start, file_);
	DefineStreamLet(BincharIO, file_position_end, file_);
	DefineStreamLet(BincharIO, file_position_set, file_);
	DefineStreamLet(BincharIO, file_charlen, file_);
	DefineStreamLet(BincharIO, file_strlen, file_);
	DefineStreamLet(BincharIO, listen, file_);
	DefineStreamLet(BincharIO, clear_input, file_);
	DefineStreamLet(BincharIO, finish_output, file_);
	DefineStreamLet(BincharIO, force_output, file_);
	DefineStreamLet(BincharIO, clear_output, file_);
	DefineStreamLet(BincharIO, exitpoint, file_);
	DefineStreamLet(BincharIO, termsize, file_);
}

