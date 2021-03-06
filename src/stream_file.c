#include "condition.h"
#include "file.h"
#include "stream_error.h"
#include "stream_file.h"
#include "stream_variable.h"
#include "stream.h"

void init_stream_binary_input(void)
{
	DefineStreamLet(BinaryInput, close, stream_file_);
	DefineStreamLet(BinaryInput, read_byte, file_);
	DefineStreamLet(BinaryInput, unread_byte, file_);
	DefineStream___(BinaryInput, write_byte);
	DefineStream___(BinaryInput, read_char);
	DefineStream___(BinaryInput, read_hang);
	DefineStream___(BinaryInput, unread_char);
	DefineStream___(BinaryInput, write_char);
	DefineStream___(BinaryInput, getleft);
	DefineStream___(BinaryInput, setleft);
	DefineStreamChk(BinaryInput, inputp, true);
	DefineStreamChk(BinaryInput, outputp, false);
	DefineStreamChk(BinaryInput, interactivep, false);
	DefineStreamChk(BinaryInput, characterp, false);
	DefineStreamChk(BinaryInput, binaryp, true);
	DefineStreamLet(BinaryInput, element_type, file_);
	DefineStreamLet(BinaryInput, external_format, file_);
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

void init_stream_binary_output(void)
{
	DefineStreamLet(BinaryOutput, close, stream_file_);
	DefineStream___(BinaryOutput, read_byte);
	DefineStream___(BinaryOutput, unread_byte);
	DefineStreamLet(BinaryOutput, write_byte, file_);
	DefineStream___(BinaryOutput, read_char);
	DefineStream___(BinaryOutput, read_hang);
	DefineStream___(BinaryOutput, unread_char);
	DefineStream___(BinaryOutput, write_char);
	DefineStream___(BinaryOutput, getleft);
	DefineStream___(BinaryOutput, setleft);
	DefineStreamChk(BinaryOutput, inputp, false);
	DefineStreamChk(BinaryOutput, outputp, true);
	DefineStreamChk(BinaryOutput, interactivep, false);
	DefineStreamChk(BinaryOutput, characterp, false);
	DefineStreamChk(BinaryOutput, binaryp, true);
	DefineStreamLet(BinaryOutput, element_type, file_);
	DefineStreamLet(BinaryOutput, external_format, file_);
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

void init_stream_binary_io(void)
{
	DefineStreamLet(BinaryIO, close, stream_file_);
	DefineStreamLet(BinaryIO, read_byte, file_);
	DefineStreamLet(BinaryIO, unread_byte, file_);
	DefineStreamLet(BinaryIO, write_byte, file_);
	DefineStream___(BinaryIO, read_char);
	DefineStream___(BinaryIO, read_hang);
	DefineStream___(BinaryIO, unread_char);
	DefineStream___(BinaryIO, write_char);
	DefineStream___(BinaryIO, getleft);
	DefineStream___(BinaryIO, setleft);
	DefineStreamChk(BinaryIO, inputp, true);
	DefineStreamChk(BinaryIO, outputp, true);
	DefineStreamChk(BinaryIO, interactivep, false);
	DefineStreamChk(BinaryIO, characterp, false);
	DefineStreamChk(BinaryIO, binaryp, true);
	DefineStreamLet(BinaryIO, element_type, file_);
	DefineStreamLet(BinaryIO, external_format, file_);
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

void init_stream_character_input(void)
{
	DefineStreamLet(CharacterInput, close, stream_file_);
	DefineStream___(CharacterInput, read_byte);
	DefineStream___(CharacterInput, unread_byte);
	DefineStream___(CharacterInput, write_byte);
	DefineStreamDef(CharacterInput, read_char);
	DefineStreamDef(CharacterInput, read_hang);
	DefineStreamDef(CharacterInput, unread_char);
	DefineStream___(CharacterInput, write_char);
	DefineStream___(CharacterInput, getleft);
	DefineStream___(CharacterInput, setleft);
	DefineStreamChk(CharacterInput, inputp, true);
	DefineStreamChk(CharacterInput, outputp, false);
	DefineStreamChk(CharacterInput, interactivep, false);
	DefineStreamChk(CharacterInput, characterp, true);
	DefineStreamChk(CharacterInput, binaryp, false);
	DefineStreamLet(CharacterInput, element_type, character_stream);
	DefineStreamLet(CharacterInput, external_format, file_);
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

void init_stream_character_output(void)
{
	DefineStreamLet(CharacterOutput, close, stream_file_);
	DefineStream___(CharacterOutput, read_byte);
	DefineStream___(CharacterOutput, unread_byte);
	DefineStream___(CharacterOutput, write_byte);
	DefineStream___(CharacterOutput, read_char);
	DefineStream___(CharacterOutput, read_hang);
	DefineStream___(CharacterOutput, unread_char);
	DefineStreamDef(CharacterOutput, write_char);
	DefineStreamDef(CharacterOutput, getleft);
	DefineStreamDef(CharacterOutput, setleft);
	DefineStreamChk(CharacterOutput, inputp, false);
	DefineStreamChk(CharacterOutput, outputp, true);
	DefineStreamChk(CharacterOutput, interactivep, false);
	DefineStreamChk(CharacterOutput, characterp, true);
	DefineStreamChk(CharacterOutput, binaryp, false);
	DefineStreamLet(CharacterOutput, element_type, character_stream);
	DefineStreamLet(CharacterOutput, external_format, file_);
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

void init_stream_character_io(void)
{
	DefineStreamLet(CharacterIO, close, stream_file_);
	DefineStream___(CharacterIO, read_byte);
	DefineStream___(CharacterIO, unread_byte);
	DefineStream___(CharacterIO, write_byte);
	DefineStreamDef(CharacterIO, read_char);
	DefineStreamDef(CharacterIO, read_hang);
	DefineStreamDef(CharacterIO, unread_char);
	DefineStreamDef(CharacterIO, write_char);
	DefineStreamDef(CharacterIO, getleft);
	DefineStreamDef(CharacterIO, setleft);
	DefineStreamChk(CharacterIO, inputp, true);
	DefineStreamChk(CharacterIO, outputp, true);
	DefineStreamChk(CharacterIO, interactivep, false);
	DefineStreamChk(CharacterIO, characterp, true);
	DefineStreamChk(CharacterIO, binaryp, false);
	DefineStreamLet(CharacterIO, element_type, character_stream);
	DefineStreamLet(CharacterIO, external_format, file_);
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

void init_stream_binchar_input(void)
{
	DefineStreamLet(BincharInput, close, stream_file_);
	DefineStreamLet(BincharInput, read_byte, file_);
	DefineStreamLet(BincharInput, unread_byte, file_);
	DefineStream___(BincharInput, write_byte);
	DefineStreamDef(BincharInput, read_char);
	DefineStreamDef(BincharInput, read_hang);
	DefineStreamDef(BincharInput, unread_char);
	DefineStream___(BincharInput, write_char);
	DefineStream___(BincharInput, getleft);
	DefineStream___(BincharInput, setleft);
	DefineStreamChk(BincharInput, inputp, true);
	DefineStreamChk(BincharInput, outputp, false);
	DefineStreamChk(BincharInput, interactivep, true);
	DefineStreamChk(BincharInput, characterp, true);
	DefineStreamChk(BincharInput, binaryp, true);
	DefineStreamLet(BincharInput, element_type, character_stream);
	DefineStreamLet(BincharInput, external_format, file_);
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

void init_stream_binchar_output(void)
{
	DefineStreamLet(BincharOutput, close, stream_file_);
	DefineStream___(BincharOutput, read_byte);
	DefineStream___(BincharOutput, unread_byte);
	DefineStreamLet(BincharOutput, write_byte, file_);
	DefineStream___(BincharOutput, read_char);
	DefineStream___(BincharOutput, read_hang);
	DefineStream___(BincharOutput, unread_char);
	DefineStreamDef(BincharOutput, write_char);
	DefineStreamDef(BincharOutput, getleft);
	DefineStreamDef(BincharOutput, setleft);
	DefineStreamChk(BincharOutput, inputp, false);
	DefineStreamChk(BincharOutput, outputp, true);
	DefineStreamChk(BincharOutput, interactivep, true);
	DefineStreamChk(BincharOutput, characterp, true);
	DefineStreamChk(BincharOutput, binaryp, true);
	DefineStreamLet(BincharOutput, element_type, character_stream);
	DefineStreamLet(BincharOutput, external_format, file_);
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

void init_stream_binchar_io(void)
{
	DefineStreamLet(BincharIO, close, stream_file_);
	DefineStreamLet(BincharIO, read_byte, file_);
	DefineStreamLet(BincharIO, unread_byte, file_);
	DefineStreamLet(BincharIO, write_byte, file_);
	DefineStreamDef(BincharIO, read_char);
	DefineStreamDef(BincharIO, read_hang);
	DefineStreamDef(BincharIO, unread_char);
	DefineStreamDef(BincharIO, write_char);
	DefineStreamDef(BincharIO, getleft);
	DefineStreamDef(BincharIO, setleft);
	DefineStreamChk(BincharIO, inputp, true);
	DefineStreamChk(BincharIO, outputp, true);
	DefineStreamChk(BincharIO, interactivep, true);
	DefineStreamChk(BincharIO, characterp, true);
	DefineStreamChk(BincharIO, binaryp, true);
	DefineStreamLet(BincharIO, element_type, character_stream);
	DefineStreamLet(BincharIO, external_format, file_);
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

void init_stream_probe(void)
{
	DefineStreamLet(Probe, close, stream_file_);
	DefineStream___(Probe, read_byte);
	DefineStream___(Probe, unread_byte);
	DefineStream___(Probe, write_byte);
	DefineStream___(Probe, read_char);
	DefineStream___(Probe, read_hang);
	DefineStream___(Probe, unread_char);
	DefineStream___(Probe, write_char);
	DefineStream___(Probe, getleft);
	DefineStream___(Probe, setleft);
	DefineStreamChk(Probe, inputp, false);
	DefineStreamChk(Probe, outputp, false);
	DefineStreamChk(Probe, interactivep, false);
	DefineStreamChk(Probe, characterp, true);
	DefineStreamChk(Probe, binaryp, false);
	DefineStreamLet(Probe, element_type, character_stream);
	DefineStreamLet(Probe, external_format, file_);
	DefineStream___(Probe, file_length);
	DefineStream___(Probe, file_position);
	DefineStream___(Probe, file_position_start);
	DefineStream___(Probe, file_position_end);
	DefineStream___(Probe, file_position_set);
	DefineStream___(Probe, file_charlen);
	DefineStream___(Probe, file_strlen);
	DefineStream___(Probe, listen);
	DefineStream___(Probe, clear_input);
	DefineStream___(Probe, finish_output);
	DefineStream___(Probe, force_output);
	DefineStream___(Probe, clear_output);
	DefineStream___(Probe, exitpoint);
	DefineStream___(Probe, termsize);
}

