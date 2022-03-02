#include "condition.h"
#include "constant.h"
#include "file.h"
#include "stream_default.h"
#include "stream_error.h"
#include "stream_object.h"
#include "stream_pipe.h"
#include "stream_variable.h"
#include "symbol.h"
#include "terme_output.h"
#include "typedef.h"
#include "windows_output.h"
#include "windows_screen.h"
#include "windows_stream.h"
#include "windows_window.h"
#include "windows_write.h"

/*
 *  build
 */
static void windows_stream_build_input()
{
	addr symbol, pos;

	GetConst(SPECIAL_STANDARD_INPUT, &symbol);
	open_pipe_stream(&pos, StreamPipe_Input);
	set_name_pipe_stream(pos, "INPUT");
	SetValueSymbol(symbol, pos);
}

static void windows_stream_build_output()
{
	addr symbol, pos;

	GetConst(SPECIAL_STANDARD_OUTPUT, &symbol);
	open_pipe_stream(&pos, StreamPipe_Output);
	set_name_pipe_stream(pos, "OUTPUT");
	SetValueSymbol(symbol, pos);
}

static void windows_stream_build_error()
{
	addr symbol, pos;

	GetConst(SPECIAL_ERROR_OUTPUT, &symbol);
	open_pipe_stream(&pos, StreamPipe_Error);
	set_name_pipe_stream(pos, "ERROR");
	SetValueSymbol(symbol, pos);
}

void windows_stream_build(void)
{
	windows_stream_build_input();
	windows_stream_build_output();
	windows_stream_build_error();
}


/*
 *  pipe-stream
 */
static int check_pipe_stream_(addr stream)
{
	if (! pipe_stream_p(stream))
		return fmte_("Stream ~S must be a pipe-stream.", stream, NULL);
	return 0;
}

static int file_charlen_windows_(addr stream, unicode u, size_t *value, int *ret)
{
	*value = 0;
	return Result(ret, 1);
}

static int file_strlen_windows_(addr stream, addr pos, size_t *value, int *ret)
{
	*value = 0;
	return Result(ret, 1);
}

static int getleft_windows_(addr stream, size_t *ret)
{
	*ret = Window_CursorX;
	return 0;
}

static int setleft_windows_(addr stream, size_t value)
{
	unsigned x;
	x = (unsigned)value;
	return windows_write_setleft_lock(x);
}


/*
 *  pipe-stream input
 */
static void windows_stream_init_input(void)
{
	PipeStreamDef(Input, close);
	PipeStream___(Input, read_byte);
	PipeStream___(Input, unread_byte);
	PipeStream___(Input, write_byte);
	PipeStream___(Input, read_char);
	PipeStream___(Input, read_hang);
	PipeStream___(Input, unread_char);
	PipeStream___(Input, write_char);
	PipeStreamLet(Input, getleft, windows_);
	PipeStreamLet(Input, setleft, windows_);
	PipeStreamChk(Input, inputp, true);
	PipeStreamChk(Input, outputp, false);
	PipeStreamChk(Input, interactivep, false);
	PipeStreamChk(Input, characterp, true);
	PipeStreamChk(Input, binaryp, false);
	PipeStreamLet(Input, element_type, character_stream);
	PipeStreamDef(Input, external_format);
	PipeStream___(Input, file_length);
	PipeStream___(Input, file_position);
	PipeStream___(Input, file_position_start);
	PipeStream___(Input, file_position_end);
	PipeStream___(Input, file_position_set);
	PipeStream___(Input, file_charlen);
	PipeStream___(Input, file_strlen);
	PipeStream___(Input, listen);
	PipeStream___(Input, clear_input);
	PipeStream___(Input, finish_output);
	PipeStream___(Input, force_output);
	PipeStream___(Input, clear_output);
	PipeStream___(Input, exitpoint);
	PipeStreamLet(Input, termsize, file_);
}


/*
 *  pipe-stream PipeOutput
 */
static int write_char_PipeOutput_(addr stream, unicode c)
{
	return windows_write_char_lock(c);
}

static int finish_output_PipeOutput_(addr stream)
{
	if (terme_finish_output())
		return fmte_("terme_finish_output error.", NULL);
	if (windows_output_flush())
		return fmte_("windows_output_flush error.", NULL);

	return 0;
}

static int force_output_PipeOutput_(addr stream)
{
	return finish_output_PipeOutput_(stream);
}

static int clear_output_PipeOutput_(addr stream)
{
	terme_clear_output();
	windows_output_clear();
	return 0;
}

static void windows_stream_init_output(void)
{
	PipeStreamDef(Output, close);
	PipeStream___(Output, read_byte);
	PipeStream___(Output, unread_byte);
	PipeStream___(Output, write_byte);
	PipeStream___(Output, read_char);
	PipeStream___(Output, read_hang);
	PipeStream___(Output, unread_char);
	PipeStreamLet(Output, write_char, PipeOutput_);
	PipeStreamLet(Output, getleft, windows_);
	PipeStreamLet(Output, setleft, windows_);
	PipeStreamChk(Output, inputp, false);
	PipeStreamChk(Output, outputp, true);
	PipeStreamChk(Output, interactivep, false);
	PipeStreamChk(Output, characterp, true);
	PipeStreamChk(Output, binaryp, false);
	PipeStreamLet(Output, element_type, character_stream);
	PipeStreamDef(Output, external_format);
	PipeStreamDef(Output, file_length);
	PipeStreamDef(Output, file_position);
	PipeStreamDef(Output, file_position_start);
	PipeStreamDef(Output, file_position_end);
	PipeStreamDef(Output, file_position_set);
	PipeStreamLet(Output, file_charlen, windows_);
	PipeStreamLet(Output, file_strlen, windows_);
	PipeStream___(Output, listen);
	PipeStream___(Output, clear_input);
	PipeStreamLet(Output, finish_output, PipeOutput_);
	PipeStreamLet(Output, force_output, PipeOutput_);
	PipeStreamLet(Output, clear_output, PipeOutput_);
	PipeStreamDef(Output, exitpoint);
	PipeStreamLet(Output, termsize, file_);
}


/*
 *  pipe-stream error
 */
static int write_char_PipeError_(addr stream, unicode c)
{
	return write_char_PipeOutput_(stream, c);
}

static void windows_stream_init_error(void)
{
	PipeStreamDef(Error, close);
	PipeStream___(Error, read_byte);
	PipeStream___(Error, unread_byte);
	PipeStream___(Error, write_byte);
	PipeStream___(Error, read_char);
	PipeStream___(Error, read_hang);
	PipeStream___(Error, unread_char);
	PipeStreamLet(Error, write_char, PipeError_);
	PipeStreamLet(Error, getleft, windows_);
	PipeStreamLet(Error, setleft, windows_);
	PipeStreamChk(Error, inputp, false);
	PipeStreamChk(Error, outputp, true);
	PipeStreamChk(Error, interactivep, false);
	PipeStreamChk(Error, characterp, true);
	PipeStreamChk(Error, binaryp, false);
	PipeStreamLet(Error, element_type, character_stream);
	PipeStreamDef(Error, external_format);
	PipeStreamDef(Error, file_length);
	PipeStreamDef(Error, file_position);
	PipeStreamDef(Error, file_position_start);
	PipeStreamDef(Error, file_position_end);
	PipeStreamDef(Error, file_position_set);
	PipeStreamLet(Error, file_charlen, windows_);
	PipeStreamLet(Error, file_strlen, windows_);
	PipeStream___(Error, listen);
	PipeStream___(Error, clear_input);
	PipeStreamLet(Error, finish_output, PipeOutput_);
	PipeStreamLet(Error, force_output, PipeOutput_);
	PipeStreamLet(Error, clear_output, PipeOutput_);
	PipeStreamDef(Error, exitpoint);
	PipeStreamLet(Error, termsize, file_);
}

void windows_stream_init(void)
{
	windows_stream_init_input();
	windows_stream_init_output();
	windows_stream_init_error();
}

