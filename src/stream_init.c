#include "cons.h"
#include "constant.h"
#include "control_object.h"
#include "file.h"
#include "function.h"
#include "pointer.h"
#include "stream.h"
#include "stream_broadcast.h"
#include "stream_concat.h"
#include "stream_echo.h"
#include "stream_error.h"
#include "stream_file.h"
#include "stream_pretty.h"
#include "stream_prompt.h"
#include "stream_string.h"
#include "stream_synonym.h"
#include "stream_twoway.h"
#include "symbol.h"

/*
 *  initialize
 */
#define LispStreamTypeError(x, type) Stream_##x[type] = x##_stream_error

static void init_stream_extend_type(int type)
{
	LispStreamTypeError(close, type);
	LispStreamTypeError(read_binary, type);
	LispStreamTypeError(readforce_binary, type);
	LispStreamTypeError(read_byte, type);
	LispStreamTypeError(unread_byte, type);
	LispStreamTypeError(write_binary, type);
	LispStreamTypeError(write_byte, type);
	LispStreamTypeError(read_char, type);
	LispStreamTypeError(read_hang, type);
	LispStreamTypeError(unread_char, type);
	LispStreamTypeError(write_char, type);
	LispStreamTypeError(terpri, type);
	LispStreamTypeError(getleft, type);
	LispStreamTypeError(setleft, type);
	LispStreamTypeError(fresh_line, type);
	LispStreamTypeError(clear_input, type);
	LispStreamTypeError(inputp, type);
	LispStreamTypeError(outputp, type);
	LispStreamTypeError(interactivep, type);
	LispStreamTypeError(characterp, type);
	LispStreamTypeError(binaryp, type);
	LispStreamTypeError(element_type, type);
	LispStreamTypeError(file_length, type);
	LispStreamTypeError(file_position, type);
	LispStreamTypeError(file_position_start, type);
	LispStreamTypeError(file_position_end, type);
	LispStreamTypeError(file_position_set, type);
	LispStreamTypeError(file_character_length, type);
	LispStreamTypeError(file_string_length, type);
	LispStreamTypeError(listen, type);
	LispStreamTypeError(finish_output, type);
	LispStreamTypeError(force_output, type);
	LispStreamTypeError(clear_output, type);
	LispStreamTypeError(exitpoint, type);
	LispStreamTypeError(terminal_width, type);
}

static void init_stream_extend(void)
{
	int i;

	for (i = 0; i < LISP_STREAM_EXTEND; i++)
		init_stream_extend_type(((int)StreamType_Size) + i);
}

static int finalize_close_stream(Execute ptr)
{
	addr stream;

	getdata_control(ptr, &stream);
	CheckType(stream, LISPTYPE_STREAM);
	return close_stream_(stream);
}

_g void push_close_stream(Execute ptr, addr stream)
{
	setprotect_control(ptr, p_finalize_close_stream, stream);
}

static void init_stream_function(void)
{
	SetPointerType(empty, finalize_close_stream);
}

_g void init_stream(void)
{
	init_stream_binary_input();
	init_stream_binary_output();
	init_stream_binary_io();
	init_stream_character_input();
	init_stream_character_output();
	init_stream_character_io();
	init_stream_binchar_input();
	init_stream_binchar_output();
	init_stream_binchar_io();
	init_stream_string_input();
	init_stream_string_output();
	init_stream_synonym();
	init_stream_broadcast();
	init_stream_concatenated();
	init_stream_twoway();
	init_stream_echo();
	init_stream_prompt();
	init_stream_pretty();
	init_stream_extend();
	init_stream_function();
}


/*
 *  build
 */
static void defvar_stream_binary_type(void)
{
	addr symbol, value;

	GetConst(COMMON_UNSIGNED_BYTE, &symbol);
	fixnum_heap(&value, 8);
	list_heap(&value, symbol, value, NULL);
	SetConstant(CONSTANT_STREAM_BINARY_TYPE, value);
}

static void defvar_external_format(void)
{
	addr symbol, value;

	GetConst(SYSTEM_EXTERNAL_FORMAT, &symbol);
	GetConst(SYSTEM_UTF_8, &value);
	SetValueSymbol(symbol, value);
}

static void defvar_system_standard_input(void)
{
	addr stream, symbol;

	make_standard_input(&stream);
	GetConst(SYSTEM_STANDARD_INPUT, &symbol);
	SetValueSymbol(symbol, stream);
	SetConst(STREAM_STDIN, stream);
}

static void defvar_system_standard_output(void)
{
	addr stream, symbol;

	make_standard_output(&stream);
	GetConst(SYSTEM_STANDARD_OUTPUT, &symbol);
	SetValueSymbol(symbol, stream);
	SetConst(STREAM_STDOUT, stream);
}

static void defvar_system_standard_error(void)
{
	addr stream, symbol;

	make_standard_error(&stream);
	GetConst(SYSTEM_STANDARD_ERROR, &symbol);
	SetValueSymbol(symbol, stream);
	SetConst(STREAM_STDERR, stream);
}

static void defvar_system_prompt(void)
{
	addr stream, symbol;

	open_prompt_stream(&stream);
	GetConst(SYSTEM_PROMPT_VALUE, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_standard_input(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SYSTEM_STANDARD_INPUT, &stream);
	open_synonym_stream(&stream, stream);
	/* defvar */
	GetConst(SPECIAL_STANDARD_INPUT, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_standard_output(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SYSTEM_STANDARD_OUTPUT, &stream);
	open_synonym_stream(&stream, stream);
	/* defvar */
	GetConst(SPECIAL_STANDARD_OUTPUT, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_error_output(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SYSTEM_STANDARD_ERROR, &stream);
	open_synonym_stream(&stream, stream);
	/* defvar */
	GetConst(SPECIAL_ERROR_OUTPUT, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_terminal_io(void)
{
	addr symbol, input, output, stream;

	/* twoway */
	GetConst(SYSTEM_PROMPT_VALUE, &input);
	open_synonym_stream(&input, input);
	GetConst(SPECIAL_STANDARD_OUTPUT, &output);
	open_synonym_stream(&output, output);
	open_twoway_stream(&stream, input, output);
	/* defvar */
	GetConst(SPECIAL_TERMINAL_IO, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_trace_output(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SPECIAL_TERMINAL_IO, &stream);
	open_synonym_stream(&stream, stream);
	/* defvar */
	GetConst(SPECIAL_TRACE_OUTPUT, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_debug_io(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SPECIAL_TERMINAL_IO, &stream);
	open_synonym_stream(&stream, stream);
	/* defvar */
	GetConst(SPECIAL_DEBUG_IO, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_query_io(void)
{
	addr symbol, stream;

	/* synonym */
	GetConst(SPECIAL_TERMINAL_IO, &stream);
	open_synonym_stream(&stream, stream);
	/* defvar */
	GetConst(SPECIAL_QUERY_IO, &symbol);
	SetValueSymbol(symbol, stream);
}

static void defvar_end_of_line(void)
{
	addr symbol, value;

	GetConst(SYSTEM_END_OF_LINE, &symbol);
	GetConst(SYSTEM_AUTO, &value);
	SetValueSymbol(symbol, value);
}

_g void build_stream(void)
{
	defvar_stream_binary_type();
	defvar_external_format();
	defvar_system_standard_input();
	defvar_system_standard_output();
	defvar_system_standard_error();
	defvar_standard_input();
	defvar_standard_output();
	defvar_system_prompt();
	defvar_error_output();
	defvar_terminal_io();
	defvar_trace_output();
	defvar_debug_io();
	defvar_query_io();
	defvar_end_of_line();
}

