#include <stdio.h>
#include <stdlib.h>
#include "bignum.h"
#include "character.h"
#include "charqueue.h"
#include "condition.h"
#include "common_header.h"
#include "cons.h"
#include "constant.h"
#include "eastasian.h"
#include "equal.h"
#include "file.h"
#include "files.h"
#include "format.h"
#include "heap.h"
#include "integer.h"
#include "memory.h"
#include "object.h"
#include "pathname.h"
#include "print.h"
#include "print_write.h"
#include "readtable.h"
#include "sequence.h"
#include "stream.h"
#include "stream_broadcast.h"
#include "stream_concat.h"
#include "stream_echo.h"
#include "stream_file.h"
#include "stream_pretty.h"
#include "stream_prompt.h"
#include "stream_string.h"
#include "stream_synonym.h"
#include "stream_twoway.h"
#include "strtype.h"
#include "symbol.h"
#include "type_parse.h"
#include "type_typep.h"

_g void *ptrbody_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrBodyStream_Low(stream);
}

_g struct StructStream *ptrstruct_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream_Low(stream);
}

_g void *ptrdata_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrDataStream_Low(stream);
}

_g void gettype_stream(addr stream, enum StreamType *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetTypeStream_Low(stream, ret);
}

_g size_t getindex_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return GetIndexStream_Low(stream);
}

_g void getpathname_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetPathnameStream_Low(stream, ret);
}

_g void setpathname_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetPathnameStream_Low(stream, value);
}

_g void getinfo_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetInfoStream_Low(stream, ret);
}

_g void setinfo_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetInfoStream_Low(stream, value);
}

_g void getinput_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetInputStream_Low(stream, ret);
}

_g void setinput_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetInputStream_Low(stream, value);
}

_g void getoutput_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetOutputStream_Low(stream, ret);
}

_g void setoutput_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetOutputStream_Low(stream, value);
}

_g void stream_alloc(LocalRoot local, addr *ret, enum StreamType type, size_t size)
{
	struct StructStream *ptr;
	size_t allsize;

	allsize = sizeoft(struct StructStream) + size;
	alloc_arraybody(local, ret,
			LISPTYPE_STREAM,
			STREAM_INDEX_SIZE,
			(byte16)allsize);
	Check(0xFFFF <= allsize, "size error");
	ptr = PtrStructStream(*ret);
	memset(ptr, 0, allsize);
	ptr->type = type;
	ptr->terpri = 0;
	ptr->unread = 0;
	ptr->unread_check = 0;
	ptr->closed = 0;
}

_g void stream_heap(addr *ret, enum StreamType type, size_t size)
{
	stream_alloc(NULL, ret, type, size);
}

_g enum StreamType getstreamtype(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream(stream)->type;
}

_g int streamp(addr stream)
{
	return GetType(stream) == LISPTYPE_STREAM;
}

_g int file_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream)) return 0;
	check = getstreamtype(stream);
	return check == StreamType_BinaryInput
		|| check == StreamType_BinaryOutput
		|| check == StreamType_BinaryIO
		|| check == StreamType_CharacterInput
		|| check == StreamType_CharacterOutput
		|| check == StreamType_CharacterIO
		|| check == StreamType_BincharInput
		|| check == StreamType_BincharOutput
		|| check == StreamType_BincharIO;
}

_g int broadcast_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_BroadCast;
}

_g int concatenated_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Concatenated;
}

_g int echo_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Echo;
}

_g int synonym_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Synonym;
}

_g int twoway_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_TwoWay;
}

_g int input_string_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_StringInput;
}

_g int output_string_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_StringOutput;
}

_g int string_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream)) return 0;
	check = getstreamtype(stream);
	return check == StreamType_StringInput
		|| check == StreamType_StringOutput;
}

_g int prompt_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Prompt;
}

_g int pretty_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Pretty;
}


/*****************************************************************************
 *  function
 *****************************************************************************/
_g int close_abort_stream(addr stream, int abort)
{
	int result;
	struct StructStream *ptr;

	CheckType(stream, LISPTYPE_STREAM);
	ptr = PtrStructStream(stream);
	result = (Stream_close[ptr->type])(stream, abort);
	ptr->terpri = 0;
	ptr->unread_check = 0;
	ptr->closed = 1;

	return result;
}

_g int open_stream_p(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream(stream)->closed == 0;
}

_g int closep_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream(stream)->closed != 0;
}

_g void close_stream(addr stream)
{
	(void)close_abort_stream(stream, 0);
}

#define CheckStream(stream, ptr) { \
	CheckType(stream, LISPTYPE_STREAM); \
	ptr = PtrStructStream(stream); \
	if (ptr->closed) { \
		fmte("The stream ~S is already closed.", stream, NULL); \
	} \
}

_g int read_binary_stream(addr stream, void *pos, size_t size, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_binary[(int)ptr->type])(stream, pos, size, ret);
}

_g int readforce_binary_stream(addr stream, void *pos, size_t size, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_readforce_binary[(int)ptr->type])(stream, pos, size, ret);
}

_g int read_byte_stream(addr stream, byte *c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_byte[(int)ptr->type])(stream, c);
}

_g int unread_byte_stream(addr stream, byte c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_unread_byte[(int)ptr->type])(stream, c);
}

_g int write_binary_stream(addr stream, const void *pos, size_t size, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_write_binary[(int)ptr->type])(stream, pos, size, ret);
}

_g int write_byte_stream(addr stream, byte c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_write_byte[(int)ptr->type])(stream, c);
}

_g int read_char_stream(addr stream, unicode *c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_char[(int)ptr->type])(stream, c);
}

_g int read_hang_stream(addr stream, unicode *c, int *hang)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_hang[(int)ptr->type])(stream, c, hang);
}

_g void unread_char_stream(addr stream, unicode c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	(Stream_unread_char[(int)ptr->type])(stream, c);
}

_g void write_char_stream(addr stream, unicode c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	(Stream_write_char[(int)ptr->type])(stream, c);
}

_g void terpri_stream(addr stream)
{
	write_char_stream(stream, '\n');
	PtrStructStream(stream)->terpri = 0;
}

_g int fresh_line_stream(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_fresh_line[(int)ptr->type])(stream);
}

_g void pageout_stream(addr stream)
{
	write_char_stream(stream, '\f');
}

_g void clear_input_stream(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	(Stream_clear_input[(int)ptr->type])(stream);
}

_g size_t terpri_position_stream(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return PtrStructStream(stream)->terpri;
}

_g int inputp_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_inputp[GetIndexStream(stream)])(stream);
}

_g int outputp_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_outputp[GetIndexStream(stream)])(stream);
}

_g int interactivep_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_interactivep[GetIndexStream(stream)])(stream);
}

_g int characterp_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_characterp[GetIndexStream(stream)])(stream);
}

_g int binaryp_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_binaryp[GetIndexStream(stream)])(stream);
}

_g void element_type_stream(addr stream, addr *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	(Stream_element_type[(int)ptr->type])(stream, ret);
}

_g void file_length_stream(addr stream, addr *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	(Stream_file_length[(int)ptr->type])(stream, ret);
}

_g int file_position_stream(addr stream, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position[(int)ptr->type])(stream, ret);
}

_g int file_position_start_stream(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_start[(int)ptr->type])(stream);
}

_g int file_position_end_stream(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_end[(int)ptr->type])(stream);
}

_g int file_position_set_stream(addr stream, size_t pos)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_set[(int)ptr->type])(stream, pos);
}

_g int file_character_length_stream(addr stream, unicode u, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_character_length[(int)ptr->type])(stream, u, ret);
}

_g int file_string_length_stream(addr stream, addr pos, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_string_length[(int)ptr->type])(stream, pos, ret);
}

_g int listen_stream(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_listen[(int)ptr->type])(stream);
}

_g void finish_output_stream(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	(Stream_finish_output[(int)ptr->type])(stream);
}

_g void force_output_stream(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	(Stream_force_output[(int)ptr->type])(stream);
}

_g void clear_output_stream(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	(Stream_clear_output[(int)ptr->type])(stream);
}


/*
 *  default
 */
_g int close_default_stream(addr stream, int abort)
{
	return 1;
}

_g int read_char_default_stream(addr stream, unicode *c)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (ptr->unread_check) {
		*c = ptr->unread;
		ptr->unread_check = 0;
		return 0;
	}
	else {
		return read_char_file(stream, c);
	}
}

_g int read_hang_default_stream(addr stream, unicode *c, int *hang)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (ptr->unread_check) {
		*c = ptr->unread;
		ptr->unread_check = 0;
		*hang = 0;
		return 0;
	}
	else {
		return read_hang_file(stream, c, hang);
	}
}

_g void unread_char_default_stream(addr stream, unicode c)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (ptr->unread_check) {
		fmte("unread already exists.", NULL);
		return;
	}
	ptr->unread = c;
	ptr->unread_check = 1;
}

_g void write_char_default_stream(addr stream, unicode c)
{
	struct StructStream *ptr;

	write_char_file(stream, c);
	ptr = PtrStructStream(stream);
	if (c == '\n' || c == '\f')
		ptr->terpri = 0;
	else
		ptr->terpri += eastasian_width(c);
}

_g int fresh_line_default_stream(addr stream)
{
	if (PtrStructStream(stream)->terpri) {
		terpri_stream(stream);
		return 1;
	}
	else {
		return 0;
	}
}

_g int checkp_true_stream(addr stream)
{
	return 1;
}

_g int checkp_false_stream(addr stream)
{
	return 0;
}

_g void element_type_character_stream(addr stream, addr *ret)
{
	GetConst(COMMON_CHARACTER, ret);
}

_g void element_type_binary_stream(addr stream, addr *ret)
{
	GetConst(STREAM_BINARY_TYPE, ret);
}

_g void element_type_io_stream(addr stream, addr *ret)
{
	addr input, output;

	GetInputStream(stream, &input);
	GetOutputStream(stream, &output);
	element_type_stream(input, &input);
	element_type_stream(output, &output);
	if (equal_function(input, output)) {
		*ret = input;
	}
	else {
		GetConst(COMMON_OR, &stream);
		list_heap(ret, stream, input, output, NULL);
	}
}

_g void file_length_default_stream(addr stream, addr *ret)
{
	size_t size;

	/* TODO: :element-type '(unsigned-byte 16) */
	if (file_length_file(stream, &size))
		*ret = Nil;
	else
		*ret = intsizeh(size);
}

_g int file_position_default_stream(addr stream, size_t *ret)
{
	return 1;
}

_g int file_position_start_default_stream(addr stream)
{
	return 1;
}

_g int file_position_end_default_stream(addr stream)
{
	return 1;
}

_g int file_position_set_default_stream(addr stream, size_t pos)
{
	return 1;
}

_g void finish_output_default_stream(addr stream)
{
}

_g void force_output_default_stream(addr stream)
{
}

_g void clear_output_default_stream(addr stream)
{
}


/*
 *  clang
 */
_g void print_ascii_stream(addr stream, const char *data)
{
	unicode c;

	CheckType(stream, LISPTYPE_STREAM);
	for (;;) {
		c = *(const byte *)data;
		if (c == 0) break;
		write_char_stream(stream, (unicode)c);
		data++;
	}
}

_g void print_unicode_stream(addr stream, const unicode *data)
{
	unicode c;

	CheckType(stream, LISPTYPE_STREAM);
	for (;;) {
		c = *data;
		if (c == 0) break;
		write_char_stream(stream, c);
		data++;
	}
}

_g void print_string_stream(addr stream, addr pos)
{
	unicode c;
	size_t size, i;

	CheckType(stream, LISPTYPE_STREAM);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		write_char_stream(stream, c);
	}
}


/*
 *  initialize
 */
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
}

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

static void defvar_exteranal_format(void)
{
	addr symbol, value;

	GetConst(SYSTEM_EXTERNAL_FORMAT, &symbol);
	GetConst(SYSTEM_UTF_8, &value);
	SetValueSymbol(symbol, value);
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
	GetValueSymbol(input, &input);
	GetConst(SYSTEM_STANDARD_OUTPUT, &output);
	GetValueSymbol(output, &output);
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
	defvar_exteranal_format();
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

static void specialvalue(Execute ptr, constindex index, addr *ret)
{
	addr symbol;
	GetConstant(index, &symbol);
	getspecialcheck_local(ptr, symbol, ret);
}

_g void standard_input_stream(Execute ptr, addr *ret)
{
	specialvalue(ptr, CONSTANT_SPECIAL_STANDARD_INPUT, ret);
}

_g void standard_output_stream(Execute ptr, addr *ret)
{
	specialvalue(ptr, CONSTANT_SPECIAL_STANDARD_OUTPUT, ret);
}

_g void error_output_stream(Execute ptr, addr *ret)
{
	specialvalue(ptr, CONSTANT_SPECIAL_ERROR_OUTPUT, ret);
}

_g void trace_output_stream(Execute ptr, addr *ret)
{
	specialvalue(ptr, CONSTANT_SPECIAL_TRACE_OUTPUT, ret);
}

_g void terminal_io_stream(Execute ptr, addr *ret)
{
	specialvalue(ptr, CONSTANT_SPECIAL_TERMINAL_IO, ret);
}

_g void debug_io_stream(Execute ptr, addr *ret)
{
	specialvalue(ptr, CONSTANT_SPECIAL_DEBUG_IO, ret);
}

_g void query_io_stream(Execute ptr, addr *ret)
{
	specialvalue(ptr, CONSTANT_SPECIAL_QUERY_IO, ret);
}

_g void output_stream_designer(Execute ptr, addr stream, addr *ret)
{
	if (stream == Unbound)
		standard_output_stream(ptr, ret);
	else if (stream == Nil)
		terminal_io_stream(ptr, ret);
	else if (stream == T)
		standard_output_stream(ptr, ret);
	else
		*ret = stream;
	Check(! streamp(*ret), "type error");
}


/*
 *  common-lisp
 */
static void make_empty_file_stream(Execute ptr, addr file)
{
	addr stream;
	if (open_output_binary_stream(ptr, &stream, file, FileOutput_supersede))
		file_error(file);
	close_stream(stream);
}

static int probe_file_stream(Execute ptr, addr file)
{
#ifdef LISP_ANSI
	return 1;
#else
	probe_file_files(ptr, &file, file);
	return file != Nil;
#endif
}

static int open_if_does_not_exist_stream(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_IfDoesNot value, int createp)
{
	if (probe_file_stream(ptr, pos))
		return 0;
	switch (value) {
		case Stream_Open_IfDoesNot_Create:
			if (createp)
				make_empty_file_stream(ptr, pos);
			break;

		case Stream_Open_IfDoesNot_Error:
			file_error(pos);
			break;

		case Stream_Open_IfDoesNot_Nil:
			*ret = Nil;
			return 1;

		default:
			fmte("Invalid :if-does-not-exist value.", NULL);
			return 0;
	}

	return 0;
}

static void open_if_exists_rename_stream(Execute ptr, addr pos)
{
	LocalRoot local;
	LocalStack stack;
	addr path, type, queue, ret1, ret2, ret3;
	size_t i;

	if (! probe_file_stream(ptr, pos))
		return;
	/* rename */
	local = ptr->local;
	for (i = 0; ; i++) {
		push_local(local, &stack);
		/* make filename */
		copy_pathname_alloc(local, &path, pos);
		charqueue_local(local, &queue, 0);
		GetPathname(path, PATHNAME_INDEX_TYPE, &type);
		if (stringp(type)) {
			pushstring_charqueue_local(local, queue, type);
			pushchar_charqueue_local(local, queue, ".");
		}
		make_index_integer_alloc(local, &type, i);
		decimal_charqueue_integer_local(local, type, queue);
		make_charqueue_local(local, queue, &type);
		SetPathname(path, PATHNAME_INDEX_TYPE, type);
		/* check */
		if (! probe_file_stream(ptr, path)) {
			rename_file_files(ptr, &ret1, &ret2, &ret3, pos, path);
			rollback_local(local, stack);
			return;
		}
		rollback_local(local, stack);
	}
}

static int open_if_exists_stream(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_IfExists value,
		enum FileOutput *mode)
{
	if (! probe_file_stream(ptr, pos)) {
		*mode = FileOutput_supersede;
		return 0;
	}
	switch (value) {
		case Stream_Open_IfExists_Error:
			file_error(pos);
			break;

		case Stream_Open_IfExists_RenameAndDelete:
		case Stream_Open_IfExists_NewVersion:
		case Stream_Open_IfExists_Supersede:
			*mode = FileOutput_supersede;
			break;

		case Stream_Open_IfExists_Rename:
			open_if_exists_rename_stream(ptr, pos);
			*mode = FileOutput_supersede;
			break;

		case Stream_Open_IfExists_Overwrite:
			*mode = FileOutput_overwrite;
			break;

		case Stream_Open_IfExists_Append:
			*mode = FileOutput_append;
			break;

		case Stream_Open_IfExists_Nil:
			*ret = Nil;
			return 1;

		default:
			fmte("Invalid :if-exist value.", NULL);
			return 0;
	}

	return 0;
}

static int open_external_input_stream(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Default:
			return open_input_stream(ptr, ret, pos);

		case Stream_Open_External_Ascii:
			return open_input_ascii_stream(ptr, ret, pos);

		case Stream_Open_External_Utf8:
			return open_input_utf8_stream(ptr, ret, pos);

		case Stream_Open_External_Utf8Bom:
			return open_input_utf8bom_stream(ptr, ret, pos);

		case Stream_Open_External_Utf16:
			return open_input_utf16_stream(ptr, ret, pos);

		case Stream_Open_External_Utf16Le:
			return open_input_utf16le_stream(ptr, ret, pos);

		case Stream_Open_External_Utf16Be:
			return open_input_utf16be_stream(ptr, ret, pos);

		case Stream_Open_External_Utf16LeBom:
			return open_input_utf16lebom_stream(ptr, ret, pos);

		case Stream_Open_External_Utf16BeBom:
			return open_input_utf16bebom_stream(ptr, ret, pos);

		case Stream_Open_External_Utf32:
			return open_input_utf32_stream(ptr, ret, pos);

		case Stream_Open_External_Utf32Le:
			return open_input_utf32le_stream(ptr, ret, pos);

		case Stream_Open_External_Utf32Be:
			return open_input_utf32be_stream(ptr, ret, pos);

		case Stream_Open_External_Utf32LeBom:
			return open_input_utf32lebom_stream(ptr, ret, pos);

		case Stream_Open_External_Utf32BeBom:
			return open_input_utf32bebom_stream(ptr, ret, pos);

		default:
			fmte("Invalid :external-format value.", NULL);
			return 1;
	}
}

static void open_direct_input_stream(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;

	/* :if-does-not-exist */
	if (open_if_does_not_exist_stream(ptr, ret, pos, if2, 1))
		return;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Binary:
			check = open_input_binary_stream(ptr, ret, pos);
			break;

		case Stream_Open_Element_Character:
			check = open_external_input_stream(ptr, ret, pos, ext);
			break;

		default:
			fmte("Invalid :element-type value.", NULL);
			return;
	}

	/* error check */
	if (check)
		file_error(pos);
}

static int open_external_output_stream(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext, enum FileOutput mode)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Default:
			return open_output_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Ascii:
			return open_output_ascii_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8:
			return open_output_utf8_stream(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf8Bom:
			return open_output_utf8_stream(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf16:
			return open_output_utf16be_stream(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16Le:
			return open_output_utf16le_stream(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16Be:
			return open_output_utf16be_stream(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16LeBom:
			return open_output_utf16le_stream(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf16BeBom:
			return open_output_utf16be_stream(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf32:
			return open_output_utf32be_stream(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32Le:
			return open_output_utf32le_stream(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32Be:
			return open_output_utf32be_stream(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32LeBom:
			return open_output_utf32le_stream(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf32BeBom:
			return open_output_utf32be_stream(ptr, ret, pos, mode, 1);

		default:
			fmte("Invalid :external-format value.", NULL);
			return 1;
	}
}

static void open_direct_output_stream(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;
	enum FileOutput mode;

	/* :if-does-not-exist */
	if (open_if_does_not_exist_stream(ptr, ret, pos, if2, 0))
		return;

	/* :if-exists */
	if (open_if_exists_stream(ptr, ret, pos, if1, &mode))
		return;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Binary:
			check = open_output_binary_stream(ptr, ret, pos, mode);
			break;

		case Stream_Open_Element_Character:
			check = open_external_output_stream(ptr, ret, pos, ext, mode);
			break;

		default:
			fmte("Invalid :element-type value.", NULL);
			return;
	}

	/* error check */
	if (check)
		file_error(pos);
}

static int open_external_io_stream(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext, enum FileOutput mode)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Default:
			return open_io_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Ascii:
			return open_io_ascii_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8:
			return open_io_utf8_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8Bom:
			return open_io_utf8bom_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16:
			return open_io_utf16_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16Le:
			return open_io_utf16le_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16Be:
			return open_io_utf16be_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16LeBom:
			return open_io_utf16lebom_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16BeBom:
			return open_io_utf16bebom_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32:
			return open_io_utf32_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32Le:
			return open_io_utf32le_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32Be:
			return open_io_utf32be_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32LeBom:
			return open_io_utf32lebom_stream(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32BeBom:
			return open_io_utf32bebom_stream(ptr, ret, pos, mode);

		default:
			fmte("Invalid :external-format value.", NULL);
			return 1;
	}
}

static void open_direct_io_stream(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;
	enum FileOutput mode;

	/* :if-does-not-exist */
	if (open_if_does_not_exist_stream(ptr, ret, pos, if2, 0))
		return;

	/* :if-exists */
	if (open_if_exists_stream(ptr, ret, pos, if1, &mode))
		return;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Binary:
			check = open_io_binary_stream(ptr, ret, pos, mode);
			break;

		case Stream_Open_Element_Character:
			check = open_external_io_stream(ptr, ret, pos, ext, mode);
			break;

		default:
			fmte("Invalid :element-type value.", NULL);
			return;
	}

	/* error check */
	if (check)
		file_error(pos);
}

static void open_direct_probe_stream(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	open_direct_input_stream(ptr, &pos, pos, type, if2, ext);
	if (pos != Nil)
		close_stream(pos);
	*ret = pos;
}

_g void open_stream(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Direction direction,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	/* :direction */
	switch (direction) {
		case Stream_Open_Direction_Input:
			open_direct_input_stream(ptr, ret, pos, type, if2, ext);
			break;

		case Stream_Open_Direction_Output:
			open_direct_output_stream(ptr, ret, pos, type, if1, if2, ext);
			break;

		case Stream_Open_Direction_Io:
			open_direct_io_stream(ptr, ret, pos, type, if1, if2, ext);
			break;

		case Stream_Open_Direction_Probe:
			open_direct_probe_stream(ptr, ret, pos, type, if2, ext);
			break;

		default:
			fmte("Invalid direction.", NULL);
			return;
	}
}


/*
 *  read-char
 */
_g void stream_designer(Execute ptr, addr pos, addr *ret, int inputp)
{
	addr type;
	constindex index;

	/* default */
	if (pos == Unbound) {
		index = inputp?
			CONSTANT_SPECIAL_STANDARD_INPUT:
			CONSTANT_SPECIAL_STANDARD_OUTPUT;
		GetConstant(index, &pos);
	}

	/* symbol */
	if (symbolp(pos)) {
		getspecialcheck_local(ptr, pos, ret);
		return;
	}

	/* stream */
	if (streamp(pos)) {
		*ret = pos;
		return;
	}

	/* error */
	GetConst(COMMON_STREAM, &type);
	type_error(pos, type);
}

_g void read_byte_common(addr *ret, addr stream, int errorp, addr value)
{
	byte c;

	if (read_byte_stream(stream, &c)) {
		if (errorp)
			end_of_file(stream);
		else {
			*ret = value;
			return;
		}
	}
	fixnum_heap(ret, (fixnum)c);
}

_g void write_byte_common(addr stream, addr value)
{
	addr pos;
	fixnum c;

	if (getfixnumtype(value, &c) || c < 0 || 0xFF < c) {
		GetConst(STREAM_BINARY_TYPE, &pos);
		type_error(value, pos);
	}
	write_byte_stream(stream, (byte)c);
}

static void end_of_file_recursive(addr pos, int recp)
{
	if (recp)
		fmte("The stream ~S reach end-of-file, but recursive-p is true.", pos, NULL);
	else
		end_of_file(pos);
}

static void peek_char_nil(addr *ret, addr stream, int errorp, addr value, int recp)
{
	unicode u;

	if (read_char_stream(stream, &u)) {
		if (errorp)
			end_of_file_recursive(stream, recp);
		*ret = value;
	}
	else {
		unread_char_stream(stream, u);
		character_heap(ret, u);
	}
}

static void peek_char_t(addr *ret, addr stream, int errorp, addr value, int recp)
{
	unicode u;

	for (;;) {
		if (read_char_stream(stream, &u)) {
			if (errorp)
				end_of_file_recursive(stream, recp);
			*ret = value;
			break;
		}
		if (! isSpaceUnicode(u)) {
			unread_char_stream(stream, u);
			character_heap(ret, u);
			break;
		}
	}
}

static void peek_char_character(addr *ret,
		addr type, addr stream, int errorp, addr value, int recp)
{
	unicode u, check;

	GetCharacter(type, &check);
	for (;;) {
		if (read_char_stream(stream, &u)) {
			if (errorp)
				end_of_file_recursive(stream, recp);
			*ret = value;
			break;
		}
		if (check == u) {
			peek_char_nil(ret, stream, errorp, value, recp);
			break;
		}
	}
}

_g void peek_char_common(Execute ptr, addr *ret,
		addr type, addr stream, int errorp, addr value, int recp)
{
	stream_designer(ptr, stream, &stream, 1);
	if (type == Nil)
		peek_char_nil(ret, stream, errorp, value, recp);
	else if (type == T)
		peek_char_t(ret, stream, errorp, value, recp);
	else
		peek_char_character(ret, type, stream, errorp, value, recp);
}

_g void read_char_no_hang_common(Execute ptr, addr *ret,
		addr pos, int errorp, addr value, int recp)
{
	int hang;
	unicode u;

	stream_designer(ptr, pos, &pos, 1);
	if (read_hang_stream(pos, &u, &hang)) {
		/* eof */
		if (errorp)
			end_of_file_recursive(pos, recp);
		*ret = value;
		return;
	}
	if (hang) {
		*ret = Nil;
		return;
	}
	/* read character */
	character_heap(ret, u);
}

_g void read_char_common(Execute ptr, addr *ret,
		addr pos, int errorp, addr value, int recp)
{
	unicode u;

	stream_designer(ptr, pos, &pos, 1);
	if (read_char_stream(pos, &u)) {
		/* eof */
		if (errorp)
			end_of_file_recursive(pos, recp);
		*ret = value;
		return;
	}
	/* read character */
	character_heap(ret, u);
}

enum EndOfLine_Mode {
	EndOfLine_Auto,
	EndOfLine_CR,
	EndOfLine_LF,
	EndOfLine_CRLF
};

_g enum EndOfLine_Mode get_end_of_line_mode(Execute ptr)
{
	addr pos, check;

	GetConst(SYSTEM_END_OF_LINE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	/* Auto */
	GetConst(SYSTEM_AUTO, &check);
	if (check == pos)
		return EndOfLine_Auto;
	/* CR */
	GetConst(SYSTEM_CR, &check);
	if (check == pos)
		return EndOfLine_CR;
	/* LF */
	GetConst(SYSTEM_LF, &check);
	if (check == pos)
		return EndOfLine_LF;
	/* CRLF */
	GetConst(SYSTEM_CRLF, &check);
	if (check == pos)
		return EndOfLine_CRLF;
	/* error */
	fmte("Invalid *end-of-line* value ~S.", pos, NULL);
	return EndOfLine_Auto;
}

_g void read_line_common(Execute ptr, addr *ret, int *miss,
		addr pos, int errorp, addr value, int recp)
{
	int check;
	enum EndOfLine_Mode mode;
	unicode u;
	addr queue;
	LocalRoot local;
	LocalStack stack;

	stream_designer(ptr, pos, &pos, 1);
	local = ptr->local;
	push_local(local, &stack);
	charqueue_local(local, &queue, 0);
	mode = get_end_of_line_mode(ptr);
	for (check = 0; ; check = 1) {
		if (read_char_stream(pos, &u))
			goto finish_eof;
		switch (mode) {
			case EndOfLine_CR:
				if (u == 0x0D)
					goto finish_value;
				break;

			case EndOfLine_LF:
				if (u == 0x0A)
					goto finish_value;
				break;

			case EndOfLine_CRLF:
				if (u == 0x0D) {
					if (read_char_stream(pos, &u) || u != 0x0A)
						fmte("Invalid CR-LF code.", NULL);
					goto finish_value;
				}
				break;

			case EndOfLine_Auto:
			default:
				if (u == 0x0A)
					goto finish_value;
				if (u == 0x0D) {
					if (read_char_stream(pos, &u) == 0 && u != 0x0A)
						unread_char_stream(pos, u);
					goto finish_value;
				}
				break;
		}
		push_charqueue_local(local, queue, u);
	}

finish_eof:
	if (check == 0)
		goto finish_error;
	make_charqueue_heap(queue, ret);
	rollback_local(local, stack);
	*miss = 1;
	return;

finish_value:
	make_charqueue_heap(queue, ret);
	rollback_local(local, stack);
	*miss = 0;
	return;

finish_error:
	if (errorp)
		end_of_file_recursive(pos, recp);
	*ret = value;
	*miss = 1;
	return;
}

static void write_string_stream(Execute ptr, addr string, addr rest, addr *ret)
{
	unicode c;
	addr stream;
	size_t size, start, end, i;

	/* argument */
	string_length(string, &size);
	if (rest == Nil) {
		stream_designer(ptr, Unbound, &stream, 0);
		start = 0;
		end = size;
	}
	else {
		getcons(rest, &stream, &rest);
		stream_designer(ptr, stream, &stream, 0);
		keyword_start_end(size, rest, &start, &end);
	}

	for (i = start; i < end; i++) {
		string_getc(string, i, &c);
		write_char_stream(stream, c);
	}
	*ret = stream;
}

_g void write_string_common(Execute ptr, addr string, addr rest)
{
	write_string_stream(ptr, string, rest, &string);
}

_g void write_line_common(Execute ptr, addr string, addr rest)
{
	write_string_stream(ptr, string, rest, &string);
	terpri_stream(string);
}

static void read_sequence_character(addr *ret,
		addr seq, addr stream, size_t start, size_t end)
{
	unicode c;
	addr value;

	for (; start < end; start++) {
		if (read_char_stream(stream, &c))
			break;
		character_heap(&value, c);
		setelt_sequence(seq, start, value);
	}
	*ret = intsizeh(start);
}

static void read_sequence_binary(addr *ret,
		addr seq, addr stream, size_t start, size_t end)
{
	byte c;
	addr value;

	for (; start < end; start++) {
		if (read_byte_stream(stream, &c))
			break;
		fixnum_heap(&value, c);
		setelt_sequence(seq, start, value);
	}
	*ret = intsizeh(start);
}

_g void read_sequence_common(addr *ret, addr seq, addr stream, size_t start, size_t end)
{
	/* character stream */
	if (characterp_stream(stream)) {
		read_sequence_character(ret, seq, stream, start, end);
		return;
	}

	/* binary stream */
	if (binaryp_stream(stream)) {
		read_sequence_binary(ret, seq, stream, start, end);
		return;
	}

	/* error */
	fmte("Invalid stream ~S.", stream, NULL);
}

static void write_sequence_character(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	unicode c;
	addr value;
	LocalStack stack;

	for (; start < end; start++) {
		push_local(local, &stack);
		getelt_sequence(local, seq, start, &value);
		if (! characterp(value))
			TypeError(value, CHARACTER);
		GetCharacter(value, &c);
		rollback_local(local, stack);
		write_char_stream(stream, c);
	}
}

static void write_sequence_binary(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	fixnum c;
	addr value;
	LocalStack stack;

	for (; start < end; start++) {
		push_local(local, &stack);
		getelt_sequence(local, seq, start, &value);
		if (! fixnump(value))
			TypeError(value, INTEGER);
		GetFixnum(value, &c);
		rollback_local(local, stack);
		if (c < 0 || 0xFF < c)
			fmte("Invalid binary value ~S.", fixnumh(c), NULL);
		write_byte_stream(stream, (byte)c);
	}
}

_g void write_sequence_common(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	/* character stream */
	if (characterp_stream(stream)) {
		write_sequence_character(local, seq, stream, start, end);
		return;
	}

	/* binary stream */
	if (binaryp_stream(stream)) {
		write_sequence_binary(local, seq, stream, start, end);
		return;
	}

	/* error */
	fmte("Invalid stream ~S.", stream, NULL);
}

_g int prompt_for_stream(Execute ptr, addr type, addr prompt, addr *ret)
{
	int result;
	addr stream, spec, value;

	/* output */
	query_io_stream(ptr, &stream);
	fresh_line_stream(stream);
	if (princ_print(ptr, stream, prompt))
		return 1;
	finish_output_stream(stream);

	/* query */
	if (type != T) {
		if (parse_type(ptr, &spec, type, Nil))
			return 1;
	}
	for (;;) {
		clear_input_stream(stream);
		if (read_stream(ptr, stream, &result, &value))
			return 1;
		if (result)
			fmte("Can't read from *query-io* stream.", NULL);
		if (type == T)
			break;
		if (typep_clang(value, spec, &result))
			return 1;
		if (result)
			break;

		fmts(stream, "~%Please answer ~A type: ", type, NULL);
		finish_output_stream(stream);
	}
	*ret = value;

	return 0;
}

_g int yes_or_no_p_common(Execute ptr, addr args, int exactp, int *ret)
{
	int miss;
	unicode c;
	addr control, stream, pos;
	size_t size;

	/* argument */
	if (args == Nil) {
		control = Nil;
	}
	else {
		GetCons(args, &control, &args);
	}

	/* output */
	query_io_stream(ptr, &stream);
	if (control != Nil) {
		fresh_line_stream(stream);
		if (format_lisp(ptr, stream, control, args, &control))
			return 1;
		fmts(stream, " ", NULL);
	}
	fmts(stream, exactp? "(yes or no) ": "(y or n) ", NULL);
	finish_output_stream(stream);

	/* query */
	for (;;) {
		clear_input_stream(stream);
		read_line_common(ptr, &pos, &miss, stream, 1, Unbound, 0);
		if (pos == Unbound)
			fmte("*query-io* don't read yes/or question.", NULL);
		if (exactp) {
			if (string_equalp_char(pos, "yes")) { *ret = 1; break; }
			if (string_equalp_char(pos, "no")) { *ret = 0; break; }
			fmts(stream, "~%Please answer yes or no: ", NULL);
		}
		else {
			string_length(pos, &size);
			if (size != 0) {
				string_getc(pos, 0, &c);
				if (toUpperUnicode(c) == 'Y') { *ret = 1; break; }
				if (toUpperUnicode(c) == 'N') { *ret = 0; break; }
			}
			fmts(stream, "~%Please answer y or n: ", NULL);
		}
		finish_output_stream(stream);
	}

	return 0;
}


/*
 *  core
 */
_g void update_standard_stream(void)
{
	addr pos;

	/* stdin */
	GetConst(STREAM_STDIN, &pos);
	update_standard_input(pos);
	/* stdout */
	GetConst(STREAM_STDOUT, &pos);
	update_standard_output(pos);
	/* stderr */
	GetConst(STREAM_STDERR, &pos);
	update_standard_error(pos);
}

_g int save_stream(addr pos)
{
	switch (PtrStructStream(pos)->type) {
		case StreamType_BinaryInput:
		case StreamType_BinaryOutput:
		case StreamType_BinaryIO:
		case StreamType_CharacterInput:
		case StreamType_CharacterOutput:
		case StreamType_CharacterIO:
			return save_stream_file(pos);

		case StreamType_BincharInput:
		case StreamType_BincharOutput:
		case StreamType_BincharIO:
			return save_stream_system(pos);

		default:
			break;
	}

	return 0;
}

