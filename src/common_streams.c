/*
 *  ANSI COMMON LISP: 21. Streams
 */
#include "common_header.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "eval_declare.h"
#include "file.h"
#include "integer.h"
#include "package.h"
#include "pathname.h"
#include "sequence.h"
#include "stream_broadcast.h"
#include "stream_concat.h"
#include "stream_echo.h"
#include "stream_string.h"
#include "stream_synonym.h"
#include "stream_twoway.h"
#include "stream.h"
#include "strtype.h"
#include "type_parse.h"
#include "type_subtypep.h"
#include "type_typep.h"

/* (defun input-stream-p (stream) ...) -> boolean */
static void function_input_stream_p(Execute ptr, addr pos)
{
	int check = inputp_stream(pos);
	setbool_control(ptr, check);
}

static void defun_input_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INPUT_STREAM_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_input_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun output-stream-p (stream) ...) -> boolean */
static void function_output_stream_p(Execute ptr, addr pos)
{
	int check = outputp_stream(pos);
	setbool_control(ptr, check);
}

static void defun_output_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_OUTPUT_STREAM_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_output_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun interactive-stream-p (stream) ...) -> boolean */
static void function_interactive_stream_p(Execute ptr, addr pos)
{
	int check = interactivep_stream(pos);
	setbool_control(ptr, check);
}

static void defun_interactive_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTERACTIVE_STREAM_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_interactive_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun open-stream-p (stream) ...) -> boolean */
static void function_open_stream_p(Execute ptr, addr pos)
{
	int check = open_stream_p(pos);
	setbool_control(ptr, check);
}

static void defun_open_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_OPEN_STREAM_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_open_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun streamp (object) ...) -> boolean */
static void function_streamp(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_STREAM);
}

static void defun_streamp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STREAMP, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_streamp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun stream-element-type (stream) ...) -> typespec  */
static void function_stream_element_type(Execute ptr, addr var)
{
	element_type_stream(var, &var);
	setresult_control(ptr, var);
}

static void type_stream_element_type(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Stream);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, TypeSpec);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_stream_element_type(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STREAM_ELEMENT_TYPE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_stream_element_type);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_stream_element_type(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-byte (stream &optional errorp value) ...) -> integer
 *   stream   input-stream
 *   errorp   t
 *   value    t
 *   integer  integer
 */
static void function_read_byte(Execute ptr, addr stream, addr errorp, addr value)
{
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	read_byte_common(&stream, stream, errorp != Nil, value);
	setresult_control(ptr, stream);
}

static void type_read_byte(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, InputStream);
	GetTypeTable(&values, T);
	typeargs_var1opt2(&arg, arg, values, values);
	GetTypeValues(&values, Integer);
	type_compiled_heap(arg, values, ret);
}

static void defun_read_byte(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_BYTE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt2(pos, p_defun_read_byte);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_byte(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-byte (byte stream) ...) -> byte
 *   byte    integer
 *   stream  output-stream
 */
static void function_write_byte(Execute ptr, addr value, addr stream)
{
	write_byte_common(stream, value);
	exitpoint_stream(stream);
	setresult_control(ptr, value);
}

static void type_write_byte(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Integer);
	GetTypeTable(&values, OutputStream);
	typeargs_var2(&arg, arg, values);
	GetTypeValues(&values, Integer);
	type_compiled_heap(arg, values, ret);
}

static void defun_write_byte(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_BYTE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_write_byte);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_write_byte(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun peek-char (&optional type stream errorp value recp) ...) -> result
 *   type    (or character boolean)
 *   stream  input-stream
 *   errorp  t  ;; boolean
 *   value   t
 *   recp    t  ;; boolean
 *   result  t
 */
static void function_peek_char(Execute ptr, addr type, addr stream,
		addr errorp, addr value, addr recp)
{
	if (type == Unbound)
		type = Nil;
	if (stream == Unbound)
		standard_input_stream(ptr, &stream);
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	peek_char_common(ptr, &type, type, stream, errorp != Nil, value, recp != Nil);
	setresult_control(ptr, type);
}

static void type_peek_char(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, Character);
	GetTypeTable(&values, Boolean);
	type2or_heap(arg, values, &arg);
	GetTypeTable(&values, StreamDesigner);
	GetTypeTable(&type, T);
	typeargs_opt5(&arg, arg, values, type, type, type);
	GetTypeValues(&values, T);
	type_compiled_heap(arg, values, ret);
}

static void defun_peek_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_PEEK_CHAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt5(pos, p_defun_peek_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_peek_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-char (&optional stream errorp value recp) ...) -> result
 *   stream  string-deginer  (input)
 *   errorp  t  ;; boolean
 *   value   t
 *   recp    t  ;; boolean
 *   result  t  ;; (or character t value)
 */
static void function_read_char(Execute ptr,
		addr stream, addr errorp, addr value, addr recp)
{
	if (stream == Unbound)
		standard_input_stream(ptr, &stream);
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	read_char_common(ptr, &stream, stream, errorp != Nil, value, recp != Nil);
	setresult_control(ptr, stream);
}

static void defun_read_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_CHAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt4(pos, p_defun_read_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ReadChar);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-char-no-hang (&optional stream errorp value recp) ...) -> result
 *   stream  string-deginer  (input)
 *   errorp  t  ;; boolean
 *   value   t
 *   recp    t  ;; boolean
 *   result  t  ;; (or character t value)
 */
static void function_read_char_no_hang(Execute ptr,
		addr stream, addr errorp, addr value, addr recp)
{
	if (stream == Unbound)
		standard_input_stream(ptr, &stream);
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	read_char_no_hang_common(ptr, &stream, stream, errorp != Nil, value, recp != Nil);
	setresult_control(ptr, stream);
}

static void defun_read_char_no_hang(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_CHAR_NO_HANG, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt4(pos, p_defun_read_char_no_hang);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ReadChar);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun terpri (&optional stream) ...) -> null */
static void function_terpri(Execute ptr, addr stream)
{
	if (stream == Unbound)
		standard_output_stream(ptr, &stream);
	terpri_stream(stream);
	exitpoint_stream(stream);
	setresult_control(ptr, Nil);
}

static void type_terpri(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, StreamDesigner);
	typeargs_opt1(&arg, arg);
	GetTypeValues(&values, Null);
	type_compiled_heap(arg, values, ret);
}

static void defun_terpri(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TERPRI, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_terpri);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_terpri(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fresh-line (&optional stream) ...) -> boolean */
static void function_fresh_line(Execute ptr, addr stream)
{
	int check;

	if (stream == Unbound)
		standard_output_stream(ptr, &stream);
	check = fresh_line_stream(stream);
	exitpoint_stream(stream);
	setbool_control(ptr, check);
}

static void type_fresh_line(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, StreamDesigner);
	typeargs_opt1(&arg, arg);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_fresh_line(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FRESH_LINE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_fresh_line);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fresh_line(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun unread-char (character &optional stream) ...) -> null */
static void function_unread_char(Execute ptr, addr pos, addr stream)
{
	unicode c;

	if (stream == Unbound)
		standard_output_stream(ptr, &stream);
	GetCharacter(pos, &c);
	unread_char_stream(stream, c);
	setresult_control(ptr, Nil);
}

static void type_unread_char(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Character);
	GetTypeTable(&values, StreamDesigner);
	typeargs_var1opt1(&arg, arg, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(arg, values, ret);
}

static void defun_unread_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_UNREAD_CHAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unread_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_unread_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-char (character &optional stream) ...) -> character */
static void function_write_char(Execute ptr, addr pos, addr stream)
{
	unicode c;

	stream_designer(ptr, stream, &stream, 0);
	GetCharacter(pos, &c);
	write_char_stream(stream, c);
	exitpoint_stream(stream);
	setresult_control(ptr, pos);
}

static void type_write_char(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Character);
	GetTypeTable(&values, StreamDesigner);
	typeargs_var1opt1(&arg, arg, values);
	GetTypeValues(&values, Character);
	type_compiled_heap(arg, values, ret);
}

static void defun_write_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_CHAR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_write_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_write_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-line (&optional stream eof-p eof-value rec-p) ...)
 *     -> line, miss-p
 *  stream     stream-designer
 *  eof-p      t  ;; boolean
 *  eof-value  t
 *  rec-p      t  ;; boolean
 *  line       t  ;; (or string t)
 *  miss-p     boolean
 */
static void function_read_line(Execute ptr,
		addr stream, addr errorp, addr value, addr recp)
{
	int miss;

	if (stream == Unbound)
		standard_input_stream(ptr, &stream);
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	read_line_common(ptr, &stream, &miss, stream, errorp != Nil, value, recp != Nil);
	setvalues_control(ptr, stream, miss? T: Nil, NULL);
}

static void type_read_line(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, StreamDesigner);
	GetTypeTable(&type, T);
	typeargs_opt4(&arg, arg, type, type, type);
	GetTypeTable(&values, Boolean);
	typevalues_values2(&values, type, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_read_line(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_LINE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt4(pos, p_defun_read_line);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_line(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-string (string &optional stream &key start end) ...) -> string */
static void function_write_string(Execute ptr, addr string, addr rest)
{
	write_string_common(ptr, string, rest);
	setresult_control(ptr, string);
}

static void defun_write_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_STRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_write_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, WriteString);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-line (string &optional stream &key start end) ...) -> string */
static void function_write_line(Execute ptr, addr string, addr rest)
{
	write_line_common(ptr, string, rest);
	setresult_control(ptr, string);
}

static void defun_write_line(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_LINE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_write_line);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, WriteString);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun read-sequence (sequence stream &key start end) ...) -> position
 *   sequence  sequence
 *   stream    input-stream
 *   start     keyword-start
 *   end       keyword-end
 *   position  index
 */
static void function_read_sequence(Execute ptr, addr var, addr stream, addr rest)
{
	size_t start, end;

	start = length_sequence(var, 0);
	keyword_start_end(start, rest, &start, &end);
	read_sequence_common(&var, var, stream, start, end);
	setresult_control(ptr, var);
}

static void type_read_sequence(addr *ret)
{
	addr arg, values, key, type1, type2;

	/* key */
	KeyTypeTable(&type1, START, KeywordStart);
	KeyTypeTable(&type2, END, KeywordEnd);
	list_heap(&key, type1, type2, NULL);
	/* arg */
	GetTypeTable(&type1, Sequence);
	GetTypeTable(&type2, InputStream);
	typeargs_var2key(&arg, type1, type2, key);
	GetTypeValues(&values, Index);
	type_compiled_heap(arg, values, ret);
}

static void defun_read_sequence(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_SEQUENCE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_read_sequence);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_sequence(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-sequence (sequence stream &key start end) ...) -> sequence */
static void function_write_sequence(Execute ptr, addr var, addr stream, addr rest)
{
	size_t start, end;

	start = length_sequence(var, 0);
	keyword_start_end(start, rest, &start, &end);
	write_sequence_common(ptr->local, var, stream, start, end);
	exitpoint_stream(stream);
	setresult_control(ptr, var);
}

static void type_write_sequence(addr *ret)
{
	addr arg, values, key, type1, type2;

	/* key */
	KeyTypeTable(&type1, START, KeywordStart);
	KeyTypeTable(&type2, END, KeywordEnd);
	list_heap(&key, type1, type2, NULL);
	/* arg */
	GetTypeTable(&type1, Sequence);
	GetTypeTable(&type2, OutputStream);
	typeargs_var2key(&arg, type1, type2, key);
	GetTypeValues(&values, Sequence);
	type_compiled_heap(arg, values, ret);
}

static void defun_write_sequence(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_SEQUENCE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_write_sequence);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_write_sequence(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-length (stream) ...) -> length
 *   stream  stream (associated with a file)
 *   length  (or Intplus null)
 */
static void function_file_length(Execute ptr, addr stream)
{
	file_length_stream(stream, &stream);
	setresult_control(ptr, stream);
}

static void type_file_length(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Stream);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, IntplusNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_file_length(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_LENGTH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_file_length);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_length(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-position (stream &optional position) ...) -> result
 *   stream    stream
 *   position  (or index (eql :start) (eql :end))
 *   result    index-null
 */
static void function_file_position(Execute ptr, addr stream, addr pos)
{
	int result;
	addr check;
	size_t size;

	/* get file-position */
	if (pos == Unbound) {
		if (file_position_stream(stream, &size))
			setresult_control(ptr, Nil);
		else
			setresult_control(ptr, intsizeh(size));
		return;
	}

	/* set start */
	GetConst(KEYWORD_START, &check);
	if (pos == check) {
		result = file_position_start_stream(stream);
		setbool_control(ptr, ! result);
		return;
	}

	/* set end */
	GetConst(KEYWORD_END, &check);
	if (pos == check) {
		result = file_position_end_stream(stream);
		setbool_control(ptr, ! result);
		return;
	}

	/* set index */
	getindex_error(pos, &size);
	result = file_position_set_stream(stream, size);
	setbool_control(ptr, ! result);
}

static void type_file_position(addr *ret)
{
	addr arg, values, type, type2, type3;

	/* position-designer */
	GetTypeTable(&type, Index);
	GetConst(KEYWORD_START, &type2);
	type_eql_heap(type2, &type2);
	GetConst(KEYWORD_END, &type3);
	type_eql_heap(type3, &type3);
	type3or_heap(type, type2, type3, &type);
	/* type */
	GetTypeTable(&arg, Stream);
	typeargs_var1opt1(&arg, arg, type);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_file_position(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_POSITION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_file_position);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_position(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun file-string-length (stream object) ...) -> length
 *   stream  output-stream
 *   object  (or string character)
 *   length  index-null
 */
static void function_file_string_length(Execute ptr, addr stream, addr pos)
{
	int check;
	unicode u;
	size_t size;

	if (characterp(pos)) {
		GetCharacter(pos, &u);
		check = file_character_length_stream(stream, u, &size);
	}
	else {
		check = file_string_length_stream(stream, pos, &size);
	}
	setresult_control(ptr, check? Nil: intsizeh(size));
}

static void type_file_string_length(addr *ret)
{
	addr arg, values, type;

	GetTypeTable(&arg, OutputStream);
	GetTypeTable(&type, String);
	GetTypeTable(&values, Character);
	type2or_heap(type, values, &values);
	typeargs_var2(&arg, arg, values);
	GetTypeValues(&values, IndexNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_file_string_length(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FILE_STRING_LENGTH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_file_string_length);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_file_string_length(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun open (filespec
 *     &key direction element-type if-exists if-does-not-exist external-format)
 *     ...) -> (or stream null)
 */
static enum Stream_Open_Direction function_open_direction(addr value)
{
	addr check;

	/* default */
	if (value == Unbound)
		return Stream_Open_Direction_Input;

	/* :input */
	GetConst(KEYWORD_INPUT, &check);
	if (value == check)
		return Stream_Open_Direction_Input;

	/* :output */
	GetConst(KEYWORD_OUTPUT, &check);
	if (value == check)
		return Stream_Open_Direction_Output;

	/* :io */
	GetConst(KEYWORD_IO, &check);
	if (value == check)
		return Stream_Open_Direction_Io;

	/* :probe */
	GetConst(KEYWORD_PROBE, &check);
	if (value == check)
		return Stream_Open_Direction_Probe;

	/* error */
	fmte("Invalid :direction value ~S.", value, NULL);
	return Stream_Open_Direction_Input;
}

static enum Stream_Open_Element function_open_element(Execute ptr, addr value)
{
	int validp;
	addr check, type;

	/* default */
	if (value == Unbound)
		return Stream_Open_Element_Character;

	/* :default */
	GetConst(KEYWORD_DEFAULT, &check);
	if (value == check)
		return Stream_Open_Element_Character;

	/* unsigned-byte */
	GetConst(COMMON_UNSIGNED_BYTE, &check);
	if (value == check)
		return Stream_Open_Element_Binary;

	/* character */
	if (! parse_type(ptr, &check, value, Nil)) {
		GetTypeTable(&type, Character);
		if (subtypep_clang(check, type, &validp))
			return Stream_Open_Element_Character;

		/* Binary */
		GetTypeTable(&type, Unsigned8);
		if (subtypep_clang(check, type, &validp))
			return Stream_Open_Element_Binary;
	}

	/* error */
	fmte("Invalid :element-type value ~S.", value, NULL);
	return Stream_Open_Element_Character;
}

static enum Stream_Open_IfExists function_open_ifexists(addr value, addr pos)
{
	addr check;

	/* default */
	if (value == Unbound) {
		GetPathname(pos, PATHNAME_INDEX_VERSION, &value);
		GetConst(KEYWORD_NEWEST, &check);
		return value == check?
			Stream_Open_IfExists_NewVersion:
			Stream_Open_IfExists_Error;
	}

	/* :error */
	GetConst(KEYWORD_APPEND, &check);
	if (value == check)
		return Stream_Open_IfExists_Error;

	/* :supersede */
	GetConst(KEYWORD_SUPERSEDE, &check);
	if (value == check)
		return Stream_Open_IfExists_Supersede;

	/* :append */
	GetConst(KEYWORD_APPEND, &check);
	if (value == check)
		return Stream_Open_IfExists_Append;

	/* :overwrite */
	GetConst(KEYWORD_OVERWRITE, &check);
	if (value == check)
		return Stream_Open_IfExists_Overwrite;

	/* :rename */
	GetConst(KEYWORD_RENAME, &check);
	if (value == check)
		return Stream_Open_IfExists_Rename;

	/* :rename-and-delete */
	GetConst(KEYWORD_RENAME_AND_DELETE, &check);
	if (value == check)
		return Stream_Open_IfExists_RenameAndDelete;

	/* :new-version */
	GetConst(KEYWORD_NEW_VERSION, &check);
	if (value == check)
		return Stream_Open_IfExists_NewVersion;

	/* nil */
	if (value == Nil)
		return Stream_Open_IfExists_Nil;

	/* others */
	fmte("Invalid :if-exists value ~S.", value, NULL);
	return Stream_Open_IfExists_Error;
}

static enum Stream_Open_IfDoesNot function_open_ifdoesnot(addr value,
		enum Stream_Open_Direction direction,
		enum Stream_Open_IfExists exists)
{
	addr check;

	/* default */
	if (value == Unbound) {
		/* :input     -> :error
		 * :overwrite -> :error
		 * :append    -> :error
		 * :output    -> :create
		 * :io        -> :create
		 * :probe     -> nil
		 */
		if (direction == Stream_Open_Direction_Input)
			return Stream_Open_IfDoesNot_Error;
		if (exists == Stream_Open_IfExists_Overwrite)
			return Stream_Open_IfDoesNot_Error;
		if (exists == Stream_Open_IfExists_Append)
			return Stream_Open_IfDoesNot_Error;
		if (direction == Stream_Open_Direction_Output)
			return Stream_Open_IfDoesNot_Create;
		if (direction == Stream_Open_Direction_Io)
			return Stream_Open_IfDoesNot_Create;
		if (direction == Stream_Open_Direction_Probe)
			return Stream_Open_IfDoesNot_Nil;
		fmte("Invalid :if-does-not-exist default value.", NULL);
		return Stream_Open_IfDoesNot_Error;
	}

	/* :error */
	GetConst(KEYWORD_ERROR, &check);
	if (value == check)
		return Stream_Open_IfDoesNot_Error;

	/* :create */
	GetConst(KEYWORD_CREATE, &check);
	if (value == check)
		return Stream_Open_IfDoesNot_Create;

	/* nil */
	if (value == Nil)
		return Stream_Open_IfDoesNot_Nil;

	/* others */
	fmte("Invalid :if-does-not-exist value ~S.", value, NULL);
	return Stream_Open_IfDoesNot_Error;
}

static int function_open_string(addr value, const char *str1, const char *str2)
{
	if (symbolp(value))
		GetNameSymbol(value, &value);
	if (! stringp(value))
		return 0;
	if (string_equalp_char(value, str1))
		return 1;
	if (str2 == NULL)
		return 0;
	return string_equalp_char(value, str2);
}

static enum Stream_Open_External function_open_external(addr value)
{
	addr check;

	/* default */
	if (value == Unbound)
		return Stream_Open_External_Utf8;

	/* :default */
	GetConst(KEYWORD_DEFAULT, &check);
	if (value == check)
		return Stream_Open_External_Default;

	/* ascii */
	if (function_open_string(value, "ASC", "ASCII"))
		return Stream_Open_External_Ascii;

	/* utf8 */
	if (function_open_string(value, "UTF8", "UTF-8"))
		return Stream_Open_External_Utf8;

	/* utf8-bom */
	if (function_open_string(value, "UTF8BOM", "UTF-8-BOM"))
		return Stream_Open_External_Utf8Bom;

	/* utf16 */
	if (function_open_string(value, "UTF16", "UTF-16"))
		return Stream_Open_External_Utf16;

	/* utf16le */
	if (function_open_string(value, "UTF16LE", "UTF-16LE"))
		return Stream_Open_External_Utf16Le;

	/* utf16be */
	if (function_open_string(value, "UTF16BE", "UTF-16BE"))
		return Stream_Open_External_Utf16Be;

	/* utf16le-bom */
	if (function_open_string(value, "UTF16LEBOM", "UTF-16LE-BOM"))
		return Stream_Open_External_Utf16LeBom;

	/* utf16be-bom */
	if (function_open_string(value, "UTF16BEBOM", "UTF-16BE-BOM"))
		return Stream_Open_External_Utf16BeBom;

	/* utf32 */
	if (function_open_string(value, "UTF32", "UTF-32"))
		return Stream_Open_External_Utf32;

	/* utf32le */
	if (function_open_string(value, "UTF32LE", "UTF-32LE"))
		return Stream_Open_External_Utf32Le;

	/* utf32be */
	if (function_open_string(value, "UTF32BE", "UTF-32BE"))
		return Stream_Open_External_Utf32Be;

	/* utf32le-bom */
	if (function_open_string(value, "UTF32LEBOM", "UTF-32LE-BOM"))
		return Stream_Open_External_Utf32LeBom;

	/* utf32be-bom */
	if (function_open_string(value, "UTF32BEBOM", "UTF-32BE-BOM"))
		return Stream_Open_External_Utf32BeBom;

	/* others */
	fmte("Invalid external-format ~S.", value, NULL);
	return Stream_Open_External_Utf8;
}

static void function_open(Execute ptr, addr pos, addr rest)
{
	addr value;
	enum Stream_Open_Direction direction;
	enum Stream_Open_Element element;
	enum Stream_Open_IfExists exists;
	enum Stream_Open_IfDoesNot doesnot;
	enum Stream_Open_External external;

	/* argument */
	physical_pathname_heap(ptr, pos, &pos);
	if (getkeyargs(rest, KEYWORD_DIRECTION, &value)) value = Unbound;
	direction = function_open_direction(value);
	if (getkeyargs(rest, KEYWORD_ELEMENT_TYPE, &value)) value = Unbound;
	element = function_open_element(ptr, value);
	if (getkeyargs(rest, KEYWORD_IF_EXISTS, &value)) value = Unbound;
	exists = function_open_ifexists(value, pos);
	if (getkeyargs(rest, KEYWORD_IF_DOES_NOT_EXIST, &value)) value = Unbound;
	doesnot = function_open_ifdoesnot(value, direction, exists);
	if (getkeyargs(rest, KEYWORD_EXTERNAL_FORMAT, &value)) value = Unbound;
	external = function_open_external(value);

	/* result */
	open_stream(ptr, &pos, pos, direction, element, exists, doesnot, external);
	setresult_control(ptr, pos);
}

static void type_open(addr *ret)
{
	addr arg, values, key, key1, key2, key3, key4, key5;

	/* key */
	KeyTypeTable(&key1, DIRECTION, OpenDirection);
	KeyTypeTable(&key2, ELEMENT_TYPE, OpenElementType);
	KeyTypeTable(&key3, IF_EXISTS, OpenIfExists);
	KeyTypeTable(&key4, IF_DOES_NOT_EXIST, OpenIfDoesNotExist);
	KeyTypeTable(&key5, EXTERNAL_FORMAT, ExternalFormat);
	list_heap(&key, key1, key2, key3, key4, key5, NULL);
	/* type */
	GetTypeTable(&arg, PathnameDesigner);
	typeargs_var1key(&arg, arg, key);
	GetTypeValues(&values, StreamNull);
	type_compiled_heap(arg, values, ret);
}

static void defun_open(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_OPEN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_open);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_open(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun stream-external-format (stream) ...) -> format
 *   stream  file-stream
 *   format  external-format-designer
 */
static void function_stream_external_format(Execute ptr, addr stream)
{
	external_format_file(stream, &stream);
	setresult_control(ptr, stream);
}

static void type_stream_external_format(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, FileStream);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, ExternalFormat);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_stream_external_format(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STREAM_EXTERNAL_FORMAT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_stream_external_format);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_stream_external_format(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-open-file ((stream filespec &rest options) ...) -> result */
static void function_with_open_file(Execute ptr, addr form, addr env)
{
	/* (let ((var (open file . args)))
	 *   ,@decl
	 *   (unwind-protect
	 *     (progn . body)
	 *     (close var)))
	 */
	addr args, var, file, body, decl, root;
	addr let, open, protect, progn, close;

	/* argument */
	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &args, &body);
	if (! consp(args)) goto error;
	GetCons(args, &var, &args);
	if (! consp(args)) goto error;
	GetCons(args, &file, &args);
	declare_body_form(body, &decl, &body);

	/* expand */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_OPEN, &open);
	GetConst(COMMON_UNWIND_PROTECT, &protect);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_CLOSE, &close);
	list_heap(&close, close, var, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&protect, protect, progn, close, NULL);
	lista_heap(&args, open, file, args, NULL);
	list_heap(&args, var, args, NULL);
	conscar_heap(&args, args);
	conscar_heap(&root, let);
	cons_heap(&root, args, root);
	while (decl != Nil) {
		GetCons(decl, &var, &decl);
		cons_heap(&root, var, root);
	}
	cons_heap(&root, protect, root);
	nreverse_list_unsafe(&root, root);
	setresult_control(ptr, root);
	return;

error:
	fmte("WITH-OPEN-FILE argument must be "
			"a ((var file options*) ...) form.", form, NULL);
}

static void defmacro_with_open_file(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_OPEN_FILE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_open_file);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun close (stream &key abort) ...) -> result */
static void function_close(Execute ptr, addr pos, addr rest)
{
	int check;
	addr abort;

	if (getkeyargs(rest, KEYWORD_DIRECTION, &abort)) abort = Nil;
	check = close_abort_stream(pos, abort != Nil);
	setbool_control(ptr, check);
}

static void type_close(addr *ret)
{
	addr arg, values, key;

	/* key */
	KeyTypeTable(&key, ABORT, T);
	conscar_heap(&key, key);
	/* type */
	GetTypeTable(&arg, Stream);
	typeargs_var1key(&arg, arg, key);
	GetTypeValues(&values, T); /* any */
	type_compiled_heap(arg, values, ret);
}

static void defun_close(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CLOSE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_close);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_close(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-open-stream ((var stream) decl* form*) ...) -> result */
static void function_with_open_stream(Execute ptr, addr form, addr env)
{
	/* `(let ((,var ,stream))
	 *   ,@decl
	 *   (unwind-protect
	 *     (progn ,@body)
	 *     (close ,var)))
	 */
	addr args, var, stream, body, decl, root;
	addr let, protect, progn, close;

	/* argument */
	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &args, &body);
	if (! consp(args)) goto error;
	GetCons(args, &var, &args);
	if (! consp(args)) goto error;
	GetCons(args, &stream, &args);
	if (args != Nil) goto error;
	declare_body_form(body, &decl, &body);

	/* expand */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_UNWIND_PROTECT, &protect);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_CLOSE, &close);
	list_heap(&close, close, var, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&protect, protect, progn, close, NULL);
	list_heap(&args, var, stream, NULL);
	conscar_heap(&args, args);
	conscar_heap(&root, let);
	cons_heap(&root, args, root);
	while (decl != Nil) {
		GetCons(decl, &var, &decl);
		cons_heap(&root, var, root);
	}
	cons_heap(&root, protect, root);
	nreverse_list_unsafe(&root, root);
	setresult_control(ptr, root);
	return;

error:
	fmte("WITH-OPEN-STREAM argument must be "
			"a ((var stream) ...) form.", form, NULL);
}

static void defun_with_open_stream(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_OPEN_STREAM, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_open_stream);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun listen (input-stream) ...) -> boolean */
static void function_listen(Execute ptr, addr stream)
{
	int check;

	if (stream == Unbound)
		standard_input_stream(ptr, &stream);
	check = listen_stream(stream);
	setbool_control(ptr, check);
}

static void type_listen(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, InputStream);
	typeargs_opt1(&arg, arg);
	GetTypeValues(&values, Boolean);
	type_compiled_heap(arg, values, ret);
}

static void defun_listen(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_LISTEN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_listen);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_listen(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun clear-input (&optional input-stream) ...) -> null */
static void function_clear_input(Execute ptr, addr stream)
{
	if (stream == Unbound)
		standard_input_stream(ptr, &stream);
	clear_input_stream(stream);
	setresult_control(ptr, Nil);
}

static void type_clear_input(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, InputStream);
	typeargs_opt1(&arg, arg);
	GetTypeValues(&values, Null);
	type_compiled_heap(arg, values, ret);
}

static void defun_clear_input(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CLEAR_INPUT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_clear_input);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_clear_input(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun finish-output (&optional output-stream) ...) -> null */
static void function_finish_output(Execute ptr, addr stream)
{
	finish_output_stream(stream);
	setresult_control(ptr, Nil);
}

static void defun_finish_output(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FINISH_OUTPUT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_finish_output);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FinishOutput);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* (defun force-output (&optional output-stream) ...) -> null */
static void function_force_output(Execute ptr, addr stream)
{
	force_output_stream(stream);
	setresult_control(ptr, Nil);
}

static void defun_force_output(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FORCE_OUTPUT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_force_output);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FinishOutput);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun clear-output (&optional output-stream) ...) -> null */
static void function_clear_output(Execute ptr, addr stream)
{
	clear_output_stream(stream);
	setresult_control(ptr, Nil);
}

static void defun_clear_output(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CLEAR_OUTPUT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_clear_output);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FinishOutput);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun y-or-n-p (&optional control &rest args) ...) -> boolean
 *   control  (or string null)
 *   args     &rest t
 *   boolean  boolean
 */
static void function_y_or_n_p(Execute ptr, addr args)
{
	int check;
	if (yes_or_no_p_common(ptr, args, 0, &check)) return;
	setbool_control(ptr, check);
}

static void defun_y_or_n_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_Y_OR_N_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_y_or_n_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, YesOrNoP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun yes-or-no-p (&optional control &rest args) ...) -> boolean
 *   control  (or string null)
 *   args     &rest t
 *   boolean  boolean
 */
static void function_yes_or_no_p(Execute ptr, addr args)
{
	int check;
	if (yes_or_no_p_common(ptr, args, 1, &check)) return;
	setbool_control(ptr, check);
}

static void defun_yes_or_no_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_YES_OR_NO_P, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_yes_or_no_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, YesOrNoP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-synonym-stream (symbol) ...) -> synonym-stream */
static void function_make_synonym_stream(Execute ptr, addr var)
{
	open_synonym_stream(&var, var);
	setresult_control(ptr, var);
}

static void type_make_synonym_stream(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, Symbol);
	GetTypeTable(&values, SynonymStream);
	typeargs_var1(&arg, arg);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_synonym_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_SYNONYM_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_make_synonym_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_synonym_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun synonym-stream-symbol (synonym-stream) ...) -> symbol */
static void function_synonym_stream_symbol(Execute ptr, addr var)
{
	get_synonym_stream(var, &var);
	setresult_control(ptr, var);
}

static void type_synonym_stream_symbol(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, SynonymStream);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, Symbol);
	type_compiled_heap(arg, values, ret);
}

static void defun_synonym_stream_symbol(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SYNONYM_STREAM_SYMBOL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_synonym_stream_symbol);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_synonym_stream_symbol(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-broadcast-stream (&rest streams) ...) -> broadcast-stream
 *   streams  (&rest (satisfies output-steram-p))
 */
static void function_make_broadcast_stream(Execute ptr, addr var)
{
	open_broadcast_stream(&var, var);
	setresult_control(ptr, var);
}

static void type_make_broadcast_stream(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, OutputStream);
	typeargs_rest(&arg, arg);
	GetTypeTable(&values, BroadcastStream);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_broadcast_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_BROADCAST_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_rest(pos, p_defun_make_broadcast_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_broadcast_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun broadcast-stream-streams (broadcast-stream) ...) -> list */
static void function_broadcast_stream_streams(Execute ptr, addr var)
{
	get_broadcast_stream(var, &var);
	setresult_control(ptr, var);
}

static void type_broadcast_stream_streams(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, BroadcastStream);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_broadcast_stream_streams(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BROADCAST_STREAM_STREAMS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_broadcast_stream_streams);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_broadcast_stream_streams(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-two-way-stream (input output) ...) -> two-way-stream */
static void function_make_two_way_stream(Execute ptr, addr input, addr output)
{
	open_twoway_stream(&input, input, output);
	setresult_control(ptr, input);
}

static void type_make_two_way_stream(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, InputStream);
	GetTypeTable(&values, OutputStream);
	typeargs_var2(&arg, arg, values);
	GetTypeTable(&values, TwoWayStream);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_two_way_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_TWO_WAY_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_make_two_way_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_two_way_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun two-way-stream-input-stream (two-way-stream) ...) -> input-stream */
static void function_two_way_stream_input_stream(Execute ptr, addr var)
{
	get_twoway_input_stream(var, &var);
	setresult_control(ptr, var);
}

static void type_two_way_stream_input_stream(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, TwoWayStream);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, InputStream);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_two_way_stream_input_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TWO_WAY_STREAM_INPUT_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_two_way_stream_input_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_two_way_stream_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun two-way-stream-output-stream (two-way-stream) ...) -> output-stream */
static void function_two_way_stream_output_stream(Execute ptr, addr var)
{
	get_twoway_output_stream(var, &var);
	setresult_control(ptr, var);
}

static void type_two_way_stream_output_stream(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, TwoWayStream);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, OutputStream);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_two_way_stream_output_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_TWO_WAY_STREAM_OUTPUT_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_two_way_stream_output_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_two_way_stream_output_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-echo-stream (input output) ...) -> echo-stream */
static void function_make_echo_stream(Execute ptr, addr input, addr output)
{
	open_echo_stream(&input, input, output);
	setresult_control(ptr, input);
}

static void type_make_echo_stream(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, InputStream);
	GetTypeTable(&values, OutputStream);
	typeargs_var2(&arg, arg, values);
	GetTypeTable(&values, EchoStream);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_echo_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_ECHO_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_make_echo_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_echo_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun echo-stream-input-stream (echo-stream) ...) -> input-stream */
static void function_echo_stream_input_stream(Execute ptr, addr var)
{
	get_echo_input_stream(var, &var);
	setresult_control(ptr, var);
}

static void type_echo_stream_input_stream(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, EchoStream);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, InputStream);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_echo_stream_input_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ECHO_STREAM_INPUT_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_echo_stream_input_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_echo_stream_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun echo-stream-output-stream (echo-stream) ...) -> output-stream */
static void function_echo_stream_output_stream(Execute ptr, addr var)
{
	get_echo_output_stream(var, &var);
	setresult_control(ptr, var);
}

static void type_echo_stream_output_stream(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, EchoStream);
	typeargs_var1(&arg, arg);
	GetTypeTable(&values, InputStream);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_echo_stream_output_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ECHO_STREAM_OUTPUT_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_echo_stream_output_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_echo_stream_output_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-concatenated-stream (&rest streams) ...) -> concatenated-stream
 *   streams  (&rest (satisfies output-steram-p))
 */
static void function_make_concatenated_stream(Execute ptr, addr var)
{
	open_concatenated_stream(&var, var);
	setresult_control(ptr, var);
}

static void type_make_concatenated_stream(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, InputStream);
	typeargs_rest(&arg, arg);
	GetTypeTable(&values, ConcatenatedStream);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_concatenated_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_CONCATENATED_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_rest(pos, p_defun_make_concatenated_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_concatenated_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun concatenated-stream-streams (concatenated-stream) ...) -> list */
static void function_concatenated_stream_streams(Execute ptr, addr var)
{
	get_concatenated_stream(var, &var);
	setresult_control(ptr, var);
}

static void type_concatenated_stream_streams(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, ConcatenatedStream);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, List);
	type_compiled_heap(arg, values, ret);
}

static void defun_concatenated_stream_streams(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONCATENATED_STREAM_STREAMS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_concatenated_stream_streams);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_concatenated_stream_streams(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-string-input-stream (string &optional start end) ...) -> stream
 *   string  string
 *   start   keyword-start
 *   end     keyword-end
 *   stream  string-stream
 */
static void function_make_string_input_stream(Execute ptr, addr var, addr rest)
{
	size_t start, end;

	string_length(var, &start);
	keyword_start_end(start, rest, &start, &end);
	open_input_string_stream2(&var, var, start, end);
	setresult_control(ptr, var);
}

static void type_make_string_input_stream(addr *ret)
{
	addr arg, values, key;

	/* key */
	KeyTypeTable(&arg, START, KeywordStart);
	KeyTypeTable(&values, END, KeywordEnd);
	list_heap(&key, arg, values, NULL);
	/* type */
	GetTypeTable(&arg, String);
	typeargs_var1key(&arg, arg, key);
	GetTypeTable(&values, StringStream);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_string_input_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_STRING_INPUT_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_string_input_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_string_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-string-output-stream (&key element-type) ...) -> string-stream */
static void function_make_string_output_stream(Execute ptr, addr rest)
{
	int validp;
	addr type, pos;

	if (! getkeyargs(rest, KEYWORD_ELEMENT_TYPE, &pos)) {
		GetTypeTable(&type, Character);
		if (parse_type(ptr, &pos, pos, Nil))
			return;
		if (! subtypep_clang(pos, type, &validp))
			fmte(":ELEMENT-TYPE ~S must be a character type.", pos, NULL);
	}
	open_output_string_stream(&pos, 0);
	setresult_control(ptr, pos);
}

static void type_make_string_output_stream(addr *ret)
{
	addr arg, values;

	/* key */
	KeyTypeTable(&arg, ELEMENT_TYPE, Symbol);
	list_heap(&arg, arg, NULL);
	/* type */
	typeargs_key(&arg, arg);
	GetTypeTable(&values, StringStream);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_make_string_output_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_STRING_OUTPUT_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_make_string_output_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_string_output_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-output-stream-string (string-stream) ...) -> simple-string */
static void function_get_output_stream_string(Execute ptr, addr var)
{
	addr type;

	if (getstreamtype(var) != StreamType_StringOutput) {
		GetTypeTable(&type, StringStream);
		type_error_stdarg(var, type,
				"The stream must be a output-string-stream.", NULL);
	}
	string_stream_heap(var, &var);
	setresult_control(ptr, var);
}

static void type_get_output_stream_string(addr *ret)
{
	addr arg, values;

	GetTypeTable(&arg, StringStream);
	typeargs_var1(&arg, arg);
	GetTypeValues(&values, SimpleString);
	type_compiled_heap(arg, values, ret);
}

static void defun_get_output_stream_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_GET_OUTPUT_STREAM_STRING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_get_output_stream_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_get_output_stream_string(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-input-from-string (var string &key index start end) decl* form*
 *   var     symbol
 *   string  string
 *   index   t
 *   start   keyword-start
 *   end     keyword-end
 */
static void with_input_from_string_noindex(addr *ret,
		addr var, addr string, addr args, addr body)
{
	/* `(let ((,var (make-string-input-stream ,string :start ,start :end ,end)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@form)
	 *      (close ,var)))
	 */
	addr let, make, unwind, progn, close, decl, pos;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAKE_STRING_INPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_CLOSE, &close);
	declare_body_form(body, &decl, &body);
	list_heap(&close, close, var, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&unwind, unwind, progn, close, NULL);
	lista_heap(&make, make, string, args, NULL);
	list_heap(&make, var, make, NULL);
	conscar_heap(&make, make);
	conscar_heap(&let, let);
	cons_heap(&let, make, let);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&let, pos, let);
	}
	cons_heap(&let, unwind, let);
	nreverse_list_unsafe(ret, let);
}

static void with_input_from_string_index(addr *ret,
		addr var, addr string, addr index, addr args, addr body)
{
	/* `(let ((,var (make-string-input-stream ,string :start ,start :end ,end)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@form)
	 *      (setf ,index (lisp-system::end-input-stream ,var))
	 *      (close ,var)))
	 */
	addr let, make, unwind, progn, setf, end, close, decl, pos;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAKE_STRING_INPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_SETF, &setf);
	GetConst(SYSTEM_END_INPUT_STREAM, &end);
	GetConst(COMMON_CLOSE, &close);
	declare_body_form(body, &decl, &body);
	list_heap(&close, close, var, NULL);
	list_heap(&end, end, var, NULL);
	list_heap(&setf, setf, index, end, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&unwind, unwind, progn, setf, close, NULL);
	lista_heap(&make, make, string, args, NULL);
	list_heap(&make, var, make, NULL);
	conscar_heap(&make, make);
	conscar_heap(&let, let);
	cons_heap(&let, make, let);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&let, pos, let);
	}
	cons_heap(&let, unwind, let);
	nreverse_list_unsafe(ret, let);
}

static void function_with_input_from_string(Execute ptr, addr form, addr env)
{
	addr args, body, key, index, var, string;

	/* argument */
	getcdr(form, &form);
	if (! consp(form)) goto error;
	GetCons(form, &args, &body);
	if (! consp(args)) goto error;
	GetCons(args, &var, &args);
	if (! consp(args)) goto error;
	GetCons(args, &string, &args);
	/* make form */
	GetConst(KEYWORD_INDEX, &key);
	if (getplist(args, key, &index)) {
		with_input_from_string_noindex(&var, var, string, args, body);
	}
	else {
		remplist_heap(args, key, &args);
		with_input_from_string_index(&var, var, string, index, args, body);
	}
	setresult_control(ptr, var);
	return;

error:
	fmte("WITH-INPUT-FROM-STRING form ~S must be a "
			"((var string ...) &body body).", form, NULL);
}

static void defmacro_with_input_from_string(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_INPUT_FROM_STRING, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_input_from_string);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro with-output-to-string
 *     (var &optional string-form &key element-type) decl* form*)
 */
static void with_output_to_string_normal(addr *ret,
		addr var, addr args, addr body)
{
	/* `(let ((,var (make-string-output-stream ,@args)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@body
	 *             (get-output-stream-string ,var))
	 *      (close ,var)))
	 */
	addr let, make, unwind, progn, get, close, decl, pos;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAKE_STRING_OUTPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_GET_OUTPUT_STREAM_STRING, &get);
	GetConst(COMMON_CLOSE, &close);
	declare_body_form(body, &decl, &body);
	list_heap(&close, close, var, NULL);
	list_heap(&get, get, var, NULL);
	conscar_heap(&progn, progn);
	while (body != Nil) {
		GetCons(body, &pos, &body);
		cons_heap(&progn, pos, progn);
	}
	cons_heap(&progn, get, progn);
	nreverse_list_unsafe(&progn, progn);
	list_heap(&unwind, unwind, progn, close, NULL);
	lista_heap(&make, make, args, NULL);
	list_heap(&make, var, make, NULL);
	conscar_heap(&make, make);
	conscar_heap(&let, let);
	cons_heap(&let, make, let);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&let, pos, let);
	}
	cons_heap(&let, unwind, let);
	nreverse_list_unsafe(ret, let);
}

static void with_output_to_string_extend(addr *ret,
		addr var, addr string, addr args, addr body)
{
	/* `(let ((,var (lisp-system::make-extend-output-stream string ,@args)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@body)
	 *      (close ,var)))
	 */
	addr let, make, unwind, progn, close, decl, pos;

	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_MAKE_EXTEND_OUTPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_CLOSE, &close);
	declare_body_form(body, &decl, &body);
	list_heap(&close, close, var, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&unwind, unwind, progn, close, NULL);
	lista_heap(&make, make, string, args, NULL);
	list_heap(&make, var, make, NULL);
	conscar_heap(&make, make);
	conscar_heap(&let, let);
	cons_heap(&let, make, let);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&let, pos, let);
	}
	cons_heap(&let, unwind, let);
	nreverse_list_unsafe(ret, let);
}

static void function_with_output_to_string(Execute ptr, addr form, addr env)
{
	addr args, var, string, body;

	/* argument */
	getcdr(form, &args);
	if (! consp(args)) goto error;
	GetCons(args, &args, &body);
	if (! consp(args)) goto error;
	GetCons(args, &var, &args);
	if (! consp(args)) {
		string = Nil;
	}
	else {
		GetCons(args, &string, &args);
	}
	if (string == Nil) {
		with_output_to_string_normal(&var, var, args, body);
	}
	else {
		with_output_to_string_extend(&var, var, string, args, body);
	}
	setresult_control(ptr, var);
	return;

error:
	fmte("WITH-OUTPUT-TO-STRING form ~S must be a "
			"((var &optional string &key element-type) &body body).", form, NULL);
}

static void defmacro_with_output_to_string(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_OUTPUT_TO_STRING, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_output_to_string);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun stream-error-stream (condition) -> stream */
static void function_stream_error_stream(Execute ptr, addr var)
{
	stream_error_stream(var, &var);
	setresult_control(ptr, var);
}

static void type_stream_error_stream(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, StreamError);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_stream_error_stream(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STREAM_ERROR_STREAM, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_stream_error_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_stream_error_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_streams(void)
{
	SetPointerCall(defun, var1, input_stream_p);
	SetPointerCall(defun, var1, output_stream_p);
	SetPointerCall(defun, var1, interactive_stream_p);
	SetPointerCall(defun, var1, open_stream_p);
	SetPointerCall(defun, var1, streamp);
	SetPointerCall(defun, var1, stream_element_type);
	SetPointerCall(defun, var1opt2, read_byte);
	SetPointerCall(defun, var2, write_byte);
	SetPointerCall(defun, opt5, peek_char);
	SetPointerCall(defun, opt4, read_char);
	SetPointerCall(defun, opt4, read_char_no_hang);
	SetPointerCall(defun, opt1, terpri);
	SetPointerCall(defun, opt1, fresh_line);
	SetPointerCall(defun, var1opt1, unread_char);
	SetPointerCall(defun, var1opt1, write_char);
	SetPointerCall(defun, opt4, read_line);
	SetPointerCall(defun, var1dynamic, write_string);
	SetPointerCall(defun, var1dynamic, write_line);
	SetPointerCall(defun, var2dynamic, read_sequence);
	SetPointerCall(defun, var2dynamic, write_sequence);
	SetPointerCall(defun, var1, file_length);
	SetPointerCall(defun, var1opt1, file_position);
	SetPointerCall(defun, var2, file_string_length);
	SetPointerCall(defun, var1dynamic, open);
	SetPointerCall(defun, var1, stream_external_format);
	SetPointerCall(defmacro, macro, with_open_file);
	SetPointerCall(defun, var1dynamic, close);
	SetPointerCall(defmacro, macro, with_open_stream);
	SetPointerCall(defun, opt1, listen);
	SetPointerCall(defun, opt1, clear_input);
	SetPointerCall(defun, opt1, finish_output);
	SetPointerCall(defun, opt1, force_output);
	SetPointerCall(defun, opt1, clear_output);
	SetPointerCall(defun, dynamic, y_or_n_p);
	SetPointerCall(defun, dynamic, yes_or_no_p);
	SetPointerCall(defun, var1, make_synonym_stream);
	SetPointerCall(defun, var1, synonym_stream_symbol);
	SetPointerCall(defun, rest, make_broadcast_stream);
	SetPointerCall(defun, var1, broadcast_stream_streams);
	SetPointerCall(defun, var2, make_two_way_stream);
	SetPointerCall(defun, var1, two_way_stream_input_stream);
	SetPointerCall(defun, var1, two_way_stream_output_stream);
	SetPointerCall(defun, var2, make_echo_stream);
	SetPointerCall(defun, var1, echo_stream_input_stream);
	SetPointerCall(defun, var1, echo_stream_output_stream);
	SetPointerCall(defun, rest, make_concatenated_stream);
	SetPointerCall(defun, var1, concatenated_stream_streams);
	SetPointerCall(defun, var1dynamic, make_string_input_stream);
	SetPointerCall(defun, dynamic, make_string_output_stream);
	SetPointerCall(defun, var1, get_output_stream_string);
	SetPointerCall(defmacro, macro, with_input_from_string);
	SetPointerCall(defmacro, macro, with_output_to_string);
	SetPointerCall(defun, var1, stream_error_stream);
}

_g void build_common_streams(void)
{
	defun_input_stream_p();
	defun_output_stream_p();
	defun_interactive_stream_p();
	defun_open_stream_p();
	defun_streamp();
	defun_stream_element_type();
	defun_read_byte();
	defun_write_byte();
	defun_peek_char();
	defun_read_char();
	defun_read_char_no_hang();
	defun_terpri();
	defun_fresh_line();
	defun_unread_char();
	defun_write_char();
	defun_read_line();
	defun_write_string();
	defun_write_line();
	defun_read_sequence();
	defun_write_sequence();
	defun_file_length();
	defun_file_position();
	defun_file_string_length();
	defun_open();
	defun_stream_external_format();
	defmacro_with_open_file();
	defun_close();
	defun_with_open_stream();
	defun_listen();
	defun_clear_input();
	defun_finish_output();
	defun_force_output();
	defun_clear_output();
	defun_y_or_n_p();
	defun_yes_or_no_p();
	defun_make_synonym_stream();
	defun_synonym_stream_symbol();
	defun_make_broadcast_stream();
	defun_broadcast_stream_streams();
	defun_make_two_way_stream();
	defun_two_way_stream_input_stream();
	defun_two_way_stream_output_stream();
	defun_make_echo_stream();
	defun_echo_stream_input_stream();
	defun_echo_stream_output_stream();
	defun_make_concatenated_stream();
	defun_concatenated_stream_streams();
	defun_make_string_input_stream();
	defun_make_string_output_stream();
	defun_get_output_stream_string();
	defmacro_with_input_from_string();
	defmacro_with_output_to_string();
	defun_stream_error_stream();
}

