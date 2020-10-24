/*
 *  ANSI COMMON LISP: 21. Streams
 */
#include "call_streams.h"
#include "common_header.h"
#include "cons.h"
#include "file.h"
#include "prompt_for.h"
#include "stream.h"
#include "stream_broadcast.h"
#include "stream_concat.h"
#include "stream_echo.h"
#include "stream_function.h"
#include "stream_string.h"
#include "stream_synonym.h"
#include "stream_twoway.h"

/* (defun input-stream-p (stream) ...) -> boolean */
static int function_input_stream_p(Execute ptr, addr pos)
{
	int check;

	Return(inputp_stream_(pos, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_input_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INPUT_STREAM_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_input_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun output-stream-p (stream) ...) -> boolean */
static int function_output_stream_p(Execute ptr, addr pos)
{
	int check;

	Return(outputp_stream_(pos, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_output_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_OUTPUT_STREAM_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_output_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun interactive-stream-p (stream) ...) -> boolean */
static int function_interactive_stream_p(Execute ptr, addr pos)
{
	int check;

	Return(interactivep_stream_(pos, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_interactive_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INTERACTIVE_STREAM_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_interactive_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun open-stream-p (stream) ...) -> boolean */
static int function_open_stream_p(Execute ptr, addr pos)
{
	int check = open_stream_p(pos);
	setbool_control(ptr, check);
	return 0;
}

static void defun_open_stream_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_OPEN_STREAM_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_open_stream_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, InputStreamP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun streamp (object) ...) -> boolean */
static int function_streamp(Execute ptr, addr var)
{
	setbool_control(ptr, GetType(var) == LISPTYPE_STREAM);
	return 0;
}

static void defun_streamp(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STREAMP, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_streamp);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun stream-element-type (stream) ...) -> typespec  */
static int function_stream_element_type(Execute ptr, addr var)
{
	Return(element_type_stream_(var, &var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_read_byte(Execute ptr, addr stream, addr errorp, addr value)
{
	Return(read_byte_common_(ptr, stream, errorp, value, &value));
	setresult_control(ptr, value);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_write_byte(Execute ptr, addr value, addr stream)
{
	Return(write_byte_common(ptr, value, stream));
	setresult_control(ptr, value);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_peek_char(Execute ptr, addr type, addr stream,
		addr errorp, addr value, addr recp)
{
	Return(peek_char_common(ptr, type, stream, errorp, value, recp, &type));
	setresult_control(ptr, type);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_read_char(Execute ptr,
		addr stream, addr errorp, addr value, addr recp)
{
	Return(read_char_common(ptr, stream, errorp, value, recp, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void defun_read_char(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_CHAR, &symbol);
	compiled_system(&pos, symbol);
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
static int function_read_char_no_hang(Execute ptr,
		addr stream, addr errorp, addr value, addr recp)
{
	Return(read_char_no_hang_common(ptr, stream, errorp, value, recp, &stream));
	setresult_control(ptr, stream);
	return 0;
}

static void defun_read_char_no_hang(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_READ_CHAR_NO_HANG, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt4(pos, p_defun_read_char_no_hang);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, ReadChar);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun terpri (&optional stream) ...) -> null */
static int function_terpri(Execute ptr, addr stream)
{
	Return(terpri_common(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_terpri);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_terpri(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun fresh-line (&optional stream) ...) -> boolean */
static int function_fresh_line(Execute ptr, addr stream)
{
	Return(fresh_line_common(ptr, stream, &stream));
	setresult_control(ptr, stream);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_fresh_line);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_fresh_line(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun unread-char (character &optional stream) ...) -> null */
static int function_unread_char(Execute ptr, addr pos, addr stream)
{
	Return(unread_char_common(ptr, pos, stream));
	setresult_control(ptr, Nil);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_unread_char);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_unread_char(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-char (character &optional stream) ...) -> character */
static int function_write_char(Execute ptr, addr pos, addr stream)
{
	Return(write_char_common(ptr, pos, stream));
	setresult_control(ptr, pos);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_read_line(Execute ptr,
		addr stream, addr errorp, addr value, addr recp)
{
	Return(read_line_common(ptr, stream, errorp, value, recp, &stream, &errorp));
	setvalues_control(ptr, stream, errorp, NULL);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_opt4(pos, p_defun_read_line);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_line(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-string (string &optional stream &key start end) ...) -> string */
static int function_write_string(Execute ptr, addr string, addr rest)
{
	Return(write_string_common(ptr, string, rest));
	setresult_control(ptr, string);
	return 0;
}

static void defun_write_string(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_STRING, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_write_string);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, WriteString);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-line (string &optional stream &key start end) ...) -> string */
static int function_write_line(Execute ptr, addr string, addr rest)
{
	Return(write_line_common(ptr, string, rest));
	setresult_control(ptr, string);
	return 0;
}

static void defun_write_line(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WRITE_LINE, &symbol);
	compiled_system(&pos, symbol);
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
static int function_read_sequence(Execute ptr, addr var, addr stream, addr rest)
{
	Return(read_sequence_common(var, stream, rest, &var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2dynamic(pos, p_defun_read_sequence);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_read_sequence(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun write-sequence (sequence stream &key start end) ...) -> sequence */
static int function_write_sequence(Execute ptr, addr var, addr stream, addr rest)
{
	Return(write_sequence_common(ptr->local, var, stream, rest));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_file_length(Execute ptr, addr stream)
{
	Return(file_length_stream_(stream, &stream));
	setresult_control(ptr, stream);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_file_position(Execute ptr, addr stream, addr pos)
{
	Return(file_position_common(ptr, stream, pos, &stream));
	setresult_control(ptr, stream);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_file_string_length(Execute ptr, addr stream, addr pos)
{
	Return(file_string_length_common(stream, pos, &stream));
	setresult_control(ptr, stream);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_open(Execute ptr, addr pos, addr rest)
{
	Return(open_common(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_stream_external_format(Execute ptr, addr stream)
{
	external_format_file(stream, &stream);
	setresult_control(ptr, stream);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_stream_external_format);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_stream_external_format(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-open-file ((stream filespec &rest options) ...) -> result */
static int function_with_open_file(Execute ptr, addr form, addr env)
{
	Return(with_open_file_common(form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_open_file(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_OPEN_FILE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_open_file);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun close (stream &key abort) ...) -> result */
static int function_close(Execute ptr, addr pos, addr rest)
{
	Return(close_common(ptr, pos, rest, &pos));
	setresult_control(ptr, pos);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_close);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_close(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro with-open-stream ((var stream) decl* form*) ...) -> result */
static int function_with_open_stream(Execute ptr, addr form, addr env)
{
	Return(with_open_stream_common(form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defun_with_open_stream(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_OPEN_STREAM, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_open_stream);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun listen (input-stream) ...) -> boolean */
static int function_listen(Execute ptr, addr stream)
{
	Return(listen_common(ptr, stream, &stream));
	setresult_control(ptr, stream);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_listen);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_listen(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun clear-input (&optional input-stream) ...) -> null */
static int function_clear_input(Execute ptr, addr stream)
{
	Return(clear_input_common(ptr, stream));
	setresult_control(ptr, Nil);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_clear_input);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_clear_input(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun finish-output (&optional output-stream) ...) -> null */
static int function_finish_output(Execute ptr, addr stream)
{
	Return(finish_output_stream_(stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_finish_output(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FINISH_OUTPUT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_finish_output);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FinishOutput);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* (defun force-output (&optional output-stream) ...) -> null */
static int function_force_output(Execute ptr, addr stream)
{
	Return(force_output_stream_(stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_force_output(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FORCE_OUTPUT, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_opt1(pos, p_defun_force_output);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, FinishOutput);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun clear-output (&optional output-stream) ...) -> null */
static int function_clear_output(Execute ptr, addr stream)
{
	Return(clear_output_stream_(stream));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_clear_output(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CLEAR_OUTPUT, &symbol);
	compiled_system(&pos, symbol);
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
static int function_y_or_n_p(Execute ptr, addr args)
{
	int check;

	Return(yes_or_no_p_common(ptr, args, 0, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_y_or_n_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_Y_OR_N_P, &symbol);
	compiled_system(&pos, symbol);
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
static int function_yes_or_no_p(Execute ptr, addr args)
{
	int check;

	Return(yes_or_no_p_common(ptr, args, 1, &check));
	setbool_control(ptr, check);

	return 0;
}

static void defun_yes_or_no_p(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_YES_OR_NO_P, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_yes_or_no_p);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, YesOrNoP);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-synonym-stream (symbol) ...) -> synonym-stream */
static int function_make_synonym_stream(Execute ptr, addr var)
{
	Return(open_synonym_stream_(&var, var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_make_synonym_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_synonym_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun synonym-stream-symbol (synonym-stream) ...) -> symbol */
static int function_synonym_stream_symbol(Execute ptr, addr var)
{
	get_synonym_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_make_broadcast_stream(Execute ptr, addr var)
{
	Return(open_broadcast_stream_(&var, var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_rest(pos, p_defun_make_broadcast_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_broadcast_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun broadcast-stream-streams (broadcast-stream) ...) -> list */
static int function_broadcast_stream_streams(Execute ptr, addr var)
{
	get_broadcast_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_broadcast_stream_streams);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_broadcast_stream_streams(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-two-way-stream (input output) ...) -> two-way-stream */
static int function_make_two_way_stream(Execute ptr, addr input, addr output)
{
	open_twoway_stream(&input, input, output);
	setresult_control(ptr, input);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_make_two_way_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_two_way_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun two-way-stream-input-stream (two-way-stream) ...) -> input-stream */
static int function_two_way_stream_input_stream(Execute ptr, addr var)
{
	get_twoway_input_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_two_way_stream_input_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_two_way_stream_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun two-way-stream-output-stream (two-way-stream) ...) -> output-stream */
static int function_two_way_stream_output_stream(Execute ptr, addr var)
{
	get_twoway_output_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_two_way_stream_output_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_two_way_stream_output_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-echo-stream (input output) ...) -> echo-stream */
static int function_make_echo_stream(Execute ptr, addr input, addr output)
{
	open_echo_stream(&input, input, output);
	setresult_control(ptr, input);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2(pos, p_defun_make_echo_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_echo_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun echo-stream-input-stream (echo-stream) ...) -> input-stream */
static int function_echo_stream_input_stream(Execute ptr, addr var)
{
	get_echo_input_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_echo_stream_input_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_echo_stream_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun echo-stream-output-stream (echo-stream) ...) -> output-stream */
static int function_echo_stream_output_stream(Execute ptr, addr var)
{
	get_echo_output_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_make_concatenated_stream(Execute ptr, addr var)
{
	Return(open_concatenated_stream_(&var, var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_rest(pos, p_defun_make_concatenated_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_concatenated_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun concatenated-stream-streams (concatenated-stream) ...) -> list */
static int function_concatenated_stream_streams(Execute ptr, addr var)
{
	get_concatenated_stream(var, &var);
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_make_string_input_stream(Execute ptr, addr var, addr rest)
{
	Return(make_string_input_stream_common(var, rest, &var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_make_string_input_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_string_input_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun make-string-output-stream (&key element-type) ...) -> string-stream */
static int function_make_string_output_stream(Execute ptr, addr rest)
{
	Return(make_string_output_stream_common(ptr, rest, &rest));
	setresult_control(ptr, rest);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_make_string_output_stream);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_string_output_stream(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun get-output-stream-string (string-stream) ...) -> simple-string */
static int function_get_output_stream_string(Execute ptr, addr var)
{
	Return(get_output_stream_string_common(ptr, var, &var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_with_input_from_string(Execute ptr, addr form, addr env)
{
	Return(with_input_from_string_common(form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_input_from_string(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_INPUT_FROM_STRING, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_input_from_string);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro with-output-to-string
 *     (var &optional string-form &key element-type) decl* form*)
 */
static int function_with_output_to_string(Execute ptr, addr form, addr env)
{
	Return(with_output_to_string_common(form, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_output_to_string(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_OUTPUT_TO_STRING, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_output_to_string);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun stream-error-stream (condition) -> stream */
static int function_stream_error_stream(Execute ptr, addr var)
{
	Return(stream_error_stream_(var, &var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
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

