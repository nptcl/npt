#include "bignum.h"
#include "character.h"
#include "common_header.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_object.h"
#include "condition.h"
#include "eval_declare.h"
#include "integer.h"
#include "object.h"
#include "pathname.h"
#include "sequence.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_string.h"
#include "strtype.h"
#include "type_parse.h"
#include "type_subtypep.h"

/*
 *  read-byte
 */
_g int read_byte_common(Execute ptr, addr stream, addr errorp, addr value, addr *ret)
{
	byte c;

	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (read_byte_stream(stream, &c)) {
		if (errorp != Nil)
			return call_end_of_file_(ptr, stream);
		else
			return Result(ret, value);
	}
	fixnum_heap(ret, (fixnum)c);
	return 0;
}


/*
 *  write-byte
 */
_g int write_byte_common(Execute ptr, addr value, addr stream)
{
	addr pos;
	fixnum c;

	if (GetFixnum_signed(value, &c) || c < 0 || 0xFF < c) {
		GetConst(STREAM_BINARY_TYPE, &pos);
		return call_type_error_(ptr, value, pos);
	}
	write_byte_stream(stream, (byte)c);
	exitpoint_stream(stream);

	return 0;
}


/*
 *  peek-char
 */
_g int peek_char_common(Execute ptr, addr type, addr stream,
		addr errorp, addr value, addr recp, addr *ret)
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
	peek_char_stream(ptr, ret, type, stream, errorp != Nil, value, recp != Nil);

	return 0;
}


/*
 *  read-char
 */
static int call_end_of_file_recursive_(Execute ptr, addr pos, int recp)
{
	if (recp) {
		return fmte_("The stream ~S "
				"reach end-of-file, but recursive-p is true.", pos, NULL);
	}
	else {
		return call_end_of_file_(ptr, pos);
	}
}

_g int read_char_common(Execute ptr,
		addr stream, addr errorp, addr value, addr recp, addr *ret)
{
	unicode c;

	if (stream == Unbound)
		standard_input_stream(ptr, &stream);
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	stream_designer(ptr, stream, &stream, 1);

	/* read-char */
	if (read_char_stream(stream, &c)) {
		if (errorp != Nil)
			return call_end_of_file_recursive_(ptr, stream, recp != Nil);
		else
			return Result(ret, value);
	}
	character_heap(ret, c);
	return 0;
}


/*
 *  read-char-no-hang
 */
_g int read_char_no_hang_common(Execute ptr,
		addr stream, addr errorp, addr value, addr recp, addr *ret)
{
	int hang;
	unicode c;

	if (stream == Unbound)
		standard_input_stream(ptr, &stream);
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	stream_designer(ptr, stream, &stream, 1);

	/* read-char */
	if (read_hang_stream(stream, &c, &hang)) {
		if (errorp != Nil)
			return call_end_of_file_recursive_(ptr, stream, recp != Nil);
		else
			return Result(ret, value);
	}
	if (hang)
		return Result(ret, Nil);
	character_heap(ret, c);
	return 0;
}


/*
 *  terpri
 */
_g int terpri_common(Execute ptr, addr stream)
{
	if (stream == Unbound)
		standard_output_stream(ptr, &stream);
	terpri_stream(stream);
	exitpoint_stream(stream);

	return 0;
}


/*
 *  fresh-line
 */
_g int fresh_line_common(Execute ptr, addr stream, addr *ret)
{
	int check;

	if (stream == Unbound)
		standard_output_stream(ptr, &stream);
	check = fresh_line_stream(stream);
	exitpoint_stream(stream);

	return Result(ret, check? T: Nil);
}


/*
 *  unread-char
 */
_g void unread_char_common(Execute ptr, addr pos, addr stream)
{
	unicode c;

	if (stream == Unbound)
		standard_output_stream(ptr, &stream);
	GetCharacter(pos, &c);
	unread_char_stream(stream, c);
}


/*
 *  write-char
 */
_g void write_char_common(Execute ptr, addr pos, addr stream)
{
	unicode c;

	stream_designer(ptr, stream, &stream, 0);
	GetCharacter(pos, &c);
	write_char_stream(stream, c);
	exitpoint_stream(stream);
}


/*
 *  read-line
 */
_g int read_line_common(Execute ptr,
		addr stream, addr errorp, addr value, addr recp,
		addr *ret, addr *sec)
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
	read_line_stream(ptr, ret, &miss, stream, errorp != Nil, value, recp != Nil);
	*sec = miss? T: Nil;

	return 0;
}


/*
 *  write-string
 */
_g int write_string_common(Execute ptr, addr string, addr rest)
{
	Return(write_string_stream(ptr, string, rest, &string));
	exitpoint_stream(string);
	return 0;
}


/*
 *  write-line
 */
_g int write_line_common(Execute ptr, addr string, addr rest)
{
	Return(write_string_stream(ptr, string, rest, &string));
	terpri_stream(string);
	exitpoint_stream(string);
	return 0;
}


/*
 *  read-sequence
 */
_g int read_sequence_common(addr var, addr stream, addr rest, addr *ret)
{
	size_t start, end;

	start = length_sequence(var, 0);
	Return(keyword_start_end_(start, rest, &start, &end));
	return read_sequence_stream(ret, var, stream, start, end);
}


/*
 *  write-sequence
 */
_g int write_sequence_common(LocalRoot local, addr var, addr stream, addr rest)
{
	size_t start, end;

	start = length_sequence(var, 0);
	Return(keyword_start_end_(start, rest, &start, &end));
	Return(write_sequence_stream(local, var, stream, start, end));
	exitpoint_stream(stream);

	return 0;
}


/*
 *  file-position
 */
_g int file_position_common(Execute ptr, addr stream, addr pos, addr *ret)
{
	int result;
	addr check;
	size_t size;

	/* get file-position */
	if (pos == Unbound) {
		if (file_position_stream(stream, &size))
			return Result(ret, Nil);
		else
			return Result(ret, intsizeh(size));
	}

	/* set start */
	GetConst(KEYWORD_START, &check);
	if (pos == check) {
		result = file_position_start_stream(stream);
		goto return_result;
	}

	/* set end */
	GetConst(KEYWORD_END, &check);
	if (pos == check) {
		result = file_position_end_stream(stream);
		goto return_result;
	}

	/* set index */
	getindex_integer(pos, &size);
	result = file_position_set_stream(stream, size);

return_result:
	return Result(ret, result? Nil: T);
}


/*
 *  file-string-length
 */
_g int file_string_length_common(addr stream, addr pos, addr *ret)
{
	int check;
	unicode c;
	size_t size;

	if (characterp(pos)) {
		GetCharacter(pos, &c);
		check = file_character_length_stream(stream, c, &size);
	}
	else {
		check = file_string_length_stream(stream, pos, &size);
	}
	if (check) {
		return Result(ret, Nil);
	}
	else {
		make_index_integer_heap(&pos, size);
		return Result(ret, pos);
	}
}


/*
 *  open
 */
static int open_common_direction(addr value, enum Stream_Open_Direction *ret)
{
	addr check;

	/* default */
	if (value == Unbound)
		return Result(ret, Stream_Open_Direction_Input);

	/* :input */
	GetConst(KEYWORD_INPUT, &check);
	if (value == check)
		return Result(ret, Stream_Open_Direction_Input);

	/* :output */
	GetConst(KEYWORD_OUTPUT, &check);
	if (value == check)
		return Result(ret, Stream_Open_Direction_Output);

	/* :io */
	GetConst(KEYWORD_IO, &check);
	if (value == check)
		return Result(ret, Stream_Open_Direction_Io);

	/* :probe */
	GetConst(KEYWORD_PROBE, &check);
	if (value == check)
		return Result(ret, Stream_Open_Direction_Probe);

	/* error */
	*ret = Stream_Open_Direction_Input;
	return fmte_("Invalid :direction value ~S.", value, NULL);
}

static int open_common_element(Execute ptr, addr value, enum Stream_Open_Element *ret)
{
	int validp;
	addr check, type;

	/* default */
	if (value == Unbound)
		return Result(ret, Stream_Open_Element_Character);

	/* :default */
	GetConst(KEYWORD_DEFAULT, &check);
	if (value == check)
		return Result(ret, Stream_Open_Element_Character);

	/* unsigned-byte */
	GetConst(COMMON_UNSIGNED_BYTE, &check);
	if (value == check)
		return Result(ret, Stream_Open_Element_Binary);

	/* character */
	if (! parse_type(ptr, &check, value, Nil)) {
		GetTypeTable(&type, Character);
		if (subtypep_clang(check, type, &validp))
			return Result(ret, Stream_Open_Element_Character);

		/* Binary */
		GetTypeTable(&type, Unsigned8);
		if (subtypep_clang(check, type, &validp))
			return Result(ret, Stream_Open_Element_Binary);
	}

	/* error */
	*ret = Stream_Open_Element_Character;
	return fmte_("Invalid :element-type value ~S.", value, NULL);
}

static int open_common_ifexists(addr value, addr pos, enum Stream_Open_IfExists *ret)
{
	addr check;

	/* default */
	if (value == Unbound) {
		GetPathname(pos, PATHNAME_INDEX_VERSION, &value);
		GetConst(KEYWORD_NEWEST, &check);
		return Result(ret, value == check?
				Stream_Open_IfExists_NewVersion:
				Stream_Open_IfExists_Error);
	}

	/* :error */
	GetConst(KEYWORD_APPEND, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_Error);

	/* :supersede */
	GetConst(KEYWORD_SUPERSEDE, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_Supersede);

	/* :append */
	GetConst(KEYWORD_APPEND, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_Append);

	/* :overwrite */
	GetConst(KEYWORD_OVERWRITE, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_Overwrite);

	/* :rename */
	GetConst(KEYWORD_RENAME, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_Rename);

	/* :rename-and-delete */
	GetConst(KEYWORD_RENAME_AND_DELETE, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_RenameAndDelete);

	/* :new-version */
	GetConst(KEYWORD_NEW_VERSION, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfExists_NewVersion);

	/* nil */
	if (value == Nil)
		return Result(ret, Stream_Open_IfExists_Nil);

	/* others */
	*ret = Stream_Open_IfExists_Error;
	return fmte_("Invalid :if-exists value ~S.", value, NULL);
}

static int open_common_ifdoesnot(addr value,
		enum Stream_Open_Direction direction,
		enum Stream_Open_IfExists exists,
		enum Stream_Open_IfDoesNot *ret)
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
			return Result(ret, Stream_Open_IfDoesNot_Error);
		if (exists == Stream_Open_IfExists_Overwrite)
			return Result(ret, Stream_Open_IfDoesNot_Error);
		if (exists == Stream_Open_IfExists_Append)
			return Result(ret, Stream_Open_IfDoesNot_Error);
		if (direction == Stream_Open_Direction_Output)
			return Result(ret, Stream_Open_IfDoesNot_Create);
		if (direction == Stream_Open_Direction_Io)
			return Result(ret, Stream_Open_IfDoesNot_Create);
		if (direction == Stream_Open_Direction_Probe)
			return Result(ret, Stream_Open_IfDoesNot_Nil);
		*ret = Stream_Open_IfDoesNot_Error;
		return fmte_("Invalid :if-does-not-exist default value.", NULL);
	}

	/* :error */
	GetConst(KEYWORD_ERROR, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfDoesNot_Error);

	/* :create */
	GetConst(KEYWORD_CREATE, &check);
	if (value == check)
		return Result(ret, Stream_Open_IfDoesNot_Create);

	/* nil */
	if (value == Nil)
		return Result(ret, Stream_Open_IfDoesNot_Nil);

	/* others */
	*ret = Stream_Open_IfDoesNot_Error;
	return fmte_("Invalid :if-does-not-exist value ~S.", value, NULL);
}

static int open_common_string(addr value, const char *str1, const char *str2)
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

static int open_common_external(addr value, enum Stream_Open_External *ret)
{
	addr check;

	/* default */
	if (value == Unbound)
		return Result(ret, Stream_Open_External_Utf8);

	/* :default */
	GetConst(KEYWORD_DEFAULT, &check);
	if (value == check)
		return Result(ret, Stream_Open_External_Default);

	/* ascii */
	if (open_common_string(value, "ASC", "ASCII"))
		return Result(ret, Stream_Open_External_Ascii);

	/* utf8 */
	if (open_common_string(value, "UTF8", "UTF-8"))
		return Result(ret, Stream_Open_External_Utf8);

	/* utf8-bom */
	if (open_common_string(value, "UTF8BOM", "UTF-8-BOM"))
		return Result(ret, Stream_Open_External_Utf8Bom);

	/* utf16 */
	if (open_common_string(value, "UTF16", "UTF-16"))
		return Result(ret, Stream_Open_External_Utf16);

	/* utf16le */
	if (open_common_string(value, "UTF16LE", "UTF-16LE"))
		return Result(ret, Stream_Open_External_Utf16Le);

	/* utf16be */
	if (open_common_string(value, "UTF16BE", "UTF-16BE"))
		return Result(ret, Stream_Open_External_Utf16Be);

	/* utf16le-bom */
	if (open_common_string(value, "UTF16LEBOM", "UTF-16LE-BOM"))
		return Result(ret, Stream_Open_External_Utf16LeBom);

	/* utf16be-bom */
	if (open_common_string(value, "UTF16BEBOM", "UTF-16BE-BOM"))
		return Result(ret, Stream_Open_External_Utf16BeBom);

	/* utf32 */
	if (open_common_string(value, "UTF32", "UTF-32"))
		return Result(ret, Stream_Open_External_Utf32);

	/* utf32le */
	if (open_common_string(value, "UTF32LE", "UTF-32LE"))
		return Result(ret, Stream_Open_External_Utf32Le);

	/* utf32be */
	if (open_common_string(value, "UTF32BE", "UTF-32BE"))
		return Result(ret, Stream_Open_External_Utf32Be);

	/* utf32le-bom */
	if (open_common_string(value, "UTF32LEBOM", "UTF-32LE-BOM"))
		return Result(ret, Stream_Open_External_Utf32LeBom);

	/* utf32be-bom */
	if (open_common_string(value, "UTF32BEBOM", "UTF-32BE-BOM"))
		return Result(ret, Stream_Open_External_Utf32BeBom);

	/* others */
	*ret = Stream_Open_External_Utf8;
	return fmte_("Invalid external-format ~S.", value, NULL);
}

_g int open_common(Execute ptr, addr pos, addr rest, addr *ret)
{
	addr value;
	enum Stream_Open_Direction direction;
	enum Stream_Open_Element element;
	enum Stream_Open_IfExists exists;
	enum Stream_Open_IfDoesNot doesnot;
	enum Stream_Open_External external;

	/* argument */
	physical_pathname_heap(ptr, pos, &pos);
	if (getkeyargs(rest, KEYWORD_DIRECTION, &value))
		value = Unbound;
	Return(open_common_direction(value, &direction));
	if (getkeyargs(rest, KEYWORD_ELEMENT_TYPE, &value))
		value = Unbound;
	Return(open_common_element(ptr, value, &element));
	if (getkeyargs(rest, KEYWORD_IF_EXISTS, &value))
		value = Unbound;
	Return(open_common_ifexists(value, pos, &exists));
	if (getkeyargs(rest, KEYWORD_IF_DOES_NOT_EXIST, &value))
		value = Unbound;
	Return(open_common_ifdoesnot(value, direction, exists, &doesnot));
	if (getkeyargs(rest, KEYWORD_EXTERNAL_FORMAT, &value))
		value = Unbound;
	Return(open_common_external(value, &external));

	/* result */
	open_stream(ptr, ret, pos, direction, element, exists, doesnot, external);
	return 0;
}


/*
 *  with-open-file
 */
_g int with_open_file_common(addr form, addr *ret)
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
	Return_getcdr(form, &form);
	if (! consp(form))
		goto error;
	GetCons(form, &args, &body);
	if (! consp(args))
		goto error;
	GetCons(args, &var, &args);
	if (! consp(args))
		goto error;
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
	nreverse_list_unsafe(ret, root);
	return 0;

error:
	return fmte_("WITH-OPEN-FILE argument must be "
			"a ((var file options*) ...) form.", form, NULL);
}


/*
 *  close
 */
_g int close_common(Execute ptr, addr pos, addr rest, addr *ret)
{
	addr abort;

	if (getkeyargs(rest, KEYWORD_ABORT, &abort))
		abort = Nil;
	if (abort != Nil) {
		GetConst(SYSTEM_CLOSE_ABORT, &abort);
		pushspecial_control(ptr, abort, T);
	}
	*ret = close_stream(pos)? T: Nil;

	return 0;
}


/*
 *  with-open-stream
 */
_g int with_open_stream_common(addr form, addr *ret)
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
	Return_getcdr(form, &form);
	if (! consp(form))
		goto error;
	GetCons(form, &args, &body);
	if (! consp(args))
		goto error;
	GetCons(args, &var, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &stream, &args);
	if (args != Nil)
		goto error;
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
	nreverse_list_unsafe(ret, root);
	return 0;

error:
	return fmte_("WITH-OPEN-STREAM argument must be "
			"a ((var stream) ...) form.", form, NULL);
}


/*
 *  listen
 */
_g void listen_common(Execute ptr, addr stream, addr *ret)
{
	int check;

	if (stream == Unbound)
		standard_input_stream(ptr, &stream);
	check = listen_stream(stream);
	*ret = check? T: Nil;;
}


/*
 *  clear-input
 */
_g void clear_input_common(Execute ptr, addr stream)
{
	if (stream == Unbound)
		standard_input_stream(ptr, &stream);
	clear_input_stream(stream);
}


/*
 *  make-string-input-stream
 */
_g int make_string_input_stream_common(addr var, addr rest, addr *ret)
{
	size_t start, end;

	string_length(var, &start);
	Return(keyword_start_end_(start, rest, &start, &end));
	open_input_string_stream2(ret, var, start, end);

	return 0;
}


/*
 *  make-string-output-stream
 */
_g int make_string_output_stream_common(Execute ptr, addr rest, addr *ret)
{
	int validp;
	addr type, pos;

	if (! getkeyargs(rest, KEYWORD_ELEMENT_TYPE, &pos)) {
		GetTypeTable(&type, Character);
		Return(parse_type(ptr, &pos, pos, Nil));
		if (! subtypep_clang(pos, type, &validp))
			return fmte_(":ELEMENT-TYPE ~S must be a character type.", pos, NULL);
	}
	open_output_string_stream(ret, 0);

	return 0;
}


/*
 * get-output-stream-string
 */
_g int get_output_stream_string_common(Execute ptr, addr var, addr *ret)
{
	addr type;

	if (getstreamtype(var) != StreamType_StringOutput) {
		GetTypeTable(&type, StringStream);
		return call_type_error_va_(ptr, var, type,
				"The stream must be a output-string-stream.", NULL);
	}
	string_stream_heap(var, ret);

	return 0;
}


/*
 *  with-input-from-string
 */
static void with_input_from_string_noindex_common(addr *ret,
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

static void with_input_from_string_index_common(addr *ret,
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

_g int with_input_from_string_common(addr form, addr *ret)
{
	addr args, body, key, index, var, string;

	/* argument */
	Return_getcdr(form, &form);
	if (! consp(form))
		goto error;
	GetCons(form, &args, &body);
	if (! consp(args))
		goto error;
	GetCons(args, &var, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &string, &args);
	/* make form */
	GetConst(KEYWORD_INDEX, &key);
	if (getplist(args, key, &index)) {
		with_input_from_string_noindex_common(ret, var, string, args, body);
	}
	else {
		remplist_heap(args, key, &args);
		with_input_from_string_index_common(ret, var, string, index, args, body);
	}
	return 0;

error:
	return fmte_("WITH-INPUT-FROM-STRING form ~S must be a "
			"((var string ...) &body body).", form, NULL);
}


/*
 *  with-output-to-string
 */
static void with_output_to_string_normal_common(addr *ret,
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

static void with_output_to_string_extend_common(addr *ret,
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

_g int with_output_to_string_common(addr form, addr *ret)
{
	addr args, var, string, body;

	/* argument */
	Return_getcdr(form, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &args, &body);
	if (! consp(args))
		goto error;
	GetCons(args, &var, &args);
	if (! consp(args)) {
		string = Nil;
	}
	else {
		GetCons(args, &string, &args);
	}
	if (string == Nil) {
		with_output_to_string_normal_common(ret, var, args, body);
	}
	else {
		with_output_to_string_extend_common(ret, var, string, args, body);
	}
	return 0;

error:
	return fmte_("WITH-OUTPUT-TO-STRING form ~S must be a "
			"((var &optional string &key element-type) &body body).", form, NULL);
}

