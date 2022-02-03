#include "bignum.h"
#include "call_streams.h"
#include "character.h"
#include "common_header.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_object.h"
#include "condition.h"
#include "declare.h"
#include "integer.h"
#include "object.h"
#include "pathname_object.h"
#include "pathname.h"
#include "sequence.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "stream_open.h"
#include "stream_string.h"
#include "strtype.h"
#include "subtypep.h"
#include "type_parse.h"

/*
 *  read-byte
 */
int read_byte_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr *ret)
{
	int check;

	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;

	Return(read_byte_stream_(stream, ret, &check));
	if (! check)
		return 0;

	/* EOF */
	if (errorp != Nil) {
		*ret = Nil;
		return call_end_of_file_(ptr, stream);
	}
	return Result(ret, value);
}


/*
 *  write-byte
 */
int write_byte_common_(Execute ptr, addr value, addr stream)
{
	Return(write_byte_stream_(stream, value));
	return exitpoint_stream_(stream);
}


/*
 *  peek-char
 */
int peek_char_common_(Execute ptr, addr type, addr stream,
		addr errorp, addr value, addr recp, addr *ret)
{
	if (type == Unbound)
		type = Nil;
	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	return peek_char_stream_(ptr, ret,
			type, stream, errorp != Nil, value, recp != Nil);
}


/*
 *  read-char
 */
static int call_end_of_file_recursive_(Execute ptr, addr pos, int recp)
{
	return call_end_of_file_(ptr, pos);
}

int read_char_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr recp, addr *ret)
{
	int check;
	unicode c;

	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	Return(input_stream_designer_(ptr, stream, &stream));

	/* read-char */
	Return(read_char_stream_(stream, &c, &check));
	if (check) {
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
int read_char_no_hang_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr recp, addr *ret)
{
	int hang, check;
	unicode c;

	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	Return(input_stream_designer_(ptr, stream, &stream));

	/* read-char */
	Return(read_hang_stream_(stream, &c, &hang, &check));
	if (check) {
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
int terpri_common_(Execute ptr, addr stream)
{
	if (stream == Unbound) {
		Return(standard_output_stream_(ptr, &stream));
	}
	else {
		Return(output_stream_designer_(ptr, stream, &stream));
	}
	Return(terpri_stream_(stream));
	return exitpoint_stream_(stream);
}


/*
 *  fresh-line
 */
int fresh_line_common_(Execute ptr, addr stream, addr *ret)
{
	int check;

	if (stream == Unbound) {
		Return(standard_output_stream_(ptr, &stream));
	}
	else {
		Return(output_stream_designer_(ptr, stream, &stream));
	}
	Return(fresh_line_stream_(stream, &check));
	Return(exitpoint_stream_(stream));

	return Result(ret, check? T: Nil);
}


/*
 *  unread-char
 */
int unread_char_common_(Execute ptr, addr pos, addr stream)
{
	unicode c;

	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	else {
		Return(input_stream_designer_(ptr, stream, &stream));
	}
	GetCharacter(pos, &c);
	return unread_char_stream_(stream, c);
}


/*
 *  write-char
 */
int write_char_common_(Execute ptr, addr pos, addr stream)
{
	unicode c;

	Return(output_stream_designer_(ptr, stream, &stream));
	GetCharacter(pos, &c);
	Return(write_char_stream_(stream, c));
	return exitpoint_stream_(stream);
}


/*
 *  read-line
 */
int read_line_common_(Execute ptr,
		addr stream, addr errorp, addr value, addr recp,
		addr *ret, addr *sec)
{
	int miss;

	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	if (errorp == Unbound)
		errorp = T;
	if (value == Unbound)
		value = Nil;
	if (recp == Unbound)
		recp = Nil;
	Return(read_line_stream_(ptr, ret,
				&miss, stream, errorp != Nil, value, recp != Nil));
	*sec = miss? T: Nil;

	return 0;
}


/*
 *  write-string
 */
int write_string_common_(Execute ptr, addr string, addr rest)
{
	Return(write_string_stream_(ptr, string, rest, &string));
	return exitpoint_stream_(string);
}


/*
 *  write-line
 */
int write_line_common_(Execute ptr, addr string, addr rest)
{
	Return(write_string_stream_(ptr, string, rest, &string));
	Return(terpri_stream_(string));
	return exitpoint_stream_(string);
}


/*
 *  read-sequence
 */
int read_sequence_common_(addr var, addr stream, addr rest, addr *ret)
{
	size_t start, end;

	Return(length_sequence_(var, 0, &start));
	Return(keyword_start_end_(start, rest, &start, &end));
	return read_sequence_stream_(ret, var, stream, start, end);
}


/*
 *  write-sequence
 */
int write_sequence_common_(LocalRoot local, addr var, addr stream, addr rest)
{
	size_t start, end;

	Return(length_sequence_(var, 0, &start));
	Return(keyword_start_end_(start, rest, &start, &end));
	Return(write_sequence_stream_(local, var, stream, start, end));
	return exitpoint_stream_(stream);
}


/*
 *  file-position
 */
int file_position_common_(Execute ptr, addr stream, addr pos, addr *ret)
{
	int check;
	addr value;
	size_t size;

	/* get file-position */
	if (pos == Unbound) {
		Return(file_position_stream_(stream, &size, &check));
		if (check)
			return Result(ret, Nil);
		else
			return Result(ret, intsizeh(size));
	}

	/* set start */
	GetConst(KEYWORD_START, &value);
	if (pos == value) {
		Return(file_position_start_stream_(stream, &check));
		goto return_result;
	}

	/* set end */
	GetConst(KEYWORD_END, &value);
	if (pos == value) {
		Return(file_position_end_stream_(stream, &check));
		goto return_result;
	}

	/* set index */
	Return(getindex_integer_(pos, &size));
	Return(file_position_set_stream_(stream, size, &check));

return_result:
	return Result(ret, check? Nil: T);
}


/*
 *  file-string-length
 */
int file_string_length_common_(addr stream, addr pos, addr *ret)
{
	int check;
	unicode c;
	size_t size;

	if (characterp(pos)) {
		GetCharacter(pos, &c);
		Return(file_charlen_stream_(stream, c, &size, &check));
	}
	else {
		Return(file_strlen_stream_(stream, pos, &size, &check));
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
static int open_common_direction_(addr value, enum Stream_Open_Direction *ret)
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

static int open_common_newest_p(addr pos)
{
	addr version, x, y;

	/* version == :newest */
	GetVersionPathname(pos, &version);
	GetConst(KEYWORD_NEWEST, &y);
	if (version == y)
		return 1;

	/* logical-pathname.version == Nil */
	GetHostPathname(pos, &x);
	if (! stringp(x))
		return 0;

	return version == Nil;
}

static int open_common_ifexists_(addr value, addr pos, enum Stream_Open_IfExists *ret)
{
	addr check;

	/* default */
	if (value == Unbound) {
		/* binary-stream */
		if (streamp(pos))
			return Result(ret, Stream_Open_IfExists_Supersede);
		/* pathname */
		return Result(ret, open_common_newest_p(pos)?
				Stream_Open_IfExists_NewVersion:
				Stream_Open_IfExists_Error);
	}

	/* :error */
	GetConst(KEYWORD_ERROR, &check);
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

static int open_common_ifdoesnot_(addr value,
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

static int open_pathname_designer_(Execute ptr, addr pos, addr *ret)
{
	addr value;

	if (memory_stream_p(pos))
		return Result(ret, pos);

	if (streamp(pos)) {
		GetPathnameStream(pos, &value);
		if (memory_stream_p(value))
			return Result(ret, value);
	}

	return pathname_designer_heap_(ptr, pos, ret);
}

int open_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	addr value;
	enum Stream_Open_Direction direction;
	enum Stream_Open_Element element;
	enum Stream_Open_IfExists exists;
	enum Stream_Open_IfDoesNot doesnot;
	enum Stream_Open_External external;

	/* argument */
	Return(open_pathname_designer_(ptr, pos, &pos));
	if (GetKeyArgs(rest, KEYWORD_DIRECTION, &value))
		value = Unbound;
	Return(open_common_direction_(value, &direction));
	if (GetKeyArgs(rest, KEYWORD_ELEMENT_TYPE, &value))
		value = Unbound;
	Return(open_element_stream_(ptr, value, &element));
	if (GetKeyArgs(rest, KEYWORD_IF_EXISTS, &value))
		value = Unbound;
	Return(open_common_ifexists_(value, pos, &exists));
	if (GetKeyArgs(rest, KEYWORD_IF_DOES_NOT_EXIST, &value))
		value = Unbound;
	Return(open_common_ifdoesnot_(value, direction, exists, &doesnot));
	if (GetKeyArgs(rest, KEYWORD_EXTERNAL_FORMAT, &value))
		value = Unbound;
	Return(open_external_format_(ptr, value, &external));
	if (external == Stream_Open_External_Error)
		return fmte_("Invalid external-format ~S.", value, NULL);

	/* result */
	return open_stream_(ptr, ret, pos, direction, element, exists, doesnot, external);
}


/*
 *  with-open-file
 */
int with_open_file_common_(addr form, addr *ret)
{
	/* (let ((var (open file . args)))
	 *   ,@decl
	 *   (unwind-protect
	 *     (progn . body)
	 *     (when var  ;; valid check.
	 *       (close var))))
	 */
	addr args, var, file, body, decl, root;
	addr let, open, protect, progn, when, close;

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
	Return(declare_body_form_(body, &decl, &body));

	/* expand */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_OPEN, &open);
	GetConst(COMMON_UNWIND_PROTECT, &protect);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_WHEN, &when);
	GetConst(COMMON_CLOSE, &close);
	list_heap(&close, close, var, NULL);
	list_heap(&when, when, var, close, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&protect, protect, progn, when, NULL);
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
	nreverse(ret, root);
	return 0;

error:
	return fmte_("WITH-OPEN-FILE argument must be "
			"a ((var file options*) ...) form.", form, NULL);
}


/*
 *  close
 */
int close_common_(Execute ptr, addr pos, addr rest, addr *ret)
{
	addr abort;

	if (GetKeyArgs(rest, KEYWORD_ABORT, &abort))
		abort = Nil;
	if (abort != Nil) {
		GetConst(SYSTEM_CLOSE_ABORT, &abort);
		pushspecial_control(ptr, abort, T);
	}

	return close_stream_(pos, ret);
}


/*
 *  with-open-stream
 */
int with_open_stream_common_(addr form, addr *ret)
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
	Return(declare_body_form_(body, &decl, &body));

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
	nreverse(ret, root);
	return 0;

error:
	return fmte_("WITH-OPEN-STREAM argument must be "
			"a ((var stream) ...) form.", form, NULL);
}


/*
 *  listen
 */
int listen_common_(Execute ptr, addr stream, addr *ret)
{
	int check;

	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	Return(listen_stream_(stream, &check));

	return Result(ret, check? T: Nil);
}


/*
 *  clear-input
 */
int clear_input_common_(Execute ptr, addr stream)
{
	if (stream == Unbound) {
		Return(standard_input_stream_(ptr, &stream));
	}
	return clear_input_stream_(stream);
}


/*
 *  finish-output
 */
int finish_output_common_(Execute ptr, addr stream)
{
	if (stream == Unbound) {
		Return(standard_output_stream_(ptr, &stream));
	}
	return finish_output_stream_(stream);
}


/*
 *  force-output
 */
int force_output_common_(Execute ptr, addr stream)
{
	if (stream == Unbound) {
		Return(standard_output_stream_(ptr, &stream));
	}
	return force_output_stream_(stream);
}


/*
 *  clear-output
 */
int clear_output_common_(Execute ptr, addr stream)
{
	if (stream == Unbound) {
		Return(standard_output_stream_(ptr, &stream));
	}
	return clear_output_stream_(stream);
}


/*
 *  make-string-input-stream
 */
int make_string_input_stream_common_(addr var, addr x, addr y, addr *ret)
{
	size_t start, end;

	string_length(var, &start);
	Return(keyword_start_end_value_(start, x, y, &start, &end));
	Return(open_input_string_stream2_(ret, var, start, end));

	return 0;
}


/*
 *  make-string-output-stream
 */
int make_string_output_stream_common_(Execute ptr, addr rest, addr *ret)
{
	int check;
	addr type, pos;

	if (! GetKeyArgs(rest, KEYWORD_ELEMENT_TYPE, &pos)) {
		GetTypeTable(&type, Character);
		Return(parse_type_(ptr, &pos, pos, Nil));
		Return(subtypep_check_(ptr, pos, type, Nil, &check, NULL));
		if (! check)
			return fmte_(":ELEMENT-TYPE ~S must be a character type.", pos, NULL);
	}
	open_output_string_stream(ret, 0);

	return 0;
}


/*
 * get-output-stream-string
 */
int get_output_stream_string_common_(Execute ptr, addr var, addr *ret)
{
	addr type;

	if (getstreamtype(var) != StreamType_StringOutput) {
		GetTypeTable(&type, StringStream);
		return call_type_error_va_(ptr, var, type,
				"The stream must be a output-string-stream.", NULL);
	}

	Return(string_stream_heap_(var, ret));
	clear_output_string_stream(var);
	return 0;
}


/*
 *  with-input-from-string
 */
static int with_input_from_string_noindex_common_(addr *ret,
		addr var, addr string, addr start, addr end, addr body)
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
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&close, close, var, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&unwind, unwind, progn, close, NULL);
	if (end == Unbound)
		list_heap(&make, make, string, start, NULL);
	else
		list_heap(&make, make, string, start, end, NULL);
	list_heap(&make, var, make, NULL);
	conscar_heap(&make, make);
	conscar_heap(&let, let);
	cons_heap(&let, make, let);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&let, pos, let);
	}
	cons_heap(&let, unwind, let);
	nreverse(ret, let);

	return 0;
}

static int with_input_from_string_index_common_(addr *ret,
		addr var, addr string, addr index, addr start, addr end, addr body)
{
	/* `(let ((,var (make-string-input-stream ,string :start ,start :end ,end)))
	 *    ,@decl
	 *    (unwind-protect
	 *      (progn ,@form)
	 *      (setf ,index (lisp-system::end-input-stream ,var))
	 *      (close ,var)))
	 */
	addr let, make, unwind, progn, setf, end_input, close, decl, pos;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_MAKE_STRING_INPUT_STREAM, &make);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_SETF, &setf);
	GetConst(SYSTEM_END_INPUT_STREAM, &end_input);
	GetConst(COMMON_CLOSE, &close);
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&close, close, var, NULL);
	list_heap(&end_input, end_input, var, NULL);
	list_heap(&setf, setf, index, end_input, NULL);
	cons_heap(&progn, progn, body);
	list_heap(&unwind, unwind, progn, setf, close, NULL);
	if (end == Unbound)
		list_heap(&make, make, string, start, NULL);
	else
		list_heap(&make, make, string, start, end, NULL);
	list_heap(&make, var, make, NULL);
	conscar_heap(&make, make);
	conscar_heap(&let, let);
	cons_heap(&let, make, let);
	while (decl != Nil) {
		GetCons(decl, &pos, &decl);
		cons_heap(&let, pos, let);
	}
	cons_heap(&let, unwind, let);
	nreverse(ret, let);

	return 0;
}

static int with_input_from_string_key_(addr list, addr *start, addr *end, addr *index)
{
	addr key, value, kstart, kend, kindex;

	GetConst(KEYWORD_INDEX, &kindex);
	GetConst(KEYWORD_START, &kstart);
	GetConst(KEYWORD_END, &kend);
	*start = *end = *index = Unbound;
	while (list != Nil) {
		Return_getcons(list, &key, &list);
		Return_getcons(list, &value, &list);
		if (key == kindex) {
			if (*index == Unbound)
				*index = value;
		}
		else if (key == kstart) {
			if (*start == Unbound)
				*start = value;
		}
		else if (key == kend) {
			if (*end == Unbound)
				*end = value;
		}
		else {
			return fmte_("Invaild key argument ~S.", key, NULL);
		}
	}
	if (*start == Unbound)
		fixnum_heap(start, 0);

	return 0;
}

int with_input_from_string_common_(addr form, addr *ret)
{
	addr args, body, var, string, start, end, index;

	/* argument */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &args, &body))
		goto error;
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(args, &string, &args))
		goto error;
	/* make form */
	Return(with_input_from_string_key_(args, &start, &end, &index));
	if (index == Unbound) {
		return with_input_from_string_noindex_common_(ret,
				var, string, start, end, body);
	}
	else {
		return with_input_from_string_index_common_(ret,
				var, string, index, start, end, body);
	}

error:
	return fmte_("WITH-INPUT-FROM-STRING form ~S must be a "
			"((var string ...) &body body).", form, NULL);
}


/*
 *  with-output-to-string
 */
static int with_output_to_string_normal_common_(addr *ret,
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
	Return(declare_body_form_(body, &decl, &body));
	list_heap(&close, close, var, NULL);
	list_heap(&get, get, var, NULL);
	conscar_heap(&progn, progn);
	while (body != Nil) {
		GetCons(body, &pos, &body);
		cons_heap(&progn, pos, progn);
	}
	cons_heap(&progn, get, progn);
	nreverse(&progn, progn);
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
	nreverse(ret, let);

	return 0;
}

static int with_output_to_string_extend_common_(addr *ret,
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
	Return(declare_body_form_(body, &decl, &body));
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
	nreverse(ret, let);

	return 0;
}

int with_output_to_string_common_(addr form, addr *ret)
{
	addr args, var, string, body;

	/* argument */
	Return_getcdr(form, &args);
	if (! consp_getcons(args, &args, &body))
		goto error;
	if (! consp_getcons(args, &var, &args))
		goto error;
	if (! consp_getcons(args, &string, &args))
		string = Nil;
	if (string == Nil) {
		Return(with_output_to_string_normal_common_(ret, var, args, body));
	}
	else {
		Return(with_output_to_string_extend_common_(ret, var, string, args, body));
	}
	return 0;

error:
	return fmte_("WITH-OUTPUT-TO-STRING form ~S must be a "
			"((var &optional string &key element-type) &body body).", form, NULL);
}

