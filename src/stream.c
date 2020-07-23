#include <stdio.h>
#include <stdlib.h>
#include "bignum_output.h"
#include "character.h"
#include "character_queue.h"
#include "condition.h"
#include "common_header.h"
#include "cons.h"
#include "constant.h"
#include "eastasian_unicode.h"
#include "equal.h"
#include "file.h"
#include "file_open.h"
#include "files.h"
#include "format.h"
#include "heap.h"
#include "hold.h"
#include "integer.h"
#include "memory.h"
#include "object.h"
#include "pathname_object.h"
#include "print.h"
#include "print_write.h"
#include "reader.h"
#include "sequence.h"
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

static void stream_alloc(LocalRoot local, addr *ret, enum StreamType type, size_t size)
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
	ptr->closed = 1;
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

_g int extend_stream_p(addr stream)
{
	return streamp(stream)
		&& ((int)StreamType_Size) <= ((int)getstreamtype(stream));
}

_g int extend_type_stream_p(addr stream, int type)
{
	return streamp(stream)
		&& ((int)getstreamtype(stream)) == type;
}


/*****************************************************************************
 *  function
 *****************************************************************************/
_g void force_close_stream(addr stream)
{
	struct StructStream *ptr;

	CheckType(stream, LISPTYPE_STREAM);
	ptr = PtrStructStream(stream);
	ptr->terpri = 0;
	ptr->unread_check = 0;
	ptr->closed = 1;
}

_g int close_stream_common(addr stream, addr *ret)
{
	struct StructStream *ptr;

	CheckType(stream, LISPTYPE_STREAM);
	ptr = PtrStructStream(stream);
	Return((Stream_close[ptr->type])(stream, ret));
	force_close_stream(stream);

	return 0;
}

_g int close_stream_(addr stream)
{
	return close_stream_common(stream, &stream);
}

_g int open_stream_p(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream(stream)->closed == 0;
}

_g void force_open_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	PtrStructStream(stream)->closed = 0;
}

#define CheckStream(stream, ptr) { \
	CheckType(stream, LISPTYPE_STREAM); \
	ptr = PtrStructStream(stream); \
	if (ptr->closed) { \
		return fmte_("The stream ~S is already closed.", stream, NULL); \
	} \
}

_g int read_binary_stream_(addr stream, void *pos, size_t size, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_binary[(int)ptr->type])(stream, pos, size, ret);
}

_g int readf_binary_stream_(addr stream, void *pos, size_t size, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_readf_binary[(int)ptr->type])(stream, pos, size, ret);
}

_g int read_byte_stream_(addr stream, byte *c, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_byte[(int)ptr->type])(stream, c, ret);
}

_g int unread_byte_stream_(addr stream, byte c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_unread_byte[(int)ptr->type])(stream, c);
}

_g int write_binary_stream_(addr stream, const void *pos, size_t size, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_write_binary[(int)ptr->type])(stream, pos, size, ret);
}

_g int write_byte_stream_(addr stream, byte c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_write_byte[(int)ptr->type])(stream, c);
}

_g int read_char_stream_(addr stream, unicode *c, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_char[(int)ptr->type])(stream, c, ret);
}

_g int read_hang_stream_(addr stream, unicode *c, int *hang, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_hang[(int)ptr->type])(stream, c, hang, ret);
}

_g int unread_char_stream_(addr stream, unicode c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_unread_char[(int)ptr->type])(stream, c);
}

_g int write_char_stream_(addr stream, unicode c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_write_char[(int)ptr->type])(stream, c);
}

_g int terpri_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_terpri[(int)ptr->type])(stream);
}

_g int getleft_stream_(addr stream, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_getleft[(int)ptr->type])(stream, ret);
}

_g int setleft_stream_(addr stream, size_t value)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_setleft[(int)ptr->type])(stream, value);
}

_g int copyleft_stream_(addr stream, addr src)
{
	size_t size;
	Return(getleft_stream_(src, &size));
	return setleft_stream_(stream, size);
}

_g int fresh_line_stream_(addr stream, int *ret)
{
	int check;
	struct StructStream *ptr;

	CheckStream(stream, ptr);
	Return((Stream_fresh_line[(int)ptr->type])(stream, &check));
	if (ret)
		*ret = check;

	return 0;
}

_g int pageout_stream_(addr stream)
{
	return write_char_stream_(stream, '\f');
}

_g int clear_input_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_clear_input[(int)ptr->type])(stream);
}

_g int inputp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_inputp[GetIndexStream(stream)])(stream, ret);
}

_g int outputp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_outputp[GetIndexStream(stream)])(stream, ret);
}

_g int interactivep_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_interactivep[GetIndexStream(stream)])(stream, ret);
}

_g int characterp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_characterp[GetIndexStream(stream)])(stream, ret);
}

_g int binaryp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_binaryp[GetIndexStream(stream)])(stream, ret);
}

_g int element_type_stream_(addr stream, addr *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_element_type[(int)ptr->type])(stream, ret);
}

_g int file_length_stream_(addr stream, addr *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_length[(int)ptr->type])(stream, ret);
}

_g int file_position_stream_(addr stream, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position[(int)ptr->type])(stream, value, ret);
}

_g int file_position_start_stream_(addr stream, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_start[(int)ptr->type])(stream, ret);
}

_g int file_position_end_stream_(addr stream, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_end[(int)ptr->type])(stream, ret);
}

_g int file_position_set_stream_(addr stream, size_t value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_set[(int)ptr->type])(stream, value, ret);
}

_g int file_charlen_stream_(addr stream, unicode u, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_charlen[(int)ptr->type])(stream, u, value, ret);
}

_g int file_strlen_stream_(addr stream, addr pos, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_strlen[(int)ptr->type])(stream, pos, value, ret);
}

_g int listen_stream_(addr stream, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_listen[(int)ptr->type])(stream, ret);
}

_g int finish_output_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_finish_output[(int)ptr->type])(stream);
}

_g int force_output_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_force_output[(int)ptr->type])(stream);
}

_g int clear_output_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_clear_output[(int)ptr->type])(stream);
}

_g int exitpoint_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_exitpoint[(int)ptr->type])(stream);
}

_g int termsize_stream_(addr stream, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_termsize[(int)ptr->type])(stream, value, ret);
}


/*
 *  default
 */
_g int close_default_stream(addr stream, addr *ret)
{
	return Result(ret, T);
}

_g int read_char_default_stream(addr stream, unicode *c, int *ret)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (! ptr->unread_check)
		return read_char_file_(stream, c, ret);

	*c = ptr->unread;
	ptr->unread_check = 0;
	return Result(ret, 0);
}

_g int read_hang_default_stream(addr stream, unicode *c, int *hang, int *ret)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (! ptr->unread_check)
		return read_hang_file_(stream, c, hang, ret);

	*c = ptr->unread;
	ptr->unread_check = 0;
	*hang = 0;
	return Result(ret, 0);
}

_g int unread_char_default_stream(addr stream, unicode c)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (ptr->unread_check)
		return fmte_("unread already exists.", NULL);
	ptr->unread = c;
	ptr->unread_check = 1;

	return 0;
}

_g int write_char_default_stream(addr stream, unicode c)
{
	Return(write_char_file_(stream, c));
	charleft_default_stream(stream, c);
	return 0;
}

_g int terpri_default_stream(addr stream)
{
	Return(write_char_stream_(stream, '\n'));
	return setleft_default_stream(stream, 0);
}

_g int getleft_default_stream(addr stream, size_t *ret)
{
	*ret = PtrStructStream(stream)->terpri;
	return 0;
}

_g int setleft_default_stream(addr stream, size_t value)
{
	PtrStructStream(stream)->terpri = value;
	return 0;
}

_g void charleft_default_stream(addr stream, unicode c)
{
	struct StructStream *ptr;

	ptr = PtrStructStream(stream);
	if (c == '\n' || c == '\f')
		ptr->terpri = 0;
	else
		ptr->terpri += eastasian_width(c);
}

_g int fresh_line_default_stream(addr stream, int *ret)
{
	size_t size;

	Return(getleft_stream_(stream, &size));
	if (size == 0) {
		if (ret)
			*ret = 0;
		return 0;
	}
	Return(terpri_stream_(stream));
	if (ret)
		*ret = 1;
	return 0;
}

_g int checkp_true_stream(addr stream, int *ret)
{
	return Result(ret, 1);
}

_g int checkp_false_stream(addr stream, int *ret)
{
	return Result(ret, 0);
}

_g int element_type_character_stream(addr stream, addr *ret)
{
	GetConst(COMMON_CHARACTER, ret);
	return 0;
}

_g int element_type_binary_stream(addr stream, addr *ret)
{
	GetConst(STREAM_BINARY_TYPE, ret);
	return 0;
}

_g int element_type_io_stream(addr stream, addr *ret)
{
	int check;
	addr input, output;

	GetInputStream(stream, &input);
	GetOutputStream(stream, &output);
	Return(element_type_stream_(input, &input));
	Return(element_type_stream_(output, &output));
	Return(equal_function_(input, output, &check));
	if (check) {
		return Result(ret, input);
	}
	else {
		GetConst(COMMON_OR, &stream);
		list_heap(ret, stream, input, output, NULL);
		return 0;
	}
}

_g int file_length_default_stream(addr stream, addr *ret)
{
	int check;
	addr pos;
	size_t size;

	/* TODO: :element-type '(unsigned-byte 16) */
	Return(file_length_file_(stream, &size, &check));
	if (check) {
		return Result(ret, Nil);
	}
	else {
		make_index_integer_heap(&pos, size);
		return Result(ret, pos);
	}
}

_g int file_position_default_stream(addr stream, size_t *value, int *ret)
{
	*value = 0;
	return Result(ret, 1);
}

_g int file_position_start_default_stream(addr stream, int *ret)
{
	return Result(ret, 1);
}

_g int file_position_end_default_stream(addr stream, int *ret)
{
	return Result(ret, 1);
}

_g int file_position_set_default_stream(addr stream, size_t value, int *ret)
{
	return Result(ret, 1);
}

_g int finish_output_default_stream(addr stream)
{
	return exitpoint_stream_(stream);
}

_g int force_output_default_stream(addr stream)
{
	return exitpoint_stream_(stream);
}

_g int clear_output_default_stream(addr stream)
{
	return 0;
}

_g int exitpoint_default_stream(addr stream)
{
	return 0;
}

_g int termsize_default_stream(addr stream, size_t *value, int *ret)
{
	*value = 0;
	return Result(ret, 1);
}


/*
 *  clang
 */
_g int print_ascii_stream_(addr stream, const char *data)
{
	unicode c;

	CheckType(stream, LISPTYPE_STREAM);
	for (;;) {
		c = *(const byte *)data;
		if (c == 0)
			break;
		Return(write_char_stream_(stream, (unicode)c));
		data++;
	}

	return 0;
}

_g int print_unicode_stream_(addr stream, const unicode *data)
{
	unicode c;

	CheckType(stream, LISPTYPE_STREAM);
	for (;;) {
		c = *data;
		if (c == 0)
			break;
		Return(write_char_stream_(stream, c));
		data++;
	}

	return 0;
}

_g int print_string_stream_(addr stream, addr pos)
{
	unicode c;
	size_t size, i;

	CheckType(stream, LISPTYPE_STREAM);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &c);
		Return(write_char_stream_(stream, c));
	}

	return 0;
}


/*
 *  initialize
 */
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
	else if (stream == T)
		terminal_io_stream(ptr, ret);
	else if (stream == Nil)
		standard_output_stream(ptr, ret);
	else
		*ret = stream;
	Check(! streamp(*ret), "type error");
}


/*
 *  common-lisp
 */
static int make_empty_file_stream(Execute ptr, addr file)
{
	addr stream;

	Return(open_output_binary_stream_(ptr, &stream, file, FileOutput_supersede));
	if (stream == NULL)
		return call_file_error_(ptr, file);

	return close_stream_(stream);
}

static int probe_file_stream_(Execute ptr, addr file, int *ret)
{
#ifdef LISP_ANSI
	return Result(ret, 1);
#else
	Return(probe_file_files_(ptr, &file, file));
	return Result(ret, file != Nil);
#endif
}

static int open_if_does_not_exist_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_IfDoesNot value, int createp, int *existp)
{
	int check;

	Return(probe_file_stream_(ptr, pos, &check));
	if (check)
		return Result(existp, 0);

	switch (value) {
		case Stream_Open_IfDoesNot_Create:
			if (createp) {
				Return(make_empty_file_stream(ptr, pos));
			}
			return Result(existp, 0);

		case Stream_Open_IfDoesNot_Error:
			return call_file_error_(ptr, pos);

		case Stream_Open_IfDoesNot_Nil:
			*ret = Nil;
			return Result(existp, 1);

		default:
			return fmte_("Invalid :if-does-not-exist value.", NULL);
	}
}

static int open_if_exists_rename_stream_(Execute ptr, addr pos)
{
	int check;
	LocalRoot local;
	LocalStack stack;
	addr path, type, queue, ret1, ret2, ret3;
	size_t i;

	Return(probe_file_stream_(ptr, pos, &check));
	if (! check)
		return 0;
	/* rename */
	local = ptr->local;
	for (i = 0; ; i++) {
		push_local(local, &stack);
		/* make filename */
		copy_pathname_alloc(local, &path, pos);
		charqueue_local(local, &queue, 0);
		GetTypePathname(path, &type);
		if (stringp(type)) {
			pushstring_charqueue_local(local, queue, type);
			pushchar_charqueue_local(local, queue, ".");
		}
		make_index_integer_alloc(local, &type, i);
		Return(decimal_charqueue_integer_local_(local, type, queue));
		make_charqueue_local(local, queue, &type);
		SetTypePathname(path, type);
		/* check */
		Return(probe_file_stream_(ptr, path, &check));
		if (! check) {
			Return(rename_file_files_(ptr, &ret1, &ret2, &ret3, pos, path));
			rollback_local(local, stack);
			return 0;
		}
		rollback_local(local, stack);
	}
}

static int open_if_exists_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_IfExists value,
		enum FileOutput *mode,
		int *existp)
{
	int check;

	Return(probe_file_stream_(ptr, pos, &check));
	if (! check) {
		*mode = FileOutput_supersede;
		return Result(existp, 0);
	}
	switch (value) {
		case Stream_Open_IfExists_Error:
			*mode = FileOutput_supersede;
			return call_file_error_(ptr, pos);

		case Stream_Open_IfExists_RenameAndDelete:
		case Stream_Open_IfExists_NewVersion:
		case Stream_Open_IfExists_Supersede:
			*mode = FileOutput_supersede;
			break;

		case Stream_Open_IfExists_Rename:
			Return(open_if_exists_rename_stream_(ptr, pos));
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
			return Result(existp, 1);

		default:
			*mode = FileOutput_supersede;
			*existp = 0;
			return fmte_("Invalid :if-exist value.", NULL);
	}

	return Result(existp, 0);
}

static int open_external_input_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Default:
			return open_input_stream_(ptr, ret, pos);

		case Stream_Open_External_Ascii:
			return open_input_ascii_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf8:
			return open_input_utf8_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf8Bom:
			return open_input_utf8bom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16:
			return open_input_utf16_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16Le:
			return open_input_utf16le_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16Be:
			return open_input_utf16be_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16LeBom:
			return open_input_utf16lebom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf16BeBom:
			return open_input_utf16bebom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32:
			return open_input_utf32_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32Le:
			return open_input_utf32le_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32Be:
			return open_input_utf32be_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32LeBom:
			return open_input_utf32lebom_stream_(ptr, ret, pos);

		case Stream_Open_External_Utf32BeBom:
			return open_input_utf32bebom_stream_(ptr, ret, pos);

		default:
			*ret = Nil;
			return fmte_("Invalid :external-format value.", NULL);
	}
}

static int open_direct_input_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;
	addr stream;

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 1, &check));
	if (check)
		return 0;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Binary:
			Return(open_input_binary_stream_(ptr, &stream, pos));
			break;

		case Stream_Open_Element_Character:
			Return(open_external_input_stream_(ptr, &stream, pos, ext));
			break;

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL)
		return call_file_error_(ptr, pos);

	return Result(ret, stream);
}

static int open_external_output_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext, enum FileOutput mode)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Default:
			return open_output_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Ascii:
			return open_output_ascii_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8:
			return open_output_utf8_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf8Bom:
			return open_output_utf8_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf16:
			return open_output_utf16be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16Le:
			return open_output_utf16le_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16Be:
			return open_output_utf16be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf16LeBom:
			return open_output_utf16le_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf16BeBom:
			return open_output_utf16be_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf32:
			return open_output_utf32be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32Le:
			return open_output_utf32le_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32Be:
			return open_output_utf32be_stream_(ptr, ret, pos, mode, 0);

		case Stream_Open_External_Utf32LeBom:
			return open_output_utf32le_stream_(ptr, ret, pos, mode, 1);

		case Stream_Open_External_Utf32BeBom:
			return open_output_utf32be_stream_(ptr, ret, pos, mode, 1);

		default:
			*ret = Nil;
			return fmte_("Invalid :external-format value.", NULL);
	}
}

static int open_direct_output_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;
	enum FileOutput mode;
	addr stream;

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 0, &check));
	if (check)
		return 0;

	/* :if-exists */
	Return(open_if_exists_stream_(ptr, ret, pos, if1, &mode, &check));
	if (check)
		return 0;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Binary:
			Return(open_output_binary_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Character:
			Return(open_external_output_stream_(ptr, &stream, pos, ext, mode));
			break;

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL)
		return call_file_error_(ptr, pos);

	return Result(ret, stream);
}

static int open_external_io_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_External ext, enum FileOutput mode)
{
	/* :external-format */
	switch (ext) {
		case Stream_Open_External_Default:
			return open_io_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Ascii:
			return open_io_ascii_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8:
			return open_io_utf8_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf8Bom:
			return open_io_utf8bom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16:
			return open_io_utf16_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16Le:
			return open_io_utf16le_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16Be:
			return open_io_utf16be_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16LeBom:
			return open_io_utf16lebom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf16BeBom:
			return open_io_utf16bebom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32:
			return open_io_utf32_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32Le:
			return open_io_utf32le_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32Be:
			return open_io_utf32be_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32LeBom:
			return open_io_utf32lebom_stream_(ptr, ret, pos, mode);

		case Stream_Open_External_Utf32BeBom:
			return open_io_utf32bebom_stream_(ptr, ret, pos, mode);

		default:
			*ret = Nil;
			return fmte_("Invalid :external-format value.", NULL);
	}
}

static int open_direct_io_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	int check;
	enum FileOutput mode;
	addr stream;

	/* :if-does-not-exist */
	Return(open_if_does_not_exist_stream_(ptr, ret, pos, if2, 0, &check));
	if (check)
		return 0;

	/* :if-exists */
	Return(open_if_exists_stream_(ptr, ret, pos, if1, &mode, &check));
	if (check)
		return 0;

	/* :element-type */
	switch (type) {
		case Stream_Open_Element_Binary:
			Return(open_io_binary_stream_(ptr, &stream, pos, mode));
			break;

		case Stream_Open_Element_Character:
			Return(open_external_io_stream_(ptr, &stream, pos, ext, mode));
			break;

		default:
			*ret = Nil;
			return fmte_("Invalid :element-type value.", NULL);
	}

	/* error check */
	if (stream == NULL)
		return call_file_error_(ptr, pos);

	return Result(ret, stream);
}

static int open_direct_probe_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Element type,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	Return(open_direct_input_stream_(ptr, &pos, pos, type, if2, ext));
	if (pos != Nil) {
		Return(close_stream_(pos));
	}
	return Result(ret, pos);
}

_g int open_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Direction direction,
		enum Stream_Open_Element type,
		enum Stream_Open_IfExists if1,
		enum Stream_Open_IfDoesNot if2,
		enum Stream_Open_External ext)
{
	/* :direction */
	switch (direction) {
		case Stream_Open_Direction_Input:
			return open_direct_input_stream_(ptr, ret, pos, type, if2, ext);

		case Stream_Open_Direction_Output:
			return open_direct_output_stream_(ptr, ret, pos, type, if1, if2, ext);

		case Stream_Open_Direction_Io:
			return open_direct_io_stream_(ptr, ret, pos, type, if1, if2, ext);

		case Stream_Open_Direction_Probe:
			return open_direct_probe_stream_(ptr, ret, pos, type, if2, ext);

		default:
			*ret = Nil;
			return fmte_("Invalid direction.", NULL);
	}
}


/*
 *  read-char
 */
_g int stream_designer_(Execute ptr, addr pos, addr *ret, int inputp)
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
	if (symbolp(pos))
		getspecialcheck_local(ptr, pos, &pos);

	/* stream */
	if (streamp(pos))
		return Result(ret, pos);

	/* error */
	*ret = Nil;
	GetConst(COMMON_STREAM, &type);
	return call_type_error_(ptr, pos, type);
}

static int end_of_file_recursive_(Execute ptr, addr pos, int recp)
{
	if (recp) {
		return fmte_("The stream ~S "
				"reach end-of-file, but recursive-p is true.", pos, NULL);
	}
	else {
		return call_end_of_file_(ptr, pos);
	}
}

static int peek_char_nil_(Execute ptr, addr *ret,
		addr stream, int errorp, addr value, int recp)
{
	int check;
	unicode c;

	Return(read_char_stream_(stream, &c, &check));
	if (check) {
		if (errorp) {
			*ret = Nil;
			return end_of_file_recursive_(ptr, stream, recp);
		}
		return Result(ret, value);
	}

	Return(unread_char_stream_(stream, c));
	character_heap(ret, c);
	return 0;
}

static int peek_char_t_(Execute ptr, addr *ret,
		addr stream, int errorp, addr value, int recp)
{
	int check;
	unicode c;

	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check) {
			if (errorp) {
				*ret = Nil;
				return end_of_file_recursive_(ptr, stream, recp);
			}
			*ret = value;
			break;
		}
		if (! isSpaceUnicode(c)) {
			Return(unread_char_stream_(stream, c));
			character_heap(ret, c);
			break;
		}
	}

	return 0;
}

static int peek_char_character_(Execute ptr, addr *ret,
		addr type, addr stream, int errorp, addr value, int recp)
{
	int check;
	unicode c, v;

	GetCharacter(type, &v);
	for (;;) {
		Return(read_char_stream_(stream, &c, &check));
		if (check) {
			if (errorp) {
				*ret = Nil;
				return end_of_file_recursive_(ptr, stream, recp);
			}
			*ret = value;
			break;
		}
		if (v == c) {
			Return(peek_char_nil_(ptr, ret, stream, errorp, value, recp));
			break;
		}
	}

	return 0;
}

_g int peek_char_stream_(Execute ptr, addr *ret,
		addr type, addr stream, int errorp, addr value, int recp)
{
	Return(stream_designer_(ptr, stream, &stream, 1));
	if (type == Nil)
		return peek_char_nil_(ptr, ret, stream, errorp, value, recp);
	else if (type == T)
		return peek_char_t_(ptr, ret, stream, errorp, value, recp);
	else
		return peek_char_character_(ptr, ret, type, stream, errorp, value, recp);
}

enum EndOfLine_Mode {
	EndOfLine_Auto,
	EndOfLine_CR,
	EndOfLine_LF,
	EndOfLine_CRLF
};

static int get_end_of_line_mode_(Execute ptr, enum EndOfLine_Mode *ret)
{
	addr pos, check;

	GetConst(SYSTEM_END_OF_LINE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	/* Auto */
	GetConst(SYSTEM_AUTO, &check);
	if (check == pos)
		return Result(ret, EndOfLine_Auto);
	/* CR */
	GetConst(SYSTEM_CR, &check);
	if (check == pos)
		return Result(ret, EndOfLine_CR);
	/* LF */
	GetConst(SYSTEM_LF, &check);
	if (check == pos)
		return Result(ret, EndOfLine_LF);
	/* CRLF */
	GetConst(SYSTEM_CRLF, &check);
	if (check == pos)
		return Result(ret, EndOfLine_CRLF);
	/* error */
	*ret = EndOfLine_Auto;
	return fmte_("Invalid *end-of-line* value ~S.", pos, NULL);
}

_g int read_line_stream_(Execute ptr, addr *ret, int *miss,
		addr pos, int errorp, addr value, int recp)
{
	int docheck, check;
	enum EndOfLine_Mode mode;
	unicode c;
	addr queue;
	LocalRoot local;
	LocalStack stack;

	Return(stream_designer_(ptr, pos, &pos, 1));
	local = ptr->local;
	push_local(local, &stack);
	charqueue_local(local, &queue, 0);
	Return(get_end_of_line_mode_(ptr, &mode));
	for (docheck = 0; ; docheck = 1) {
		Return(read_char_stream_(pos, &c, &check));
		if (check)
			goto finish_eof;
		switch (mode) {
			case EndOfLine_CR:
				if (c == 0x0D)
					goto finish_value;
				break;

			case EndOfLine_LF:
				if (c == 0x0A)
					goto finish_value;
				break;

			case EndOfLine_CRLF:
				if (c == 0x0D) {
					Return(read_char_stream_(pos, &c, &check));
					if (check || c != 0x0A)
						return fmte_("Invalid CR-LF code.", NULL);
					goto finish_value;
				}
				break;

			case EndOfLine_Auto:
			default:
				if (c == 0x0A)
					goto finish_value;
				if (c == 0x0D) {
					Return(read_char_stream_(pos, &c, &check));
					if (check == 0 && c != 0x0A) {
						Return(unread_char_stream_(pos, c));
					}
					goto finish_value;
				}
				break;
		}
		push_charqueue_local(local, queue, c);
	}

finish_eof:
	if (docheck == 0)
		goto finish_error;
	make_charqueue_heap(queue, ret);
	rollback_local(local, stack);
	return Result(miss, 1);

finish_value:
	make_charqueue_heap(queue, ret);
	rollback_local(local, stack);
	return Result(miss, 0);

finish_error:
	if (errorp)
		return end_of_file_recursive_(ptr, pos, recp);
	*ret = value;
	return Result(miss, 1);
}

_g int write_string_stream(Execute ptr, addr string, addr rest, addr *ret)
{
	unicode c;
	addr stream;
	size_t size, start, end, i;

	/* argument */
	string_length(string, &size);
	if (rest == Nil) {
		Return(stream_designer_(ptr, Unbound, &stream, 0));
		start = 0;
		end = size;
	}
	else {
		Return_getcons(rest, &stream, &rest);
		Return(stream_designer_(ptr, stream, &stream, 0));
		Return(keyword_start_end_(size, rest, &start, &end));
	}

	for (i = start; i < end; i++) {
		string_getc(string, i, &c);
		Return(write_char_stream_(stream, c));
	}

	return Result(ret, stream);
}

static int read_sequence_character_(addr *ret,
		addr seq, addr stream, size_t start, size_t end)
{
	int check;
	unicode c;
	addr value;

	for (; start < end; start++) {
		Return(read_char_stream_(stream, &c, &check));
		if (check)
			break;
		character_heap(&value, c);
		setelt_sequence(seq, start, value);
	}
	make_index_integer_heap(&value, start);

	return Result(ret, value);
}

static int read_sequence_binary_(addr *ret,
		addr seq, addr stream, size_t start, size_t end)
{
	int check;
	byte c;
	addr value;

	for (; start < end; start++) {
		Return(read_byte_stream_(stream, &c, &check));
		if (check)
			break;
		fixnum_heap(&value, c);
		setelt_sequence(seq, start, value);
	}
	make_index_integer_heap(&value, start);

	return Result(ret, value);
}

_g int read_sequence_stream(addr *ret, addr seq, addr stream, size_t start, size_t end)
{
	int check;

	/* character stream */
	Return(characterp_stream_(stream, &check));
	if (check)
		return read_sequence_character_(ret, seq, stream, start, end);

	/* binary stream */
	Return(binaryp_stream_(stream, &check));
	if (check)
		return read_sequence_binary_(ret, seq, stream, start, end);

	/* error */
	return fmte_("Invalid stream ~S.", stream, NULL);
}

static int write_sequence_character_(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	unicode c;
	addr value;
	LocalStack stack;

	for (; start < end; start++) {
		push_local(local, &stack);
		getelt_sequence(local, seq, start, &value);
		if (! characterp(value))
			return TypeError_(value, CHARACTER);
		GetCharacter(value, &c);
		rollback_local(local, stack);
		Return(write_char_stream_(stream, c));
	}

	return 0;
}

static int write_sequence_binary_(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	fixnum c;
	addr value;
	LocalStack stack;

	for (; start < end; start++) {
		push_local(local, &stack);
		getelt_sequence(local, seq, start, &value);
		if (! fixnump(value))
			return TypeError_(value, INTEGER);
		GetFixnum(value, &c);
		rollback_local(local, stack);
		if (c < 0 || 0xFF < c)
			return fmte_("Invalid binary value ~S.", fixnumh(c), NULL);
		Return(write_byte_stream_(stream, (byte)c));
	}

	return 0;
}

_g int write_sequence_stream(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end)
{
	int check;

	/* character stream */
	Return(characterp_stream_(stream, &check));
	if (check)
		return write_sequence_character_(local, seq, stream, start, end);

	/* binary stream */
	Return(binaryp_stream_(stream, &check));
	if (check)
		return write_sequence_binary_(local, seq, stream, start, end);

	/* error */
	return fmte_("Invalid stream ~S.", stream, NULL);
}

_g int prompt_for_stream(Execute ptr, addr type, addr prompt, addr *ret)
{
	int result;
	addr stream, spec, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	/* output */
	query_io_stream(ptr, &stream);
	localhold_push(hold, stream);
	Return(fresh_line_stream_(stream, NULL));
	Return(princ_print(ptr, stream, prompt));
	Return(finish_output_stream_(stream));

	/* query */
	if (type != T) {
		Return(parse_type(ptr, &spec, type, Nil));
		localhold_push(hold, spec);
	}
	for (;;) {
		Return(clear_input_stream_(stream));
		Return(read_stream(ptr, stream, &result, &value));
		if (result)
			return fmte_("Can't read from *query-io* stream.", NULL);
		localhold_set(hold, 0, value);
		if (type == T)
			break;
		Return(typep_clang(ptr, value, spec, &result));
		if (result)
			break;

		format_stream(ptr, stream, "~%Please answer ~A type: ", type, NULL);
		Return(finish_output_stream_(stream));
	}
	localhold_end(hold);

	return Result(ret, value);
}

_g int yes_or_no_p_common(Execute ptr, addr args, int exactp, int *ret)
{
	int miss;
	unicode c;
	addr control, stream, pos;
	size_t size;
	LocalHold hold;

	/* argument */
	if (args == Nil) {
		control = Nil;
	}
	else {
		GetCons(args, &control, &args);
	}

	hold = LocalHold_array(ptr, 1);
	/* output */
	query_io_stream(ptr, &stream);
	localhold_push(hold, stream);

	if (control != Nil) {
		Return(fresh_line_stream_(stream, NULL));
		Return(format_lisp(ptr, stream, control, args, &control));
		Return(print_ascii_stream_(stream, " "));
	}
	Return(print_ascii_stream_(stream, exactp? "(yes or no) ": "(y or n) "));
	Return(finish_output_stream_(stream));

	/* query */
	for (;;) {
		Return(clear_input_stream_(stream));
		Return(read_line_stream_(ptr, &pos, &miss, stream, 1, Unbound, 0));
		if (pos == Unbound)
			return fmte_("*query-io* don't read yes/or question.", NULL);
		if (exactp) {
			if (string_equalp_char(pos, "yes")) {
				*ret = 1;
				break;
			}
			if (string_equalp_char(pos, "no")) {
				*ret = 0;
				break;
			}
			format_stream(ptr, stream, "~%Please answer yes or no: ", NULL);
		}
		else {
			string_length(pos, &size);
			if (size != 0) {
				string_getc(pos, 0, &c);
				if (toUpperUnicode(c) == 'Y') {
					*ret = 1;
					break;
				}
				if (toUpperUnicode(c) == 'N') {
					*ret = 0;
					break;
				}
			}
			format_stream(ptr, stream, "~%Please answer y or n: ", NULL);
		}
		Return(finish_output_stream_(stream));
	}
	localhold_end(hold);

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

