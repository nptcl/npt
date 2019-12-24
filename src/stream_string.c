#include "array.h"
#include "array_object.h"
#include "array_vector.h"
#include "charqueue.h"
#include "condition.h"
#include "eastasian_unicode.h"
#include "integer.h"
#include "object.h"
#include "stream_error.h"
#include "stream_string.h"
#include "stream.h"
#include "strtype.h"
#include "unicode.h"

/*****************************************************************************
 *  StringInput
 *****************************************************************************/
struct stream_StringInput {
	size_t index, size;
};

#define CheckInputStringStream(stream) { \
	Check(! input_string_stream_p(stream), "type error"); \
}
#define PtrStringInputStream(pos) ((struct stream_StringInput *)PtrDataStream(pos))

static void make_input_string(addr *ret, addr string, size_t start, size_t end)
{
	addr pos;
	struct stream_StringInput *input;

	stream_heap(&pos, StreamType_StringInput, sizeoft(struct stream_StringInput));
	input = PtrStringInputStream(pos);
	input->index = start;
	input->size = end;
	SetInfoStream(pos, string);
	force_open_stream(pos, ret);
}

_g void open_input_string_stream(addr *ret, addr string)
{
	size_t size;

	if (! stringp(string))
		TypeError(string, STRING);
	string_length(string, &size);
	make_input_string(ret, string, 0, size);
}

_g void open_input_string_stream1(addr *ret, addr string, size_t start)
{
	addr pos1, pos2;
	size_t size;

	if (! stringp(string))
		TypeError(string, STRING);
	string_length(string, &size);
	if (size < start) {
		make_index_integer_alloc(NULL, &pos1, start);
		make_index_integer_alloc(NULL, &pos2, size);
		fmte("The start index ~S must be less than equal to length of string ~S.",
				pos1, pos2, NULL);
	}
	make_input_string(ret, string, start, size);
}

_g void open_input_string_stream2(addr *ret, addr string, size_t start, size_t end)
{
	addr pos1, pos2;
	size_t size;

	if (! stringp(string))
		TypeError(string, STRING);
	string_length(string, &size);
	if (size < start) {
		make_index_integer_alloc(NULL, &pos1, start);
		make_index_integer_alloc(NULL, &pos2, size);
		fmte("The start index ~S must be less than equal to length of string ~S.",
				pos1, pos2, NULL);
	}
	if (size < end) {
		make_index_integer_alloc(NULL, &pos1, end);
		make_index_integer_alloc(NULL, &pos2, size);
		fmte("The end index ~S must be less than equal to length of string ~S.",
				pos1, pos2, NULL);
	}
	if (end < start) {
		make_index_integer_alloc(NULL, &pos1, start);
		make_index_integer_alloc(NULL, &pos2, end);
		fmte("The start index ~S must be less than equal to the end index~S.",
				pos1, pos2, NULL);
	}
	make_input_string(ret, string, start, end);
}

_g void open_input_char_stream(addr *stream, const char *str)
{
	addr pos;
	strvect_char_heap(&pos, str);
	open_input_string_stream(stream, pos);
}

_g void null_input_string_stream(addr *stream)
{
	addr pos;
	struct StructStream *str;

	make_input_string(&pos, Nil, 0, 0);
	str = PtrStructStream(pos);
	str->unread_check = 0;
	str->closed = 1;
	str->unread = 0;
	str->terpri = 0;
	*stream = pos;
}

_g void getindex_input_stream(addr stream, size_t *ret)
{
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	input = PtrStringInputStream(stream);
	*ret = input->index;
}

static void unread_char_StringInput(addr stream, unicode c)
{
	struct StructStream *ptr;
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	/* unread check */
	ptr = PtrStructStream(stream);
	if (ptr->unread_check) {
		fmte("unread already exists.", NULL);
		return;
	}
	/* index check */
	input = PtrStringInputStream(stream);
	if (input->index == 0) {
		fmte("index underflow.", NULL);
		return;
	}
	input->index--;
	ptr->unread = c;
	ptr->unread_check = 1;
}

static int read_char_StringInput(addr stream, unicode *c)
{
	addr string;
	struct StructStream *ptr;
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	ptr = PtrStructStream(stream);
	input = PtrStringInputStream(stream);
	if (ptr->unread_check) {
		*c = ptr->unread;
		ptr->unread_check = 0;
		input->index++;
	}
	else {
		if (input->size <= input->index) return 1; /* EOF */
		GetInfoStream(stream, &string);
		string_getc(string, input->index++, c);
	}

	return 0;
}

static int read_hang_StringInput(addr stream, unicode *c, int *hang)
{
	int result;

	CheckInputStringStream(stream);
	result = read_char_StringInput(stream, c);
	if (result == 0)
		*hang = 0;

	return result;
}

_g void setvalue_input_string_stream(addr stream, addr value)
{
	struct StructStream *str;
	struct stream_StringInput *input;
	size_t size;

	CheckInputStringStream(stream);
	Check(! stringp(value), "type error string.");
	/* string-stream */
	string_length(value, &size);
	input = PtrStringInputStream(stream);
	input->index = 0;
	input->size = size;
	SetInfoStream(stream, value);
	/* stream */
	str = PtrStructStream(stream);
	str->unread_check = 0;
	str->closed = 0;
	str->unread = 0;
	str->terpri = 0;
}

_g void clear_input_string_stream(addr stream)
{
	struct StructStream *str;
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	/* string-stream */
	input = PtrStringInputStream(stream);
	input->index = 0;
	input->size = 0;
	SetInfoStream(stream, Nil);
	/* stream */
	str = PtrStructStream(stream);
	str->unread_check = 0;
	str->closed = 0;
	str->unread = 0;
	str->terpri = 0;
}

static int file_position_StringInput(addr stream, size_t *ret)
{
	struct StructStream *str;
	struct stream_StringInput *input;
	size_t size;

	CheckInputStringStream(stream);
	input = PtrStringInputStream(stream);
	size = input->index;
	str = PtrStructStream(stream);
	if (str->unread_check) {
		if (size == 0)
			fmte("The stream ~S position is minus value.", stream, NULL);
		size--;
	}
	*ret = size;

	return 0;
}

static int file_position_start_StringInput(addr stream)
{
	struct StructStream *str;
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	str = PtrStructStream(stream);
	input = PtrStringInputStream(stream);
	str->unread_check = 0;
	input->index = 0;

	return 0;
}

static int file_position_end_StringInput(addr stream)
{
	struct stream_StringInput *str;

	CheckInputStringStream(stream);
	str = PtrStringInputStream(stream);
	str->index = str->size;
	PtrStructStream(stream)->unread_check = 0;

	return 0;
}

static int file_position_set_StringInput(addr stream, size_t pos)
{
	struct stream_StringInput *str;

	CheckInputStringStream(stream);
	str = PtrStringInputStream(stream);
	if (pos <= str->size)
		str->index = pos;
	else
		fmte("The position ~A is too large.", intsizeh(pos), NULL);
	PtrStructStream(stream)->unread_check = 0;

	return 0;
}

static int listen_StringInput(addr stream)
{
	CheckInputStringStream(stream);
	return 1;
}

static void clear_input_StringInput(addr stream)
{
	CheckInputStringStream(stream);
	/* Don't care unread-char */
}

_g void init_stream_string_input(void)
{
	DefineStreamDef(StringInput, close);
	DefineStream___(StringInput, read_binary);
	DefineStream___(StringInput, readforce_binary);
	DefineStream___(StringInput, read_byte);
	DefineStream___(StringInput, unread_byte);
	DefineStream___(StringInput, write_binary);
	DefineStream___(StringInput, write_byte);
	DefineStreamSet(StringInput, read_char);
	DefineStreamSet(StringInput, read_hang);
	DefineStreamSet(StringInput, unread_char);
	DefineStream___(StringInput, write_char);
	DefineStream___(StringInput, terpri);
	DefineStream___(StringInput, fresh_line);
	DefineStreamChk(StringInput, inputp, true);
	DefineStreamChk(StringInput, outputp, false);
	DefineStreamChk(StringInput, interactivep, false);
	DefineStreamChk(StringInput, characterp, true);
	DefineStreamChk(StringInput, binaryp, false);
	DefineStreamLet(StringInput, element_type, character_stream);
	DefineStream___(StringInput, file_length);
	DefineStreamSet(StringInput, file_position);
	DefineStreamSet(StringInput, file_position_start);
	DefineStreamSet(StringInput, file_position_end);
	DefineStreamSet(StringInput, file_position_set);
	DefineStream___(StringInput, file_character_length);
	DefineStream___(StringInput, file_string_length);
	DefineStreamSet(StringInput, listen);
	DefineStreamSet(StringInput, clear_input);
	DefineStream___(StringInput, finish_output);
	DefineStream___(StringInput, force_output);
	DefineStream___(StringInput, clear_output);
	DefineStreamDef(StringInput, exitpoint);
	DefineStreamDef(StringInput, terminal_width);
}


/*****************************************************************************
 *  StringOutput
 *****************************************************************************/
struct stream_StringOutput {
	unsigned extend_p : 1;
	unsigned width_p : 1;
	size_t width;
};

#define CheckOutputStringStream(stream) { \
	Check(! output_string_stream_p(stream), "type error"); \
}
#define PtrStringOutputStream(pos) ((struct stream_StringOutput *)PtrDataStream(pos))

static int extend_string_p(addr stream)
{
	struct stream_StringOutput *output = PtrStringOutputStream(stream);
	return output->extend_p;
}

_g void open_output_string_stream(addr *stream, size_t size)
{
	addr pos, queue;
	struct stream_StringOutput *str;

	stream_heap(&pos, StreamType_StringOutput, sizeoft(struct stream_StringOutput));
	str = PtrStringOutputStream(pos);
	str->extend_p = 0;
	str->width_p = 0;
	str->width = 0;
	charqueue_heap(&queue, size);
	SetInfoStream(pos, queue);
	force_open_stream(pos, stream);
}

_g void copy_terminal_width_string_stream(addr stream, addr src)
{
	struct stream_StringOutput *str;
	size_t size;

	CheckOutputStringStream(stream);
	str = PtrStringOutputStream(stream);
	if (terminal_width_stream(src, &size)) {
		str->width_p = 0;
		str->width = 0;
	}
	else {
		str->width_p = 1;
		str->width = size;
	}
}

_g void string_stream_alloc(LocalRoot local, addr stream, addr *string)
{
	addr queue;

	if (extend_string_p(stream))
		fmte("The extended-string-stream ~S don't make a string.", stream, NULL);
	GetInfoStream(stream, &queue);
	if (queue == Nil)
		fmte("stream is already closed.", NULL);
	make_charqueue_alloc(local, queue, string);
}

_g void string_stream_local(LocalRoot local, addr stream, addr *string)
{
	Check(local == NULL, "local error");
	string_stream_alloc(local, stream, string);
}

_g void string_stream_heap(addr stream, addr *string)
{
	string_stream_alloc(NULL, stream, string);
}

_g void clear_output_string_stream(addr stream)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (queue == Nil)
		fmte("stream is already closed.", NULL);
	clear_charqueue(queue);
}

_g void open_extend_output_stream(addr *stream, addr array)
{
	addr pos;
	struct stream_StringOutput *str;

	stream_heap(&pos, StreamType_StringOutput, sizeoft(struct stream_StringOutput));
	str = PtrStringOutputStream(pos);
	str->extend_p = 1;
	str->width_p = 0;
	str->width = 0;
	SetInfoStream(pos, array);
	force_open_stream(pos, stream);
}

static int close_StringOutput(addr stream, int abort)
{
	CheckOutputStringStream(stream);
	SetInfoStream(stream, Nil);
	return 1;
}

static void write_char_StringOutput_normal(addr stream, unicode c)
{
	struct StructStream *ptr;
	addr queue;

	/* stream */
	GetInfoStream(stream, &queue);
	if (queue == Nil)
		fmte("stream is already closed.", NULL);
	if (GetStatusDynamic(stream))
		push_charqueue_local(Local_Thread, queue, c);
	else
		push_charqueue_heap(queue, c);

	/* terpri */
	ptr = PtrStructStream(stream);
	if (c == '\n' || c == '\f')
		ptr->terpri = 0;
	else
		ptr->terpri += eastasian_width(c);
}

static void write_char_StringOutput_extend(addr stream, unicode c)
{
	struct StructStream *ptr;
	addr queue, value;

	/* stream */
	GetInfoStream(stream, &queue);
	if (queue == Nil)
		fmte("stream is already closed.", NULL);
	character_heap(&value, c);
	vector_push_extend_common(value, queue, Unbound, &value);

	/* terpri */
	ptr = PtrStructStream(stream);
	if (c == '\n' || c == '\f')
		ptr->terpri = 0;
	else
		ptr->terpri += eastasian_width(c);
}

static void write_char_StringOutput(addr stream, unicode c)
{
	CheckOutputStringStream(stream);
	if (extend_string_p(stream))
		write_char_StringOutput_extend(stream, c);
	else
		write_char_StringOutput_normal(stream, c);
}

static int file_position_StringOutput(addr stream, size_t *ret)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream))
		*ret = array_vector_length(queue, 1); /* fill-pointer */
	else
		getsize_charqueue(queue, ret);

	return 0;
}

static int file_position_start_StringOutput(addr stream)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream)) {
		return array_fill_pointer_start(queue);
	}
	else {
		clear_charqueue(queue);
		return 0;
	}
}

static int file_position_end_StringOutput(addr stream)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream))
		return array_fill_pointer_end(queue);
	else
		return 0;
}

static int file_position_set_StringOutput(addr stream, size_t pos)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream))
		return array_fill_pointer_set(queue, pos);
	else
		return position_charqueue(queue, pos);
}

static int file_character_length_StringOutput(addr stream, unicode u, size_t *ret)
{
	CheckOutputStringStream(stream);
	*ret = 1;
	return 0;
}

static int file_string_length_StringOutput(addr stream, addr pos, size_t *ret)
{
	CheckOutputStringStream(stream);
	string_length(pos, ret);
	return 0;
}

static int terminal_width_StringOutput(addr stream, size_t *ret)
{
	struct stream_StringOutput *str;

	CheckOutputStringStream(stream);
	str = PtrStringOutputStream(stream);
	if (str->width_p)
		*ret = str->width;

	return str->width_p;
}

_g void init_stream_string_output(void)
{
	DefineStreamSet(StringOutput, close);
	DefineStream___(StringOutput, read_binary);
	DefineStream___(StringOutput, readforce_binary);
	DefineStream___(StringOutput, read_byte);
	DefineStream___(StringOutput, unread_byte);
	DefineStream___(StringOutput, write_binary);
	DefineStream___(StringOutput, write_byte);
	DefineStream___(StringOutput, read_char);
	DefineStream___(StringOutput, read_hang);
	DefineStream___(StringOutput, unread_char);
	DefineStreamSet(StringOutput, write_char);
	DefineStreamDef(StringOutput, terpri);
	DefineStreamDef(StringOutput, fresh_line);
	DefineStreamChk(StringOutput, inputp, false);
	DefineStreamChk(StringOutput, outputp, true);
	DefineStreamChk(StringOutput, interactivep, false);
	DefineStreamChk(StringOutput, characterp, true);
	DefineStreamChk(StringOutput, binaryp, false);
	DefineStreamLet(StringOutput, element_type, character_stream);
	DefineStream___(StringOutput, file_length);
	DefineStreamSet(StringOutput, file_position);
	DefineStreamSet(StringOutput, file_position_start);
	DefineStreamSet(StringOutput, file_position_end);
	DefineStreamSet(StringOutput, file_position_set);
	DefineStreamSet(StringOutput, file_character_length);
	DefineStreamSet(StringOutput, file_string_length);
	DefineStream___(StringOutput, listen);
	DefineStream___(StringOutput, clear_input);
	DefineStreamDef(StringOutput, finish_output);
	DefineStreamDef(StringOutput, force_output);
	DefineStreamDef(StringOutput, clear_output);
	DefineStreamDef(StringOutput, exitpoint);
	DefineStreamSet(StringOutput, terminal_width);
}

