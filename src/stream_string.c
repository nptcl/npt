#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "array_vector.h"
#include "character.h"
#include "character_queue.h"
#include "condition.h"
#include "eastasian_unicode.h"
#include "integer.h"
#include "object.h"
#include "stream.h"
#include "stream_error.h"
#include "stream_function.h"
#include "stream_string.h"
#include "stream_variable.h"
#include "strtype.h"
#include "strvect.h"

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
	force_open_stream(pos);
	*ret = pos;
}

_g int open_input_string_stream_(addr *ret, addr string)
{
	size_t size;

	if (! stringp(string))
		return TypeError_(string, STRING);
	string_length(string, &size);
	make_input_string(ret, string, 0, size);

	return 0;
}

_g int open_input_string_stream1_(addr *ret, addr string, size_t start)
{
	addr pos1, pos2;
	size_t size;

	if (! stringp(string))
		return TypeError_(string, STRING);
	string_length(string, &size);
	if (size < start) {
		make_index_integer_alloc(NULL, &pos1, start);
		make_index_integer_alloc(NULL, &pos2, size);
		return fmte_("The start index ~S "
				"must be less than equal to length of string ~S.", pos1, pos2, NULL);
	}
	make_input_string(ret, string, start, size);

	return 0;
}

_g int open_input_string_stream2_(addr *ret, addr string, size_t start, size_t end)
{
	addr pos1, pos2;
	size_t size;

	if (! stringp(string))
		return TypeError_(string, STRING);
	string_length(string, &size);
	if (size < start) {
		make_index_integer_alloc(NULL, &pos1, start);
		make_index_integer_alloc(NULL, &pos2, size);
		return fmte_("The start index ~S "
				"must be less than equal to length of string ~S.", pos1, pos2, NULL);
	}
	if (size < end) {
		make_index_integer_alloc(NULL, &pos1, end);
		make_index_integer_alloc(NULL, &pos2, size);
		return fmte_("The end index ~S "
				"must be less than equal to length of string ~S.", pos1, pos2, NULL);
	}
	if (end < start) {
		make_index_integer_alloc(NULL, &pos1, start);
		make_index_integer_alloc(NULL, &pos2, end);
		return fmte_("The start index ~S "
				"must be less than equal to the end index~S.", pos1, pos2, NULL);
	}
	make_input_string(ret, string, start, end);

	return 0;
}

_g void open_input_char_stream(addr *stream, const char *str)
{
	addr pos;
	size_t size;

	strvect_char_heap(&pos, str);
	string_length(pos, &size);
	make_input_string(stream, pos, 0, size);
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

_g void close_input_string_stream(addr stream)
{
	force_close_stream(stream);
}

_g void getindex_input_stream(addr stream, size_t *ret)
{
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	input = PtrStringInputStream(stream);
	*ret = input->index;
}

static int unread_char_StringInput(addr stream, unicode c)
{
	struct StructStream *ptr;
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	/* unread check */
	ptr = PtrStructStream(stream);
	if (ptr->unread_check)
		return fmte_("unread already exists.", NULL);
	/* index check */
	input = PtrStringInputStream(stream);
	if (input->index == 0)
		return fmte_("index underflow.", NULL);
	input->index--;
	ptr->unread = c;
	ptr->unread_check = 1;

	return 0;
}

static int read_char_StringInput(addr stream, unicode *c, int *ret)
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
		if (input->size <= input->index)
			return Result(ret, 1); /* EOF */
		GetInfoStream(stream, &string);
		Return(string_getc_(string, input->index++, c));
	}

	return Result(ret, 0);
}

static int read_hang_StringInput(addr stream, unicode *c, int *hang, int *ret)
{
	int check;

	CheckInputStringStream(stream);
	Return(read_char_StringInput(stream, c, &check));
	*hang = (check != 0);

	return Result(ret, check);
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

static int file_position_StringInput(addr stream, size_t *value, int *ret)
{
	struct StructStream *str;
	struct stream_StringInput *input;
	size_t size;

	CheckInputStringStream(stream);
	input = PtrStringInputStream(stream);
	size = input->index;
	str = PtrStructStream(stream);
	if (str->unread_check) {
		if (size == 0) {
			*value = 0;
			*ret = 0;
			return fmte_("The stream ~S position is minus value.", stream, NULL);
		}
	}
	*value = size;
	return Result(ret, 0);
}

static int file_position_start_StringInput(addr stream, int *ret)
{
	struct StructStream *str;
	struct stream_StringInput *input;

	CheckInputStringStream(stream);
	str = PtrStructStream(stream);
	input = PtrStringInputStream(stream);
	str->unread_check = 0;
	input->index = 0;
	return Result(ret, 0);
}

static int file_position_end_StringInput(addr stream, int *ret)
{
	struct stream_StringInput *str;

	CheckInputStringStream(stream);
	str = PtrStringInputStream(stream);
	str->index = str->size;
	PtrStructStream(stream)->unread_check = 0;

	return Result(ret, 0);
}

static int file_position_set_StringInput(addr stream, size_t value, int *ret)
{
	struct stream_StringInput *str;
	addr pos;

	CheckInputStringStream(stream);
	str = PtrStringInputStream(stream);
	if (str->size < value) {
		*ret = 0;
		make_index_integer_heap(&pos, value);
		return fmte_("The position ~A is too large.", pos, NULL);
	}

	str->index = value;
	PtrStructStream(stream)->unread_check = 0;
	return Result(ret, 0);
}

static int listen_StringInput(addr stream, int *ret)
{
	CheckInputStringStream(stream);
	return Result(ret, 1);
}

static int clear_input_StringInput(addr stream)
{
	CheckInputStringStream(stream);
	/* Don't care unread-char */
	return 0;
}

_g void init_stream_string_input(void)
{
	DefineStreamDef(StringInput, close);
	DefineStream___(StringInput, read_byte);
	DefineStream___(StringInput, unread_byte);
	DefineStream___(StringInput, write_byte);
	DefineStreamSet(StringInput, read_char);
	DefineStreamSet(StringInput, read_hang);
	DefineStreamSet(StringInput, unread_char);
	DefineStream___(StringInput, write_char);
	DefineStream___(StringInput, getleft);
	DefineStream___(StringInput, setleft);
	DefineStreamChk(StringInput, inputp, true);
	DefineStreamChk(StringInput, outputp, false);
	DefineStreamChk(StringInput, interactivep, false);
	DefineStreamChk(StringInput, characterp, true);
	DefineStreamChk(StringInput, binaryp, false);
	DefineStreamLet(StringInput, element_type, character_stream);
	DefineStreamDef(StringInput, external_format);
	DefineStream___(StringInput, file_length);
	DefineStreamSet(StringInput, file_position);
	DefineStreamSet(StringInput, file_position_start);
	DefineStreamSet(StringInput, file_position_end);
	DefineStreamSet(StringInput, file_position_set);
	DefineStream___(StringInput, file_charlen);
	DefineStream___(StringInput, file_strlen);
	DefineStreamSet(StringInput, listen);
	DefineStreamSet(StringInput, clear_input);
	DefineStream___(StringInput, finish_output);
	DefineStream___(StringInput, force_output);
	DefineStream___(StringInput, clear_output);
	DefineStreamDef(StringInput, exitpoint);
	DefineStreamDef(StringInput, termsize);
}


/*****************************************************************************
 *  StringOutput
 *****************************************************************************/
struct stream_StringOutput {
	unsigned extend_p : 1;
	unsigned width_p : 1;
	unsigned pretty : 1;
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
	str->pretty = 0;
	str->width = 0;
	charqueue_heap(&queue, size);
	SetInfoStream(pos, queue);
	force_open_stream(pos);
	*stream = pos;
}

_g int copy_termsize_string_stream_(addr stream, addr src)
{
	int check;
	struct stream_StringOutput *str;
	size_t size;

	CheckOutputStringStream(stream);
	str = PtrStringOutputStream(stream);
	Return(termsize_stream_(src, &size, &check));
	if (check) {
		str->width_p = 0;
		str->width = 0;
	}
	else {
		str->width_p = 1;
		str->width = size;
	}

	return 0;
}

_g int string_stream_alloc_(LocalRoot local, addr stream, addr *string)
{
	addr queue;

	if (extend_string_p(stream)) {
		*string = Nil;
		return fmte_("The extended-string-stream ~S "
				"don't make a string.", stream, NULL);
	}
	GetInfoStream(stream, &queue);
	if (queue == Nil) {
		*string = Nil;
		return fmte_("stream is already closed.", NULL);
	}
	make_charqueue_alloc(local, queue, string);

	return 0;
}

_g int string_stream_local_(LocalRoot local, addr stream, addr *string)
{
	Check(local == NULL, "local error");
	return string_stream_alloc_(local, stream, string);
}

_g int string_stream_heap_(addr stream, addr *string)
{
	return string_stream_alloc_(NULL, stream, string);
}

_g void clear_output_string_stream(addr stream)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	Check(queue == Nil, "stream is already closed.");
	clear_charqueue(queue);
}

_g void set_pretty_output_string_stream(addr stream)
{
	CheckOutputStringStream(stream);
	PtrStringOutputStream(stream)->pretty = 1;
}

_g int get_pretty_output_string_stream(addr stream)
{
	CheckOutputStringStream(stream);
	return PtrStringOutputStream(stream)->pretty != 0;
}

_g void open_extend_output_stream(addr *stream, addr array)
{
	addr pos;
	struct stream_StringOutput *str;

	stream_heap(&pos, StreamType_StringOutput, sizeoft(struct stream_StringOutput));
	str = PtrStringOutputStream(pos);
	str->extend_p = 1;
	str->width_p = 0;
	str->pretty = 0;
	str->width = 0;
	SetInfoStream(pos, array);
	force_open_stream(pos);
	*stream = pos;
}

_g void close_output_string_stream(addr stream)
{
	CheckOutputStringStream(stream);
	SetInfoStream(stream, Nil);
	force_close_stream(stream);
}

static int close_StringOutput(addr stream, addr *ret)
{
	close_output_string_stream(stream);
	return Result(ret, T);
}

static int write_char_StringOutput_normal_(addr stream, unicode c)
{
	addr queue;

	/* stream */
	GetInfoStream(stream, &queue);
	if (queue == Nil)
		return fmte_("stream is already closed.", NULL);
	if (GetStatusDynamic(stream)) {
		Return(push_charqueue_local_(Local_Thread, queue, c));
	}
	else {
		Return(push_charqueue_heap_(queue, c));
	}

	/* terpri */
	charleft_default_stream(stream, c);

	return 0;
}

static int write_char_StringOutput_extend_(addr stream, unicode c)
{
	addr queue, value;

	/* stream */
	GetInfoStream(stream, &queue);
	if (queue == Nil)
		return fmte_("stream is already closed.", NULL);
	character_heap(&value, c);
	Return(vector_push_extend_common_(Execute_Thread, value, queue, Unbound, &value));

	/* terpri */
	charleft_default_stream(stream, c);

	return 0;
}

static int write_char_StringOutput(addr stream, unicode c)
{
	CheckOutputStringStream(stream);
	if (extend_string_p(stream))
		return write_char_StringOutput_extend_(stream, c);
	else
		return write_char_StringOutput_normal_(stream, c);
}

static int file_position_StringOutput(addr stream, size_t *value, int *ret)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream)) {
		/* fill-pointer */
		Return(array_get_vector_length_(queue, 1, value));
	}
	else {
		getsize_charqueue(queue, value);
	}

	return Result(ret, 0);
}

static int file_position_start_StringOutput(addr stream, int *ret)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream)) {
		*ret = array_fill_pointer_start(queue);
		return 0;
	}
	else {
		clear_charqueue(queue);
		return Result(ret, 0);
	}
}

static int file_position_end_StringOutput(addr stream, int *ret)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream)) {
		*ret = array_fill_pointer_end(queue);
		return 0;
	}
	else {
		return Result(ret, 0);
	}
}

static int file_position_set_StringOutput(addr stream, size_t value, int *ret)
{
	addr queue;

	CheckOutputStringStream(stream);
	GetInfoStream(stream, &queue);
	if (extend_string_p(stream))
		*ret = array_fill_pointer_set(queue, value);
	else
		*ret = position_charqueue(queue, value);

	return 0;
}

static int file_charlen_StringOutput(addr stream, unicode u, size_t *value, int *ret)
{
	CheckOutputStringStream(stream);
	*value = 1;
	return Result(ret, 0);
}

static int file_strlen_StringOutput(addr stream, addr pos, size_t *value, int *ret)
{
	CheckOutputStringStream(stream);
	string_length(pos, value);
	return Result(ret, 0);
}

static int termsize_StringOutput(addr stream, size_t *value, int *ret)
{
	struct stream_StringOutput *str;

	CheckOutputStringStream(stream);
	str = PtrStringOutputStream(stream);
	if (str->width_p)
		*value = str->width;

	return Result(ret, str->width_p);
}

_g void init_stream_string_output(void)
{
	DefineStreamSet(StringOutput, close);
	DefineStream___(StringOutput, read_byte);
	DefineStream___(StringOutput, unread_byte);
	DefineStream___(StringOutput, write_byte);
	DefineStream___(StringOutput, read_char);
	DefineStream___(StringOutput, read_hang);
	DefineStream___(StringOutput, unread_char);
	DefineStreamSet(StringOutput, write_char);
	DefineStreamDef(StringOutput, getleft);
	DefineStreamDef(StringOutput, setleft);
	DefineStreamChk(StringOutput, inputp, false);
	DefineStreamChk(StringOutput, outputp, true);
	DefineStreamChk(StringOutput, interactivep, false);
	DefineStreamChk(StringOutput, characterp, true);
	DefineStreamChk(StringOutput, binaryp, false);
	DefineStreamLet(StringOutput, element_type, character_stream);
	DefineStreamDef(StringOutput, external_format);
	DefineStream___(StringOutput, file_length);
	DefineStreamSet(StringOutput, file_position);
	DefineStreamSet(StringOutput, file_position_start);
	DefineStreamSet(StringOutput, file_position_end);
	DefineStreamSet(StringOutput, file_position_set);
	DefineStreamSet(StringOutput, file_charlen);
	DefineStreamSet(StringOutput, file_strlen);
	DefineStream___(StringOutput, listen);
	DefineStream___(StringOutput, clear_input);
	DefineStreamDef(StringOutput, finish_output);
	DefineStreamDef(StringOutput, force_output);
	DefineStreamDef(StringOutput, clear_output);
	DefineStreamDef(StringOutput, exitpoint);
	DefineStreamSet(StringOutput, termsize);
}

