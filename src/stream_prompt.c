#include "constant.h"
#include "control_object.h"
#include "file.h"
#include "prompt.h"
#include "prompt_arch.h"
#include "stream.h"
#include "stream_error.h"
#include "stream_function.h"
#include "stream_prompt.h"
#include "stream_string.h"
#include "stream_variable.h"
#include "symbol.h"

#define CheckPromptStream(stream) { \
	Check(! prompt_stream_p(stream), "type error"); \
}

#ifdef LISP_PROMPT_DISABLE
void open_prompt_stream(addr *stream)
{
	Error(standard_input_stream_(Execute_Thread, stream));
}
#else
void open_prompt_stream(addr *stream)
{
	addr pos, value;

	stream_heap(&pos, StreamType_Prompt, 0);
	null_input_string_stream(&value);
	SetInfoStream(pos, value);
	force_open_stream(pos);
	*stream = pos;
}
#endif

static int close_Prompt(addr stream, addr *ret)
{
	CheckPromptStream(stream);
	GetInfoStream(stream, &stream);
	clear_input_string_stream(stream);
	return Result(ret, T);
}

static int input_prompt_stream(addr stream, addr *ret)
{
	addr pos, prompt, dribble;
	Execute ptr;

	/* read */
	ptr = Execute_Thread;
	getvalue_prompt(ptr, &prompt);
	Return(input_prompt_(ptr, &pos));
	if (pos == Nil) /* eof */
		return Result(ret, Nil);
	/* dribble check */
	GetConst(SYSTEM_DRIBBLE_FILE, &dribble);
	GetValueSymbol(dribble, &dribble);
	if (dribble != Unbound) {
		if (prompt != Nil) {
			Return(print_string_stream_(dribble, prompt));
		}
		Return(print_string_stream_(dribble, pos));
	}
	/* result */
	return Result(ret, pos);
}

static int read_char_prompt_line_(addr stream, unicode *c, int *ret)
{
	int check;
	addr string, pos;

	GetInfoStream(stream, &string);
	if (! open_stream_p(string)) {
		Return(input_prompt_stream(stream, &pos));
		if (pos == Nil)
			return Result(ret, 1); /* eof */
		setvalue_input_string_stream(string, pos);
	}
	for (;;) {
		Return(read_char_stream_(string, c, &check));
		if (! check)
			break;
		Return(input_prompt_stream(stream, &pos));
		if (pos == Nil)
			return Result(ret, 1); /* eof */
		setvalue_input_string_stream(string, pos);
	}

	/* normal */
	return Result(ret, 0);
}

static int read_char_Prompt(addr stream, unicode *c, int *ret)
{
	struct StructStream *ptr;

	CheckPromptStream(stream);
	ptr = PtrStructStream(stream);
	if (! ptr->unread_check)
		return read_char_prompt_line_(stream, c, ret);

	*c = ptr->unread;
	ptr->unread_check = 0;
	return Result(ret, 0);
}

static int read_hang_Prompt(addr stream, unicode *c, int *hang, int *ret)
{
	int check;
	addr string;
	struct StructStream *ptr;

	CheckPromptStream(stream);
	/* unread */
	ptr = PtrStructStream(stream);
	if (ptr->unread_check) {
		ptr->unread_check = 0;
		*c = ptr->unread;
		*hang = 0;
		return Result(ret, 0);
	}

	/* read string-buffer */
	GetInfoStream(stream, &string);
	Return(read_char_stream_(stream, c, &check));
	*hang = (check != 0);

	return Result(ret, 0);
}

static int listen_Prompt(addr stream, int *ret)
{
	int check;
	unicode c;
	struct StructStream *ptr;

	CheckPromptStream(stream);
	/* unread */
	ptr = PtrStructStream(stream);
	if (ptr->unread_check)
		return Result(ret, 1);
	/* string */
	GetInfoStream(stream, &stream);
	Return(read_char_stream_(stream, &c, &check));
	if (check) {
		/* input-prompt */
		return Result(ret, 0);
	}
	Return(unread_char_stream_(stream, c));

	return Result(ret, 1);
}

static int clear_input_Prompt(addr stream)
{
	CheckPromptStream(stream);
	PtrStructStream(stream)->unread_check = 0;
	GetInfoStream(stream, &stream);
	clear_input_string_stream(stream);
	return 0;
}

void init_stream_prompt(void)
{
	DefineStreamSet(Prompt, close);
	DefineStream___(Prompt, read_byte);
	DefineStream___(Prompt, unread_byte);
	DefineStream___(Prompt, write_byte);
	DefineStreamSet(Prompt, read_char);
	DefineStreamSet(Prompt, read_hang);
	DefineStreamDef(Prompt, unread_char);
	DefineStream___(Prompt, write_char);
	DefineStream___(Prompt, getleft);
	DefineStream___(Prompt, setleft);
	DefineStreamChk(Prompt, inputp, true);
	DefineStreamChk(Prompt, outputp, false);
	DefineStreamChk(Prompt, interactivep, true);
	DefineStreamChk(Prompt, characterp, true);
	DefineStreamChk(Prompt, binaryp, false);
	DefineStreamLet(Prompt, element_type, character_stream);
	DefineStreamDef(Prompt, external_format);
	DefineStream___(Prompt, file_length);
	DefineStreamDef(Prompt, file_position);
	DefineStreamDef(Prompt, file_position_start);
	DefineStreamDef(Prompt, file_position_end);
	DefineStreamDef(Prompt, file_position_set);
	DefineStream___(Prompt, file_charlen);
	DefineStream___(Prompt, file_strlen);
	DefineStreamSet(Prompt, listen);
	DefineStreamSet(Prompt, clear_input);
	DefineStream___(Prompt, finish_output);
	DefineStream___(Prompt, force_output);
	DefineStream___(Prompt, clear_output);
	DefineStream___(Prompt, exitpoint);
	DefineStreamLet(Prompt, termsize, file_);
}

