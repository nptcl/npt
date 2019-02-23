#include "prompt.h"
#include "stream_error.h"
#include "stream_string.h"
#include "stream.h"

#define CheckPromptStream(stream) { \
	Check(! prompt_stream_p(stream), "type error"); \
}

/*
 * #define LISP_PROMPT_DEFAULT     Use *query-io*
 * #define LISP_PROMPT_READLINE    Use readline  (for Linux)
 * #define LISP_PROMPT_EDITLINE    Use editline  (for BSD)
 */
#ifdef LISP_PROMPT_DEFAULT
void open_prompt_stream(addr *stream)
{
	standard_input_stream(Execute_Thread, stream);
}
#else
void open_prompt_stream(addr *stream)
{
	addr pos, value;

	stream_heap(&pos, StreamType_Prompt, 0);
	null_input_string_stream(&value);
	SetInfoStream(pos, value);
	*stream = pos;
}
#endif

static int close_Prompt(addr stream, int abort)
{
	CheckPromptStream(stream);
	GetInfoStream(stream, &stream);
	clear_input_string_stream(stream);
	return 1;
}

static int read_char_prompt_line(addr stream, unicode *c)
{
	addr string, pos;

	GetInfoStream(stream, &string);
	if (closep_stream(string)) {
		if (input_prompt(&pos))
			return 1;
		setvalue_input_string_stream(string, pos);
	}
	while (read_char_stream(string, c)) {
		if (input_prompt(&pos))
			return 1;
		setvalue_input_string_stream(string, pos);
	}

	return 0;
}

static int read_char_Prompt(addr stream, unicode *c)
{
	struct StructStream *ptr;

	CheckPromptStream(stream);
	ptr = PtrStructStream(stream);
	if (ptr->unread_check) {
		*c = ptr->unread;
		ptr->unread_check = 0;
		return 0;
	}
	else {
		return read_char_prompt_line(stream, c);
	}
}

static int read_hang_Prompt(addr stream, unicode *c, int *hang)
{
	addr string;
	struct StructStream *ptr;

	CheckPromptStream(stream);
	/* unread */
	ptr = PtrStructStream(stream);
	if (ptr->unread_check) {
		ptr->unread_check = 0;
		*c = ptr->unread;
		*hang = 0;
		return 0;
	}

	/* read string-buffer */
	GetInfoStream(stream, &string);
	*hang = read_char_stream(string, c) != 0;

	return 0;
}

static int listen_Prompt(addr stream)
{
	unicode c;
	struct StructStream *ptr;

	CheckPromptStream(stream);
	/* unread */
	ptr = PtrStructStream(stream);
	if (ptr->unread_check) return 1;
	/* string */
	GetInfoStream(stream, &stream);
	if (read_char_stream(stream, &c)) {
		/* input-prompt */
		return 0;
	}
	unread_char_stream(stream, c);

	return 1;
}

static void clear_input_Prompt(addr stream)
{
	CheckPromptStream(stream);
	PtrStructStream(stream)->unread_check = 0;
	GetInfoStream(stream, &stream);
	clear_input_string_stream(stream);
}

void init_stream_prompt(void)
{
	DefineStreamSet(Prompt, close);
	DefineStream___(Prompt, read_binary);
	DefineStream___(Prompt, readforce_binary);
	DefineStream___(Prompt, read_byte);
	DefineStream___(Prompt, unread_byte);
	DefineStream___(Prompt, write_binary);
	DefineStream___(Prompt, write_byte);
	DefineStreamSet(Prompt, read_char);
	DefineStreamSet(Prompt, read_hang);
	DefineStreamDef(Prompt, unread_char);
	DefineStream___(Prompt, write_char);
	DefineStream___(Prompt, fresh_line);
	DefineStreamChk(Prompt, inputp, true);
	DefineStreamChk(Prompt, outputp, false);
	DefineStreamChk(Prompt, interactivep, true);
	DefineStreamLet(Prompt, element_type, character_stream);
	DefineStream___(Prompt, file_length);
	DefineStreamDef(Prompt, file_position);
	DefineStreamDef(Prompt, file_position_start);
	DefineStreamDef(Prompt, file_position_end);
	DefineStreamDef(Prompt, file_position_set);
	DefineStream___(Prompt, file_character_length);
	DefineStream___(Prompt, file_string_length);
	DefineStreamSet(Prompt, listen);
	DefineStreamSet(Prompt, clear_input);
	DefineStream___(Prompt, finish_output);
	DefineStream___(Prompt, force_output);
	DefineStream___(Prompt, clear_output);
}

