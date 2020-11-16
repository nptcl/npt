#include "condition.h"
#include "object.h"
#include "stream.h"
#include "stream_error.h"
#include "stream_function.h"
#include "stream_synonym.h"
#include "stream_variable.h"
#include "symbol.h"

#define CheckSynonymStream(stream) { \
	Check(! synonym_stream_p(stream), "type error"); \
}

int open_synonym_stream_(addr *stream, addr symbol)
{
	addr pos;

	if (! symbolp(symbol))
		return TypeError_(symbol, SYMBOL);
	stream_heap(&pos, StreamType_Synonym, 0);
	SetInfoStream(pos, symbol);
	force_open_stream(pos);

	return Result(stream, pos);
}

void get_synonym_stream(addr stream, addr *ret)
{
	CheckSynonymStream(stream);
	GetInfoStream(stream, ret);
}

void set_synonym_stream(addr stream, addr symbol)
{
	CheckSynonymStream(stream);
	CheckType(symbol, LISPTYPE_SYMBOL);
	SetInfoStream(stream, symbol);
}

static int getstream_synonym_(addr stream, addr *ret)
{
	CheckSynonymStream(stream);
	GetInfoStream(stream, &stream);
	return getspecialcheck_local_(Execute_Thread, stream, ret);
}

static int read_byte_Synonym(addr stream, addr *value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return read_byte_stream_(stream, value, ret);
}

static int unread_byte_Synonym(addr stream, byte c)
{
	Return(getstream_synonym_(stream, &stream));
	return unread_byte_stream_(stream, c);
}

static int write_byte_Synonym(addr stream, addr pos)
{
	Return(getstream_synonym_(stream, &stream));
	return write_byte_stream_(stream, pos);
}

static int read_char_Synonym(addr stream, unicode *u, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return read_char_stream_(stream, u, ret);
}

static int read_hang_Synonym(addr stream, unicode *u, int *hang, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return read_hang_stream_(stream, u, hang, ret);
}

static int unread_char_Synonym(addr stream, unicode c)
{
	Return(getstream_synonym_(stream, &stream));
	return unread_char_stream_(stream, c);
}

static int write_char_Synonym(addr stream, unicode u)
{
	Return(getstream_synonym_(stream, &stream));
	return write_char_stream_(stream, u);
}

static int getleft_Synonym(addr stream, size_t *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return getleft_stream_(stream, ret);
}

static int setleft_Synonym(addr stream, size_t value)
{
	Return(getstream_synonym_(stream, &stream));
	return setleft_stream_(stream, value);
}

static int inputp_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return inputp_stream_(stream, ret);
}

static int outputp_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return outputp_stream_(stream, ret);
}

static int interactivep_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return interactivep_stream_(stream, ret);
}

static int characterp_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return characterp_stream_(stream, ret);
}

static int binaryp_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return binaryp_stream_(stream, ret);
}

static int element_type_Synonym(addr stream, addr *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return element_type_stream_(stream, ret);
}

static int external_format_Synonym(addr stream, addr *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return external_format_stream_(stream, ret);
}

static int file_length_Synonym(addr stream, addr *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_length_stream_(stream, ret);
}

static int file_position_Synonym(addr stream, size_t *value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_position_stream_(stream, value, ret);
}

static int file_position_start_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_position_start_stream_(stream, ret);
}

static int file_position_end_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_position_end_stream_(stream, ret);
}

static int file_position_set_Synonym(addr stream, size_t value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_position_set_stream_(stream, value, ret);
}

static int file_charlen_Synonym(addr stream, unicode u, size_t *value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_charlen_stream_(stream, u, value, ret);
}

static int file_strlen_Synonym(addr stream, addr pos, size_t *value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return file_strlen_stream_(stream, pos, value, ret);
}

static int listen_Synonym(addr stream, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return listen_stream_(stream, ret);
}

static int clear_input_Synonym(addr stream)
{
	Return(getstream_synonym_(stream, &stream));
	return clear_input_stream_(stream);
}

static int finish_output_Synonym(addr stream)
{
	Return(getstream_synonym_(stream, &stream));
	return finish_output_stream_(stream);
}

static int force_output_Synonym(addr stream)
{
	Return(getstream_synonym_(stream, &stream));
	return force_output_stream_(stream);
}

static int clear_output_Synonym(addr stream)
{
	Return(getstream_synonym_(stream, &stream));
	return clear_output_stream_(stream);
}

static int exitpoint_Synonym(addr stream)
{
	Return(getstream_synonym_(stream, &stream));
	return exitpoint_stream_(stream);
}

static int termsize_Synonym(addr stream, size_t *value, int *ret)
{
	Return(getstream_synonym_(stream, &stream));
	return termsize_stream_(stream, value, ret);
}

void init_stream_synonym(void)
{
	DefineStreamDef(Synonym, close);
	DefineStreamSet(Synonym, read_byte);
	DefineStreamSet(Synonym, unread_byte);
	DefineStreamSet(Synonym, write_byte);
	DefineStreamSet(Synonym, read_char);
	DefineStreamSet(Synonym, read_hang);
	DefineStreamSet(Synonym, unread_char);
	DefineStreamSet(Synonym, write_char);
	DefineStreamSet(Synonym, getleft);
	DefineStreamSet(Synonym, setleft);
	DefineStreamSet(Synonym, inputp);
	DefineStreamSet(Synonym, outputp);
	DefineStreamSet(Synonym, interactivep);
	DefineStreamSet(Synonym, characterp);
	DefineStreamSet(Synonym, binaryp);
	DefineStreamSet(Synonym, element_type);
	DefineStreamSet(Synonym, external_format);
	DefineStreamSet(Synonym, file_length);
	DefineStreamSet(Synonym, file_position);
	DefineStreamSet(Synonym, file_position_start);
	DefineStreamSet(Synonym, file_position_end);
	DefineStreamSet(Synonym, file_position_set);
	DefineStreamSet(Synonym, file_charlen);
	DefineStreamSet(Synonym, file_strlen);
	DefineStreamSet(Synonym, listen);
	DefineStreamSet(Synonym, clear_input);
	DefineStreamSet(Synonym, finish_output);
	DefineStreamSet(Synonym, force_output);
	DefineStreamSet(Synonym, clear_output);
	DefineStreamSet(Synonym, exitpoint);
	DefineStreamSet(Synonym, termsize);
}

