#include "condition.h"
#include "object.h"
#include "stream_error.h"
#include "stream_synonym.h"
#include "stream.h"
#include "symbol.h"

#define CheckSynonymStream(stream) { \
	Check(! synonym_stream_p(stream), "type error"); \
}

_g void open_synonym_stream(addr *stream, addr symbol)
{
	addr pos;

	if (! symbolp(symbol))
		TypeError(symbol, SYMBOL);
	stream_heap(&pos, StreamType_Synonym, 0);
	SetInfoStream(pos, symbol);
	force_open_stream(pos);
	*stream = pos;
}

_g void get_synonym_stream(addr stream, addr *ret)
{
	CheckSynonymStream(stream);
	GetInfoStream(stream, ret);
}

_g void set_synonym_stream(addr stream, addr symbol)
{
	CheckSynonymStream(stream);
	CheckType(symbol, LISPTYPE_SYMBOL);
	SetInfoStream(stream, symbol);
}

static void getstream_synonym(addr stream, addr *ret)
{
	CheckSynonymStream(stream);
	GetInfoStream(stream, &stream);
	getspecialcheck_local(Execute_Thread, stream, ret);
}

static int read_binary_Synonym(addr stream, void *pos, size_t size, size_t *ret)
{
	getstream_synonym(stream, &stream);
	return read_binary_stream_(stream, pos, size, ret);
}

static int readf_binary_Synonym(addr stream, void *pos, size_t size, size_t *ret)
{
	getstream_synonym(stream, &stream);
	return readf_binary_stream_(stream, pos, size, ret);
}

static int read_byte_Synonym(addr stream, byte *c, int *ret)
{
	getstream_synonym(stream, &stream);
	return read_byte_stream_(stream, c, ret);
}

static int unread_byte_Synonym(addr stream, byte c)
{
	getstream_synonym(stream, &stream);
	return unread_byte_stream_(stream, c);
}

static int write_binary_Synonym(addr stream, const void *pos, size_t size, size_t *ret)
{
	getstream_synonym(stream, &stream);
	return write_binary_stream_(stream, pos, size, ret);
}

static int write_byte_Synonym(addr stream, byte c)
{
	getstream_synonym(stream, &stream);
	return write_byte_stream_(stream, c);
}

static int read_char_Synonym(addr stream, unicode *u, int *ret)
{
	getstream_synonym(stream, &stream);
	return read_char_stream_(stream, u, ret);
}

static int read_hang_Synonym(addr stream, unicode *u, int *hang, int *ret)
{
	getstream_synonym(stream, &stream);
	return read_hang_stream_(stream, u, hang, ret);
}

static int unread_char_Synonym(addr stream, unicode c)
{
	getstream_synonym(stream, &stream);
	return unread_char_stream_(stream, c);
}

static int write_char_Synonym(addr stream, unicode u)
{
	getstream_synonym(stream, &stream);
	return write_char_stream_(stream, u);
}

static int terpri_Synonym(addr stream)
{
	getstream_synonym(stream, &stream);
	return terpri_stream_(stream);
}

static int getleft_Synonym(addr stream, size_t *ret)
{
	getstream_synonym(stream, &stream);
	return getleft_stream_(stream, ret);
}

static int setleft_Synonym(addr stream, size_t value)
{
	getstream_synonym(stream, &stream);
	return setleft_stream_(stream, value);
}

static int fresh_line_Synonym(addr stream, int *ret)
{
	getstream_synonym(stream, &stream);
	return fresh_line_stream_(stream, ret);
}

static int inputp_Synonym(addr stream)
{
	getstream_synonym(stream, &stream);
	return inputp_stream(stream);
}

static int outputp_Synonym(addr stream)
{
	getstream_synonym(stream, &stream);
	return outputp_stream(stream);
}

static int interactivep_Synonym(addr stream)
{
	getstream_synonym(stream, &stream);
	return interactivep_stream(stream);
}

static int characterp_Synonym(addr stream)
{
	getstream_synonym(stream, &stream);
	return characterp_stream(stream);
}

static int binaryp_Synonym(addr stream)
{
	getstream_synonym(stream, &stream);
	return binaryp_stream(stream);
}

static int element_type_Synonym(addr stream, addr *ret)
{
	getstream_synonym(stream, &stream);
	return element_type_stream_(stream, ret);
}

static int file_length_Synonym(addr stream, addr *ret)
{
	getstream_synonym(stream, &stream);
	return file_length_stream_(stream, ret);
}

static int file_position_Synonym(addr stream, size_t *value, int *ret)
{
	getstream_synonym(stream, &stream);
	return file_position_stream_(stream, value, ret);
}

static int file_position_start_Synonym(addr stream, int *ret)
{
	getstream_synonym(stream, &stream);
	return file_position_start_stream_(stream, ret);
}

static int file_position_end_Synonym(addr stream, int *ret)
{
	getstream_synonym(stream, &stream);
	return file_position_end_stream_(stream, ret);
}

static int file_position_set_Synonym(addr stream, size_t value, int *ret)
{
	getstream_synonym(stream, &stream);
	return file_position_set_stream_(stream, value, ret);
}

static int file_charlen_Synonym(addr stream, unicode u, size_t *value, int *ret)
{
	getstream_synonym(stream, &stream);
	return file_charlen_stream_(stream, u, value, ret);
}

static int file_strlen_Synonym(addr stream, addr pos, size_t *value, int *ret)
{
	getstream_synonym(stream, &stream);
	return file_strlen_stream_(stream, pos, value, ret);
}

static int listen_Synonym(addr stream, int *ret)
{
	getstream_synonym(stream, &stream);
	return listen_stream_(stream, ret);
}

static int clear_input_Synonym(addr stream)
{
	getstream_synonym(stream, &stream);
	return clear_input_stream_(stream);
}

static int finish_output_Synonym(addr stream)
{
	getstream_synonym(stream, &stream);
	return finish_output_stream_(stream);
}

static int force_output_Synonym(addr stream)
{
	getstream_synonym(stream, &stream);
	return force_output_stream_(stream);
}

static int clear_output_Synonym(addr stream)
{
	getstream_synonym(stream, &stream);
	return clear_output_stream_(stream);
}

static int exitpoint_Synonym(addr stream)
{
	getstream_synonym(stream, &stream);
	return exitpoint_stream_(stream);
}

static int termsize_Synonym(addr stream, size_t *value, int *ret)
{
	getstream_synonym(stream, &stream);
	return termsize_stream_(stream, value, ret);
}

_g void init_stream_synonym(void)
{
	DefineStreamDef(Synonym, close);
	DefineStreamSet(Synonym, read_binary);
	DefineStreamSet(Synonym, readf_binary);
	DefineStreamSet(Synonym, read_byte);
	DefineStreamSet(Synonym, unread_byte);
	DefineStreamSet(Synonym, write_binary);
	DefineStreamSet(Synonym, write_byte);
	DefineStreamSet(Synonym, read_char);
	DefineStreamSet(Synonym, read_hang);
	DefineStreamSet(Synonym, unread_char);
	DefineStreamSet(Synonym, write_char);
	DefineStreamSet(Synonym, terpri);
	DefineStreamSet(Synonym, getleft);
	DefineStreamSet(Synonym, setleft);
	DefineStreamSet(Synonym, fresh_line);
	DefineStreamSet(Synonym, inputp);
	DefineStreamSet(Synonym, outputp);
	DefineStreamSet(Synonym, interactivep);
	DefineStreamSet(Synonym, characterp);
	DefineStreamSet(Synonym, binaryp);
	DefineStreamSet(Synonym, element_type);
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

