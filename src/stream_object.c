#include "heap.h"
#include "memory.h"
#include "stream.h"
#include "stream_object.h"
#include "stream_synonym.h"
#include "stream_twoway.h"
#include "symbol.h"
#include "typedef.h"

void *ptrbody_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrBodyStream_Low(stream);
}

struct StructStream *ptrstruct_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream_Low(stream);
}

void *ptrdata_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrDataStream_Low(stream);
}

void gettype_stream(addr stream, enum StreamType *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetTypeStream_Low(stream, ret);
}

size_t getindex_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return GetIndexStream_Low(stream);
}

void getpathname_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetPathnameStream_Low(stream, ret);
}

void setpathname_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetPathnameStream_Low(stream, value);
}

void getinfo_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetInfoStream_Low(stream, ret);
}

void setinfo_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetInfoStream_Low(stream, value);
}

void getinput_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetInputStream_Low(stream, ret);
}

void setinput_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetInputStream_Low(stream, value);
}

void getoutput_stream(addr stream, addr *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	GetOutputStream_Low(stream, ret);
}

void setoutput_stream(addr stream, addr value)
{
	CheckType(stream, LISPTYPE_STREAM);
	Check(GetStatusReadOnly(stream), "readonly error");
	SetOutputStream_Low(stream, value);
}

void stream_heap(addr *ret, enum StreamType type, size_t size)
{
	struct StructStream *ptr;
	size_t allsize;

	allsize = sizeoft(struct StructStream) + size;
	heap_arraybody(ret, LISPTYPE_STREAM,
			STREAM_INDEX_SIZE, (byte16)allsize);
	Check(0xFFFF <= allsize, "size error");
	ptr = PtrStructStream(*ret);
	memset(ptr, 0, allsize);
	ptr->type = type;
	ptr->terpri = 0;
	ptr->unread = 0;
	ptr->unread_check = 0;
	ptr->closed = 1;
}

enum StreamType getstreamtype(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	return PtrStructStream(stream)->type;
}

int streamp(addr stream)
{
	return GetType(stream) == LISPTYPE_STREAM;
}

int file_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_BinaryInput
		|| check == StreamType_BinaryOutput
		|| check == StreamType_BinaryIO
		|| check == StreamType_CharacterInput
		|| check == StreamType_CharacterOutput
		|| check == StreamType_CharacterIO
		|| check == StreamType_BincharInput
		|| check == StreamType_BincharOutput
		|| check == StreamType_BincharIO
		|| check == StreamType_Probe;
}

int broadcast_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_BroadCast;
}

int concatenated_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Concatenated;
}

int echo_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Echo;
}

int synonym_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Synonym;
}

int twoway_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_TwoWay;
}

int input_string_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_StringInput;
}

int output_string_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_StringOutput;
}

int string_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_StringInput
		|| check == StreamType_StringOutput;
}

int prompt_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Prompt;
}

int pretty_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Pretty;
}

int input_memory_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_MemoryInput;
}

int output_memory_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_MemoryOutput;
}

int io_memory_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_MemoryIO;
}

int memory_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_MemoryInput
		|| check == StreamType_MemoryOutput
		|| check == StreamType_MemoryIO;
}

int read_memory_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_MemoryInput
		|| check == StreamType_MemoryIO;
}

int write_memory_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_MemoryOutput
		|| check == StreamType_MemoryIO;
}

int pipe_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_Pipe;
}

int extend_stream_p(addr stream)
{
	return streamp(stream)
		&& ((int)StreamExtend_Index(0)) <= ((int)getstreamtype(stream));
}

int extend_type_stream_p(addr stream, int type)
{
	return streamp(stream)
		&& ((int)getstreamtype(stream)) == type;
}


/*
 *  control
 */
void force_open_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	PtrStructStream(stream)->closed = 0;
}

void force_close_stream(addr stream)
{
	struct StructStream *ptr;

	CheckType(stream, LISPTYPE_STREAM);
	ptr = PtrStructStream(stream);
	ptr->terpri = 0;
	ptr->unread_check = 0;
	ptr->closed = 1;
}


/*
 *  check prompt
 */
int use_prompt_stream(Execute ptr, addr stream)
{
	if (! streamp(stream))
		return 0;
	if (prompt_stream_p(stream))
		return 1;

	/* synonym */
	if (synonym_stream_p(stream)) {
		get_synonym_stream(stream, &stream);
		if (! symbolp(stream))
			return 0;
		getspecial_local(ptr, stream, &stream);
		if (stream == Unbound)
			return 0;
		return use_prompt_stream(ptr, stream);
	}

	/* two-way */
	if (twoway_stream_p(stream)) {
		get_twoway_input_stream(stream, &stream);
		return use_prompt_stream(ptr, stream);
	}

	return 0;
}

