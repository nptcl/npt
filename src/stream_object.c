#include "heap.h"
#include "memory.h"
#include "stream_object.h"
#include "typedef.h"

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

_g void stream_heap(addr *ret, enum StreamType type, size_t size)
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

	if (! streamp(stream))
		return 0;
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

_g int input_memory_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_MemoryInput;
}

_g int output_memory_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_MemoryOutput;
}

_g int io_memory_stream_p(addr stream)
{
	return streamp(stream)
		&& getstreamtype(stream) == StreamType_MemoryIO;
}

_g int memory_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_MemoryInput
		|| check == StreamType_MemoryOutput
		|| check == StreamType_MemoryIO;
}

_g int read_memory_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_MemoryInput
		|| check == StreamType_MemoryIO;
}

_g int write_memory_stream_p(addr stream)
{
	enum StreamType check;

	if (! streamp(stream))
		return 0;
	check = getstreamtype(stream);
	return check == StreamType_MemoryOutput
		|| check == StreamType_MemoryIO;
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


/*
 *  control
 */
_g void force_open_stream(addr stream)
{
	CheckType(stream, LISPTYPE_STREAM);
	PtrStructStream(stream)->closed = 0;
}

_g void force_close_stream(addr stream)
{
	struct StructStream *ptr;

	CheckType(stream, LISPTYPE_STREAM);
	ptr = PtrStructStream(stream);
	ptr->terpri = 0;
	ptr->unread_check = 0;
	ptr->closed = 1;
}

