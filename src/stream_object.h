#ifndef __STREAM_OBJECT_HEADER__
#define __STREAM_OBJECT_HEADER__

#include "memory.h"
#include "execute.h"
#include "typedef.h"

#define ptrbody_stream _n(ptrbody_stream)
#define ptrstruct_stream _n(ptrstruct_stream)
#define ptrdata_stream _n(ptrdata_stream)
#define gettype_stream _n(gettype_stream)
#define getindex_stream _n(getindex_stream)
#define getpathname_stream _n(getpathname_stream)
#define setpathname_stream _n(setpathname_stream)
#define getinfo_stream _n(getinfo_stream)
#define setinfo_stream _n(setinfo_stream)
#define getinput_stream _n(getinput_stream)
#define setinput_stream _n(setinput_stream)
#define getoutput_stream _n(getoutput_stream)
#define setoutput_stream _n(setoutput_stream)
#define stream_heap _n(stream_heap)
#define getstreamtype _n(getstreamtype)
#define streamp _n(streamp)
#define file_stream_p _n(file_stream_p)
#define broadcast_stream_p _n(broadcast_stream_p)
#define concatenated_stream_p _n(concatenated_stream_p)
#define echo_stream_p _n(echo_stream_p)
#define synonym_stream_p _n(synonym_stream_p)
#define twoway_stream_p _n(twoway_stream_p)
#define input_string_stream_p _n(input_string_stream_p)
#define output_string_stream_p _n(output_string_stream_p)
#define string_stream_p _n(string_stream_p)
#define prompt_stream_p _n(prompt_stream_p)
#define pretty_stream_p _n(pretty_stream_p)
#define input_memory_stream_p _n(input_memory_stream_p)
#define output_memory_stream_p _n(output_memory_stream_p)
#define io_memory_stream_p _n(io_memory_stream_p)
#define read_memory_stream_p _n(read_memory_stream_p)
#define write_memory_stream_p _n(write_memory_stream_p)
#define memory_stream_p _n(memory_stream_p)
#define pipe_stream_p _n(pipe_stream_p)
#define extend_stream_p _n(extend_stream_p)
#define extend_type_stream_p _n(extend_type_stream_p)

#define force_open_stream _n(force_open_stream)
#define force_close_stream _n(force_close_stream)
#define use_prompt_stream _n(use_prompt_stream)

enum STREAM_INDEX {
	STREAM_INDEX_PATHNAME = 0,
	STREAM_INDEX_INFO,
	STREAM_INDEX_INPUT,
	STREAM_INDEX_OUTPUT,
	STREAM_INDEX_SIZE
};

enum StreamType {
	StreamType_BinaryInput = 0,
	StreamType_BinaryOutput,
	StreamType_BinaryIO,
	StreamType_CharacterInput,
	StreamType_CharacterOutput,
	StreamType_CharacterIO,
	StreamType_BincharInput,
	StreamType_BincharOutput,
	StreamType_BincharIO,
	StreamType_Probe,
	/* stream object */
	StreamType_StringInput,
	StreamType_StringOutput,
	StreamType_Synonym,
	StreamType_BroadCast,
	StreamType_Concatenated,
	StreamType_TwoWay,
	StreamType_Echo,
	/* system object */
	StreamType_Prompt,
	StreamType_Pretty,
	StreamType_MemoryInput,
	StreamType_MemoryOutput,
	StreamType_MemoryIO,
	StreamType_Pipe,
	StreamType_Size
};

enum StreamPipe {
	StreamPipe_Input,
	StreamPipe_Output,
	StreamPipe_Error,
	StreamPipe_Size
};

#define Stream_Size  (StreamType_Size + StreamPipe_Size + LISP_STREAM_EXTEND)
#define StreamPipe_Index(x)  (StreamType_Size + (x))
#define StreamExtend_Index(x)  (StreamType_Size + StreamPipe_Size + (x))

struct StructStream {
	unsigned unread_check : 1;
	unsigned closed : 1;
	enum StreamType type;
	unicode unread;
	size_t terpri;
#ifdef __cplusplus
	byte64 data[1];
#else
	byte64 data[0];
#endif
};

#define PtrBodyStream_Low(x)		((void *)PtrBodyABa(x, STREAM_INDEX_SIZE))
#define PtrStructStream_Low(s)		((struct StructStream *)PtrBodyStream(s))
#define PtrDataStream_Low(s)		((void *)(PtrStructStream(s)->data))
#define GetTypeStream_Low(s,v)		(*(v) = PtrStructStream(s)->type)
#define GetIndexStream_Low(s)		((size_t)PtrStructStream(s)->type)

#define GetPathnameStream_Low(s,v)	GetArrayAB((s), STREAM_INDEX_PATHNAME, (v))
#define SetPathnameStream_Low(s,v)	SetArrayAB((s), STREAM_INDEX_PATHNAME, (v))
#define GetInfoStream_Low(s,v)		GetArrayAB((s), STREAM_INDEX_INFO, (v))
#define SetInfoStream_Low(s,v)		SetArrayAB((s), STREAM_INDEX_INFO, (v))
#define GetInputStream_Low(s,v)		GetArrayAB((s), STREAM_INDEX_INPUT, (v))
#define SetInputStream_Low(s,v)		SetArrayAB((s), STREAM_INDEX_INPUT, (v))
#define GetOutputStream_Low(s,v)	GetArrayAB((s), STREAM_INDEX_OUTPUT, (v))
#define SetOutputStream_Low(s,v)	SetArrayAB((s), STREAM_INDEX_OUTPUT, (v))

#ifdef LISP_DEBUG
#define PtrBodyStream(x)			ptrbody_stream(x)
#define PtrStructStream(s)			ptrstruct_stream(s)
#define PtrDataStream(s)			ptrdata_stream(s)
#define GetTypeStream(s,v)			gettype_stream((s), (v))
#define GetIndexStream(s)			getindex_stream(s)
#define GetPathnameStream(s,v)		getpathname_stream((s), (v))
#define SetPathnameStream(s,v)		setpathname_stream((s), (v))
#define GetInfoStream(s,v)			getinfo_stream((s), (v))
#define SetInfoStream(s,v)			setinfo_stream((s), (v))
#define GetInputStream(s,v)			getinput_stream((s), (v))
#define SetInputStream(s,v)			setinput_stream((s), (v))
#define GetOutputStream(s,v)		getoutput_stream((s), (v))
#define SetOutputStream(s,v)		setoutput_stream((s), (v))
#else
#define PtrBodyStream(x)			PtrBodyStream_Low(x)
#define PtrStructStream(s)			PtrStructStream_Low(s)
#define PtrDataStream(s)			PtrDataStream_Low(s)
#define GetTypeStream(s,v)			GetTypeStream_Low((s), (v))
#define GetIndexStream(s)			GetIndexStream_Low(s)
#define GetPathnameStream(s,v)		GetPathnameStream_Low(s,v)
#define SetPathnameStream(s,v)		SetPathnameStream_Low(s,v)
#define GetInfoStream(s,v)			GetInfoStream_Low(s,v)
#define SetInfoStream(s,v)			SetInfoStream_Low(s,v)
#define GetInputStream(s,v)			GetInputStream_Low(s,v)
#define SetInputStream(s,v)			SetInputStream_Low(s,v)
#define GetOutputStream(s,v)		GetOutputStream_Low(s,v)
#define SetOutputStream(s,v)		SetOutputStream_Low(s,v)
#endif

void *ptrbody_stream(addr stream);
struct StructStream *ptrstruct_stream(addr stream);
void *ptrdata_stream(addr stream);
void gettype_stream(addr stream, enum StreamType *ret);
size_t getindex_stream(addr stream);
void getpathname_stream(addr stream, addr *ret);
void setpathname_stream(addr stream, addr value);
void getinfo_stream(addr stream, addr *ret);
void setinfo_stream(addr stream, addr value);
void getinput_stream(addr stream, addr *ret);
void setinput_stream(addr stream, addr value);
void getoutput_stream(addr stream, addr *ret);
void setoutput_stream(addr stream, addr value);

void stream_heap(addr *ret, enum StreamType type, size_t size);
enum StreamType getstreamtype(addr stream);
int streamp(addr stream);
int file_stream_p(addr stream);
int broadcast_stream_p(addr stream);
int concatenated_stream_p(addr stream);
int echo_stream_p(addr stream);
int synonym_stream_p(addr stream);
int twoway_stream_p(addr stream);
int input_string_stream_p(addr stream);
int output_string_stream_p(addr stream);
int string_stream_p(addr stream);
int prompt_stream_p(addr stream);
int pretty_stream_p(addr stream);
int input_memory_stream_p(addr stream);
int output_memory_stream_p(addr stream);
int io_memory_stream_p(addr stream);
int read_memory_stream_p(addr stream);
int write_memory_stream_p(addr stream);
int memory_stream_p(addr stream);
int pipe_stream_p(addr stream);
int extend_stream_p(addr stream);
int extend_type_stream_p(addr stream, int type);

void force_open_stream(addr stream);
void force_close_stream(addr stream);
int use_prompt_stream(Execute ptr, addr stream);

#endif

