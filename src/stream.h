#ifndef __STREAM_HEADER__
#define __STREAM_HEADER__

#include "build.h"
#include "file_type.h"
#include "memory.h"

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
	StreamType_Size
};

struct StructStream {
	unsigned unread_check : 1;
	unsigned closed : 1;
	enum StreamType type;
	unicode unread;
	size_t terpri;
	byte64 data[]; /* alignment 8byte */
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

void stream_alloc(LocalRoot local, addr *ret, enum StreamType type, size_t size);
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

int open_stream_p(addr stream);
int closep_stream(addr stream);
void close_stream(addr stream);
void terpri_stream(addr stream);
void pageout_stream(addr stream);
size_t terpri_position_stream(addr stream);
void print_ascii_stream(addr stream, const char *data);
void print_unicode_stream(addr stream, const unicode *data);
void print_string_stream(addr stream, addr pos);

void init_stream(void);
void build_stream(void);
void standard_input_stream(Execute ptr, addr *ret);
void standard_output_stream(Execute ptr, addr *ret);
void error_output_stream(Execute ptr, addr *ret);
void trace_output_stream(Execute ptr, addr *ret);
void terminal_io_stream(Execute ptr, addr *ret);
void debug_io_stream(Execute ptr, addr *ret);
void query_io_stream(Execute ptr, addr *ret);

/* function */
#define DefineStreamLet(x,y,z) Stream_##y[StreamType_##x] = y##_##z;
#define DefineStreamSet(x,y) Stream_##y[StreamType_##x] = y##_##x;
#define DefineStreamErr(x,y) Stream_##y[StreamType_##x] = y##_stream_error;
#define DefineStream___(x,y) Stream_##y[StreamType_##x] = y##_stream_error;
#define DefineStreamDef(x,y) Stream_##y[StreamType_##x] = y##_default_stream;
#define DefineStreamEql(x,y,z) Stream_##y[StreamType_##x] = z;
#define DefineStreamChk(x,y,z) Stream_##y[StreamType_##x] = checkp_##z##_stream;

extern int (*Stream_close[StreamType_Size])(addr, int);
extern int (*Stream_read_binary[StreamType_Size])(addr, void *, size_t, size_t *);
extern int (*Stream_readforce_binary[StreamType_Size])(addr, void *, size_t, size_t *);
extern int (*Stream_read_byte[StreamType_Size])(addr, byte *);
extern int (*Stream_unread_byte[StreamType_Size])(addr, byte);
extern int (*Stream_write_binary[StreamType_Size])(addr, const void *, size_t, size_t *);
extern int (*Stream_write_byte[StreamType_Size])(addr, byte);
extern int (*Stream_read_char[StreamType_Size])(addr, unicode *);
extern int (*Stream_read_hang[StreamType_Size])(addr, unicode *, int *);
extern void (*Stream_unread_char[StreamType_Size])(addr, unicode);
extern void (*Stream_write_char[StreamType_Size])(addr, unicode);
extern int (*Stream_fresh_line[StreamType_Size])(addr);
extern void (*Stream_clear_input[StreamType_Size])(addr);
extern int (*Stream_inputp[StreamType_Size])(addr);
extern int (*Stream_outputp[StreamType_Size])(addr);
extern int (*Stream_interactivep[StreamType_Size])(addr);
extern int (*Stream_characterp[StreamType_Size])(addr);
extern int (*Stream_binaryp[StreamType_Size])(addr);
extern void (*Stream_element_type[StreamType_Size])(addr, addr *);
extern void (*Stream_file_length[StreamType_Size])(addr, addr *);
extern int (*Stream_file_position[StreamType_Size])(addr, size_t *);
extern int (*Stream_file_position_start[StreamType_Size])(addr);
extern int (*Stream_file_position_end[StreamType_Size])(addr);
extern int (*Stream_file_position_set[StreamType_Size])(addr, size_t);
extern int (*Stream_file_character_length[StreamType_Size])(addr, unicode, size_t *);
extern int (*Stream_file_string_length[StreamType_Size])(addr, addr, size_t *);
extern int (*Stream_listen[StreamType_Size])(addr);
extern void (*Stream_finish_output[StreamType_Size])(addr);
extern void (*Stream_force_output[StreamType_Size])(addr);
extern void (*Stream_clear_output[StreamType_Size])(addr);

int close_abort_stream(addr stream, int abort);
int read_binary_stream(addr stream, void *pos, size_t size, size_t *ret);
int readforce_binary_stream(addr stream, void *pos, size_t size, size_t *ret);
int read_byte_stream(addr stream, byte *c);
int unread_byte_stream(addr stream, byte c);
int write_binary_stream(addr stream, const void *pos, size_t size, size_t *ret);
int write_byte_stream(addr stream, byte c);
int read_char_stream(addr stream, unicode *c);
int read_hang_stream(addr stream, unicode *c, int *hang);
void unread_char_stream(addr stream, unicode c);
void write_char_stream(addr stream, unicode c);
int fresh_line_stream(addr stream);
void clear_input_stream(addr stream);
int inputp_stream(addr stream);
int outputp_stream(addr stream);
int interactivep_stream(addr stream);
int characterp_stream(addr stream);
int binaryp_stream(addr stream);
void element_type_stream(addr stream, addr *ret);
void file_length_stream(addr stream, addr *ret);
int file_position_stream(addr stream, size_t *ret);
int file_position_start_stream(addr stream);
int file_position_end_stream(addr stream);
int file_position_set_stream(addr stream, size_t pos);
int file_character_length_stream(addr stream, unicode u, size_t *ret);
int file_string_length_stream(addr stream, addr pos, size_t *ret);
int listen_stream(addr stream);
void finish_output_stream(addr stream);
void force_output_stream(addr stream);
void clear_output_stream(addr stream);

int close_default_stream(addr stream, int abort);
int read_char_default_stream(addr stream, unicode *c);
int read_hang_default_stream(addr stream, unicode *c, int *hang);
void unread_char_default_stream(addr stream, unicode c);
void write_char_default_stream(addr stream, unicode c);
int fresh_line_default_stream(addr stream);
int checkp_true_stream(addr stream);
int checkp_false_stream(addr stream);
void element_type_character_stream(addr stream, addr *ret);
void element_type_binary_stream(addr stream, addr *ret);
void element_type_io_stream(addr stream, addr *ret);
void file_length_default_stream(addr stream, addr *ret);
int file_position_default_stream(addr stream, size_t *ret);
int file_position_start_default_stream(addr stream);
int file_position_end_default_stream(addr stream);
int file_position_set_default_stream(addr stream, size_t pos);
void finish_output_default_stream(addr stream);
void force_output_default_stream(addr stream);
void clear_output_default_stream(addr stream);


/*
 *  common-lisp
 */
enum Stream_Open_Direction {
	Stream_Open_Direction_Input,
	Stream_Open_Direction_Output,
	Stream_Open_Direction_Io,
	Stream_Open_Direction_Probe
};

enum Stream_Open_Element {
	Stream_Open_Element_Character,
	Stream_Open_Element_Binary
};

enum Stream_Open_IfExists {
	Stream_Open_IfExists_Error,
	Stream_Open_IfExists_NewVersion,
	Stream_Open_IfExists_Rename,
	Stream_Open_IfExists_RenameAndDelete,
	Stream_Open_IfExists_Overwrite,
	Stream_Open_IfExists_Append,
	Stream_Open_IfExists_Supersede,
	Stream_Open_IfExists_Nil
};

enum Stream_Open_IfDoesNot {
	Stream_Open_IfDoesNot_Create,
	Stream_Open_IfDoesNot_Error,
	Stream_Open_IfDoesNot_Nil
};

enum Stream_Open_External {
	Stream_Open_External_Default,
	Stream_Open_External_Ascii,
	Stream_Open_External_Utf8,
	Stream_Open_External_Utf8Bom,
	Stream_Open_External_Utf16,
	Stream_Open_External_Utf16Le,
	Stream_Open_External_Utf16Be,
	Stream_Open_External_Utf16LeBom,
	Stream_Open_External_Utf16BeBom,
	Stream_Open_External_Utf32,
	Stream_Open_External_Utf32Le,
	Stream_Open_External_Utf32Be,
	Stream_Open_External_Utf32LeBom,
	Stream_Open_External_Utf32BeBom
};

void open_stream(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Direction direction,
		enum Stream_Open_Element element,
		enum Stream_Open_IfExists exist,
		enum Stream_Open_IfDoesNot doesnot,
		enum Stream_Open_External external);
void stream_designer(Execute ptr, addr pos, addr *ret, int inputp);
void read_byte_common(addr *ret, addr stream, int errorp, addr value);
void write_byte_common(addr stream, addr value);
void peek_char_common(Execute ptr, addr *ret,
		addr type, addr stream, int errorp, addr value, int recp);
void read_char_no_hang_common(Execute ptr, addr *ret,
		addr pos, int errorp, addr value, int recp);
void read_char_common(Execute ptr, addr *ret,
		addr pos, int errorp, addr value, int recp);
void read_line_common(Execute ptr, addr *ret, int *miss,
		addr pos, int errorp, addr value, int recp);
void write_string_common(Execute ptr, addr string, addr rest);
void write_line_common(Execute ptr, addr string, addr rest);
void read_sequence_common(addr *ret, addr seq, addr stream, size_t start, size_t end);
void write_sequence_common(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end);
int yes_or_no_p_common(Execute ptr, addr args, int exactp, int *ret);

#endif

