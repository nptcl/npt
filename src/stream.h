#ifndef __STREAM_HEADER__
#define __STREAM_HEADER__

#include "build.h"
#include "file_type.h"
#include "file_memory.h"
#include "memory.h"
#include "stream_init.h"
#include "typedef_stream.h"

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
#define extend_stream_p _n(extend_stream_p)
#define extend_type_stream_p _n(extend_type_stream_p)
#define open_stream_p _n(open_stream_p)
#define force_open_stream _n(force_open_stream)
#define force_close_stream _n(force_close_stream)
#define close_stream_common _n(close_stream_common)
#define close_stream_ _n(close_stream_)
#define terpri_stream_ _n(terpri_stream_)
#define getleft_stream_ _n(getleft_stream_)
#define setleft_stream_ _n(setleft_stream_)
#define copyleft_stream_ _n(copyleft_stream_)
#define pageout_stream_ _n(pageout_stream_)
#define print_ascii_stream_ _n(print_ascii_stream_)
#define print_unicode_stream_ _n(print_unicode_stream_)
#define print_string_stream_ _n(print_string_stream_)
#define standard_input_stream_ _n(standard_input_stream_)
#define standard_output_stream_ _n(standard_output_stream_)
#define error_output_stream_ _n(error_output_stream_)
#define trace_output_stream_ _n(trace_output_stream_)
#define terminal_io_stream_ _n(terminal_io_stream_)
#define debug_io_stream_ _n(debug_io_stream_)
#define query_io_stream_ _n(query_io_stream_)
#define output_stream_designer_ _n(output_stream_designer_)
#define read_binary_stream_ _n(read_binary_stream_)
#define readf_binary_stream_ _n(readf_binary_stream_)
#define read_byte_stream_ _n(read_byte_stream_)
#define unread_byte_stream_ _n(unread_byte_stream_)
#define write_binary_stream_ _n(write_binary_stream_)
#define write_byte_stream_ _n(write_byte_stream_)
#define read_char_stream_ _n(read_char_stream_)
#define read_hang_stream_ _n(read_hang_stream_)
#define unread_char_stream_ _n(unread_char_stream_)
#define write_char_stream_ _n(write_char_stream_)
#define fresh_line_stream_ _n(fresh_line_stream_)
#define clear_input_stream_ _n(clear_input_stream_)
#define inputp_stream_ _n(inputp_stream_)
#define outputp_stream_ _n(outputp_stream_)
#define interactivep_stream_ _n(interactivep_stream_)
#define characterp_stream_ _n(characterp_stream_)
#define binaryp_stream_ _n(binaryp_stream_)
#define element_type_stream_ _n(element_type_stream_)
#define file_length_stream_ _n(file_length_stream_)
#define file_position_stream_ _n(file_position_stream_)
#define file_position_start_stream_ _n(file_position_start_stream_)
#define file_position_end_stream_ _n(file_position_end_stream_)
#define file_position_set_stream_ _n(file_position_set_stream_)
#define file_charlen_stream_ _n(file_charlen_stream_)
#define file_strlen_stream_ _n(file_strlen_stream_)
#define listen_stream_ _n(listen_stream_)
#define finish_output_stream_ _n(finish_output_stream_)
#define force_output_stream_ _n(force_output_stream_)
#define clear_output_stream_ _n(clear_output_stream_)
#define exitpoint_stream_ _n(exitpoint_stream_)
#define termsize_stream_ _n(termsize_stream_)
#define close_default_stream _n(close_default_stream)
#define read_char_default_stream _n(read_char_default_stream)
#define read_hang_default_stream _n(read_hang_default_stream)
#define unread_char_default_stream _n(unread_char_default_stream)
#define write_char_default_stream _n(write_char_default_stream)
#define terpri_default_stream _n(terpri_default_stream)
#define getleft_default_stream _n(getleft_default_stream)
#define setleft_default_stream _n(setleft_default_stream)
#define charleft_default_stream _n(charleft_default_stream)
#define fresh_line_default_stream _n(fresh_line_default_stream)
#define checkp_true_stream _n(checkp_true_stream)
#define checkp_false_stream _n(checkp_false_stream)
#define element_type_character_stream _n(element_type_character_stream)
#define element_type_binary_stream _n(element_type_binary_stream)
#define element_type_io_stream _n(element_type_io_stream)
#define file_length_default_stream _n(file_length_default_stream)
#define file_position_default_stream _n(file_position_default_stream)
#define file_position_start_default_stream _n(file_position_start_default_stream)
#define file_position_end_default_stream _n(file_position_end_default_stream)
#define file_position_set_default_stream _n(file_position_set_default_stream)
#define finish_output_default_stream _n(finish_output_default_stream)
#define force_output_default_stream _n(force_output_default_stream)
#define clear_output_default_stream _n(clear_output_default_stream)
#define exitpoint_default_stream _n(exitpoint_default_stream)
#define termsize_default_stream _n(termsize_default_stream)
#define open_stream_ _n(open_stream_)
#define stream_designer_ _n(stream_designer_)
#define peek_char_stream_ _n(peek_char_stream_)
#define read_line_stream_ _n(read_line_stream_)
#define write_string_stream _n(write_string_stream)
#define read_sequence_stream _n(read_sequence_stream)
#define write_sequence_stream _n(write_sequence_stream)
#define update_standard_stream _n(update_standard_stream)
#define save_stream _n(save_stream)

#define Stream_binaryp _n(Stream_binaryp)
#define Stream_characterp _n(Stream_characterp)
#define Stream_clear_input _n(Stream_clear_input)
#define Stream_clear_output _n(Stream_clear_output)
#define Stream_close _n(Stream_close)
#define Stream_element_type _n(Stream_element_type)
#define Stream_exitpoint _n(Stream_exitpoint)
#define Stream_file_charlen _n(Stream_file_charlen)
#define Stream_file_length _n(Stream_file_length)
#define Stream_file_position _n(Stream_file_position)
#define Stream_file_position_end _n(Stream_file_position_end)
#define Stream_file_position_set _n(Stream_file_position_set)
#define Stream_file_position_start _n(Stream_file_position_start)
#define Stream_file_strlen _n(Stream_file_strlen)
#define Stream_finish_output _n(Stream_finish_output)
#define Stream_force_output _n(Stream_force_output)
#define Stream_fresh_line _n(Stream_fresh_line)
#define Stream_getleft _n(Stream_getleft)
#define Stream_inputp _n(Stream_inputp)
#define Stream_interactivep _n(Stream_interactivep)
#define Stream_listen _n(Stream_listen)
#define Stream_outputp _n(Stream_outputp)
#define Stream_read_binary _n(Stream_read_binary)
#define Stream_read_byte _n(Stream_read_byte)
#define Stream_read_char _n(Stream_read_char)
#define Stream_read_hang _n(Stream_read_hang)
#define Stream_readf_binary _n(Stream_readf_binary)
#define Stream_setleft _n(Stream_setleft)
#define Stream_termsize _n(Stream_termsize)
#define Stream_terpri _n(Stream_terpri)
#define Stream_unread_byte _n(Stream_unread_byte)
#define Stream_unread_char _n(Stream_unread_char)
#define Stream_write_binary _n(Stream_write_binary)
#define Stream_write_byte _n(Stream_write_byte)
#define Stream_write_char _n(Stream_write_char)

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
	StreamType_Pretty,
	StreamType_Size
};

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

_g void *ptrbody_stream(addr stream);
_g struct StructStream *ptrstruct_stream(addr stream);
_g void *ptrdata_stream(addr stream);
_g void gettype_stream(addr stream, enum StreamType *ret);
_g size_t getindex_stream(addr stream);
_g void getpathname_stream(addr stream, addr *ret);
_g void setpathname_stream(addr stream, addr value);
_g void getinfo_stream(addr stream, addr *ret);
_g void setinfo_stream(addr stream, addr value);
_g void getinput_stream(addr stream, addr *ret);
_g void setinput_stream(addr stream, addr value);
_g void getoutput_stream(addr stream, addr *ret);
_g void setoutput_stream(addr stream, addr value);

_g void stream_heap(addr *ret, enum StreamType type, size_t size);
_g enum StreamType getstreamtype(addr stream);
_g int streamp(addr stream);
_g int file_stream_p(addr stream);
_g int broadcast_stream_p(addr stream);
_g int concatenated_stream_p(addr stream);
_g int echo_stream_p(addr stream);
_g int synonym_stream_p(addr stream);
_g int twoway_stream_p(addr stream);
_g int input_string_stream_p(addr stream);
_g int output_string_stream_p(addr stream);
_g int string_stream_p(addr stream);
_g int prompt_stream_p(addr stream);
_g int pretty_stream_p(addr stream);
_g int extend_stream_p(addr stream);
_g int extend_type_stream_p(addr stream, int type);

_g int open_stream_p(addr stream);
_g void force_open_stream(addr stream);
_g void force_close_stream(addr stream);
_g int close_stream_common(addr stream, addr *ret);
_g int close_stream_(addr stream);
_g int terpri_stream_(addr stream);
_g int getleft_stream_(addr stream, size_t *ret);
_g int setleft_stream_(addr stream, size_t value);
_g int copyleft_stream_(addr stream, addr src);
_g int pageout_stream_(addr stream);
_g int print_ascii_stream_(addr stream, const char *data);
_g int print_unicode_stream_(addr stream, const unicode *data);
_g int print_string_stream_(addr stream, addr pos);

_g int standard_input_stream_(Execute ptr, addr *ret);
_g int standard_output_stream_(Execute ptr, addr *ret);
_g int error_output_stream_(Execute ptr, addr *ret);
_g int trace_output_stream_(Execute ptr, addr *ret);
_g int terminal_io_stream_(Execute ptr, addr *ret);
_g int debug_io_stream_(Execute ptr, addr *ret);
_g int query_io_stream_(Execute ptr, addr *ret);
_g int output_stream_designer_(Execute ptr, addr stream, addr *ret);

/* function */
#define DefineStreamLet(x,y,z) Stream_##y[StreamType_##x] = y##_##z;
#define DefineStreamSet(x,y) Stream_##y[StreamType_##x] = y##_##x;
#define DefineStreamErr(x,y) Stream_##y[StreamType_##x] = y##_stream_error;
#define DefineStream___(x,y) Stream_##y[StreamType_##x] = y##_stream_error;
#define DefineStreamDef(x,y) Stream_##y[StreamType_##x] = y##_default_stream;
#define DefineStreamEql(x,y,z) Stream_##y[StreamType_##x] = z;
#define DefineStreamChk(x,y,z) Stream_##y[StreamType_##x] = checkp_##z##_stream;

#define StreamType_Array	(StreamType_Size + LISP_STREAM_EXTEND)
__extern lisp_streamtype_close Stream_close[StreamType_Array];
__extern lisp_streamtype_read_binary Stream_read_binary[StreamType_Array];
__extern lisp_streamtype_readf_binary Stream_readf_binary[StreamType_Array];
__extern lisp_streamtype_read_byte Stream_read_byte[StreamType_Array];
__extern lisp_streamtype_unread_byte Stream_unread_byte[StreamType_Array];
__extern lisp_streamtype_write_binary Stream_write_binary[StreamType_Array];
__extern lisp_streamtype_write_byte Stream_write_byte[StreamType_Array];
__extern lisp_streamtype_read_char Stream_read_char[StreamType_Array];
__extern lisp_streamtype_read_hang Stream_read_hang[StreamType_Array];
__extern lisp_streamtype_unread_char Stream_unread_char[StreamType_Array];
__extern lisp_streamtype_write_char Stream_write_char[StreamType_Array];
__extern lisp_streamtype_terpri Stream_terpri[StreamType_Array];
__extern lisp_streamtype_getleft Stream_getleft[StreamType_Array];
__extern lisp_streamtype_setleft Stream_setleft[StreamType_Array];
__extern lisp_streamtype_fresh_line Stream_fresh_line[StreamType_Array];
__extern lisp_streamtype_clear_input Stream_clear_input[StreamType_Array];
__extern lisp_streamtype_inputp Stream_inputp[StreamType_Array];
__extern lisp_streamtype_outputp Stream_outputp[StreamType_Array];
__extern lisp_streamtype_interactivep Stream_interactivep[StreamType_Array];
__extern lisp_streamtype_characterp Stream_characterp[StreamType_Array];
__extern lisp_streamtype_binaryp Stream_binaryp[StreamType_Array];
__extern lisp_streamtype_element_type Stream_element_type[StreamType_Array];
__extern lisp_streamtype_file_length Stream_file_length[StreamType_Array];
__extern lisp_streamtype_file_position Stream_file_position[StreamType_Array];
__extern lisp_streamtype_file_position_start Stream_file_position_start[StreamType_Array];
__extern lisp_streamtype_file_position_end Stream_file_position_end[StreamType_Array];
__extern lisp_streamtype_file_position_set Stream_file_position_set[StreamType_Array];
__extern lisp_streamtype_file_charlen Stream_file_charlen[StreamType_Array];
__extern lisp_streamtype_file_strlen Stream_file_strlen[StreamType_Array];
__extern lisp_streamtype_listen Stream_listen[StreamType_Array];
__extern lisp_streamtype_finish_output Stream_finish_output[StreamType_Array];
__extern lisp_streamtype_force_output Stream_force_output[StreamType_Array];
__extern lisp_streamtype_clear_output Stream_clear_output[StreamType_Array];
__extern lisp_streamtype_exitpoint Stream_exitpoint[StreamType_Array];
__extern lisp_streamtype_termsize Stream_termsize[StreamType_Array];

_g int read_binary_stream_(addr stream, void *pos, size_t size, size_t *ret);
_g int readf_binary_stream_(addr stream, void *pos, size_t size, size_t *ret);
_g int read_byte_stream_(addr stream, byte *c, int *ret);
_g int unread_byte_stream_(addr stream, byte c);
_g int write_binary_stream_(addr stream, const void *pos, size_t size, size_t *ret);
_g int write_byte_stream_(addr stream, byte c);
_g int read_char_stream_(addr stream, unicode *c, int *ret);
_g int read_hang_stream_(addr stream, unicode *c, int *hang, int *ret);
_g int unread_char_stream_(addr stream, unicode c);
_g int write_char_stream_(addr stream, unicode c);
_g int fresh_line_stream_(addr stream, int *ret);
_g int clear_input_stream_(addr stream);
_g int inputp_stream_(addr stream, int *ret);
_g int outputp_stream_(addr stream, int *ret);
_g int interactivep_stream_(addr stream, int *ret);
_g int characterp_stream_(addr stream, int *ret);
_g int binaryp_stream_(addr stream, int *ret);
_g int element_type_stream_(addr stream, addr *ret);
_g int file_length_stream_(addr stream, addr *ret);
_g int file_position_stream_(addr stream, size_t *value, int *ret);
_g int file_position_start_stream_(addr stream, int *ret);
_g int file_position_end_stream_(addr stream, int *ret);
_g int file_position_set_stream_(addr stream, size_t value, int *ret);
_g int file_charlen_stream_(addr stream, unicode u, size_t *value, int *ret);
_g int file_strlen_stream_(addr stream, addr pos, size_t *value, int *ret);
_g int listen_stream_(addr stream, int *ret);
_g int finish_output_stream_(addr stream);
_g int force_output_stream_(addr stream);
_g int clear_output_stream_(addr stream);
_g int exitpoint_stream_(addr stream);
_g int termsize_stream_(addr stream, size_t *value, int *ret);

_g int close_default_stream(addr stream, addr *ret);
_g int read_char_default_stream(addr stream, unicode *c, int *ret);
_g int read_hang_default_stream(addr stream, unicode *c, int *hang, int *ret);
_g int unread_char_default_stream(addr stream, unicode c);
_g int write_char_default_stream(addr stream, unicode c);
_g int terpri_default_stream(addr stream);
_g int getleft_default_stream(addr stream, size_t *ret);
_g int setleft_default_stream(addr stream, size_t value);
_g void charleft_default_stream(addr stream, unicode c);
_g int fresh_line_default_stream(addr stream, int *ret);
_g int checkp_true_stream(addr stream, int *ret);
_g int checkp_false_stream(addr stream, int *ret);
_g int element_type_character_stream(addr stream, addr *ret);
_g int element_type_binary_stream(addr stream, addr *ret);
_g int element_type_io_stream(addr stream, addr *ret);
_g int file_length_default_stream(addr stream, addr *ret);
_g int file_position_default_stream(addr stream, size_t *value, int *ret);
_g int file_position_start_default_stream(addr stream, int *ret);
_g int file_position_end_default_stream(addr stream, int *ret);
_g int file_position_set_default_stream(addr stream, size_t value, int *ret);
_g int finish_output_default_stream(addr stream);
_g int force_output_default_stream(addr stream);
_g int clear_output_default_stream(addr stream);
_g int exitpoint_default_stream(addr stream);
_g int termsize_default_stream(addr stream, size_t *value, int *ret);


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

_g int open_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Direction direction,
		enum Stream_Open_Element element,
		enum Stream_Open_IfExists exist,
		enum Stream_Open_IfDoesNot doesnot,
		enum Stream_Open_External external);
_g int stream_designer_(Execute ptr, addr pos, addr *ret, int inputp);
_g int peek_char_stream_(Execute ptr, addr *ret,
		addr type, addr stream, int errorp, addr value, int recp);
_g int read_line_stream_(Execute ptr, addr *ret, int *miss,
		addr pos, int errorp, addr value, int recp);
_g int write_string_stream(Execute ptr, addr string, addr rest, addr *ret);
_g int read_sequence_stream(addr *ret, addr seq, addr stream, size_t start, size_t end);
_g int write_sequence_stream(LocalRoot local,
		addr seq, addr stream, size_t start, size_t end);

/* core */
_g void update_standard_stream(void);
_g int save_stream(addr pos);

#endif

