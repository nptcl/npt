#include "condition.h"
#include "stream.h"
#include "stream_function.h"
#include "stream_object.h"
#include "stream_variable.h"
#include "typedef.h"

#define CheckStream(stream, ptr) { \
	CheckType(stream, LISPTYPE_STREAM); \
	ptr = PtrStructStream(stream); \
	if (ptr->closed) { \
		return fmte_("The stream ~S is already closed.", stream, NULL); \
	} \
}

_g int close_stream_(addr stream, addr *ret)
{
	struct StructStream *ptr;
	addr pos;

	CheckType(stream, LISPTYPE_STREAM);
	ptr = PtrStructStream(stream);
	Return((Stream_close[ptr->type])(stream, &pos));
	force_close_stream(stream);
	if (ret)
		*ret = pos;

	return 0;
}

_g int read_binary_stream_(addr stream, void *pos, size_t size, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_binary[(int)ptr->type])(stream, pos, size, ret);
}

_g int readf_binary_stream_(addr stream, void *pos, size_t size, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_readf_binary[(int)ptr->type])(stream, pos, size, ret);
}

_g int read_byte_stream_(addr stream, byte *c, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_byte[(int)ptr->type])(stream, c, ret);
}

_g int unread_byte_stream_(addr stream, byte c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_unread_byte[(int)ptr->type])(stream, c);
}

_g int write_binary_stream_(addr stream, const void *pos, size_t size, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_write_binary[(int)ptr->type])(stream, pos, size, ret);
}

_g int write_byte_stream_(addr stream, byte c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_write_byte[(int)ptr->type])(stream, c);
}

_g int read_char_stream_(addr stream, unicode *c, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_char[(int)ptr->type])(stream, c, ret);
}

_g int read_hang_stream_(addr stream, unicode *c, int *hang, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_read_hang[(int)ptr->type])(stream, c, hang, ret);
}

_g int unread_char_stream_(addr stream, unicode c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_unread_char[(int)ptr->type])(stream, c);
}

_g int write_char_stream_(addr stream, unicode c)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_write_char[(int)ptr->type])(stream, c);
}

_g int terpri_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_terpri[(int)ptr->type])(stream);
}

_g int getleft_stream_(addr stream, size_t *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_getleft[(int)ptr->type])(stream, ret);
}

_g int setleft_stream_(addr stream, size_t value)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_setleft[(int)ptr->type])(stream, value);
}

_g int fresh_line_stream_(addr stream, int *ret)
{
	int check;
	struct StructStream *ptr;

	CheckStream(stream, ptr);
	Return((Stream_fresh_line[(int)ptr->type])(stream, &check));
	if (ret)
		*ret = check;

	return 0;
}

_g int clear_input_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_clear_input[(int)ptr->type])(stream);
}

_g int inputp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_inputp[GetIndexStream(stream)])(stream, ret);
}

_g int outputp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_outputp[GetIndexStream(stream)])(stream, ret);
}

_g int interactivep_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_interactivep[GetIndexStream(stream)])(stream, ret);
}

_g int characterp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_characterp[GetIndexStream(stream)])(stream, ret);
}

_g int binaryp_stream_(addr stream, int *ret)
{
	CheckType(stream, LISPTYPE_STREAM);
	return (Stream_binaryp[GetIndexStream(stream)])(stream, ret);
}

_g int element_type_stream_(addr stream, addr *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_element_type[(int)ptr->type])(stream, ret);
}

_g int file_length_stream_(addr stream, addr *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_length[(int)ptr->type])(stream, ret);
}

_g int file_position_stream_(addr stream, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position[(int)ptr->type])(stream, value, ret);
}

_g int file_position_start_stream_(addr stream, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_start[(int)ptr->type])(stream, ret);
}

_g int file_position_end_stream_(addr stream, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_end[(int)ptr->type])(stream, ret);
}

_g int file_position_set_stream_(addr stream, size_t value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_position_set[(int)ptr->type])(stream, value, ret);
}

_g int file_charlen_stream_(addr stream, unicode u, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_charlen[(int)ptr->type])(stream, u, value, ret);
}

_g int file_strlen_stream_(addr stream, addr pos, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_file_strlen[(int)ptr->type])(stream, pos, value, ret);
}

_g int listen_stream_(addr stream, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_listen[(int)ptr->type])(stream, ret);
}

_g int finish_output_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_finish_output[(int)ptr->type])(stream);
}

_g int force_output_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_force_output[(int)ptr->type])(stream);
}

_g int clear_output_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_clear_output[(int)ptr->type])(stream);
}

_g int exitpoint_stream_(addr stream)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_exitpoint[(int)ptr->type])(stream);
}

_g int termsize_stream_(addr stream, size_t *value, int *ret)
{
	struct StructStream *ptr;
	CheckStream(stream, ptr);
	return (Stream_termsize[(int)ptr->type])(stream, value, ret);
}

