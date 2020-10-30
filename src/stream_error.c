#include "condition.h"
#include "condition_define.h"
#include "stream_error.h"
#include "typedef.h"
#include "type_table.h"

static int stream_type_error(const char *str, addr stream)
{
	addr type;
	GetTypeTable(&type, Stream);
	return call_type_error_va_(NULL, stream, type, str, stream, NULL);
}

_g int close_stream_error(addr stream, addr *ret)
{
	*ret = Nil;
	return stream_type_error(
			"The stream ~S don't run close function.", stream);
}

_g int read_byte_stream_error(addr stream, addr *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run read-byte function.", stream);
}

_g int unread_byte_stream_error(addr stream, byte c)
{
	return stream_type_error(
			"The stream ~S don't run unread-byte function.", stream);
}

_g int write_byte_stream_error(addr stream, addr pos)
{
	return stream_type_error(
			"The stream ~S don't run write-byte function.", stream);
}

_g int read_char_stream_error(addr stream, unicode *c, int *ret)
{
	return stream_type_error(
			"The stream ~S don't run read-char function.", stream);
}

_g int read_hang_stream_error(addr stream, unicode *c, int *hang, int *ret)
{
	*c = 0;
	*hang = 0;
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run read-char-no-hang function.", stream);
}

_g int unread_char_stream_error(addr stream, unicode c)
{
	return stream_type_error(
			"The stream ~S don't run unread-char function.", stream);
}

_g int write_char_stream_error(addr stream, unicode c)
{
	return stream_type_error(
			"The stream ~S don't run write-char function.", stream);
}

_g int getleft_stream_error(addr stream, size_t *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run getleft function.", stream);
}

_g int setleft_stream_error(addr stream, size_t value)
{
	return stream_type_error(
			"The stream ~S don't run setleft function.", stream);
}

_g int inputp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run inputp function.", stream);
}

_g int outputp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run outputp function.", stream);
}

_g int interactivep_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run interactivep function.", stream);
}

_g int characterp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run characterp function.", stream);
}

_g int binaryp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run binaryp function.", stream);
}

_g int element_type_stream_error(addr stream, addr *ret)
{
	*ret = Nil;
	return stream_type_error(
			"The stream ~S don't run element-type function.", stream);
}

_g int external_format_stream_error(addr stream, addr *ret)
{
	*ret = Nil;
	return stream_type_error(
			"The stream ~S don't run external_format function.", stream);
}

_g int file_length_stream_error(addr stream, addr *ret)
{
	*ret = Nil;
	return stream_type_error(
			"The stream ~S don't run file-length function.", stream);
}

_g int file_position_stream_error(addr stream, size_t *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-position function.", stream);
}

_g int file_position_start_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-position-start function.", stream);
}

_g int file_position_end_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-position-end function.", stream);
}

_g int file_position_set_stream_error(addr stream, size_t value, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-position-set function.", stream);
}

_g int file_charlen_stream_error(addr stream, unicode u, size_t *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-charlen function.", stream);
}

_g int file_strlen_stream_error(addr stream, addr pos, size_t *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run file-strlen function.", stream);
}

_g int listen_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return stream_type_error(
			"The stream ~S don't run listen function.", stream);
}

_g int clear_input_stream_error(addr stream)
{
	return stream_type_error(
			"The stream ~S don't run clear-input function.", stream);
}

_g int finish_output_stream_error(addr stream)
{
	return stream_type_error(
			"The stream ~S don't run finish-output function.", stream);
}

_g int force_output_stream_error(addr stream)
{
	return stream_type_error(
			"The stream ~S don't run force-output function.", stream);
}

_g int clear_output_stream_error(addr stream)
{
	return stream_type_error(
			"The stream ~S don't run clear-output function.", stream);
}

_g int exitpoint_stream_error(addr stream)
{
	return stream_type_error(
			"The stream ~S don't run exitpoint function.", stream);
}

_g int termsize_stream_error(addr stream, size_t *value, int *ret)
{
	/* return stream_type_error(
	 *    "The stream ~S don't run termsize function.", stream);
	 */
	*value = 0;
	return Result(ret, 1);
}

