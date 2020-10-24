#include "condition.h"
#include "stream_error.h"
#include "typedef.h"

_g int close_stream_error(addr stream, addr *ret)
{
	*ret = Nil;
	return fmte_("The stream ~S don't run close function.", stream, NULL);
}

_g int read_binary_stream_error(addr stream, void *ptr, size_t size, size_t *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run read-binary function.", stream, NULL);
}

_g int readf_binary_stream_error(addr stream, void *ptr, size_t size, size_t *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run readforce-binary function.", stream, NULL);
}

_g int read_byte_stream_error(addr stream, addr *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return fmte_("The stream ~S don't run read-byte function.", stream, NULL);
}

_g int unread_byte_stream_error(addr stream, byte c)
{
	return fmte_("The stream ~S don't run unread-byte function.", stream, NULL);
}

_g int write_binary_stream_error(addr stream, const void *ptr, size_t size, size_t *ret)
{
	return fmte_("The stream ~S don't run write-binary function.", stream, NULL);
}

_g int write_byte_stream_error(addr stream, addr pos)
{
	return fmte_("The stream ~S don't run write-byte function.", stream, NULL);
}

_g int read_char_stream_error(addr stream, unicode *c, int *ret)
{
	return fmte_("The stream ~S don't run read-char function.", stream, NULL);
}

_g int read_hang_stream_error(addr stream, unicode *c, int *hang, int *ret)
{
	*c = 0;
	*hang = 0;
	*ret = 0;
	return fmte_("The stream ~S don't run read-char-no-hang function.", stream, NULL);
}

_g int unread_char_stream_error(addr stream, unicode c)
{
	return fmte_("The stream ~S don't run unread-char function.", stream, NULL);
}

_g int write_char_stream_error(addr stream, unicode c)
{
	return fmte_("The stream ~S don't run write-char function.", stream, NULL);
}

_g int terpri_stream_error(addr stream)
{
	return fmte_("The stream ~S don't run terpri function.", stream, NULL);
}

_g int getleft_stream_error(addr stream, size_t *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run getleft function.", stream, NULL);
}

_g int setleft_stream_error(addr stream, size_t value)
{
	return fmte_("The stream ~S don't run setleft function.", stream, NULL);
}

_g int fresh_line_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run fresh-line function.", stream, NULL);
}

_g int clear_input_stream_error(addr stream)
{
	return fmte_("The stream ~S don't run clear-input function.", stream, NULL);
}

_g int inputp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run inputp function.", stream, NULL);
}

_g int outputp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run outputp function.", stream, NULL);
}

_g int interactivep_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run interactivep function.", stream, NULL);
}

_g int characterp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run characterp function.", stream, NULL);
}

_g int binaryp_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run binaryp function.", stream, NULL);
}

_g int element_type_stream_error(addr stream, addr *ret)
{
	*ret = Nil;
	return fmte_("The stream ~S don't run element-type function.", stream, NULL);
}

_g int file_length_stream_error(addr stream, addr *ret)
{
	*ret = Nil;
	return fmte_("The stream ~S don't run file-length function.", stream, NULL);
}

_g int file_position_stream_error(addr stream, size_t *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return fmte_("The stream ~S don't run file-position function.", stream, NULL);
}

_g int file_position_start_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run file-position-start function.", stream, NULL);
}

_g int file_position_end_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run file-position-end function.", stream, NULL);
}

_g int file_position_set_stream_error(addr stream, size_t value, int *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run file-position-set function.", stream, NULL);
}

_g int file_charlen_stream_error(addr stream, unicode u, size_t *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return fmte_("The stream ~S don't run file-charlen function.", stream, NULL);
}

_g int file_strlen_stream_error(addr stream, addr pos, size_t *value, int *ret)
{
	*value = 0;
	*ret = 0;
	return fmte_("The stream ~S don't run file-strlen function.", stream, NULL);
}

_g int listen_stream_error(addr stream, int *ret)
{
	*ret = 0;
	return fmte_("The stream ~S don't run listen function.", stream, NULL);
}

_g int finish_output_stream_error(addr stream)
{
	return fmte_("The stream ~S don't run finish-output function.", stream, NULL);
}

_g int force_output_stream_error(addr stream)
{
	return fmte_("The stream ~S don't run force-output function.", stream, NULL);
}

_g int clear_output_stream_error(addr stream)
{
	return fmte_("The stream ~S don't run clear-output function.", stream, NULL);
}

_g int exitpoint_stream_error(addr stream)
{
	return fmte_("The stream ~S don't run exitpoint function.", stream, NULL);
}

_g int termsize_stream_error(addr stream, size_t *value, int *ret)
{
	/* return fmte_("The stream ~S don't run termsize function.", stream, NULL); */
	*value = 0;
	return Result(ret, 1);
}

