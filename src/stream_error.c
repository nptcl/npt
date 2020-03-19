#include "condition.h"
#include "stream_error.h"
#include "typedef.h"

_g int close_stream_error(addr stream)
{
	_fmte("The stream ~S don't run close function.", stream, NULL);
	return 1;
}

_g int read_binary_stream_error(addr stream, void *ptr, size_t size, size_t *ret)
{
	_fmte("The stream ~S don't run read-binary function.", stream, NULL);
	return 0;
}

_g int readforce_binary_stream_error(addr stream, void *ptr, size_t size, size_t *ret)
{
	_fmte("The stream ~S don't run readforce-binary function.", stream, NULL);
	return 0;
}

_g int read_byte_stream_error(addr stream, byte *ret)
{
	_fmte("The stream ~S don't run read-byte function.", stream, NULL);
	return 0;
}

_g int unread_byte_stream_error(addr stream, byte c)
{
	_fmte("The stream ~S don't run unread-byte function.", stream, NULL);
	return 0;
}

_g int write_binary_stream_error(addr stream, const void *ptr, size_t size, size_t *ret)
{
	_fmte("The stream ~S don't run write-binary function.", stream, NULL);
	return 0;
}

_g int write_byte_stream_error(addr stream, byte c)
{
	_fmte("The stream ~S don't run write-byte function.", stream, NULL);
	return 0;
}

_g int read_char_stream_error(addr stream, unicode *ret)
{
	_fmte("The stream ~S don't run read-char function.", stream, NULL);
	return 0;
}

_g int read_hang_stream_error(addr stream, unicode *ret, int *hang)
{
	_fmte("The stream ~S don't run read-char-no-hang function.", stream, NULL);
	return 0;
}

_g void unread_char_stream_error(addr stream, unicode c)
{
	_fmte("The stream ~S don't run unread-char function.", stream, NULL);
}

_g void write_char_stream_error(addr stream, unicode c)
{
	_fmte("The stream ~S don't run write-char function.", stream, NULL);
}

_g void terpri_stream_error(addr stream)
{
	_fmte("The stream ~S don't run terpri function.", stream, NULL);
}

_g size_t getleft_stream_error(addr stream)
{
	_fmte("The stream ~S don't run getleft function.", stream, NULL);
	return 0;
}

_g void setleft_stream_error(addr stream, size_t value)
{
	_fmte("The stream ~S don't run setleft function.", stream, NULL);
}

_g int fresh_line_stream_error(addr stream)
{
	_fmte("The stream ~S don't run fresh-line function.", stream, NULL);
	return 0;
}

_g void clear_input_stream_error(addr stream)
{
	_fmte("The stream ~S don't run clear-input function.", stream, NULL);
}

_g int inputp_stream_error(addr stream)
{
	_fmte("The stream ~S don't run input-stream-p function.", stream, NULL);
	return 0;
}

_g int outputp_stream_error(addr stream)
{
	_fmte("The stream ~S don't run output-stream-p function.", stream, NULL);
	return 0;
}

_g int interactivep_stream_error(addr stream)
{
	_fmte("The stream ~S don't run interactive-stream-p function.", stream, NULL);
	return 0;
}

_g int characterp_stream_error(addr stream)
{
	_fmte("The stream ~S don't run character-stream-p function.", stream, NULL);
	return 0;
}

_g int binaryp_stream_error(addr stream)
{
	_fmte("The stream ~S don't run binary-stream-p function.", stream, NULL);
	return 0;
}

_g void element_type_stream_error(addr stream, addr *ret)
{
	_fmte("The stream ~S don't run element-type function.", stream, NULL);
}

_g void file_length_stream_error(addr stream, addr *ret)
{
	_fmte("The stream ~S don't run file-length function.", stream, NULL);
}

_g int file_position_stream_error(addr stream, size_t *ret)
{
	_fmte("The stream ~S don't run file-position function.", stream, NULL);
	return 1;
}

_g int file_position_start_stream_error(addr stream)
{
	_fmte("The stream ~S don't run file-position-start function.", stream, NULL);
	return 1;
}

_g int file_position_end_stream_error(addr stream)
{
	_fmte("The stream ~S don't run file-position-end function.", stream, NULL);
	return 1;
}

_g int file_position_set_stream_error(addr stream, size_t size)
{
	_fmte("The stream ~S don't run file-position-set function.", stream, NULL);
	return 1;
}

_g int file_character_length_stream_error(addr stream, unicode u, size_t *ret)
{
	_fmte("The stream ~S don't run file-character-length function.", stream, NULL);
	return 1;
}

_g int file_string_length_stream_error(addr stream, addr pos, size_t *ret)
{
	_fmte("The stream ~S don't run file-string-length function.", stream, NULL);
	return 1;
}

_g int listen_stream_error(addr stream)
{
	_fmte("The stream ~S don't run listen function.", stream, NULL);
	return 1;
}

_g void finish_output_stream_error(addr stream)
{
	_fmte("The stream ~S don't run finish-output function.", stream, NULL);
}

_g void force_output_stream_error(addr stream)
{
	_fmte("The stream ~S don't run force-output function.", stream, NULL);
}

_g void clear_output_stream_error(addr stream)
{
	_fmte("The stream ~S don't run clear-output function.", stream, NULL);
}

_g void exitpoint_stream_error(addr stream)
{
	_fmte("The stream ~S don't run exitpoint function.", stream, NULL);
}

_g int terminal_width_stream_error(addr stream, size_t *ret)
{
	/* _fmte("The stream ~S don't run terminal-width function.", stream, NULL); */
	return 1;
}

