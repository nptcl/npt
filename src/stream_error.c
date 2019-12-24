#include "condition.h"
#include "stream_error.h"
#include "typedef.h"

_g int close_stream_error(addr stream, int abort)
{
	fmte("The stream ~S don't run close function.", stream, NULL);
	return 1;
}

_g int read_binary_stream_error(addr stream, void *ptr, size_t size, size_t *ret)
{
	fmte("The stream ~S don't run read-binary function.", stream, NULL);
	return 0;
}

_g int readforce_binary_stream_error(addr stream, void *ptr, size_t size, size_t *ret)
{
	fmte("The stream ~S don't run readforce-binary function.", stream, NULL);
	return 0;
}

_g int read_byte_stream_error(addr stream, byte *ret)
{
	fmte("The stream ~S don't run read-byte function.", stream, NULL);
	return 0;
}

_g int unread_byte_stream_error(addr stream, byte c)
{
	fmte("The stream ~S don't run unread-byte function.", stream, NULL);
	return 0;
}

_g int write_binary_stream_error(addr stream, const void *ptr, size_t size, size_t *ret)
{
	fmte("The stream ~S don't run write-binary function.", stream, NULL);
	return 0;
}

_g int write_byte_stream_error(addr stream, byte c)
{
	fmte("The stream ~S don't run write-byte function.", stream, NULL);
	return 0;
}

_g int read_char_stream_error(addr stream, unicode *ret)
{
	fmte("The stream ~S don't run read-char function.", stream, NULL);
	return 0;
}

_g int read_hang_stream_error(addr stream, unicode *ret, int *hang)
{
	fmte("The stream ~S don't run read-char-no-hang function.", stream, NULL);
	return 0;
}

_g void unread_char_stream_error(addr stream, unicode c)
{
	fmte("The stream ~S don't run unread-char function.", stream, NULL);
}

_g void write_char_stream_error(addr stream, unicode c)
{
	fmte("The stream ~S don't run write-char function.", stream, NULL);
}

_g void terpri_stream_error(addr stream)
{
	fmte("The stream ~S don't run terpri function.", stream, NULL);
}

_g int fresh_line_stream_error(addr stream)
{
	fmte("The stream ~S don't run fresh-line function.", stream, NULL);
	return 0;
}

_g int inputp_stream_error(addr stream)
{
	fmte("The stream ~S don't run input-stream-p function.", stream, NULL);
	return 0;
}

_g int outputp_stream_error(addr stream)
{
	fmte("The stream ~S don't run output-stream-p function.", stream, NULL);
	return 0;
}

_g int interactivep_stream_error(addr stream)
{
	fmte("The stream ~S don't run interactive-stream-p function.", stream, NULL);
	return 0;
}

_g int characterp_stream_error(addr stream)
{
	fmte("The stream ~S don't run character-stream-p function.", stream, NULL);
	return 0;
}

_g int binaryp_stream_error(addr stream)
{
	fmte("The stream ~S don't run binary-stream-p function.", stream, NULL);
	return 0;
}

_g void file_length_stream_error(addr stream, addr *ret)
{
	fmte("The stream ~S don't run file-length function.", stream, NULL);
}

_g int file_character_length_stream_error(addr stream, unicode u, size_t *ret)
{
	fmte("The stream ~S don't run file-string-length function.", stream, NULL);
	return 1;
}

_g int file_string_length_stream_error(addr stream, addr pos, size_t *ret)
{
	fmte("The stream ~S don't run file-string-length function.", stream, NULL);
	return 1;
}

_g int listen_stream_error(addr stream)
{
	fmte("The stream ~S don't run listen function.", stream, NULL);
	return 1;
}

_g void clear_input_stream_error(addr stream)
{
	fmte("The stream ~S don't run clear-input function.", stream, NULL);
}

_g void finish_output_stream_error(addr stream)
{
	fmte("The stream ~S don't run finish-output function.", stream, NULL);
}

_g void force_output_stream_error(addr stream)
{
	fmte("The stream ~S don't run force-output function.", stream, NULL);
}

_g void clear_output_stream_error(addr stream)
{
	fmte("The stream ~S don't run clear-output function.", stream, NULL);
}

_g void exitpoint_stream_error(addr stream)
{
	fmte("The stream ~S don't run exitpoint function.", stream, NULL);
}

_g int terminal_width_stream_error(addr stream, size_t *ret)
{
	/* fmte("The stream ~S don't run terminal-width function.", stream, NULL); */
	return 1;
}

