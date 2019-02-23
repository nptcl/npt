#include "condition.h"
#include "stream_error.h"
#include "typedef.h"

int close_stream_error(addr stream, int abort)
{
	fmte("The stream ~S don't run close function.", stream, NULL);
	return 1;
}

int read_binary_stream_error(addr stream, void *ptr, size_t size, size_t *ret)
{
	fmte("The stream ~S don't run read-binary function.", stream, NULL);
	return 0;
}

int readforce_binary_stream_error(addr stream, void *ptr, size_t size, size_t *ret)
{
	fmte("The stream ~S don't run readforce-binary function.", stream, NULL);
	return 0;
}

int read_byte_stream_error(addr stream, byte *ret)
{
	fmte("The stream ~S don't run read-byte function.", stream, NULL);
	return 0;
}

int unread_byte_stream_error(addr stream, byte c)
{
	fmte("The stream ~S don't run unread-byte function.", stream, NULL);
	return 0;
}

int write_binary_stream_error(addr stream, const void *ptr, size_t size, size_t *ret)
{
	fmte("The stream ~S don't run write-binary function.", stream, NULL);
	return 0;
}

int write_byte_stream_error(addr stream, byte c)
{
	fmte("The stream ~S don't run write-byte function.", stream, NULL);
	return 0;
}

int read_char_stream_error(addr stream, unicode *ret)
{
	fmte("The stream ~S don't run read-char function.", stream, NULL);
	return 0;
}

int read_hang_stream_error(addr stream, unicode *ret, int *hang)
{
	fmte("The stream ~S don't run read-char-no-hang function.", stream, NULL);
	return 0;
}

void unread_char_stream_error(addr stream, unicode c)
{
	fmte("The stream ~S don't run unread-char function.", stream, NULL);
}

void write_char_stream_error(addr stream, unicode c)
{
	fmte("The stream ~S don't run write-char function.", stream, NULL);
}

int fresh_line_stream_error(addr stream)
{
	fmte("The stream ~S don't run fresh-line function.", stream, NULL);
	return 0;
}

int inputp_stream_error(addr stream)
{
	fmte("The stream ~S don't run input-stream-p function.", stream, NULL);
	return 0;
}

int outputp_stream_error(addr stream)
{
	fmte("The stream ~S don't run input-stream-p function.", stream, NULL);
	return 0;
}

int interactivep_stream_error(addr stream)
{
	fmte("The stream ~S don't run input-stream-p function.", stream, NULL);
	return 0;
}

void file_length_stream_error(addr stream, addr *ret)
{
	fmte("The stream ~S don't run file-length function.", stream, NULL);
}

int file_character_length_stream_error(addr stream, unicode u, size_t *ret)
{
	fmte("The stream ~S don't run file-string-length function.", stream, NULL);
	return 1;
}

int file_string_length_stream_error(addr stream, addr pos, size_t *ret)
{
	fmte("The stream ~S don't run file-string-length function.", stream, NULL);
	return 1;
}

int listen_stream_error(addr stream)
{
	fmte("The stream ~S don't run listen function.", stream, NULL);
	return 1;
}

void clear_input_stream_error(addr stream)
{
	fmte("The stream ~S don't run clear-input function.", stream, NULL);
}

void finish_output_stream_error(addr stream)
{
	fmte("The stream ~S don't run finish-output function.", stream, NULL);
}

void force_output_stream_error(addr stream)
{
	fmte("The stream ~S don't run force-output function.", stream, NULL);
}

void clear_output_stream_error(addr stream)
{
	fmte("The stream ~S don't run clear-output function.", stream, NULL);
}

