#include "compile_stream.h"
#include "compile_typedef.h"
#include "condition.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "typedef.h"

/*
 *  write
 */
int faslwrite_buffer_(addr stream, const void *ptr, size_t size)
{
	size_t check;

	Return(write_binary_stream_(stream, ptr, size, &check));
	if (size != check)
		return fmte_("write-binary-stream size error.", NULL);

	return 0;
}

int faslwrite_type_(addr stream, enum FaslCode code)
{
	return write_unsigned8_stream_(stream, (byte)code);
}

int faslwrite_byte_(addr stream, byte value)
{
	return write_unsigned8_stream_(stream, value);
}


/*
 *  read
 */
int faslread_buffer_check_(addr stream, void *ptr, size_t size, int *ret)
{
	size_t check;
	Return(read_binary_stream_(stream, ptr, size, &check));
	return Result(ret, (size != check));
}

int faslread_buffer_(addr stream, void *ptr, size_t size)
{
	int check;

	Return(faslread_buffer_check_(stream, ptr, size, &check));
	if (check)
		return fmte_("readforce-binary-stream error.", NULL);

	return 0;
}

int faslread_type_(addr stream, enum FaslCode *ret)
{
	int check;
	byte c;

	Return(read_unsigned8_stream_(stream, &c, &check));
	if (check) {
		*ret = FaslCode_error;
		return fmte_("read-byte-stream error.", NULL);
	}

	return Result(ret, (enum FaslCode)c);
}

int faslread_type_check_(addr stream, enum FaslCode value)
{
	enum FaslCode check;

	Return(faslread_type_(stream, &check));
	if (check != value)
		return fmte_("Invalid fasl format.", NULL);

	return 0;
}

int faslread_byte_(addr stream, byte *ret)
{
	int check;

	Return(read_unsigned8_stream_(stream, ret, &check));
	if (check) {
		*ret = 0;
		return fmte_("read-byte-stream error.", NULL);
	}

	return 0;
}

