#include "compile_typedef.h"
#include "condition.h"
#include "stream.h"
#include "typedef.h"


/*
 *  write
 */
_g void faslwrite_buffer(addr stream, const void *ptr, size_t size)
{
	size_t check;

	if (write_binary_stream(stream, ptr, size, &check)) {
		fmte("write-binary-stream error.", NULL);
		return;
	}
	if (size != check) {
		fmte("write-binary-stream size error.", NULL);
		return;
	}
}

_g void faslwrite_type(addr stream, enum FaslCode code)
{
	if (write_byte_stream(stream, (byte)code))
		fmte("write-byte-stream error.", NULL);
}

_g void faslwrite_byte(addr stream, byte value)
{
	if (write_byte_stream(stream, value))
		fmte("write-byte-stream error..", NULL);
}


/*
 *  read
 */
_g int faslread_buffer_check(addr stream, void *ptr, size_t size)
{
	int check;
	size_t result;

	check = readforce_binary_stream(stream, ptr, size, &result);
	if (check)
		return 1;
	if (size != result)
		return 1;

	return 0;
}

_g void faslread_buffer(addr stream, void *ptr, size_t size)
{
	if (faslread_buffer_check(stream, ptr, size))
		fmte("readforce-binary-stream error.", NULL);
}

_g void faslread_type(addr stream, enum FaslCode *ret)
{
	byte c;

	if (read_byte_stream(stream, &c))
		fmte("read-byte-stream error.", NULL);
	*ret = (enum FaslCode)c;
}

_g void faslread_type_check(addr stream, enum FaslCode value)
{
	enum FaslCode check;
	faslread_type(stream, &check);
	if (check != value)
		fmte("Invalid fasl format.", NULL);
}

_g void faslread_byte(addr stream, byte *ret)
{
	if (read_byte_stream(stream, ret))
		fmte("read-byte-stream error.", NULL);
}

