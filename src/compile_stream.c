#include "compile_stream.h"
#include "compile_typedef.h"
#include "condition.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "typedef.h"

/*
 *  buffer
 */
int faslwrite_buffer_(addr stream, const void *ptr, size_t size)
{
	size_t check;

	Return(write_binary_stream_(stream, ptr, size, &check));
	if (size != check)
		return fmte_("write-binary-stream size error.", NULL);

	return 0;
}

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


/*
 *  type
 */
int faslwrite_type_(addr stream, enum FaslCode code)
{
	return write_unsigned8_stream_(stream, (byte)code);
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


/*
 *  status
 */
int faslwrite_status_(addr stream, addr pos)
{
	byte status, user;

	status = GetStatus(pos);
	user = GetUser(pos);
	status &= (1 << LISPSTATUS_READONLY);
	Return(faslwrite_byte_(stream, status));
	Return(faslwrite_byte_(stream, user));

	return 0;
}

int faslwrite_type_status_(addr stream, addr pos, enum FaslCode code)
{
	Return(faslwrite_type_(stream, code));
	Return(faslwrite_status_(stream, pos));

	return 0;
}

int faslread_status_(addr stream, FaslStatus *ret)
{
	byte status, user;

	Return(faslread_byte_(stream, &status));
	Return(faslread_byte_(stream, &user));
	if (ret) {
		ret->status = status;
		ret->user = user;
	}

	return 0;
}

void faslread_status_update(addr pos, FaslStatus v)
{
	if (v.status & (1 << LISPSTATUS_READONLY)) {
		SetStatusReadOnly(pos);
	}
	SetUser(pos, v.user);
}


/*
 *  byte
 */
int faslwrite_byte_(addr stream, byte value)
{
	return write_unsigned8_stream_(stream, value);
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


/*
 *  size
 */
int faslwrite_size_(addr stream, size_t value)
{
	uint8_t v1;
	uint16_t v2;
	uint32_t v4;
#ifdef LISP_ARCH_64BIT
	uint64_t v8;
#endif

	if (value <= 0xFFUL) {
		v1 = (uint8_t)(value & 0xFFUL);
		Return(faslwrite_byte_(stream, sizeoft(v1)));
		return faslwrite_buffer_(stream, &v1, sizeoft(v1));
	}
	if (value <= 0xFFFFUL) {
		v2 = (uint16_t)(value & 0xFFFFUL);
		Return(faslwrite_byte_(stream, sizeoft(v2)));
		return faslwrite_buffer_(stream, &v2, sizeoft(v2));
	}
#ifdef LISP_ARCH_64BIT
	if (value <= 0xFFFFFFFFUL) {
		v4 = (uint32_t)(value & 0xFFFFFFFFUL);
		Return(faslwrite_byte_(stream, sizeoft(v4)));
		return faslwrite_buffer_(stream, &v4, sizeoft(v4));
	}
	v8 = (uint64_t)value;
	Return(faslwrite_byte_(stream, sizeoft(v8)));
	return faslwrite_buffer_(stream, &v8, sizeoft(v8));
#else
	v4 = (uint32_t)value;
	Return(faslwrite_byte_(stream, sizeoft(v4)));
	return faslwrite_buffer_(stream, &v4, sizeoft(v4));
#endif
}

int faslread_size_(addr stream, size_t *ret)
{
	byte size;
	uint8_t v1;
	uint16_t v2;
	uint32_t v4;
#ifdef LISP_ARCH_64BIT
	uint64_t v8;
#endif
	addr pos;

	Return(faslread_byte_(stream, &size));
	switch (size) {
		case 1:
			Return(faslread_buffer_(stream, &v1, size));
			return Result(ret, (size_t)v1);

		case 2:
			Return(faslread_buffer_(stream, &v2, size));
			return Result(ret, (size_t)v2);

		case 4:
			Return(faslread_buffer_(stream, &v4, size));
			return Result(ret, (size_t)v4);

#ifdef LISP_ARCH_64BIT
		case 8:
			Return(faslread_buffer_(stream, &v8, size));
			return Result(ret, (size_t)v8);
#endif

		default:
			*ret = 0;
			fixnum_heap(&pos, (fixnum)size);
			return fmte_("Invalid size_t length, ~A.", pos, NULL);
	}
}

