#ifndef __COMPILE_STREAM_HEADER__
#define __COMPILE_STREAM_HEADER__

#include "compile_typedef.h"
#include "typedef.h"

#define faslread_variable_(stream, value, ret) \
	faslread_buffer_check_(stream, &(value), sizeoft(value), (ret))

_g int faslwrite_buffer_(addr stream, const void *ptr, size_t size);
_g int faslwrite_type_(addr stream, enum FaslCode code);
_g int faslwrite_byte_(addr stream, byte value);

_g int faslread_buffer_check_(addr stream, void *ptr, size_t size, int *ret);
_g int faslread_buffer_(addr stream, void *ptr, size_t size);
_g int faslread_type_(addr stream, enum FaslCode *ret);
_g int faslread_type_check_(addr stream, enum FaslCode value);
_g int faslread_byte_(addr stream, byte *ret);

#endif

