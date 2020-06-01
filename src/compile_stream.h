#ifndef __COMPILE_STREAM_HEADER__
#define __COMPILE_STREAM_HEADER__

#include "compile_typedef.h"
#include "typedef.h"

#define faslread_variable(stream, value) \
	faslread_buffer_check(stream, &(value), sizeoft(value))

_g void faslwrite_buffer(addr stream, const void *ptr, size_t size);
_g void faslwrite_type(addr stream, enum FaslCode code);
_g int faslread_buffer_check(addr stream, void *ptr, size_t size);
_g void faslread_buffer(addr stream, void *ptr, size_t size);
_g void faslread_type(addr stream, enum FaslCode *ret);

#endif

