#ifndef __COMPILE_STREAM_HEADER__
#define __COMPILE_STREAM_HEADER__

#include "compile_typedef.h"
#include "typedef.h"

#define faslwrite_buffer_ _n(faslwrite_buffer_)
#define faslwrite_type_ _n(faslwrite_type_)
#define faslwrite_byte_ _n(faslwrite_byte_)
#define faslread_buffer_check_ _n(faslread_buffer_check_)
#define faslread_buffer_ _n(faslread_buffer_)
#define faslread_type_ _n(faslread_type_)
#define faslread_type_check_ _n(faslread_type_check_)
#define faslread_byte_ _n(faslread_byte_)

#define faslread_variable_(stream, value, ret) \
	faslread_buffer_check_(stream, &(value), sizeoft(value), (ret))

int faslwrite_buffer_(addr stream, const void *ptr, size_t size);
int faslwrite_type_(addr stream, enum FaslCode code);
int faslwrite_byte_(addr stream, byte value);

int faslread_buffer_check_(addr stream, void *ptr, size_t size, int *ret);
int faslread_buffer_(addr stream, void *ptr, size_t size);
int faslread_type_(addr stream, enum FaslCode *ret);
int faslread_type_check_(addr stream, enum FaslCode value);
int faslread_byte_(addr stream, byte *ret);

#endif

