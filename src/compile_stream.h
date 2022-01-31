#ifndef __COMPILE_STREAM_HEADER__
#define __COMPILE_STREAM_HEADER__

#include "compile_typedef.h"
#include "typedef.h"

#define faslwrite_buffer_ _n(faslwrite_buffer_)
#define faslwrite_type_ _n(faslwrite_type_)
#define faslwrite_status_ _n(faslwrite_status_)
#define faslwrite_type_status_ _n(faslwrite_type_status_)
#define faslwrite_byte_ _n(faslwrite_byte_)
#define faslwrite_size_ _n(faslwrite_size_)

#define faslread_buffer_check_ _n(faslread_buffer_check_)
#define faslread_buffer_ _n(faslread_buffer_)
#define faslread_type_ _n(faslread_type_)
#define faslread_type_check_ _n(faslread_type_check_)
#define faslread_status_ _n(faslread_status_)
#define faslread_status_update _n(faslread_status_update)
#define faslread_byte_ _n(faslread_byte_)
#define faslread_size_ _n(faslread_size_)

#define faslread_variable_(stream, value, ret) \
	faslread_buffer_check_(stream, &(value), sizeoft(value), (ret))

struct faslstatus {
	byte status, user;
};
typedef struct faslstatus FaslStatus;

int faslwrite_buffer_(addr stream, const void *ptr, size_t size);
int faslwrite_type_(addr stream, enum FaslCode code);
int faslwrite_status_(addr stream, addr pos);
int faslwrite_type_status_(addr stream, addr pos, enum FaslCode code);
int faslwrite_byte_(addr stream, byte value);
int faslwrite_size_(addr stream, size_t value);

int faslread_buffer_check_(addr stream, void *ptr, size_t size, int *ret);
int faslread_buffer_(addr stream, void *ptr, size_t size);
int faslread_type_(addr stream, enum FaslCode *ret);
int faslread_type_check_(addr stream, enum FaslCode value);
int faslread_status_(addr stream, FaslStatus *ret);
void faslread_status_update(addr pos, FaslStatus status);
int faslread_byte_(addr stream, byte *ret);
int faslread_size_(addr stream, size_t *ret);

#endif

