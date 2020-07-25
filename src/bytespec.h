#ifndef __BYTESPEC_HEADER__
#define __BYTESPEC_HEADER__

#include "typedef.h"
#include "local.h"

struct bytespec_struct {
	size_t size, position;
};

#define PtrByteSpec			PtrBodyB2
#define ByteSpecStruct(x)	((struct bytespec_struct *)PtrByteSpec(x))

_g void bytespec_alloc(LocalRoot local, addr *ret, size_t size, size_t posi);
_g void bytespec_local(LocalRoot local, addr *ret, size_t size, size_t posi);
_g void bytespec_heap(addr *ret, size_t size, size_t posi);

_g int byte_common_(addr size, addr posi, addr *ret);
_g void byte_size_common(addr pos, addr *ret);
_g void byte_position_common(addr pos, addr *ret);

/* mask */
struct bytespec_mask {
	size_t size, position, posend, index;
	size_t start, end;
};
_g void bytespec_mask_init(struct bytespec_mask *ptr, addr pos);
_g bigtype bytespec_mask_get(struct bytespec_mask *ptr);
_g size_t bytespec_mask_getsize(struct bytespec_mask *ptr);

#endif

