#ifndef __BYTESPEC_HEADER__
#define __BYTESPEC_HEADER__

#include "typedef.h"
#include "local.h"

#define bytespec_alloc _n(bytespec_alloc)
#define bytespec_local _n(bytespec_local)
#define bytespec_heap _n(bytespec_heap)
#define byte_common_ _n(byte_common_)
#define byte_size_common _n(byte_size_common)
#define byte_position_common _n(byte_position_common)
#define bytespec_mask_init _n(bytespec_mask_init)
#define bytespec_mask_get _n(bytespec_mask_get)
#define bytespec_mask_getsize _n(bytespec_mask_getsize)

struct bytespec_struct {
	size_t size, position;
};

#define PtrByteSpec			PtrBodyB2
#define ByteSpecStruct(x)	((struct bytespec_struct *)PtrByteSpec(x))

void bytespec_alloc(LocalRoot local, addr *ret, size_t size, size_t posi);
void bytespec_local(LocalRoot local, addr *ret, size_t size, size_t posi);
void bytespec_heap(addr *ret, size_t size, size_t posi);

int byte_common_(addr size, addr posi, addr *ret);
void byte_size_common(addr pos, addr *ret);
void byte_position_common(addr pos, addr *ret);

/* mask */
struct bytespec_mask {
	size_t size, position, posend, index;
	size_t start, end;
};
void bytespec_mask_init(struct bytespec_mask *ptr, addr pos);
bigtype bytespec_mask_get(struct bytespec_mask *ptr);
size_t bytespec_mask_getsize(struct bytespec_mask *ptr);

#endif

