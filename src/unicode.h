#ifndef __UNICODE_HEADER__
#define __UNICODE_HEADER__

#include "local.h"
#include "typedef.h"

_g addr string8_size_heapr(const char *name, size_t len);
_g addr string8_size_localr(LocalRoot local, const char *name, size_t len);
_g addr string8_size_allocr(LocalRoot local, const char *name, size_t len);
_g void string8_size_heap(addr *ret, const char *name, size_t len);
_g void string8_size_local(LocalRoot local, addr *ret, const char *name, size_t len);
_g void string8_size_alloc(LocalRoot local, addr *ret, const char *name, size_t len);

_g addr string8_null_heapr(const char *name);
_g addr string8_null_localr(LocalRoot local, const char *name);
_g addr string8_null_allocr(LocalRoot local, const char *name);
_g void string8_null_heap(addr *ret, const char *name);
_g void string8_null_local(LocalRoot local, addr *ret, const char *name);
_g void string8_null_alloc(LocalRoot local, addr *ret, const char *name);

_g addr string16_size_heapr(const byte16 *name, size_t len);
_g addr string16_size_localr(LocalRoot local, const byte16 *name, size_t len);
_g addr string16_size_allocr(LocalRoot local, const byte16 *name, size_t len);
_g void string16_size_heap(addr *ret, const byte16 *name, size_t len);
_g void string16_size_local(LocalRoot local, addr *ret, const byte16 *name, size_t len);
_g void string16_size_alloc(LocalRoot local, addr *ret, const byte16 *name, size_t len);

_g addr string16_null_heapr(const byte16 *name);
_g addr string16_null_localr(LocalRoot local, const byte16 *name);
_g addr string16_null_allocr(LocalRoot local, const byte16 *name);
_g void string16_null_heap(addr *ret, const byte16 *name);
_g void string16_null_local(LocalRoot local, addr *ret, const byte16 *name);
_g void string16_null_alloc(LocalRoot local, addr *ret, const byte16 *name);

#endif

