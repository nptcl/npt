#ifndef __UNICODE_HEADER__
#define __UNICODE_HEADER__

#include "local.h"
#include "typedef.h"

_g int string8_size_alloc_(LocalRoot local, addr *ret, const char *name, size_t len);
_g int string8_size_local_(LocalRoot local, addr *ret, const char *name, size_t len);
_g int string8_size_heap_(addr *ret, const char *name, size_t len);
_g int string8_null_alloc_(LocalRoot local, addr *ret, const char *name);
_g int string8_null_local_(LocalRoot local, addr *ret, const char *name);
_g int string8_null_heap_(addr *ret, const char *name);

_g int string16_size_alloc_(LocalRoot local, addr *ret, const byte16 *name, size_t len);
_g int string16_size_local_(LocalRoot local, addr *ret, const byte16 *name, size_t len);
_g int string16_size_heap_(addr *ret, const byte16 *name, size_t len);
_g int string16_null_alloc_(LocalRoot local, addr *ret, const byte16 *name);
_g int string16_null_local_(LocalRoot local, addr *ret, const byte16 *name);
_g int string16_null_heap_(addr *ret, const byte16 *name);

_g int string32_size_alloc_(LocalRoot local, addr *ret, const unicode *name, size_t len);
_g int string32_size_local_(LocalRoot local, addr *ret, const unicode *name, size_t len);
_g int string32_size_heap_(addr *ret, const unicode *name, size_t len);
_g int string32_null_alloc_(LocalRoot local, addr *ret, const unicode *name);
_g int string32_null_local_(LocalRoot local, addr *ret, const unicode *name);
_g int string32_null_heap_(addr *ret, const unicode *name);

#endif

