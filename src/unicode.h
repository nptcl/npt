#ifndef __UNICODE_HEADER__
#define __UNICODE_HEADER__

#include "local.h"
#include "typedef.h"

#define string8_size_alloc_ _n(string8_size_alloc_)
#define string8_size_local_ _n(string8_size_local_)
#define string8_size_heap_ _n(string8_size_heap_)
#define string8_null_alloc_ _n(string8_null_alloc_)
#define string8_null_local_ _n(string8_null_local_)
#define string8_null_heap_ _n(string8_null_heap_)
#define string8_null_char1_heap_ _n(string8_null_char1_heap_)
#define string16_size_alloc_ _n(string16_size_alloc_)
#define string16_size_local_ _n(string16_size_local_)
#define string16_size_heap_ _n(string16_size_heap_)
#define string16_null_alloc_ _n(string16_null_alloc_)
#define string16_null_local_ _n(string16_null_local_)
#define string16_null_heap_ _n(string16_null_heap_)
#define string32_size_alloc_ _n(string32_size_alloc_)
#define string32_size_local_ _n(string32_size_local_)
#define string32_size_heap_ _n(string32_size_heap_)
#define string32_null_alloc_ _n(string32_null_alloc_)
#define string32_null_local_ _n(string32_null_local_)
#define string32_null_heap_ _n(string32_null_heap_)

int string8_size_alloc_(LocalRoot local, addr *ret, const char *name, size_t len);
int string8_size_local_(LocalRoot local, addr *ret, const char *name, size_t len);
int string8_size_heap_(addr *ret, const char *name, size_t len);
int string8_null_alloc_(LocalRoot local, addr *ret, const char *name);
int string8_null_local_(LocalRoot local, addr *ret, const char *name);
int string8_null_heap_(addr *ret, const char *name);
int string8_null_char1_heap_(addr *ret, const char *name, unicode c);

int string16_size_alloc_(LocalRoot local, addr *ret, const byte16 *name, size_t len);
int string16_size_local_(LocalRoot local, addr *ret, const byte16 *name, size_t len);
int string16_size_heap_(addr *ret, const byte16 *name, size_t len);
int string16_null_alloc_(LocalRoot local, addr *ret, const byte16 *name);
int string16_null_local_(LocalRoot local, addr *ret, const byte16 *name);
int string16_null_heap_(addr *ret, const byte16 *name);

int string32_size_alloc_(LocalRoot local, addr *ret, const unicode *name, size_t len);
int string32_size_local_(LocalRoot local, addr *ret, const unicode *name, size_t len);
int string32_size_heap_(addr *ret, const unicode *name, size_t len);
int string32_null_alloc_(LocalRoot local, addr *ret, const unicode *name);
int string32_null_local_(LocalRoot local, addr *ret, const unicode *name);
int string32_null_heap_(addr *ret, const unicode *name);

#endif

