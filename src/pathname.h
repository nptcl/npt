#ifndef __PATHNAME_HEADER__
#define __PATHNAME_HEADER__

#include "typedef.h"
#include "local.h"

/*  pathname
 *    host -> unix, windows, "logical"
 *    [unix]
 *      device = nil
 *      version = :unspecific
 *    [windows]
 *      device = (universal | device | "[A-Z]" | Nil)
 *      version = :unspecific
 *    [logical]
 *      device = :unspecific
 *      version = (or null integer)
 */
_g void defaults_pathname_heap(Execute ptr, addr *ret, addr defaults);
_g void parse_pathname_full_heap(Execute ptr, addr thing, addr host,
		addr defaults, size_t start, size_t end, int junk, addr *ret, size_t *pos);
_g void parse_pathname_host_heap(Execute ptr, addr thing, addr host, addr *ret);

_g void parse_pathname_char_heap(Execute ptr, const char *str, addr *ret);
_g void pathname_designer_alloc(Execute ptr, addr pos, addr *ret, int localp);
_g void pathname_designer_heap(Execute ptr, addr pos, addr *ret);
_g void pathname_designer_local(Execute ptr, addr pos, addr *ret);

_g void physical_pathname_alloc(Execute ptr, addr pos, addr *ret, int localp);
_g void physical_pathname_heap(Execute ptr, addr pos, addr *ret);
_g void physical_pathname_local(Execute ptr, addr pos, addr *ret);

_g int file_name_pathname_heap_(LocalRoot local, addr pos, addr *ret);
_g int file_name_pathname_local_(LocalRoot local, addr pos, addr *ret);

_g void directory_name_pathname_heap(LocalRoot local, addr pos, addr *ret);
_g void directory_name_pathname_local(LocalRoot local, addr pos, addr *ret);
_g int name_pathname_heap_(Execute ptr, addr pos, addr *ret);
_g int name_pathname_local_(Execute ptr, addr pos, addr *ret);
_g void merge_pathnames_clang(Execute ptr,
		addr pos, addr defpath, addr defver, addr *ret);

/* initialize */
_g void init_pathname(void);
_g void build_pathname(void);

#endif

