#ifndef __PATHNAME_HEADER__
#define __PATHNAME_HEADER__

#include "typedef.h"
#include "local.h"

#define defaults_pathname_heap_ _n(defaults_pathname_heap_)
#define parse_pathname_full_heap_ _n(parse_pathname_full_heap_)
#define parse_pathname_host_heap_ _n(parse_pathname_host_heap_)
#define parse_pathname_char_heap_ _n(parse_pathname_char_heap_)
#define pathname_designer_alloc_ _n(pathname_designer_alloc_)
#define pathname_designer_heap_ _n(pathname_designer_heap_)
#define pathname_designer_local_ _n(pathname_designer_local_)
#define physical_pathname_alloc_ _n(physical_pathname_alloc_)
#define physical_pathname_heap_ _n(physical_pathname_heap_)
#define physical_pathname_local_ _n(physical_pathname_local_)
#define file_name_pathname_heap_ _n(file_name_pathname_heap_)
#define file_name_pathname_local_ _n(file_name_pathname_local_)
#define directory_name_pathname_heap_ _n(directory_name_pathname_heap_)
#define directory_name_pathname_local_ _n(directory_name_pathname_local_)
#define name_pathname_heap_ _n(name_pathname_heap_)
#define name_pathname_local_ _n(name_pathname_local_)
#define merge_pathnames_clang_ _n(merge_pathnames_clang_)
#define build_pathname _n(build_pathname)

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
_g int defaults_pathname_heap_(Execute ptr, addr *ret, addr defaults);
_g int parse_pathname_full_heap_(Execute ptr, addr thing, addr host,
		addr defaults, size_t start, size_t end, int junk, addr *ret, size_t *pos);
_g int parse_pathname_host_heap_(Execute ptr, addr thing, addr host, addr *ret);

_g int parse_pathname_char_heap_(Execute ptr, const char *str, addr *ret);
_g int pathname_designer_alloc_(Execute ptr, addr pos, addr *ret, int localp);
_g int pathname_designer_heap_(Execute ptr, addr pos, addr *ret);
_g int pathname_designer_local_(Execute ptr, addr pos, addr *ret);

_g int physical_pathname_alloc_(Execute ptr, addr pos, addr *ret, int localp);
_g int physical_pathname_heap_(Execute ptr, addr pos, addr *ret);
_g int physical_pathname_local_(Execute ptr, addr pos, addr *ret);

_g int file_name_pathname_heap_(LocalRoot local, addr pos, addr *ret);
_g int file_name_pathname_local_(LocalRoot local, addr pos, addr *ret);

_g int directory_name_pathname_heap_(LocalRoot local, addr pos, addr *ret);
_g int directory_name_pathname_local_(LocalRoot local, addr pos, addr *ret);
_g int name_pathname_heap_(Execute ptr, addr pos, addr *ret);
_g int name_pathname_local_(Execute ptr, addr pos, addr *ret);
_g int merge_pathnames_clang_(Execute ptr,
		addr pos, addr defpath, addr defver, addr *ret);

/* initialize */
_g void build_pathname(void);

#endif

