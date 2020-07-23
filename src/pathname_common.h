#ifndef __PATHNAME_COMMON_HEADER__
#define __PATHNAME_COMMON_HEADER__

#include "execute.h"
#include "typedef.h"

_g int make_pathname_(Execute ptr, addr *ret, addr rest);
_g int get_logical_pathname_translations_(addr host, addr *ret);
_g int set_logical_pathname_translations_(Execute ptr, addr host, addr list);
_g int pathname_host_(addr pos, addr *ret, int localp);
_g int pathname_device_(addr pos, addr *ret, int localp);
_g int pathname_directory_(addr pos, addr *ret, int localp);
_g int pathname_name_(addr pos, addr *ret, int localp);
_g int pathname_type_(addr pos, addr *ret, int localp);
_g void pathname_version(addr pos, addr *ret);
_g int logical_pathname_(Execute ptr, addr *ret, addr pos);
_g int namestring_pathname_(Execute ptr, addr *ret, addr pos);
_g int file_namestring_pathname_(Execute ptr, addr *ret, addr pos);
_g int directory_namestring_pathname_(Execute ptr, addr *ret, addr pos);
_g int host_namestring_pathname_(Execute ptr, addr *ret, addr pos);
_g int enough_namestring_pathname_(Execute ptr, addr *ret, addr pos, addr defaults);
_g int parse_namestring_(Execute ptr, addr *ret, addr *position,
		addr thing, addr host, addr defaults, addr start, addr end, addr junk);
_g int wild_pathname_p_(Execute ptr, addr *ret, addr file, addr field);
_g int wildcard_pathname_(addr a, addr b, int wild, int *ret);
_g int pathname_match_p_(Execute ptr, addr *ret, addr pos, addr wild);
_g int translate_pathname_(Execute ptr, addr *ret, addr pos, addr from, addr to);
_g int translate_logical_pathname_(Execute ptr, addr *ret, addr pos);
_g int merge_pathnames_(Execute ptr, addr *ret, addr pos, addr defaults, addr version);
_g int load_logical_pathname_translations_common(Execute ptr, addr pos, int *ret);

_g void init_pathname_common(void);

#endif

