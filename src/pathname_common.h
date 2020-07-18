#ifndef __PATHNAME_COMMON_HEADER__
#define __PATHNAME_COMMON_HEADER__

#include "execute.h"
#include "typedef.h"

_g void make_pathname(Execute ptr, addr *ret, addr rest);
_g void get_logical_pathname_translations(addr host, addr *ret);
_g int set_logical_pathname_translations(Execute ptr, addr host, addr list);
_g void pathname_host(addr pos, addr *ret, int localp);
_g void pathname_device(addr pos, addr *ret, int localp);
_g void pathname_directory(addr pos, addr *ret, int localp);
_g void pathname_name(addr pos, addr *ret, int localp);
_g void pathname_type(addr pos, addr *ret, int localp);
_g void pathname_version(addr pos, addr *ret);
_g void logical_pathname(Execute ptr, addr *ret, addr pos);
_g int namestring_pathname_(Execute ptr, addr *ret, addr pos);
_g int file_namestring_pathname_(Execute ptr, addr *ret, addr pos);
_g void directory_namestring_pathname(Execute ptr, addr *ret, addr pos);
_g void host_namestring_pathname(Execute ptr, addr *ret, addr pos);
_g int enough_namestring_pathname_(Execute ptr, addr *ret, addr pos, addr defaults);
_g void parse_namestring(Execute ptr, addr *ret, addr *position,
		addr thing, addr host, addr defaults, addr start, addr end, addr junk);
_g void wild_pathname_p(Execute ptr, addr *ret, addr file, addr field);
_g int wildcard_pathname(addr a, addr b, int wild);
_g void pathname_match_p(Execute ptr, addr *ret, addr pos, addr wild);
_g void translate_pathname(Execute ptr, addr *ret, addr pos, addr from, addr to);
_g void translate_logical_pathname(Execute ptr, addr *ret, addr pos);
_g void merge_pathnames(Execute ptr, addr *ret, addr pos, addr defaults, addr version);
_g int load_logical_pathname_translations_common(Execute ptr, addr pos, int *ret);

_g void init_pathname_common(void);

#endif

