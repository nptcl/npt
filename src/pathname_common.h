#ifndef __PATHNAME_COMMON_HEADER__
#define __PATHNAME_COMMON_HEADER__

#include "execute.h"
#include "typedef.h"

#define make_pathname_ _n(make_pathname_)
#define get_logical_pathname_translations_ _n(get_logical_pathname_translations_)
#define set_logical_pathname_translations_ _n(set_logical_pathname_translations_)
#define pathname_host_ _n(pathname_host_)
#define pathname_device_ _n(pathname_device_)
#define pathname_directory_ _n(pathname_directory_)
#define pathname_name_ _n(pathname_name_)
#define pathname_type_ _n(pathname_type_)
#define pathname_version _n(pathname_version)
#define logical_pathname_ _n(logical_pathname_)
#define namestring_pathname_ _n(namestring_pathname_)
#define file_namestring_pathname_ _n(file_namestring_pathname_)
#define directory_namestring_pathname_ _n(directory_namestring_pathname_)
#define host_namestring_pathname_ _n(host_namestring_pathname_)
#define enough_namestring_pathname_ _n(enough_namestring_pathname_)
#define parse_namestring_ _n(parse_namestring_)
#define wild_pathname_p_ _n(wild_pathname_p_)
#define pathname_match_p_ _n(pathname_match_p_)
#define translate_pathname_ _n(translate_pathname_)
#define translate_logical_pathname_ _n(translate_logical_pathname_)
#define merge_pathnames_ _n(merge_pathnames_)
#define load_logical_pathname_translations_common _n(load_logical_pathname_translations_common)
#define init_pathname_common _n(init_pathname_common)

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
_g int pathname_match_p_(Execute ptr, addr *ret, addr pos, addr wild);
_g int translate_pathname_(Execute ptr, addr *ret, addr pos, addr from, addr to);
_g int translate_logical_pathname_(Execute ptr, addr *ret, addr pos);
_g int merge_pathnames_(Execute ptr, addr *ret, addr pos, addr defaults, addr version);
_g int load_logical_pathname_translations_common(Execute ptr, addr pos, int *ret);

_g void init_pathname_common(void);

#endif

