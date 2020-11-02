#ifndef __CALL_FILENAMES_HEADER__
#define __CALL_FILENAMES_HEADER__

#include "execute.h"
#include "typedef.h"

#define pathname_common_ _n(pathname_common_)
#define make_pathname_common_ _n(make_pathname_common_)
#define pathnamep_common _n(pathnamep_common)
#define pathname_host_common_ _n(pathname_host_common_)
#define pathname_device_common_ _n(pathname_device_common_)
#define pathname_directory_common_ _n(pathname_directory_common_)
#define pathname_name_common_ _n(pathname_name_common_)
#define pathname_type_common_ _n(pathname_type_common_)
#define pathname_version_common_ _n(pathname_version_common_)
#define load_logical_pathname_translations_common_ \
	_n(load_logical_pathname_translations_common_)
#define logical_pathname_translations_common_ \
	_n(logical_pathname_translations_common_)
#define setf_logical_pathname_translations_common_ \
	_n(setf_logical_pathname_translations_common_)
#define logical_pathname_common_ _n(logical_pathname_common_)
#define default_pathname_defaults_common _n(default_pathname_defaults_common)
#define namestring_common_ _n(namestring_common_)
#define file_namestring_common_ _n(file_namestring_common_)
#define directory_namestring_common_ _n(directory_namestring_common_)
#define host_namestring_common_ _n(host_namestring_common_)
#define enough_namestring_common_ _n(enough_namestring_common_)
#define parse_namestring_common_ _n(parse_namestring_common_)
#define wild_pathname_p_common_ _n(wild_pathname_p_common_)
#define pathname_match_p_common_ _n(pathname_match_p_common_)
#define translate_pathname_common_ _n(translate_pathname_common_)
#define translate_logical_pathname_common_ _n(translate_logical_pathname_common_)
#define merge_pathnames_common_ _n(merge_pathnames_common_)

_g int pathname_common_(Execute ptr, addr var, addr *ret);
_g int make_pathname_common_(Execute ptr, addr rest, addr *ret);
_g void pathnamep_common(addr var, addr *ret);
_g int pathname_host_common_(Execute ptr, addr pos, addr rest, addr *ret);
_g int pathname_device_common_(Execute ptr, addr pos, addr rest, addr *ret);
_g int pathname_directory_common_(Execute ptr, addr pos, addr rest, addr *ret);
_g int pathname_name_common_(Execute ptr, addr pos, addr rest, addr *ret);
_g int pathname_type_common_(Execute ptr, addr pos, addr rest, addr *ret);
_g int pathname_version_common_(Execute ptr, addr pos, addr *ret);
_g int load_logical_pathname_translations_common_(Execute ptr, addr host, addr *ret);
_g int logical_pathname_translations_common_(addr host, addr *ret);
_g int setf_logical_pathname_translations_common_(Execute ptr, addr host, addr list);
_g int logical_pathname_common_(Execute ptr, addr *ret, addr pos);
_g void default_pathname_defaults_common(void);
_g int namestring_common_(Execute ptr, addr *ret, addr pos);
_g int file_namestring_common_(Execute ptr, addr *ret, addr pos);
_g int directory_namestring_common_(Execute ptr, addr *ret, addr pos);
_g int host_namestring_common_(Execute ptr, addr *ret, addr pos);
_g int enough_namestring_common_(Execute ptr, addr *ret, addr pos, addr defaults);
_g int parse_namestring_common_(Execute ptr,
		addr thing, addr rest, addr *ret1, addr *ret2);
_g int wild_pathname_p_common_(Execute ptr, addr *ret, addr file, addr field);
_g int pathname_match_p_common_(Execute ptr, addr *ret, addr a, addr b);
_g int translate_pathname_common_(Execute ptr, addr *ret, addr pos, addr from, addr to);
_g int translate_logical_pathname_common_(Execute ptr, addr *ret, addr pos);
_g int merge_pathnames_common_(Execute ptr,
		addr *ret, addr pos, addr defaults, addr version);

#endif

