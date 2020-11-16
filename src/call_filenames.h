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

int pathname_common_(Execute ptr, addr var, addr *ret);
int make_pathname_common_(Execute ptr, addr rest, addr *ret);
void pathnamep_common(addr var, addr *ret);
int pathname_host_common_(Execute ptr, addr pos, addr rest, addr *ret);
int pathname_device_common_(Execute ptr, addr pos, addr rest, addr *ret);
int pathname_directory_common_(Execute ptr, addr pos, addr rest, addr *ret);
int pathname_name_common_(Execute ptr, addr pos, addr rest, addr *ret);
int pathname_type_common_(Execute ptr, addr pos, addr rest, addr *ret);
int pathname_version_common_(Execute ptr, addr pos, addr *ret);
int load_logical_pathname_translations_common_(Execute ptr, addr host, addr *ret);
int logical_pathname_translations_common_(addr host, addr *ret);
int setf_logical_pathname_translations_common_(Execute ptr, addr host, addr list);
int logical_pathname_common_(Execute ptr, addr *ret, addr pos);
void default_pathname_defaults_common(void);
int namestring_common_(Execute ptr, addr *ret, addr pos);
int file_namestring_common_(Execute ptr, addr *ret, addr pos);
int directory_namestring_common_(Execute ptr, addr *ret, addr pos);
int host_namestring_common_(Execute ptr, addr *ret, addr pos);
int enough_namestring_common_(Execute ptr, addr *ret, addr pos, addr defaults);
int parse_namestring_common_(Execute ptr,
		addr thing, addr rest, addr *ret1, addr *ret2);
int wild_pathname_p_common_(Execute ptr, addr *ret, addr file, addr field);
int pathname_match_p_common_(Execute ptr, addr *ret, addr a, addr b);
int translate_pathname_common_(Execute ptr, addr *ret, addr pos, addr from, addr to);
int translate_logical_pathname_common_(Execute ptr, addr *ret, addr pos);
int merge_pathnames_common_(Execute ptr,
		addr *ret, addr pos, addr defaults, addr version);

#endif

