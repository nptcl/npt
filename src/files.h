#ifndef __FILES_HEADER__
#define __FILES_HEADER__

#include "execute.h"
#include "typedef.h"

_g void directory_files(Execute ptr, addr *ret, addr pos);
_g void probe_file_files(Execute ptr, addr *ret, addr pos);
_g void ensure_directories_exist_files(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose);
_g void file_author_files(Execute ptr, addr *ret, addr pos);
_g void file_write_date_files(Execute ptr, addr *ret, addr pos);
_g void rename_file_files(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr file, addr to);
_g void delete_file_files(Execute ptr, addr pos);

#endif

