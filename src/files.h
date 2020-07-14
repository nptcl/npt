#ifndef __FILES_HEADER__
#define __FILES_HEADER__

#include "execute.h"
#include "typedef.h"

_g int directory_files_(Execute ptr, addr *ret, addr pos);
_g int probe_file_files_(Execute ptr, addr *ret, addr pos);
_g int ensure_directories_exist_files_(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose);
_g int file_author_files_(Execute ptr, addr *ret, addr pos);
_g int file_write_date_files_(Execute ptr, addr *ret, addr pos);
_g int rename_file_files_(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr file, addr to);
_g int delete_file_files_(Execute ptr, addr pos);
_g int remove_file_common_(Execute ptr, addr pos, int errorp, int *ret);
_g int remove_directory_common_(Execute ptr, addr pos, int errorp, int *ret);
_g int truename_files_(Execute ptr, addr file, addr *ret, int errorp);

#endif

