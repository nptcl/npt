#ifndef __FILES_HEADER__
#define __FILES_HEADER__

#include "execute.h"
#include "typedef.h"

#define directory_files_ _n(directory_files_)
#define probe_file_files_ _n(probe_file_files_)
#define ensure_directories_exist_files_ _n(ensure_directories_exist_files_)
#define file_author_files_ _n(file_author_files_)
#define file_write_date_files_ _n(file_write_date_files_)
#define rename_file_files_ _n(rename_file_files_)
#define delete_file_files_ _n(delete_file_files_)
#define remove_file_common_ _n(remove_file_common_)
#define remove_directory_common_ _n(remove_directory_common_)
#define truename_files_ _n(truename_files_)

int directory_files_(Execute ptr, addr *ret, addr pos);
int probe_file_files_(Execute ptr, addr *ret, addr pos);
int ensure_directories_exist_files_(Execute ptr,
		addr *ret1, addr *ret2, addr pos, int verbose);
int file_author_files_(Execute ptr, addr *ret, addr pos);
int file_write_date_files_(Execute ptr, addr *ret, addr pos);
int rename_file_files_(Execute ptr,
		addr *ret1, addr *ret2, addr *ret3, addr file, addr to);
int delete_file_files_(Execute ptr, addr pos);
int remove_file_common_(Execute ptr, addr pos, int errorp, int *ret);
int remove_directory_common_(Execute ptr, addr pos, int errorp, int *ret);
int truename_files_(Execute ptr, addr file, addr *ret, int errorp);

#endif

