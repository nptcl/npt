#ifndef __PATHNAME_TABLE_HEADER__
#define __PATHNAME_TABLE_HEADER__

#include "constant.h"
#include "execute.h"
#include "pathname_localp.h"
#include "typedef.h"

struct fileparse {
	unsigned junk : 1;
	unsigned force_host : 1;
	Execute ptr;
	LocalpRoot local;
	addr thing, path, queue, result;
	addr host, device, directory, name, type, version;
	size_t start, end, endpos;
	struct localp_struct local_buffer;
};

_g void init_fileparse(struct fileparse *pa, Execute ptr, int localp);
_g void pathname_fileparse_alloc(struct fileparse *pa, int logical);
_g int wild_value_pathname_(addr input, addr *ret);
_g int make_parse_logical_pathname_(struct fileparse *pa);
_g int pushrange_pathname_(LocalpRoot local,
		addr queue, const unicode *body, size_t n1, size_t n2);
_g void make_charqueue_fileparse(struct fileparse *pa, addr queue, addr *ret);
_g int nametype_pathname_(struct fileparse *pa, size_t index);
_g int pushdirectory_fileparse_(struct fileparse *pa, addr *list, addr name);
_g void pushconstant_fileparse(struct fileparse *pa, addr *list, constindex index);

_g int check_host_logical_pathname_(LocalpRoot local, addr queue, int *ret);
_g int check_drive_logical_pathname_(LocalpRoot local, int drive, int *ret);

_g int parser_logical_pathname_(struct fileparse *pa);
_g int parser_unix_pathname_(struct fileparse *pa);
_g int parser_windows_pathname_(struct fileparse *pa);

#endif

