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
_g void wild_value_pathname(addr input, addr *ret);
_g void make_parse_logical_pathname(struct fileparse *pa);
_g void pushrange_pathname(LocalpRoot local,
		addr queue, const unicode *body, size_t n1, size_t n2);
_g void make_charqueue_fileparse(struct fileparse *pa, addr queue, addr *ret);
_g void nametype_pathname(struct fileparse *pa, size_t index);
_g void pushdirectory_fileparse(struct fileparse *pa, addr *list, addr name);
_g void pushconstant_fileparse(struct fileparse *pa, addr *list, constindex index);

_g int check_host_logical_pathname(LocalpRoot local, addr queue);
_g int check_drive_logical_pathname(LocalpRoot local, int drive);

_g void parser_logical_pathname(struct fileparse *pa);
_g void parser_unix_pathname(struct fileparse *pa);
_g void parser_windows_pathname(struct fileparse *pa);

#endif

