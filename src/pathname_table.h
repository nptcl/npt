#ifndef __PATHNAME_TABLE_HEADER__
#define __PATHNAME_TABLE_HEADER__

#include "constant.h"
#include "execute.h"
#include "pathname_localp.h"
#include "typedef.h"

#define init_fileparse _n(init_fileparse)
#define pathname_fileparse_alloc _n(pathname_fileparse_alloc)
#define wild_value_pathname_ _n(wild_value_pathname_)
#define make_parse_logical_pathname_ _n(make_parse_logical_pathname_)
#define pushrange_pathname_ _n(pushrange_pathname_)
#define make_charqueue_fileparse _n(make_charqueue_fileparse)
#define nametype_pathname_ _n(nametype_pathname_)
#define pushdirectory_fileparse_ _n(pushdirectory_fileparse_)
#define pushconstant_fileparse _n(pushconstant_fileparse)
#define check_host_logical_pathname_ _n(check_host_logical_pathname_)
#define check_drive_logical_pathname_ _n(check_drive_logical_pathname_)
#define parser_logical_pathname_ _n(parser_logical_pathname_)
#define parser_unix_pathname_ _n(parser_unix_pathname_)
#define parser_windows_pathname_ _n(parser_windows_pathname_)

struct fileparse {
	unsigned junk : 1;
	unsigned force_host : 1;
	unsigned errorp : 1;
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
		addr queue, addr thing, size_t n1, size_t n2);
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

