#ifndef __PATHNAME_HEADER__
#define __PATHNAME_HEADER__

#include "typedef.h"
#include "local.h"

#define RefLogicalPathname(x)		((int)GetUser(x))
#define GetLogicalPathname(x,y)		(*(y) = (int)GetUser(x))
#define SetLogicalPathname(x,y)		(SetUser((x),(y)))

#define SetPathname SetArrayA2
#define GetPathname GetArrayA2

enum PATHNAME_INDEX {
	PATHNAME_INDEX_HOST = 0,
	PATHNAME_INDEX_DEVICE,
	PATHNAME_INDEX_DIRECTORY,
	PATHNAME_INDEX_NAME,
	PATHNAME_INDEX_TYPE,
	PATHNAME_INDEX_VERSION,
	PATHNAME_INDEX_SIZE
};

/* initialize */
_g void build_pathname(void);
_g void sethost_pathname(addr key, addr value);
/* found=0, notfound=1 */
_g int gethost_pathname(addr key, addr *ret);

/* pathname object */
_g void make_pathname_alloc(LocalRoot local, addr *ret, int logical);
_g void pathname_alloc(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type);
_g void pathname_local(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type);
_g void pathname_heap(addr *ret,
		addr host, addr device, addr directory, addr name, addr type);
_g void logical_pathname_alloc(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version);
_g void logical_pathname_local(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version);
_g void logical_pathname_heap(addr *ret, addr host,
		addr directory, addr name, addr type, addr version);
_g int pathnamep(addr pos);
_g int pathname_pathname_p(addr pos);
_g int pathname_logical_p(addr pos);
_g int pathname_file_p(addr pos);
_g int pathname_directory_p(addr pos);
_g void copylocal_pathname_array(LocalRoot local, addr a, int i, addr b);
_g void copy_pathname_alloc(LocalRoot local, addr *ret, addr pos);
#define copy_pathname_heap(x,y) copy_pathname_alloc(NULL, (x), (y))
_g int pathname_equal(addr left, addr right);

/* parse */
_g void parse_pathname_char_heap(Execute ptr, const char *str, addr *ret);
_g void pathname_designer_alloc(Execute ptr, addr pos, addr *ret, int localp);
_g void pathname_designer_heap(Execute ptr, addr pos, addr *ret);
_g void pathname_designer_local(Execute ptr, addr pos, addr *ret);

_g void physical_pathname_alloc(Execute ptr, addr pos, addr *ret, int localp);
_g void physical_pathname_heap(Execute ptr, addr pos, addr *ret);
_g void physical_pathname_local(Execute ptr, addr pos, addr *ret);

_g void directory_name_pathname_heap(LocalRoot local, addr pos, addr *ret);
_g void directory_name_pathname_local(LocalRoot local, addr pos, addr *ret);
_g void name_pathname_heap(Execute ptr, addr pos, addr *ret);
_g void name_pathname_local(Execute ptr, addr pos, addr *ret);
_g void merge_pathnames_clang(Execute ptr,
		addr pos, addr defpath, addr defver, addr *ret);

/* common-lisp */
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
_g void namestring_pathname(Execute ptr, addr *ret, addr pos);
_g void file_namestring_pathname(Execute ptr, addr *ret, addr pos);
_g void directory_namestring_pathname(Execute ptr, addr *ret, addr pos);
_g void host_namestring_pathname(Execute ptr, addr *ret, addr pos);
_g void enough_namestring_pathname(Execute ptr, addr *ret, addr pos, addr defaults);
_g void parse_namestring(Execute ptr, addr *ret, addr *position,
		addr thing, addr host, addr defaults, addr start, addr end, addr junk);
_g int wild_pathname_boolean(addr file, addr field);
_g void wild_pathname_p(Execute ptr, addr *ret, addr file, addr field);
_g int wildcard_pathname(addr a, addr b, int wild);
_g void pathname_match_p(Execute ptr, addr *ret, addr pos, addr wild);
_g void translate_pathname(Execute ptr, addr *ret, addr pos, addr from, addr to);
_g void translate_logical_pathname(Execute ptr, addr *ret, addr pos);
_g void merge_pathnames(Execute ptr, addr *ret, addr pos, addr defaults, addr version);
_g int load_logical_pathname_translations_common(Execute ptr, addr pos, int *ret);

/* initialize */
_g void init_pathname(void);

#endif

