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
void build_pathname(void);
void sethost_pathname(addr key, addr value);
/* found=0, notfound=1 */
int gethost_pathname(addr key, addr *ret);

/* pathname object */
void make_pathname_alloc(LocalRoot local, addr *ret, int logical);
void pathname_alloc(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type);
void pathname_local(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type);
void pathname_heap(addr *ret,
		addr host, addr device, addr directory, addr name, addr type);
void logical_pathname_alloc(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version);
void logical_pathname_local(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version);
void logical_pathname_heap(addr *ret, addr host,
		addr directory, addr name, addr type, addr version);
int pathnamep(addr pos);
int pathname_pathname_p(addr pos);
int pathname_logical_p(addr pos);
void copylocal_pathname_array(LocalRoot local, addr a, int i, addr b);
void copy_pathname_alloc(LocalRoot local, addr *ret, addr pos);
int pathname_equal(addr left, addr right);

/* parse */
void parse_pathname_char_heap(Execute ptr, const char *str, addr *ret);
void pathname_designer_alloc(Execute ptr, addr pos, addr *ret, int localp);
void pathname_designer_heap(Execute ptr, addr pos, addr *ret);
void pathname_designer_local(Execute ptr, addr pos, addr *ret);

void physical_pathname_alloc(Execute ptr, addr pos, addr *ret, int localp);
void physical_pathname_heap(Execute ptr, addr pos, addr *ret);
void physical_pathname_local(Execute ptr, addr pos, addr *ret);

void directory_name_pathname_heap(LocalRoot local, addr pos, addr *ret);
void directory_name_pathname_local(LocalRoot local, addr pos, addr *ret);
void name_pathname_heap(Execute ptr, addr pos, addr *ret);
void name_pathname_local(Execute ptr, addr pos, addr *ret);
void merge_pathnames_clang(Execute ptr,
		addr pos, addr defpath, addr defver, addr *ret);
	
/* common-lisp */
void make_pathname(Execute ptr, addr *ret, addr rest);
void get_logical_pathname_translations(addr host, addr *ret);
void set_logical_pathname_translations(Execute ptr, addr host, addr list);
void pathname_host(addr pos, addr *ret, int localp);
void pathname_device(addr pos, addr *ret, int localp);
void pathname_directory(addr pos, addr *ret, int localp);
void pathname_name(addr pos, addr *ret, int localp);
void pathname_type(addr pos, addr *ret, int localp);
void pathname_version(addr pos, addr *ret);
void logical_pathname(Execute ptr, addr *ret, addr pos);
void namestring_pathname(Execute ptr, addr *ret, addr pos);
void file_namestring_pathname(Execute ptr, addr *ret, addr pos);
void directory_namestring_pathname(Execute ptr, addr *ret, addr pos);
void host_namestring_pathname(Execute ptr, addr *ret, addr pos);
void enough_namestring_pathname(Execute ptr, addr *ret, addr pos, addr defaults);
void parse_namestring(Execute ptr, addr *ret, addr *position,
		addr thing, addr host, addr defaults, addr start, addr end, addr junk);
int wild_pathname_boolean(addr file, addr field);
void wild_pathname_p(Execute ptr, addr *ret, addr file, addr field);
int wildcard_pathname(addr a, addr b, int wild);
void pathname_match_p(Execute ptr, addr *ret, addr pos, addr wild);
void translate_pathname(Execute ptr, addr *ret, addr pos, addr from, addr to);
void translate_logical_pathname(Execute ptr, addr *ret, addr pos);
void merge_pathnames(Execute ptr, addr *ret, addr pos, addr defaults, addr version);

/* initialize */
void init_pathname(void);

#endif

