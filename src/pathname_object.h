#ifndef __PATHNAME_OBJECT_HEADER__
#define __PATHNAME_OBJECT_HEADER__

#include "equal.h"
#include "local.h"
#include "typedef.h"

enum PATHNAME_INDEX {
	PATHNAME_INDEX_HOST,
	PATHNAME_INDEX_DEVICE,
	PATHNAME_INDEX_DIRECTORY,
	PATHNAME_INDEX_NAME,
	PATHNAME_INDEX_TYPE,
	PATHNAME_INDEX_VERSION,
	PATHNAME_INDEX_SIZE
};

#define GetArrayPathname_Low			GetArrayA2
#define SetArrayPathname_Low			SetArrayA2
#define RefLogicalPathname_Low(x)		((int)GetUser(x))
#define GetLogicalPathname_Low(x,y)		(*(y) = (int)GetUser(x))
#define SetLogicalPathname_Low(x,y)		(SetUser((x),(y)))

#define GetHostPathname_Low(x,y)		GetArrayA2((x),PATHNAME_INDEX_HOST,(y))
#define SetHostPathname_Low(x,y)		SetArrayA2((x),PATHNAME_INDEX_HOST,(y))
#define GetDevicePathname_Low(x,y)		GetArrayA2((x),PATHNAME_INDEX_DEVICE,(y))
#define SetDevicePathname_Low(x,y)		SetArrayA2((x),PATHNAME_INDEX_DEVICE,(y))
#define GetDirectoryPathname_Low(x,y)	GetArrayA2((x),PATHNAME_INDEX_DIRECTORY,(y))
#define SetDirectoryPathname_Low(x,y)	SetArrayA2((x),PATHNAME_INDEX_DIRECTORY,(y))
#define GetNamePathname_Low(x,y)		GetArrayA2((x),PATHNAME_INDEX_NAME,(y))
#define SetNamePathname_Low(x,y)		SetArrayA2((x),PATHNAME_INDEX_NAME,(y))
#define GetTypePathname_Low(x,y)		GetArrayA2((x),PATHNAME_INDEX_TYPE,(y))
#define SetTypePathname_Low(x,y)		SetArrayA2((x),PATHNAME_INDEX_TYPE,(y))
#define GetVersionPathname_Low(x,y)		GetArrayA2((x),PATHNAME_INDEX_VERSION,(y))
#define SetVersionPathname_Low(x,y)		SetArrayA2((x),PATHNAME_INDEX_VERSION,(y))

#ifdef LISP_DEBUG
#define GetArrayPathname				getarray_pathname
#define SetArrayPathname				setarray_pathname
#define RefLogicalPathname				reflogical_pathname
#define GetLogicalPathname				getlogical_pathname
#define SetLogicalPathname				setlogical_pathname

#define GetHostPathname					gethost_pathname
#define SetHostPathname					sethost_pathname
#define GetDevicePathname				getdevice_pathname
#define SetDevicePathname				setdevice_pathname
#define GetDirectoryPathname			getdirectory_pathname
#define SetDirectoryPathname			setdirectory_pathname
#define GetNamePathname					getname_pathname
#define SetNamePathname					setname_pathname
#define GetTypePathname					gettype_pathname
#define SetTypePathname					settype_pathname
#define GetVersionPathname				getversion_pathname
#define SetVersionPathname				setversion_pathname
#else
#define GetArrayPathname				GetArrayPathname_Low
#define SetArrayPathname				SetArrayPathname_Low
#define RefLogicalPathname				RefLogicalPathname_Low
#define GetLogicalPathname				GetLogicalPathname_Low
#define SetLogicalPathname				SetLogicalPathname_Low

#define GetHostPathname					GetHostPathname_Low
#define SetHostPathname					SetHostPathname_Low
#define GetDevicePathname				GetDevicePathname_Low
#define SetDevicePathname				SetDevicePathname_Low
#define GetDirectoryPathname			GetDirectoryPathname_Low
#define SetDirectoryPathname			SetDirectoryPathname_Low
#define GetNamePathname					GetNamePathname_Low
#define SetNamePathname					SetNamePathname_Low
#define GetTypePathname					GetTypePathname_Low
#define SetTypePathname					SetTypePathname_Low
#define GetVersionPathname				GetVersionPathname_Low
#define SetVersionPathname				SetVersionPathname_Low
#endif

#ifdef LISP_PATHNAME_EQUALP
#define LispPathnameEqual_ equalp_function_
#else
#define LispPathnameEqual_ equal_function_
#endif

/* access */
_g void getarray_pathname(addr pos, enum PATHNAME_INDEX index, addr *ret);
_g void setarray_pathname(addr pos, enum PATHNAME_INDEX index, addr value);
_g int reflogical_pathname(addr pos);
_g void getlogical_pathname(addr pos, int *ret);
_g void setlogical_pathname(addr pos, int value);
_g void gethost_pathname(addr pos, addr *ret);
_g void sethost_pathname(addr pos, addr value);
_g void getdevice_pathname(addr pos, addr *ret);
_g void setdevice_pathname(addr pos, addr value);
_g void getdirectory_pathname(addr pos, addr *ret);
_g void setdirectory_pathname(addr pos, addr value);
_g void getname_pathname(addr pos, addr *ret);
_g void setname_pathname(addr pos, addr value);
_g void gettype_pathname(addr pos, addr *ret);
_g void settype_pathname(addr pos, addr value);
_g void getversion_pathname(addr pos, addr *ret);
_g void setversion_pathname(addr pos, addr value);

/* pathname object */
_g void make_pathname_alloc(LocalRoot local, addr *ret, int logical);

/* pathname */
_g void pathname_alloc(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type);
_g void pathname_local(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type);
_g void pathname_heap(addr *ret,
		addr host, addr device, addr directory, addr name, addr type);

/* logical-pathname */
_g void logical_pathname_alloc(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version);
_g void logical_pathname_local(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version);
_g void logical_pathname_heap(addr *ret, addr host,
		addr directory, addr name, addr type, addr version);

/* check */
_g int pathnamep(addr pos);
_g int pathname_pathname_p(addr pos);
_g int pathname_logical_p(addr pos);
_g int pathname_file_p(addr pos);
_g int pathname_directory_p(addr pos);
_g void copylocal_pathname_array(LocalRoot local, addr a, int i, addr b);
_g void copy_pathname_alloc(LocalRoot local, addr *ret, addr pos);
#define copy_pathname_heap(x,y) copy_pathname_alloc(NULL, (x), (y))
_g int pathname_equal_(addr left, addr right, int *ret);
_g int wild_pathname_boolean_(addr file, addr field, int *ret);
_g int wildcard_stringp_p_(addr pos, int *ret);
_g int wildcard_string_pathname_(addr a, addr b, int *ret);
_g int wildcard_eq_pathname_(addr a, addr b, int *ret);

#endif

