#ifndef __PATHNAME_OBJECT_HEADER__
#define __PATHNAME_OBJECT_HEADER__

#include "equal.h"
#include "local.h"
#include "typedef.h"

#define getarray_pathname _n(getarray_pathname)
#define setarray_pathname _n(setarray_pathname)
#define reflogical_pathname _n(reflogical_pathname)
#define getlogical_pathname _n(getlogical_pathname)
#define setlogical_pathname _n(setlogical_pathname)
#define gethost_pathname _n(gethost_pathname)
#define sethost_pathname _n(sethost_pathname)
#define getdevice_pathname _n(getdevice_pathname)
#define setdevice_pathname _n(setdevice_pathname)
#define getdirectory_pathname _n(getdirectory_pathname)
#define setdirectory_pathname _n(setdirectory_pathname)
#define getname_pathname _n(getname_pathname)
#define setname_pathname _n(setname_pathname)
#define gettype_pathname _n(gettype_pathname)
#define settype_pathname _n(settype_pathname)
#define getversion_pathname _n(getversion_pathname)
#define setversion_pathname _n(setversion_pathname)
#define pathname_equal_function _n(pathname_equal_function)

#define make_pathname_alloc _n(make_pathname_alloc)
#define pathname_alloc _n(pathname_alloc)
#define pathname_local _n(pathname_local)
#define pathname_heap _n(pathname_heap)
#define logical_pathname_alloc _n(logical_pathname_alloc)
#define logical_pathname_local _n(logical_pathname_local)
#define logical_pathname_heap _n(logical_pathname_heap)
#define pathnamep _n(pathnamep)
#define pathname_pathname_p _n(pathname_pathname_p)
#define pathname_logical_p _n(pathname_logical_p)
#define pathname_file_p _n(pathname_file_p)
#define pathname_directory_p _n(pathname_directory_p)
#define copylocal_pathname_array _n(copylocal_pathname_array)
#define copy_pathname_alloc _n(copy_pathname_alloc)
#define pathname_equal_ _n(pathname_equal_)

#define make_pathname_heap_ _n(make_pathname_heap_)
#define pathname_host_ _n(pathname_host_)
#define pathname_device_ _n(pathname_device_)
#define pathname_directory_ _n(pathname_directory_)
#define pathname_name_ _n(pathname_name_)
#define pathname_type_ _n(pathname_type_)
#define pathname_version _n(pathname_version)

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

/* access */
void getarray_pathname(addr pos, enum PATHNAME_INDEX index, addr *ret);
void setarray_pathname(addr pos, enum PATHNAME_INDEX index, addr value);
int reflogical_pathname(addr pos);
void getlogical_pathname(addr pos, int *ret);
void setlogical_pathname(addr pos, int value);
void gethost_pathname(addr pos, addr *ret);
void sethost_pathname(addr pos, addr value);
void getdevice_pathname(addr pos, addr *ret);
void setdevice_pathname(addr pos, addr value);
void getdirectory_pathname(addr pos, addr *ret);
void setdirectory_pathname(addr pos, addr value);
void getname_pathname(addr pos, addr *ret);
void setname_pathname(addr pos, addr value);
void gettype_pathname(addr pos, addr *ret);
void settype_pathname(addr pos, addr value);
void getversion_pathname(addr pos, addr *ret);
void setversion_pathname(addr pos, addr value);
lisp_equal_calltype pathname_equal_function(addr pos);

/* pathname object */
void make_pathname_alloc(LocalRoot local, addr *ret, int logical);

/* pathname */
void pathname_alloc(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type);
void pathname_local(LocalRoot local, addr *ret,
		addr host, addr device, addr directory, addr name, addr type);
void pathname_heap(addr *ret,
		addr host, addr device, addr directory, addr name, addr type);

/* logical-pathname */
void logical_pathname_alloc(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version);
void logical_pathname_local(LocalRoot local, addr *ret, addr host,
		addr directory, addr name, addr type, addr version);
void logical_pathname_heap(addr *ret, addr host,
		addr directory, addr name, addr type, addr version);

/* check */
int pathnamep(addr pos);
int pathname_pathname_p(addr pos);
int pathname_logical_p(addr pos);
int pathname_file_p(addr pos);
int pathname_directory_p(addr pos);
void copylocal_pathname_array(LocalRoot local, addr a, int i, addr b);
void copy_pathname_alloc(LocalRoot local, addr *ret, addr pos);
#define copy_pathname_heap(x,y) copy_pathname_alloc(NULL, (x), (y))
int pathname_equal_(addr left, addr right, int *ret);

int make_pathname_heap_(addr *ret,
		addr host, addr device, addr directory,
		addr name, addr type, addr version, int localp);
int pathname_host_(addr pos, addr *ret, int localp);
int pathname_device_(addr pos, addr *ret, int localp);
int pathname_directory_(addr pos, addr *ret, int localp);
int pathname_name_(addr pos, addr *ret, int localp);
int pathname_type_(addr pos, addr *ret, int localp);
void pathname_version(addr pos, addr *ret);

#endif

