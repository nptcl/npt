#ifndef __READER_INFO_HEADER__
#define __READER_INFO_HEADER__

#include "execute.h"
#include "typedef.h"

#define get_readinfo _n(get_readinfo)
#define set_readinfo _n(set_readinfo)
#define struct_readinfo _n(struct_readinfo)
#define getreadinfo _n(getreadinfo)
#define getreadinfo_struct _n(getreadinfo_struct)
#define pushreadinfo _n(pushreadinfo)
#define pushreadinfo_recursive_ _n(pushreadinfo_recursive_)
#define getpackage_readinfo _n(getpackage_readinfo)
#define setpackage_readinfo _n(setpackage_readinfo)
#define getqueue_readinfo _n(getqueue_readinfo)
#define getpreserving_readinfo _n(getpreserving_readinfo)
#define getescape_readinfo _n(getescape_readinfo)
#define setescape_readinfo _n(setescape_readinfo)
#define getdot_readinfo _n(getdot_readinfo)
#define getreplace_readinfo _n(getreplace_readinfo)
#define getstate_readinfo _n(getstate_readinfo)
#define setstate_readinfo _n(setstate_readinfo)
#define clear_readinfo _n(clear_readinfo)

enum ReadInfo_Index {
	ReadInfo_Package,
	ReadInfo_Queue,
	ReadInfo_Label,
	ReadInfo_Size
};

enum ReadInfo_State {
	ReadInfo_State_First,
	ReadInfo_State_Colon1,
	ReadInfo_State_Colon2,
	ReadInfo_State_Gensym
};

struct readinfo_struct {
	unsigned preserving : 1;
	unsigned recursive : 1;
	unsigned escape : 1;
	unsigned dot : 1;
	unsigned replace : 1;
	unsigned unexport : 1;
	enum ReadInfo_State state : 4;
	size_t backquote;
};

#define GetReadInfo_Low			GetArraySS
#define SetReadInfo_Low			SetArraySS
#define ReadInfoStruct_Low(x)	\
	((struct readinfo_struct *)PtrBodySSa((x), ReadInfo_Size))

#ifdef LISP_DEBUG
#define GetReadInfo				get_readinfo
#define SetReadInfo				set_readinfo
#define ReadInfoStruct			struct_readinfo
#else
#define GetReadInfo				GetReadInfo_Low
#define SetReadInfo				SetReadInfo_Low
#define ReadInfoStruct			ReadInfoStruct_Low
#endif

void get_readinfo(addr pos, size_t index, addr *ret);
void set_readinfo(addr pos, size_t index, addr value);
struct readinfo_struct *struct_readinfo(addr pos);

void getreadinfo(Execute ptr, addr *ret);
struct readinfo_struct *getreadinfo_struct(Execute ptr);
void pushreadinfo(Execute ptr, addr *ret);
int pushreadinfo_recursive_(Execute ptr, addr *ret);
void getpackage_readinfo(Execute ptr, addr *ret);
void setpackage_readinfo(Execute ptr, addr value);
void getqueue_readinfo(Execute ptr, addr *ret);
unsigned getpreserving_readinfo(Execute ptr);
unsigned getescape_readinfo(Execute ptr);
void setescape_readinfo(Execute ptr, unsigned value);
unsigned getdot_readinfo(Execute ptr);
unsigned getreplace_readinfo(Execute ptr, addr *label);
enum ReadInfo_State getstate_readinfo(Execute ptr);
void setstate_readinfo(Execute ptr, enum ReadInfo_State value);
void clear_readinfo(Execute ptr);

#endif

