#ifndef __READER_INFO_HEADER__
#define __READER_INFO_HEADER__

#include "execute.h"
#include "typedef.h"

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

_g void get_readinfo(addr pos, size_t index, addr *ret);
_g void set_readinfo(addr pos, size_t index, addr value);
_g struct readinfo_struct *struct_readinfo(addr pos);

_g void getreadinfo(Execute ptr, addr *ret);
_g struct readinfo_struct *getreadinfo_struct(Execute ptr);
_g void pushreadinfo(Execute ptr, addr *ret);
_g void pushreadinfo_recursive(Execute ptr, addr *ret);
_g void getpackage_readinfo(Execute ptr, addr *ret);
_g void setpackage_readinfo(Execute ptr, addr value);
_g void getqueue_readinfo(Execute ptr, addr *ret);
_g unsigned getpreserving_readinfo(Execute ptr);
_g unsigned getescape_readinfo(Execute ptr);
_g void setescape_readinfo(Execute ptr, unsigned value);
_g unsigned getdot_readinfo(Execute ptr);
_g unsigned getreplace_readinfo(Execute ptr, addr *label);
_g enum ReadInfo_State getstate_readinfo(Execute ptr);
_g void setstate_readinfo(Execute ptr, enum ReadInfo_State value);
_g void clear_readinfo(Execute ptr);

#endif

