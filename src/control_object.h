#ifndef __CONTROL_OBJECT_HEADER__
#define __CONTROL_OBJECT_HEADER__

#include "constant.h"
#include "define.h"
#include "execute.h"
#include "pointer.h"

#ifdef LISP_DEBUG_FORCE_GC
__extern size_t GcCounterForce;
#endif
__extern size_t ControlCounter;

/*
 *  control
 */
enum Control_Index {
	Control_Next,
	Control_Cons,
	Control_ConsTail,
	Control_Lexical,
	Control_Special,
	Control_Table,
	Control_Data,
	Control_Size
};

struct control_struct {
	unsigned p_protect : 1;
	LocalStack stack;
	addr trace;
	size_t point;
};

#define exit_control(ptr)				exit_code(ptr, LISPCODE_CONTROL);
#define PtrBodyControl_Low(p)			PtrBodySSa(p, Control_Size)
#define StructControl_Low(p)			((struct control_struct *)PtrBodyControl(p))
#define GetControl_Low					GetArraySS
#define SetControl_Low					SetArraySS

#ifdef LISP_DEBUG
#define PtrBodyControl					ptrbodycontrol_debug
#define StructControl					structcontrol_debug
#define GetControl						getcontrol_debug
#define SetControl						setcontrol_debug
#else
#define PtrBodyControl					PtrBodyControl_Low
#define StructControl					StructControl_Low
#define GetControl						GetControl_Low
#define SetControl						SetControl_Low
#endif

_g void *ptrbodycontrol_debug(addr pos);
_g struct control_struct *structcontrol_debug(addr pos);
_g void getcontrol_debug(addr pos, size_t index, addr *ret);
_g void setcontrol_debug(addr pos, size_t index, addr value);


/*
 *  taginfo
 */
enum TagInfo_Index {
	TagInfo_Name,
	TagInfo_Size
};

struct taginfo_struct {
	int open;
	addr control;
	size_t point;
};

#define PtrTagInfo_Low(p)			PtrBodySSa((p), TagInfo_Size)
#define StructTagInfo_Low(p)		((struct taginfo_struct *)PtrTagInfo(p))
#define GetNameTagInfo_Low(p,v)		GetArraySS(p, TagInfo_Name, v)
#define SetNameTagInfo_Low(p,v)		SetArraySS(p, TagInfo_Name, v)

#ifdef LISP_DEBUG
#define PtrTagInfo					ptrtaginfo_debug
#define StructTagInfo				structtaginfo_debug
#define GetNameTagInfo				getnametaginfo_debug
#define SetNameTagInfo				setnametaginfo_debug
#else
#define PtrTagInfo					PtrTagInfo_Low
#define StructTagInfo				StructTagInfo_Low
#define GetNameTagInfo				GetNameTagInfo_Low
#define SetNameTagInfo				SetNameTagInfo_Low
#endif

_g void taginfo_heap(addr *ret, addr control, addr tag, size_t point);
_g void *ptrtaginfo_debug(addr pos);
_g struct taginfo_struct *structtaginfo_debug(addr pos);
_g void getnametaginfo_debug(addr pos, addr *ret);
_g void setnametaginfo_debug(addr pos, addr value);


/*
 *  handler
 */
enum Handler_Index {
	Handler_Name,
	Handler_Call,
	Handler_Size
};

#define GetNameHandler_Low(p,x)		GetArrayA2((p), Handler_Name, (x))
#define SetNameHandler_Low(p,x)		SetArrayA2((p), Handler_Name, (x))
#define GetCallHandler_Low(p,x)		GetArrayA2((p), Handler_Call, (x))
#define SetCallHandler_Low(p,x)		SetArrayA2((p), Handler_Call, (x))
#define GetEscapeHandler_Low(p,x)	(*(x) = (int)GetUser(p))
#define SetEscapeHandler_Low(p,x)	SetUser((p), (byte)(x))

#ifdef LISP_DEBUG
#define GetNameHandler				getnamehandler_debug
#define SetNameHandler				setnamehandler_debug
#define GetCallHandler				getcallhandler_debug
#define SetCallHandler				setcallhandler_debug
#define GetEscapeHandler			getescapehandler_debug
#define SetEscapeHandler			setescapehandler_debug
#else
#define GetNameHandler				GetNameHandler_Low
#define SetNameHandler				SetNameHandler_Low
#define GetCallHandler				GetCallHandler_Low
#define SetCallHandler				SetCallHandler_Low
#define GetEscapeHandler			GetEscapeHandler_Low
#define SetEscapeHandler			SetEscapeHandler_Low
#endif

_g void handler_local(LocalRoot local, addr *ret, addr name, addr call, int esc);
_g void getnamehandler_debug(addr pos, addr *ret);
_g void setnamehandler_debug(addr pos, addr value);
_g void getcallhandler_debug(addr pos, addr *ret);
_g void setcallhandler_debug(addr pos, addr value);
_g void getescapehandler_debug(addr pos, int *ret);
_g void setescapehandler_debug(addr pos, int value);
_g int checkhandler_control(addr pos, addr instance);


/*
 *  function
 */
/* push control */
_g struct control_struct *push_control(Execute ptr);
_g void push_new_control(Execute ptr, addr *ret);
_g void push_args_control(Execute ptr, addr *ret);
_g int free_control_(Execute ptr, addr control);
_g int rollback_control_(Execute ptr, addr control);

/* data */
_g int stack_check_control(Execute ptr);
_g void pushlexical_control(Execute ptr, addr pos, addr value);
_g void pushspecial_control(Execute ptr, addr pos, addr value);
_g void pushcallname_control(Execute ptr, addr pos, addr value);
_g void pushfunction_control(Execute ptr, addr pos, addr value);
_g void pushsetf_control(Execute ptr, addr pos, addr value);
_g void pushtable_control(Execute ptr, constindex index, addr pos);
_g void pushtagbody_control(Execute ptr, addr pos, addr value);
_g void pushblock_control(Execute ptr, addr pos);
_g int existspecial_control(Execute ptr, addr pos);

/* access */
_g void getdata_control(Execute ptr, addr *ret);
_g void setdata_control(Execute ptr, addr value);

_g int gettable_control(addr pos, constindex index, addr *ret);
_g int gettagbody_control(addr pos, addr *ret);
_g int getblock_control(addr pos, addr *ret);
_g int getcatch_control(addr pos, addr *ret);
_g int getcondition_control(addr pos, addr *ret);
_g int gethandler_control(addr pos, addr *ret);
_g int getrestart_control(addr pos, addr *ret);

_g void settable_control(LocalRoot local, addr control, constindex index, addr value);
_g void seteval_control(LocalRoot local, addr pos);
_g void settagbody_control(LocalRoot local, addr pos, addr value);
_g void setblock_control(LocalRoot local, addr pos, addr value);
_g void setcatch_control(LocalRoot local, addr pos, addr value);
_g void setprotect_plist_control(LocalRoot local, addr pos, addr value);
_g void setprotect_control(Execute ptr, pointer id, addr value);
_g void setprotect_control_local(Execute ptr, pointer id, addr value);

#endif

