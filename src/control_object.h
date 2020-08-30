#ifndef __CONTROL_OBJECT_HEADER__
#define __CONTROL_OBJECT_HEADER__

#include "constant.h"
#include "define.h"
#include "execute.h"
#include "pointer.h"

#define GcCounterForce _n(GcCounterForce)
#define ControlCounter _n(ControlCounter)
#define ptrbodycontrol_debug _n(ptrbodycontrol_debug)
#define structcontrol_debug _n(structcontrol_debug)
#define getcontrol_debug _n(getcontrol_debug)
#define setcontrol_debug _n(setcontrol_debug)
#define taginfo_heap _n(taginfo_heap)
#define ptrtaginfo_debug _n(ptrtaginfo_debug)
#define structtaginfo_debug _n(structtaginfo_debug)
#define getnametaginfo_debug _n(getnametaginfo_debug)
#define setnametaginfo_debug _n(setnametaginfo_debug)
#define handler_local _n(handler_local)
#define getnamehandler_debug _n(getnamehandler_debug)
#define setnamehandler_debug _n(setnamehandler_debug)
#define getcallhandler_debug _n(getcallhandler_debug)
#define setcallhandler_debug _n(setcallhandler_debug)
#define getescape_handler _n(getescape_handler)
#define setescape_handler _n(setescape_handler)
#define getdisable_handler _n(getdisable_handler)
#define setdisable_handler _n(setdisable_handler)
#define checkhandler_control_ _n(checkhandler_control_)
#define push_control _n(push_control)
#define push_args_control _n(push_args_control)
#define pop_control_ _n(pop_control_)
#define free_control_degrade_ _n(free_control_degrade_)
#define stack_check_control _n(stack_check_control)
#define pushspecial_control _n(pushspecial_control)
#define pushtaginfo_control _n(pushtaginfo_control)
#define pushhandler_control _n(pushhandler_control)
#define pushrestart_control _n(pushrestart_control)
#define existspecial_control _n(existspecial_control)
#define getdata_control _n(getdata_control)
#define setdata_control _n(setdata_control)
#define getcatch_control _n(getcatch_control)
#define getcondition_control _n(getcondition_control)
#define gethandler_control _n(gethandler_control)
#define getrestart_control _n(getrestart_control)
#define setcatch_control _n(setcatch_control)
#define sethandler_control _n(sethandler_control)
#define setrestart_control _n(setrestart_control)
#define setprotect_value_control _n(setprotect_value_control)
#define setprotect_control _n(setprotect_control)
#define setprotect_control_local _n(setprotect_control_local)

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
	Control_Special,
	Control_Close,
	Control_Protect,
	Control_Table,
	Control_Data,
	Control_Size
};

struct control_struct {
	LocalStack stack;
	addr *lexical_reader;
	addr lexical_vector;
	addr trace;
	size_t point;
};

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
 *  special
 */
enum Special_Index {
	Special_Symbol,
	Special_Value,
	Special_Size
};


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

#ifdef LISP_DEBUG
#define GetNameHandler				getnamehandler_debug
#define SetNameHandler				setnamehandler_debug
#define GetCallHandler				getcallhandler_debug
#define SetCallHandler				setcallhandler_debug
#else
#define GetNameHandler				GetNameHandler_Low
#define SetNameHandler				SetNameHandler_Low
#define GetCallHandler				GetCallHandler_Low
#define SetCallHandler				SetCallHandler_Low
#endif

_g void handler_local(LocalRoot local, addr *ret, addr name, addr call, int esc);
_g void getnamehandler_debug(addr pos, addr *ret);
_g void setnamehandler_debug(addr pos, addr value);
_g void getcallhandler_debug(addr pos, addr *ret);
_g void setcallhandler_debug(addr pos, addr value);

_g int getescape_handler(addr pos);
_g void setescape_handler(addr pos, int value);
_g int getdisable_handler(addr pos);
_g void setdisable_handler(addr pos, int value);
_g int checkhandler_control_(addr pos, addr instance, int *ret);


/*
 *  function
 */
/* push control */
_g void push_control(Execute ptr, addr *ret);
_g void push_args_control(Execute ptr, addr *ret);
_g int pop_control_(Execute ptr, addr control);
_g int free_control_degrade_(Execute ptr, addr control);

/* data */
_g int stack_check_control(Execute ptr);
_g void pushspecial_control(Execute ptr, addr pos, addr value);
_g void pushtaginfo_control(Execute ptr, addr pos);
_g void pushhandler_control(Execute ptr, addr pos);
_g void pushrestart_control(Execute ptr, addr pos);
_g int existspecial_control(Execute ptr, addr pos);

/* access */
_g void getdata_control(Execute ptr, addr *ret);
_g void setdata_control(Execute ptr, addr value);

_g int getcatch_control(addr pos, addr *ret);
_g int getcondition_control(addr pos, addr *ret);
_g int gethandler_control(addr pos, addr *ret);
_g int getrestart_control(addr pos, addr *ret);

_g void setcatch_control(LocalRoot local, addr pos, addr value);
_g void sethandler_control(LocalRoot local, addr pos, addr value);
_g void setrestart_control(LocalRoot local, addr pos, addr value);
_g void setprotect_value_control(addr pos, addr value);
_g void setprotect_control(Execute ptr, pointer id, addr value);
_g void setprotect_control_local(Execute ptr, pointer id, addr value);

#endif

