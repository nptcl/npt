#ifndef __CODE_QUEUE_HEADER__
#define __CODE_QUEUE_HEADER__

#include "constant.h"
#include "local.h"
#include "typedef.h"

#define code_queue_local _n(code_queue_local)
#define code_queue_mode _n(code_queue_mode)
#define code_queue_setp _n(code_queue_setp)
#define code_queue_pushp _n(code_queue_pushp)
#define code_queue_remp _n(code_queue_remp)
#define code_queue_rollback _n(code_queue_rollback)
#define code_queue_setmode _n(code_queue_setmode)
#define code_queue_pushmode _n(code_queue_pushmode)
#define code_queue_remmode _n(code_queue_remmode)
#define code_queue_add2 _n(code_queue_add2)
#define code_queue_push _n(code_queue_push)
#define code_queue_list _n(code_queue_list)
#define code_queue_single _n(code_queue_single)
#define code_queue_cons _n(code_queue_cons)
#define code_queue_double _n(code_queue_double)
#define code_queue_ifpush _n(code_queue_ifpush)
#define code_queue_push_simple _n(code_queue_push_simple)
#define code_queue_push_new _n(code_queue_push_new)
#define code_queue_push_args _n(code_queue_push_args)
#define code_queue_pop _n(code_queue_pop)
#define code_make_execute_set _n(code_make_execute_set)
#define code_make_execute_push _n(code_make_execute_push)
#define code_make_execute_rem _n(code_make_execute_rem)
#define code_make_execute_control _n(code_make_execute_control)
#define code_make_single _n(code_make_single)
#define code_make_object _n(code_make_object)
#define code_queue_make_label _n(code_queue_make_label)
#define code_queue_push_label _n(code_queue_push_label)
#define code_queue_if_unbound _n(code_queue_if_unbound)
#define code_queue_if_nil _n(code_queue_if_nil)
#define code_queue_if_t _n(code_queue_if_t)
#define code_queue_goto _n(code_queue_goto)

enum CodeQueue_Mode {
	CodeQueue_ModeSet,
	CodeQueue_ModePush,
	CodeQueue_ModeRemove,
	CodeQueue_ModeSize
};

enum CodeQueue_Index {
	CodeQueue_Code,
	CodeQueue_Stack,
	CodeQueue_Size
};

struct code_queue {
	enum CodeQueue_Mode mode;
	size_t size, label;
};

struct code_queue_switch {
	enum CodeQueue_Mode mode;
};
typedef struct code_queue_switch modeswitch;

#define RefCodeQueue				RefEval
#define GetCodeQueue				GetEval
#define SetCodeQueue				SetEval
#define PtrCodeQueue(p)			PtrEvalBody(p, CodeQueue_Size)
#define StructCodeQueue(p)		((struct code_queue *)PtrCodeQueue(p))
#define CheckTypeCodeQueue(p)	Check(! eval_code_p(p), "type error")

#define ConstantCode(x,y,z) ((x)? CONSTANT_CODE_##y: CONSTANT_CODE_##z)
#define GetConstantCode(x,y,z,w) GetConstant(ConstantCode(x,y,z),(w))

_g void code_queue_local(LocalRoot local, addr *ret);
_g enum CodeQueue_Mode code_queue_mode(addr code);
_g int code_queue_setp(addr code);
_g int code_queue_pushp(addr code);
_g int code_queue_remp(addr code);

_g void code_queue_rollback(addr code, modeswitch *mode);
_g void code_queue_setmode(addr code, modeswitch *mode);
_g void code_queue_pushmode(addr code, modeswitch *mode);
_g void code_queue_remmode(addr code, modeswitch *mode);
_g void code_queue_add2(LocalRoot local, addr code, addr left, addr right);
_g void code_queue_push(LocalRoot local, addr code, addr pos, ...);
_g void code_queue_list(LocalRoot local, addr code, constindex index, ...);
_g void code_queue_single(LocalRoot, addr, constindex);
_g void code_queue_cons(LocalRoot, addr, constindex, addr);
_g void code_queue_double(LocalRoot, addr, constindex, addr, addr);
#define CodeQueue_single(a,b,c) code_queue_single(a,b,CONSTANT_CODE_##c)
#define CodeQueue_cons(a,b,c,d) code_queue_cons(a,b,CONSTANT_CODE_##c,d)
#define CodeQueue_double(a,b,c,d,e) code_queue_double(a,b,CONSTANT_CODE_##c,d,e)
_g void code_queue_ifpush(LocalRoot local, addr code);

_g void code_queue_push_simple(LocalRoot local, addr code);
_g void code_queue_push_new(LocalRoot local, addr code);
_g void code_queue_push_args(LocalRoot local, addr code);
_g void code_queue_pop(LocalRoot local, addr code, addr *ret);

_g void code_make_execute_set(LocalRoot local, addr code, addr scope);
_g void code_make_execute_push(LocalRoot local, addr code, addr scope);
_g void code_make_execute_rem(LocalRoot local, addr code, addr scope);
_g void code_make_execute_control(LocalRoot local, addr code, addr pos);
_g void code_make_single(LocalRoot, addr, constindex, constindex);
_g void code_make_object(LocalRoot local, addr code, addr value);

_g void code_queue_make_label(LocalRoot local, addr code, addr *ret);
_g void code_queue_push_label(LocalRoot local, addr code, addr label);
_g void code_queue_if_unbound(LocalRoot local, addr code, addr label);
_g void code_queue_if_nil(LocalRoot local, addr code, addr label);
_g void code_queue_if_t(LocalRoot local, addr code, addr label);
_g void code_queue_goto(LocalRoot local, addr code, addr label);

#endif

