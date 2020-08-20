#ifndef __CODE_QUEUE_HEADER__
#define __CODE_QUEUE_HEADER__

#include "constant.h"
#include "local.h"
#include "typedef.h"

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

