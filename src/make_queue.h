#ifndef __MAKE_QUEUE_HEADER__
#define __MAKE_QUEUE_HEADER__

#include "constant.h"
#include "local.h"
#include "make_typedef.h"
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
#define code_queue_index _n(code_queue_index)
#define code_queue_ifpush _n(code_queue_ifpush)
#define code_queue_push_code _n(code_queue_push_code)
#define code_queue_pop _n(code_queue_pop)
#define code_make_execute_set_ _n(code_make_execute_set_)
#define code_make_execute_push_ _n(code_make_execute_push_)
#define code_make_execute_rem_ _n(code_make_execute_rem_)
#define code_make_single _n(code_make_single)
#define code_make_object _n(code_make_object)
#define code_queue_make_label _n(code_queue_make_label)
#define code_queue_push_label _n(code_queue_push_label)
#define code_queue_if_unbound _n(code_queue_if_unbound)
#define code_queue_if_nil _n(code_queue_if_nil)
#define code_queue_if_t _n(code_queue_if_t)
#define code_queue_goto _n(code_queue_goto)

#define code_escape_clear _n(code_escape_clear)
#define code_escape_wake _n(code_escape_wake)
#define code_escape_get _n(code_escape_get)
#define code_make_begin _n(code_make_begin)
#define code_make_begin_call _n(code_make_begin_call)
#define code_make_end _n(code_make_end)
#define code_jump_escape _n(code_jump_escape)
#define code_jump_escape_not _n(code_jump_escape_not)
#define code_jump_escape_wake _n(code_jump_escape_wake)
#define code_jump_escape_not_wake _n(code_jump_escape_not_wake)

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

void code_queue_local(LocalRoot local, addr *ret);
enum CodeQueue_Mode code_queue_mode(CodeMake ptr);
int code_queue_setp(CodeMake ptr);
int code_queue_pushp(CodeMake ptr);
int code_queue_remp(CodeMake ptr);

void code_queue_rollback(CodeMake ptr, modeswitch *mode);
void code_queue_setmode(CodeMake ptr, modeswitch *mode);
void code_queue_pushmode(CodeMake ptr, modeswitch *mode);
void code_queue_remmode(CodeMake ptr, modeswitch *mode);
void code_queue_add2(CodeMake ptr, addr x, addr y);
void code_queue_push(CodeMake ptr, addr pos, ...);
void code_queue_list(CodeMake ptr, constindex index, ...);
void code_queue_single(CodeMake ptr, constindex);
void code_queue_cons(CodeMake ptr, constindex x, addr y);
void code_queue_double(CodeMake ptr, constindex x, addr y, addr z);
void code_queue_index(CodeMake ptr, constindex x, size_t y);
#define CodeQueue_single(a,b) code_queue_single(a,CONSTANT_CODE_##b)
#define CodeQueue_cons(a,b,c) code_queue_cons(a,CONSTANT_CODE_##b,c)
#define CodeQueue_double(a,b,c,d) code_queue_double(a,CONSTANT_CODE_##b,c,d)
#define CodeQueue_index(a,b,c) code_queue_index(a,CONSTANT_CODE_##b,c)
void code_queue_ifpush(CodeMake ptr);

void code_queue_push_code(CodeMake ptr);
void code_queue_pop(CodeMake ptr, addr *ret);

int code_make_execute_set_(CodeMake ptr, addr scope);
int code_make_execute_push_(CodeMake ptr, addr scope);
int code_make_execute_rem_(CodeMake ptr, addr scope);
void code_make_single(CodeMake ptr, constindex set, constindex push);
void code_make_object(CodeMake ptr, addr value);

void code_queue_make_label(CodeMake ptr, addr *ret);
void code_queue_push_label(CodeMake ptr, addr label);
void code_queue_if_unbound(CodeMake ptr, addr label);
void code_queue_if_nil(CodeMake ptr, addr label);
void code_queue_if_t(CodeMake ptr, addr label);
void code_queue_goto(CodeMake ptr, addr label);

void code_escape_clear(CodeMake ptr);
void code_escape_wake(CodeMake ptr);
int code_escape_get(CodeMake ptr);
void code_make_begin(CodeMake ptr, fixnum *ret);
void code_make_begin_call(CodeMake ptr, fixnum *ret);
void code_make_end(CodeMake ptr, fixnum value);
void code_jump_escape(CodeMake ptr, addr label);
void code_jump_escape_not(CodeMake ptr, addr label);
void code_jump_escape_wake(CodeMake ptr, addr label);
void code_jump_escape_not_wake(CodeMake ptr, addr label);

#endif

