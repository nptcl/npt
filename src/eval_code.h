#ifndef __EVAL_CODE_HEADER__
#define __EVAL_CODE_HEADER__

#include "local.h"
#include "typedef.h"

enum EvalCode_Mode {
	EvalCode_ModeSet,
	EvalCode_ModePush,
	EvalCode_ModeRemove,
	EvalCode_ModeSize
};

_g enum EvalCode_Mode evalcode_mode(addr code);

_g void evalcode_single(LocalRoot, addr, constindex);
_g void evalcode_carcdr(LocalRoot, addr, constindex, addr);
_g void evalcode_double(LocalRoot, addr, constindex, addr, addr);
_g void evalcode_push3(LocalRoot, addr, constindex, addr, addr, addr);
#define EvalCode_single(a,b,c) evalcode_single(a,b,CONSTANT_CODE_##c)
#define EvalCode_carcdr(a,b,c,d) evalcode_carcdr(a,b,CONSTANT_CODE_##c,d)
#define EvalCode_double(a,b,c,d,e) evalcode_double(a,b,CONSTANT_CODE_##c,d,e)
#define EvalCode_push3(a,b,c,d,e,f) evalcode_push3(a,b,CONSTANT_CODE_##c,d,e,f)

_g void eval_code_execute_set(LocalRoot local, addr code, addr scope);
_g void eval_code_execute_push(LocalRoot local, addr code, addr scope);
_g void eval_code_execute_rem(LocalRoot local, addr code, addr scope);

_g void evalcode_single_sp(LocalRoot, addr, constindex, constindex);
_g void evalcode_carcdr_sp(LocalRoot, addr, constindex, constindex, addr);
#define EvalCode_single_sp(a,b,c,d) \
	evalcode_single_sp(a,b,CONSTANT_CODE_##c,CONSTANT_CODE_##d)
#define EvalCode_carcdr_sp(a,b,c,d,e) \
	evalcode_carcdr_sp(a,b,CONSTANT_CODE_##c,CONSTANT_CODE_##d,e)

_g void eval_code_object(LocalRoot local, addr code, addr value);
_g void eval_code_execute(LocalRoot local, addr code, addr scope);
_g void eval_code(LocalRoot local, addr *ret, addr scope);
_g void init_eval_code(void);

#endif

