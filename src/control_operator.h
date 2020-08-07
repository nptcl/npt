#ifndef __CONTROL_OPERATOR_HEADER__
#define __CONTROL_OPERATOR_HEADER__

#include "execute.h"
#include "execute_values.h"
#include "pointer.h"

/* arguments */
_g void setargs_va_control(Execute ptr, ...);
_g void setargs_nil_control(Execute ptr);
_g void setargs_list_control(Execute ptr, addr list);
_g void pushargs_control(Execute ptr, addr value);
_g int popargs_control_(Execute ptr, addr *ret);
_g void getargs_control(Execute ptr, size_t index, addr *ret);
_g void getargs_tail_control(Execute ptr, addr *ret);
#define GetArgsControl(ptr, ret) GetControl((ptr)->control, Control_Cons, (ret))
_g void getargs_list_control_unsafe(Execute ptr, size_t index, addr *ret);
_g void getargs_list_control_heap(Execute ptr, size_t index, addr *ret);
_g void pushargs_allvalues(Execute ptr);

/* flow control */
_g int goto_control_(Execute ptr, size_t point);
_g int go_control_(Execute ptr, addr tag);
_g int return_from_control_(Execute ptr, addr name);
_g void catch_control(Execute ptr, addr name);
_g int throw_control_(Execute ptr, addr name);

_g int pushhandler_common_(Execute ptr, addr name, addr call, int escape);
_g void reverse_handler_control(Execute ptr);
_g void pushbind_restart_control(Execute ptr, addr list, int escape);
_g void reverse_restart_control(Execute ptr);

_g int find_condition_control_(Execute ptr, addr instance, int *ret);
_g int invoke_handler_control_(Execute ptr, addr pos);
_g int invoke_restart_control_(Execute ptr, addr restart, addr args);
_g int invoke_restart_interactively_control_(Execute ptr, addr restart);

_g int find_restart_control_(Execute ptr,
		addr name, addr condition, addr *value, int *ret);
_g int find_restart_control_error_(Execute ptr, addr name, addr condition, addr *ret);
_g int compute_restarts_control_(Execute ptr, addr condition, addr *ret);

_g int restart_control(Execute ptr, int (*call)(Execute, void *), void *voidp);
_g int restart0_control(Execute ptr, addr restart,
		int (*call)(Execute, void *), void *voidp);
_g int restart1_control(Execute ptr, addr restart,
		int (*call)(Execute, addr), addr v1);
_g int restart1r_control(Execute ptr, addr restart,
		int (*call)(Execute, addr, addr *), addr v1, addr *ret);
_g int restart2_control(Execute ptr, addr restart,
		int (*call)(Execute, addr, addr), addr v1, addr v2);
	
_g void set_taginfo_control(Execute ptr, addr list);
_g void set_blockinfo_control(Execute ptr, addr pos);
_g void set_protect_control(Execute ptr, addr pos);
_g int catch_clang(Execute ptr, pointer call, addr tag, addr value);

#endif

