#ifndef __CONTROL_OPERATOR_HEADER__
#define __CONTROL_OPERATOR_HEADER__

#include "execute.h"
#include "execute_values.h"
#include "pointer.h"

#define setargs_va_control _n(setargs_va_control)
#define setargs_nil_control _n(setargs_nil_control)
#define setargs_list_control _n(setargs_list_control)
#define pushargs_control _n(pushargs_control)
#define popargs_control_ _n(popargs_control_)
#define getargs_control _n(getargs_control)
#define getargs_tail_control _n(getargs_tail_control)
#define getargs_list_control_unsafe _n(getargs_list_control_unsafe)
#define getargs_list_control_heap _n(getargs_list_control_heap)
#define pushargs_allvalues _n(pushargs_allvalues)
#define goto_control_ _n(goto_control_)
#define go_control_ _n(go_control_)
#define return_from_control_ _n(return_from_control_)
#define catch_control _n(catch_control)
#define throw_control_ _n(throw_control_)
#define pushhandler_common_ _n(pushhandler_common_)
#define reverse_handler_control _n(reverse_handler_control)
#define pushbind_restart_control _n(pushbind_restart_control)
#define reverse_restart_control _n(reverse_restart_control)
#define find_condition_control_ _n(find_condition_control_)
#define invoke_handler_control_ _n(invoke_handler_control_)
#define invoke_restart_control_ _n(invoke_restart_control_)
#define invoke_restart_interactively_control_ _n(invoke_restart_interactively_control_)
#define find_restart_control_ _n(find_restart_control_)
#define find_restart_control_error_ _n(find_restart_control_error_)
#define compute_restarts_control_ _n(compute_restarts_control_)
#define restart_control _n(restart_control)
#define restart0_control _n(restart0_control)
#define restart1_control _n(restart1_control)
#define restart1r_control _n(restart1r_control)
#define restart2_control _n(restart2_control)
#define set_taginfo_control _n(set_taginfo_control)
#define set_blockinfo_control _n(set_blockinfo_control)
#define set_protect_control _n(set_protect_control)
#define catch_clang _n(catch_clang)

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

