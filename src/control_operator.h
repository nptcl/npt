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
void setargs_va_control(Execute ptr, ...);
void setargs_nil_control(Execute ptr);
void setargs_list_control(Execute ptr, addr list);
void pushargs_control(Execute ptr, addr value);
int popargs_control_(Execute ptr, addr *ret);
void getargs_control(Execute ptr, size_t index, addr *ret);
void getargs_tail_control(Execute ptr, addr *ret);
#define GetArgsControl(ptr, ret) GetControl((ptr)->control, Control_Cons, (ret))
void getargs_list_control_unsafe(Execute ptr, size_t index, addr *ret);
void getargs_list_control_heap(Execute ptr, size_t index, addr *ret);
void pushargs_allvalues(Execute ptr);

/* flow control */
int goto_control_(Execute ptr, size_t point);
int go_control_(Execute ptr, addr tag);
int return_from_control_(Execute ptr, addr name);
void catch_control(Execute ptr, addr name);
int throw_control_(Execute ptr, addr name);

int pushhandler_common_(Execute ptr, addr name, addr call, int escape);
void reverse_handler_control(Execute ptr);
void pushbind_restart_control(Execute ptr, addr list, int escape);
void reverse_restart_control(Execute ptr);

int find_condition_control_(Execute ptr, addr instance, int *ret);
int invoke_handler_control_(Execute ptr, addr pos);
int invoke_restart_control_(Execute ptr, addr restart, addr args);
int invoke_restart_interactively_control_(Execute ptr, addr restart);

int find_restart_control_(Execute ptr,
		addr name, addr condition, addr *value, int *ret);
int find_restart_control_error_(Execute ptr, addr name, addr condition, addr *ret);
int compute_restarts_control_(Execute ptr, addr condition, addr *ret);

int restart_control(Execute ptr, int (*call)(Execute, void *), void *voidp);
int restart0_control(Execute ptr, addr restart,
		int (*call)(Execute, void *), void *voidp);
int restart1_control(Execute ptr, addr restart,
		int (*call)(Execute, addr), addr v1);
int restart1r_control(Execute ptr, addr restart,
		int (*call)(Execute, addr, addr *), addr v1, addr *ret);
int restart2_control(Execute ptr, addr restart,
		int (*call)(Execute, addr, addr), addr v1, addr v2);

void set_taginfo_control(Execute ptr, addr list);
void set_blockinfo_control(Execute ptr, addr pos);
void set_protect_control(Execute ptr, addr pos);
int catch_clang(Execute ptr, pointer call, addr tag, addr value);

#endif

