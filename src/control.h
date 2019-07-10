#ifndef __CONTROL_HEADER__
#define __CONTROL_HEADER__

#include <stdarg.h>
#include "execute.h"
#include "typedef.h"

struct runcode_value {
	enum ExecuteControl signal;
	struct taginfo_struct *taginfo;
};

/* symstack */
_g void pushlexical_control(Execute ptr, addr pos, addr value);
_g void pushspecial_control(Execute ptr, addr pos, addr value);
_g void pushcallname_control(Execute ptr, addr pos, addr value);
_g void pushfunction_control(Execute ptr, addr pos, addr value);
_g void pushsetf_control(Execute ptr, addr pos, addr value);

/* stack */
_g void push_return_control(Execute ptr, addr *ret);
_g void push_push_control(Execute ptr, addr *ret);
_g void push_close_control(Execute ptr, addr *ret);
_g void push_local_control(Execute ptr, addr *ret);
_g void push_finalize_control(Execute ptr, addr *ret);
_g void setfinalize_control(Execute ptr, addr control, addr code);
_g void push_restart_control(Execute ptr, addr *ret);

/* free-control */
_g void runcode_push(Execute ptr, struct runcode_value *value);
_g void runcode_rollback(Execute ptr, const struct runcode_value *value);
_g int runcode_free_control(Execute ptr, addr control);
_g int free_check_control(Execute ptr, addr control, int check);
_g int free_control(Execute ptr, addr control);

/* getcontrol */
_g void getdata_control(Execute ptr, addr *ret);
_g void setdata_control(Execute ptr, addr value);
_g void array_data_control(Execute ptr, size_t size);
_g void getdata_array_control(Execute ptr, size_t index, addr *ret);
_g void setdata_array_control(Execute ptr, size_t index, addr value);

_g void pushhandler_control(Execute ptr, addr pos, addr call, int escape);
_g void reverse_handler_control(Execute ptr);
_g void pushobject_restart_control(Execute ptr, addr object);
_g void pushbind_restart_control(Execute ptr, addr list, int escape);
_g void reverse_restart_control(Execute ptr);

_g int invoke_handler_control(Execute ptr, addr pos, int *ret);
_g int invoke_restart_control(Execute ptr, addr restart, addr args);
_g int invoke_restart_interactively_control(Execute ptr, addr restart);
_g int find_restart_control(Execute ptr, addr name, addr condition, addr *ret);
_g void compute_restarts_control(Execute ptr, addr condition, addr *ret);

/* argument */
_g void setargs_control(Execute ptr, addr value);
_g void setargs_va_control(Execute ptr, ...);
_g void setargs_nil_control(Execute ptr);
_g void setargs_list_control_unsafe(Execute ptr, addr list);
_g void setargs_list_control(Execute ptr, addr list);
_g void pushargs_control(Execute ptr, addr value);
_g void pushargs_list_control(Execute ptr, addr list);
_g void getargs_control(Execute ptr, size_t index, addr *ret);
_g void getargs_tail_control(Execute ptr, addr *ret);
_g void getargs_list_control_unsafe(Execute ptr, size_t index, addr *ret);
_g void getargs_list_control_heap(Execute ptr, size_t index, addr *ret);

/* values */
_g void setresult_control(Execute ptr, addr value);
_g void setbool_control(Execute ptr, int value);
_g void setvalues_control(Execute ptr, ...);
_g void setvalues_nil_control(Execute ptr);
_g void setvalues_list_control(Execute ptr, addr list);
_g void getresult_control(Execute ptr, addr *ret);
_g void getvalues_control(Execute ptr, size_t index, addr *ret);
_g void getvalues_nil_control(Execute ptr, size_t index, addr *ret);
_g void getvalues_list_control_local(Execute ptr, addr *ret);
_g void getvalues_list_control_heap(Execute ptr, addr *ret);
_g void getvalues_unsafe_control(Execute ptr, size_t index, addr *ret);
_g void setvalues_unsafe_control(Execute ptr, size_t index, addr value);
_g size_t lengthvalues_control(Execute ptr);
_g void pushargs_allvalues(Execute ptr);

/* call_compiled_function */
_g void init_control(void);

/* condition/restart */
void push_restart_initialize_control(Execute ptr, addr *ret);

/* execute */
_g int signal_control(Execute ptr);
#define SignalControl() signal_control(Execute_Thread);
_g int runcode_control(Execute ptr, addr code);
_g int execute_control(Execute ptr, addr call);
_g int apply_control(Execute ptr, addr call, addr args);
_g int stdarg_control(Execute ptr, addr call, va_list args);
_g int funcall_control(Execute ptr, addr call, ...);
_g int call_control(Execute ptr, addr args);

_g void goto_control(Execute ptr, size_t point);
_g void go_control(Execute ptr, addr tag);
_g void return_from_control(Execute ptr, addr name);
_g void catch_control(Execute ptr, addr name);
_g void throw_control(Execute ptr, addr name);
_g void gettagbody_execute(Execute ptr, addr *ret, addr pos);
_g void getblock_execute(Execute ptr, addr *ret, addr pos);
_g void hide_lexical_control(Execute ptr);

/* C language */
_g int callablep(addr pos);
_g int callclang_values_apply_heap(Execute ptr, addr *ret, addr call, addr cons);
_g int callclang_values_apply_local(Execute ptr, addr *ret, addr call, addr cons);
_g int callclang_values_stdarg_heap(Execute ptr, addr *ret, addr call, va_list args);
_g int callclang_values_stdarg_local(Execute ptr, addr *ret, addr call, va_list args);
_g int callclang_values_funcall_heap(Execute ptr, addr *ret, addr call, ...);
_g int callclang_values_funcall_local(Execute ptr, addr *ret, addr call, ...);
_g int callclang_values_char_heap(Execute ptr, addr *ret,
		const char *package, const char *name, ...);
_g int callclang_values_char_local(Execute ptr, addr *ret,
		const char *package, const char *name, ...);
_g int callclang_apply(Execute ptr, addr *ret, addr call, addr cons);
_g int callclang_stdarg(Execute ptr, addr *ret, addr call, va_list args);
_g int callclang_funcall(Execute ptr, addr *ret, addr call, ...);
_g int callclang_char(Execute ptr, addr *ret,
		const char *package, const char *name, ...);

/* debug */
_g void info_control(addr control);

#endif

