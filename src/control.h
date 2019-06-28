#ifndef __CONTROL_HEADER__
#define __CONTROL_HEADER__

#include <stdarg.h>
#include "execute.h"
#include "typedef.h"

struct runcode_value {
	enum ExecuteControl signal;
	void *data;
};

/* symstack */
void pushlexical_control(Execute ptr, addr pos, addr value);
void pushspecial_control(Execute ptr, addr pos, addr value);
void pushcallname_control(Execute ptr, addr pos, addr value);
void pushfunction_control(Execute ptr, addr pos, addr value);
void pushsetf_control(Execute ptr, addr pos, addr value);

/* stack */
void push_return_control(Execute ptr, addr *ret);
void push_push_control(Execute ptr, addr *ret);
void push_close_control(Execute ptr, addr *ret);
void push_local_control(Execute ptr, addr *ret);
void push_finalize_control(Execute ptr, addr *ret);
void setfinalize_control(Execute ptr, addr control, addr code);
void push_restart_control(Execute ptr, addr *ret);

/* free-control */
void runcode_push(Execute ptr, struct runcode_value *value);
void runcode_rollback(Execute ptr, const struct runcode_value *value);
int runcode_free_control(Execute ptr, addr control);
int free_check_control(Execute ptr, addr control, int check);
int free_control(Execute ptr, addr control);

/* getcontrol */
void getdata_control(Execute ptr, addr *ret);
void setdata_control(Execute ptr, addr value);
void array_data_control(Execute ptr, size_t size);
void getdata_array_control(Execute ptr, size_t index, addr *ret);
void setdata_array_control(Execute ptr, size_t index, addr value);

void pushhandler_control(Execute ptr, addr pos, addr call, int escape);
void reverse_handler_control(Execute ptr);
void pushobject_restart_control(Execute ptr, addr object);
void pushbind_restart_control(Execute ptr, addr list, int escape);
void reverse_restart_control(Execute ptr);

int invoke_handler_control(Execute ptr, addr pos);
int invoke_restart_control(Execute ptr, addr restart, addr args);
int invoke_restart_interactively_control(Execute ptr, addr restart);
int find_restart_control(Execute ptr, addr name, addr condition, addr *ret);
void compute_restarts_control(Execute ptr, addr condition, addr *ret);

/* argument */
void setargs_control(Execute ptr, addr value);
void setargs_va_control(Execute ptr, ...);
void setargs_nil_control(Execute ptr);
void setargs_list_control_unsafe(Execute ptr, addr list);
void setargs_list_control(Execute ptr, addr list);
void pushargs_control(Execute ptr, addr value);
void pushargs_list_control(Execute ptr, addr list);
void getargs_control(Execute ptr, size_t index, addr *ret);
void getargs_tail_control(Execute ptr, addr *ret);
void getargs_list_control_unsafe(Execute ptr, size_t index, addr *ret);
void getargs_list_control_heap(Execute ptr, size_t index, addr *ret);

/* values */
void setresult_control(Execute ptr, addr value);
void setbool_control(Execute ptr, int value);
void setvalues_control(Execute ptr, ...);
void setvalues_nil_control(Execute ptr);
void setvalues_list_control(Execute ptr, addr list);
void getresult_control(Execute ptr, addr *ret);
void getvalues_control(Execute ptr, size_t index, addr *ret);
void getvalues_nil_control(Execute ptr, size_t index, addr *ret);
void getvalues_list_control_local(Execute ptr, addr *ret);
void getvalues_list_control_heap(Execute ptr, addr *ret);
void getvalues_unsafe_control(Execute ptr, size_t index, addr *ret);
void setvalues_unsafe_control(Execute ptr, size_t index, addr value);
size_t lengthvalues_control(Execute ptr);
void pushargs_allvalues(Execute ptr);

/* call_compiled_function */
void init_control(void);

/* execute */
int signal_control(Execute ptr);
#define SignalControl() signal_control(Execute_Thread);
int runcode_control(Execute ptr, addr code);
int execute_control(Execute ptr, addr call);
int apply_control(Execute ptr, addr call, addr args);
int stdarg_control(Execute ptr, addr call, va_list args);
int funcall_control(Execute ptr, addr call, ...);
int call_control(Execute ptr, addr args);

void goto_control(Execute ptr, size_t point);
void go_control(Execute ptr, addr tag);
void return_from_control(Execute ptr, addr name);
void catch_control(Execute ptr, addr name);
void throw_control(Execute ptr, addr name);
void gettagbody_execute(Execute ptr, addr *ret, addr pos);
void getblock_execute(Execute ptr, addr *ret, addr pos);
void hide_lexical_control(Execute ptr);

/* C language */
int callablep(addr pos);
int callclang_values_apply_heap(Execute ptr, addr *ret, addr call, addr cons);
int callclang_values_apply_local(Execute ptr, addr *ret, addr call, addr cons);
int callclang_values_stdarg_heap(Execute ptr, addr *ret, addr call, va_list args);
int callclang_values_stdarg_local(Execute ptr, addr *ret, addr call, va_list args);
int callclang_values_funcall_heap(Execute ptr, addr *ret, addr call, ...);
int callclang_values_funcall_local(Execute ptr, addr *ret, addr call, ...);
int callclang_values_char_heap(Execute ptr, addr *ret,
		const char *package, const char *name, ...);
int callclang_values_char_local(Execute ptr, addr *ret,
		const char *package, const char *name, ...);
int callclang_apply(Execute ptr, addr *ret, addr call, addr cons);
int callclang_stdarg(Execute ptr, addr *ret, addr call, va_list args);
int callclang_funcall(Execute ptr, addr *ret, addr call, ...);
int callclang_char(Execute ptr, addr *ret,
		const char *package, const char *name, ...);

/* debug */
void info_control(addr control);

#endif

