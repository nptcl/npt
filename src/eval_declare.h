#ifndef __EVAL_DECLARE_HEADER__
#define __EVAL_DECLARE_HEADER__

#include "execute.h"
#include "typedef.h"

typedef signed char OptimizeType;

addr refevaldeclare(addr pos, size_t index);
void getevaldeclare(addr pos, size_t index, addr *ret);
void setevaldeclare(addr pos, size_t index, addr value);
OptimizeType refevaldeclareoptimize(addr pos, size_t index);
void getevaldeclareoptimize(addr pos, size_t index, OptimizeType *ret);
void setevaldeclareoptimize(addr pos, size_t index, OptimizeType value);

void eval_declare_alloc(LocalRoot local, addr *ret);
void eval_declare_local(LocalRoot local, addr *ret);
void eval_declare_heap(addr *ret);
int empty_declare(addr pos);
int empty_nil_declare(addr pos);
void apply_array_declare(OptimizeType *array, addr pos);

OptimizeType get_optimize_compilation_declare(addr pos);
OptimizeType get_optimize_debug_declare(addr pos);
OptimizeType get_optimize_safety_declare(addr pos);
OptimizeType get_optimize_space_declare(addr pos);
OptimizeType get_optimize_speed_declare(addr pos);

void getall_declaration_declare(addr pos, addr *ret);
void getall_inline_declare(addr pos, addr *ret);
void getall_special_declare(addr pos, addr *ret);
void getall_type_value_declare(addr pos, addr *ret);
void getall_type_function_declare(addr pos, addr *ret);
const OptimizeType *getall_optimize_declare(addr pos);
void getall_dynamic_value_declare(addr pos, addr *ret);
void getall_dynamic_function_declare(addr pos, addr *ret);
void getall_ignore_value_declare(addr pos, addr *ret);
void getall_ignore_function_declare(addr pos, addr *ret);

void getroot_declare(addr *ret);
void setroot_declare(addr pos);
void build_eval_declare(void);
void push_declaration_declaim(addr symbol);
void copy_optimize_declare(OptimizeType *array);
void apply_compilation_speed_declaim(OptimizeType value);
void apply_debug_declaim(OptimizeType value);
void apply_safety_declaim(OptimizeType value);
void apply_space_declaim(OptimizeType value);
void apply_speed_declaim(OptimizeType value);

int parse_declaim_heap(Execute ptr, addr env, addr decl, addr *ret);
int parse_declare_heap(Execute ptr, addr env, addr decl, addr *ret);
void declare_body_form(addr cons, addr *retdecl, addr *retbody);
int declare_body(Execute ptr, addr env, addr cons, addr *retdecl, addr *retbody);
int declare_body_documentation(Execute ptr, addr env,
		addr cons, addr *rdoc, addr *rdecl, addr *rbody);

void copy_eval_declare_alloc(LocalRoot local, addr *ret, addr pos);
void copy_eval_declare_local(LocalRoot local, addr *ret, addr pos);
void copy_eval_declare_heap(addr *ret, addr pos);

/* debug */
void set_optimize_compilation_declare(addr pos, OptimizeType value);
void set_optimize_debug_declare(addr pos, OptimizeType value);
void set_optimize_safety_declare(addr pos, OptimizeType value);
void set_optimize_space_declare(addr pos, OptimizeType value);
void set_optimize_speed_declare(addr pos, OptimizeType value);

#endif

