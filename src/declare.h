#ifndef __DECLARE_HEADER__
#define __DECLARE_HEADER__

#include "execute.h"
#include "typedef.h"

typedef signed char OptimizeType;

_g addr refevaldeclare(addr pos, size_t index);
_g void getevaldeclare(addr pos, size_t index, addr *ret);
_g void setevaldeclare(addr pos, size_t index, addr value);
_g OptimizeType refevaldeclareoptimize(addr pos, size_t index);
_g void getevaldeclareoptimize(addr pos, size_t index, OptimizeType *ret);
_g void setevaldeclareoptimize(addr pos, size_t index, OptimizeType value);

_g void eval_declare_alloc(LocalRoot local, addr *ret);
_g void eval_declare_local(LocalRoot local, addr *ret);
_g void eval_declare_heap(addr *ret);
_g int empty_declare(addr pos);
_g int empty_nil_declare(addr pos);
_g void apply_array_declare(OptimizeType *array, addr pos);

_g OptimizeType get_optimize_declare(addr pos, enum EVAL_OPTIMIZE index);
_g OptimizeType get_optimize_compilation_declare(addr pos);
_g OptimizeType get_optimize_debug_declare(addr pos);
_g OptimizeType get_optimize_safety_declare(addr pos);
_g OptimizeType get_optimize_space_declare(addr pos);
_g OptimizeType get_optimize_speed_declare(addr pos);

_g void getall_declaration_declare(addr pos, addr *ret);
_g void getall_inline_declare(addr pos, addr *ret);
_g void getall_special_declare(addr pos, addr *ret);
_g void getall_type_value_declare(addr pos, addr *ret);
_g void getall_type_function_declare(addr pos, addr *ret);
_g const OptimizeType *getall_optimize_declare(addr pos);
_g void getall_dynamic_value_declare(addr pos, addr *ret);
_g void getall_dynamic_function_declare(addr pos, addr *ret);
_g void getall_ignore_value_declare(addr pos, addr *ret);
_g void getall_ignore_function_declare(addr pos, addr *ret);

_g void getroot_declare(addr *ret);
_g void setroot_declare(addr pos);
_g void build_declare(void);
_g void push_declaration_declaim(addr symbol);
_g void copy_optimize_declare(OptimizeType *array);
_g void apply_compilation_speed_declaim(OptimizeType value);
_g void apply_debug_declaim(OptimizeType value);
_g void apply_safety_declaim(OptimizeType value);
_g void apply_space_declaim(OptimizeType value);
_g void apply_speed_declaim(OptimizeType value);

_g int parse_declaim_heap_(Execute ptr, addr env, addr decl, addr *ret);
_g int parse_declare_heap_(Execute ptr, addr env, addr decl, addr *ret);
_g int parse_optimize_heap_(addr decl, addr *value, int *ret);
_g int declare_body_form_(addr list, addr *retdecl, addr *retbody);
_g int declare_body_(Execute ptr, addr env, addr cons, addr *retdecl, addr *retbody);
_g int declare_body_documentation_(Execute ptr, addr env,
		addr cons, addr *rdoc, addr *rdecl, addr *rbody);
_g int split_decl_body_doc_(addr list, addr *rdoc, addr *rdecl, addr *rbody);

_g void copy_eval_declare_alloc(LocalRoot local, addr *ret, addr pos);
_g void copy_eval_declare_local(LocalRoot local, addr *ret, addr pos);
_g void copy_eval_declare_heap(addr *ret, addr pos);

/* debug */
_g void set_optimize_compilation_declare(addr pos, OptimizeType value);
_g void set_optimize_debug_declare(addr pos, OptimizeType value);
_g void set_optimize_safety_declare(addr pos, OptimizeType value);
_g void set_optimize_space_declare(addr pos, OptimizeType value);
_g void set_optimize_speed_declare(addr pos, OptimizeType value);

/* common */
_g int proclaim_common(Execute ptr, addr var);

#endif

