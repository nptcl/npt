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

void set_type_declare_alloc(LocalRoot local, addr pos, addr symbol, addr type);
int get_type_declare(addr pos, addr symbol, addr *ret);
void set_ftype_declare_alloc(LocalRoot local, addr pos, addr symbol, addr type);
int get_ftype_declare(addr pos, addr symbol, addr *ret);
void push_inline_declare_alloc(LocalRoot local, addr pos, addr symbol);
int check_inline_declare(addr pos, addr symbol);
void push_notinline_declare_alloc(LocalRoot local, addr pos, addr symbol);
int check_notinline_declare(addr pos, addr symbol);
void push_ignore_value_declare_alloc(LocalRoot local, addr pos, addr symbol);
int check_ignore_value_declare(addr pos, addr symbol);
void push_ignorable_value_declare_alloc(LocalRoot local, addr pos, addr symbol);
int check_ignorable_value_declare(addr pos, addr symbol);
void push_ignore_function_declare_alloc(LocalRoot local, addr pos, addr symbol);
int check_ignore_function_declare(addr pos, addr symbol);
void push_ignorable_function_declare_alloc(LocalRoot local, addr pos, addr symbol);
int check_ignorable_function_declare(addr pos, addr symbol);
void push_special_declare_alloc(LocalRoot local, addr pos, addr symbol);
int check_special_declare(addr pos, addr symbol);
void push_dynamic_value_declare_alloc(LocalRoot local, addr pos, addr symbol);
int check_dynamic_value_declare(addr pos, addr symbol);
void push_dynamic_function_declare_alloc(LocalRoot local, addr pos, addr symbol);
int check_dynamic_function_declare(addr pos, addr symbol);
void push_declaration_declare_alloc(LocalRoot local, addr pos, addr symbol);
int check_declaration_declare(addr pos, addr symbol);
void set_optimize_compilation_declare(addr pos, OptimizeType value);
OptimizeType get_optimize_compilation_declare(addr pos);
void set_optimize_debug_declare(addr pos, OptimizeType value);
OptimizeType get_optimize_debug_declare(addr pos);
void set_optimize_safety_declare(addr pos, OptimizeType value);
OptimizeType get_optimize_safety_declare(addr pos);
void set_optimize_space_declare(addr pos, OptimizeType value);
OptimizeType get_optimize_space_declare(addr pos);
void set_optimize_speed_declare(addr pos, OptimizeType value);
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

void parse_declaim_alloc(LocalRoot local, addr decl, addr *ret);
void parse_declare_alloc(LocalRoot local, addr decl, addr *ret);
void declare_body_form(addr cons, addr *retdecl, addr *retbody);
void declare_body(addr cons, addr *retdecl, addr *retbody);
void declare_body_documentation(addr cons, addr *rdoc, addr *rdecl, addr *rbody);

void copy_eval_declare_alloc(LocalRoot local, addr *ret, addr pos);
void copy_eval_declare_local(LocalRoot local, addr *ret, addr pos);
void copy_eval_declare_heap(addr *ret, addr pos);

#endif

