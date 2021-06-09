#ifndef __DECLARE_HEADER__
#define __DECLARE_HEADER__

#include "execute.h"
#include "typedef.h"

enum EVAL_DECLARE {
	EVAL_DECLARE_TYPE_VALUE,
	EVAL_DECLARE_TYPE_FUNCTION,
	EVAL_DECLARE_SPECIAL,
	EVAL_DECLARE_INLINE,
	EVAL_DECLARE_IGNORE_VALUE,
	EVAL_DECLARE_IGNORE_FUNCTION,
	EVAL_DECLARE_DYNAMIC_VALUE,
	EVAL_DECLARE_DYNAMIC_FUNCTION,
	EVAL_DECLARE_DECLARATION,
	EVAL_DECLARE_SIZE
};

#define RefEvalDeclare_Low      RefEval
#define GetEvalDeclare_Low      GetEval
#define SetEvalDeclare_Low      SetEval

#define PtrEvalDeclare_Low(p)	((OptimizeType *)PtrEvalBody(p, EVAL_DECLARE_SIZE))
#define RefEvalDeclareOptimize_Low(p,i)		(PtrEvalDeclare_Low(p)[i])
#define GetEvalDeclareOptimize_Low(p,i,v)	(*(v) = PtrEvalDeclare_Low(p)[i])
#define SetEvalDeclareOptimize_Low(p,i,v)	(PtrEvalDeclare_Low(p)[i] = (v))

#ifdef LISP_DEBUG
#define RefEvalDeclare          refevaldeclare
#define GetEvalDeclare          getevaldeclare
#define SetEvalDeclare          setevaldeclare
#define RefEvalDeclareOptimize	refevaldeclareoptimize
#define GetEvalDeclareOptimize	getevaldeclareoptimize
#define SetEvalDeclareOptimize	setevaldeclareoptimize
#else
#define RefEvalDeclare          RefEvalDeclare_Low
#define GetEvalDeclare          GetEvalDeclare_Low
#define SetEvalDeclare          SetEvalDeclare_Low
#define RefEvalDeclareOptimize	RefEvalDeclareOptimize_Low
#define GetEvalDeclareOptimize	GetEvalDeclareOptimize_Low
#define SetEvalDeclareOptimize	SetEvalDeclareOptimize_Low
#endif

#define DEFAULT_OPTIMIZE 1

#define refevaldeclare _n(refevaldeclare)
#define getevaldeclare _n(getevaldeclare)
#define setevaldeclare _n(setevaldeclare)
#define refevaldeclareoptimize _n(refevaldeclareoptimize)
#define getevaldeclareoptimize _n(getevaldeclareoptimize)
#define setevaldeclareoptimize _n(setevaldeclareoptimize)
#define eval_declare_alloc _n(eval_declare_alloc)
#define eval_declare_local _n(eval_declare_local)
#define eval_declare_heap _n(eval_declare_heap)
#define empty_declare _n(empty_declare)
#define empty_nil_declare _n(empty_nil_declare)
#define apply_array_declare _n(apply_array_declare)
#define get_optimize_declare _n(get_optimize_declare)
#define get_optimize_compilation_declare _n(get_optimize_compilation_declare)
#define get_optimize_debug_declare _n(get_optimize_debug_declare)
#define get_optimize_safety_declare _n(get_optimize_safety_declare)
#define get_optimize_space_declare _n(get_optimize_space_declare)
#define get_optimize_speed_declare _n(get_optimize_speed_declare)
#define getall_declaration_declare _n(getall_declaration_declare)
#define getall_inline_declare _n(getall_inline_declare)
#define getall_special_declare _n(getall_special_declare)
#define getall_type_value_declare _n(getall_type_value_declare)
#define getall_type_function_declare _n(getall_type_function_declare)
#define getall_optimize_declare _n(getall_optimize_declare)
#define getall_dynamic_value_declare _n(getall_dynamic_value_declare)
#define getall_dynamic_function_declare _n(getall_dynamic_function_declare)
#define getall_ignore_value_declare _n(getall_ignore_value_declare)
#define getall_ignore_function_declare _n(getall_ignore_function_declare)
#define getroot_declare _n(getroot_declare)
#define setroot_declare _n(setroot_declare)
#define build_declare _n(build_declare)
#define push_declaration_declaim _n(push_declaration_declaim)
#define copy_optimize_declare _n(copy_optimize_declare)
#define apply_compilation_speed_declaim _n(apply_compilation_speed_declaim)
#define apply_debug_declaim _n(apply_debug_declaim)
#define apply_safety_declaim _n(apply_safety_declaim)
#define apply_space_declaim _n(apply_space_declaim)
#define apply_speed_declaim _n(apply_speed_declaim)
#define parse_declaim_heap_ _n(parse_declaim_heap_)
#define parse_declare_heap_ _n(parse_declare_heap_)
#define parse_optimize_heap_ _n(parse_optimize_heap_)
#define declare_body_form_ _n(declare_body_form_)
#define declare_body_ _n(declare_body_)
#define declare_body_documentation_ _n(declare_body_documentation_)
#define split_decl_body_doc_ _n(split_decl_body_doc_)
#define copy_eval_declare_alloc _n(copy_eval_declare_alloc)
#define copy_eval_declare_local _n(copy_eval_declare_local)
#define copy_eval_declare_heap _n(copy_eval_declare_heap)
#define set_optimize_compilation_declare _n(set_optimize_compilation_declare)
#define set_optimize_debug_declare _n(set_optimize_debug_declare)
#define set_optimize_safety_declare _n(set_optimize_safety_declare)
#define set_optimize_space_declare _n(set_optimize_space_declare)
#define set_optimize_speed_declare _n(set_optimize_speed_declare)
#define proclaim_common _n(proclaim_common)

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

OptimizeType get_optimize_declare(addr pos, enum EVAL_OPTIMIZE index);
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
void build_declare(void);
void push_declaration_declaim(addr symbol);
void copy_optimize_declare(OptimizeType *array);
void apply_compilation_speed_declaim(OptimizeType value);
void apply_debug_declaim(OptimizeType value);
void apply_safety_declaim(OptimizeType value);
void apply_space_declaim(OptimizeType value);
void apply_speed_declaim(OptimizeType value);

int parse_declaim_heap_(Execute ptr, addr env, addr decl, addr *ret);
int parse_declare_heap_(Execute ptr, addr env, addr decl, addr *ret);
int parse_optimize_heap_(addr decl, addr *value, int *ret);
int declare_body_form_(addr list, addr *retdecl, addr *retbody);
int declare_body_(Execute ptr, addr env, addr cons, addr *retdecl, addr *retbody);
int declare_body_documentation_(Execute ptr, addr env,
		addr cons, addr *rdoc, addr *rdecl, addr *rbody);
int split_decl_body_doc_(addr list, addr *rdoc, addr *rdecl, addr *rbody);

void copy_eval_declare_alloc(LocalRoot local, addr *ret, addr pos);
void copy_eval_declare_local(LocalRoot local, addr *ret, addr pos);
void copy_eval_declare_heap(addr *ret, addr pos);

/* debug */
void set_optimize_compilation_declare(addr pos, OptimizeType value);
void set_optimize_debug_declare(addr pos, OptimizeType value);
void set_optimize_safety_declare(addr pos, OptimizeType value);
void set_optimize_space_declare(addr pos, OptimizeType value);
void set_optimize_speed_declare(addr pos, OptimizeType value);

/* common */
int proclaim_common(Execute ptr, addr var);

#endif

