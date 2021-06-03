#include "code_make.h"
#include "compile_eval.h"
#include "compile_write.h"
#include "compile_file.h"
#include "cons.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "eval_stack.h"
#include "eval_value.h"
#include "hold.h"
#include "load_code.h"
#include "load_depend.h"
#include "load_gensym.h"
#include "load_time_value.h"
#include "optimize_parse.h"
#include "parse_function.h"
#include "parse_object.h"
#include "reader.h"
#include "scope.h"
#include "scope_declare.h"
#include "scope_object.h"
#include "stream_memory.h"
#include "symbol.h"
#include "type_table.h"
#include "typedef.h"

static int compile_eval_scope_(Execute ptr, addr pos, addr *rtype);

/*
 *  eval-when check
 */
static int compile_eval_execute_p_(Execute ptr, int *ret)
{
	return load_toplevel_p_eval_(ptr, ret);
}

static int compile_eval_compile_p_(Execute ptr, int *ret)
{
	Return(compile_toplevel_p_eval_(ptr, ret));
	if (*ret)
		return 0;

	return compile_time_too_eval_(ptr, ret);
}


/*
 *  execute
 */
static int compile_eval_execute_call_(Execute ptr, addr pos, addr *rtype)
{
	int check;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	localhold_push(hold, pos);

	/* scope */
	localhold_set(hold, 0, pos);
	Return(eval_scope_(ptr, &pos, pos));

	/* type */
	if (rtype) {
		GetEvalScopeThe(pos, rtype);
		localhold_set(hold, 1, *rtype);
	}

	/* code */
	localhold_set(hold, 0, pos);
	code_make(ptr->local, &pos, pos);

	/* load-value */
	localhold_set(hold, 0, pos);
	Return(load_value_code_(ptr, pos));

	/* execute */
	Return(compile_eval_execute_p_(ptr, &check));
	if (check) {
		localhold_set(hold, 0, pos);
		Return(eval_compile_file(ptr, pos));
	}

	/* compile-toplevel */
	Return(compile_eval_compile_p_(ptr, &check));
	if (check) {
		localhold_set(hold, 0, pos);
		Return(runcode_control_(ptr, pos));
	}
	localhold_end(hold);

	return 0;
}

static int compile_eval_execute_(Execute ptr, addr pos, addr *rtype)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_eval_execute_call_(ptr, pos, rtype);
	return pop_control_(ptr, control);
}


/*
 *  progn
 */
static int compile_eval_progn(Execute ptr, addr pos, addr *rtype)
{
	addr list;

	GetEvalParse(pos, 0, &list);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(compile_eval_scope_(ptr, pos, rtype));
	}

	return 0;
}


/*
 *  locally
 */
static int compile_eval_implicit(Execute ptr, addr decl, addr list, addr *rtype)
{
	addr stack, free, pos;

	/* new stack */
	Return(newstack_nil_(ptr, &stack));
	Return(apply_declare_(ptr, stack, decl, &free));

	/* locally */
	eval_parse_heap(&pos, EVAL_PARSE_LOCALLY, 3);
	SetEvalParse(pos, 0, decl);
	SetEvalParse(pos, 1, Nil);
	SetEvalParse(pos, 2, free);
	Return(compile_eval_execute_(ptr, pos, rtype));

	/* body */
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(compile_eval_scope_(ptr, pos, rtype));
	}

	/* free stack */
	return freestack_eval_(ptr, stack);
}

static int compile_eval_locally(Execute ptr, addr pos, addr *rtype)
{
	addr decl, list;

	GetEvalParse(pos, 0, &decl);
	GetEvalParse(pos, 1, &list);

	return compile_eval_implicit(ptr, decl, list, rtype);
}


/*
 *  eval-when
 */
static int compile_eval_eval_when(Execute ptr, addr pos, addr *rtype)
{
	addr list;
	addr compile, load, exec, mode;
	addr compile1, load1, exec1, mode1;

	GetEvalParse(pos, 0, &list);
	GetEvalParse(pos, 1, &compile);
	GetEvalParse(pos, 2, &load);
	GetEvalParse(pos, 3, &exec);
	GetEvalParse(pos, 5, &mode);

	/* save */
	Return(get_compile_time_eval_(ptr, &mode1));
	Return(get_compile_toplevel_eval_(ptr, &compile1));
	Return(get_load_toplevel_eval_(ptr, &load1));
	Return(get_execute_eval_(ptr, &exec1));

	/* set */
	set_compile_time_eval(ptr, mode);
	set_compile_toplevel_eval(ptr, compile);
	set_load_toplevel_eval(ptr, load);
	set_execute_eval(ptr, exec);

	/* body */
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(compile_eval_scope_(ptr, pos, rtype));
	}

	/* rollback */
	set_compile_time_eval(ptr, mode1);
	set_compile_toplevel_eval(ptr, compile1);
	set_load_toplevel_eval(ptr, load1);
	set_execute_eval(ptr, exec1);

	return 0;
}


/*
 *  read
 */
static int compile_eval_progn_(Execute ptr, addr pos, addr *rtype)
{
	EvalParse type;

	GetEvalParseType(pos, &type);
	switch (type) {
		case EVAL_PARSE_PROGN:
			return compile_eval_progn(ptr, pos, rtype);

		case EVAL_PARSE_LOCALLY:
			return compile_eval_locally(ptr, pos, rtype);

		case EVAL_PARSE_EVAL_WHEN:
			return compile_eval_eval_when(ptr, pos, rtype);

		default:
			return compile_eval_execute_(ptr, pos, rtype);
	}
}

static int compile_eval_scope_(Execute ptr, addr pos, addr *rtype)
{
	addr control;

	push_control(ptr, &control);
	gchold_push_special(ptr, pos);
	(void)compile_eval_progn_(ptr, pos, rtype);
	return pop_control_(ptr, control);
}

static int compile_eval_parse_(Execute ptr, addr pos, addr *rtype)
{
	LocalHold hold;

	/* parse */
	hold = LocalHold_array(ptr, 3);
	localhold_set(hold, 0, pos);
	Return(parse_execute_toplevel_(ptr, &pos, pos));

	/* optimize parse */
	localhold_set(hold, 1, pos);
	Return(optimize_parse_(ptr->local, pos, &pos, NULL));

	/* scope */
	localhold_set(hold, 2, pos);
	Return(compile_eval_progn_(ptr, pos, rtype));

	/* free */
	localhold_end(hold);

	return 0;
}

static int compile_eval_push_output_(Execute ptr, addr *ret)
{
	addr symbol, stream;

	GetConst(SYSTEM_COMPILE_OUTPUT, &symbol);
	Return(open_io_memory_stream_(&stream, Nil, 0, 0, 0));
	pushspecial_control(ptr, symbol, stream);

	return Result(ret, stream);
}

static int compile_eval_code_call_(Execute ptr, addr pos, addr code, addr *ret)
{
	addr stream;

	/* push */
	Return(compile_eval_push_output_(ptr, &stream));
	begin_load_push(ptr);

	/* execute */
	Return(compile_eval_parse_(ptr, pos, NULL));
	Return(load_depend_code_(ptr, code, stream, pos, ret));

	return 0;
}

static int compile_eval_code_(Execute ptr, addr pos, addr code, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_eval_code_call_(ptr, pos, code, ret);
	return pop_control_(ptr, control);
}

static int compile_eval_read_(Execute ptr, addr input, addr *ret)
{
	int check;
	addr pos, code;

	code = Nil;
	for (;;) {
		Return(read_stream(ptr, input, &check, &pos));
		if (check)
			break;
		Return(compile_eval_code_(ptr, pos, code, &code));
	}

	return Result(ret, code);
}


/*
 *  write
 */
static int compile_output_break_(Execute ptr)
{
	addr pos;

	GetConst(SYSTEM_COMPILE_OUTPUT, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	return faslwrite_break_(pos);
}

static int compile_eval_output_(Execute ptr)
{
	addr pos;
	LocalHold hold;
	size_t size;

	hold = LocalHold_array(ptr, 1);
	Return(get_load_size_(ptr, &pos));
	GetIndex(pos, &size);
	if (size) {
		code_make_load_alloc(ptr->local, &pos, pos);
		localhold_set(hold, 0, pos);
		Return(eval_compile_file(ptr, pos));
		Return(compile_output_break_(ptr));
	}
	localhold_end(hold);

	return 0;
}

static int compile_gensym_output_(Execute ptr)
{
	addr pos;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	Return(list_load_gensym_(ptr, &pos));
	if (pos != Nil) {
		code_make_load_gensym(ptr->local, &pos, pos);
		localhold_set(hold, 0, pos);
		Return(eval_compile_file(ptr, pos));
		Return(compile_output_break_(ptr));
	}
	localhold_end(hold);

	return 0;
}

static int compile_depend_output_(Execute ptr, addr depend)
{
	addr stream;

	GetConst(SYSTEM_COMPILE_OUTPUT, &stream);
	Return(getspecialcheck_local_(ptr, stream, &stream));
	return compile_depend_make_(ptr, stream, depend);
}


/*
 *  start
 */
static int compile_eval_start_(Execute ptr, addr stream)
{
	addr depend;

	Return(compile_eval_read_(ptr, stream, &depend));
	/* write */
	Return(compile_eval_output_(ptr));
	Return(compile_gensym_output_(ptr));
	Return(compile_depend_output_(ptr, depend));

	return 0;
}


/*
 *  interface
 */
static int compile_eval_call_(Execute ptr, addr stream)
{
	/* special variable */
	push_toplevel_eval(ptr, T);
	push_compile_time_eval(ptr, Nil);
	push_compile_toplevel_eval(ptr, Nil);
	push_load_toplevel_eval(ptr, T);
	push_execute_eval(ptr, T);
	/* init */
	init_load_time_value(ptr);
	Return(begin_eval_stack_(ptr));
	free_eval_stack(ptr);

	return compile_eval_start_(ptr, stream);
}

int compile_load_stream_(Execute ptr, addr stream)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_eval_call_(ptr, stream);
	return pop_control_(ptr, control);
}


/*
 *  partial
 */
static int compile_partial_make_call_(Execute ptr, addr pos, addr *ret, addr *rtype)
{
	addr stream;

	/* push */
	Return(compile_eval_push_output_(ptr, &stream));
	begin_load_push(ptr);

	/* execute */
	GetTypeTable(rtype, T);
	Return(compile_eval_parse_(ptr, pos, rtype));
	return load_depend_partial_(ptr, stream, pos, ret);
}

static int compile_partial_make_(Execute ptr, addr pos, addr *ret, addr *rtype)
{
	addr control;

	push_control(ptr, &control);
	(void)compile_partial_make_call_(ptr, pos, ret, rtype);
	return pop_control_(ptr, control);
}

int compile_partial_(Execute ptr, addr pos, addr *ret, addr *rtype)
{
	addr depend;

	Return(compile_partial_make_(ptr, pos, &depend, rtype));
	get_index_load_depend(depend, ret);
	return push_load_push_(ptr, depend);
}


/*
 *  instance single
 */
static int compile_instance_execute_call_(Execute ptr, addr *ret, addr pos, addr index)
{
	addr stream, depend;

	/* push */
	Return(compile_eval_push_output_(ptr, &stream));
	begin_load_push(ptr);

	/* execute */
	Return(compile_eval_parse_(ptr, pos, NULL));

	/* depend */
	load_depend_heap(&depend, stream, pos, index);
	Return(end_load_push_(ptr, depend));

	return Result(ret, depend);
}

static int compile_instance_execute_(Execute ptr, addr *ret, addr pos, addr index)
{
	addr control;

	push_control(ptr, &control);
	gchold_push_special(ptr, pos);
	(void)compile_instance_execute_call_(ptr, ret, pos, index);
	return pop_control_(ptr, control);
}

static int compile_instance_instance_(Execute ptr, addr *ret, addr instance, addr init)
{
	addr funcall;

	GetConst(COMMON_FUNCALL, &funcall);
	list_heap(&init, funcall, init, instance, NULL);
	return compile_instance_execute_(ptr, ret, init, Nil);
}

int compile_instance_(Execute ptr, addr pos, addr make, addr init)
{
	addr instance, index;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_push(hold, pos);
	localhold_push(hold, make);
	localhold_push(hold, init);

	/* instance */
	Return(incf_load_size_(ptr, &index));
	Return(intern_load_table_(ptr, pos, index));
	load_depend_heap(&instance, Nil, pos, Nil);
	localhold_push(hold, instance);

	/* form */
	Return(compile_instance_execute_(ptr, &make, make, index));
	localhold_push(hold, make);
	if (init != Nil) {
		Return(compile_instance_instance_(ptr, &init, pos, init));
		localhold_push(hold, init);
	}

	Return(load_depend_instance_(ptr, instance, make, init));
	Return(push_load_push_(ptr, instance));
	localhold_end(hold);

	return 0;
}

