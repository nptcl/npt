#include "compile_write.h"
#include "condition.h"
#include "cons_list.h"
#include "constant.h"
#include "control_object.h"
#include "execute.h"
#include "hashtable.h"
#include "load_code.h"
#include "load_depend.h"
#include "load_time_value.h"
#include "stream.h"
#include "stream_function.h"
#include "symbol.h"
#include "typedef.h"


/*
 *  symbol
 */
static void depend_loop_symbol(addr *ret)
{
	GetConst(SYSTEM_DEPEND_LOOP, ret);
}
static int get_depend_loop_(Execute ptr, addr *ret)
{
	addr symbol;
	depend_loop_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}
static void set_depend_loop(Execute ptr, addr value)
{
	addr symbol;
	depend_loop_symbol(&symbol);
	setspecial_local(ptr, symbol, value);
}

static void depend_pass_symbol(addr *ret)
{
	GetConst(SYSTEM_DEPEND_PASS, ret);
}
static void push_depend_pass(Execute ptr, addr value)
{
	addr symbol;
	depend_pass_symbol(&symbol);
	pushspecial_control(ptr, symbol, value);
}
static int get_depend_pass_(Execute ptr, addr *ret)
{
	addr symbol;
	depend_pass_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}
static void set_depend_pass(Execute ptr, addr value)
{
	addr symbol;
	depend_pass_symbol(&symbol);
	setspecial_local(ptr, symbol, value);
}

static void depend_error_symbol(addr *ret)
{
	GetConst(SYSTEM_DEPEND_ERROR, ret);
}
static void push_depend_error(Execute ptr, addr value)
{
	addr symbol;
	depend_error_symbol(&symbol);
	pushspecial_control(ptr, symbol, value);
}
static int get_depend_error_(Execute ptr, addr *ret)
{
	addr symbol;
	depend_error_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}
static void set_depend_error(Execute ptr, addr value)
{
	addr symbol;
	depend_error_symbol(&symbol);
	setspecial_local(ptr, symbol, value);
}

static void depend_root_symbol(addr *ret)
{
	GetConst(SYSTEM_DEPEND_ROOT, ret);
}

int get_depend_root_(Execute ptr, addr *ret)
{
	addr symbol;
	depend_root_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

static void set_depend_root(Execute ptr, addr value)
{
	addr symbol;
	depend_root_symbol(&symbol);
	setspecial_local(ptr, symbol, value);
}


/*
 *  object
 */
void load_depend_heap(addr *ret, addr stream, addr value, addr index)
{
	addr pos;

	vector2_heap(&pos, 3);
	SetArrayA2(pos, 0, stream);
	SetArrayA2(pos, 1, value);
	SetArrayA2(pos, 2, index);
	*ret = pos;
}

static void empty_load_depend(addr *ret)
{
	load_depend_heap(ret, Nil, Nil, Nil);
}

void get_stream_load_depend(addr pos, addr *ret)
{
	GetArrayA2(pos, 0, ret);
}

void get_index_load_depend(addr pos, addr *ret)
{
	GetArrayA2(pos, 2, ret);
}


/*
 *  *load-depend*
 */
static void load_depend_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_DEPEND, ret);
}

static int get_load_depend_(Execute ptr, addr *ret)
{
	addr symbol;
	load_depend_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

static int load_depend_find(addr x, addr y, addr list)
{
	addr x1, y1;

	while (list != Nil) {
		GetCons(list, &x1, &list);
		GetCons(x1, &x1, &y1);
		if (x1 == x && y1 == y)
			return 1;
	}

	return 0;
}

static int load_depend_pushnew_(Execute ptr, addr x, addr y)
{
	addr symbol, list;

	load_depend_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	if (load_depend_find(x, y, list))
		return 0;
	cons_heap(&x, x, y);
	cons_heap(&list, x, list);
	setspecial_local(ptr, symbol, list);

	return 0;
}


/*
 *  *load-push*
 */
static void load_push_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_PUSH, ret);
}

void begin_load_push(Execute ptr)
{
	addr symbol;

	load_push_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
}

int end_load_push_(Execute ptr, addr code)
{
	addr symbol, list, pos;

	load_push_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(load_depend_pushnew_(ptr, code, pos));
	}

	return 0;
}

int push_load_push_(Execute ptr, addr code)
{
	addr symbol, list;

	load_push_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	cons_heap(&list, code, list);
	setspecial_local(ptr, symbol, list);

	return 0;
}


/*
 *  loop
 */
static int load_depend_set_loop_(Execute ptr, addr x, addr y)
{
	addr list, cons;

	Return(get_depend_loop_(ptr, &list));
	if (find_assoc_eq_unsafe(x, list, &cons)) {
		SetCdr(cons, y);
		return 0;
	}

	/* push */
	cons_heap(&cons, x, y);
	cons_heap(&list, cons, list);
	set_depend_loop(ptr, list);

	return 0;
}


/*
 *  depend
 */
int load_depend_code_(Execute ptr, addr stream, addr value)
{
	addr code, pos, symbol, list;

	/* code */
	Return(get_depend_root_(ptr, &code));
	if (code == Nil)
		empty_load_depend(&code);
	load_depend_heap(&pos, stream, value, Nil);
	Return(load_depend_pushnew_(ptr, pos, code));

	/* push */
	load_push_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &list));
	while (list != Nil) {
		GetCons(list, &code, &list);
		Return(load_depend_pushnew_(ptr, pos, code));
	}
	set_depend_root(ptr, pos);

	return 0;
}

int load_depend_partial_(Execute ptr, addr stream, addr value, addr *ret)
{
	addr depend, index;

	Return(incf_load_size_(ptr, &index));
	load_depend_heap(&depend, stream, value, index);
	Return(end_load_push_(ptr, depend));

	return Result(ret, depend);
}

int load_depend_instance_(Execute ptr, addr instance, addr make, addr init)
{
	/*  (makefile :instance :make :init)
	 *  (makefile :init :make)
	 *  (makefile-loop :init :instance)
	 */
	Return(load_depend_pushnew_(ptr, instance, make));
	if (init == Nil)
		return 0;
	Return(load_depend_pushnew_(ptr, instance, init));
	Return(load_depend_pushnew_(ptr, init, make));
	Return(load_depend_set_loop_(ptr, init, instance));

	return 0;
}


/*
 *  make
 */
static int compile_depend_error_(Execute ptr, addr code)
{
	addr list;

	Return(get_depend_error_(ptr, &list));
	if (find_list_eq_unsafe(code, list))
		return fmte_("The loop occured by the ~S.", code, NULL);

	return 0;
}

static int compile_depend_pass_(Execute ptr, addr code, int *ret)
{
	int check;
	addr list;

	Return(get_depend_pass_(ptr, &list));
	check = find_list_eq_unsafe(code, list);

	return Result(ret, check);
}

static int compile_depend_loop_(Execute ptr, addr code)
{
	addr list, pos;

	Return(get_depend_loop_(ptr, &list));
	find_assoc_eq_unsafe(code, list, &pos);
	if (pos == Nil)
		return 0;
	GetCdr(pos, &pos);
	if (delete_list_eq_unsafe(code, list, &list))
		set_depend_loop(ptr, list);

	return 0;
}

static int compile_depend_push_error_(Execute ptr, addr code)
{
	addr list;

	Return(get_depend_error_(ptr, &list));
	cons_heap(&list, code, list);
	set_depend_error(ptr, list);

	return 0;
}

static int compile_depend_push_pass_(Execute ptr, addr code)
{
	addr list;

	Return(get_depend_pass_(ptr, &list));
	cons_heap(&list, code, list);
	set_depend_pass(ptr, list);

	return 0;
}

static int compile_depend_gather_(Execute ptr, addr code, addr *ret)
{
	addr root, list, x, y;

	Return(get_load_depend_(ptr, &root));
	list = Nil;
	while (root != Nil) {
		GetCons(root, &x, &root);
		GetCons(x, &x, &y);
		if (code == x)
			cons_heap(&list, y, list);
	}

	return Result(ret, list);
}

static int compile_depend_set_(Execute ptr, addr stream, addr index)
{
	addr pos;
	code_make_load_set(ptr, &pos, index);
	return faslwrite_value(ptr, stream, pos);
}

static int compile_depend_redirect_(Execute ptr, addr stream, addr code)
{
	int ignore;
	addr memory, index;

	/* ignore */
	get_stream_load_depend(code, &memory);
	if (memory == Nil)
		return 0;

	/* memory */
	Return(file_position_start_stream_(memory, &ignore));
	Return(redirect_unsigned8_stream_(ptr, memory, stream));
	Return(faslwrite_break_(stream));

	/* index */
	get_index_load_depend(code, &index);
	if (index != Nil) {
		Return(compile_depend_set_(ptr, stream, index));
		Return(faslwrite_break_(stream));
	}

	return 0;
}

static int compile_depend_execute_(Execute ptr, addr stream, addr code);
static int compile_depend_execute_let_(Execute ptr, addr stream, addr code)
{
	addr list, pos;

	Return(compile_depend_loop_(ptr, code));
	Return(compile_depend_push_error_(ptr, code));
	Return(compile_depend_push_pass_(ptr, code));
	Return(compile_depend_gather_(ptr, code, &list));
	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(compile_depend_execute_(ptr, stream, pos));
	}

	return compile_depend_redirect_(ptr, stream, code);
}

static int compile_depend_execute_(Execute ptr, addr stream, addr code)
{
	int check;
	addr control, list;

	Return(compile_depend_error_(ptr, code));
	Return(compile_depend_pass_(ptr, code, &check));
	if (check)
		return 0;

	Return(get_depend_error_(ptr, &list));
	push_control(ptr, &control);
	push_depend_error(ptr, list);
	(void)compile_depend_execute_let_(ptr, stream, code);
	return pop_control_(ptr, control);
}

int compile_depend_make_(Execute ptr, addr stream, addr code)
{
	addr control;

	push_control(ptr, &control);
	push_depend_pass(ptr, Nil);
	push_depend_error(ptr, Nil);
	/* make */
	(void)compile_depend_execute_(ptr, stream, code);
	return pop_control_(ptr, control);
}


/*
 *  initialize
 */
void init_load_depend(Execute ptr)
{
	addr symbol;

	/* *load-depend* */
	load_depend_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);

	/* *load-push* */
	load_push_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);

	/* *depend-loop* */
	depend_loop_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);

	/* *depend-root* */
	depend_root_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
}

void disable_load_depend(Execute ptr)
{
	addr symbol;

	/* *load-depend* */
	load_depend_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);

	/* *load-push* */
	load_push_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);

	/* *depend-loop* */
	depend_loop_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);

	/* *depend-root* */
	depend_root_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);
}

