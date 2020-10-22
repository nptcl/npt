#include "array_vector.h"
#include "build.h"
#include "code_object.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "constant.h"
#include "function.h"
#include "hold.h"
#include "pointer.h"
#include "print_function.h"
#include "print_pretty.h"
#include "print_write.h"
#include "sequence.h"
#include "stream.h"
#include "stream_function.h"
#include "strvect.h"
#include "stream_pretty.h"
#include "symbol.h"

/*
 *  unwind-protect
 */
static int pprint_logical_block_close(Execute ptr)
{
	addr stream;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(close_pretty_stream_(ptr, stream));
	setresult_control(ptr, Nil);

	return 0;
}


/*
 *  pprint-fill
 */
static int pprint_logical_block_type_form(Execute ptr, enum pprint_newline type)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(pprint_exit_common(ptr, stream));
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, type, stream));
	}

	return 0;
}

static int pprint_logical_block_type_call_(Execute ptr, pointer type, addr stream)
{
	addr gensym;

	setprotect_control(ptr, p_pprint_logical_block_close, stream);
	Return(gensym_pretty_stream_(stream, &gensym));
	return catch_clang(ptr, type, gensym, stream);
}

static int pprint_logical_block_type(Execute ptr, pointer type)
{
	addr stream, control;

	/* stream */
	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	/* unwind-protect */
	push_control(ptr, &control);
	(void)pprint_logical_block_type_call_(ptr, type, stream);
	return pop_control_(ptr, control);
}

static int pprint_type_print(Execute ptr,
		addr stream, addr list, int colon, pointer type)
{
	addr prefix, suffix, lambda;

	/* make-pprint-stream */
	prefix = suffix = Nil;
	if (colon) {
		strvect_char_heap(&prefix, "(");
		strvect_char_heap(&suffix, ")");
	}
	Return(open_pretty_stream_(ptr, &stream, stream, list, prefix, Nil, suffix));

	/* function */
	compiled_heap(&lambda, Nil);
	setcompiled_empty(lambda, type);
	SetDataFunction(lambda, stream);

	/* call */
	gchold_pushva_local(ptr->local, stream, lambda, NULL);
	return call_pretty_stream(ptr, stream, lambda);
}

static int pprint_list_common(Execute ptr, addr stream, addr list, pointer type)
{
	return pprint_type_print(ptr, stream, list, 1, type);
}

static int pprint_logical_block_fill_form(Execute ptr)
{
	return pprint_logical_block_type_form(ptr, pprint_newline_fill);
}

static int pprint_logical_block_fill(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_logical_block_fill_form);
}

_g int pprint_fill_print(Execute ptr, addr stream, addr list, int colon)
{
	/* (defun pprint-fill (*standard-output* list &optional (colon t) atsign)
	 *   (declare (ignore atsign))
	 *   (pprint-logical-block
	 *     (nil list :prefix (if colon "(" "") :suffix (if colon ")" ""))
	 *     (pprint-exit-if-list-exhausted)
	 *     (loop (write (pprint-pop))
	 *           (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-newline :fill))))
	 */
	return pprint_type_print(ptr, stream, list, colon, p_pprint_logical_block_fill);
}


/*
 *  pprint-linaer
 */
static int pprint_logical_block_linear_form(Execute ptr)
{
	return pprint_logical_block_type_form(ptr, pprint_newline_linear);
}

static int pprint_logical_block_linear(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_logical_block_linear_form);
}

_g int pprint_linear_print(Execute ptr, addr stream, addr list, int colon)
{
	/* (defun pprint-linear (*standard-output* list &optional (colon t) atsign)
	 *   (declare (ignore atsign))
	 *   (pprint-logical-block
	 *     (nil list :prefix (if colon "(" "") :suffix (if colon ")" ""))
	 *     (pprint-exit-if-list-exhausted)
	 *     (loop (write (pprint-pop))
	 *           (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-newline :linear))))
	 */
	return pprint_type_print(ptr, stream, list, colon, p_pprint_logical_block_linear);
}


/*
 *  pprint-tabular
 */
static int pprint_logical_block_tabular_form(Execute ptr)
{
	addr cons, stream, pos;
	fixnum colinc;

	getdata_control(ptr, &cons);
	GetCons(cons, &stream, &pos);
	Check(! pretty_stream_p(stream), "type error");
	GetFixnum(pos, &colinc);
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(pprint_exit_common(ptr, stream));
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_tab_section_relative_(ptr, stream, 0, colinc));
		Return(pprint_newline_print_(ptr, pprint_newline_fill, stream));
	}

	return 0;
}

static int pprint_logical_block_tabular_call_(Execute ptr, addr stream, addr cons)
{
	addr gensym;

	setprotect_control(ptr, p_pprint_logical_block_close, stream);
	Return(gensym_pretty_stream_(stream, &gensym));
	return catch_clang(ptr, p_pprint_logical_block_tabular_form, gensym, cons);
}

static int pprint_logical_block_tabular(Execute ptr)
{
	addr cons, stream, control;

	/* stream */
	getdata_control(ptr, &cons);
	GetCar(cons, &stream);
	Check(! pretty_stream_p(stream), "type error");
	/* unwind-protect */
	push_control(ptr, &control);
	(void)pprint_logical_block_tabular_call_(ptr, stream, cons);
	return pop_control_(ptr, control);
}

_g int pprint_tabular_print(Execute ptr,
		addr stream, addr list, int colon, fixnum tabsize)
{
	/* (defun pprint-tabular
	 *   (*standard-output* list &optional (colon t) atsign (tabsize 16))
	 *   (declare (ignore atsign))
	 *   (pprint-logical-block
	 *     (nil list :prefix (if colon "(" "") :suffix (if colon ")" ""))
	 *     (pprint-exit-if-list-exhausted)
	 *     (loop (write (pprint-pop))
	 *           (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-tab :section-relative 0 tabsize)
	 *           (pprint-newline :fill))))
	 */
	addr prefix, suffix, lambda, cons;

	/* make-pprint-stream */
	prefix = suffix = Nil;
	if (colon) {
		strvect_char_heap(&prefix, "(");
		strvect_char_heap(&suffix, ")");
	}
	Return(open_pretty_stream_(ptr, &stream, stream, list, prefix, Nil, suffix));

	/* closure */
	fixnum_heap(&cons, tabsize);
	cons_heap(&cons, stream, cons);

	/* function */
	compiled_heap(&lambda, Nil);
	setcompiled_empty(lambda, p_pprint_logical_block_tabular);
	SetDataFunction(lambda, cons);

	/* call */
	gchold_pushva_local(ptr->local, stream, lambda, NULL);
	return call_pretty_stream(ptr, stream, lambda);
}


/*
 *  dispatch-vector
 */
static int pprint_dispatch_vector2(Execute ptr)
{
	addr cons, stream, pos, vector;
	size_t size, i;

	getdata_control(ptr, &cons);
	GetCons(cons, &stream, &vector);
	Check(! pretty_stream_p(stream), "type error");
	Check(! vectorp_sequence_debug(vector), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(length_sequence_(vector, 1, &size));
	if (size == 0)
		return 0;
	i = 0;
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(getelt_sequence_(NULL, vector, i, &pos));
		Return(write_print(ptr, stream, pos));
		i++;
		if (size <= i)
			break;
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_fill, stream));
	}

	return 0;
}

static int pprint_dispatch_vector1_call_(Execute ptr, addr stream, addr cons)
{
	addr gensym;

	setprotect_control(ptr, p_pprint_logical_block_close, stream);
	Return(gensym_pretty_stream_(stream, &gensym));
	return catch_clang(ptr, p_pprint_dispatch_vector2, gensym, cons);
}

static int pprint_dispatch_vector1(Execute ptr)
{
	addr cons, stream, control;

	/* stream */
	getdata_control(ptr, &cons);
	GetCar(cons, &stream);
	Check(! pretty_stream_p(stream), "type error");
	/* unwind-protect */
	push_control(ptr, &control);
	(void)pprint_dispatch_vector1_call_(ptr, stream, cons);
	return pop_control_(ptr, control);
}

static int pprint_dispatch_vector(Execute ptr, addr stream, addr pos)
{
	/* (defun dispatch-vector (*standard-output* v)
	 *   (pprint-logical-block (nil nil :prefix "#(" :suffix ")")
	 *     (let ((end (length v)) (i 0))
	 *       (when (plusp end)
	 *         (loop (pprint-pop)
	 *               (write (aref v i))
	 *               (if (= (incf i) end) (return nil))
	 *               (write-char #\Space)
	 *               (pprint-newline :fill))))))
	 */
	addr prefix, suffix, lambda, cons;

	/* make-pprint-stream */
	strvect_char_heap(&prefix, "#(");
	strvect_char_heap(&suffix, ")");
	Return(open_pretty_stream_(ptr, &stream, stream, Nil, prefix, Nil, suffix));

	/* closure */
	cons_heap(&cons, stream, pos);

	/* function */
	compiled_heap(&lambda, Nil);
	setcompiled_empty(lambda, p_pprint_dispatch_vector1);
	SetDataFunction(lambda, cons);

	/* call */
	gchold_pushva_local(ptr->local, stream, lambda, NULL);
	return call_pretty_stream(ptr, stream, lambda);
}


/*
 *  dispatch-quote
 */
static int pprint_dispatch_quote2(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(write_char_stream_(stream, '\''));
	Return(pprint_pop_common(ptr, stream, &pos));  /* quote */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(write_print(ptr, stream, pos));

	return 0;
}

static int pprint_dispatch_quote1(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_quote2);
}

static int pprint_dispatch_quote(Execute ptr, addr stream, addr list)
{
	/* (defun dispatch-quote (*standard-outupt* list)
	 *   (pprint-logical-block (nil list)
	 *     ;; name
	 *     (write-char #\')
	 *     (pprint-pop) ;; quote
	 *     (write (pprint-pop))))
	 */
	return pprint_type_print(ptr, stream, list, 0, p_pprint_dispatch_quote1);
}


/*
 *  dispatch-call
 */
static int pprint_dispatch_call2(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(write_print(ptr, stream, pos));
	Return(pprint_exit_common(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	Return(pprint_indent_print_(ptr, 0, 0, stream));
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
	}

	return 0;
}

static int pprint_dispatch_call1(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_call2);
}

static int pprint_dispatch_call(Execute ptr, addr stream, addr list)
{
	/* (defun dispatch-call (*standard-outupt* list)
	 *   (pprint-logical-block (nil list :prefix "(" :suffix ")")
	 *     ;; name
	 *     (write (pprint-pop))
	 *     (pprint-exit-if-list-exhausted)
	 *     (write-char #\Space)
	 *     (pprint-newline :miser)
	 *     (pprint-indent :current 0)
	 *     ;; args
	 *     (loop (write (pprint-pop))
	 *           (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-newline :linear))))
	 */
	return pprint_list_common(ptr, stream, list, p_pprint_dispatch_call1);
}


/*
 *  dispatch-defun
 */
static int pprint_dispatch_defun6(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(pprint_exit_common(ptr, stream));
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
	}

	return 0;
}

static int pprint_dispatch_defun5(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_defun6);
}

static int pprint_dispatch_defun4(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* body */
	Return(pprint_exit_common(ptr, stream));
	for (;;) {
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(pprint_list_common(ptr, stream, pos, p_pprint_dispatch_defun5));
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_fill, stream));
	}

	return 0;
}

static int pprint_dispatch_defun3(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_defun4);
}

static int pprint_dispatch_defun2(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* defun */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(write_print(ptr, stream, pos));
	Return(pprint_exit_common(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	/* name */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(write_print(ptr, stream, pos));
	Return(pprint_exit_common(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	/* args */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(pprint_list_common(ptr, stream, pos, p_pprint_dispatch_defun3));
	/* body */
	Return(pprint_indent_print_(ptr, 1, 1, stream));
	for (;;) {
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
	}

	return 0;
}

static int pprint_dispatch_defun1(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_defun2);
}

static int pprint_dispatch_defun(Execute ptr, addr stream, addr list)
{
	/* (defun dispatch-defun (*standard-output* list)
	 *   (pprint-logical-block (nil list :prefix "(" :suffix ")")
	 *     ;; defun
	 *     (write (pprint-pop))
	 *     (pprint-exit-if-list-exhausted)
	 *     (write-char #\Space)
	 *     (pprint-newline :miser)
	 *     ;; name
	 *     (write (pprint-pop))
	 *     (pprint-exit-if-list-exhausted)
	 *     (write-char #\Space)
	 *     (pprint-newline :miser)
	 *     ;; args
	 *     (pprint-logical-block (nil (pprint-pop) :prefix "(" :suffix ")")
	 *       (pprint-exit-if-list-exhausted)
	 *       (loop (pprint-logical-block (nil (pprint-pop) :prefix "(" :suffix ")")
	 *               (pprint-exit-if-list-exhausted)
	 *               (loop (write (pprint-pop))
	 *                     (pprint-exit-if-list-exhausted)
	 *                     (write-char #\Space)
	 *                     (pprint-newline :linear)))
	 *             (pprint-exit-if-list-exhausted)
	 *             (write-char #\Space)
	 *             (pprint-newline :fill)))
	 *     ;; body
	 *     (pprint-indent :block 1)
	 *     (loop (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-newline :linear)
	 *           (write (pprint-pop)))))
	 */
	return pprint_list_common(ptr, stream, list, p_pprint_dispatch_defun1);
}


/*
 *  dispatch-let
 */
static int pprint_dispatch_let2(Execute ptr)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	Return(check_pretty_stream(ptr, stream));
	/* let */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(write_print(ptr, stream, pos));
	Return(pprint_exit_common(ptr, stream));
	Return(write_char_stream_(stream, ' '));
	Return(pprint_newline_print_(ptr, pprint_newline_miser, stream));
	/* args */
	Return(pprint_pop_common(ptr, stream, &pos));
	Return(pprint_list_common(ptr, stream, pos, p_pprint_dispatch_defun3));
	/* body */
	Return(pprint_indent_print_(ptr, 1, 1, stream));
	for (;;) {
		Return(pprint_exit_common(ptr, stream));
		Return(write_char_stream_(stream, ' '));
		Return(pprint_newline_print_(ptr, pprint_newline_linear, stream));
		Return(pprint_pop_common(ptr, stream, &pos));
		Return(write_print(ptr, stream, pos));
	}

	return 0;
}

static int pprint_dispatch_let1(Execute ptr)
{
	return pprint_logical_block_type(ptr, p_pprint_dispatch_let2);
}

static int pprint_dispatch_let(Execute ptr, addr stream, addr list)
{
	/* (defun dispatch-let (*standard-output* list)
	 *   (pprint-logical-block (nil list :prefix "(" :suffix ")")
	 *     ;; let
	 *     (write (pprint-pop))
	 *     (pprint-exit-if-list-exhausted)
	 *     (write-char #\Space)
	 *     (pprint-newline :miser)
	 *     ;; args
	 *     (pprint-logical-block (nil (pprint-pop) :prefix "(" :suffix ")")
	 *       (pprint-exit-if-list-exhausted)
	 *       (loop (pprint-logical-block (nil (pprint-pop) :prefix "(" :suffix ")")
	 *               (pprint-exit-if-list-exhausted)
	 *               (loop (write (pprint-pop))
	 *                     (pprint-exit-if-list-exhausted)
	 *                     (write-char #\Space)
	 *                     (pprint-newline :linear)))
	 *             (pprint-exit-if-list-exhausted)
	 *             (write-char #\Space)
	 *             (pprint-newline :fill)))
	 *     ;; body
	 *     (pprint-indent :block 1)
	 *     (loop (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space)
	 *           (pprint-newline :linear)
	 *           (write (pprint-pop)))))
	 */
	return pprint_list_common(ptr, stream, list, p_pprint_dispatch_let1);
}


/*
 *  initialize
 */
_g void init_print_function(void)
{
	SetPointerType(empty, pprint_logical_block_close);
	/* pprint-fill */
	SetPointerType(empty, pprint_logical_block_fill_form);
	SetPointerType(empty, pprint_logical_block_fill);
	/* pprint-linear */
	SetPointerType(empty, pprint_logical_block_linear_form);
	SetPointerType(empty, pprint_logical_block_linear);
	/* pprint-tabular */
	SetPointerType(empty, pprint_logical_block_tabular_form);
	SetPointerType(empty, pprint_logical_block_tabular);
	/* dispatch-vector */
	SetPointerType(empty, pprint_dispatch_vector2);
	SetPointerType(empty, pprint_dispatch_vector1);
	SetPointerType(var2, pprint_dispatch_vector);
	/* dispatch-quote */
	SetPointerType(empty, pprint_dispatch_quote2);
	SetPointerType(empty, pprint_dispatch_quote1);
	SetPointerType(var2, pprint_dispatch_quote);
	/* dispatch-call */
	SetPointerType(empty, pprint_dispatch_call2);
	SetPointerType(empty, pprint_dispatch_call1);
	SetPointerType(var2, pprint_dispatch_call);
	/* defun */
	SetPointerType(empty, pprint_dispatch_defun6);
	SetPointerType(empty, pprint_dispatch_defun5);
	SetPointerType(empty, pprint_dispatch_defun4);
	SetPointerType(empty, pprint_dispatch_defun3);
	SetPointerType(empty, pprint_dispatch_defun2);
	SetPointerType(empty, pprint_dispatch_defun1);
	SetPointerType(var2, pprint_dispatch_defun);
	/* let */
	SetPointerType(empty, pprint_dispatch_let2);
	SetPointerType(empty, pprint_dispatch_let1);
	SetPointerType(var2, pprint_dispatch_let);
}

