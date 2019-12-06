#include "array_vector.h"
#include "build.h"
#include "code.h"
#include "control.h"
#include "constant.h"
#include "function.h"
#include "pointer.h"
#include "print_function.h"
#include "print_pretty.h"
#include "print_write.h"
#include "sequence.h"
#include "stream.h"
#include "stream_pretty.h"
#include "symbol.h"
#include "unicode.h"

/*
 *  unwind-protect
 */
static void pprint_logical_block_close(Execute ptr)
{
	addr stream;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	close_pretty_stream(ptr, stream);
	setresult_control(ptr, Nil);
}


/*
 *  pprint-fill
 */
static void pprint_logical_block_type_form(Execute ptr, enum pprint_newline type)
{
	addr stream, pos;

	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	if (check_pretty_stream(ptr, stream))
		return;
	/* body */
	if (pprint_exit_common(ptr, stream))
		return;
	for (;;) {
		if (pprint_pop_common(ptr, stream, &pos))
			return;
		if (write_print(ptr, stream, pos))
			return;
		if (pprint_exit_common(ptr, stream))
			return;
		write_char_stream(stream, ' ');
		pprint_newline_common(ptr, type, stream);
	}
}

static void pprint_logical_block_type(Execute ptr, pointer type)
{
	int check;
	addr stream, control, code, gensym;

	/* stream */
	getdata_control(ptr, &stream);
	Check(! pretty_stream_p(stream), "type error");
	/* unwind-protect */
	push_finalize_control(ptr, &control);
	syscall_code(ptr->local, &code, p_pprint_logical_block_close, stream);
	setfinalize_control(ptr, control, code);
	/* code */
	gensym_pretty_stream(stream, &gensym);
	catch_syscall_code(&code, type, gensym, stream);
	check = callclang_funcall(ptr, &code, code, NULL);
	free_check_control(ptr, control, check);
}

static int pprint_type_common(Execute ptr,
		addr stream, addr list, int colon, pointer type)
{
	addr prefix, suffix, lambda;

	/* make-pprint-stream */
	prefix = suffix = Nil;
	if (colon) {
		strvect_char_heap(&prefix, "(");
		strvect_char_heap(&suffix, ")");
	}
	open_pretty_stream(ptr, &stream, stream, list, prefix, Nil, suffix);
	/* function */
	compiled_heap(&lambda, Nil);
	setcompiled_empty(lambda, type);
	SetDataFunction(lambda, stream);
	/* call */
	return call_pretty_stream(ptr, stream, lambda);
}

static void pprint_logical_block_fill_form(Execute ptr)
{
	pprint_logical_block_type_form(ptr, pprint_newline_fill);
}

static void pprint_logical_block_fill(Execute ptr)
{
	pprint_logical_block_type(ptr, p_pprint_logical_block_fill_form);
}

_g int pprint_fill_common(Execute ptr, addr stream, addr list, int colon)
{
	/* (defun pprint-fill (stream list &optional (colon t) atsign)
	 *   (declare (ignore atsign))
	 *   (pprint-logical-block
	 *     (stream list :prefix (if colon "(" "") :suffix (if colon ")" ""))
	 *     (pprint-exit-if-list-exhausted)
	 *     (loop (write (pprint-pop) :stream stream)
	 *           (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space stream)
	 *           (pprint-newline :fill stream))))
	 */
	return pprint_type_common(ptr, stream, list, colon, p_pprint_logical_block_fill);
}


/*
 *  pprint-linaer
 */
static void pprint_logical_block_linear_form(Execute ptr)
{
	pprint_logical_block_type_form(ptr, pprint_newline_linear);
}

static void pprint_logical_block_linear(Execute ptr)
{
	pprint_logical_block_type(ptr, p_pprint_logical_block_linear_form);
}

_g int pprint_linear_common(Execute ptr, addr stream, addr list, int colon)
{
	/* (defun pprint-fill (stream list &optional (colon t) atsign)
	 *   (declare (ignore atsign))
	 *   (pprint-logical-block
	 *     (stream list :prefix (if colon "(" "") :suffix (if colon ")" ""))
	 *     (pprint-exit-if-list-exhausted)
	 *     (loop (write (pprint-pop) :stream stream)
	 *           (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space stream)
	 *           (pprint-newline :linear stream))))
	 */
	return pprint_type_common(ptr, stream, list, colon, p_pprint_logical_block_linear);
}


/*
 *  pprint-tabular
 */
static void pprint_logical_block_tabular_form(Execute ptr)
{
	addr cons, stream, pos;
	fixnum colinc;

	getdata_control(ptr, &cons);
	GetCons(cons, &stream, &pos);
	Check(! pretty_stream_p(stream), "type error");
	GetFixnum(pos, &colinc);
	if (check_pretty_stream(ptr, stream))
		return;
	/* body */
	if (pprint_exit_common(ptr, stream))
		return;
	for (;;) {
		if (pprint_pop_common(ptr, stream, &pos))
			return;
		if (write_print(ptr, stream, pos))
			return;
		if (pprint_exit_common(ptr, stream))
			return;
		write_char_stream(stream, ' ');
		pprint_tab_section_relative(ptr, stream, 0, colinc);
		pprint_newline_common(ptr, pprint_newline_fill, stream);
	}
}

static void pprint_logical_block_tabular(Execute ptr)
{
	int check;
	addr cons, stream, control, code, gensym;

	/* stream */
	getdata_control(ptr, &cons);
	GetCar(cons, &stream);
	Check(! pretty_stream_p(stream), "type error");
	/* unwind-protect */
	push_finalize_control(ptr, &control);
	syscall_code(ptr->local, &code, p_pprint_logical_block_close, stream);
	setfinalize_control(ptr, control, code);
	/* code */
	gensym_pretty_stream(stream, &gensym);
	catch_syscall_code(&code, p_pprint_logical_block_tabular_form, gensym, cons);
	check = callclang_funcall(ptr, &code, code, NULL);
	free_check_control(ptr, control, check);
}

_g int pprint_tabular_common(Execute ptr,
		addr stream, addr list, int colon, fixnum size)
{
	/* (defun pprint-tabular (stream list &optional (colon t) atsign (tabsize 16))
	 *   (declare (ignore atsign))
	 *   (pprint-logical-block
	 *     (stream list :prefix (if colon "(" "") :suffix (if colon ")" ""))
	 *     (pprint-exit-if-list-exhausted)
	 *     (loop (write (pprint-pop) :stream stream)
	 *           (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space stream)
	 *           (pprint-tab :section-relative 0 tabsize stream)
	 *           (pprint-newline :fill stream))))
	 */
	addr prefix, suffix, lambda, cons;

	/* make-pprint-stream */
	prefix = suffix = Nil;
	if (colon) {
		strvect_char_heap(&prefix, "(");
		strvect_char_heap(&suffix, ")");
	}
	open_pretty_stream(ptr, &stream, stream, list, prefix, Nil, suffix);
	/* closure */
	fixnum_heap(&cons, size);
	cons_heap(&cons, stream, cons);
	/* function */
	compiled_heap(&lambda, Nil);
	setcompiled_empty(lambda, p_pprint_logical_block_tabular);
	SetDataFunction(lambda, cons);
	/* call */
	return call_pretty_stream(ptr, stream, lambda);
}


/*
 *  pprint-vector
 */
static void pprint_logical_block_vector_form(Execute ptr)
{
	addr cons, stream, pos, vector;
	size_t size, i;

	getdata_control(ptr, &cons);
	GetCons(cons, &stream, &vector);
	Check(! pretty_stream_p(stream), "type error");
	Check(! array_vector_p(vector), "type error");
	if (check_pretty_stream(ptr, stream))
		return;
	/* body */
	size = length_sequence(vector, 1);
	if (size == 0)
		return;
	i = 0;
	for (;;) {
		if (pprint_pop_common(ptr, stream, &pos))
			return;
		vector_get(vector, i, &pos);
		if (write_print(ptr, stream, pos))
			return;
		i++;
		if (size <= i)
			return;
		write_char_stream(stream, ' ');
		pprint_newline_common(ptr, pprint_newline_fill, stream);
	}
}

static void pprint_logical_block_vector(Execute ptr)
{
	int check;
	addr cons, stream, control, code, gensym;

	/* stream */
	getdata_control(ptr, &cons);
	GetCar(cons, &stream);
	Check(! pretty_stream_p(stream), "type error");
	/* unwind-protect */
	push_finalize_control(ptr, &control);
	syscall_code(ptr->local, &code, p_pprint_logical_block_close, stream);
	setfinalize_control(ptr, control, code);
	/* code */
	gensym_pretty_stream(stream, &gensym);
	catch_syscall_code(&code, p_pprint_logical_block_vector_form, gensym, cons);
	check = callclang_funcall(ptr, &code, code, NULL);
	free_check_control(ptr, control, check);
}

_g int vector_default_dispatch(Execute ptr, addr stream, addr pos)
{
	/* (defun pprint-vector (*standard-output* v)
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
	open_pretty_stream(ptr, &stream, stream, Nil, prefix, Nil, suffix);
	/* closure */
	cons_heap(&cons, stream, pos);
	/* function */
	compiled_heap(&lambda, Nil);
	setcompiled_empty(lambda, p_pprint_logical_block_vector);
	SetDataFunction(lambda, cons);
	/* call */
	return call_pretty_stream(ptr, stream, lambda);
}


/*
 *  pprint-defun
 */
_g int defun_default_dispatch(Execute ptr, addr stream, addr pos)
{
	/* (defun pprint-defun (stream list)
	 *   (pprint-logical-block (stream list :prefix "(" :suffix ")")
	 *     ;; defun
	 *     (write (pprint-pop) :stream stream)
	 *     (pprint-exit-if-list-exhausted)
	 *     (write-char #\Space stream)
	 *     ;; name
	 *     (pprint-newline :miser stream)
	 *     (pprint-indent :current 0 stream)
	 *     (write (pprint-pop) :stream stream)
	 *     (pprint-exit-if-list-exhausted)
	 *     (write-char #\Space stream)
	 *     ;; args
	 *     (pprint-logical-block (stream (pprint-pop) :prefix "(" :suffix ")")
	 *       (pprint-exit-if-list-exhausted)
	 *       (loop (pprint-logical-block (stream (pprint-pop) :prefix "(" :suffix ")")
	 *               (pprint-exit-if-list-exhausted)
	 *               (loop (write (pprint-pop) :stream stream)
	 *                     (pprint-exit-if-list-exhausted)
	 *                     (write-char #\Space stream)
	 *                     (pprint-newline :linear stream)))
	 *             (pprint-exit-if-list-exhausted)
	 *             (write-char #\Space stream)
	 *             (pprint-newline :fill stream)))
	 *     ;; body
	 *     (pprint-indent :block 1 stream)
	 *     (loop (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space stream)
	 *           (pprint-newline :linear stream)
	 *           (write (pprint-pop) :stream stream))))
	 */
	return 0;
}


/*
 *  pprint-let
 */
_g int let_default_dispatch(Execute ptr, addr stream, addr pos)
{
	/* (defun pprint-let (stream list)
	 *   (pprint-logical-block (stream list :prefix "(" :suffix ")")
	 *     ;; let
	 *     (write (pprint-pop) :stream stream)
	 *     (pprint-exit-if-list-exhausted)
	 *     (write-char #\Space stream)
	 *     ;; args
	 *     (pprint-logical-block (stream (pprint-pop) :prefix "(" :suffix ")")
	 *       (pprint-exit-if-list-exhausted)
	 *       (loop (pprint-logical-block (stream (pprint-pop) :prefix "(" :suffix ")")
	 *               (pprint-exit-if-list-exhausted)
	 *               (loop (write (pprint-pop) :stream stream)
	 *                     (pprint-exit-if-list-exhausted)
	 *                     (write-char #\Space stream)
	 *                     (pprint-newline :linear stream)))
	 *             (pprint-exit-if-list-exhausted)
	 *             (write-char #\Space stream)
	 *             (pprint-newline :fill stream)))
	 *     ;; body
	 *     (pprint-indent :block 1 stream)
	 *     (loop (pprint-exit-if-list-exhausted)
	 *           (write-char #\Space stream)
	 *           (pprint-newline :linear stream)
	 *           (write (pprint-pop) :stream stream))))
	 */
	return 0;
}


/*
 *  initialize
 */
_g void init_print_function(void)
{
	SetPointerType(empty, pprint_logical_block_close);
	SetPointerType(empty, pprint_logical_block_fill);
	SetPointerType(empty, pprint_logical_block_fill_form);
	SetPointerType(empty, pprint_logical_block_linear);
	SetPointerType(empty, pprint_logical_block_linear_form);
	SetPointerType(empty, pprint_logical_block_tabular);
	SetPointerType(empty, pprint_logical_block_tabular_form);
	SetPointerType(empty, pprint_logical_block_vector);
	SetPointerType(empty, pprint_logical_block_vector_form);
}

