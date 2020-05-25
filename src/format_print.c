#include "character.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "format.h"
#include "format_parse.h"
#include "format_print.h"
#include "hold.h"
#include "print_pretty.h"
#include "stream.h"
#include "stream_string.h"
#include "stream_pretty.h"
#include "strtype.h"

/*
 *  format execute
 */
_g void fmtprint_abort(fmtprint print, size_t index, const char *str, ...)
{
	va_list args;

	va_start(args, str);
	format_abort(print->format, index, str, args);
	va_end(args);
}

_g void fmtprop_abort(fmtprint print,
		struct format_operator *fmt, const char *str, ...)
{
	va_list args;

	va_start(args, str);
	format_abort(print->format, fmt->position, str, args);
	va_end(args);
}

_g void fmtprint_make(fmtprint print, Execute ptr, addr stream, addr format)
{
	clearpoint(print);
	print->loop = 1;
	print->first = 1;
	print->conversion = fmtcase_normal;
	print->ptr = ptr;
	print->local = ptr->local;
	print->stream = stream;
	print->format = format;
}

_g void fmtprint_copy(fmtprint print, fmtprint src)
{
	fmtprint_make(print, src->ptr, src->stream, src->format);
	print->string = src->string;
}

_g addr fmtprint_make_string(fmtprint print, addr *ret)
{
	addr backup_stream, src, stream;

	backup_stream = print->string;
	src = print->stream;
	open_output_string_stream(&stream, 0);
	gchold_push_local(print->local, stream);
	copyleft_stream(stream, src);
	copy_terminal_width_string_stream(stream, src);
	if (pretty_stream_p(src))
		set_pretty_output_string_stream(stream);
	print->string = stream;
	*ret = stream;

	return backup_stream;
}

_g void fmtprint_stream(fmtprint print, addr *ret)
{
	addr stream;

	stream = print->string;
	Check(! output_string_stream_p(stream), "string-stream error");
	clear_output_string_stream(stream);
	copyleft_stream(stream, print->stream);
	*ret = stream;
}

_g void fmtprint_stream_output(fmtprint print)
{
	addr pos, stream;

	stream = print->string;
	Check(! output_string_stream_p(stream), "string-stream error");
	string_stream_local(print->local, stream, &pos);
	clear_output_string_stream(stream);
	fmtprint_string(print, pos);
}

_g struct format_operator *fmtprint_operator(fmtprint print)
{
	return (struct format_operator *)
		(print->now + (byte *)format_pointer(print->format));
}


/*
 *  putc
 */
static void fmtprint_char(fmtprint print, unicode u)
{
	addr stream;

	stream = print->stream;
	if (print->pretty == 0)
		goto output;
	if (print->fill == 0)
		goto output;

	if (u == ' ') {
		print->fill_white = 1;
		goto output;
	}
	if (print->fill_white && print->fill_ignore == 0)
		pprint_newline_print(print->ptr, pprint_newline_fill, stream);
	print->fill_white = 0;
	print->fill_ignore = 0;

output:
	write_char_stream(stream, u);
}

_g void fmtprint_putc(fmtprint print, unicode u)
{
	switch (print->conversion) {
		case fmtcase_upcase:
			u = toUpperUnicode(u);
			break;

		case fmtcase_downcase:
			u = toLowerUnicode(u);
			break;

		case fmtcase_capitalize_all:
			if (print->word == 0 && isAlphanumeric(u))
				u = toUpperUnicode(u);
			else
				u = toLowerUnicode(u);
			break;

		case fmtcase_capitalize_first:
			if (print->word == 0 && print->first && isAlphanumeric(u)) {
				u = toUpperUnicode(u);
				print->first = 0;
			}
			else
				u = toLowerUnicode(u);
			break;

		case fmtcase_normal:
		default:
			break;
	}

	print->word = isAlphanumeric(u);
	fmtprint_char(print, u);
}

_g void fmtprint_putc_times(fmtprint print, unicode c, size_t size)
{
	for (; size; size--)
		fmtprint_putc(print, c);
}

_g void fmtprint_string(fmtprint print, addr string)
{
	unicode u;
	size_t size, i;

	string_length(string, &size);
	for (i = 0; i < size; i++) {
		string_getc(string, i, &u);
		fmtprint_putc(print, u);
	}
}


/*
 *  pop
 */
static void fmtprint_pop_error(fmtprint print, struct format_operator *str,
		addr list, addr *car, addr *cdr, size_t n)
{
	addr pos;
	size_t i;

	for (i = 0; i < n; i++) {
		if (! consp(list)) {
			fmtprop_abort(print, str, "Too few format arguments.", NULL);
			return;
		}
		getcons(list, &pos, &list);
	}
	if (car)
		*car = pos;
	*cdr = list;
}

static int fmtprint_pop1(fmtprint print, struct format_operator *str, addr *ret)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	fmtprint_pop_error(print, str, ptr->front, ret, &(ptr->front), 1);
	ptr->index++;

	return 0;
}

static int fmtprint_pop2(fmtprint print, struct format_operator *str, addr *ret)
{
	addr stream;

	stream = print->stream;
	Check(! pretty_stream_p(stream), "pretty stream error");
	return pprint_pop_common(print->ptr, stream, ret);
}

_g int fmtprint_pop(fmtprint print, struct format_operator *str, addr *ret)
{
	addr pos;

	if (print->pretty == 0) {
		return fmtprint_pop1(print, str, ret);
	}
	else {
		Return(fmtprint_pop2(print, str, ret));
		return fmtprint_pop1(print, str, &pos);
	}
}


/*
 *  peek
 */
_g void fmtprint_peek(fmtprint print, struct format_operator *str, addr *ret)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	if (ptr->front == Nil) {
		fmtprop_abort(print, str, "Too few format arguments.", NULL);
		return;
	}
	getcar(ptr->front, ret);
}


/*
 *  forward
 */
static void fmtprint_forward1(fmtprint print, struct format_operator *str, size_t n)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	fmtprint_pop_error(print, str, ptr->front, NULL, &(ptr->front), n);
	ptr->index += n;
}

static void fmtprint_forward2(fmtprint print, struct format_operator *str, size_t n)
{
	addr stream, list;

	stream = print->stream;
	Check(! pretty_stream_p(stream), "pretty stream error");

	root_pretty_stream(stream, &list);
	fmtprint_pop_error(print, str, list, NULL, &list, n);
	setroot_pretty_stream(stream, list);
}

_g void fmtprint_forward(fmtprint print, struct format_operator *str, size_t n)
{
	if (n == 0)
		return;
	fmtprint_forward1(print, str, n);
	if (print->pretty)
		fmtprint_forward2(print, str, n);
}


/*
 *  rollback
 */
_g void fmtprint_rollback1(fmtprint print, struct format_operator *str, size_t n)
{
	struct fmtstack *rest;
	size_t size;

	rest = print->rest;
	size = rest->index;
	if (size < n) {
		fmtprop_abort(print, str, "Cannot rollback ~~:*.", NULL);
		return;
	}
	rest->front = rest->root;
	rest->index = 0;
	size -= n;
	fmtprint_pop_error(print, str, rest->front, NULL, &(rest->front), size);
	rest->index = size;
}

_g void fmtprint_rollback2(fmtprint print, struct format_operator *str, size_t n)
{
	addr stream;

	stream = print->stream;
	Check(! pretty_stream_p(stream), "pretty stream error");
	setroot_pretty_stream(stream, print->rest->front);
}

_g void fmtprint_rollback(fmtprint print, struct format_operator *str, size_t n)
{
	if (n == 0)
		return;
	fmtprint_rollback1(print, str, n);
	if (print->pretty)
		fmtprint_rollback2(print, str, n);
}


/*
 *  absolute
 */
_g void fmtprint_absolute(fmtprint print, struct format_operator *str, size_t n)
{
	size_t now;

	now = print->rest->index;
	if (now < n)
		fmtprint_forward(print, str, n - now);
	else
		fmtprint_rollback(print, str, now - n);
}


/*
 *  clear
 */
static void fmtprint_clear1(fmtprint print)
{
	struct fmtstack *rest;

	rest = print->rest;
	rest->root = Nil;
	rest->front = Nil;
	rest->index = 0;
}

static void fmtprint_clear2(fmtprint print)
{
	addr stream;

	stream = print->stream;
	Check(! pretty_stream_p(stream), "pretty stream error");
	setroot_pretty_stream(stream, Nil);
}

_g void fmtprint_clear(fmtprint print)
{
	fmtprint_clear1(print);
	if (print->pretty)
		fmtprint_clear2(print);
}

