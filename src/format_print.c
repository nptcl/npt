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
_g int fmtprint_abort_(fmtprint print, size_t index, const char *str, ...)
{
	va_list args;

	va_start(args, str);
	Return(format_abort_(print->format, index, str, args));
	va_end(args);

	return 0;
}

_g int fmtprop_abort_(fmtprint print,
		struct format_operator *fmt, const char *str, ...)
{
	va_list args;

	va_start(args, str);
	Return(format_abort_(print->format, fmt->position, str, args));
	va_end(args);

	return 0;
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

_g int fmtprint_make_string_(fmtprint print, addr *ret, addr *backup)
{
	addr backup_stream, src, stream;

	backup_stream = print->string;
	src = print->stream;
	open_output_string_stream(&stream, 0);
	gchold_push_local(print->local, stream);
	Return(copyleft_stream_(stream, src));
	Return(copy_termsize_string_stream_(stream, src));
	if (pretty_stream_p(src))
		set_pretty_output_string_stream(stream);
	print->string = stream;
	*ret = stream;
	*backup = backup_stream;

	return 0;
}

_g int fmtprint_stream_(fmtprint print, addr *ret)
{
	addr stream;

	stream = print->string;
	Check(! output_string_stream_p(stream), "string-stream error");
	clear_output_string_stream(stream);
	Return(copyleft_stream_(stream, print->stream));
	return Result(ret, stream);
}

_g int fmtprint_stream_output_(fmtprint print)
{
	addr pos, stream;

	stream = print->string;
	Check(! output_string_stream_p(stream), "string-stream error");
	string_stream_local(print->local, stream, &pos);
	clear_output_string_stream(stream);
	return fmtprint_string_(print, pos);
}

_g struct format_operator *fmtprint_operator(fmtprint print)
{
	return (struct format_operator *)
		(print->now + (byte *)format_pointer(print->format));
}


/*
 *  putc
 */
static int fmtprint_char_(fmtprint print, unicode u)
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
	return write_char_stream_(stream, u);
}

_g int fmtprint_putc_(fmtprint print, unicode u)
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
	return fmtprint_char_(print, u);
}

_g int fmtprint_putc_times_(fmtprint print, unicode c, size_t size)
{
	for (; size; size--) {
		Return(fmtprint_putc_(print, c));
	}

	return 0;
}

_g int fmtprint_string_(fmtprint print, addr string)
{
	unicode u;
	size_t size, i;

	string_length(string, &size);
	for (i = 0; i < size; i++) {
		string_getc(string, i, &u);
		Return(fmtprint_putc_(print, u));
	}

	return 0;
}


/*
 *  pop
 */
static int fmtprint_pop_error_(fmtprint print, struct format_operator *str,
		addr list, addr *car, addr *cdr, size_t n)
{
	addr pos;
	size_t i;

	for (i = 0; i < n; i++) {
		if (! consp(list))
			return fmtprop_abort_(print, str, "Too few format arguments.", NULL);
		Return_getcons(list, &pos, &list);
	}
	if (car)
		*car = pos;
	*cdr = list;

	return 0;
}

static int fmtprint_pop1_(fmtprint print, struct format_operator *str, addr *ret)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	Return(fmtprint_pop_error_(print, str, ptr->front, ret, &(ptr->front), 1));
	ptr->index++;

	return 0;
}

static int fmtprint_pop2_(fmtprint print, struct format_operator *str, addr *ret)
{
	addr stream;

	stream = print->stream;
	Check(! pretty_stream_p(stream), "pretty stream error");
	return pprint_pop_common(print->ptr, stream, ret);
}

_g int fmtprint_pop_(fmtprint print, struct format_operator *str, addr *ret)
{
	addr pos;

	if (print->pretty == 0) {
		return fmtprint_pop1_(print, str, ret);
	}
	else {
		Return(fmtprint_pop2_(print, str, ret));
		return fmtprint_pop1_(print, str, &pos);
	}
}


/*
 *  peek
 */
_g int fmtprint_peek_(fmtprint print, struct format_operator *str, addr *ret)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	if (ptr->front == Nil)
		return fmtprop_abort_(print, str, "Too few format arguments.", NULL);
	Return_getcar(ptr->front, ret);

	return 0;
}


/*
 *  forward
 */
static int fmtprint_forward1_(fmtprint print, struct format_operator *str, size_t n)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	Return(fmtprint_pop_error_(print, str, ptr->front, NULL, &(ptr->front), n));
	ptr->index += n;

	return 0;
}

static int fmtprint_forward2_(fmtprint print, struct format_operator *str, size_t n)
{
	addr stream, list;

	stream = print->stream;
	Check(! pretty_stream_p(stream), "pretty stream error");

	root_pretty_stream(stream, &list);
	Return(fmtprint_pop_error_(print, str, list, NULL, &list, n));
	setroot_pretty_stream(stream, list);

	return 0;
}

_g int fmtprint_forward_(fmtprint print, struct format_operator *str, size_t n)
{
	if (n == 0)
		return 0;
	Return(fmtprint_forward1_(print, str, n));
	if (print->pretty) {
		Return(fmtprint_forward2_(print, str, n));
	}

	return 0;
}


/*
 *  rollback
 */
static int fmtprint_rollback1_(fmtprint print, struct format_operator *str, size_t n)
{
	struct fmtstack *rest;
	size_t size;

	rest = print->rest;
	size = rest->index;
	if (size < n)
		return fmtprop_abort_(print, str, "Cannot rollback ~~:*.", NULL);
	rest->front = rest->root;
	rest->index = 0;
	size -= n;
	Return(fmtprint_pop_error_(print, str, rest->front, NULL, &(rest->front), size));
	rest->index = size;

	return 0;
}

static void fmtprint_rollback2(fmtprint print, struct format_operator *str, size_t n)
{
	addr stream;

	stream = print->stream;
	Check(! pretty_stream_p(stream), "pretty stream error");
	setroot_pretty_stream(stream, print->rest->front);
}

_g int fmtprint_rollback_(fmtprint print, struct format_operator *str, size_t n)
{
	if (n == 0)
		return 0;
	Return(fmtprint_rollback1_(print, str, n));
	if (print->pretty)
		fmtprint_rollback2(print, str, n);
	
	return 0;
}


/*
 *  absolute
 */
_g int fmtprint_absolute_(fmtprint print, struct format_operator *str, size_t n)
{
	size_t now;

	now = print->rest->index;
	if (now < n)
		return fmtprint_forward_(print, str, n - now);
	else
		return fmtprint_rollback_(print, str, now - n);
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

