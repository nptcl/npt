#include "character.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "format.h"
#include "format_parse.h"
#include "format_print.h"
#include "gc.h"
#include "print_pretty.h"
#include "stream.h"
#include "stream_string.h"
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
	copy_terpri_position_stream(stream, src);
	copy_terminal_width_string_stream(stream, src);
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
	copy_terpri_position_stream(stream, print->stream);
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

static void fmtprint_char(fmtprint print, unicode u)
{
	addr stream;

	stream = print->stream;
	if (print->pretty && print->fill) {
		if (print->fill_check) {
			if (u != ' ')
				print->fill_check = 0;
		}
		else {
			if (u == ' ') {
				pprint_newline_common(print->ptr, pprint_newline_fill, stream);
				print->fill_check = 1;
			}
		}
	}
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

_g void fmtprint_pop(fmtprint print, struct format_operator *str, addr *ret)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	if (ptr->front == Nil) {
		fmtprop_abort(print, str, "Too few format arguments.", NULL);
		return;
	}
	getcons(ptr->front, ret, &(ptr->front));
	ptr->index++;
}

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

_g void fmtprint_forward(fmtprint print, struct format_operator *str, size_t count)
{
	addr front;
	size_t i;
	struct fmtstack *ptr;

	ptr = print->rest;
	front = ptr->front;
	for (i = 0; i < count; i++) {
		if (front == Nil) {
			fmtprop_abort(print, str, "Too few format arguments.", NULL);
			return;
		}
		getcdr(front, &front);
		ptr->index++;
	}
	ptr->front = front;
}

_g void fmtprint_rollback(fmtprint print, struct format_operator *str, size_t count)
{
	addr root;
	size_t size;
	struct fmtstack *ptr;

	ptr = print->rest;
	root = ptr->root;
	size = ptr->index;
	if (size < count) {
		fmtprop_abort(print, str, "Cannot rollback ~~:*.", NULL);
		return;
	}
	size -= count;
	ptr->index = size;
	getnthcdr_unsafe(root, size, &root);
	ptr->front = root;
}

_g void fmtprint_absolute(fmtprint print, struct format_operator *str, size_t count)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	ptr->front = ptr->root;
	ptr->index = 0;
	fmtprint_forward(print, str, count);
}

