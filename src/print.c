#include <stdarg.h>
#include "array.h"
#include "array_object.h"
#include "bignum.h"
#include "bit.h"
#include "bytespec.h"
#include "character.h"
#include "cmpl.h"
#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "execute.h"
#include "fmtfloat.h"
#include "function.h"
#include "hashtable.h"
#include "heap.h"
#include "integer.h"
#include "package.h"
#include "pathname.h"
#include "print.h"
#include "quote.h"
#include "random_state.h"
#include "ratio.h"
#include "readtable.h"
#include "real_float.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"
#include "structure.h"
#include "symbol.h"
#include "type_name.h"
#include "unicode.h"

#define PRINT_STREAM_SIZE		64

typedef int (*calltype_write_print)(struct PrintFormat *, addr, addr);
static calltype_write_print call_write_print[LISPTYPE_SIZE];

static int print_unreadable_object(struct PrintFormat *format,
		addr stream, addr pos, int type, int identity,
		calltype_write_print call)
{
	/* #<type stream identity> */
	int body, first;
	char buffer[32];
	addr str, name;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	size_t size;

	/* body */
	ptr = format->ptr;
	local = NULL;
	if (call == NULL) {
		body = 0;
	}
	else {
		local = ptr->local;
		push_local(local, &stack);
		open_output_string_stream(&str, 0);
		if (call(format, str, pos)) return 1;
		string_stream_local(local, str, &str);
		string_length(str, &size);
		body = (size != 0);
	}

	/* output */
	write_char_stream(stream, '#');
	write_char_stream(stream, '<');
	first = 1;
	if (type) {
		type_name(&name, pos);
		if (princ_print(ptr, stream, name)) return 1;
		first = 0;
	}
	if (body) {
		if (first == 0)
			write_char_stream(stream, ' ');
		print_string_stream(stream, str);
		first = 0;
	}
	if (identity) {
		if (first == 0)
			write_char_stream(stream, ' ');
		snprintf(buffer, 32, "%p", (void *)pos);
		print_ascii_stream(stream, buffer);
	}
	write_char_stream(stream, '>');
	if (local)
		rollback_local(local, stack);

	return 0;
}


/*
 *  clos, structure
 */
static int write_structure(struct PrintFormat *format, addr stream, addr pos)
{
	/* TODO: structure */
	print_ascii_stream(stream, "#S(...)");
	return 0;
}

static int write_clos_body(struct PrintFormat *format, addr stream, addr pos)
{
	if (clos_class_p(pos)) {
		stdget_class_name(pos, &pos);
		return write_print(format, stream, pos);
	}
	else {
		return 0;
	}
}

static int write_clos(struct PrintFormat *format, addr stream, addr pos)
{
	if (structure_instance_p(pos))
		return write_structure(format, stream, pos);
	if (clos_class_p(pos))
		return print_unreadable_object(format, stream, pos, 1, 0, write_clos_body);
	else
		return print_unreadable_object(format, stream, pos, 1, 1, NULL);
}


/*
 *  cons
 */
static int write_cons(struct PrintFormat *format, addr stream, addr right)
{
	fixnum level;
	addr left;
	size_t i, len;

	level = format->level;
	len = (size_t)format->length;
	if (0 <= level && level <= format->now) {
		write_char_stream(stream, '#');
		return 0;
	}
	format->now++;
	write_char_stream(stream, '(');

	for (i = 0; ; i++) {
		if (len <= i) {
			print_ascii_stream(stream, "...");
			break;
		}

		GetCons(right, &left, &right);
		if (write_print(format, stream, left)) return 1;
		if (right == Nil) {
			break;
		}
		if (GetType(right) == LISPTYPE_CONS) {
			write_char_stream(stream, ' ');
		}
		else {
			print_ascii_stream(stream, " . ");
			if (write_print(format, stream, right)) return 1;
			break;
		}
	}

	format->now--;
	write_char_stream(stream, ')');

	return 0;
}


/*
 *  array
 */
static int recursive_write_array(struct PrintFormat *format, addr stream, addr pos,
		const size_t *data, size_t limit, size_t depth, size_t size)
{
	fixnum level;
	LocalRoot local;
	LocalStack stack;
	size_t i, len, length;

	/* restrict */
	level = format->level;
	len = (size_t)format->length;
	if (0 <= level && level <= format->now) {
		write_char_stream(stream, '#');
		return 0;
	}

	/* output */
	if (depth < limit) {
		size = depth? (size * data[depth]): 0;
		length = data[depth];
		format->now++;
		write_char_stream(stream, '(');
		for (i = 0; i < length; i++) {
			if (len <= i) {
				print_ascii_stream(stream, "...");
				break;
			}
			if (i != 0)
				write_char_stream(stream, ' ');
			if (recursive_write_array(format, stream, pos,
						data, limit, depth + 1, size + i))
				return 1;
		}
		format->now--;
		write_char_stream(stream, ')');
	}
	else {
		local = format->ptr->local;
		push_local(local, &stack);
		array_get(local, pos, size, &pos);
		if (write_print(format, stream, pos)) return 1;
		rollback_local(local, stack);
	}

	return 0;
}

static int write_array_vector(struct PrintFormat *format, addr stream, addr pos)
{
	struct array_struct *str;
	LocalRoot local;
	LocalStack stack;
	addr temp;
	fixnum level;
	size_t size, i, len;

	/* restrict */
	level = format->level;
	len = (size_t)format->length;
	if (0 <= level && level <= format->now) {
		write_char_stream(stream, '#');
		return 0;
	}

	str = ArrayInfoStruct(pos);
	size = str->front;
	local = format->ptr->local;
	format->now++;
	write_char_stream(stream, '(');
	for (i = 0; i < size; i++) {
		if (len <= i) {
			print_ascii_stream(stream, "...");
			break;
		}
		if (i != 0)
			write_char_stream(stream, ' ');
		push_local(local, &stack);
		array_get(local, pos, i, &temp);
		if (write_print(format, stream, temp)) return 1;
		rollback_local(local, stack);
	}
	format->now--;
	write_char_stream(stream, ')');

	return 0;
}

static int write_array(struct PrintFormat *format, addr stream, addr pos)
{
	struct array_struct *str;
	const size_t *data;
	size_t dimension;

	str = ArrayInfoStruct(pos);
	write_char_stream(stream, '#');
	dimension = str->dimension;
	if (dimension == 1) {
		write_char_stream(stream, 'A');
		return write_array_vector(format, stream, pos);
	}
	else {
		if (write_print(format, stream, intsizeh(dimension))) return 1;
		write_char_stream(stream, 'A');
		data = array_ptrsize(pos);
		return recursive_write_array(format, stream, pos, data, dimension, 0, 0);
	}

	return 0;
}


/*
 *  vector
 */
static int write_vector(struct PrintFormat *format, addr stream, addr pos)
{
	fixnum level;
	addr temp;
	size_t size, i, len;

	/* restrict */
	level = format->level;
	len = (size_t)format->length;
	if (0 <= level && level <= format->now) {
		write_char_stream(stream, '#');
		return 0;
	}

	/* output */
	format->now++;
	print_ascii_stream(stream, "#(");
	lenarray(pos, &size);
	for (i = 0; i < size; i++) {
		if (len <= i) {
			print_ascii_stream(stream, "...");
			break;
		}
		if (i != 0)
			write_char_stream(stream, ' ');
		getarray(pos, i, &temp);
		if (write_print(format, stream, temp)) return 1;
	}
	format->now--;
	write_char_stream(stream, ')');

	return 0;
}


/*
 *  character
 */
static void write_fixnum_value(addr stream, fixnum value, unsigned base)
{
	/* zero */
	if (value == 0) {
		write_char_stream(stream, '0');
		return;
	}

	/* output */
	if (value < 0)
		write_char_stream(stream, '-');
	output_nosign_fixnum(Local_Thread, stream, value, base, 1);
}

static void write_character_name(addr stream, unicode u)
{
	if (isStandardType(u)) {
		write_char_stream(stream, u);
	}
	else {
		write_char_stream(stream, 'u');
		write_fixnum_value(stream, (fixnum)u, 16);
	}
}

static void write_character_string(addr stream, addr string)
{
	const unicode *body;
	size_t i, size;

	string_posbodylen(string, &body, &size);
	for (i = 0; i < size; i++)
		write_char_stream(stream, body[i]);
}

static int write_character(struct PrintFormat *format, addr stream, addr object)
{
	addr pos;
	unicode u;

	if (! format->escape) {
		GetCharacter(object, &u);
		write_char_stream(stream, u);
		return 0;
	}

	if (findtable_char_name(&pos, object)) {
		/* not found */
		print_ascii_stream(stream, "#\\");
		GetCharacter(object, &u);
		write_character_name(stream, u);
	}
	else {
		/* found */
		print_ascii_stream(stream, "#\\");
		write_character_string(stream, pos);
	}

	return 0;
}


/*
 *  string
 */
static int write_strtype(struct PrintFormat *format, addr stream, addr object)
{
	const unicode *body;
	unicode c;
	size_t size, i;

	string_posbodylen(object, &body, &size);
	if (format->escape) {
		write_char_stream(stream, '\"');
		for (i = 0; i < size; i++) {
			c = body[i];
			if (c == '\"' || c == '\\')
				write_char_stream(stream, '\\');
			write_char_stream(stream, c);
		}
		write_char_stream(stream, '\"');
	}
	else {
		for (i = 0; i < size; i++)
			write_char_stream(stream, body[i]);
	}

	return 0;
}


/*
 *  hash-table
 */
static int write_hashtable_body(struct PrintFormat *format, addr stream, addr pos)
{
	addr test, value;
	Execute ptr;

	GetConst(KEYWORD_TEST, &test);
	gettest_symbol_hashtable(pos, &value);
	ptr = format->ptr;
	if (prin1_print(ptr, stream, test)) return 1;
	write_char_stream(stream, ' ');
	if (prin1_print(ptr, stream, value)) return 1;

	return 0;
}

static int write_hashtable(struct PrintFormat *format, addr stream, addr pos)
{
	return print_unreadable_object(format, stream, pos, 1, 1, write_hashtable_body);
}


/*
 *  symbol
 */
static void direct_norm(addr stream, addr pos)
{
	size_t i, size;
	unicode u;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		write_char_stream(stream, u);
	}
}

static void downcase_norm(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		write_char_stream(stream, toLowerUnicode(u));
	}
}

static void upcase_norm(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		write_char_stream(stream, toUpperUnicode(u));
	}
}

static void up_cap_norm(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (isAlphanumeric(u)) {
			if (check) {
				write_char_stream(stream, u);
				check = 0;
			}
			else {
				write_char_stream(stream, toLowerUnicode(u));
			}
		}
		else {
			write_char_stream(stream, u);
			check = 1;
		}
	}
}

static void down_cap_norm(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (isAlphanumeric(u)) {
			if (check) {
				write_char_stream(stream, toUpperUnicode(u));
				check = 0;
			}
			else {
				write_char_stream(stream, u);
			}
		}
		else {
			write_char_stream(stream, u);
			check = 1;
		}
	}
}

static enum PrintCase check_invert(addr pos)
{
	enum PrintCase check;
	unicode u;
	size_t i, size;

	GetNameSymbol(pos, &pos);
	string_length(pos, &size);
	check = PrintCase_unread;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (isUpperCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_upcase;
			}
			else if (check != PrintCase_upcase) {
				return PrintCase_preserve;
			}
		}
		else if (isLowerCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_downcase;
			}
			else if (check != PrintCase_downcase) {
				return PrintCase_preserve;
			}
		}
	}

	return check;
}

static void invert_norm(addr stream, addr pos)
{
	switch (check_invert(pos)) {
		case PrintCase_upcase:
			downcase_norm(stream, pos);
			break;

		case PrintCase_downcase:
			upcase_norm(stream, pos);
			break;

		default:
			direct_norm(stream, pos);
			break;
	}
}

static int check_escape(unicode c)
{
	return (c == ' ')
		|| (c == '`') || (c == ',')
		|| (c == '(') || (c == ')')
		|| (c == '|') || (c == '\\')
		|| (c == ':') || (c == ';')
		|| (c == '\'') || (c == '"');
}

static void direct_escape(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (u == '\\' || u == '|')
			write_char_stream(stream, '\\');
		write_char_stream(stream, u);
	}
}

static int check_upcase_escape(addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (check_escape(u)) return 1;
		if (isLowerCase(u)) return 1;
	}

	return 0;
}

static void up_up_output(addr stream, addr pos)
{
	if (check_upcase_escape(pos)) {
		write_char_stream(stream, '|');
		direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		direct_escape(stream, pos);
	}
}

static void downcase_escape(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		write_char_stream(stream, toLowerUnicode(u));
	}
}

static void up_down_output(addr stream, addr pos)
{
	if (check_upcase_escape(pos)) {
		write_char_stream(stream, '|');
		direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		downcase_escape(stream, pos);
	}
}

static void capitalize_escape(addr stream, addr pos)
{
	int check;
	unicode u;
	size_t i, size;

	check = 1;
	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (isAlphanumeric(u)) {
			if (check) {
				write_char_stream(stream, toUpperUnicode(u));
				check = 0;
			}
			else {
				write_char_stream(stream, toLowerUnicode(u));
			}
		}
		else {
			write_char_stream(stream, u);
			check = 1;
		}
	}
}

static void up_cap_output(addr stream, addr pos)
{
	if (check_upcase_escape(pos)) {
		write_char_stream(stream, '|');
		direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		capitalize_escape(stream, pos);
	}
}

static void output_escape(struct PrintFormat *format,
		addr stream, addr pos, void (*call)(addr, addr))
{
	addr package, check;

	getpackage(format->ptr, &check);
	GetPackageSymbol(pos, &package);
	if (package == Nil) {
		/* gensym */
		if (format->gensym)
			print_ascii_stream(stream, "#:");
	}
	else if (checksymbol_package(pos, check)) {
		/* no package name */
	}
	else if (keywordp(pos)) {
		print_ascii_stream(stream, ":");
	}
	else if (package != check) {
		/* package name */
		getname_package(package, &package);
		call(stream, package);
		print_ascii_stream(stream, "::");
	}
	/* symbol name */
	GetNameSymbol(pos, &pos);
	call(stream, pos);
}

static int check_downcase_escape(addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (check_escape(u)) return 1;
		if (isUpperCase(u)) return 1;
	}

	return 0;
}

static void upcase_escape(addr stream, addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		write_char_stream(stream, toUpperUnicode(u));
	}
}

static void down_up_output(addr stream, addr pos)
{
	if (check_downcase_escape(pos)) {
		write_char_stream(stream, '|');
		direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		upcase_escape(stream, pos);
	}
}

static void down_down_output(addr stream, addr pos)
{
	if (check_downcase_escape(pos)) {
		write_char_stream(stream, '|');
		direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		direct_escape(stream, pos);
	}
}

static void down_cap_output(addr stream, addr pos)
{
	if (check_downcase_escape(pos)) {
		write_char_stream(stream, '|');
		direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		capitalize_escape(stream, pos);
	}
}

static int check_preserve_escape(addr pos)
{
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (check_escape(u)) return 1;
	}

	return 0;
}

static void preserve_output(addr stream, addr pos)
{
	if (check_preserve_escape(pos)) {
		write_char_stream(stream, '|');
		direct_escape(stream, pos);
		write_char_stream(stream, '|');
	}
	else {
		direct_escape(stream, pos);
	}
}

static enum PrintCase check_invert_escape(addr pos)
{
	enum PrintCase check;
	unicode u;
	size_t i, size;

	string_length(pos, &size);
	check = PrintCase_unread;
	for (i = 0; i < size; i++) {
		string_getc(pos, i, &u);
		if (check_escape(u)) {
			return PrintCase_escape;
		}
		else if (isUpperCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_upcase;
			}
			else if (check != PrintCase_upcase) {
				return PrintCase_preserve;
			}
		}
		else if (isLowerCase(u)) {
			if (check == PrintCase_unread) {
				check = PrintCase_downcase;
			}
			else if (check != PrintCase_downcase) {
				return PrintCase_preserve;
			}
		}
	}

	return check;
}

static void invert_output(addr stream, addr pos)
{
	switch (check_invert_escape(pos)) {
		case PrintCase_upcase:
			downcase_escape(stream, pos);
			break;

		case PrintCase_downcase:
			upcase_escape(stream, pos);
			break;

		case PrintCase_escape:
			write_char_stream(stream, '|');
			direct_escape(stream, pos);
			write_char_stream(stream, '|');
			break;

		default:
			direct_escape(stream, pos);
			break;
	}
}

static void up_up_esc(struct PrintFormat *format, addr stream, addr pos)
{
	output_escape(format, stream, pos, up_up_output);
}

static void up_down_esc(struct PrintFormat *format, addr stream, addr pos)
{
	output_escape(format, stream, pos, up_down_output);
}

static void up_cap_esc(struct PrintFormat *format, addr stream, addr pos)
{
	output_escape(format, stream, pos, up_cap_output);
}

static void down_up_esc(struct PrintFormat *format, addr stream, addr pos)
{
	output_escape(format, stream, pos, down_up_output);
}

static void down_down_esc(struct PrintFormat *format, addr stream, addr pos)
{
	output_escape(format, stream, pos, down_down_output);
}

static void down_cap_esc(struct PrintFormat *format, addr stream, addr pos)
{
	output_escape(format, stream, pos, down_cap_output);
}

static void preserve_esc(struct PrintFormat *format, addr stream, addr pos)
{
	output_escape(format, stream, pos, preserve_output);
}

static void invert_esc(struct PrintFormat *format, addr stream, addr pos)
{
	output_escape(format, stream, pos, invert_output);
}

static void update_readcase(struct PrintFormat *format)
{
	addr pos;

	GetConst(SPECIAL_READTABLE, &pos);
	getspecialcheck_local(format->ptr, pos, &pos);
	switch (getcase_readtable(pos)) {
		case ReadTable_upcase:
			format->readcase = PrintCase_upcase;
			return;

		case ReadTable_downcase:
			format->readcase = PrintCase_downcase;
			return;

		case ReadTable_preserve:
			format->readcase = PrintCase_preserve;
			return;

		case ReadTable_invert:
			format->readcase = PrintCase_invert;
			return;

		default:
			fmte("Unknown readtable-case type.", NULL);
			break;
	}
}

#define switch_printcase(fmt, up, down, cap) { \
	switch (fmt->printcase) { \
		case PrintCase_upcase: up; break; \
		case PrintCase_downcase: down; break; \
		case PrintCase_capitalize: cap; break; \
		default: fmte("printcase error", NULL); break; \
	}\
};
#define switch_escape(fmt, esc, norm) { \
	if (fmt->escape) { esc; } else { norm; } \
}

static int write_symbol(struct PrintFormat *format, addr stream, addr pos)
{
	if (format->readcase == PrintCase_unread)
		update_readcase(format);
	switch (format->readcase) {
		case PrintCase_upcase:
			switch_printcase(format,
					switch_escape(format,
						up_up_esc(format, stream, pos),
						direct_norm(stream, pos)),
					switch_escape(format,
						up_down_esc(format, stream, pos),
						downcase_norm(stream, pos)),
					switch_escape(format,
						up_cap_esc(format, stream, pos),
						up_cap_norm(stream, pos)));
			break;

		case PrintCase_downcase:
			switch_printcase(format,
					switch_escape(format,
						down_up_esc(format, stream, pos),
						upcase_norm(stream, pos)),
					switch_escape(format,
						down_down_esc(format, stream, pos),
						direct_norm(stream, pos)),
					switch_escape(format,
						down_cap_esc(format, stream, pos),
						down_cap_norm(stream, pos)));
			break;

		case PrintCase_preserve:
			if (format->escape)
				preserve_esc(format, stream, pos);
			else
				direct_norm(stream, pos);
			break;

		case PrintCase_invert:
			if (format->escape)
				invert_esc(format, stream, pos);
			else
				invert_norm(stream, pos);
			break;

		default:
			fmte("readcase error", NULL);
			break;
	}

	return 0;
}


/*
 *  fixnum
 */
static void write_radix_front(addr stream, unsigned base)
{
	char buffer[8];

	Check(! isBaseChar(base), "base error");
	switch (base) {
		case 2:
			print_ascii_stream(stream, "#b");
			break;

		case 8:
			print_ascii_stream(stream, "#o");
			break;

		case 16:
			print_ascii_stream(stream, "#x");
			break;

		default:
			snprintf(buffer, 8, "#%ur", base);
			print_ascii_stream(stream, buffer);
			break;
	}
}

static int write_fixnum(struct PrintFormat *format, addr stream, addr object)
{
	unsigned base, radix;
	fixnum value;

	base = format->base;
	radix = format->radix;
	if (radix && base != 10)
		write_radix_front(stream, base);
	GetFixnum(object, &value);
	write_fixnum_value(stream, value, base);
	if (radix && base == 10)
		write_char_stream(stream, '.');

	return 0;
}


/*
 *  bignum
 */
static void write_bignum_value(addr stream, int sign, addr object, unsigned base)
{
	/* zero */
	if (zerop_bignum(object)) {
		write_char_stream(stream, '0');
		return;
	}

	/* output */
	if (sign)
		write_char_stream(stream, '-');
	output_nosign_bignum(Local_Thread, stream, object, base, 1);
}

static void write_bignum_sign(struct PrintFormat *format,
		addr stream, int sign, addr object)
{
	unsigned base, radix;

	base = format->base;
	radix = format->radix;
	if (radix && base != 10)
		write_radix_front(stream, base);
	write_bignum_value(stream, sign, object, base);
	if (radix && base == 10)
		write_char_stream(stream, '.');
}

static int write_bignum(struct PrintFormat *format, addr stream, addr object)
{
	int sign;
	GetSignBignum(object, &sign);
	write_bignum_sign(format, stream, sign, object);

	return 0;
}


/*
 *  ratio
 */
static int write_ratio(struct PrintFormat *format, addr stream, addr object)
{
	int sign;
	addr check;
	unsigned base;

	/* zero */
	if (zerop_ratio(object)) {
		write_char_stream(stream, '0');
		return 0;
	}

	/* integer */
	GetDenomRatio(object, &check);
	if (equal_value_nosign_bignum(check, 1)) {
		GetSignRatio(object, &sign);
		GetNumerRatio(object, &check);
		write_bignum_sign(format, stream, sign, check);
		return 0;
	}

	/* ratio */
	base = format->base;
	if (format->radix)
		write_radix_front(stream, base);
	GetSignRatio(object, &sign);
	if (sign)
		write_char_stream(stream, '-');
	output_nosign_ratio(Local_Thread, stream, object, base, 1);

	return 0;
}


/*
 *  float
 */
static int write_single_float(struct PrintFormat *format, addr stream, addr object)
{
	int markerp, marker;
	single_float value;

	GetSingleFloat(object, &value);
	markerp = format->readfloat == ReadTable_single;
	marker = markerp? 'E': 'F';
	fmtfloat_princ_single_float(stream, value, markerp, marker);

	return 0;
}

static int write_double_float(struct PrintFormat *format, addr stream, addr object)
{
	int markerp, marker;
	double_float value;

	GetDoubleFloat(object, &value);
	markerp = format->readfloat == ReadTable_double;
	marker = markerp? 'E': 'D';
	fmtfloat_princ_double_float(stream, value, markerp, marker);

	return 0;
}

static int write_long_float(struct PrintFormat *format, addr stream, addr object)
{
	int markerp, marker;
	long_float value;

	GetLongFloat(object, &value);
	markerp = format->readfloat == ReadTable_long;
	marker = markerp? 'E': 'L';
	fmtfloat_princ_long_float(stream, value, markerp, marker);

	return 0;
}


/*
 *  complex
 */
static int write_complex(struct PrintFormat *format, addr stream, addr object)
{
	addr real, imag;

	GetRealComplex(object, &real);
	GetImagComplex(object, &imag);
	print_ascii_stream(stream, "#C(");
	if (write_print(format, stream, real)) return 1;
	write_char_stream(stream, ' ');
	if (write_print(format, stream, imag)) return 1;
	write_char_stream(stream, ')');

	return 0;
}


/*
 *  callname
 */
static int write_callname_body(struct PrintFormat *format, addr stream, addr pos)
{
	LocalRoot local;
	LocalStack stack;

	local = format->ptr->local;
	push_local(local, &stack);
	name_callname_local(local, pos, &pos);
	if (write_print(format, stream, pos)) return 1;
	rollback_local(local, stack);

	return 0;
}

static int write_callname(struct PrintFormat *format, addr stream, addr pos)
{
	return print_unreadable_object(format, stream, pos, 1, 1, write_callname_body);
}


/*
 *  function
 */
static int write_function_body(struct PrintFormat *format, addr stream, addr pos)
{
	const char *name;
	struct function_struct *ptr;

	/* type */
	ptr = StructFunction(pos);
	if (ptr->system)
		name = "SYSTEM-FUNCTION ";
	else if (ptr->compiled)
		name = "COMPILED-FUNCTION ";
	else
		name = "FUNCTION ";
	print_ascii_stream(stream, name);

	/* name */
	GetNameFunction(pos, &pos);
	if (pos == Nil)
		print_ascii_stream(stream, "LAMBDA");
	else {
		if (RefCallNameType(pos) == CALLNAME_SYMBOL) {
			GetCallName(pos, &pos);
			if (princ_print(format->ptr, stream, pos)) return 1;
		}
		else {
			GetCallName(pos, &pos);
			print_ascii_stream(stream, "(SETF ");
			if (princ_print(format->ptr, stream, pos)) return 1;
			print_ascii_stream(stream, ")");
		}
	}

	return 0;
}

static int write_function(struct PrintFormat *format, addr stream, addr pos)
{
	int identity;
	addr name;

	/* #<FUNCTION NAME> */
	GetNameFunction(pos, &name);
	identity = (name == Nil);
	return print_unreadable_object(format,
			stream, pos, 0, identity, write_function_body);
}


/*
 *  index
 */
static int write_index_body(struct PrintFormat *format, addr stream, addr pos)
{
	LocalRoot local;
	LocalStack stack;
	size_t size;

	GetIndex(pos, &size);
	local = format->ptr->local;
	push_local(local, &stack);
	if (write_print(format, stream, intsizea(local, size))) return 1;
	rollback_local(local, stack);

	return 0;
}

static int write_index(struct PrintFormat *format, addr stream, addr pos)
{
	return print_unreadable_object(format, stream, pos, 1, 0, write_index_body);
}


/*
 *  package
 */
static int write_package_body(struct PrintFormat *format, addr stream, addr pos)
{
	getname_package(pos, &pos);
	print_string_stream(stream, pos);
	return 0;
}

static int write_package(struct PrintFormat *format, addr stream, addr pos)
{
	/* #<PACKAGE NAME> */
	return print_unreadable_object(format, stream, pos, 1, 0, write_package_body);
}


/*
 *  random-state
 */
static int write_random_state_body(struct PrintFormat *format, addr stream, addr pos)
{
	LocalRoot local;
	LocalStack stack;
	struct PrintFormat fmt;
	Execute ptr;

	ptr = format->ptr;
	format_print(ptr, &fmt);
	fmt.escape = 0;
	fmt.readably = 0;
	fmt.radix = 1;
	fmt.base = 16;
	fmt.printcase = PrintCase_upcase;
	fmt.ptr = ptr;

	local = ptr->local;
	push_local(local, &stack);
	make_bignum_random_state_local(local, pos, &pos);
	if (write_print(&fmt, stream, pos)) return 1;
	rollback_local(local, stack);

	return 0;
}

static int write_random_state(struct PrintFormat *format, addr stream, addr pos)
{
	return print_unreadable_object(format, stream, pos, 1, 0, write_random_state_body);
}


/*
 *  pathname
 */
static int write_pathname(struct PrintFormat *format, addr stream, addr pos)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = format->ptr;
	local = ptr->local;
	push_local(local, &stack);
	name_pathname_local(ptr, pos, &pos);
	if (format->escape)
		print_ascii_stream(stream, "#p");
	(void)write_strtype(format, stream, pos);
	rollback_local(local, stack);

	return 0;
}


/*
 *  stream
 */
static int write_stream_body(struct PrintFormat *format, addr stream, addr pos)
{
	struct StructStream *str;

	str = PtrStructStream(pos);
	switch (str->type) {
		case StreamType_BinaryInput:
			print_ascii_stream(stream, "FILE-INPUT BINARY");
			break;

		case StreamType_BinaryOutput:
			print_ascii_stream(stream, "FILE-OUTPUT BINARY");
			break;

		case StreamType_BinaryIO:
			print_ascii_stream(stream, "FILE-IO BINARY");
			break;

		case StreamType_CharacterInput:
			print_ascii_stream(stream, "FILE-INPUT CHARACTER");
			break;

		case StreamType_CharacterOutput:
			print_ascii_stream(stream, "FILE-OUTPUT CHARACTER");
			break;

		case StreamType_CharacterIO:
			print_ascii_stream(stream, "FILE-IO CHARACTER");
			break;

		case StreamType_BincharInput:
			print_ascii_stream(stream, "FILE-INPUT SYSTEM");
			break;

		case StreamType_BincharOutput:
			print_ascii_stream(stream, "FILE-OUTPUT SYSTEM");
			break;

		case StreamType_BincharIO:
			print_ascii_stream(stream, "FILE-IO SYSTEM");
			break;

		case StreamType_StringInput:
			print_ascii_stream(stream, "STREAM STRING-INPUT");
			break;

		case StreamType_StringOutput:
			print_ascii_stream(stream, "STREAM STRING-OUTPUT");
			break;

		case StreamType_Synonym:
			print_ascii_stream(stream, "SYNONYM-STREAM");
			break;

		case StreamType_BroadCast:
			print_ascii_stream(stream, "BROADCAST-STREAM");
			break;

		case StreamType_Concatenated:
			print_ascii_stream(stream, "CONCATENATED-STREAM");
			break;

		case StreamType_TwoWay:
			print_ascii_stream(stream, "TWO-WAY-STREAM");
			break;

		case StreamType_Echo:
			print_ascii_stream(stream, "ECHO-STREAM");
			break;

		case StreamType_Prompt:
			print_ascii_stream(stream, "PROMPT-STREAM");
			break;

		default:
			print_ascii_stream(stream, "STREAM");
			break;
	}

	return 0;
}

static int write_stream(struct PrintFormat *format, addr stream, addr pos)
{
	return print_unreadable_object(format, stream, pos, 0, 1, write_stream_body);
}


/*
 *  quote
 */
static int write_quote(struct PrintFormat *format, addr stream, addr pos)
{
	if (quote_back_p(pos)) {
		getprint_quote(pos, &pos);
		write_char_stream(stream, '`');
		return write_print(format, stream, pos);
	}
	if (quote_comma_p(pos)) {
		getprint_quote(pos, &pos);
		write_char_stream(stream, ',');
		return write_print(format, stream, pos);
	}
	if (quote_atsign_p(pos)) {
		getprint_quote(pos, &pos);
		print_ascii_stream(stream, ",@");
		return write_print(format, stream, pos);
	}
	if (quote_dot_p(pos)) {
		getprint_quote(pos, &pos);
		print_ascii_stream(stream, ",.");
		return write_print(format, stream, pos);
	}
	return print_unreadable_object(format, stream, pos, 1, 1, NULL);
}


/*
 *  restart
 */
static int write_restart_body(struct PrintFormat *format, addr stream, addr pos)
{
	getname_restart(pos, &pos);
	return princ_print(format->ptr, stream, pos);
}

static int write_restart(struct PrintFormat *format, addr stream, addr pos)
{
	addr report;
	Execute ptr;

	ptr = format->ptr;
	getreport_restart(pos, &report);
	if (report == Nil || format->escape) {
		/* #<RESTART NAME #xADDRESS> */
		return print_unreadable_object(format, stream, pos, 1, 1, write_restart_body);
	}
	else if (stringp(report)) {
		return write_strtype(format, stream, report);
	}
	else {
		return callclang_funcall(ptr, &report, report, stream, NULL);
	}
}


/*
 *  bitvector
 */
static int write_bitvector(struct PrintFormat *format, addr stream, addr pos)
{
	int value;
	size_t size, i;

	bitmemory_length(pos, &size);
	print_ascii_stream(stream, "#*");
	for (i = 0; i < size; i++) {
		bitmemory_getint(pos, i, &value);
		write_char_stream(stream, value? '1': '0');
	}

	return 0;
}


/*
 *  byte
 */
static int write_bytespec_body(struct PrintFormat *format, addr stream, addr pos)
{
	char data[256];
	struct bytespec_struct *ptr;

	ptr = ByteSpecStruct(pos);
	snprintf(data, 256, "SIZE:%zu POSITION:%zu", ptr->size, ptr->position);
	print_ascii_stream(stream, data);

	return 0;
}

static int write_bytespec(struct PrintFormat *format, addr stream, addr pos)
{
	return print_unreadable_object(format, stream, pos, 1, 0, write_bytespec_body);
}


/*
 *  write
 */
_g int write_print(struct PrintFormat *format, addr stream, addr object)
{
	int index = (int)GetType(object);
	return (call_write_print[index])(format, stream, object);
}

static int write_error_body(struct PrintFormat *format, addr stream, addr pos)
{
	print_ascii_stream(stream, "INVALID-OBJECT");
	if (! type_name_p(&pos, pos)) {
		write_char_stream(stream, ' ');
		return write_print(format, stream, pos);
	}
	else {
		return 0;
	}
}

static int write_error(struct PrintFormat *format, addr stream, addr pos)
{
	return print_unreadable_object(format, stream, pos, 0, 1, write_error_body);
}

static int write_system(struct PrintFormat *format, addr stream, addr pos)
{
	return print_unreadable_object(format, stream, pos, 1, 1, NULL);
}

_g void init_print(void)
{
	int i;

	/* error */
	for (i = 0; i < LISPTYPE_SIZE; i++)
		call_write_print[i] = write_error;

	/* call */
	call_write_print[LISPTYPE_NIL] = write_symbol;
	call_write_print[LISPTYPE_T] = write_symbol;
	call_write_print[LISPTYPE_TYPE] = write_system;
	call_write_print[LISPTYPE_CLOS] = write_clos;
	call_write_print[LISPTYPE_CONS] = write_cons;
	call_write_print[LISPTYPE_ARRAY] = write_array;
	call_write_print[LISPTYPE_VECTOR] = write_vector;
	call_write_print[LISPTYPE_CHARACTER] = write_character;
	call_write_print[LISPTYPE_STRING] = write_strtype;
	call_write_print[LISPTYPE_HASHTABLE] = write_hashtable;
	call_write_print[LISPTYPE_READTABLE] = write_system;
	call_write_print[LISPTYPE_SYMBOL] = write_symbol;
	call_write_print[LISPTYPE_FIXNUM] = write_fixnum;
	call_write_print[LISPTYPE_BIGNUM] = write_bignum;
	call_write_print[LISPTYPE_RATIO] = write_ratio;
	call_write_print[LISPTYPE_SHORT_FLOAT] = write_error;
	call_write_print[LISPTYPE_SINGLE_FLOAT] = write_single_float;
	call_write_print[LISPTYPE_DOUBLE_FLOAT] = write_double_float;
	call_write_print[LISPTYPE_LONG_FLOAT] = write_long_float;
	call_write_print[LISPTYPE_COMPLEX] = write_complex;
	call_write_print[LISPTYPE_CONTROL] = write_system;
	call_write_print[LISPTYPE_CODE] = write_system;
	call_write_print[LISPTYPE_CALLNAME] = write_callname;
	call_write_print[LISPTYPE_FUNCTION] = write_function;
	call_write_print[LISPTYPE_INDEX] = write_index;
	call_write_print[LISPTYPE_SYSTEM] = write_system;
	call_write_print[LISPTYPE_PACKAGE] = write_package;
	call_write_print[LISPTYPE_RANDOM_STATE] = write_random_state;
	call_write_print[LISPTYPE_PATHNAME] = write_pathname;
	call_write_print[LISPTYPE_STREAM] = write_stream;
	call_write_print[LISPTYPE_QUOTE] = write_quote;
	call_write_print[LISPTYPE_RESTART] = write_restart;
	call_write_print[LISPTYPE_EVAL] = write_system;
	call_write_print[LISPTYPE_ENVIRONMENT] = write_system;
	call_write_print[LISPTYPE_BITVECTOR] = write_bitvector;
	call_write_print[LISPTYPE_PPRINT] = write_system;
	call_write_print[LISPTYPE_BYTESPEC] = write_bytespec;

	call_write_print[LISPSYSTEM_CHARACTER2] = write_system;
	call_write_print[LISPSYSTEM_CHARQUEUE] = write_system;
	call_write_print[LISPSYSTEM_CHARBIT] = write_system;
	call_write_print[LISPSYSTEM_SYMSTACK] = write_system;
	call_write_print[LISPSYSTEM_SYMARRAY] = write_system;
	call_write_print[LISPSYSTEM_BITTYPE] = write_system;
	call_write_print[LISPSYSTEM_READLABEL] = write_system;
	call_write_print[LISPSYSTEM_READINFO] = write_system;
	call_write_print[LISPSYSTEM_READTYPE] = write_system;
	call_write_print[LISPSYSTEM_BITCONS] = write_system;
	call_write_print[LISPSYSTEM_BITBUFFER] = write_system;
	call_write_print[LISPSYSTEM_HASHITERATOR] = write_system;
	call_write_print[LISPSYSTEM_PACKAGEITERATOR] = write_system;
	call_write_print[LISPSYSTEM_TAGINFO] = write_system;
	call_write_print[LISPSYSTEM_ARRAY_DIMENSION] = write_system;
	call_write_print[LISPSYSTEM_ARRAY_GENERAL] = write_system;
	call_write_print[LISPSYSTEM_ARRAY_SPECIALIZED] = write_system;
}


/*
 *  print
 */
static int getboolean(Execute ptr, constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	getspecialcheck_local(ptr, symbol, &symbol);
	return symbol != Nil;
}

static unsigned getbase(Execute ptr, int index)
{
	addr symbol;
	fixnum value;

	GetConstant(index, &symbol);
	getspecialcheck_local(ptr, symbol, &symbol);
	if (GetType(symbol) != LISPTYPE_FIXNUM)
		TypeError(symbol, FIXNUM);
	GetFixnum(symbol, &value);
	if (! isBaseChar(value))
		fmte("base must be a number between 2 and 36.", NULL);

	return (unsigned)value;
}

static enum PrintCase getprintcase(Execute ptr, int index)
{
	addr symbol, check;

	GetConstant(index, &symbol);
	getspecialcheck_local(ptr, symbol, &symbol);
	GetConst(KEYWORD_UPCASE, &check);
	if (symbol == check)
		return PrintCase_upcase;
	GetConst(KEYWORD_DOWNCASE, &check);
	if (symbol == check)
		return PrintCase_downcase;
	GetConst(KEYWORD_CAPITALIZE, &check);
	if (symbol != check)
		fmte("type error", NULL);

	return PrintCase_capitalize;
}

static enum ReadTable_float getreadfloat(Execute ptr)
{
	addr pos, check;

	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	GetConst(COMMON_SINGLE_FLOAT, &check);
	if (check == pos) return ReadTable_single;
	GetConst(COMMON_DOUBLE_FLOAT, &check);
	if (check == pos) return ReadTable_double;
	GetConst(COMMON_LONG_FLOAT, &check);
	if (check == pos) return ReadTable_long;
	GetConst(COMMON_SHORT_FLOAT, &check);
	if (check == pos) return ReadTable_short;
	fmte("Invalid *read-default-float-format* value ~S.", pos, NULL);

	return ReadTable_single;
}

static fixnum getintnil(Execute ptr, constindex index)
{
	addr symbol, pos;
	fixnum value;

	GetConstant(index, &symbol);
	getspecialcheck_local(ptr, symbol, &pos);
	if (pos == Nil)
		return -1;
	if (GetType(pos) != LISPTYPE_FIXNUM)
		TypeError(pos, FIXNUM);
	GetFixnum(pos, &value);
	if (value <= 0)
		fmte("The ~S value ~S must be non-negative integer.", symbol, pos, NULL);

	return value;
}

_g void format_print(Execute ptr, struct PrintFormat *format)
{
	format->array = getboolean(ptr, CONSTANT_SPECIAL_PRINT_ARRAY);
	format->circle = getboolean(ptr, CONSTANT_SPECIAL_PRINT_CIRCLE);
	format->escape = getboolean(ptr, CONSTANT_SPECIAL_PRINT_ESCAPE);
	format->gensym = getboolean(ptr, CONSTANT_SPECIAL_PRINT_GENSYM);
	format->pretty = getboolean(ptr, CONSTANT_SPECIAL_PRINT_PRETTY);
	format->radix = getboolean(ptr, CONSTANT_SPECIAL_PRINT_RADIX);
	format->readably = getboolean(ptr, CONSTANT_SPECIAL_PRINT_READABLY);
	format->printcase = getprintcase(ptr, CONSTANT_SPECIAL_PRINT_CASE);
	format->readcase = PrintCase_unread;
	format->readfloat = getreadfloat(ptr);
	format->base = getbase(ptr, CONSTANT_SPECIAL_PRINT_BASE);
	format->ptr = ptr;
	GetConst(SPECIAL_PRINT_PPRINT_DISPATCH, &(format->dispatch));
	format->length = getintnil(ptr, CONSTANT_SPECIAL_PRINT_LENGTH);
	format->level = getintnil(ptr, CONSTANT_SPECIAL_PRINT_LEVEL);
	format->now = 0;
	format->lines = getintnil(ptr, CONSTANT_SPECIAL_PRINT_LINES);
	format->width = getintnil(ptr, CONSTANT_SPECIAL_PRINT_MISER_WIDTH);
	format->margin = getintnil(ptr, CONSTANT_SPECIAL_PRINT_RIGHT_MARGIN);
}

_g int princ_print(Execute ptr, addr stream, addr object)
{
	struct PrintFormat format;

	format_print(ptr, &format);
	format.escape = 0;
	format.readably = 0;
	format.ptr = ptr;
	return write_print(&format, stream, object);
}

_g int prin1_print(Execute ptr, addr stream, addr object)
{
	struct PrintFormat format;

	format_print(ptr, &format);
	format.escape = 1;
	format.ptr = ptr;
	return write_print(&format, stream, object);
}

_g int princ_string(Execute ptr, LocalRoot local, addr *ret, addr object)
{
	addr stream;

	open_output_string_stream(&stream, PRINT_STREAM_SIZE);
	if (princ_print(ptr, stream, object)) return 1;
	string_stream_alloc(local, stream, ret);
	close_stream(stream);

	return 0;
}

_g int prin1_string(Execute ptr, LocalRoot local, addr *ret, addr object)
{
	addr stream;

	open_output_string_stream(&stream, PRINT_STREAM_SIZE);
	if (prin1_print(ptr, stream, object)) return 1;
	string_stream_alloc(local, stream, ret);
	close_stream(stream);

	return 0;
}


/*
 *  pretty print
 */
_g void pprint_dispatch_heap(addr *ret)
{
	heap_smallsize(ret, LISPTYPE_PPRINT, 1, 0);
}

