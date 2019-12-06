#include <stdarg.h>
#include <math.h>
#include "bigcons.h"
#include "bignum.h"
#include "c99.h"
#include "character.h"
#include "charqueue.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control.h"
#include "equal.h"
#include "format.h"
#include "fmtfloat.h"
#include "integer.h"
#include "number.h"
#include "package.h"
#include "print.h"
#include "print_pretty.h"
#include "print_write.h"
#include "radix.h"
#include "ratio.h"
#include "sequence.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"
#include "symbol.h"
#include "unicode.h"

/*****************************************************************************
 *  format-lisp
 *****************************************************************************/
/*
 * ~args opt operator
 *
 * arg -> [+-]?[0-9]+ | '. | [vV] | #
 * args -> (arg (, arg)*)?
 * opt -> : | @ | :@ | @:
 * operator -> aAsSdDbBoOxXrRpPcCfFeEgG$%&|~\ntT*?_wWiI()[];{}<>^
 *
 * vV		argument
 * #		count
 *
 * aA		Ascii
 * sS		S-expression
 * dD		Decimal
 * bB		Binary
 * oO		Octal
 * xX		Hexadecimal
 * rR		Radix
 * pP		Plural
 * cC		Character
 * fF		Fixed-format floating-point
 * eE		Exponential floating-point
 * gG		General floating-point
 * $		Dollars floating-point
 * %		terpri
 * &		fresh-line
 * |		Page out
 * ~		Tilde
 * \n		Line espace
 * tT		Tabulate
 * *		Ignore
 * ?		Indirection
 * _		Conditional newline
 * wW		Write
 * iI		Indent
 * ()		Case conversion
 * []		Conditional expression
 * ;		sparate
 * {}		Iteration
 * <>		Justification
 * ^		Up and out
 */
static void formatabort(addr format, size_t position)
{
	unicode u;
	size_t i, size;
	addr stream;

	error_output_stream(Execute_Thread, &stream);
	fresh_line_stream(stream);

	/* Format error: ~1,2,3,4,5,6,7,8F */
	print_ascii_stream(stream, "Format error: ");
	string_length(format, &size);
	for (i = 0; i < size; i++) {
		string_getc(format, i, &u);
		write_char_stream(stream, u);
	}
	fresh_line_stream(stream);

	/*                      |          */
	print_ascii_stream(stream, "              ");
	string_length(format, &size);
	for (i = 0; i < position; i++)
		write_char_stream(stream, ' ');
	write_char_stream(stream, '^');
	fresh_line_stream(stream);
}


/*
 *  fmtinput
 */
struct fmtinput_struct {
	LocalRoot local;
	addr format;
	size_t size, index;
};

typedef struct fmtinput_struct *fmtinput;

static void fmtinput_init(fmtinput input, LocalRoot local, addr format)
{
	memset(input, 0, sizeoft(struct fmtinput_struct));
	input->local = local;
	input->format = format;
	input->index = 0;
	string_length(format, &(input->size));
}

static int fmtinput_peek(fmtinput input, unicode *u)
{
	if (input->size <= input->index) {
		return 1;
	}
	else {
		string_getc(input->format, input->index, u);
		return 0;
	}
}

static int fmtinput_getc(fmtinput input, unicode *u)
{
	int result;

	result = fmtinput_peek(input, u);
	if (! result)
		input->index++;

	return result;
}

static void fmtinput_getcheck(fmtinput input, unicode *u)
{
	if (fmtinput_getc(input, u)) {
		formatabort(input->format, input->index);
		fmte("Invalid format string.", NULL);
		return;
	}
}


/*
 *  fmtchar-parse
 */
#define FORMAT_ARGUMENT_SIZE		8
#define FORMAT_ARGUMENT_BUFFER		32

enum fmtargs_type {
	fmtargs_nil,
	fmtargs_integer,
	fmtargs_character,
	fmtargs_argument,
	fmtargs_count,
	fmtargs_index,
	fmtargs_size
};

struct fmtargs {
	struct fmtargs *next;
	enum fmtargs_type type;
	size_t position;
	union format_union {
		unicode character;
		fixnum value;
		size_t index;
	} u;
};

struct fmtchar {
	unsigned colon : 1;
	unsigned atsign : 1;
	unsigned colon_close : 1;
	unsigned size;
	unicode character;
	struct fmtchar *next, *option;
	size_t position, colon_pos, atsign_pos;
	struct fmtargs args[FORMAT_ARGUMENT_SIZE], *root, *tail;
	addr symbol;
};

static void fmtchar_init(fmtinput input, struct fmtchar *str)
{
	int i;

	memset(str, 0, sizeoft(struct fmtchar));
	for (i = 0; i < FORMAT_ARGUMENT_SIZE; i++)
		str->args[i].type = fmtargs_nil;
	str->position = input->index;
}

static struct fmtargs *fmtargs_make(fmtinput input, struct fmtchar *str)
{
	struct fmtargs *ptr;
	LocalRoot local;

	local = input->local;
	if (str->size < FORMAT_ARGUMENT_SIZE) {
		ptr = &(str->args[str->size]);
	}
	else {
		ptr = (struct fmtargs *)lowlevel_local(local, sizeoft(struct fmtargs));
		memset(ptr, 0, sizeoft(struct fmtargs));
		ptr->type = fmtargs_nil;
	}

	if (str->root)
		str->tail = str->tail->next = ptr;
	else
		str->root = str->tail = ptr;
	ptr->position = input->index;
	str->size++;

	return ptr;
}

static void fmtchar_push(fmtinput input, struct fmtchar *str)
{
	struct fmtargs *argtype;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_argument;
}

static void fmtchar_sharp(fmtinput input, struct fmtchar *str)
{
	struct fmtargs *argtype;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_count;
}

static void fmtchar_character(fmtinput input, struct fmtchar *str)
{
	unicode u;
	struct fmtargs *argtype;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_character;
	fmtinput_getcheck(input, &u);
	argtype->u.character = u;
}

static void fmtchar_sign(fmtinput input, int *sign, unicode u)
{
	if (*sign) {
		formatabort(input->format, input->index);
		fmte("Invalid sign character.", NULL);
		return;
	}
	*sign = (int)u;
}

static void fmtchar_nil(fmtinput input, struct fmtchar *str)
{
	struct fmtargs *argtype;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_nil;
}

static fixnum parse_fixnum_value(fmtinput input, int sign, addr queue)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	fixnum value;

	local = input->local;
	push_local(local, &stack);
	if (sign == '+') sign = signplus_bignum;
	if (sign == '-') sign = signminus_bignum;
	if (fixnum_cons_alloc(local, &pos, sign, queue)) {
		formatabort(input->format, input->index);
		fmte("Too large integer value.", NULL);
		return 0;
	}
	GetFixnum(pos, &value);
	rollback_local(local, stack);
	clear_bigcons(queue);

	return value;
}

static void fmtchar_value(fmtinput input, struct fmtchar *str, int *sign, addr queue)
{
	struct fmtargs *argtype;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_integer;
	argtype->u.value = parse_fixnum_value(input, *sign, queue);
	*sign = 0;
}

static int fmtchar_nilcheck(fmtinput input, int sign, addr queue)
{
	if (bigcons_empty_p(queue)) {
		if (sign) {
			formatabort(input->format, input->index);
			fmte("Invalid integer value.", NULL);
			return 1;
		}
		return 1;
	}
	else {
		return 0;
	}
}

static void fmtchar_colon(fmtinput input, struct fmtchar *str)
{
	if (str->colon) {
		formatabort(input->format, input->index);
		fmte("Format parameter ':' already supplies.", NULL);
		return;
	}
	str->colon_pos = input->index;
	str->colon = 1;
}

static void fmtchar_atsign(fmtinput input, struct fmtchar *str)
{
	if (str->atsign) {
		formatabort(input->format, input->index);
		fmte("Format parameter '@' already supplies.", NULL);
		return;
	}
	str->atsign_pos = input->index;
	str->atsign = 1;
}

static void fmtchar_parse_function(fmtinput input, struct fmtchar *str)
{
	unicode u;
	addr queue, package, pos;
	LocalRoot local;
	size_t size;

	local = input->local;
	charqueue_local(local, &queue, FORMAT_ARGUMENT_BUFFER);

first:
	fmtinput_getcheck(input, &u);
	if (u == '/')
		goto end_first;
	if (u == ':')
		goto colon_first;
	u = toUpperUnicode(u);
	push_charqueue_local(local, queue, u);
	goto first;

end_first:
	GetCharQueueSize(queue, &size);
	if (size == 0)
		goto error_name;
	find_char_package(LISP_COMMON_USER, &package);
	Check(package == Nil, "package error");
	make_charqueue_heap(queue, &pos); /* Don't use local. */
	intern_package(package, pos, &pos);
	goto finish;

colon_first:
	GetCharQueueSize(queue, &size);
	if (size == 0) {
		formatabort(input->format, input->index);
		fmte("The package name is empty. (Don't use keyword)", NULL);
		return;
	}
	make_charqueue_local(local, queue, &package);
	clear_charqueue(queue);
	find_package(package, &package);
	if (package == Nil) {
		formatabort(input->format, input->index);
		fmte("The package ~S is not exist.", package, NULL);
		return;
	}

	fmtinput_getcheck(input, &u);
	if (u == '/')
		goto error_name;
	if (u == ':')
		goto colon_second;
	u = toUpperUnicode(u);
	push_charqueue_local(local, queue, u);
	goto second;

colon_second:
	fmtinput_getcheck(input, &u);
	if (u == '/')
		goto error_name;
	if (u == ':') {
		formatabort(input->format, input->index);
		fmte("Invalid colon ::: separator.", NULL);
		return;
	}
	u = toUpperUnicode(u);
	push_charqueue_local(local, queue, u);
	goto second;

second:
	fmtinput_getcheck(input, &u);
	if (u == '/')
		goto end_second;
	/* if (u == ':') goto error; */
	u = toUpperUnicode(u);
	push_charqueue_local(local, queue, u);
	goto second;

end_second:
	make_charqueue_heap(queue, &pos); /* Don't use local. */
	intern_package(package, pos, &pos);
	goto finish;

error_name:
	formatabort(input->format, input->index);
	fmte("The function name is empty.", NULL);
	return;

finish:
	str->symbol = pos;
}

static void fmtchar_parse(fmtinput input, struct fmtchar *str)
{
	int sign, comma;
	unicode u;
	addr queue;
	LocalRoot local;

	local = input->local;
	bigcons_local(local, &queue);
	fmtchar_init(input, str);
	sign = 0;
	comma = 0;
args:
	fmtinput_getcheck(input, &u);
	if (u == ':' || u == '@') {
		goto colon_atsign;
	}
	if (u == 'v' || u == 'V') {
		fmtchar_push(input, str);
		comma = 1;
		goto args;
	}
	if (u == '#') {
		fmtchar_sharp(input, str);
		comma = 1;
		goto args;
	}
	if (u == '\'') {
		fmtchar_character(input, str);
		comma = 1;
		goto args;
	}
	if (u == '-' || u == '+') {
		fmtchar_sign(input, &sign, u);
		comma = 1;
		goto args;
	}
	if (isDigitCase(u)) {
		push_bigcons(local, queue, 10, (unsigned)(u - '0'));
		comma = 1;
		goto args;
	}
	if (u == ',') {
		goto push_value;
	}
	goto operator_label;

push_value:
	if (fmtchar_nilcheck(input, sign, queue)) {
		if (comma == 0)
			fmtchar_nil(input, str);
	}
	else {
		fmtchar_value(input, str, &sign, queue);
	}
	comma = 0;
	goto args;

colon_atsign:
	if (! fmtchar_nilcheck(input, sign, queue))
		fmtchar_value(input, str, &sign, queue);

colon_atsign_loop:
	if (u == ':')
		fmtchar_colon(input, str);
	else if (u == '@')
		fmtchar_atsign(input, str);
	else
		goto operator_label;
	fmtinput_getcheck(input, &u);
	goto colon_atsign_loop;

operator_label:
	if (! fmtchar_nilcheck(input, sign, queue))
		fmtchar_value(input, str, &sign, queue);
	if (u == 0) {
		formatabort(input->format, input->index);
		fmte("Invalid operator character, \\0.", NULL);
		return;
	}
	if (u == '/') {
		fmtchar_parse_function(input, str);
	}
	str->character = u;
}


/*
 *  fmtchar-group
 */
struct fmtroot {
	struct fmtchar *root, *tail;
	size_t start;
};

static struct fmtchar *fmtchar_local(LocalRoot local)
{
	struct fmtchar *ptr;
	ptr = (struct fmtchar *)lowlevel_local(local, sizeoft(struct fmtchar));
	memset(ptr, 0, sizeoft(struct fmtchar));
	return ptr;
}

static void fmtroot_push(LocalRoot local, struct fmtroot *root, struct fmtchar *comm)
{
	if (root->root)
		root->tail = root->tail->next = comm;
	else
		root->tail = root->root = comm;
}

static void fmtroot_text(fmtinput input, struct fmtroot *root)
{
	struct fmtchar *comm;
	size_t p1, p2;
	LocalRoot local;

	local = input->local;
	p1 = root->start;
	p2 = input->index;
	if (p1 != p2) {
		comm = fmtchar_local(local);
		comm->character = 0;
		comm->size = 2;
		comm->args[0].type = fmtargs_index;
		comm->args[0].u.index = p1;
		comm->args[1].type = fmtargs_index;
		comm->args[1].u.index = p2;
		comm->position = p2;
		fmtroot_push(local, root, comm);
		root->start = p2;
	}
}

static struct fmtchar *fmtchar_loop(fmtinput input)
{
	unicode u;
	struct fmtchar *comm;
	struct fmtroot root;
	LocalRoot local;

	local = input->local;
	memset(&root, 0, sizeoft(struct fmtroot));
	while (! fmtinput_peek(input, &u)) {
		if (u == '~') {
			fmtroot_text(input, &root);
			input->index++;
			comm = fmtchar_local(local);
			fmtchar_parse(input, comm);
			fmtroot_push(local, &root, comm);
			root.start = input->index;
		}
		else {
			input->index++;
		}
	}
	fmtroot_text(input, &root);

	return root.root;
}

/* fmtgroup */
struct fmtgroup {
	struct fmtchar *root, *list;
	LocalRoot local;
	addr format;
	unicode a;
	int semicolon;
};

static void fmtchar_group_eof(struct fmtgroup *group)
{
	addr pos;
	size_t size;

	character_local(group->local, &pos, group->a);
	string_length(group->format, &size);
	formatabort(group->format, size);
	fmte("There is no parensis ~S.", pos, NULL);
}

static int fmtchar_group(struct fmtgroup *group);
static void fmtchar_group_set(struct fmtgroup *group, unicode a, int semicolon)
{
	int colon;
	struct fmtchar *list, *next;
	struct fmtgroup backup;

	/* backup */
	memcpy(&backup, group, sizeoft(struct fmtgroup));
	list = group->list;
	next = list->next;
	/* parameter */
	group->a = a;
	group->semicolon = semicolon;
	group->list = next;
	/* group */
	colon = fmtchar_group(group);
	/* option */
	list->option = next;
	list->colon_close = colon;
	/* next list */
	list = list->next = group->list;
	memcpy(group, &backup, sizeoft(struct fmtgroup));
	group->list = list;
}

static int fmtchar_group_close(struct fmtgroup *group)
{
	addr pos;
	struct fmtchar *list;

	list = group->list;
	if (group->a != list->character) {
		formatabort(group->format, list->position);
		character_local(group->local, &pos, group->a);
		fmte("The close parensis ~S mismatch.", pos, NULL);
		return 0;
	}
	group->list = list->next;
	list->next = NULL;

	return list->colon;
}

static void fmtchar_group_semicolon(struct fmtgroup *group)
{
	if (! group->semicolon) {
		formatabort(group->format, group->list->position);
		fmte("~Invalid ~; parameter.", NULL);
		return;
	}
	group->list = group->list->next;
}

static int fmtchar_group(struct fmtgroup *group)
{
	while (group->list) {
		switch (group->list->character) {
			case '(':
				fmtchar_group_set(group, ')', 0);
				break;

			case '[': /* semicolon */
				fmtchar_group_set(group, ']', 1);
				break;

			case '{':
				fmtchar_group_set(group, '}', 0);
				break;

			case '<': /* semicolon */
				fmtchar_group_set(group, '>', 1);
				break;

			case ')':
			case ']':
			case '}':
			case '>':
				return fmtchar_group_close(group);

			case ';':
				fmtchar_group_semicolon(group);
				break;

			default:
				group->list = group->list->next;
				break;
		}
	}
	if (group->a != 0)
		fmtchar_group_eof(group);

	return 0;
}

static struct fmtchar *fmtchar_make(LocalRoot local, addr format)
{
	struct fmtchar *list;
	struct fmtinput_struct input;
	struct fmtgroup group;

	/* loop */
	fmtinput_init(&input, local, format);
	list = fmtchar_loop(&input);

	/* group */
	memset(&group, 0, sizeoft(struct fmtgroup));
	group.local = local;
	group.format = format;
	group.root = group.list = list;
	fmtchar_group(&group);

	return list;
}


/*
 *  format execute
 */
struct fmtstack {
	addr root;
	addr front;
	size_t index;
};

enum fmtcase {
	fmtcase_normal,
	fmtcase_upcase,
	fmtcase_downcase,
	fmtcase_capitalize_all,
	fmtcase_capitalize_first
};

struct fmtprint_struct {
	unsigned delete_space : 1;
	unsigned semicolon : 1;
	unsigned loop_colon : 1;
	unsigned loop : 1;
	unsigned word : 1;
	unsigned first : 1;
	unsigned last : 1;
	enum fmtcase conversion : 4; /* signed */
	Execute ptr;
	LocalRoot local;
	addr stream, format;
	struct fmtstack *rest;
	struct fmtchar *list;
};

typedef struct fmtprint_struct *fmtprint;
typedef int (*fmtcalltype)(fmtprint, struct fmtchar *);
static fmtcalltype FormatCallTable[0x80];

static void fmtprint_putc(fmtprint print, unicode u)
{
	switch (print->conversion) {
		case fmtcase_upcase:
			u = toUpperUnicode(u);
			break;

		case fmtcase_downcase:
			u = toLowerUnicode(u);
			break;

		case fmtcase_capitalize_all:
			if (print->word == 0 && isLowerCase(u))
				u = toUpperUnicode(u);
			else
				u = toLowerUnicode(u);
			break;

		case fmtcase_capitalize_first:
			if (print->word == 0 && print->first && isLowerCase(u)) {
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
	write_char_stream(print->stream, u);
}

static void fmtprint_pop(fmtprint print, struct fmtchar *str, addr *ret)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	if (ptr->front == Nil) {
		formatabort(print->format, str->position);
		fmte("Too few format arguments.", NULL);
		return;
	}
	getcons(ptr->front, ret, &(ptr->front));
	ptr->index++;
}

static void fmtprint_peek(fmtprint print, struct fmtchar *str, addr *ret)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	if (ptr->front == Nil) {
		formatabort(print->format, str->position);
		fmte("Too few format arguments.", NULL);
		return;
	}
	getcar(ptr->front, ret);
}

static void fmtprint_forward(fmtprint print, struct fmtchar *str, size_t count)
{
	addr front;
	size_t i;
	struct fmtstack *ptr;

	ptr = print->rest;
	front = ptr->front;
	for (i = 0; i < count; i++) {
		if (front == Nil) {
			formatabort(print->format, str->position);
			fmte("Too few format arguments.", NULL);
			return;
		}
		getcdr(front, &front);
		ptr->index++;
	}
	ptr->front = front;
}

static void fmtprint_rollback(fmtprint print, struct fmtchar *str, size_t count)
{
	addr root;
	size_t size;
	struct fmtstack *ptr;

	ptr = print->rest;
	root = ptr->root;
	size = ptr->index;
	if (size < count) {
		formatabort(print->format, str->position);
		fmte("Cannot rollback ~~:*.", NULL);
		return;
	}
	size -= count;
	ptr->index = size;
	getnthcdr_unsafe(root, size, &root);
	ptr->front = root;
}

static void fmtprint_absolute(fmtprint print, struct fmtchar *str, size_t count)
{
	struct fmtstack *ptr;

	ptr = print->rest;
	ptr->front = ptr->root;
	ptr->index = 0;
	fmtprint_forward(print, str, count);
}

static void getint_count(fmtprint print, fixnum *ret)
{
	*ret = (fixnum)length_list_safe(print->rest->front);
	Check(*ret < 0, "cast error");
}

static int getint_argument(fmtprint print, struct fmtchar *str, fixnum *ret)
{
	addr pos;

	fmtprint_pop(print, str, &pos);
	if (pos == Nil)
		return 1;
	if (GetType(pos) == LISPTYPE_BIGNUM) {
		formatabort(print->format, str->position);
		fmte("Too large the format argument ~S.", pos, NULL);
		return 1;
	}
	if (GetType(pos) != LISPTYPE_FIXNUM) {
		formatabort(print->format, str->position);
		fmte("The format argument ~S must be an integer.", pos, NULL);
		return 1;
	}
	GetFixnum(pos, ret);

	return 0;
}

static int fmtint_nilp(fmtprint print,
		struct fmtchar *str, unsigned index, fixnum *ret)
{
	struct fmtargs *ptr;

	Check(FORMAT_ARGUMENT_SIZE <= index, "index error");
	ptr = &(str->args[index]);
	switch (ptr->type) {
		case fmtargs_nil:
			return 1;

		case fmtargs_integer:
			*ret = ptr->u.value;
			return 0;

		case fmtargs_count:
			getint_count(print, ret);
			return 0;

		case fmtargs_argument:
			return getint_argument(print, str, ret);

		default:
			formatabort(print->format, str->position);
			fmte("The format parameter must be an integer.", NULL);
			return 1;
	}

	return 0;
}

static int getchar_argument(fmtprint print, struct fmtchar *str, unicode *ret)
{
	addr pos;

	fmtprint_pop(print, str, &pos);
	if (pos == Nil)
		return 1;
	if (GetType(pos) != LISPTYPE_CHARACTER) {
		formatabort(print->format, str->position);
		fmte("The format argument ~S must be a character.", pos, NULL);
		return 1;
	}
	GetCharacter(pos, ret);

	return 0;
}

static int fmtchar_nilp(fmtprint print,
		struct fmtchar *str, unsigned index, unicode *ret)
{
	struct fmtargs *ptr;

	Check(FORMAT_ARGUMENT_SIZE <= index, "index error");
	ptr = &(str->args[index]);
	switch (ptr->type) {
		case fmtargs_nil:
			return 1;

		case fmtargs_character:
			*ret = ptr->u.character;
			return 0;

		case fmtargs_argument:
			return getchar_argument(print, str, ret);

		case fmtargs_count:
		default:
			formatabort(print->format, str->position);
			fmte("The format argument must be a character.", NULL);
			return 1;
	}

	return 0;
}

static void fmtint_default(fmtprint print, struct fmtchar *str,
		unsigned index, fixnum *ret, fixnum defvar)
{
	if (fmtint_nilp(print, str, index, ret))
		*ret = defvar;
}

static void fmtchar_default(fmtprint print, struct fmtchar *str,
		unsigned index, unicode *ret, unicode defvar)
{
	if (fmtchar_nilp(print, str, index, ret))
		*ret = defvar;
}

static int format_output(fmtprint print, struct fmtchar *str)
{
	int delete_space;
	addr format;
	unicode u;
	size_t p1, p2, i;

	p1 = str->args[0].u.index;
	p2 = str->args[1].u.index;
	format = print->format;
	delete_space = print->delete_space;
	for (i = p1; i < p2; i++) {
		string_getc(format, i, &u);
		if ((! delete_space) || (! isSpaceUnicode(u))) {
			fmtprint_putc(print, u);
			delete_space = 0;
		}
	}
	print->delete_space = delete_space;

	return 0;
}

static void write_times(fmtprint print, unicode c, size_t size)
{
	for (; size; size--)
		fmtprint_putc(print, c);
}

static void write_string(fmtprint print, addr string)
{
	unicode u;
	size_t size, i;

	string_length(string, &size);
	for (i = 0; i < size; i++) {
		string_getc(string, i, &u);
		fmtprint_putc(print, u);
	}
}

static void write_margin(fmtprint print, addr string,
		int atsign, fixnum mincol, fixnum colinc, fixnum minpad, unicode padchar)
{
	fixnum check;
	size_t size, space;

	string_length(string, &size);
	check = (fixnum)size;
	Check(check < 0, "error");

	/* output margin */
	space = 0;
	if (check < mincol) {
		space = mincol - check;
		space = (space / colinc) + ((space % colinc)? 1: 0);
		space *= colinc;
	}
	if (space < (size_t)minpad)
		space = (size_t)minpad;

	if (atsign) {
		/* insert space in left */
		write_times(print, padchar, (size_t)space);
		write_string(print, string);
	}
	else {
		/* insert space in right */
		write_string(print, string);
		write_times(print, padchar, (size_t)space);
	}
}

static void format_ascii_parameter(fmtprint print, addr pos,
		fixnum mincol, fixnum colinc, fixnum minpad, unicode padchar,
		int colon, int atsign)
{
	if (pos == Nil && colon)
		strvect_char_local(print->local, &pos, "()");
	else
		princ_string_local(print->ptr, &pos, pos);
	write_margin(print, pos, atsign, mincol, colinc, minpad, padchar);
}

static void formatabort_args(fmtprint print, struct fmtchar *str, unsigned index)
{
	formatabort(print->format, str->args[index].position);
}

static int format_ascii(fmtprint print, struct fmtchar *str)
{
	addr control, symbol, pos;
	fixnum mincol, colinc, minpad;
	unicode padchar;
	Execute ptr;

	if (4 < str->size) {
		formatabort_args(print, str, 4);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	ptr = print->ptr;
	push_close_control(ptr, &control);
	/*
	 *  (let ((*print-escape* nil)
	 *        (*print-readably* nil))
	 *    ...)
	 */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(SPECIAL_PRINT_READABLY, &symbol);
	pushspecial_control(ptr, symbol, Nil);

	fmtint_default(print, str, 0, &mincol, 0);
	fmtint_default(print, str, 1, &colinc, 1);
	fmtint_default(print, str, 2, &minpad, 0);
	fmtchar_default(print, str, 3, &padchar, ' ');
	if (mincol < 0) {
		formatabort_args(print, str, 0);
		fmte("The parameter must be a positive integer.", NULL);
		return 1;
	}
	if (colinc < 1) {
		formatabort_args(print, str, 1);
		fmte("The parameter must be greater than 1.", NULL);
		return 1;
	}
	if (minpad < 0) {
		formatabort_args(print, str, 2);
		fmte("The parameter must be a positive integer.", NULL);
		return 1;
	}

	fmtprint_pop(print, str, &pos);
	format_ascii_parameter(print, pos,
			mincol, colinc, minpad, padchar, str->colon, str->atsign);

	return free_control(ptr, control);
}

static int format_s_express(fmtprint print, struct fmtchar *str)
{
	addr control, symbol, pos;
	fixnum mincol, colinc, minpad;
	unicode padchar;
	Execute ptr;
	LocalRoot local;

	if (4 < str->size) {
		formatabort_args(print, str, 4);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	ptr = print->ptr;
	local = ptr->local;
	push_close_control(ptr, &control);
	/* (let ((*print-escape* t)) ...) */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	pushspecial_control(ptr, symbol, T);

	fmtint_default(print, str, 0, &mincol, 0);
	fmtint_default(print, str, 1, &colinc, 1);
	fmtint_default(print, str, 2, &minpad, 0);
	fmtchar_default(print, str, 3, &padchar, ' ');
	if (mincol < 0) {
		formatabort_args(print, str, 0);
		fmte("The parameter must be a positive integer.", NULL);
		return 1;
	}
	if (colinc < 1) {
		formatabort_args(print, str, 1);
		fmte("The parameter must be greater than 1.", NULL);
		return 1;
	}
	if (minpad < 0) {
		formatabort_args(print, str, 2);
		fmte("The parameter must be a positive integer.", NULL);
		return 1;
	}

	/* string */
	fmtprint_pop(print, str, &pos);
	if (pos == Nil && str->colon)
		strvect_char_local(local, &pos, "()");
	else
		prin1_string_local(ptr, &pos, pos);
	write_margin(print, pos, str->atsign, mincol, colinc, minpad, padchar);

	return free_control(ptr, control);
}

static void format_radix_parameter(fmtprint print, struct fmtchar *str,
		unsigned radix, fixnum mincol, unicode padchar, fixnum range, unicode comma)
{
	int minusp;
	addr pos, stream;
	LocalRoot local;
	size_t size;

	local = print->local;
	size = (size_t)range;
	Check(size < 0, "cast error");

	fmtprint_pop(print, str, &pos);
	if (integerp(pos)) {
		open_output_string_stream(&stream, 0);
		/* sign */
		minusp = minusp_integer(pos);
		if (str->atsign || minusp)
			write_char_stream(stream, minusp? '-': '+');
		/* body */
		if (str->colon)
			output_nosign_comma_integer(local, stream, pos, radix, 1, size, comma);
		else
			output_nosign_integer(local, stream, pos, radix, 1);
		/* output */
		string_stream_local(print->local, stream, &pos);
		write_margin(print, pos, 1, mincol, 1, 0, padchar);
	}
	else {
		format_ascii_parameter(print, pos, mincol, 1, 0, padchar, 0, 1);
	}
}

static int format_radix_integer(fmtprint print, struct fmtchar *str, unsigned radix)
{
	addr control, symbol;
	fixnum mincol, range;
	unicode padchar, comma;
	Execute ptr;

	if (4 < str->size) {
		formatabort_args(print, str, 4);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	ptr = print->ptr;
	push_close_control(ptr, &control);
	/*
	 *  (let ((*print-escape* nil)
	 *        (*print-radix* nil)
	 *        (*print-base* [radix]))
	 *    ...)
	 */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(SPECIAL_PRINT_RADIX, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(SPECIAL_PRINT_BASE, &symbol);
	pushspecial_control(ptr, symbol, fixnum_heapr(radix));

	fmtint_default(print, str, 0, &mincol, 0);
	fmtchar_default(print, str, 1, &padchar, ' ');
	fmtchar_default(print, str, 2, &comma, ',');
	fmtint_default(print, str, 3, &range, 3);
	if (mincol < 0) {
		formatabort_args(print, str, 0);
		fmte("The parameter must be a positive integer.", NULL);
		return 1;
	}
	if (range < 2) {
		formatabort_args(print, str, 3);
		fmte("The parameter must be greate than 1.", NULL);
		return 1;
	}
	format_radix_parameter(print, str, radix, mincol, padchar, range, comma);

	return free_control(ptr, control);
}

static int format_binary(fmtprint print, struct fmtchar *str)
{
	return format_radix_integer(print, str, 2);
}
static int format_octal(fmtprint print, struct fmtchar *str)
{
	return format_radix_integer(print, str, 8);
}
static int format_decimal(fmtprint print, struct fmtchar *str)
{
	return format_radix_integer(print, str, 10);
}
static int format_hexadecimal(fmtprint print, struct fmtchar *str)
{
	return format_radix_integer(print, str, 16);
}

static void format_radix_text(fmtprint print, struct fmtchar *str)
{
	addr pos;
	fixnum value;

	fmtprint_pop(print, str, &pos);
	if (str->atsign) {
		/* roma */
		if (GetType(pos) != LISPTYPE_FIXNUM) {
			formatabort(print->format, str->position);
			fmte("~@R argument ~S must be an integer between 1 and 3999.", pos, NULL);
			return;
		}
		GetFixnum(pos, &value);
		if (! (1 <= value && value <= 3999)) {
			formatabort(print->format, str->position);
			fmte("~@R argument ~S must be an integer between 1 and 3999.", pos, NULL);
			return;
		}
		roma_integer(print->stream, value, str->colon);
	}
	else {
		/* english */
		if (! integerp(pos)) {
			formatabort(print->format, str->position);
			fmte("~~R argument ~S must be an integer.", pos, NULL);
			return;
		}
		english_integer(print->local, print->stream, pos, str->colon == 0);
	}
}

static int format_radix(fmtprint print, struct fmtchar *str)
{
	addr control, symbol;
	fixnum mincol, range, radix;
	unicode padchar, comma;
	Execute ptr;

	if (5 < str->size) {
		formatabort_args(print, str, 5);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	ptr = print->ptr;
	push_close_control(ptr, &control);
	/*
	 *  (let ((*print-escape* nil)
	 *        (*print-radix* nil)
	 *        (*print-base* [radix]))
	 *    ...)
	 */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	GetConst(SPECIAL_PRINT_RADIX, &symbol);
	pushspecial_control(ptr, symbol, Nil);

	if (fmtint_nilp(print, str, 0, &radix)) {
		/* radix = 10 */
		GetConst(SPECIAL_PRINT_BASE, &symbol);
		pushspecial_control(ptr, symbol, fixnum_heapr(10));
		format_radix_text(print, str);
	}
	else {
		if (! isBaseChar(radix)) {
			formatabort_args(print, str, 0);
			fmte("The parameter must be an integer between 2 and 36.", NULL);
			return 1;
		}
		GetConst(SPECIAL_PRINT_BASE, &symbol);
		pushspecial_control(ptr, symbol, fixnum_heapr(radix));

		fmtint_default(print, str, 1, &mincol, 0);
		fmtchar_default(print, str, 2, &padchar, ' ');
		fmtchar_default(print, str, 3, &comma, ',');
		fmtint_default(print, str, 4, &range, 3);
		if (mincol < 0) {
			formatabort_args(print, str, 1);
			fmte("The paramter must be a positive integer.", NULL);
			return 1;
		}
		if (range < 2) {
			formatabort_args(print, str, 4);
			fmte("The parameter must be greater than 1.", NULL);
			return 1;
		}
		format_radix_parameter(print,
				str, (unsigned)radix, mincol, padchar, range, comma);
	}

	return free_control(ptr, control);
}

static int format_plural(fmtprint print, struct fmtchar *str)
{
	addr pos;

	if (0 < str->size) {
		formatabort_args(print, str, 0);
		fmte("Too many parameters.", NULL);
		return 1;
	}

	/* ~:* */
	if (str->colon)
		fmtprint_rollback(print, str, 1);

	/* plural */
	fmtprint_pop(print, str, &pos);
	if (! str->atsign) {
		if (! eql_function(pos, fixnum_heapr(1)))
			fmtprint_putc(print, 's');
	}
	else {
		if (eql_function(pos, fixnum_heapr(1))) {
			fmtprint_putc(print, 'y');
		}
		else {
			fmtprint_putc(print, 'i');
			fmtprint_putc(print, 'e');
			fmtprint_putc(print, 's');
		}
	}

	return 0;
}

static int format_character(fmtprint print, struct fmtchar *str)
{
	addr pos, name;

	if (0 < str->size) {
		formatabort_args(print, str, 0);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	if (! str->colon && str->atsign) {
		/* ~@C */
		return format_s_express(print, str);
	}
	if (str->colon || str->atsign) {
		/* ~:C or ~:@C */
		fmtprint_pop(print, str, &pos);
		if (findtable_char_name(&name, pos))
			write_string(print, name);
		else
			fmtprint_putc(print, RefCharacter(pos));
	}
	else {
		/* ~C */
		fmtprint_pop(print, str, &pos);
		if (GetType(pos) != LISPTYPE_CHARACTER) {
			formatabort(print->format, str->position);
			fmte("The argument ~S must be a character.", pos, NULL);
			return 1;
		}
		fmtprint_putc(print, RefCharacter(pos));
	}

	return 0;
}

static void fmtfloat_w(fmtprint print,
		struct fmtchar *str, fmtfloat ff, unsigned index)
{
	int check;
	fixnum value;

	check = fmtint_nilp(print, str, index, &value);
	if (! check && value < 0) {
		formatabort_args(print, str, index);
		fmte("The parameter must be a positive integer.", NULL);
		return;
	}
	ff->wp = ! check;
	ff->w = check? 0: (size_t)value;
}

static void fmtfloat_d(fmtprint print,
		struct fmtchar *str, fmtfloat ff, unsigned index)
{
	int check;
	fixnum value;

	check = fmtint_nilp(print, str, index, &value);
	if (! check && value < 0) {
		formatabort_args(print, str, index);
		fmte("The parameter must be a positive integer.", NULL);
		return;
	}
	ff->dp = ! check;
	ff->d = check? 0: (size_t)value;
}

static void fmtfloat_e(fmtprint print,
		struct fmtchar *str, fmtfloat ff, unsigned index)
{
	fixnum value;

	fmtint_default(print, str, index, &value, 1);
	if (value < 0) {
		formatabort_args(print, str, index);
		fmte("The parameter must be a positive integer.", NULL);
		return;
	}
	if (value == 0)
		value = 1;
	ff->ep = 1;
	ff->e = (size_t)value;
}

static void fmtfloat_k(fmtprint print,
		struct fmtchar *str, fmtfloat ff, unsigned index, fixnum value)
{
	fmtint_default(print, str, index, &(ff->k), value);
}

static void fmtfloat_overflowchar(fmtprint print,
		struct fmtchar *str, fmtfloat ff, unsigned index)
{
	int check;
	unicode u;

	check = fmtchar_nilp(print, str, index, &u);
	ff->overflowp = ! check;
	ff->overflow = check? 0: u;
}

static void fmtfloat_padchar(fmtprint print,
		struct fmtchar *str, fmtfloat ff, unsigned index)
{
	fmtchar_default(print, str, index, &(ff->pad), ' ');
}

static void format_fixed_single(fmtprint print, fmtfloat ff, addr pos)
{
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE)) {
		fmte("Invalid single-float.", NULL);
		return;
	}
	fmtfloat_fixed(print->stream, ff, &dec);
}

static void format_fixed_double(fmtprint print, fmtfloat ff, addr pos)
{
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE)) {
		fmte("Invalid double-float.", NULL);
		return;
	}
	fmtfloat_fixed(print->stream, ff, &dec);
}

static void format_fixed_long(fmtprint print, fmtfloat ff, addr pos)
{
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG)) {
		fmte("Invalid long-float.", pos, NULL);
		return;
	}
	fmtfloat_fixed(print->stream, ff, &dec);
}

static void format_fixed_argument(fmtprint print, struct fmtchar *str, fmtfloat ff)
{
	memset(ff, 0, sizeoft(struct fmtfloat_struct));
	fmtfloat_w(print, str, ff, 0);
	fmtfloat_d(print, str, ff, 1);
	fmtfloat_k(print, str, ff, 2, 0);
	fmtfloat_overflowchar(print, str, ff, 3);
	fmtfloat_padchar(print, str, ff, 4);
	ff->k_bias = 0; /* 0 if fixed */
	ff->sign = str->atsign;
}

static void format_fixed_fixnum(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	format_fixed_single(print, ff, pos);
}

static void format_fixed_bignum(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_bignum_local(print->local, &pos, pos);
	format_fixed_single(print, ff, pos);
}

static void format_fixed_ratio(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_local(print->local, &pos, single_float_ratio(pos));
	format_fixed_single(print, ff, pos);
}

static void format_fixed_float(fmtprint print, struct fmtchar *str, fmtfloat ff)
{
	addr pos;

	fmtprint_pop(print, str, &pos);
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			format_fixed_double(print, ff, pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			format_fixed_single(print, ff, pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			format_fixed_long(print, ff, pos);
			break;

		case LISPTYPE_FIXNUM:
			format_fixed_fixnum(print, ff, pos);
			break;

		case LISPTYPE_BIGNUM:
			format_fixed_bignum(print, ff, pos);
			break;

		case LISPTYPE_RATIO:
			format_fixed_ratio(print, ff, pos);
			break;

		default:
			formatabort(print->format, str->position);
			fmte("~~F argument ~S must be a real type.", pos, NULL);
			return;
	}
}

static int format_fixed(fmtprint print, struct fmtchar *str)
{
	addr control, symbol;
	Execute ptr;
	struct fmtfloat_struct ff;

	if (5 < str->size) {
		formatabort_args(print, str, 5);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	ptr = print->ptr;
	push_close_control(ptr, &control);
	/* (let ((*print-escape* nil)) ...) */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	format_fixed_argument(print, str, &ff);
	format_fixed_float(print, str, &ff);

	return free_control(ptr, control);
}

static void format_exponent_single(fmtprint print, fmtfloat ff, addr pos)
{
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE)) {
		fmte("Invalid single-float.", pos, NULL);
		return;
	}
	fmtfloat_exponent(print->stream, ff, &dec);
}

static void format_exponent_double(fmtprint print, fmtfloat ff, addr pos)
{
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE)) {
		fmte("Invalid double-float.", pos, NULL);
		return;
	}
	fmtfloat_exponent(print->stream, ff, &dec);
}

static void format_exponent_long(fmtprint print, fmtfloat ff, addr pos)
{
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG)) {
		fmte("Invalid long-float.", pos, NULL);
		return;
	}
	fmtfloat_exponent(print->stream, ff, &dec);
}

static void format_exponent_argument(fmtprint print, struct fmtchar *str, fmtfloat ff)
{
	memset(ff, 0, sizeoft(struct fmtfloat_struct));
	fmtfloat_w(print, str, ff, 0);
	fmtfloat_d(print, str, ff, 1);
	fmtfloat_e(print, str, ff, 2);
	fmtfloat_k(print, str, ff, 3, 1);
	fmtfloat_overflowchar(print, str, ff, 4);
	fmtfloat_padchar(print, str, ff, 5);
	ff->k_bias = 1; /* 1 if exponent */
	ff->markerp = 1;
	ff->sign_exponent = 1;
	ff->sign = str->atsign;
}

static unicode fmtfloat_default_marker(Execute ptr)
{
	addr pos, check;

	/* default */
	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	/* single-flaot */
	GetConst(COMMON_SINGLE_FLOAT, &check);
	if (check == pos) return 'F';
	/* double-float */
	GetConst(COMMON_DOUBLE_FLOAT, &check);
	if (check == pos) return 'D';
	/* long-float */
	GetConst(COMMON_LONG_FLOAT, &check);
	if (check == pos) return 'L';
	/* short-float */
	GetConst(COMMON_SHORT_FLOAT, &check);
	if (check == pos) return 'S';
	/* error */
	fmte("Invalid *read-default-float-format* value ~S.", pos, NULL);

	return 'E';
}

static unicode fmtfloat_type_marker(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT: return 'F';
		case LISPTYPE_DOUBLE_FLOAT: return 'D';
		case LISPTYPE_LONG_FLOAT: return 'L';
		default: return 'E';
	}
}

static void fmtfloat_marker(fmtprint print,
		struct fmtchar *str, fmtfloat ff, addr pos)
{
	int marker1, marker2;
	unicode u;

	if (! fmtchar_nilp(print, str, 6, &u)) {
		ff->marker = u;
	}
	else {
		marker1 = fmtfloat_default_marker(print->ptr);
		marker2 = fmtfloat_type_marker(pos);
		ff->marker = (marker1 == marker2)? 'E': marker2;
	}
}

static void format_exponent_fixnum(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	format_exponent_single(print, ff, pos);
}

static void format_exponent_bignum(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_bignum_local(print->local, &pos, pos);
	format_exponent_single(print, ff, pos);
}

static void format_exponent_ratio(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_local(print->local, &pos, single_float_ratio(pos));
	format_exponent_single(print, ff, pos);
}

static void format_exponent_float(fmtprint print,
		struct fmtchar *str, fmtfloat ff, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			format_exponent_double(print, ff, pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			format_exponent_single(print, ff, pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			format_exponent_long(print, ff, pos);
			break;

		case LISPTYPE_FIXNUM:
			format_exponent_fixnum(print, ff, pos);
			break;

		case LISPTYPE_BIGNUM:
			format_exponent_bignum(print, ff, pos);
			break;

		case LISPTYPE_RATIO:
			format_exponent_ratio(print, ff, pos);
			break;

		default:
			formatabort(print->format, str->position);
			fmte("~~E argument ~S must be a real type.", pos, NULL);
			return;
	}
}

static int format_exponent(fmtprint print, struct fmtchar *str)
{
	addr control, symbol, pos;
	Execute ptr;
	struct fmtfloat_struct ff;

	if (7 < str->size) {
		formatabort_args(print, str, 7);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	ptr = print->ptr;
	push_close_control(ptr, &control);
	/* (let ((*print-escape* nil)) ...) */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	format_exponent_argument(print, str, &ff);
	fmtprint_pop(print, str, &pos);
	fmtfloat_marker(print, str, &ff, pos);
	format_exponent_float(print, str, &ff, pos);

	return free_control(ptr, control);
}

static void fmtfloat_e_general(fmtprint print,
		struct fmtchar *str, fmtfloat ff, unsigned index)
{
	fixnum value;

	if (fmtint_nilp(print, str, index, &value)) {
		ff->ep = 0;
		ff->e = 1;
	}
	else {
		if (value < 0) {
			formatabort_args(print, str, index);
			fmte("The parameter must be a positive integer.", NULL);
			return;
		}
		ff->ep = 1;
		ff->e = (size_t)(value? value: 1);
	}
}

static void format_general_argument(fmtprint print, struct fmtchar *str, fmtfloat ff)
{
	memset(ff, 0, sizeoft(struct fmtfloat_struct));
	fmtfloat_w(print, str, ff, 0);
	fmtfloat_d(print, str, ff, 1);
	fmtfloat_e_general(print, str, ff, 2);
	fmtfloat_k(print, str, ff, 3, 1);
	fmtfloat_overflowchar(print, str, ff, 4);
	fmtfloat_padchar(print, str, ff, 5);
	ff->sign = str->atsign;
}

static void format_general_single(fmtprint print, fmtfloat ff, addr pos)
{
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE)) {
		fmte("Invalid single-float.", pos, NULL);
		return;
	}
	fmtfloat_general(print->stream, ff, &dec);
}

static void format_general_double(fmtprint print, fmtfloat ff, addr pos)
{
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE)) {
		fmte("Invalid double-float.", pos, NULL);
		return;
	}
	fmtfloat_general(print->stream, ff, &dec);
}

static void format_general_long(fmtprint print, fmtfloat ff, addr pos)
{
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG)) {
		fmte("Invalid long-float.", pos, NULL);
		return;
	}
	fmtfloat_general(print->stream, ff, &dec);
}

static void format_general_fixnum(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	format_general_single(print, ff, pos);
}

static void format_general_bignum(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_bignum_local(print->local, &pos, pos);
	format_general_single(print, ff, pos);
}

static void format_general_ratio(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_local(print->local, &pos, single_float_ratio(pos));
	format_general_single(print, ff, pos);
}

static void format_general_float(fmtprint print,
		struct fmtchar *str, fmtfloat ff, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			format_general_double(print, ff, pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			format_general_single(print, ff, pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			format_general_long(print, ff, pos);
			break;

		case LISPTYPE_FIXNUM:
			format_general_fixnum(print, ff, pos);
			break;

		case LISPTYPE_BIGNUM:
			format_general_bignum(print, ff, pos);
			break;

		case LISPTYPE_RATIO:
			format_general_ratio(print, ff, pos);
			break;

		default:
			formatabort(print->format, str->position);
			fmte("~~G argument ~S must be a real type.", pos, NULL);
			return;
	}
}

static int format_general(fmtprint print, struct fmtchar *str)
{
	addr control, symbol, pos;
	Execute ptr;
	struct fmtfloat_struct ff;

	if (7 < str->size) {
		formatabort_args(print, str, 7);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	ptr = print->ptr;
	push_close_control(ptr, &control);
	/* (let ((*print-escape* nil)) ...) */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	format_general_argument(print, str, &ff);
	fmtprint_pop(print, str, &pos);
	fmtfloat_marker(print, str, &ff, pos);
	format_general_float(print, str, &ff, pos);

	return free_control(ptr, control);
}

static void format_dollars_single(fmtprint print, fmtfloat ff, addr pos)
{
	single_float value;
	struct fmtdecimal_struct dec;

	GetSingleFloat(pos, &value);
	ff->u.value_single = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_single_float(&dec, value, FMTFLOAT_ROUND_SINGLE)) {
		fmte("Invalid single-float.", pos, NULL);
		return;
	}
	fmtfloat_dollars(print->stream, ff, &dec);
}

static void format_dollars_double(fmtprint print, fmtfloat ff, addr pos)
{
	double_float value;
	struct fmtdecimal_struct dec;

	GetDoubleFloat(pos, &value);
	ff->u.value_double = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_double_float(&dec, value, FMTFLOAT_ROUND_DOUBLE)) {
		fmte("Invalid double-float.", pos, NULL);
		return;
	}
	fmtfloat_dollars(print->stream, ff, &dec);
}

static void format_dollars_long(fmtprint print, fmtfloat ff, addr pos)
{
	long_float value;
	struct fmtdecimal_struct dec;

	GetLongFloat(pos, &value);
	ff->u.value_long = value;
	ff->signbit = signbit(value)? 1: 0;

	if (fmtdecimal_long_float(&dec, value, FMTFLOAT_ROUND_LONG)) {
		fmte("Invalid long-float.", pos, NULL);
		return;
	}
	fmtfloat_dollars(print->stream, ff, &dec);
}

static void format_dollars_fixnum(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_fixnum_local(print->local, &pos, pos);
	format_dollars_single(print, ff, pos);
}

static void format_dollars_bignum(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_bignum_local(print->local, &pos, pos);
	format_dollars_single(print, ff, pos);
}

static void format_dollars_ratio(fmtprint print, fmtfloat ff, addr pos)
{
	/* force cast single-float */
	single_float_local(print->local, &pos, single_float_ratio(pos));
	format_dollars_single(print, ff, pos);
}

static void format_dollars_float(fmtprint print, struct fmtchar *str, fmtfloat ff)
{
	addr pos;

	fmtprint_pop(print, str, &pos);
	switch (GetType(pos)) {
		case LISPTYPE_DOUBLE_FLOAT:
			format_dollars_double(print, ff, pos);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			format_dollars_single(print, ff, pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			format_dollars_long(print, ff, pos);
			break;

		case LISPTYPE_FIXNUM:
			format_dollars_fixnum(print, ff, pos);
			break;

		case LISPTYPE_BIGNUM:
			format_dollars_bignum(print, ff, pos);
			break;

		case LISPTYPE_RATIO:
			format_dollars_ratio(print, ff, pos);
			break;

		default:
			formatabort(print->format, str->position);
			fmte("~~$ argument ~S must be a real type.", pos, NULL);
			return;
	}
}

static void fmtfloat_n(fmtprint print,
		struct fmtchar *str, fmtfloat ff, unsigned index)
{
	int check;
	fixnum value;

	check = fmtint_nilp(print, str, index, &value);
	if (! check && value < 0) {
		formatabort_args(print, str, index);
		fmte("The parameter must be a positive integer.", NULL);
		return;
	}
	ff->np = ! check;
	ff->n = check? 0: (size_t)value;
}

static void format_dollars_argument(fmtprint print, struct fmtchar *str, fmtfloat ff)
{
	memset(ff, 0, sizeoft(struct fmtfloat_struct));
	fmtfloat_d(print, str, ff, 0);
	fmtfloat_n(print, str, ff, 1);
	fmtfloat_w(print, str, ff, 2);
	fmtfloat_padchar(print, str, ff, 3);
	ff->sign = str->atsign;
	ff->markerp = str->colon;
}

static int format_dollars(fmtprint print, struct fmtchar *str)
{
	addr control, symbol;
	Execute ptr;
	struct fmtfloat_struct ff;

	if (4 < str->size) {
		formatabort_args(print, str, 4);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	ptr = print->ptr;
	push_close_control(ptr, &control);
	/* (let ((*print-escape* nil)) ...) */
	GetConst(SPECIAL_PRINT_ESCAPE, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	format_dollars_argument(print, str, &ff);
	format_dollars_float(print, str, &ff);

	return free_control(ptr, control);
}

static int format_terpri(fmtprint print, struct fmtchar *str)
{
	fixnum i, size;
	addr stream;

	if (1 < str->size) {
		formatabort_args(print, str, 1);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	fmtint_default(print, str, 0, &size, 1);
	if (size < 0) {
		formatabort_args(print, str, 0);
		fmte("The parameter must be a positive integer.", NULL);
		return 1;
	}
	stream = print->stream;
	for (i = 0; i < size; i++)
		terpri_stream(stream);

	return 0;
}

static int format_fresh_line(fmtprint print, struct fmtchar *str)
{
	fixnum i, size;
	addr stream;

	if (1 < str->size) {
		formatabort_args(print, str, 1);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	fmtint_default(print, str, 0, &size, 1);
	if (size < 0) {
		formatabort_args(print, str, 0);
		fmte("The parameter must be a positive integer.", NULL);
		return 1;
	}
	if (size) {
		stream = print->stream;
		fresh_line_stream(stream);
		for (i = 1; i < size; i++)
			terpri_stream(stream);
	}

	return 0;
}

static int format_pageout(fmtprint print, struct fmtchar *str)
{
	fixnum i, size;
	addr stream;

	if (1 < str->size) {
		formatabort_args(print, str, 1);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	fmtint_default(print, str, 0, &size, 1);
	if (size < 0) {
		formatabort_args(print, str, 0);
		fmte("The parameter must be a positive integer.", NULL);
		return 1;
	}
	stream = print->stream;
	for (i = 0; i < size; i++)
		pageout_stream(stream);

	return 0;
}

static int format_tilde(fmtprint print, struct fmtchar *str)
{
	fixnum size;

	if (1 < str->size) {
		formatabort_args(print, str, 1);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	fmtint_default(print, str, 0, &size, 1);
	if (size < 0) {
		formatabort_args(print, str, 0);
		fmte("The parameter must be a positive integer.", NULL);
		return 1;
	}
	write_times(print, '~', size);

	return 0;
}

static int format_line_escape(fmtprint print, struct fmtchar *str)
{
	if (0 < str->size) {
		formatabort_args(print, str, 0);
		fmte("Too many parameters.", NULL);
		return 1;
	}

	/*
	 * ~T -> delete space
	 * ~:T -> do-nothing
	 * ~@T -> output newline, delete space
	 * ~:@T -> output newline
	 */
	if (str->atsign) {
		/* output newline */
		terpri_stream(print->stream);
	}
	if (! str->colon) {
		/* delete space */
		print->delete_space = 1;
	}

	return 0;
}

static int format_tabulate(fmtprint print, struct fmtchar *str)
{
	fixnum column, colinc, now;

	if (2 < str->size) {
		formatabort_args(print, str, 2);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	fmtint_default(print, str, 0, &column, 1);
	fmtint_default(print, str, 1, &colinc, 1);
	if (column < 0) {
		formatabort_args(print, str, 0);
		fmte("The parameter must be greater than equal to 0.", NULL);
		return 1;
	}
	if (colinc < 0) {
		formatabort_args(print, str, 1);
		fmte("The parameter must be greater than equal to 0.", NULL);
		return 1;
	}

	if (str->colon) {
		if (str->atsign)
			pprint_tab_section_relative(print->ptr, print->stream, column, colinc);
		else
			pprint_tab_section(print->ptr, print->stream, column, colinc);
	}
	else {
		now = (fixnum)terpri_position_stream(print->stream);
		Check(now < 0, "cast error");
		if (str->atsign)
			pprint_tab_relative_force(print->stream, column, colinc, now);
		else
			pprint_tab_absolute_force(print->stream, column, colinc, now);
	}

	return 0;
}

static int format_ignore(fmtprint print, struct fmtchar *str)
{
	fixnum count;

	if (1 < str->size) {
		formatabort_args(print, str, 1);
		fmte("Too many ~~* argument.", NULL);
		return 1;
	}
	if (! str->atsign) {
		fmtint_default(print, str, 0, &count, 1);
		if (count < 0) {
			formatabort_args(print, str, 0);
			fmte("The parameter must be greater than equal to 0.", NULL);
			return 1;
		}
		if (! str->colon)
			fmtprint_forward(print, str, (size_t)count);
		else
			fmtprint_rollback(print, str, (size_t)count);
	}
	else {
		fmtint_default(print, str, 0, &count, 0);
		if (count < 0) {
			formatabort_args(print, str, 0);
			fmte("The parameter must be greater than equal to 0.", NULL);
			return 1;
		}
		if (! str->colon) {
			fmtprint_absolute(print, str, (size_t)count);
		}
		else {
			formatabort(print->format, str->position);
			fmte("The parameter don't accept both : and @ parameter (~:@*).", NULL);
			return 1;
		}
	}

	return 0;
}

static int fmtcall_args(Execute, addr, addr, struct fmtstack *);
static int format_indirection(fmtprint print, struct fmtchar *str)
{
	addr format, args;

	if (0 < str->size) {
		formatabort_args(print, str, 0);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	if (str->colon) {
		formatabort(print->format, str->colon_pos);
		fmte("~? argument don't accept : parameter (~:?).", NULL);
		return 1;
	}
	if (! str->atsign) {
		fmtprint_pop(print, str, &format);
		fmtprint_pop(print, str, &args);
		return format_stream_lisp(print->ptr, print->stream, format, args);
	}
	else {
		fmtprint_pop(print, str, &format);
		return fmtcall_args(print->ptr, print->stream, format, print->rest);
	}

	return 0;
}

static int format_newline(fmtprint print, struct fmtchar *str)
{
	if (str->colon && str->atsign)
		pprint_newline_common(print->ptr, pprint_newline_mandatory, print->stream);
	else if (str->colon)
		pprint_newline_common(print->ptr, pprint_newline_fill, print->stream);
	else if (str->atsign)
		pprint_newline_common(print->ptr, pprint_newline_miser, print->stream);
	else
		pprint_newline_common(print->ptr, pprint_newline_linear, print->stream);
	return 0;
}

static int format_write(fmtprint print, struct fmtchar *str)
{
	Execute ptr;
	addr control, pos;

	ptr = print->ptr;
	push_close_control(ptr, &control);
	if (str->colon) {
		push_pretty_print(ptr, 1);
	}
	if (str->atsign) {
		push_level_nil_print(ptr);
		push_length_nil_print(ptr);
	}
	fmtprint_pop(print, str, &pos);
	if (write_print(ptr, print->stream, pos))
		return 1;
	return free_control(ptr, control);
}

static int format_indent(fmtprint print, struct fmtchar *str)
{
	fixnum n;

	fmtint_default(print, str, 0, &n, 0);
	if (str->atsign)
		fmte("The format ~I don't accept @ operator (~@I).", NULL);
	pprint_indent_common(print->ptr, ! str->colon, n, print->stream);

	return 0;
}

static int fmtcall(fmtprint print, int *loop);
static int format_case(fmtprint print, struct fmtchar *str)
{
	int check;
	enum fmtcase now, next;
	struct fmtchar *list;

	if (0 < str->size) {
		formatabort_args(print, str, 0);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	/*
	 *  ~(		downcase
	 *  ~:(		all capitalize
	 *  ~@(		first capitalize
	 *  ~:@(	upcase
	 */
	if (str->colon && str->atsign)
		next = fmtcase_upcase;
	else if (str->colon)
		next = fmtcase_capitalize_all;
	else if (str->atsign) {
		print->first = 1;
		next = fmtcase_capitalize_first;
	}
	else
		next = fmtcase_downcase;

	/* update */
	now = print->conversion;
	list = print->list;
	print->conversion = next;
	print->list = str->option;
	/* call */
	if (fmtcall(print, &check)) return 1;
	/* rollback */
	print->conversion = now;
	print->list = list;

	return 0;
}

static int format_case_close(fmtprint print, struct fmtchar *str)
{
	/* do-nothing */
	return 0;
}

static int semicolon_default_p(fmtprint print, struct fmtchar *str)
{
	if (str->character != ';') return 0;
	if (0 < str->size) {
		formatabort_args(print, str, 1);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	if (str->atsign) {
		formatabort(print->format, str->atsign_pos);
		fmte("The operator ~; don't allow at-sign (~@;).", NULL);
		return 1;
	}
	return str->colon;
}

static int semicolon_normal_p(fmtprint print, struct fmtchar *str)
{
	if (str->character != ';') return 0;
	return ! semicolon_default_p(print, str);
}

static int format_condition_index(fmtprint print,
		struct fmtchar *str, fixnum index, int ignore)
{
	int check;
	fixnum i;
	struct fmtchar *list, *comm;

	/* select */
	if (index < 0)
		ignore = 1;
	comm = str->option;
	for (i = 0; ignore == 0 && i < index; ) {
		if (comm == NULL)
			return 0; /* do-nothing */
		if (semicolon_default_p(print, comm)) {
			comm = comm->next;
			break;
		}
		if (semicolon_normal_p(print, comm))
			i++;
		comm = comm->next;
	}

	/* update */
	list = print->list;
	print->list = comm;
	print->semicolon = 1;
	/* call */
	if (fmtcall(print, &check)) return 1;
	/* rollback */
	print->list = list;
	print->semicolon = 0;

	return 0;
}

static int format_condition_select(fmtprint print, struct fmtchar *str)
{
	int ignore;
	addr pos;
	fixnum index;

	if (1 < str->size) {
		formatabort_args(print, str, 1);
		fmte("Too many parameters.", NULL);
		return 1;
	}

	/* index */
	ignore = 0;
	if (fmtint_nilp(print, str, 0, &index)) {
		fmtprint_pop(print, str, &pos);
		if (! integerp(pos)) {
			formatabort(print->format, str->position);
			fmte("The argument ~S must be an integer.", pos, NULL);
			return 1;
		}
		if (GetType(pos) != LISPTYPE_FIXNUM)
			ignore = 1;
		GetFixnum(pos, &index);
	}

	/* print */
	return format_condition_index(print, str, index, ignore);
}

static int format_condition_boolean(fmtprint print, struct fmtchar *str)
{
	addr pos;

	if (2 < str->size) {
		formatabort_args(print, str, 2);
		fmte("Too many parameters.", NULL);
		return 1;
	}

	fmtprint_pop(print, str, &pos);
	return format_condition_index(print, str, (pos != Nil), 0);
}

static int format_condition_true(fmtprint print, struct fmtchar *str)
{
	addr pos;

	if (2 < str->size) {
		formatabort_args(print, str, 2);
		fmte("Too many parameters.", NULL);
		return 1;
	}

	fmtprint_peek(print, str, &pos);
	if (pos == Nil) {
		fmtprint_pop(print, str, &pos);
		/* do nothing */
		return 0;
	}
	else {
		return format_condition_index(print, str, 0, 0);
	}
}

static int format_condition(fmtprint print, struct fmtchar *str)
{
	/*
	 *  ~[		select
	 *  ~:[		boolean
	 *  ~@[		true
	 *  ~:@[	error
	 */
	if (str->colon && str->atsign) {
		formatabort(print->format, str->position);
		fmte("The parameter don't accept both : and @ parameter (~:@[).", NULL);
		return 1;
	}
	else if (str->colon)
		return format_condition_boolean(print, str);
	else if (str->atsign)
		return format_condition_true(print, str);
	else
		return format_condition_select(print, str);
}

static int format_condition_close(fmtprint print, struct fmtchar *str)
{
	/* do-nothing */
	return 0;
}

static void fmtprint_make(fmtprint print, Execute ptr, addr stream, addr format,
		struct fmtchar *list, struct fmtstack *args)
{
	clearpoint(print);
	print->ptr = ptr;
	print->local = ptr->local;
	print->loop = 1;
	print->word = 0;
	print->first = 1;
	print->conversion = fmtcase_normal;
	print->stream = stream;
	print->format = format;
	print->list = list;
	print->rest = args;
	print->last = 0;
}

static void fmtprint_copy(fmtprint dst, fmtprint src)
{
	fmtprint_make(dst, src->ptr, src->stream, src->format, src->list, src->rest);
}

static int fmtcall_list(Execute ptr,
		addr stream, addr format, struct fmtchar *list, struct fmtstack *args)
{
	int check;
	struct fmtprint_struct print;

	fmtprint_make(&print, ptr, stream, format, list, args);
	return fmtcall(&print, &check);
}

static int format_iteration_list(fmtprint print,
		struct fmtchar *str, struct fmtchar *list)
{
	int intp, forcep;
	fixnum index, i;
	addr pos;
	struct fmtstack args;

	fmtprint_pop(print, str, &pos);
	intp = fmtint_nilp(print, str, 0, &index);
	forcep = str->colon_close;

	args.root = pos;
	args.front = pos;
	args.index = 0;
	for (i = 0; ; i++) {
		if ((! intp) && (index <= i)) break;
		if ((! forcep) && args.front == Nil) break;
		forcep = 0;
		if (fmtcall_list(print->ptr, print->stream, print->format, list, &args))
			return 1;
	}

	return 0;
}

static int format_iteration_listargs(fmtprint print,
		struct fmtchar *str, struct fmtchar *list)
{
	int intp, forcep, check;
	fixnum index, i;
	addr car, cdr;
	struct fmtstack args;
	struct fmtprint_struct one;

	fmtprint_pop(print, str, &cdr);
	intp = fmtint_nilp(print, str, 0, &index);
	forcep = str->colon_close;

	for (i = 0; ; i++) {
		if ((! intp) && (index <= i)) break;
		if ((! forcep) && cdr == Nil) break;
		forcep = 0;

		getcons(cdr, &car, &cdr);
		args.root = car;
		args.front = car;
		args.index = 0;
		fmtprint_copy(&one, print);
		one.rest = &args;
		one.last = (cdr == Nil);
		one.list = list;
		if (fmtcall(&one, &check)) return 1;
		if (check) break;
	}

	return 0;
}

static int format_iteration_rest(fmtprint print,
		struct fmtchar *str, struct fmtchar *list)
{
	int intp, forcep;
	fixnum index, i;
	struct fmtstack *args;

	intp = fmtint_nilp(print, str, 0, &index);
	forcep = str->colon_close;

	args = print->rest;
	for (i = 0; ; i++) {
		if ((! intp) && (index <= i)) break;
		if ((! forcep) && args->front == Nil) break;
		forcep = 0;
		if (fmtcall_list(print->ptr, print->stream, print->format, list, args))
			return 1;
	}

	return 0;
}

static int format_iteration_restargs(fmtprint print,
		struct fmtchar *str, struct fmtchar *list)
{
	int intp, forcep, check;
	addr pos;
	fixnum index, i;
	struct fmtstack *print_args, args;
	struct fmtprint_struct one;

	intp = fmtint_nilp(print, str, 0, &index);
	forcep = str->colon_close;

	print_args = print->rest;
	for (i = 0; ; i++) {
		if ((! intp) && (index <= i)) break;
		if (print_args->front == Nil) {
			if (! forcep) break;
			pos = Nil;
		}
		else {
			fmtprint_pop(print, str, &pos);
		}
		forcep = 0;

		args.root = pos;
		args.front = pos;
		args.index = 0;
		fmtprint_copy(&one, print);
		one.rest = &args;
		one.last = (print->rest->front == Nil);
		one.list = list;
		if (fmtcall(&one, &check)) return 1;
		if (check) break;
	}

	return 0;
}

static struct fmtchar *make_iteration_empty(LocalRoot local,
		struct fmtchar *str, addr pos, addr *ret)
{
	addr queue;
	const char *last;

	charqueue_local(local, &queue, 0);
	push_charqueue_local(local, queue, '~');
	if (str->colon)
		push_charqueue_local(local, queue, ':');
	if (str->atsign)
		push_charqueue_local(local, queue, '@');
	push_charqueue_local(local, queue, '{');
	pushstring_charqueue_local(local, queue, pos);
	last = str->option->colon? "~:}": "~}";
	pushchar_charqueue_local(local, queue, last);
	make_charqueue_local(local, queue, &pos);
	*ret = pos;

	return fmtchar_make(local, pos)->option;
}

static int format_iteration_execute(fmtprint print,
		struct fmtchar *str, struct fmtchar *list)
{
	if (str->colon && str->atsign)
		return format_iteration_restargs(print, str, list);
	else if (str->colon)
		return format_iteration_listargs(print, str, list);
	else if (str->atsign)
		return format_iteration_rest(print, str, list);
	else
		return format_iteration_list(print, str, list);
}

static int format_iteration_string(fmtprint print, struct fmtchar *str)
{
	addr format, one;
	struct fmtchar *list;

	fmtprint_pop(print, str, &one);
	if (! stringp(one)) {
		formatabort(print->format, str->position);
		fmte("Invalid format string ~S.", one, NULL);
		return 1;
	}
	list = make_iteration_empty(print->local, str, one, &one);

	format = print->format;
	print->format = one;
	if (format_iteration_execute(print, str, list)) return 1;
	print->format = format;

	return 0;
}

static int format_iteration(fmtprint print, struct fmtchar *str)
{
	struct fmtchar *list;

	/*
	 *  "~{"		"~{ ~S ~S~}" '(a 1 b 2 c 3)
	 *  "~:{"		"~{ ~S ~S~}" '((a 1) (b 2) (c 3))
	 *  "~@{"		"~{ ~S ~S~}" 'a '1 'b '2 'c '3
	 *  "~:@{"		"~{ ~S ~S~}" '(a 1) '(b 2) '(c 3)
	 */
	if (1 < str->size) {
		formatabort_args(print, str, 1);
		fmte("Too many parameters.", NULL);
		return 1;
	}

	/* empty list */
	list = str->option;
	if (list->character == '}')
		return format_iteration_string(print, str);
	else
		return format_iteration_execute(print, str, list);
}

static int format_iteration_close(fmtprint print, struct fmtchar *str)
{
	/* do-nothing */
	return 0;
}

static int format_justification(fmtprint print, struct fmtchar *str)
{
	fixnum mincol, colinc, minpad;
	unicode padchar;

	if (4 < str->size) {
		formatabort_args(print, str, 4);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	fmtint_default(print, str, 0, &mincol, 0);
	fmtint_default(print, str, 1, &colinc, 1);
	fmtint_default(print, str, 2, &minpad, 0);
	fmtchar_default(print, str, 3, &padchar, ' ');
	if (mincol < 0) {
		formatabort_args(print, str, 0);
		fmte("The parameter must be a positive integer.", NULL);
		return 1;
	}
	if (colinc < 1) {
		formatabort_args(print, str, 1);
		fmte("The parameter must be greater than 1.", NULL);
		return 1;
	}
	if (minpad < 0) {
		formatabort_args(print, str, 2);
		fmte("The parameter must be a positive integer.", NULL);
		return 1;
	}

	fmte("TODO", NULL);
	return 1;
}

static int format_justification_close(fmtprint print, struct fmtchar *str)
{
	/* do-nothing */
	return 0;
}

static int format_upandout(fmtprint print, struct fmtchar *str)
{
	int ex1, ex2, ex3;
	fixnum v1, v2, v3;

	if (3 < str->size) {
		formatabort_args(print, str, 3);
		fmte("Too many parameters.", NULL);
		return 1;
	}
	v1 = v2 = v3 = 0;
	ex1 = ! fmtint_nilp(print, str, 0, &v1);
	ex2 = ! fmtint_nilp(print, str, 1, &v2);
	ex3 = ! fmtint_nilp(print, str, 2, &v3);
	if (str->atsign) {
		formatabort(print->format, str->atsign_pos);
		fmte("The operator ~^ don't allow at-sign (~@^).", NULL);
		return 1;
	}

	if (ex1 && ex2 && ex3) {
		if (v1 <= v2 && v2 <= v3)
			goto break_outside;
		return 0;
	}
	if (ex1 && ex2) {
		if (v1 == v2)
			goto break_outside;
		return 0;
	}
	if (ex1) {
		if (v1 == 0)
			goto break_outside;
		return 0;
	}
	else if (str->colon) {
		if (print->last)
			goto break_outside;
		return 0;
	}
	else {
		if (print->rest->front == Nil)
			goto break_outside;
		return 0;
	}

break_outside:
	print->loop = 0;
	print->loop_colon = str->colon;
	return 0;
}

static int format_close(fmtprint print, struct fmtchar *str)
{
	if (print->semicolon) {
		print->loop = 0;
	}
	else {
		formatabort(print->format, str->position);
		fmte("Invalid ~; operator.", NULL);
		return 1;
	}

	return 0;
}

static void fmtargs_object(fmtprint print, struct fmtchar *str,
		struct fmtargs *args, addr *ret)
{
	fixnum value;
	LocalRoot local;

	local = print->local;
	switch (args->type) {
		case fmtargs_nil:
			*ret = Nil;
			break;

		case fmtargs_integer:
			fixnum_local(local, ret, args->u.value);
			break;

		case fmtargs_character:
			character_local(local, ret, args->u.character);
			break;

		case fmtargs_argument:
			fmtprint_pop(print, str, ret);
			break;

		case fmtargs_count:
			getint_count(print, &value);
			fixnum_local(local, ret, value);
			break;

		default:
			formatabort(print->format, args->position);
			fmte("Invalid format parameter.", NULL);
			return;
	}
}

static int format_function(fmtprint print, struct fmtchar *str)
{
	addr symbol, pos, root, param;
	struct fmtargs *args;
	LocalRoot local;
	LocalStack stack;

	/* (symbol stream pos colon atsign ...) */
	local = print->local;
	args = str->root;
	symbol = str->symbol;
	fmtprint_pop(print, str, &pos);

	push_local(local, &stack);
	root = Nil;
	cons_local(local, &root, print->stream, root);
	cons_local(local, &root, pos, root);
	cons_local(local, &root, str->colon? T: Nil, root);
	cons_local(local, &root, str->atsign? T: Nil, root);
	for (; args; args = args->next) {
		fmtargs_object(print, str, args, &param);
		cons_local(local, &root, param, root);
	}
	nreverse_list_unsafe(&root, root);
	if (callclang_apply(print->ptr, &pos, symbol, root)) return 1;
	rollback_local(local, stack);

	return 0;
}

#define SetFormatCallTable(x,y) (FormatCallTable[(size_t)(x)] = (y))
_g void init_format(void)
{
	int i;

	for (i = 0; i < 0x80; i++)
		FormatCallTable[i] = NULL;
	SetFormatCallTable('\0', format_output);
	SetFormatCallTable('a', format_ascii);
	SetFormatCallTable('A', format_ascii);
	SetFormatCallTable('s', format_s_express);
	SetFormatCallTable('S', format_s_express);
	SetFormatCallTable('b', format_binary);
	SetFormatCallTable('B', format_binary);
	SetFormatCallTable('o', format_octal);
	SetFormatCallTable('O', format_octal);
	SetFormatCallTable('d', format_decimal);
	SetFormatCallTable('D', format_decimal);
	SetFormatCallTable('x', format_hexadecimal);
	SetFormatCallTable('X', format_hexadecimal);
	SetFormatCallTable('r', format_radix);
	SetFormatCallTable('R', format_radix);
	SetFormatCallTable('p', format_plural);
	SetFormatCallTable('P', format_plural);
	SetFormatCallTable('c', format_character);
	SetFormatCallTable('C', format_character);
	SetFormatCallTable('f', format_fixed);
	SetFormatCallTable('F', format_fixed);
	SetFormatCallTable('e', format_exponent);
	SetFormatCallTable('E', format_exponent);
	SetFormatCallTable('g', format_general);
	SetFormatCallTable('G', format_general);
	SetFormatCallTable('$', format_dollars);
	SetFormatCallTable('%', format_terpri);
	SetFormatCallTable('&', format_fresh_line);
	SetFormatCallTable('|', format_pageout);
	SetFormatCallTable('~', format_tilde);
	SetFormatCallTable('\n', format_line_escape);
	SetFormatCallTable('t', format_tabulate);
	SetFormatCallTable('T', format_tabulate);
	SetFormatCallTable('*', format_ignore);
	SetFormatCallTable('?', format_indirection);
	SetFormatCallTable('_', format_newline);
	SetFormatCallTable('w', format_write);
	SetFormatCallTable('W', format_write);
	SetFormatCallTable('i', format_indent);
	SetFormatCallTable('I', format_indent);
	SetFormatCallTable('(', format_case);
	SetFormatCallTable(')', format_case_close);
	SetFormatCallTable('[', format_condition);
	SetFormatCallTable(']', format_condition_close);
	SetFormatCallTable('{', format_iteration);
	SetFormatCallTable('}', format_iteration_close);
	SetFormatCallTable('<', format_justification);
	SetFormatCallTable('>', format_justification_close);
	SetFormatCallTable('^', format_upandout);
	SetFormatCallTable(';', format_close);
	SetFormatCallTable('/', format_function);
}


/*
 *  parse-format
 */
static int fmtcall(fmtprint print, int *loop)
{
	fmtcalltype call;
	struct fmtchar *list;
	size_t index;

	for (list = print->list; list && print->loop; list = list->next) {
		index = (size_t)list->character;
		if (index != 0)
			print->delete_space = 0;
		if (0x80 <= index) {
			fmte("Invalid format character.", NULL);
			return 1;
		}
		call = FormatCallTable[index];
		if (call == NULL) {
			formatabort(print->format, list->position + 1);
			fmte("Invalid format character.", NULL);
			return 1;
		}
		if (call(print, list)) return 1;
	}
	print->list = list;
	print->loop = 1;
	*loop = print->loop_colon;

	return 0;
}

static int fmtcall_args(Execute ptr, addr stream, addr format, struct fmtstack *args)
{
	struct fmtchar *comm = fmtchar_make(ptr->local, format);
	return fmtcall_list(ptr, stream, format, comm, args);
}


/*
 *  format
 */
_g int format_stream_args(Execute ptr, addr stream, addr format, addr args, addr *tail)
{
	struct fmtstack stack;

	stack.root = args;
	stack.front = args;
	stack.index = 0;
	if (fmtcall_args(ptr, stream, format, &stack)) {
		*tail = 0;
		return 1;
	}
	*tail = stack.front;
	exitpoint_stream(stream);

	return 0;
}

_g int format_string_args(Execute ptr, addr format, addr args, addr *ret, addr *tail)
{
	addr stream;

	open_output_string_stream(&stream, 0);
	if (format_stream_args(ptr, stream, format, args, tail)) {
		close_stream(stream);
		return 1;
	}
	string_stream_heap(stream, ret);
	close_stream(stream);

	return 0;
}

_g int format_args(Execute ptr,
		addr stream, addr format, addr args, addr *ret, addr *tail)
{
	if (stream == Nil) {
		return format_string_args(ptr, format, args, ret, tail);
	}
	else if (stream == T) {
		standard_output_stream(ptr, &stream);
		return format_stream_args(ptr, stream, format, args, tail);
	}
	else if (stringp(stream)) {
		Abort("TODO: make vector-push-extend stream.");
	}
	else {
		return format_stream_args(ptr, stream, format, args, tail);
	}

	return 0;
}

_g int format_stream_lisp(Execute ptr, addr stream, addr format, addr args)
{
	return format_stream_args(ptr, stream, format, args, &args);
}

_g int format_string_lisp(Execute ptr, addr format, addr args, addr *ret)
{
	return format_string_args(ptr, format, args, ret, &args);
}

_g int format_lisp(Execute ptr, addr stream, addr format, addr args, addr *ret)
{
	return format_args(ptr, stream, format, args, ret, &args);
}


/*
 *  format clang
 */
_g int format_stdarg(addr stream, const char *str, va_list args)
{
	addr format, list;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	strvect_char_local(local, &format, str);
	list_alloc_stdarg(local, &list, args);
	if (format_lisp(ptr, stream, format, list, &list)) return 1;
	rollback_local(local, stack);

	return 0;
}

_g int format_stream(addr stream, const char *str, ...)
{
	int check;
	va_list args;

	va_start(args, str);
	check = format_stdarg(stream, str, args);
	va_end(args);

	return check;
}

_g int format(const char *str, ...)
{
	int check;
	addr stream;
	va_list args;

	standard_output_stream(Execute_Thread, &stream);
	va_start(args, str);
	check = format_stdarg(stream, str, args);
	va_end(args);

	return check;
}

_g addr format_string_stdarg(LocalRoot local, const char *str, va_list args)
{
	addr stream, pos;

	open_output_string_stream(&stream, 0);
	if (format_stdarg(stream, str, args)) {
		fmte("Cannot execute format call.", NULL);
		return NULL;
	}
	string_stream_alloc(local, stream, &pos);
	close_stream(stream);

	return pos;
}

_g addr format_alloc(LocalRoot local, const char *str, ...)
{
	addr pos;
	va_list args;

	va_start(args, str);
	pos = format_string_stdarg(local, str, args);
	va_end(args);

	return pos;
}

_g addr format_local(LocalRoot local, const char *str, ...)
{
	addr pos;
	va_list args;

	Check(local == NULL, "local error");
	va_start(args, str);
	pos = format_string_stdarg(local, str, args);
	va_end(args);

	return pos;
}

_g addr format_heap(const char *str, ...)
{
	addr pos;
	va_list args;

	va_start(args, str);
	pos = format_string_stdarg(NULL, str, args);
	va_end(args);

	return pos;
}

_g addr fmtl(const char *str, ...)
{
	addr pos;
	va_list args;

	va_start(args, str);
	pos = format_string_stdarg(Local_Thread, str, args);
	va_end(args);

	return pos;
}


/*
 *  printf -> string
 */
_g addr localf_stdarg(LocalRoot local, const char *str, va_list args)
{
	int size;
	addr pos;
	char *ptr;
	va_list copy;

	Check(local == NULL, "local error");
	va_copy(copy, args);
	size = vsnprintc(NULL, 0, str, copy);
	if (size < 0) {
		fmte("vsnprintc error.", NULL);
		return NULL;
	}
	size++;

	ptr = (char *)lowlevel_local(local, size);
	size = vsnprintc(ptr, size, str, args);
	if (size < 0) {
		fmte("vsnprintc error.", NULL);
		return NULL;
	}
	strvect_char_local(local, &pos, ptr);

	return pos;
}

_g addr heapf_stdarg(const char *str, va_list args)
{
	int size;
	addr pos;
	char *ptr;
	LocalRoot local;
	LocalStack stack;
	va_list copy;

	va_copy(copy, args);
	size = vsnprintc(NULL, 0, str, copy);
	if (size < 0) {
		fmte("vsnprintc error.", NULL);
		return NULL;
	}
	size++;

	local = Local_Thread;
	push_local(local, &stack);
	ptr = (char *)lowlevel_local(local, size);
	size = vsnprintc(ptr, size, str, args);
	if (size < 0) {
		fmte("vsnprintc error.", NULL);
		return NULL;
	}
	strvect_char_heap(&pos, ptr);
	rollback_local(local, stack);

	return pos;
}

_g addr allocf_stdarg(LocalRoot local, const char *str, va_list args)
{
	if (local)
		return localf_stdarg(local, str, args);
	else
		return heapf_stdarg(str, args);
}

_g addr allocf(LocalRoot local, const char *str, ...)
{
	addr pos;
	va_list args;

	va_start(args, str);
	pos = allocf_stdarg(local, str, args);
	va_end(args);

	return pos;
}

_g addr localf(const char *str, ...)
{
	addr pos;
	va_list args;

	va_start(args, str);
	pos = localf_stdarg(Local_Thread, str, args);
	va_end(args);

	return pos;
}

_g addr heapf(const char *str, ...)
{
	addr pos;
	va_list args;

	va_start(args, str);
	pos = heapf_stdarg(str, args);
	va_end(args);

	return pos;
}

