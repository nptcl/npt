#include <string.h>
#include "bignum.h"
#include "bignum_cons.h"
#include "bignum_object.h"
#include "character.h"
#include "character_queue.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "format_float.h"
#include "format_parse.h"
#include "format_typedef.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"
#include "strvect.h"

/*
 *  fmtinput
 */
_g int format_abort_(addr format, size_t position, const char *str, va_list args)
{
	size_t i, size;
	addr pos, list, stream;

	copylocal_list_stdarg(NULL, &list, args);
	if (formatp(format))
		format_string_heap(&format, format);
	else
		copyheap(&format, format);
	conscar_heap(&pos, format);
	nconc2_unsafe(list, pos, &list);

	strvect_char_heap(&pos, str);
	open_output_string_stream(&stream, 0);
	Return(print_string_stream_(stream, pos));
	Return(fresh_line_stream_(stream, NULL));

	/* Format error: ~1,2,3,4,5,6,7,8F */
	Return(print_ascii_stream_(stream, "Format error: ~A"));
	string_length(format, &size);
	Return(fresh_line_stream_(stream, NULL));

	/*                      |          */
	Return(print_ascii_stream_(stream, "             "));
	string_length(format, &size);
	for (i = 0; i < position; i++) {
		Return(write_char_stream_(stream, ' '));
	}
	Return(write_char_stream_(stream, '^'));

	/* error */
	Return(string_stream_heap_(stream, &pos));
	return call_simple_error_(Execute_Thread, pos, list);
}

static int format_abort_va_(addr format, size_t index, const char *str, ...)
{
	va_list args;

	va_start(args, str);
	Return(format_abort_(format, index, str, args));
	va_end(args);

	return 0;
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

struct fmtargs {
	struct fmtargs *next;
	enum fmtargs_type type;
	size_t position;
	union format_union u;
};

struct fmtchar {
	unsigned colon : 1;
	unsigned atsign : 1;
	unsigned close_colon : 1;
	unsigned close_atsign : 1;
	unsigned option_check : 1;
	unsigned prefix : 1;
	unsigned suffix : 1;
	unsigned size;
	unicode character;
	enum FormatType type;
	struct fmtchar *next, *option;
	size_t position, colon_pos, atsign_pos, option_count;
	struct fmtargs *root, *tail;
	addr intern, format;
};

static int fmtinput_abort_(fmtinput input, const char *str, ...)
{
	va_list args;

	va_start(args, str);
	Return(format_abort_(input->format, input->index, str, args));
	va_end(args);

	return 0;
}

static int fmtinput_init_(fmtinput input, LocalRoot local, addr format)
{
#ifdef LISP_DEBUG
	aamemory(input, sizeoft(struct fmtinput_struct));
#endif
	input->local = local;
	input->format = format;
	input->index = 0;
	string_length(format, &(input->size));

	return 0;
}

static int fmtinput_peek_(fmtinput input, unicode *c, int *ret)
{
	if (input->size <= input->index)
		return Result(ret, 1);
	string_getc(input->format, input->index, c);
	return Result(ret, 0);
}

static int fmtinput_getc_(fmtinput input, unicode *c, int *ret)
{
	int check;

	Return(fmtinput_peek_(input, c, &check));
	if (! check)
		input->index++;

	return Result(ret, check);
}

static int fmtinput_getcheck_(fmtinput input, unicode *c)
{
	int check;

	Return(fmtinput_getc_(input, c, &check));
	if (check)
		return fmtinput_abort_(input, "Invalid format string.", NULL);

	return 0;
}


/*
 *  fmtchar-parse
 */
static void fmtchar_init(fmtinput input, struct fmtchar *str)
{
	clearpoint(str);
	str->type = FormatType_Error;
	str->position = input->index;
	str->format = input->format;
}

static struct fmtargs *fmtargs_make(fmtinput input, struct fmtchar *str)
{
	struct fmtargs *ptr;

	ptr = (struct fmtargs *)lowlevel_local(input->local, sizeoft(struct fmtargs));
	clearpoint(ptr);
	if (str->root)
		str->tail = str->tail->next = ptr;
	else
		str->root = str->tail = ptr;
	ptr->type = fmtargs_nil;
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

static int fmtchar_character_(fmtinput input, struct fmtchar *str)
{
	unicode u;
	struct fmtargs *argtype;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_character;
	Return(fmtinput_getcheck_(input, &u));
	argtype->u.character = u;

	return 0;
}

static int fmtchar_sign_(fmtinput input, int *sign, unicode u)
{
	if (*sign)
		return fmtinput_abort_(input, "Invalid sign character.", NULL);
	*sign = (int)u;

	return 0;
}

static void fmtchar_nil(fmtinput input, struct fmtchar *str)
{
	struct fmtargs *argtype;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_nil;
}

static int fmtchar_value_parse_(fmtinput input, int sign, addr queue, fixnum *ret)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;
	fixnum value;

	local = input->local;
	push_local(local, &stack);
	if (sign == '+')
		sign = signplus_bignum;
	if (sign == '-')
		sign = signminus_bignum;
	if (fixnum_cons_alloc(local, &pos, sign, queue)) {
		*ret = 0;
		return fmtinput_abort_(input, "Too large integer value.", NULL);
	}
	GetFixnum(pos, &value);
	rollback_local(local, stack);
	clear_bigcons(queue);

	return Result(ret, value);
}

static int fmtchar_value_(fmtinput input, struct fmtchar *str, int *sign, addr queue)
{
	struct fmtargs *argtype;
	fixnum value;

	argtype = fmtargs_make(input, str);
	argtype->type = fmtargs_integer;
	Return(fmtchar_value_parse_(input, *sign, queue, &value));
	argtype->u.value = value;
	*sign = 0;

	return 0;
}

static int fmtchar_nilcheck_(fmtinput input, int sign, addr queue, int *ret)
{
	if (! bigcons_empty_p(queue))
		return Result(ret, 0);
	if (sign == 0)
		return Result(ret, 1);
	/* error */
	*ret = 0;
	return fmtinput_abort_(input, "Invalid integer value.", NULL);
}

static int fmtchar_colon_(fmtinput input, struct fmtchar *str)
{
	if (str->colon)
		return fmtinput_abort_(input, "Format parameter ':' already supplies.", NULL);
	str->colon_pos = input->index;
	str->colon = 1;

	return 0;
}

static int fmtchar_atsign_(fmtinput input, struct fmtchar *str)
{
	if (str->atsign)
		return fmtinput_abort_(input, "Format parameter '@' already supplies.", NULL);
	str->atsign_pos = input->index;
	str->atsign = 1;

	return 0;
}

static enum FormatType FormatCharacter[0x80];
static int fmtchar_type_(fmtinput input, struct fmtchar *str)
{
	enum FormatType type;
	addr pos;
	unicode c;

	c = str->character;
	if (0x80 <= c)
		goto error;
	type = FormatCharacter[c];
	if (type == FormatType_Radix) {
		if (str->size == 0)
			type = FormatType_RadixText;
	}
	if (type != FormatType_Error) {
		str->type = type;
		return 0;
	}
	if (c == '>' || c == ')' || c == ']' || c == '}') {
		str->type = FormatType_Error;
		return 0;
	}

error:
	character_heap(&pos, c);
	str->type = FormatType_Error;
	return fmtinput_abort_(input, "Invalid operator character, ~S.", pos, NULL);
}

static int fmtchar_parse_function_(fmtinput input, struct fmtchar *str)
{
	unicode u;
	addr queue, package, pos;
	LocalRoot local;
	size_t size;

	local = input->local;
	charqueue_local(local, &queue, 0);

first:
	Return(fmtinput_getcheck_(input, &u));
	if (u == '/')
		goto first1;
	if (u == ':')
		goto colon1;
	u = toUpperUnicode(u);
	push_charqueue_local(local, queue, u);
	goto first;

first1:
	GetCharQueueSize(queue, &size);
	if (size == 0)
		goto error;
	strvect_char_local(local, &package, LISP_COMMON_USER);
	goto finish;

colon1:
	GetCharQueueSize(queue, &size);
	if (size == 0) {
		return fmtinput_abort_(input,
				"The package name is empty. (Don't use keyword)", NULL);
	}
	make_charqueue_local(local, queue, &package);
	clear_charqueue(queue);

	Return(fmtinput_getcheck_(input, &u));
	if (u == '/')
		goto error;
	if (u == ':')
		goto colon2;
	u = toUpperUnicode(u);
	push_charqueue_local(local, queue, u);
	goto colon3;

colon2:
	Return(fmtinput_getcheck_(input, &u));
	if (u == '/')
		goto error;
	if (u == ':')
		return fmtinput_abort_(input, "Invalid colon ::: separator.", NULL);
	u = toUpperUnicode(u);
	push_charqueue_local(local, queue, u);
	goto colon3;

colon3:
	Return(fmtinput_getcheck_(input, &u));
	if (u == '/')
		goto finish;
	u = toUpperUnicode(u);
	push_charqueue_local(local, queue, u);
	goto colon3;

error:
	return fmtinput_abort_(input, "The function name is empty.", NULL);

finish:
	make_charqueue_local(local, queue, &pos);
	cons_local(local, &pos, package, pos);
	str->intern = pos;
	return 0;
}

static int fmtchar_parse_(fmtinput input, struct fmtchar *str, addr queue)
{
	int check, sign, comma;
	unicode u;
	LocalRoot local;

	local = input->local;
	clear_bigcons(queue);
	fmtchar_init(input, str);
	sign = 0;
	comma = 0;
arguments:
	Return(fmtinput_getcheck_(input, &u));
	if (u == ':' || u == '@') {
		goto colon_atsign;
	}
	if (u == 'v' || u == 'V') {
		fmtchar_push(input, str);
		comma = 1;
		goto arguments;
	}
	if (u == '#') {
		fmtchar_sharp(input, str);
		comma = 1;
		goto arguments;
	}
	if (u == '\'') {
		Return(fmtchar_character_(input, str));
		comma = 1;
		goto arguments;
	}
	if (u == '-' || u == '+') {
		Return(fmtchar_sign_(input, &sign, u));
		comma = 1;
		goto arguments;
	}
	if (isDigitCase(u)) {
		push_bigcons(local, queue, 10, (unsigned)(u - '0'));
		comma = 1;
		goto arguments;
	}
	if (u == ',') {
		goto push_value;
	}
	goto finish;

push_value:
	Return(fmtchar_nilcheck_(input, sign, queue, &check));
	if (check) {
		if (comma == 0)
			fmtchar_nil(input, str);
	}
	else {
		Return(fmtchar_value_(input, str, &sign, queue));
	}
	comma = 0;
	goto arguments;

colon_atsign:
	Return(fmtchar_nilcheck_(input, sign, queue, &check));
	if (! check) {
		Return(fmtchar_value_(input, str, &sign, queue));
	}

colon_atsign_loop:
	if (u == ':') {
		Return(fmtchar_colon_(input, str));
	}
	else if (u == '@') {
		Return(fmtchar_atsign_(input, str));
	}
	else {
		goto finish;
	}
	Return(fmtinput_getcheck_(input, &u));
	goto colon_atsign_loop;

finish:
	Return(fmtchar_nilcheck_(input, sign, queue, &check));
	if (! check) {
		Return(fmtchar_value_(input, str, &sign, queue));
	}
	if (u == 0) {
		return fmtinput_abort_(input, "Invalid operator character, \\0.", NULL);
	}
	if (u == '/') {
		Return(fmtchar_parse_function_(input, str));
	}

	/* operator */
	str->character = toUpperUnicode(u);
	return fmtchar_type_(input, str);
}


/*
 *  fmtchar-group
 */
struct fmtroot {
	struct fmtchar *root, *tail;
	size_t start;
};

static struct fmtchar *fmtchar_local(fmtinput input)
{
	struct fmtchar *ptr;

	ptr = (struct fmtchar *)lowlevel_local(input->local, sizeoft(struct fmtchar));
	clearpoint(ptr);
	ptr->format = input->format;

	return ptr;
}

static void fmtroot_push(LocalRoot local, struct fmtroot *root, struct fmtchar *comm)
{
	if (root->root)
		root->tail = root->tail->next = comm;
	else
		root->tail = root->root = comm;
}

static void fmtroot_output(fmtinput input, struct fmtroot *root)
{
	struct fmtchar *comm;
	struct fmtargs *args;
	size_t p1, p2;
	LocalRoot local;

	local = input->local;
	p1 = root->start;
	p2 = input->index;
	if (p1 != p2) {
		comm = fmtchar_local(input);
		comm->character = 0;
		comm->type = FormatType_Output;
		comm->position = p2;
		/* args[0]: count */
		args = fmtargs_make(input, comm);
		args->type = fmtargs_index;
		args->u.index = (p2 - p1);
		/* args[1]: start */
		args = fmtargs_make(input, comm);
		args->type = fmtargs_index;
		args->u.index = p1;
		/* args[2]: end */
		args = fmtargs_make(input, comm);
		args->type = fmtargs_index;
		args->u.index = p2;
		/* result */
		fmtroot_push(local, root, comm);
		root->start = p2;
	}
}

static int fmtchar_loop_(fmtinput input, struct fmtchar **ret)
{
	int check;
	LocalRoot local;
	addr queue;
	unicode u;
	struct fmtchar *comm;
	struct fmtroot root;

	local = input->local;
	cleartype(root);
	bigcons_local(local, &queue);
	for (;;) {
		Return(fmtinput_peek_(input, &u, &check));
		if (check)
			break;
		if (u != '~') {
			input->index++;
			continue;
		}
		/* text output */
		fmtroot_output(input, &root);
		input->index++;
		/* operator */
		comm = fmtchar_local(input);
		Return(fmtchar_parse_(input, comm, queue));
		fmtroot_push(local, &root, comm);
		root.start = input->index;
	}
	/* text output */
	fmtroot_output(input, &root);

	return Result(ret, root.root);
}


/* fmtgroup */
struct fmtgroup {
	unsigned semicolon : 1;
	unsigned result_colon : 1;
	unsigned result_atsign : 1;
	struct fmtchar *root, *list;
	LocalRoot local;
	addr format;
	unicode a;
};

static int fmtchar_group_eof_(struct fmtgroup *group)
{
	addr pos;
	size_t size;

	character_local(group->local, &pos, group->a);
	string_length(group->format, &size);
	return format_abort_va_(group->format,
			size, "There is no parensis ~S.", pos, NULL);
}

static int fmtchar_group_justification_(addr format, struct fmtchar *list)
{
	if (list->close_colon) {
		list->type = FormatType_LogicalBlock;
		return 0;
	}

	/* Disable operator in Justification block.
	 *   ~W          write
	 *   ~_          pprint-newline
	 *   ~I          pprint-indent
	 *   ~:T         pprint-tab
	 *   ~<...~:>    pprint-logical-block
	 */
	for (list = list->option; list; list = list->next) {
		switch (list->type) {
			case FormatType_Write:
			case FormatType_ConditionalNewline:
			case FormatType_Indent:
			case FormatType_LogicalBlock:
				goto error;

			case FormatType_Tabulate:
				if (list->colon)
					goto error;
				break;

			default:
				break;
		}
	}
	return 0;

	/* error */
error:
	return format_abort_va_(format, list->position,
			"Don't use the operator in Justification block.", NULL);
}

static int fmtchar_group_type_(addr format, struct fmtchar *list)
{
	switch (list->type) {
		case FormatType_Justification:
			return fmtchar_group_justification_(format, list);

		case FormatType_Error:
			return fmte_("format type error", NULL);

		default:
			return 0;
	}
}

static int fmtchar_group_(struct fmtgroup *group);
static int fmtchar_group_set_(struct fmtgroup *group, unicode a, int semicolon)
{
	struct fmtchar *list, *next;
	struct fmtgroup backup;

	/* backup */
	backup = *group;
	list = group->list;
	next = list->next;
	/* parameter */
	group->semicolon = (semicolon != 0);
	group->result_colon = 0;
	group->result_atsign = 0;
	group->a = a;
	group->list = next;
	/* group */
	Return(fmtchar_group_(group));
	/* option */
	list->option = next;
	list->close_colon = group->result_colon;
	list->close_atsign = group->result_atsign;
	group->result_colon = 0;
	group->result_atsign = 0;
	Return(fmtchar_group_type_(group->format, list));
	/* next list */
	list = list->next = group->list;
	*group = backup;
	group->list = list;

	return 0;
}

static int fmtchar_group_close_(struct fmtgroup *group)
{
	addr pos;
	struct fmtchar *list;

	list = group->list;
	if (group->a != list->character) {
		character_heap(&pos, group->a);
		return format_abort_va_(group->format, list->position,
				"The close parensis ~S mismatch.", pos, NULL);
	}
	if (list->size) {
		character_heap(&pos, group->a);
		return format_abort_va_(group->format, list->position,
				"The close parensis ~S cannot use parameters.", pos, NULL);
	}
	group->list = list->next;
	list->next = NULL;
	group->result_colon = list->colon;
	group->result_atsign = list->atsign;

	return 0;
}

static int fmtchar_group_semicolon_(struct fmtgroup *group)
{
	if (! group->semicolon) {
		return format_abort_va_(group->format, group->list->position,
				"Invalid ~~; parameter.", NULL);
	}
	group->list = group->list->next;

	return 0;
}

static int fmtchar_group_(struct fmtgroup *group)
{
	while (group->list) {
		switch (group->list->character) {
			case '(':
				Return(fmtchar_group_set_(group, ')', 0));
				break;

			case '[': /* semicolon */
				Return(fmtchar_group_set_(group, ']', 1));
				break;

			case '{':
				Return(fmtchar_group_set_(group, '}', 0));
				break;

			case '<': /* semicolon */
				Return(fmtchar_group_set_(group, '>', 1));
				break;

			case ')':
			case ']':
			case '}':
			case '>':
				return fmtchar_group_close_(group);

			case ';':
				Return(fmtchar_group_semicolon_(group));
				break;

			default:
				group->list = group->list->next;
				break;
		}
	}
	if (group->a != 0) {
		Return(fmtchar_group_eof_(group));
	}

	return 0;
}

static struct fmtchar *fmtchar_empty(fmtinput input)
{
	struct fmtchar *comm;
	struct fmtargs *args;

	comm = fmtchar_local(input);
	comm->character = 0;
	comm->type = FormatType_Output;
	comm->position = 0;
	/* args[0]: count */
	args = fmtargs_make(input, comm);
	args->type = fmtargs_index;
	args->u.index = 0;
	/* args[1]: start */
	args = fmtargs_make(input, comm);
	args->type = fmtargs_index;
	args->u.index = 0;
	/* args[2]: end */
	args = fmtargs_make(input, comm);
	args->type = fmtargs_index;
	args->u.index = 0;

	return comm;
}

static int fmtchar_make_(LocalRoot local, addr format, struct fmtchar **ret)
{
	struct fmtinput_struct input;
	struct fmtchar *list;
	struct fmtgroup group;

	/* loop */
	Return(fmtinput_init_(&input, local, format));
	Return(fmtchar_loop_(&input, &list));
	if (list == NULL)
		list = fmtchar_empty(&input);

	/* group */
	cleartype(group);
	group.local = local;
	group.format = format;
	group.root = group.list = list;
	Return(fmtchar_group_(&group));

	return Result(ret, list);
}


/*
 *  format write
 */
static int format_size_(struct fmtchar *list, size_t *ret);
static int format_write_(struct fmtchar *list, byte *ptr, size_t *ret);

#define FormatOperatorSize (sizeoft(struct format_operator))
#define FormatArgumentSize (sizeoft(struct format_argument))
#define FormatByteSize(x) (FormatOperatorSize + FormatArgumentSize * (x))

static struct format_operator *format_write_operator(
		struct fmtchar *list, byte *ptr, size_t args)
{
	struct format_operator *str;

	str = (struct format_operator *)ptr;
	str->type = list->type;
	str->size = FormatByteSize(args);
	str->colon = list->colon;
	str->atsign = list->atsign;
	str->close_colon = list->close_colon;
	str->close_atsign = list->close_atsign;
	str->option_check = list->option_check;
	str->prefix = list->prefix;
	str->suffix = list->suffix;
	str->args_size = args;
	str->position = list->position;
	str->colon_pos = list->colon_pos;
	str->atsign_pos = list->atsign_pos;

	return str;
}

static byte *format_write_body(byte *ptr, size_t index)
{
	return ptr + FormatByteSize(index);
}

static struct format_argument *format_write_argument(
		struct fmtargs **list, byte *ptr, size_t index)
{
	struct format_argument *str;
	struct fmtargs *arg;

	Check(list == NULL, "list error");
	arg = *list;
	str = (struct format_argument *)format_write_body(ptr, index);
	str->type = arg? arg->type: fmtargs_nil;
	str->position = arg? arg->position: 0;
	cleartype(str->u);
	switch (str->type) {
		case fmtargs_integer:
			str->u.value = arg->u.value;
			break;

		case fmtargs_character:
			str->u.character = arg->u.character;
			break;

		case fmtargs_index:
			str->u.index = arg->u.index;
			break;

		default:
			break;
	}
	if (arg)
		*list = arg->next;

	return str;
}

/* Output */
static int format_size_Output(struct fmtchar *list, size_t *ret)
{
	struct fmtargs *root;
	size_t count;
#ifdef LISP_DEBUG
	size_t a, b;
#endif

	Check(list->size != 3, "size error");
	/* count */
	root = list->root;
	Check(root->type != fmtargs_index, "argument count error");
	count = root->u.index;
#ifdef LISP_DEBUG
	/* start */
	root = root->next;
	Check(root->type != fmtargs_index, "argument start error");
	a = root->u.index;
	/* end */
	root = root->next;
	Check(root->type != fmtargs_index, "argument end error");
	b = root->u.index;
	/* size */
	Check(b < a, "size1 error");
	Check(count != (b - a), "size2 error");
#endif
	*ret = FormatByteSize(3) + count * sizeoft(unicode);

	return 0;
}

static int format_write_Output(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct fmtargs *root;
	size_t count, a, b, i;
	unicode c, *body;

	/* structure */
	str = format_write_operator(list, ptr, 3);
	/* count */
	root = list->root;
	count = root->u.index;
	format_write_argument(&root, ptr, 0);
	/* start */
	a = root->u.index;
	format_write_argument(&root, ptr, 1);
	/* end */
	b = root->u.index;
	format_write_argument(&root, ptr, 2);
	/* body */
	format = list->format;
	str->size += count * sizeoft(unicode);
	body = (unicode *)format_write_body(ptr, 3);
	for (i = 0; a < b; a++) {
		string_getc(format, a, &c);
		body[i++] = c;
	}
	/* result */
	return Result(ret, str->size);
}


/* Format */
static int format_size_Format(struct fmtchar *list, size_t *ret)
{
	size_t size;

	string_length(list->format, &size);
	*ret = FormatByteSize(1) + size * sizeoft(unicode);

	return 0;
}

static struct format_operator *format_write_empty_structure(byte *ptr,
		enum FormatType type, size_t args)
{
	struct format_operator *str;

	str = (struct format_operator *)ptr;
	str->type = type;
	str->size = FormatByteSize(args);
	str->colon = 0;
	str->atsign = 0;
	str->close_colon = 0;
	str->close_atsign = 0;
	str->option_check = 0;
	str->prefix = 0;
	str->suffix = 0;
	str->args_size = args;
	str->position = 0;
	str->colon_pos = 0;
	str->atsign_pos = 0;

	return str;
}

static int format_write_Format(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct fmtargs args, *root;
	size_t size, i;
	unicode c, *body;

	/* structure */
	str = format_write_empty_structure(ptr, FormatType_Format, 1);
	format = list->format;
	string_length(format, &size);
	/* start */
	cleartype(args);
	args.type = fmtargs_index;
	args.u.index = size;
	root = &args;
	format_write_argument(&root, ptr, 0);
	/* body */
	str->size += size * sizeoft(unicode);
	body = (unicode *)format_write_body(ptr, 1);
	for (i = 0; i < size; i++) {
		string_getc(format, i, &c);
		body[i] = c;
	}
	/* result */
	return Result(ret, str->size);
}


/* End */
static int format_size_End(size_t *ret)
{
	return Result(ret, FormatOperatorSize);
}

static int format_write_End(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	str = format_write_empty_structure(ptr, FormatType_End, 0);
	return Result(ret, str->size);
}


/* default parameter */
static int format_size_error_(struct fmtchar *list, size_t index)
{
	if (index < list->size) {
		return format_abort_va_(list->format,
				list->position, "Too many parameters.", NULL);
	}

	return 0;
}

static int format_size_index_(struct fmtchar *list, size_t *ret, size_t index)
{
	Return(format_size_error_(list, index));
	return Result(ret, FormatByteSize(index));
}
static int format_size_size0(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 0);
}
static int format_size_size1(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 1);
}
static int format_size_size2(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 2);
}
static int format_size_size3(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 3);
}
static int format_size_size4(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 4);
}
static int format_size_size5(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 5);
}
static int format_size_size7(struct fmtchar *list, size_t *ret)
{
	return format_size_index_(list, ret, 7);
}


/* ~A */
static int format_argument_integer_check_(addr format,
		struct format_argument *ptr, fixnum defvalue,
		fixnum *value, int *ret)
{
	switch (ptr->type) {
		case fmtargs_nil:
			ptr->type = fmtargs_integer;
			ptr->u.value = defvalue;
			*value = defvalue;
			return Result(ret, 1);

		case fmtargs_integer:
			*value = ptr->u.value;
			return Result(ret, 1);

		case fmtargs_argument:
		case fmtargs_count:
			return Result(ret, 0);

		default:
			*value = 0;
			*ret = 0;
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}
}

static int format_argument_integer_(addr format,
		struct format_argument *ptr, fixnum value)
{
	int check;
	return format_argument_integer_check_(format, ptr, value, &value, &check);
}

static int format_argument_less_(addr format,
		struct format_argument *ptr, fixnum defvalue, fixnum value)
{
	int check;
	fixnum less;

	Return(format_argument_integer_check_(format, ptr, defvalue, &less, &check));
	if (check) {
		if (less < value) {
			return format_abort_va_(format, ptr->position,
					"The parameter must be greater than ~A.",
					fixnumh(value), NULL);
		}
	}

	return 0;
}

static int format_argument_integer_nil_(addr format, struct format_argument *ptr)
{
	switch (ptr->type) {
		case fmtargs_nil:
		case fmtargs_integer:
		case fmtargs_argument:
		case fmtargs_count:
			return 0;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}
}

static int format_argument_less_nil_(addr format,
		struct format_argument *ptr, fixnum check)
{
	switch (ptr->type) {
		case fmtargs_integer:
			if (ptr->u.value < check) {
				return format_abort_va_(format, ptr->position,
						"The parameter must be greater than ~A.",
						fixnumh(check), NULL);
			}
			break;

		case fmtargs_nil:
		case fmtargs_argument:
		case fmtargs_count:
			break;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}

	return 0;
}

static int format_argument_radix_(addr format, struct format_argument *ptr)
{
	switch (ptr->type) {
		case fmtargs_integer:
			if (! isBaseChar(ptr->u.value)) {
				return format_abort_va_(format, ptr->position,
						"The parameter must be an integer between 2 and 36.", NULL);
			}
			break;

		case fmtargs_argument:
		case fmtargs_count:
			break;

		case fmtargs_nil:
			return format_abort_va_(format, ptr->position,
					"There is no radix parameter.", NULL);

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}

	return 0;
}

static int format_argument_character_(addr format,
		struct format_argument *ptr, unicode value)
{
	switch (ptr->type) {
		case fmtargs_nil:
			ptr->type = fmtargs_character;
			ptr->u.value = value;
			break;

		case fmtargs_character:
		case fmtargs_argument:
		case fmtargs_count:
			break;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be a character.", NULL);
	}

	return 0;
}

static int format_argument_character_nil_(addr format, struct format_argument *ptr)
{
	switch (ptr->type) {
		case fmtargs_nil:
		case fmtargs_character:
		case fmtargs_argument:
		case fmtargs_count:
			return 0;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be a character.", NULL);
	}
}


/* ~A, ~S */
static int format_write_Aesthetic(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 4);
	format = list->format;
	/* mincol */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(format, arg, 0, 0));
	/* colinc */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_less_(format, arg, 1, 1));
	/* minpad */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_less_(format, arg, 0, 0));
	/* padchar */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_character_(format, arg, ' '));
	/* result */
	return Result(ret, str->size);
}


/* ~B, ~O, ~D, ~X */
static int format_write_Binary(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 4);
	format = list->format;
	/* mincol */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(format, arg, 0, 0));
	/* padchar */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_character_(format, arg, ' '));
	/* commachar */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_character_(format, arg, ','));
	/* commainterval */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_less_(format, arg, 3, 1));
	/* result */
	return Result(ret, str->size);
}


/* ~nR */
static int format_write_Radix(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 5);
	format = list->format;
	/* radix */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_radix_(format, arg));
	/* mincol */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_less_(format, arg, 0, 0));
	/* padchar */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_character_(format, arg, ' '));
	/* commachar */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_character_(format, arg, ','));
	/* commainterval */
	arg = format_write_argument(&root, ptr, 4);
	Return(format_argument_less_(format, arg, 3, 1));
	/* result */
	return Result(ret, str->size);
}

/* ~R, ~P, ~C, ~\n */
static int format_write_Empty(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	str = format_write_operator(list, ptr, 0);
	return Result(ret, str->size);
}


/* ~F */
static int format_argument_wdn_(addr format, struct format_argument *ptr)
{
	switch (ptr->type) {
		case fmtargs_integer:
			if (ptr->u.value < 0) {
				return format_abort_va_(format, ptr->position,
						"The parameter must be a positive integer.", NULL);
			}
			break;

		case fmtargs_nil:
		case fmtargs_argument:
		case fmtargs_count:
			break;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}

	return 0;
}

static int format_write_Fixed(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* colon */
	if (list->colon) {
		return format_abort_va_(list->format, list->colon_pos,
				"The operator cannot use : parameter.", NULL);
	}
	/* parameter */
	str = format_write_operator(list, ptr, 5);
	format = list->format;
	/* w */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_wdn_(format, arg));
	/* d */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_wdn_(format, arg));
	/* k */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_integer_(format, arg, 0)); /* fixed = 0 */
	/* overflowchar */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_character_nil_(format, arg));
	/* padchar */
	arg = format_write_argument(&root, ptr, 4);
	Return(format_argument_character_(format, arg, ' '));
	/* result */
	return Result(ret, str->size);
}


/* ~E */
static int format_write_Exponent(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* colon */
	if (list->colon) {
		return format_abort_va_(list->format, list->colon_pos,
				"The operator cannot use : parameter.", NULL);
	}
	/* parameter */
	str = format_write_operator(list, ptr, 7);
	format = list->format;
	/* w */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_wdn_(format, arg));
	/* d */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_wdn_(format, arg));
	/* e */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_less_(format, arg, 1, 0));
	if (arg->u.value == 0)
		arg->u.value = 1;
	/* k */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_integer_(format, arg, 1)); /* exponent = 1 */
	/* overflowchar */
	arg = format_write_argument(&root, ptr, 4);
	Return(format_argument_character_nil_(format, arg));
	/* padchar */
	arg = format_write_argument(&root, ptr, 5);
	Return(format_argument_character_(format, arg, ' '));
	/* exponentchar */
	arg = format_write_argument(&root, ptr, 6);
	Return(format_argument_character_nil_(format, arg));
	/* result */
	return Result(ret, str->size);
}


/* ~G */
static int format_argument_e_general_(addr format, struct format_argument *ptr)
{
	switch (ptr->type) {
		case fmtargs_integer:
			if (ptr->u.value < 0) {
				return format_abort_va_(format, ptr->position,
						"The parameter must be a positive integer.", NULL);
			}
			if (ptr->u.value == 0)
				ptr->u.value = 1;
			break;

		case fmtargs_nil:
		case fmtargs_argument:
		case fmtargs_count:
			break;

		default:
			return format_abort_va_(format, ptr->position,
					"The format parameter must be an integer.", NULL);
	}

	return 0;
}

static int format_write_General(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* colon */
	if (list->colon) {
		return format_abort_va_(list->format, list->colon_pos,
				"The operator cannot use : parameter.", NULL);
	}
	/* parameter */
	str = format_write_operator(list, ptr, 7);
	format = list->format;
	/* w */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_wdn_(format, arg));
	/* d */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_wdn_(format, arg));
	/* e */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_e_general_(format, arg));
	/* k */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_integer_(format, arg, 1)); /* general = 1 */
	/* overflowchar */
	arg = format_write_argument(&root, ptr, 4);
	Return(format_argument_character_nil_(format, arg));
	/* padchar */
	arg = format_write_argument(&root, ptr, 5);
	Return(format_argument_character_(format, arg, ' '));
	/* exponentchar */
	arg = format_write_argument(&root, ptr, 6);
	Return(format_argument_character_nil_(format, arg));
	/* result */
	return Result(ret, str->size);
}


/* ~$ */
static int format_write_Monetary(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 4);
	format = list->format;
	/* d */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_wdn_(format, arg));
	/* n */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_wdn_(format, arg));
	/* w */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_e_general_(format, arg));
	/* padchar */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_character_(format, arg, ' '));
	/* result */
	return Result(ret, str->size);
}


/* ~%, ~&, ~|, ~~ */
static int format_write_Newline(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 1);
	/* times */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(list->format, arg, 1, 0));
	/* colon */
	if (list->colon) {
		return format_abort_va_(list->format, list->colon_pos,
				"The operator cannot use : parameter.", NULL);
	}
	/* atsign */
	if (list->atsign) {
		return format_abort_va_(list->format, list->atsign_pos,
				"The operator cannot use @ parameter.", NULL);
	}
	/* result */
	return Result(ret, str->size);
}


/* ~T */
static int format_write_Tabulate(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 2);
	format = list->format;
	/* column */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(format, arg, 1, 0));
	/* colinc */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_less_(format, arg, 1, 0));
	/* result */
	return Result(ret, str->size);
}


/* ~T */
static int format_write_GoTo(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	str = format_write_operator(list, ptr, 1);
	/* count */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(list->format, arg, list->atsign? 0: 1, 0));
	/* result */
	return Result(ret, str->size);
}


/* ~? */
static int format_write_Recursive(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;

	str = format_write_operator(list, ptr, 0);
	/* colon */
	if (list->colon) {
		return format_abort_va_(list->format, list->colon_pos,
				"The operator cannot use : parameter.", NULL);
	}
	/* result */
	return Result(ret, str->size);
}


/* ~? */
static int format_write_Indent(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* atsign */
	if (list->atsign) {
		return format_abort_va_(list->format, list->atsign_pos,
				"The operator cannot use @ parameter.", NULL);
	}
	/* parameter */
	str = format_write_operator(list, ptr, 1);
	/* indent */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_integer_(list->format, arg, 0));
	/* result */
	return Result(ret, str->size);
}


/* ~( */
static int format_size_Case(struct fmtchar *list, size_t *ret)
{
	struct fmtchar *x;
	size_t size, value;

	/* close parensis */
	if (list->close_colon) {
		return format_abort_va_(list->format, list->position,
				"Cannot use : parameter at the close parensis.", NULL);
	}
	if (list->close_atsign) {
		return format_abort_va_(list->format, list->position,
				"Cannot use @ parameter at the close parensis.", NULL);
	}

	/* argument */
	Return(format_size_index_(list, &size, 0));
	/* body */
	for (x = list->option; x; x = x->next) {
		if (x == NULL) {
			return format_abort_va_(x->format, x->position,
					"There is no close parensis ~~).", NULL);
		}
		if (x->type == FormatType_ClauseSeparator) {
			return format_abort_va_(x->format, x->position,
					"Cannot use ~~; operator in the ~~(...~~).", NULL);
		}
		if (x->character == ')')
			break;
		Return(format_size_(x, &value));
		size += value;
	}
	/* end */
	Return(format_size_End(&value));
	size += value;
	/* result */
	return Result(ret, size);
}

static int format_write_Case(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct fmtchar *x;
	size_t size, value;

	str = format_write_operator(list, ptr, 0);
	size = str->size;
	/* body */
	for (x = list->option; x->character != ')'; x = x->next) {
		Return(format_write_(x, ptr + size, &value));
		size += value;
	}
	/* end */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	*ret = str->size = size;
	return 0;
}


/* ~[, ~] */
static int format_size_Condition_body_(struct fmtchar *list,
		int pcheck, int *rfinal, size_t *rcount, size_t *rsize)
{
	int check, final;
	struct fmtchar *x;
	size_t size, value, count;

	size = 0;
	count = 0;
	final = 0;
	check = 1;
	for (x = list->option; ; x = x->next) {
		if (x == NULL)
			goto error1;
		if (x->character == ']')
			break;
		if (check) {
			count++;
			check = 0;
		}
		if (x->type == FormatType_ClauseSeparator) {
			if (x->atsign)
				goto error2;
			if (x->size)
				goto error3;
			if (final)
				goto error4;
			if (x->colon) {
				if (pcheck)
					goto error5;
				final = 1;
			}
			check = 1;
		}
		Return(format_size_(x, &value));
		size += value;
	}
	if (check)
		count++;
	/* result */
	*rfinal = final;
	*rcount = count;
	*rsize = size;
	return 0;

error1:
	*rfinal = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"There is no close parensis ~~].", NULL);

error2:
	*rfinal = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->atsign_pos,
			"Cannot use @ parameter at ~~; operator.", NULL);

error3:
	*rfinal = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"Cannot use prefix parameters at ~~; operator.", NULL);

error4:
	*rfinal = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"After ~~:; clause must be a last position.", NULL);

error5:
	*rfinal = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->colon_pos,
			"Cannot use ~~:; operator at the ~~:[ or ~~@[ operator.", NULL);
}

static int format_size_Condition(struct fmtchar *list, size_t *ret)
{
	int colon, atsign, any, final;
	size_t size, value, count;

	/* close parensis */
	if (list->close_colon)
		goto error1;
	if (list->close_atsign)
		goto error2;
	/* arguments */
	colon = list->colon;
	atsign = list->atsign;
	any = colon || atsign;
	Return(format_size_index_(list, &size, any? 0: 1));
	if (colon && atsign)
		goto error3;
	/* body */
	Return(format_size_Condition_body_(list, any, &final, &count, &value));
	if (colon && count != 2)
		goto error4;
	if (atsign && count != 1)
		goto error5;
	size += value;
	/* end */
	Return(format_size_End(&value));
	size += value;
	/* count */
	list->option_check = (final != 0);
	list->option_count = count;
	size += sizeoft(size_t) * (count + 1);
	/* result */
	return Result(ret, size);

error1:
	*ret = 0;
	return format_abort_va_(list->format, list->position,
			"Cannot use : parameter at the close parensis.", NULL);

error2:
	*ret = 0;
	return format_abort_va_(list->format, list->position,
			"Cannot use @ parameter at the close parensis.", NULL);

error3:
	*ret = 0;
	return format_abort_va_(list->format, list->position,
			"The parameter don't accept both : and @ parameter (~~:@[).", NULL);

error4:
	*ret = 0;
	return format_abort_va_(list->format, list->position,
			"Count of clauses must be a 2 in ~~:[...~~].", NULL);

error5:
	*ret = 0;
	return format_abort_va_(list->format, list->position,
			"Count of clauses must be a 1 in ~~@[...~~].", NULL);
}

static int format_write_Condition(struct fmtchar *list, byte *ptr, size_t *ret)
{
	int colon, atsign, any, check;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;
	struct fmtchar *x;
	size_t size, value, *array;

	/* error */
	colon = list->colon;
	atsign = list->atsign;
	any = colon || atsign;
	str = format_write_operator(list, ptr, any? 0: 1);
	Check(colon && atsign, ":@[ error");
	/* argument */
	if (any == 0) {
		root = list->root;
		arg = format_write_argument(&root, ptr, 0);
		Return(format_argument_integer_nil_(list->format, arg));
	}
	/* condition array */
	size = str->size;
	array = (size_t *)(ptr + size);
	value = list->option_count;
	*(array++) = value;
	size += sizeoft(size_t) * (value + 1);
	/* body */
	check = 1;
	for (x = list->option; x->character != ']'; x = x->next) {
		if (check) {
			*(array++) = size;
			check = 0;
		}
		if (x->type == FormatType_ClauseSeparator)
			check = 1;
		Return(format_write_(x, ptr + size, &value));
		size += value;
	}
	if (check)
		*(array++) = size;
	/* end */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	*ret = str->size = size;
	return 0;
}


/* ~{, ~} */
static int format_size_Iteration(struct fmtchar *list, size_t *ret)
{
	int check;
	struct fmtchar *x;
	size_t size, value;

	/* close parensis */
	if (list->close_atsign) {
		return format_abort_va_(list->format, list->position,
				"Cannot use @ parameter at the close parensis.", NULL);
	}
	/* argument */
	Return(format_size_index_(list, &size, 1));
	/* body */
	check = 1;
	for (x = list->option; ; x = x->next) {
		if (x == NULL) {
			return format_abort_va_(x->format, x->position,
					"There is no close parensis ~~}.", NULL);
		}
		if (x->character == '}')
			break;
		if (x->type == FormatType_ClauseSeparator) {
			return format_abort_va_(x->format, x->position,
					"Cannot use ~~; operator in the ~~{...~~}.", NULL);
		}
		Return(format_size_(x, &value));
		size += value;
		check = 0;
	}
	/* end */
	Return(format_size_End(&value));
	size += value;
	/* result */
	list->option_check = check;
	return Result(ret, size);
}

static int format_write_Iteration(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtchar *x;
	struct fmtargs *root;
	size_t size, value;

	str = format_write_operator(list, ptr, 1);
	size = str->size;
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_nil_(list->format, arg, 0));
	/* body */
	for (x = list->option; x->character != '}'; x = x->next) {
		Return(format_write_(x, ptr + size, &value));
		size += value;
	}
	/* end */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	*ret = str->size = size;
	return 0;
}


/* Justification: ~<, ~> */
static int format_size_Justification_body_(struct fmtchar *list,
		int *rfirst, size_t *rcount, size_t *rsize)
{
	int check, first, first_check;
	struct fmtchar *x;
	size_t size, value, count;

	size = 0;
	count = 0;
	first = 0;
	first_check = 1;
	check = 1;
	for (x = list->option; ; x = x->next) {
		if (x == NULL)
			goto error1;
		if (x->character == '>')
			break;
		if (check) {
			count++;
			check = 0;
		}
		if (x->type == FormatType_ClauseSeparator) {
			if (x->atsign)
				goto error2;
			if (x->colon) {
				if (first_check)
					goto error3;
				if (2 < x->size)
					goto error4;
				first = 1;
			}
			else {
				if (x->size)
					goto error5;
			}
			check = 1;
		}
		Return(format_size_(x, &value));
		size += value;
		first_check = 0;
	}
	if (check)
		count++;
	*rfirst = first;
	*rcount = count;
	*rsize = size;
	return 0;

error1:
	*rfirst = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"There is no close parensis ~~>.", NULL);

error2:
	*rfirst = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->atsign_pos,
			"The operator cannot use @ parameter.", NULL);

error3:
	*rfirst = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"The separator ~~:; must be a first clause.", NULL);

error4:
	*rfirst = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"Count of prefix parameters ~~...:; must be less than equal to 2.", NULL);

error5:
	*rfirst = 0;
	*rcount = 0;
	*rsize = 0;
	return format_abort_va_(x->format, x->position,
			"Cannot use prefix parameters ~~...; in the ~~<...~~>.", NULL);
}

static int format_size_Justification(struct fmtchar *list, size_t *ret)
{
	int first;
	size_t size, value, count;

	/* close parensis */
	if (list->close_colon) {
		return format_abort_va_(list->format, list->position,
				"Cannot use : parameter at the close parensis.", NULL);
	}
	if (list->close_atsign) {
		return format_abort_va_(list->format, list->position,
				"Cannot use @ parameter at the close parensis.", NULL);
	}
	/* arguments */
	Return(format_size_index_(list, &size, 4));
	/* body */
	Return(format_size_Justification_body_(list, &first, &count, &value));
	size += value;
	/* end */
	Return(format_size_End(&value));
	size += value;
	/* count */
	list->option_check = (first != 0);
	list->option_count = count;
	size += sizeoft(size_t) * (count + 1);
	/* result */
	return Result(ret, size);
}

static int format_write_Justification(struct fmtchar *list, byte *ptr, size_t *ret)
{
	int check;
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;
	struct fmtchar *x;
	size_t size, value, *array;

	/* arguments */
	str = format_write_operator(list, ptr, 4);
	format = list->format;
	/* mincol */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_less_(format, arg, 0, 0));
	/* colinc */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_less_(format, arg, 1, 1));
	/* minpad */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_less_(format, arg, 0, 0));
	/* padchar */
	arg = format_write_argument(&root, ptr, 3);
	Return(format_argument_character_(format, arg, ' '));
	/* array */
	size = str->size;
	array = (size_t *)(ptr + size);
	value = list->option_count;
	*(array++) = value;
	size += sizeoft(size_t) * (value + 1);
	/* body */
	check = 1;
	for (x = list->option; x->character != '>'; x = x->next) {
		if (check) {
			*(array++) = size;
			check = 0;
		}
		if (x->type == FormatType_ClauseSeparator)
			check = 1;
		Return(format_write_(x, ptr + size, &value));
		size += value;
	}
	if (check)
		*(array++) = size;
	/* end */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	*ret = str->size = size;
	return 0;
}


/* Logical Block: ~<, ~:> */
static int format_size_LogicalBlock_count_(
		struct fmtchar *list, size_t *ret, int *atsign)
{
	int check, first, first_check;
	struct fmtchar *x;
	size_t count;

	check = 1;
	first = 0;
	first_check = 1;
	count = 0;
	for (x = list->option; ; x = x->next) {
		if (x == NULL)
			goto error1;
		if (x->character == '>')
			break;
		if (check) {
			if (3 <= count)
				goto error2;
			count++;
			check = 0;
		}
		if (x->type == FormatType_ClauseSeparator) {
			if (x->colon)
				goto error3;
			if (x->size)
				goto error4;
			if (x->atsign) {
				if (first_check == 0)
					goto error5;
				first = 1;
			}
			check = 1;
			first_check = 0;
		}
	}
	if (check)
		count++;
	*ret = count;
	*atsign = first;
	return 0;

error1:
	*ret = 0;
	*atsign = 0;
	return format_abort_va_(x->format, x->position,
			"There is no close parensis ~~>.", NULL);

error2:
	*ret = 0;
	*atsign = 0;
	return format_abort_va_(x->format, x->position,
			"Count of clauses must be less than equal to 3.", NULL);

error3:
	*ret = 0;
	*atsign = 0;
	return format_abort_va_(x->format, x->atsign_pos,
			"The operator cannot use : parameter.", NULL);

error4:
	*ret = 0;
	*atsign = 0;
	return format_abort_va_(x->format, x->position,
			"Cannot use prefix parameters ~~...;.", NULL);

error5:
	*ret = 0;
	*atsign = 0;
	return format_abort_va_(x->format, x->position,
			"The separator ~~:; must be a first clause.", NULL);
}

static int format_size_LogicalBlock_body_(struct fmtchar *list, size_t *ret)
{
	struct fmtchar *x;
	size_t size, value;

	size = 0;
	for (x = list->option; x->character != '>'; x = x->next) {
		Check(x->type == FormatType_ClauseSeparator, "Clause separator error");
		Return(format_size_(x, &value));
		size += value;
	}
	return Result(ret, size);
}

static int format_size_LogicalBlock_output_(struct fmtchar *list, size_t *ret)
{
	struct fmtargs *root;
	size_t count;
#ifdef LISP_DEBUG
	size_t a, b;
#endif

	Check(list->size != 3, "size error");
	/* count */
	root = list->root;
	Check(root->type != fmtargs_index, "argument count error");
	count = root->u.index;
#ifdef LISP_DEBUG
	/* start */
	root = root->next;
	Check(root->type != fmtargs_index, "argument start error");
	a = root->u.index;
	/* end */
	root = root->next;
	Check(root->type != fmtargs_index, "argument end error");
	b = root->u.index;
	/* size */
	Check(b < a, "size1 error");
	Check(count != (b - a), "size2 error");
#endif
	return Result(ret, IdxSize + count * sizeoft(unicode));
}

static int format_end_separator(struct fmtchar *x)
{
	return x->type == FormatType_ClauseSeparator || x->character == '>';
}

static int format_size_LogicalBlock_text_(
		struct fmtchar *x, size_t *ret, struct fmtchar **next)
{
	Check(x == NULL, "Invalid prefix in logical block.");
	if (format_end_separator(x)) {
		*next = x->next;
		*ret = IdxSize;
		return 0;
	}
	if (x->type != FormatType_Output)
		goto error;
	Return(format_size_LogicalBlock_output_(x, ret));
	x = x->next;
	Check(x == NULL, "Invalid prefix in logical block.");
	if (format_end_separator(x)) {
		*next = x->next;
		return 0;
	}
	goto error;

error:
	*next = NULL;
	*ret = 0;
	return format_abort_va_(x->format, x->position,
			"Cannot use format directive in prefix/suffix form.", NULL);
}

static int format_size_LogicalBlock_prefix_(struct fmtchar *list, size_t *ret)
{
	struct fmtchar *x;
	size_t size, value;

	/* prefix */
	size = 0;
	x = list->option;
	Return(format_size_LogicalBlock_text_(x, &value, &x));
	size += value;
	/* body */
	for (;;) {
		if (x == NULL) {
			return format_abort_va_(x->format, x->position,
					"There is no clause separator ~~;.", NULL);
		}
		if (x->type == FormatType_ClauseSeparator) {
			x = x->next;
			break;
		}
		if (x->character == '>') {
			x = NULL;
			break;
		}
		Return(format_size_(x, &value));
		size += value;
		x = x->next;
	}
	/* suffix */
	if (x) {
		Return(format_size_LogicalBlock_text_(x, &value, &x));
		Check(x, "Invalid suffix in logical block.");
		size += value;
	}
	/* result */
	return Result(ret, size);
}

static int format_size_LogicalBlock(struct fmtchar *list, size_t *ret)
{
	int atsign;
	size_t size, value, count;

	/* close parensis */
	if (! list->close_colon) {
		return format_abort_va_(list->format, list->position,
				"Invalid close parensis ~~> in the logical-block.", NULL);
	}
	/* arguments */
	Return(format_size_index_(list, &size, 0));
	/* body */
	Return(format_size_LogicalBlock_count_(list, &count, &atsign));
	if (count <= 1) {
		Return(format_size_LogicalBlock_body_(list, &value));
	}
	else {
		Return(format_size_LogicalBlock_prefix_(list, &value));
	}
	size += value;
	/* end */
	Return(format_size_End(&value));
	size += value;
	/* count */
	list->option_check = (atsign != 0);
	list->option_count = count;
	list->prefix = (2 <= count);
	list->suffix = (3 <= count);
	/* result */
	return Result(ret, size);
}

static int format_write_LogicalBlock_output_(struct fmtchar *x, byte *ptr, size_t *ret)
{
	addr pos;
	struct fmtargs *root;
	unicode *data, u;
	size_t count, a, b, i;

	Check(x->size != 3, "size error");
	/* count */
	root = x->root;
	Check(root->type != fmtargs_index, "argument count error");
	count = root->u.index;
	/* start */
	root = root->next;
	Check(root->type != fmtargs_index, "argument start error");
	a = root->u.index;
	/* end */
	root = root->next;
	Check(root->type != fmtargs_index, "argument end error");
	b = root->u.index;
	/* write */
	Check(b < a, "size1 error");
	Check(count != (b - a), "size2 error");
	*(size_t *)ptr = count;
	data = (unicode *)(ptr + IdxSize);
	pos = x->format;
	for (i = 0; a < b; a++) {
		string_getc(pos, a, &u);
		data[i++] = u;
	}
	/* size */
	return Result(ret, IdxSize + count * sizeoft(unicode));
}

static int format_write_LogicalBlock_prefix_(struct fmtchar *x, byte *ptr, size_t *ret)
{
	if (format_end_separator(x)) {
		*(size_t *)ptr = 0;
		*ret = IdxSize;
	}
	else {
		Check(x->type != FormatType_Output, "type error");
		Return(format_write_LogicalBlock_output_(x, ptr, ret));
	}

	return 0;
}

static void format_write_LogicalBlock_next(
		int count, struct fmtchar *x, struct fmtchar **ret)
{
	int i;

	for (i = 0; i < count; i++) {
		for (;;) {
			Check(x == NULL, "fmtchar error");
			if (x->type == FormatType_ClauseSeparator) {
				x = x->next;
				break;
			}
			x = x->next;
		}
	}
	*ret = x;
}

static int format_write_LogicalBlock_suffix_(struct fmtchar *x, byte *ptr, size_t *ret)
{
	format_write_LogicalBlock_next(2, x, &x);
	return format_write_LogicalBlock_prefix_(x, ptr, ret);
}

static int format_write_LogicalBlock(struct fmtchar *list, byte *ptr, size_t *ret)
{
	struct format_operator *str;
	struct fmtchar *x;
	size_t size, value;

	/* arguments */
	str = format_write_operator(list, ptr, 0);
	/* prefix */
	x = list->option;
	size = str->size;
	if (str->prefix) {
		Return(format_write_LogicalBlock_prefix_(x, ptr + size, &value));
		size += value;
	}
	/* suffix */
	if (str->suffix) {
		Return(format_write_LogicalBlock_suffix_(x, ptr + size, &value));
		size += value;
	}
	/* body */
	if (str->prefix)
		format_write_LogicalBlock_next(1, x, &x);
	for (; ! format_end_separator(x); x = x->next) {
		Return(format_write_(x, ptr + size, &value));
		size += value;
	}
	/* end */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	*ret = str->size = size;
	return 0;
}


/* ~^ */
static int format_write_EscapeUpward(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* atsign */
	if (list->atsign) {
		return format_abort_va_(list->format, list->atsign_pos,
				"The operator cannot use @ parameter.", NULL);
	}
	/* parameter */
	str = format_write_operator(list, ptr, 3);
	format = list->format;
	/* v1 */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_integer_nil_(format, arg));
	/* v2 */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_integer_nil_(format, arg));
	/* v3 */
	arg = format_write_argument(&root, ptr, 2);
	Return(format_argument_integer_nil_(format, arg));
	/* result */
	return Result(ret, str->size);
}


/* ~; */
static int format_write_ClauseSeparator(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr format;
	struct format_operator *str;
	struct format_argument *arg;
	struct fmtargs *root;

	/* parameters */
	str = format_write_operator(list, ptr, 2);
	format = list->format;
	/* size */
	root = list->root;
	arg = format_write_argument(&root, ptr, 0);
	Return(format_argument_integer_nil_(format, arg));
	/* width */
	arg = format_write_argument(&root, ptr, 1);
	Return(format_argument_integer_nil_(format, arg));
	/* result */
	return Result(ret, str->size);
}


/* ~/ */
static int format_size_CallFunction(struct fmtchar *list, size_t *ret)
{
	addr package, name;
	size_t size, value;

	/* argument */
	Return(format_size_index_(list, &size, list->size));
	Check(list->intern == NULL, "intern error");
	GetCons(list->intern, &package, &name);
	/* package */
	size += IdxSize;
	strvect_length(package, &value);
	size += value * sizeoft(unicode);
	/* name */
	size += IdxSize;
	strvect_length(name, &value);
	size += value * sizeoft(unicode);
	/* result */
	return Result(ret, size);
}

static int format_write_CallFunction_string_(
		struct format_operator *str, byte *ptr, addr pos)
{
	size_t size, i;
	unicode *data, u;

	/* size_t */
	strvect_length(pos, &size);
	*((size_t *)(ptr + str->size)) = size;
	str->size += IdxSize;
	/* unicode */
	data = (unicode *)(ptr + str->size);
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &u);
		data[i] = u;
	}
	str->size += size * sizeoft(unicode);

	return 0;
}

static int format_write_CallFunction(struct fmtchar *list, byte *ptr, size_t *ret)
{
	addr package, name;
	struct format_operator *str;
	struct fmtargs *x, *root;
	size_t i;

	/* parameters */
	str = format_write_operator(list, ptr, list->size);
	i = 0;
	for (x = list->root; x; x = x->next) {
		root = x;
		format_write_argument(&root, ptr, i++);
	}
	/* body */
	GetCons(list->intern, &package, &name);
	Return(format_write_CallFunction_string_(str, ptr, package));
	Return(format_write_CallFunction_string_(str, ptr, name));
	/* result */
	return Result(ret, str->size);
}


/*
 *  format write call
 */
typedef int (*format_size_call)(struct fmtchar *, size_t *);
typedef int (*format_write_call)(struct fmtchar *, byte *, size_t *);
static format_size_call FormatSize[FormatType_size];
static format_write_call FormatWrite[FormatType_size];

static int format_size_(struct fmtchar *list, size_t *ret)
{
	format_size_call call;

	call = FormatSize[list->type];
	Check(call == NULL, "size call error");
	return (*call)(list, ret);
}

static int format_write_(struct fmtchar *list, byte *ptr, size_t *ret)
{
	format_write_call call;

	call = FormatWrite[list->type];
	Check(call == NULL, "write call error");
	return (*call)(list, ptr, ret);
}

static int format_size_list_(struct fmtchar *list, size_t *ret)
{
	size_t size, value;

	/* Format */
	Return(format_size_Format(list, &size));
	/* body */
	for (; list; list = list->next) {
		if (list->type == FormatType_ClauseSeparator) {
			*ret = 0;
			return format_abort_va_(list->format, list->position,
					"Cannot use ~~; operator.", NULL);
		}
		Return(format_size_(list, &value));
		size += value;
	}
	/* End */
	Return(format_size_End(&value));
	size += value;
	/* result */
	return Result(ret, size);
}

static int format_write_list_(struct fmtchar *list, byte *ptr, size_t *ret)
{
	size_t size, value;

	/* Format */
	Return(format_write_Format(list, ptr, &size));
	/* body */
	for (; list; list = list->next) {
		Return(format_write_(list, ptr + size, &value));
		size += value;
	}
	/* End */
	Return(format_write_End(list, ptr + size, &value));
	size += value;
	/* result */
	return Result(ret, size);
}


/*
 *  format object
 */
_g int formatp(addr pos)
{
	return GetType(pos) == LISPTYPE_FORMAT;
}

static void format_alloc(LocalRoot local, addr *ret, size_t size)
{
	alloc_body4(local, ret, LISPTYPE_FORMAT, size);
}

_g void *format_pointer(addr pos)
{
	CheckType(pos, LISPTYPE_FORMAT);
	return (void *)PtrBodyB4(pos);
}

_g size_t format_bytesize(size_t count)
{
	return FormatByteSize(count);
}

static int format_parse_(LocalRoot local, addr *ret, addr format, int localp)
{
	addr pos;
	byte *ptr;
	struct fmtchar *list;
	size_t size, check;

	/* parse */
	Return(fmtchar_make_(local, format, &list));
	Return(format_size_list_(list, &size));
	/* allocation */
	format_alloc(localp? local: NULL, &pos, size);
	ptr = (byte *)format_pointer(pos);
#ifdef LISP_DEBUG
	aamemory(ptr, size);
#endif
	Return(format_write_list_(list, ptr, &check));
	Check(size != check, "size error");
	return Result(ret, pos);
}

_g int format_parse_local_(LocalRoot local, addr *ret, addr format)
{
	CheckLocal(local);
	CheckType(format, LISPTYPE_STRING);
	return format_parse_(local, ret, format, 1);
}

_g int format_parse_heap_(LocalRoot local, addr *ret, addr format)
{
	LocalStack stack;

	CheckLocal(local);
	CheckType(format, LISPTYPE_STRING);
	push_local(local, &stack);
	Return(format_parse_(local, ret, format, 0));
	rollback_local(local, stack);

	return 0;
}

_g void format_string_alloc(LocalRoot local, addr *ret, addr format)
{
	byte *body;
	struct format_argument *arg;
	unicode *u;
	addr pos;
	size_t size, i;
#ifdef LISP_DEBUG
	struct format_operator *str;
#endif


	CheckType(format, LISPTYPE_FORMAT);
	/* header */
	body = (byte *)format_pointer(format);
#ifdef LISP_DEBUG
	str = (struct format_operator *)body;
	Check(str->type != FormatType_Format, "Invalid format object.");
#endif
	arg = (struct format_argument *)format_write_body(body, 0);
	Check(arg->type != fmtargs_index, "Invalid format arguemnt.");
	size = arg->u.index;
	/* body */
	strvect_alloc(local, &pos, size);
	u = (unicode *)format_write_body(body, 1);
	for (i = 0; i < size; i++)
		strvect_setc(pos, i, u[i]);
	*ret = pos;
}

_g void format_string_local(LocalRoot local, addr *ret, addr format)
{
	CheckLocal(local);
	format_string_alloc(local, ret, format);
}

_g void format_string_heap(addr *ret, addr format)
{
	format_string_alloc(NULL, ret, format);
}


/*
 *  table
 */
#define SetFormatSize(x,y) (FormatSize[FormatType_##x] = format_size_##y)
#define SetFormatWrite(x,y) (FormatWrite[FormatType_##x] = format_write_##y)
#define SetFormatCharacter(x,y) (FormatCharacter[(int)(x)] = (y))

_g void init_format_parse(void)
{
	cleartype(FormatCharacter);
	SetFormatCharacter(0, FormatType_Output);
	SetFormatCharacter('A', FormatType_Aesthetic);
	SetFormatCharacter('S', FormatType_Standard);
	SetFormatCharacter('B', FormatType_Binary);
	SetFormatCharacter('O', FormatType_Octal);
	SetFormatCharacter('D', FormatType_Decimal);
	SetFormatCharacter('X', FormatType_Hexadecimal);
	SetFormatCharacter('R', FormatType_Radix);
	SetFormatCharacter('P', FormatType_Plural);
	SetFormatCharacter('C', FormatType_Character);
	SetFormatCharacter('F', FormatType_Fixed);
	SetFormatCharacter('E', FormatType_Exponential);
	SetFormatCharacter('G', FormatType_General);
	SetFormatCharacter('$', FormatType_Monetary);
	SetFormatCharacter('%', FormatType_Newline);
	SetFormatCharacter('&', FormatType_FreshLine);
	SetFormatCharacter('|', FormatType_Page);
	SetFormatCharacter('~', FormatType_Tilde);
	SetFormatCharacter('\n', FormatType_IgnoredNewline);
	SetFormatCharacter('T', FormatType_Tabulate);
	SetFormatCharacter('*', FormatType_GoTo);
	SetFormatCharacter('?', FormatType_Recursive);
	SetFormatCharacter('_', FormatType_ConditionalNewline);
	SetFormatCharacter('W', FormatType_Write);
	SetFormatCharacter('I', FormatType_Indent);
	SetFormatCharacter('(', FormatType_Case);
	SetFormatCharacter('[', FormatType_Condition);
	SetFormatCharacter('{', FormatType_Iteration);
	SetFormatCharacter('<', FormatType_Justification);
	SetFormatCharacter('^', FormatType_EscapeUpward);
	SetFormatCharacter(';', FormatType_ClauseSeparator);
	SetFormatCharacter('/', FormatType_CallFunction);

	cleartype(FormatSize);
	SetFormatSize(Output,              Output);           /* text */
	SetFormatSize(Aesthetic,           size4);            /* A */
	SetFormatSize(Standard,            size4);            /* S */
	SetFormatSize(Binary,              size4);            /* B */
	SetFormatSize(Octal,               size4);            /* O */
	SetFormatSize(Decimal,             size4);            /* D */
	SetFormatSize(Hexadecimal,         size4);            /* X */
	SetFormatSize(Radix,               size5);            /* R */
	SetFormatSize(RadixText,           size0);            /* R */
	SetFormatSize(Plural,              size0);            /* P */
	SetFormatSize(Character,           size0);            /* C */
	SetFormatSize(Fixed,               size5);            /* F */
	SetFormatSize(Exponential,         size7);            /* E */
	SetFormatSize(General,             size7);            /* G */
	SetFormatSize(Monetary,            size4);            /* $ */
	SetFormatSize(Newline,             size1);            /* % */
	SetFormatSize(FreshLine,           size1);            /* & */
	SetFormatSize(Page,                size1);            /* | */
	SetFormatSize(Tilde,               size1);            /* ~ */
	SetFormatSize(IgnoredNewline,      size0);            /* \n */
	SetFormatSize(Tabulate,            size2);            /* T */
	SetFormatSize(GoTo,                size1);            /* * */
	SetFormatSize(Recursive,           size0);            /* ? */
	SetFormatSize(ConditionalNewline,  size0);            /* _ */
	SetFormatSize(Write,               size0);            /* W */
	SetFormatSize(Indent,              size1);            /* I */
	SetFormatSize(Case,                Case);             /* () */
	SetFormatSize(Condition,           Condition);        /* [] */
	SetFormatSize(Iteration,           Iteration);        /* {} */
	SetFormatSize(Justification,       Justification);    /* <> */
	SetFormatSize(LogicalBlock,        LogicalBlock);     /* <> */
	SetFormatSize(EscapeUpward,        size3);            /* ^ */
	SetFormatSize(ClauseSeparator,     size2);            /* ; */
	SetFormatSize(CallFunction,        CallFunction);     /* / */

	cleartype(FormatWrite);
	SetFormatWrite(Output,              Output);           /* text */
	SetFormatWrite(Aesthetic,           Aesthetic);        /* A */
	SetFormatWrite(Standard,            Aesthetic);        /* S */
	SetFormatWrite(Binary,              Binary);           /* B */
	SetFormatWrite(Octal,               Binary);           /* O */
	SetFormatWrite(Decimal,             Binary);           /* D */
	SetFormatWrite(Hexadecimal,         Binary);           /* X */
	SetFormatWrite(Radix,               Radix);            /* R */
	SetFormatWrite(RadixText,           Empty);            /* R */
	SetFormatWrite(Plural,              Empty);            /* P */
	SetFormatWrite(Character,           Empty);            /* C */
	SetFormatWrite(Fixed,               Fixed);            /* F */
	SetFormatWrite(Exponential,         Exponent);         /* E */
	SetFormatWrite(General,             General);          /* G */
	SetFormatWrite(Monetary,            Monetary);         /* $ */
	SetFormatWrite(Newline,             Newline);          /* % */
	SetFormatWrite(FreshLine,           Newline);          /* & */
	SetFormatWrite(Page,                Newline);          /* | */
	SetFormatWrite(Tilde,               Newline);          /* ~ */
	SetFormatWrite(IgnoredNewline,      Empty);            /* \n */
	SetFormatWrite(Tabulate,            Tabulate);         /* T */
	SetFormatWrite(GoTo,                GoTo);             /* * */
	SetFormatWrite(Recursive,           Recursive);        /* ? */
	SetFormatWrite(ConditionalNewline,  Empty);            /* _ */
	SetFormatWrite(Write,               Empty);            /* W */
	SetFormatWrite(Indent,              Indent);           /* I */
	SetFormatWrite(Case,                Case);             /* () */
	SetFormatWrite(Condition,           Condition);        /* [] */
	SetFormatWrite(Iteration,           Iteration);        /* {} */
	SetFormatWrite(Justification,       Justification);    /* <> */
	SetFormatWrite(LogicalBlock,        LogicalBlock);     /* <> */
	SetFormatWrite(EscapeUpward,        EscapeUpward);     /* ^ */
	SetFormatWrite(ClauseSeparator,     ClauseSeparator);  /* ; */
	SetFormatWrite(CallFunction,        CallFunction);     /* / */
}

