#include "charqueue.h"
#include "condition.h"
#include "cons_list.h"
#include "control.h"
#include "function.h"
#include "memory.h"
#include "object.h"
#include "print_write.h"
#include "print_pretty.h"
#include "stream_error.h"
#include "stream_pretty.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  stream-pretty object
 */
struct stream_Pretty {
	unsigned list : 1;
	unsigned discard : 1;
	unsigned alive : 1;
	size_t length, depth;
};

#define CheckPrettyStream(stream) { \
	Check(! pretty_stream_p(stream), "type error"); \
}
#define PtrPrettyStream(pos) ((struct stream_Pretty *)PtrDataStream(pos))

enum StreamPretty_Index {
	StreamPretty_Stream,
	StreamPretty_Root,
	StreamPretty_Object,
	StreamPretty_Prefix,
	StreamPretty_PerLine,
	StreamPretty_Suffix,
	StreamPretty_Gensym,
	StreamPretty_Stack,
	StreamPretty_Queue,
	StreamPretty_Sharp,
	StreamPretty_Size
};


/*
 *  access
 */
static void setalive_pretty_stream(addr stream, int value)
{
	CheckPrettyStream(stream);
	PtrPrettyStream(stream)->alive = value;
}

static void increment_pretty_stream(addr stream)
{
	CheckPrettyStream(stream);
	(PtrPrettyStream(stream)->length)++;
}

static void alive_pretty_stream(addr stream)
{
	CheckPrettyStream(stream);
	if (! PtrPrettyStream(stream)->alive)
		fmte("The stream ~S is already closed.", stream, NULL);
}

_g void setlistp_pretty_stream(addr stream, int value)
{
	CheckPrettyStream(stream);
	PtrPrettyStream(stream)->list = value;
}

_g int listp_pretty_stream(addr stream)
{
	return pretty_stream_p(stream)
		&& PtrPrettyStream(stream)->list != 0;
}

_g void setdiscard_pretty_stream(addr stream, int value)
{
	CheckPrettyStream(stream);
	PtrPrettyStream(stream)->discard = value;
}

_g int discard_pretty_stream(addr stream)
{
	return pretty_stream_p(stream)
		&& PtrPrettyStream(stream)->discard != 0;
}

static void getinfo_pretty_stream(addr stream, addr *ret)
{
	alive_pretty_stream(stream);
	GetInfoStream(stream, ret);
}

_g size_t length_pretty_stream(addr stream)
{
	alive_pretty_stream(stream);
	return PtrPrettyStream(stream)->length;
}

_g int first_pretty_stream(addr stream)
{
	return length_pretty_stream(stream) == 0;
}

static void getstream_pretty_stream(addr stream, addr *ret)
{
	getinfo_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Stream, ret);
}

_g void gensym_pretty_stream(addr stream, addr *ret)
{
	getinfo_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Gensym, ret);
}

_g void root_pretty_stream(addr stream, addr *ret)
{
	getinfo_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Root, ret);
}

_g void setroot_pretty_stream(addr stream, addr value)
{
	getinfo_pretty_stream(stream, &stream);
	SetArrayA2(stream, StreamPretty_Root, value);
}

_g void object_pretty_stream(addr stream, addr *ret)
{
	getinfo_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Object, ret);
}

static void queue_pretty_stream(addr stream, addr *ret)
{
	getinfo_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Queue, ret);
}


/*
 *  external function
 */
static void nocheck_info_pretty_stream(addr stream, addr *ret)
{
	CheckPrettyStream(stream);
	GetInfoStream(stream, ret);
}

_g void prefix_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Prefix, ret);
}

_g void perline_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_PerLine, ret);
}

_g void suffix_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Suffix, ret);
}

_g void stream_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Stream, ret);
}

_g void result_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Stack, ret);
}

_g void sharp_pretty_stream(addr stream, addr *ret)
{
	nocheck_info_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Sharp, ret);
}

_g void setsharp_pretty_stream(addr stream, addr value)
{
	nocheck_info_pretty_stream(stream, &stream);
	SetArrayA2(stream, StreamPretty_Sharp, value);
}

static void push_unsafe_pretty_stream(addr stream, addr pos)
{
	addr stack;

	getinfo_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Stack, &stack);
	cons_heap(&stack, pos, stack);
	SetArrayA2(stream, StreamPretty_Stack, stack);
}

static void flush_pretty_stream(addr stream)
{
	addr queue, value;
	size_t size;

	queue_pretty_stream(stream, &queue);
	getsize_charqueue(queue, &size);
	if (size != 0) {
		make_charqueue_heap(queue, &value);
		clear_charqueue(queue);
		push_unsafe_pretty_stream(stream, value);
	}
}


/*
 *  make
 */
static void make_info_pretty_vector(addr *ret, addr stream,
		addr root, addr prefix, addr perline, addr suffix, addr gensym, addr queue)
{
	addr pos;

	vector2_heap(&pos, StreamPretty_Size);
	SetArrayA2(pos, StreamPretty_Stream, stream);
	SetArrayA2(pos, StreamPretty_Root, root);
	SetArrayA2(pos, StreamPretty_Object, root);
	SetArrayA2(pos, StreamPretty_Prefix, prefix);
	SetArrayA2(pos, StreamPretty_PerLine, perline);
	SetArrayA2(pos, StreamPretty_Suffix, suffix);
	SetArrayA2(pos, StreamPretty_Gensym, gensym);
	SetArrayA2(pos, StreamPretty_Queue, queue);
	SetArrayA2(pos, StreamPretty_Sharp, Nil);
	*ret = pos;
}

static void make_info_pretty_stream(Execute ptr, addr *ret,
		addr stream, addr root, addr prefix, addr perline, addr suffix)
{
	addr gensym, queue;

	/* gensym */
	make_gensym(ptr, &gensym);
	/* charqueue */
	if (pretty_stream_p(stream)) {
		flush_pretty_stream(stream);
		queue_pretty_stream(stream, &queue);
	}
	else {
		charqueue_heap(&queue, 0);
	}
	/* info */
	make_info_pretty_vector(ret, stream, root, prefix, perline, suffix, gensym, queue);
}

_g void open_pretty_stream(Execute ptr, addr *ret,
		addr stream, addr root, addr prefix, addr perline, addr suffix)
{
	addr pos, info;
	struct stream_Pretty *str;

	/* type check */
	if (! streamp(stream))
		TypeError(stream, STREAM);
	if (prefix != Nil && (! stringp(prefix)))
		TypeError(prefix, STRING);
	if (perline != Nil && (! stringp(perline)))
		TypeError(perline, STRING);
	if (suffix != Nil && (! stringp(suffix)))
		TypeError(suffix, STRING);
	if (prefix != Nil && perline != Nil)
		fmte("Cannot supply both :PREFIX and :PER-LINE-PREFIX.", NULL);

	/* make */
	stream_heap(&pos, StreamType_Pretty, sizeoft(struct stream_Pretty));
	str = PtrPrettyStream(pos);
	str->list = listp(root);
	str->discard = discard_pretty_stream(stream);
	str->alive = 1;
	str->length = 0;
	getdepth_print_write(ptr, &(str->depth));
	/* info */
	make_info_pretty_stream(ptr, &info, stream, root, prefix, perline, suffix);
	SetInfoStream(pos, info);
	/* result */
	force_open_stream(pos, ret);
}


/*
 *  pretty-stream function
 */
static void nreverse_unsafe_pretty_stream(addr stream)
{
	addr stack;

	getinfo_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Stack, &stack);
	nreverse_list_unsafe(&stack, stack);
	SetArrayA2(stream, StreamPretty_Stack, stack);
}

_g void setdepth_pretty_stream(Execute ptr, addr stream, size_t inc)
{
	size_t depth = PtrPrettyStream(stream)->depth;
	setdepth_print_write(ptr, depth + inc);
}

_g void close_pretty_stream(Execute ptr, addr stream)
{
	addr pos;

	/* depth */
	setdepth_pretty_stream(ptr, stream, 0);
	/* discard */
	if (discard_pretty_stream(stream)) {
		setalive_pretty_stream(stream, 0);
		return;
	}
	/* stack */
	flush_pretty_stream(stream);
	nreverse_unsafe_pretty_stream(stream);
	/* close */
	setalive_pretty_stream(stream, 0);
	close_abort_stream(stream, 0);
	/* output */
	stream_pretty_stream(stream, &pos);
	if (pretty_stream_p(pos))
		push_pretty_stream(pos, stream);
	else
		pprint_output(ptr, pos, stream);
}

_g void push_pretty_stream(addr stream, addr pos)
{
	flush_pretty_stream(stream);
	push_unsafe_pretty_stream(stream, pos);
}

_g int pop_pretty_stream(addr stream, addr *ret)
{
	addr info, list;

	getinfo_pretty_stream(stream, &info);
	GetArrayA2(info, StreamPretty_Root, &list);
	if (list == Nil)
		return 1;
	if (consp(list)) {
		GetCons(list, ret, &list);
	}
	else {
		*ret = list;
		list = Nil;
	}
	increment_pretty_stream(stream);
	SetArrayA2(info, StreamPretty_Root, list);

	return 0;
}

static void character_pretty_stream(addr stream, unicode u)
{
	if (u == 0x0A) {
		pprint_newline_terpri(stream);
	}
	else {
		queue_pretty_stream(stream, &stream);
		push_charqueue_heap(stream, u);
	}
}

_g int push_pretty_stream_p(addr stream)
{
	return output_string_stream_p(stream)?
		get_pretty_output_string_stream(stream):
		pretty_stream_p(stream);
}

static int Push_pretty_stream_p(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return push_pretty_stream_p(stream);
}

static void rollback_pretty_stream(addr stream)
{
	addr root, pos, info;
	struct stream_Pretty *str;

	/* object */
	GetInfoStream(stream, &info);
	GetArrayA2(info, StreamPretty_Object, &root);
	SetArrayA2(info, StreamPretty_Root, root);
	SetArrayA2(info, StreamPretty_Stack, Nil);
	GetArrayA2(info, StreamPretty_Queue, &pos);
	clear_charqueue(pos);

	/* struct */
	str = PtrPrettyStream(stream);
	str->list = listp(root);
	str->discard = 0;
	str->alive = 1;
	str->length = 0;
}

_g int call_pretty_stream(Execute ptr, addr stream, addr call)
{
	int check;
	addr pos;

	Check(! pretty_stream_p(stream), "type error");
	Check(! functionp(call), "type error");

	check = Push_pretty_stream_p(stream);
	if (! check) {
		push_return_control(ptr, &pos);
		push_write_object(ptr);
	}
	/* normal */
	if (! circle_print(ptr))
		return callclang_funcall(ptr, &pos, call, NULL);
	if (check)
		return callclang_funcall(ptr, &pos, call, NULL);
	/* circle check */
	setdiscard_pretty_stream(stream, 1);
	root_pretty_stream(stream, &pos);
	/* check */
	write_check_call(ptr, pos);
	/* call */
	if (callclang_funcall(ptr, &pos, call, NULL))
		return 1;
	/* circle output */
	rollback_pretty_stream(stream);
	write_check_all_clear(ptr);
	return callclang_funcall(ptr, &pos, call, NULL);
}


/*
 *  stream function
 */
static int close_Pretty(addr stream, int abort)
{
	stream_pretty_stream(stream, &stream);
	return 1;
}

static int read_char_Pretty(addr stream, unicode *u)
{
	getstream_pretty_stream(stream, &stream);
	return read_char_stream(stream, u);
}

static int read_hang_Pretty(addr stream, unicode *u, int *hang)
{
	getstream_pretty_stream(stream, &stream);
	return read_hang_stream(stream, u, hang);
}

static void unread_char_Pretty(addr stream, unicode u)
{
	getstream_pretty_stream(stream, &stream);
	unread_char_stream(stream, u);
}

static void write_char_Pretty(addr stream, unicode u)
{
	character_pretty_stream(stream, u);
}

static void terpri_Pretty(addr stream)
{
	alive_pretty_stream(stream);
	pprint_newline_terpri(stream);
}

static int fresh_line_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return fresh_line_stream(stream);
}

static int inputp_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return inputp_stream(stream);
}

static int outputp_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return outputp_stream(stream);
}

static int interactivep_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return interactivep_stream(stream);
}

static int characterp_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return characterp_stream(stream);
}

static int binaryp_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return binaryp_stream(stream);
}

static void element_type_Pretty(addr stream, addr *ret)
{
	getstream_pretty_stream(stream, &stream);
	element_type_stream(stream, ret);
}

static void file_length_Pretty(addr stream, addr *ret)
{
	getstream_pretty_stream(stream, &stream);
	file_length_stream(stream, ret);
}

static int file_position_Pretty(addr stream, size_t *ret)
{
	getstream_pretty_stream(stream, &stream);
	return file_position_stream(stream, ret);
}

static int file_position_start_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return file_position_start_stream(stream);
}

static int file_position_end_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return file_position_end_stream(stream);
}

static int file_position_set_Pretty(addr stream, size_t pos)
{
	getstream_pretty_stream(stream, &stream);
	return file_position_set_stream(stream, pos);
}

static int file_character_length_Pretty(addr stream, unicode u, size_t *ret)
{
	getstream_pretty_stream(stream, &stream);
	return file_character_length_stream(stream, u, ret);
}

static int file_string_length_Pretty(addr stream, addr pos, size_t *ret)
{
	getstream_pretty_stream(stream, &stream);
	return file_string_length_stream(stream, pos, ret);
}

static int listen_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return listen_stream(stream);
}

static void clear_input_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	clear_input_stream(stream);
}

static void finish_output_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	finish_output_stream(stream);
}

static void force_output_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	force_output_stream(stream);
}

static void clear_output_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	clear_output_stream(stream);
}

static int terminal_width_Pretty(addr stream, size_t *ret)
{
	getstream_pretty_stream(stream, &stream);
	return terminal_width_stream(stream, ret);
}

_g void init_stream_pretty(void)
{
	DefineStreamSet(Pretty, close);
	DefineStream___(Pretty, read_binary);
	DefineStream___(Pretty, readforce_binary);
	DefineStream___(Pretty, read_byte);
	DefineStream___(Pretty, unread_byte);
	DefineStream___(Pretty, write_binary);
	DefineStream___(Pretty, write_byte);
	DefineStreamSet(Pretty, read_char);
	DefineStreamSet(Pretty, read_hang);
	DefineStreamSet(Pretty, unread_char);
	DefineStreamSet(Pretty, write_char);
	DefineStreamSet(Pretty, terpri);
	DefineStreamSet(Pretty, fresh_line);
	DefineStreamSet(Pretty, inputp);
	DefineStreamSet(Pretty, outputp);
	DefineStreamSet(Pretty, interactivep);
	DefineStreamSet(Pretty, characterp);
	DefineStreamSet(Pretty, binaryp);
	DefineStreamSet(Pretty, element_type);
	DefineStreamSet(Pretty, file_length);
	DefineStreamSet(Pretty, file_position);
	DefineStreamSet(Pretty, file_position_start);
	DefineStreamSet(Pretty, file_position_end);
	DefineStreamSet(Pretty, file_position_set);
	DefineStreamSet(Pretty, file_character_length);
	DefineStreamSet(Pretty, file_string_length);
	DefineStreamSet(Pretty, listen);
	DefineStreamSet(Pretty, clear_input);
	DefineStreamSet(Pretty, finish_output);
	DefineStreamSet(Pretty, force_output);
	DefineStreamSet(Pretty, clear_output);
	DefineStreamDef(Pretty, exitpoint);
	DefineStreamSet(Pretty, terminal_width);
}

