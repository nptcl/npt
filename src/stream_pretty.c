#include "character_queue.h"
#include "condition.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
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
	size_t length, depth, terpri;
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

static int make_info_pretty_stream_(Execute ptr, addr *ret,
		addr stream, addr root, addr prefix, addr perline, addr suffix)
{
	addr gensym, queue;

	/* gensym */
	Return(make_gensym_(ptr, &gensym));
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
	return 0;
}

_g int open_pretty_stream_(Execute ptr, addr *ret,
		addr stream, addr root, addr prefix, addr perline, addr suffix)
{
	addr pos, info;
	struct stream_Pretty *str;

	/* type check */
	if (! streamp(stream))
		return TypeError_(stream, STREAM);
	if (prefix != Nil && (! stringp(prefix)))
		return TypeError_(prefix, STRING);
	if (perline != Nil && (! stringp(perline)))
		return TypeError_(perline, STRING);
	if (suffix != Nil && (! stringp(suffix)))
		return TypeError_(suffix, STRING);
	if (prefix != Nil && perline != Nil)
		return fmte_("Cannot supply both :PREFIX and :PER-LINE-PREFIX.", NULL);

	/* make */
	stream_heap(&pos, StreamType_Pretty, sizeoft(struct stream_Pretty));
	str = PtrPrettyStream(pos);
	str->list = listp(root);
	str->discard = discard_pretty_stream(stream);
	str->alive = 1;
	str->length = 0;
	Return(getleft_stream_(stream, &(str->terpri)));
	getdepth_print_write(ptr, &(str->depth));
	/* info */
	Return(make_info_pretty_stream_(ptr, &info, stream, root, prefix, perline, suffix));
	SetInfoStream(pos, info);
	/* result */
	force_open_stream(pos);
	return Result(ret, pos);
}


/*
 *  pretty-stream function
 */
static void nreverse_unsafe_pretty_stream(addr stream)
{
	addr stack;

	getinfo_pretty_stream(stream, &stream);
	GetArrayA2(stream, StreamPretty_Stack, &stack);
	nreverse(&stack, stack);
	SetArrayA2(stream, StreamPretty_Stack, stack);
}

_g void setdepth_pretty_stream(Execute ptr, addr stream, size_t inc)
{
	size_t depth;

	depth = PtrPrettyStream(stream)->depth;
	setdepth_print_write(ptr, depth + inc);
}

_g int close_pretty_stream_(Execute ptr, addr stream)
{
	addr pos;

	/* depth */
	setdepth_pretty_stream(ptr, stream, 0);
	/* discard */
	if (discard_pretty_stream(stream)) {
		setalive_pretty_stream(stream, 0);
		return 0;
	}
	/* stack */
	flush_pretty_stream(stream);
	nreverse_unsafe_pretty_stream(stream);
	/* close */
	setalive_pretty_stream(stream, 0);
	force_close_stream(stream);
	/* output */
	stream_pretty_stream(stream, &pos);
	if (pretty_stream_p(pos)) {
		push_pretty_stream(pos, stream);
	}
	else {
		Return(pprint_output_(ptr, pos, stream));
	}

	return 0;
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

static int rollback_pretty_stream_(addr stream)
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
	return setleft_stream_(stream, str->terpri);
}

_g int call_pretty_stream(Execute ptr, addr stream, addr call)
{
	int check;
	addr pos;

	Check(! pretty_stream_p(stream), "type error");
	Check(! functionp(call), "type error");

	check = Push_pretty_stream_p(stream);
	if (! check) {
		push_new_control(ptr, &pos);
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
	Return(write_check_call_(ptr, pos));
	/* call */
	Return(callclang_funcall(ptr, &pos, call, NULL));
	/* circle output */
	Return(rollback_pretty_stream_(stream));
	write_check_all_clear(ptr);
	return callclang_funcall(ptr, &pos, call, NULL);
}


/*
 *  stream function
 */
static int close_Pretty(addr stream, addr *ret)
{
	stream_pretty_stream(stream, &stream);
	return Result(ret, T);
}

static int read_char_Pretty(addr stream, unicode *u, int *ret)
{
	getstream_pretty_stream(stream, &stream);
	return read_char_stream_(stream, u, ret);
}

static int read_hang_Pretty(addr stream, unicode *u, int *hang, int *ret)
{
	getstream_pretty_stream(stream, &stream);
	return read_hang_stream_(stream, u, hang, ret);
}

static int unread_char_Pretty(addr stream, unicode c)
{
	getstream_pretty_stream(stream, &stream);
	return unread_char_stream_(stream, c);
}

static int write_char_Pretty(addr stream, unicode u)
{
	character_pretty_stream(stream, u);
	return 0;
}

static int terpri_Pretty(addr stream)
{
	alive_pretty_stream(stream);
	pprint_newline_terpri(stream);
	return 0;
}

static int getleft_Pretty(addr stream, size_t *ret)
{
	getstream_pretty_stream(stream, &stream);
	return getleft_stream_(stream, ret);
}

static int setleft_Pretty(addr stream, size_t value)
{
	getstream_pretty_stream(stream, &stream);
	return setleft_stream_(stream, value);
}

static int fresh_line_Pretty(addr stream, int *ret)
{
	getstream_pretty_stream(stream, &stream);
	return fresh_line_stream_(stream, ret);
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

static int element_type_Pretty(addr stream, addr *ret)
{
	getstream_pretty_stream(stream, &stream);
	return element_type_stream_(stream, ret);
}

static int file_length_Pretty(addr stream, addr *ret)
{
	getstream_pretty_stream(stream, &stream);
	return file_length_stream_(stream, ret);
}

static int file_position_Pretty(addr stream, size_t *value, int *ret)
{
	getstream_pretty_stream(stream, &stream);
	return file_position_stream_(stream, value, ret);
}

static int file_position_start_Pretty(addr stream, int *ret)
{
	getstream_pretty_stream(stream, &stream);
	return file_position_start_stream_(stream, ret);
}

static int file_position_end_Pretty(addr stream, int *ret)
{
	getstream_pretty_stream(stream, &stream);
	return file_position_end_stream_(stream, ret);
}

static int file_position_set_Pretty(addr stream, size_t value, int *ret)
{
	getstream_pretty_stream(stream, &stream);
	return file_position_set_stream_(stream, value, ret);
}

static int file_charlen_Pretty(addr stream, unicode u, size_t *value, int *ret)
{
	getstream_pretty_stream(stream, &stream);
	return file_charlen_stream_(stream, u, value, ret);
}

static int file_strlen_Pretty(addr stream, addr pos, size_t *value, int *ret)
{
	getstream_pretty_stream(stream, &stream);
	return file_strlen_stream_(stream, pos, value, ret);
}

static int listen_Pretty(addr stream, int *ret)
{
	getstream_pretty_stream(stream, &stream);
	return listen_stream_(stream, ret);
}

static int clear_input_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return clear_input_stream_(stream);
}

static int finish_output_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return finish_output_stream_(stream);
}

static int force_output_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return force_output_stream_(stream);
}

static int clear_output_Pretty(addr stream)
{
	getstream_pretty_stream(stream, &stream);
	return clear_output_stream_(stream);
}

static int termsize_Pretty(addr stream, size_t *value, int *ret)
{
	getstream_pretty_stream(stream, &stream);
	return termsize_stream_(stream, value, ret);
}

_g void init_stream_pretty(void)
{
	DefineStreamSet(Pretty, close);
	DefineStream___(Pretty, read_binary);
	DefineStream___(Pretty, readf_binary);
	DefineStream___(Pretty, read_byte);
	DefineStream___(Pretty, unread_byte);
	DefineStream___(Pretty, write_binary);
	DefineStream___(Pretty, write_byte);
	DefineStreamSet(Pretty, read_char);
	DefineStreamSet(Pretty, read_hang);
	DefineStreamSet(Pretty, unread_char);
	DefineStreamSet(Pretty, write_char);
	DefineStreamSet(Pretty, terpri);
	DefineStreamSet(Pretty, getleft);
	DefineStreamSet(Pretty, setleft);
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
	DefineStreamSet(Pretty, file_charlen);
	DefineStreamSet(Pretty, file_strlen);
	DefineStreamSet(Pretty, listen);
	DefineStreamSet(Pretty, clear_input);
	DefineStreamSet(Pretty, finish_output);
	DefineStreamSet(Pretty, force_output);
	DefineStreamSet(Pretty, clear_output);
	DefineStreamDef(Pretty, exitpoint);
	DefineStreamSet(Pretty, termsize);
}

