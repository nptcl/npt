#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "bigcons.h"
#include "bignum.h"
#include "bit.h"
#include "character.h"
#include "charqueue.h"
#include "cmpl.h"
#include "code.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control.h"
#include "equal.h"
#include "eval.h"
#include "file.h"
#include "function.h"
#include "gc.h"
#include "hashtable.h"
#include "heap.h"
#include "integer.h"
#include "number.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "pointer.h"
#include "quote.h"
#include "rational.h"
#include "readtable.h"
#include "sequence.h"
#include "stream.h"
#include "stream_string.h"
#include "strtype.h"
#include "structure.h"
#include "symbol.h"
#include "token.h"
#include "type_table.h"

/*
 *  chartable
 */
struct chartable {
	unsigned chartype : 1;
	unsigned exponent : 1;
};
static struct chartable CharTable[0x80];

static void init_chartable(void)
{
	static const char *const str1 =
		"!\"#$%&'(),;<=>?[\\]^_`{|}~.+-*/@"
		"0123456789"
		"abcdefghijklmnopqrstuvwxyz"
		"ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	static const char *const str2 = "defslDEFSL";
	const char *str;

	/* Illegal */
	cleartype(CharTable);
	/* char */
	for (str = str1; *str; str++)
		CharTable[(int)*str].chartype = 1;
	/* exponent */
	for (str = str2; *str; str++)
		CharTable[(int)*str].exponent = 1;
}


/*
 *  readlabel
 */
enum ReadLabel_Index {
	ReadLabel_Label,
	ReadLabel_Value,
	ReadLabel_List,
	ReadLabel_Size
};

#define RefReadLabel	RefArrayA2
#define GetReadLabel	GetArrayA2
#define SetReadLabel	SetArrayA2

static void gensym_readlabel(addr pos)
{
	CheckType(pos, LISPSYSTEM_READLABEL);
	SetUser(pos, 1);
}

static void normal_readlabel(addr pos)
{
	CheckType(pos, LISPSYSTEM_READLABEL);
	SetUser(pos, 0);
}

static int gensymp_readlabel(addr pos)
{
	CheckType(pos, LISPSYSTEM_READLABEL);
	return GetUser(pos);
}

static void readlabel_heap(Execute ptr, addr *ret, addr label)
{
	addr pos, gensym;

	Check(! integerp(label), "label error");
	heap_array2(&pos, LISPSYSTEM_READLABEL, ReadLabel_Size);
	gensym_readlabel(pos);
	make_gensym_char(ptr, "READ-LABEL", label, &gensym);
	SetReadLabel(pos, ReadLabel_Label, label);
	SetReadLabel(pos, ReadLabel_Value, gensym);
	*ret = pos;
}


/*
 *  readinfo
 */
enum ReadInfo_Index {
	ReadInfo_Package,
	ReadInfo_Queue,
	ReadInfo_Label,
	ReadInfo_Size
};

enum ReadInfo_State {
	ReadInfo_State_First,
	ReadInfo_State_Colon1,
	ReadInfo_State_Colon2,
	ReadInfo_State_Gensym
};

struct readinfo_struct {
	unsigned preserving : 1;
	unsigned recursive : 1;
	unsigned escape : 1;
	unsigned dot : 1;
	unsigned replace : 1;
	enum ReadInfo_State state : 4;
	size_t backquote;
};

#define RefReadInfo			RefArraySS
#define GetReadInfo			GetArraySS
#define SetReadInfo			SetArraySS
#define ReadInfoStruct(x)	((struct readinfo_struct *)PtrBodySSa(x, ReadInfo_Size))

static void readinfo_local(LocalRoot local, addr *ret)
{
	addr pos, value;
	struct readinfo_struct *str;

	/* readinfo */
	local_smallsize(local, &pos, LISPSYSTEM_READINFO,
			ReadInfo_Size, sizeoft(struct readinfo_struct));
	str = ReadInfoStruct(pos);
	memset(str, 0, sizeoft(struct readinfo_struct));
	/* charqueue */
	charqueue_local(local, &value, 0);
	SetReadInfo(pos, ReadInfo_Queue, value);
	/* label */
	consnil_heap(&value);
	SetReadInfo(pos, ReadInfo_Label, value);
	/* result */
	*ret = pos;
}

static void readinfo_symbol(addr *ret)
{
	GetConst(SYSTEM_READINFO_SPECIAL, ret);
}

static void getreadinfo(Execute ptr, addr *ret)
{
	addr symbol;

	readinfo_symbol(&symbol);
	getspecialcheck_local(ptr, symbol, ret);
	CheckType(*ret, LISPSYSTEM_READINFO);
}

static void pushreadinfo(Execute ptr, addr *ret)
{
	addr symbol, info;

	readinfo_symbol(&symbol);
	readinfo_local(ptr->local, &info);
	pushspecial_control(ptr, symbol, info);
	*ret = info;
}

static void pushreadinfo_recursive(Execute ptr, addr *ret)
{
	unsigned preserving, replace;
	addr symbol, info, label;
	struct readinfo_struct *str;
	size_t backquote;

	/* outside read */
	readinfo_symbol(&symbol);
	getspecial_local(ptr, symbol, &info);
	if (info == Unbound)
		fmte("Outside read don't accept recursive-p parameter.", NULL);
	str = ReadInfoStruct(info);
	preserving = str->preserving;
	replace = str->replace;
	backquote = str->backquote;
	GetReadInfo(info, ReadInfo_Label, &label);

	/* push readinfo */
	readinfo_local(ptr->local, &info);
	str = ReadInfoStruct(info);
	str->preserving = preserving;
	str->replace = replace;
	str->backquote = backquote;
	str->recursive = 1;
	SetReadInfo(info, ReadInfo_Label, label);
	pushspecial_control(ptr, symbol, info);
	*ret = info;
}

static void getpackage_readinfo(Execute ptr, addr *ret)
{
	addr pos;
	getreadinfo(ptr, &pos);
	GetReadInfo(pos, ReadInfo_Package, ret);
}

static void setpackage_readinfo(Execute ptr, addr value)
{
	addr pos;
	getreadinfo(ptr, &pos);
	SetReadInfo(pos, ReadInfo_Package, value);
}

static void getqueue_readinfo(Execute ptr, addr *ret)
{
	addr pos;
	getreadinfo(ptr, &pos);
	GetReadInfo(pos, ReadInfo_Queue, ret);
}

static unsigned getpreserving_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->preserving;
}

static unsigned getescape_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->escape;
}

static void setescape_readinfo(Execute ptr, unsigned value)
{
	addr pos;

	Check(1U < value, "value error");
	getreadinfo(ptr, &pos);
	ReadInfoStruct(pos)->escape = value;
}

static unsigned getdot_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->dot;
}

static unsigned getreplace_readinfo(Execute ptr, addr *label)
{
	unsigned ret;
	addr pos;

	getreadinfo(ptr, &pos);
	ret = ReadInfoStruct(pos)->replace;
	if (ret) {
		GetReadInfo(pos, ReadInfo_Label, &pos);
		GetCar(pos, label);
	}

	return ret;
}

static enum ReadInfo_State getstate_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->state;
}

static void setstate_readinfo(Execute ptr, enum ReadInfo_State value)
{
	addr pos;

	getreadinfo(ptr, &pos);
	ReadInfoStruct(pos)->state = value;
}

static void clear_readinfo(Execute ptr)
{
	addr pos, value;
	struct readinfo_struct *str;

	getreadinfo(ptr, &pos);
	/* package */
	SetReadInfo(pos, ReadInfo_Package, Nil);
	/* queue */
	GetReadInfo(pos, ReadInfo_Queue, &value);
	clear_charqueue(value);
	/* state, escape */
	str = ReadInfoStruct(pos);
	str->escape = 0;
	str->state = ReadInfo_State_First;
}


/*
 *  readtype
 */
enum ReadTable_Type {
	ReadTable_Type_illegal,
	ReadTable_Type_whitespace,
	ReadTable_Type_constituent,
	ReadTable_Type_macro_term,
	ReadTable_Type_macro_nonterm,
	ReadTable_Type_escape_single,
	ReadTable_Type_escape_multiple,
	ReadTable_Type_SIZE
};

struct readtype_struct {
	unsigned dispatch : 1;
	enum ReadTable_Type type : 5;
	unicode code;
};

#define readtype_heap(p) \
	heap_smallsize((p), LISPSYSTEM_READTYPE, 1, sizeoft(struct readtype_struct))
#define PtrReadType(p) PtrBodySSa((p), 1)
#define ReadTypeStruct(p) ((struct readtype_struct *)PtrReadType(p))
#define GetReadType(p,v) GetArraySS((p), 0, (v))
#define SetReadType(p,v) SetArraySS((p), 0, (v))

static int dispatch_readtype(addr pos)
{
	CheckType(pos, LISPSYSTEM_READTYPE);
	return ReadTypeStruct(pos)->dispatch;
}

static void make_readtype(addr *ret,
		enum ReadTable_Type type, unicode code, unsigned dispatch)
{
	addr pos;
	struct readtype_struct *str;

	readtype_heap(&pos);
	str = ReadTypeStruct(pos);
	str->type = type;
	str->code = code;
	str->dispatch = dispatch;
	*ret = pos;
}

static void copy_readtype(addr *ret, addr copy)
{
	addr pos;
	struct readtype_struct *str;

	str = ReadTypeStruct(copy);
	make_readtype(&pos, str->type, str->code, str->dispatch);
	GetReadType(copy, &copy);
	SetReadType(pos, copy);
	*ret = pos;
}

static void array_readtype(addr array, enum ReadTable_Type type, unicode code)
{
	addr pos;
	make_readtype(&pos, type, code, 0);
	SetArrayA2(array, (size_t)code, pos);
}

static void macro_readtype(addr array,
		enum ReadTable_Type type,
		unicode code,
		unsigned dispatch,
		constindex index)
{
	addr pos, symbol;

	make_readtype(&pos, type, code, dispatch);
	GetConstant(index, &symbol);
	GetFunctionSymbol(symbol, &symbol);
	SetReadType(pos, symbol);
	SetArrayA2(array, (size_t)code, pos);
}

#define TermReadType(a,b,c) \
	macro_readtype(a,ReadTable_Type_macro_term,b,0,CONSTANT_SYSTEM_##c##_READER)
#define DispatchReadType(a,b,c) \
	macro_readtype(a,ReadTable_Type_macro_nonterm,b,1,CONSTANT_SYSTEM_##c##_READER)

static void default_macro_readtype(addr array)
{
	TermReadType(array, '"',  DOUBLE_QUOTE);
	TermReadType(array, '\'', SINGLE_QUOTE);
	TermReadType(array, '(',  PARENSIS_OPEN);
	TermReadType(array, ')',  PARENSIS_CLOSE);
	TermReadType(array, ';',  SEMICOLON);
	TermReadType(array, '`',  BACKQUOTE);
	TermReadType(array, ',',  COMMA);
	DispatchReadType(array, '#', SHARP);
}

static const char *const Default_WhiteSpace =
"\x09"      /* Horizontal Tab */
"\x20"      /* Space */
"\x0C"      /* Page */
"\x0A"      /* Linefeed, Newline */
"\x0D";     /* Return */

static const char *const Default_Constituent =
"0123456789"
"abcdefghijklmnopqrstuvwxyz"
"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
"!$%&*+-./:<=>?@[]^_{}~"
"\x08"  /* Backspace */
"\x7F"; /* Delete (Rubout) */

static void default_array_readtype(addr array)
{
	const char *str;

	/* whitespace */
	for (str = Default_WhiteSpace; *str; str++)
		array_readtype(array, ReadTable_Type_whitespace, (unicode)*str);
	/* constituent */
	for (str = Default_Constituent; *str; str++)
		array_readtype(array, ReadTable_Type_constituent, (unicode)*str);
	/* escape */
	array_readtype(array, ReadTable_Type_escape_single, '\\');
	array_readtype(array, ReadTable_Type_escape_multiple, '|');
	/* macro */
	default_macro_readtype(array);
}

static void dispatch_character(addr pos, unicode a, unicode b, constindex index)
{
	addr key, value, cons;

	/* hash */
	Check(isLowerCase(b), "case error");
	character2_heap(&key, a, b);
	intern_hashheap(pos, key, &cons);
	/* value */
	GetConstant(index, &value);
	GetFunctionSymbol(value, &value);
	Check(value == Unbound, "unbound error");
	/* set */
	SetCdr(cons, value);
}

#define DispatchCharacter(a,u,b,c) \
	dispatch_character(a,u,b,CONSTANT_SYSTEM_##c##_DISPATCH)

static void default_dispatch_readtype(addr pos, unicode u)
{
	DispatchCharacter(pos, u, 0x08, ERROR); /* backspace */
	DispatchCharacter(pos, u, 0x09, ERROR); /* htab */
	DispatchCharacter(pos, u, 0x0A, ERROR); /* newline */
	DispatchCharacter(pos, u, 0x0C, ERROR); /* page */
	DispatchCharacter(pos, u, 0x0D, ERROR); /* return */
	DispatchCharacter(pos, u, 0x20, ERROR); /* space */
	DispatchCharacter(pos, u, '=',  EQUAL);
	DispatchCharacter(pos, u, '#',  SHARP);
	DispatchCharacter(pos, u, '\'', SINGLE_QUOTE);
	DispatchCharacter(pos, u, '(',  PARENSIS_OPEN);
	DispatchCharacter(pos, u, ')',  PARENSIS_CLOSE);
	DispatchCharacter(pos, u, '*',  ASTERISK);
	DispatchCharacter(pos, u, ':',  COLON);
	DispatchCharacter(pos, u, '<',  LESS);
	DispatchCharacter(pos, u, '\\', BACKSLASH);
	DispatchCharacter(pos, u, '|',  OR);
	DispatchCharacter(pos, u, '+',  PLUS);
	DispatchCharacter(pos, u, '-',  MINUS);
	DispatchCharacter(pos, u, '.',  DOT);
	DispatchCharacter(pos, u, 'A',  ARRAY);
	DispatchCharacter(pos, u, 'B',  BINARY);
	DispatchCharacter(pos, u, 'C',  COMPLEX);
	DispatchCharacter(pos, u, 'O',  OCTAL);
	DispatchCharacter(pos, u, 'P',  PATHNAME);
	DispatchCharacter(pos, u, 'R',  RADIX);
	DispatchCharacter(pos, u, 'S',  STRUCTURE);
	DispatchCharacter(pos, u, 'X',  HEXADECIMAL);
}

static void array_readtype_heap(addr *ret)
{
	vector2_heap(ret, 0x80);
}

static void dispatch_readtype_heap(addr *ret)
{
	addr pos;
	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	*ret = pos;
}

static void make_array_readtype(addr *ret)
{
	addr pos;
	array_readtype_heap(&pos);
	default_array_readtype(pos);
	*ret = pos;
}

static void make_table_readtype(addr *ret)
{
	addr pos;
	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	*ret = pos;
}

static void make_dispatch_readtype(addr *ret)
{
	addr pos;
	dispatch_readtype_heap(&pos);
	default_dispatch_readtype(pos, '#');
	*ret = pos;
}


/*
 *  readtable
 */
#define GetArrayReadtable(p,v) GetArraySS((p), READTABLE_ARRAY, (v))
#define SetArrayReadtable(p,v) SetArraySS((p), READTABLE_ARRAY, (v))
#define GetTableReadtable(p,v) GetArraySS((p), READTABLE_TABLE, (v))
#define SetTableReadtable(p,v) SetArraySS((p), READTABLE_TABLE, (v))
#define GetDispatchReadtable(p,v) GetArraySS((p), READTABLE_DISPATCH, (v))
#define SetDispatchReadtable(p,v) SetArraySS((p), READTABLE_DISPATCH, (v))
#define PtrReadtable(p) PtrBodySSa((p), READTABLE_SIZE)
#define PtrCaseReadtable(p) ((enum ReadTable_Case *)PtrReadtable(p))

static void copy_array_readtable(addr from, addr to)
{
	int i;
	addr one;

	for (i = 0; i < 0x80; i++) {
		GetArrayA2(from, i, &one);
		if (one != Nil)
			copy_readtype(&one, one);
		SetArrayA2(to, i, one);
	}
}

static void copy_table_readtable(addr from, addr to)
{
	addr table, list, car, cdr, cell;
	size_t size, i;

	CheckType(from, LISPTYPE_HASHTABLE);
	CheckType(to, LISPTYPE_HASHTABLE);
	getsize_hashtable(from, &size);

	/* (maphash
	 *   (lambda (key value)
	 *     (setf (gethash key to) (copy-readtype value)))
	 *   from)
	 */
	GetTableHash(from, &table);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &cdr, &list);
			GetCons(cdr, &car, &cdr);
			copy_readtype(&cdr, cdr);
			intern_hashheap(to, car, &cell);
			SetCdr(cell, cdr);
		}
	}
}

static void copy_dispatch_readtable(addr from, addr to)
{
	addr table, list, car, cdr, cell;
	size_t size, i;

	CheckType(from, LISPTYPE_HASHTABLE);
	CheckType(to, LISPTYPE_HASHTABLE);
	getsize_hashtable(from, &size);

	/* (maphash
	 *   (lambda (key value)
	 *     (setf (gethash key to) value))
	 *   copy)
	 */
	GetTableHash(from, &table);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &cdr, &list);
			GetCons(cdr, &car, &cdr);
			intern_hashheap(to, car, &cell);
			SetCdr(cell, cdr);
		}
	}
}

_g void copy_readtable(addr from, addr to)
{
	addr a, b;

	/* case */
	*PtrCaseReadtable(to) = *PtrCaseReadtable(from);
	/* array */
	GetArrayReadtable(from, &a);
	GetArrayReadtable(to, &b);
	copy_array_readtable(a, b);
	/* table */
	GetTableReadtable(from, &a);
	GetTableReadtable(to, &b);
	clear_hashtable_heap(b);
	copy_table_readtable(a, b);
	/* dispatch */
	GetDispatchReadtable(from, &a);
	GetDispatchReadtable(to, &b);
	clear_hashtable_heap(b);
	copy_dispatch_readtable(a, b);
}

_g void copy_readtable_heap(addr from, addr *ret)
{
	addr to, a, b;

	heap_smallsize(&to, LISPTYPE_READTABLE,
			READTABLE_SIZE, sizeoft(enum ReadTable_Case));
	/* case */
	*PtrCaseReadtable(to) = *PtrCaseReadtable(from);
	/* array */
	GetArrayReadtable(from, &a);
	array_readtype_heap(&b);
	copy_array_readtable(a, b);
	SetArrayReadtable(to, b);
	/* table */
	GetTableReadtable(from, &a);
	make_table_readtype(&b);
	copy_table_readtable(a, b);
	SetTableReadtable(to, b);
	/* dispatch */
	GetDispatchReadtable(from, &a);
	dispatch_readtype_heap(&b);
	copy_dispatch_readtable(a, b);
	SetDispatchReadtable(to, b);
	/* result */
	*ret = to;
}

_g void copy_default_readtable(addr pos)
{
	int i;
	addr one;

	/* case */
	*PtrCaseReadtable(pos) = ReadTable_upcase;
	/* array */
	GetArrayReadtable(pos, &one);
	for (i = 0; i < 0x80; i++)
		SetArrayA2(one, i, Nil);
	default_array_readtype(one);
	/* table */
	GetTableReadtable(pos, &one);
	clear_hashtable_heap(one);
	/* dispatch */
	GetDispatchReadtable(pos, &one);
	clear_hashtable_heap(one);
	default_dispatch_readtype(one, '#');
}

_g void readtable_heap(addr *ret)
{
	addr pos, one;

	heap_smallsize(&pos, LISPTYPE_READTABLE,
			READTABLE_SIZE, sizeoft(enum ReadTable_Case));
	/* case */
	*PtrCaseReadtable(pos) = ReadTable_upcase;
	/* array */
	make_array_readtype(&one);
	SetArrayReadtable(pos, one);
	/* table */
	make_table_readtype(&one);
	SetTableReadtable(pos, one);
	/* dispatch */
	make_dispatch_readtype(&one);
	SetDispatchReadtable(pos, one);
	/* result */
	*ret = pos;
}

static void setreadtype_readtable(addr pos, unicode code, addr type)
{
	addr key;

	if (code < 0x80) {
		GetArrayReadtable(pos, &pos);
		SetArrayA2(pos, code, type);
	}
	else {
		GetTableReadtable(pos, &pos);
		character_heap(&key, code);
		intern_hashheap(pos, key, &pos);
		SetCdr(pos, type);
	}
}

_g void make_dispatch_macro_character(addr pos, addr character, int nonterm)
{
	unicode code;
	addr type, call;
	enum ReadTable_Type value;

	CheckType(pos, LISPTYPE_READTABLE);
	/* object */
	GetCharacter(character, &code);
	value = nonterm? ReadTable_Type_macro_nonterm: ReadTable_Type_macro_term;
	make_readtype(&type, value, code, 1);
	/* function */
	GetConst(SYSTEM_DISPATCH_FUNCTION, &call);
	GetFunctionSymbol(call, &call);
	Check(call == Unbound, "unbound error.");
	SetReadType(type, call);
	/* add readtable */
	setreadtype_readtable(pos, code, type);
}

#define DefaultDispatch(u,a,b) { \
	if (u == a) { \
		GetConst(SYSTEM_##b##_DISPATCH, &pos); \
		GetFunctionSymbol(pos, ret); \
		return; \
	} \
}
static void get_default_dispatch_sharp(addr code, addr *ret)
{
	addr pos;
	unicode u;

	GetCharacter(code, &u);
	u = toUpperUnicode(u);
	DefaultDispatch(u, 0x08, ERROR); /* backspace */
	DefaultDispatch(u, 0x09, ERROR); /* htab */
	DefaultDispatch(u, 0x0A, ERROR); /* newline */
	DefaultDispatch(u, 0x0C, ERROR); /* page */
	DefaultDispatch(u, 0x0D, ERROR); /* return */
	DefaultDispatch(u, 0x20, ERROR); /* space */
	DefaultDispatch(u, '=',  EQUAL);
	DefaultDispatch(u, '#',  SHARP);
	DefaultDispatch(u, '\'', SINGLE_QUOTE);
	DefaultDispatch(u, '(',  PARENSIS_OPEN);
	DefaultDispatch(u, ')',  PARENSIS_CLOSE);
	DefaultDispatch(u, '*',  ASTERISK);
	DefaultDispatch(u, ':',  COLON);
	DefaultDispatch(u, '<',  LESS);
	DefaultDispatch(u, '\\', BACKSLASH);
	DefaultDispatch(u, '|',  OR);
	DefaultDispatch(u, '+',  PLUS);
	DefaultDispatch(u, '-',  MINUS);
	DefaultDispatch(u, '.',  DOT);
	DefaultDispatch(u, 'A',  ARRAY);
	DefaultDispatch(u, 'B',  BINARY);
	DefaultDispatch(u, 'C',  COMPLEX);
	DefaultDispatch(u, 'O',  OCTAL);
	DefaultDispatch(u, 'P',  PATHNAME);
	DefaultDispatch(u, 'R',  RADIX);
	DefaultDispatch(u, 'S',  STRUCTURE);
	DefaultDispatch(u, 'X',  HEXADECIMAL);
	*ret = Nil;
}

_g void get_default_dispatch_macro(addr code1, addr code2, addr *ret)
{
	unicode u;

	GetCharacter(code1, &u);
	if (u != '#')
		fmte("The character ~S is not dispatch macro.", code1, NULL);
	get_default_dispatch_sharp(code2, ret);
}


/*
 *  macro_character_execute
 */
static void readtype_readtable(addr pos, unicode c, addr *ret)
{
	if (c < 0x80) {
		GetArrayReadtable(pos, &pos);
		GetArrayA2(pos, c, ret);
	}
	else {
		GetTableReadtable(pos, &pos);
		findvalue_unicode_hashtable(pos, c, ret);
	}
}

static int macro_character_call(Execute ptr, int *result, addr *ret,
		addr call, addr stream, addr code)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	if (funcall_control(ptr, call, stream, code, NULL)) {
		return runcode_free_control(ptr, control);
	}
	if (lengthvalues_control(ptr) == 0) {
		*result = 0;
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		*result = 1;
	}
	Return1(free_control(ptr, control));
	localhold_end(hold);

	return 0;
}

static int macro_character_execute(Execute ptr, int *result, addr *ret,
		unicode c, addr stream, addr table)
{
	addr call, code;
	LocalHold hold;

	character_heap(&code, c);
	readtype_readtable(table, c, &call);
	if (call == Nil)
		goto error;
	GetReadType(call, &call);
	if (call == Nil)
		goto error;
	*result = 0;

	hold = LocalHold_local_push(ptr, code);
	Return1(macro_character_call(ptr, result, ret, call, stream, code));
	localhold_end(hold);
	return 0;

error:
	*result = 0;
	fmte("Character ~S don't have a macro code.", code, NULL);
	return 0;
}

_g void get_dispatch_macro_character(addr pos, unicode u1, unicode u2, addr *ret)
{
	addr check;

	CheckType(pos, LISPTYPE_READTABLE);
	/* dispatch check */
	readtype_readtable(pos, u1, &check);
	if (check == Nil)
		goto error;
	if (! dispatch_readtype(check))
		goto error;
	/* find */
	GetDispatchReadtable(pos, &check);
	u2 = toUpperUnicode(u2);
	findvalue_character2_hashtable(check, u1, u2, ret);
	return;

error:
	character_heap(&check, u1);
	fmte("The character ~S is not dispatch macro.", check, NULL);
}

_g void rem_dispatch_macro_character(addr pos, unicode u1, unicode u2)
{
	addr check, key;

	CheckType(pos, LISPTYPE_READTABLE);
	/* dispatch check */
	readtype_readtable(pos, u1, &check);
	if (check == Nil)
		goto error;
	if (! dispatch_readtype(check))
		goto error;
	/* delete */
	GetDispatchReadtable(pos, &check);
	u2 = toUpperUnicode(u2);
	findcons_character2_hashtable(check, u1, u2, &key);
	if (key != Nil) {
		GetCar(key, &key);
		delete_hashtable(check, key);
	}
	return;

error:
	character_heap(&check, u1);
	fmte("The character ~S is not dispatch macro.", check, NULL);
}

_g void set_dispatch_macro_character(addr pos, unicode u1, unicode u2, addr call)
{
	addr check, cons;

	CheckType(pos, LISPTYPE_READTABLE);
	/* dispatch check */
	readtype_readtable(pos, u1, &check);
	if (check == Nil)
		goto error;
	if (! dispatch_readtype(check))
		goto error;
	/* intern */
	GetDispatchReadtable(pos, &check);
	u2 = toUpperUnicode(u2);
	findcons_character2_hashtable(check, u1, u2, &cons);
	if (cons != Nil) {
		SetCdr(cons, call);
	}
	else {
		character2_heap(&cons, u1, u2);
		intern_hashheap(check, cons, &cons);
		SetCdr(cons, call);
	}
	return;

error:
	character_heap(&check, u1);
	fmte("The character ~S is not dispatch macro.", check, NULL);
}

#define DefaultTermMacro(u,a,b) { \
	if (u == a) { \
		GetConst(SYSTEM_##b##_READER, &pos); \
		GetFunctionSymbol(pos, ret); \
		*nonterm = 0; \
		return; \
	} \
}
#define DefaultNonTermMacro(u,a,b) { \
	if (u == a) { \
		GetConst(SYSTEM_##b##_READER, &pos); \
		GetFunctionSymbol(pos, ret); \
		*nonterm = 1; \
		return; \
	} \
}
_g void get_default_macro_character(unicode u, addr *ret, int *nonterm)
{
	addr pos;

	DefaultTermMacro(u, '"',  DOUBLE_QUOTE);
	DefaultTermMacro(u, '\'', SINGLE_QUOTE);
	DefaultTermMacro(u, '(',  PARENSIS_OPEN);
	DefaultTermMacro(u, ')',  PARENSIS_CLOSE);
	DefaultTermMacro(u, ';',  SEMICOLON);
	DefaultTermMacro(u, '`',  BACKQUOTE);
	DefaultTermMacro(u, ',',  COMMA);
	DefaultNonTermMacro(u, '#', SHARP);
	*ret = Nil;
	*nonterm = 0;
}

_g void get_macro_character(addr pos, unicode u, addr *ret, int *nonterm)
{
	addr type;
	struct readtype_struct *str;

	CheckType(pos, LISPTYPE_READTABLE);
	readtype_readtable(pos, u, &type);
	if (type == Nil) {
		*ret = Nil;
		*nonterm = 0;
		return;
	}

	str = ReadTypeStruct(type);
	if (str->type == ReadTable_Type_macro_term) {
		GetReadType(type, ret);
		*nonterm = 0;
	}
	else if (str->type == ReadTable_Type_macro_nonterm) {
		GetReadType(type, ret);
		*nonterm = 1;
	}
	else {
		*ret = Nil;
		*nonterm = 0;
	}
}

_g void set_macro_character(addr pos, unicode u, int nonterm, addr call)
{
	addr type;
	enum ReadTable_Type value;

	CheckType(pos, LISPTYPE_READTABLE);
	value = nonterm? ReadTable_Type_macro_nonterm: ReadTable_Type_macro_term;
	make_readtype(&type, value, u, 0);
	SetReadType(type, call);
	/* add readtable */
	setreadtype_readtable(pos, u, type);
}

static int readtype_whitespace(unicode u)
{
	if (0x80 <= u) return 0;
	return strchr(Default_WhiteSpace, (int)u) != NULL;
}

static int readtype_constituent(unicode u)
{
	if (0x80 <= u) return 0;
	return strchr(Default_Constituent, (int)u) != NULL;
}

#define ReadTypeTermMacro(u,a,b) { \
	if (u == a) { \
		GetConst(SYSTEM_##b##_READER, &pos); \
		GetFunctionSymbol(pos, ret); \
		return 1; \
	} \
}
static int readtype_termmacro(unicode u, addr *ret)
{
	addr pos;

	ReadTypeTermMacro(u, '"',  DOUBLE_QUOTE);
	ReadTypeTermMacro(u, '\'', SINGLE_QUOTE);
	ReadTypeTermMacro(u, '(',  PARENSIS_OPEN);
	ReadTypeTermMacro(u, ')',  PARENSIS_CLOSE);
	ReadTypeTermMacro(u, ';',  SEMICOLON);
	ReadTypeTermMacro(u, '`',  BACKQUOTE);
	ReadTypeTermMacro(u, ',',  COMMA);

	return 0;
}

static int readtype_sharpmacro(unicode u, addr *ret)
{
	addr pos;

	if (u == '#') {
		GetConst(SYSTEM_SHARP_READER, &pos);
		GetFunctionSymbol(pos, ret);
		return 1;
	}

	return 0;
}

static void delete_dispatch_macro(addr pos, unicode u)
{
	addr table, list, car, cdr;
	size_t size, i;

	CheckType(pos, LISPTYPE_HASHTABLE);
	getsize_hashtable(pos, &size);

	/* (maphash
	 *   (lambda (key value)
	 *     (if (eq (car key) u)
	 *       (remhash key pos)))
	 *   pos)
	 */
	GetTableHash(pos, &table);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &cdr, &list);
			GetCar(cdr, &car);
			if (refcharacter2a(car) == u)
				delete_hashtable(pos, car);
		}
	}
}

static void delete_readtype(addr pos, unicode u)
{
	addr check;

	readtype_readtable(pos, u, &check);
	if (check == Nil)
		return;
	if (dispatch_readtype(check)) {
		GetDispatchReadtable(pos, &check);
		delete_dispatch_macro(check, u);
	}
	/* delete */
	if (u < 0x80) {
		GetArrayReadtable(pos, &pos);
		SetArrayA2(pos, u, Nil);
	}
	else {
		GetTableReadtable(pos, &pos);
		findcons_unicode_hashtable(pos, u, &check);
		if (check != Nil) {
			GetCar(check, &check);
			delete_hashtable(pos, check);
		}
	}
}

static void setreadtype_default(addr pos,
		unicode u, enum ReadTable_Type type, addr call)
{
	addr one;

	make_readtype(&one, type, u, 0);
	SetReadType(one, call);
	setreadtype_readtable(pos, u, one);
}

static void setdispatch_default(addr pos, unicode u, addr call)
{
	addr one;

	make_readtype(&one, ReadTable_Type_macro_nonterm, u, 1);
	SetReadType(one, call);
	setreadtype_readtable(pos, u, one);

	GetDispatchReadtable(pos, &one);
	default_dispatch_readtype(one, u);
}

_g void set_syntax_from_default(unicode u1, unicode u2, addr to)
{
	addr pos;

	delete_readtype(to, u1);
	if (readtype_whitespace(u2)) {
		setreadtype_default(to, u1, ReadTable_Type_whitespace, Nil);
		return;
	}
	if (readtype_constituent(u2)) {
		setreadtype_default(to, u1, ReadTable_Type_constituent, Nil);
		return;
	}
	if (u2 == '\\') {
		setreadtype_default(to, u1, ReadTable_Type_escape_single, Nil);
		return;
	}
	if (u2 == '|') {
		setreadtype_default(to, u1, ReadTable_Type_escape_multiple, Nil);
		return;
	}
	if (readtype_termmacro(u2, &pos)) {
		setreadtype_default(to, u1, ReadTable_Type_macro_term, pos);
		return;
	}
	if (readtype_sharpmacro(u2, &pos)) {
		setdispatch_default(to, u1, pos);
		return;
	}
	/* delete only */
}

static void copy_dispatch_macro(unicode u1, unicode u2, addr to, addr from)
{
	addr table, list, car, cdr;
	size_t size, i;

	CheckType(from, LISPTYPE_HASHTABLE);
	CheckType(to, LISPTYPE_HASHTABLE);
	getsize_hashtable(from, &size);

	/* (maphash ...) */
	GetTableHash(from, &table);
	for (i = 0; i < size; i++) {
		GetArrayHash(table, i, &list);
		while (list != Nil) {
			GetCons(list, &cdr, &list);
			GetCons(cdr, &car, &cdr);
			if (refcharacter2a(car) == u2) {
				character2_heap(&car, u1, refcharacter2b(car));
				intern_hashheap(to, car, &car);
				SetCdr(car, cdr);
			}
		}
	}
}

_g void set_syntax_from_char(unicode u1, unicode u2, addr to, addr from)
{
	addr one, type, call;
	struct readtype_struct *str;

	delete_readtype(to, u1);
	readtype_readtable(from, u2, &type);
	if (type != Nil) {
		str = ReadTypeStruct(type);
		GetReadType(type, &call);
		/* set-readtype */
		make_readtype(&one, str->type, u1, str->dispatch);
		setreadtype_readtable(to, u1, one);
		SetReadType(one, call);
		/* set-dispatch */
		if (str->dispatch) {
			GetDispatchReadtable(to, &to);
			GetDispatchReadtable(from, &from);
			copy_dispatch_macro(u1, u2, to, from);
		}
	}
}


/*
 *  tokentype
 */
static int checktable_char(unicode c)
{
	return (c < 0x80) && (CharTable[c].chartype);
}

static int checktable_base(unsigned base, unicode c)
{
	return ! getvalue_digit(base, c, &base);
}

static int checktable_sign(unicode c)
{
	return (c == '+') || (c == '-');
}

static int checktable_exponent(unicode c)
{
	return (c < 0x80) && (CharTable[c].exponent);
}

static int checktable_potential(unsigned base, unicode c)
{
	return checktable_base(base, c)
		|| c == '+' || c == '-' || c == '/'
		|| c == '.' || c == '^' || c == '_'
		|| checktable_exponent(c);
}

static int checktable_firstpotential(unsigned base, unicode c)
{
	return checktable_base(base, c)
		|| c == '+' || c == '-' || c == '.' || c == '^' || c == '_';
}

static int checktable_isdigit(unicode c)
{
	return (c < 0x80) && isDigitCase(c);
}

static int checktable_isalpha(unicode c)
{
	return (c < 0x80) && isAlphabetic(c);
}

/* for debug */
/* #define OUTPUTSTATE(x, c)  printf("[%s:%c]\n", x, c) */
#define OUTPUTSTATE(x, c)  /*donothing*/

#define ReadtableNext() { \
	getchar_charqueue(queue, i++, &c); \
	if (c && !checktable_char(c)) goto error; \
	if (!pot) pot = checktable_base(base, c); \
}

#define checkbasegoto(base, digit, c, label) { \
	if (checktable_base(base, c)) { \
		if (digit) digit = checktable_isdigit(c); \
		goto label; \
	} \
}

enum TokenType {
	TokenType_symbol = 0,
	TokenType_potential,
	TokenType_integer,
	TokenType_float,
	TokenType_ratio,
	TokenType_dot,
	TokenType_empty,
	TokenType_error
};

static enum TokenType tokentype(unsigned base, addr queue)
{
	unicode c;
	size_t i;
	int first, pot, digit;

	/* init */
	i = 0;
	pot = 0;
	digit = 1;

	/* first */
	ReadtableNext();
	first = checktable_firstpotential(base, c);
	OUTPUTSTATE("first", c);
	if (c == 0) goto error;
	if (checktable_sign(c)) goto digit1;
	checkbasegoto(base, digit, c, digit2);
	if (checktable_isdigit(c)) goto float1;
	if (c == '.') goto dot1;
	goto symbol;

	/* dot check */
dot1:
	ReadtableNext();
	OUTPUTSTATE("dot1", c);
	if (c == 0) goto token_dot;
	if (checktable_isdigit(c)) goto float6;
	if (c == '.') goto dot2;
	goto symbol;

dot2:
	ReadtableNext();
	OUTPUTSTATE("dot2", c);
	if (c == '.') goto dot2;
	if (c == 0) goto error;
	goto symbol;

	/* integer */
digit1:
	ReadtableNext();
	OUTPUTSTATE("digit1", c);
	checkbasegoto(base, digit, c, digit2);
	if (checktable_isdigit(c)) goto float1;
	if (c == '.') goto float5;
	goto symbol_sign;

digit2:
	ReadtableNext();
	OUTPUTSTATE("digit2", c);
	if (c == 0) goto token_digit;
	if (c == '.') goto digit3;
	if (c == '/') goto ratio1;
	checkbasegoto(base, digit, c, digit2);
	if (checktable_isdigit(c)) goto float1;
	if (checktable_exponent(c)) goto exponent1;
	goto symbol;

digit3:
	ReadtableNext();
	OUTPUTSTATE("digit3", c);
	if (c == 0) goto token_digit;
	if (checktable_isdigit(c)) goto float4;
	if (checktable_exponent(c)) goto exponent3;
	goto symbol;

	/* ratio */
ratio1:
	ReadtableNext();
	OUTPUTSTATE("ratio1", c);
	if (checktable_base(base, c)) goto ratio2;
	goto symbol;

ratio2:
	ReadtableNext();
	OUTPUTSTATE("ratio2", c);
	if (c == 0) goto token_ratio;
	if (checktable_base(base, c)) goto ratio2;
	goto symbol;

	/* float */
float1:
	ReadtableNext();
	OUTPUTSTATE("float1", c);
	if (checktable_isdigit(c)) goto float1;
	if (checktable_exponent(c)) goto exponent1;
	if (c == '.') goto float3;
	goto symbol;

float3:
	ReadtableNext();
	OUTPUTSTATE("float3", c);
	if (checktable_exponent(c)) goto exponent1;
	if (checktable_isdigit(c)) goto float4;
	goto symbol;

float4:
	ReadtableNext();
	OUTPUTSTATE("float4", c);
	if (c == 0) goto check_float;
	if (checktable_exponent(c)) goto exponent1;
	if (checktable_isdigit(c)) goto float4;
	goto symbol;

float5:
	ReadtableNext();
	OUTPUTSTATE("float5", c);
	if (checktable_isdigit(c)) goto float6;
	goto symbol;

float6:
	ReadtableNext();
	OUTPUTSTATE("float6", c);
	if (c == 0) goto check_float;
	if (checktable_isdigit(c)) goto float6;
	if (checktable_exponent(c)) goto exponent1;
	goto symbol;

check_float:
	if (digit) goto token_float;
	goto token_potential;

	/* exponent */
exponent1:
	ReadtableNext();
	OUTPUTSTATE("exponent1", c);
	if (checktable_sign(c)) goto exponent2;
	if (checktable_isdigit(c)) goto exponent3;
	goto symbol_exponent;

exponent2:
	ReadtableNext();
	OUTPUTSTATE("exponent2", c);
	if (checktable_isdigit(c)) goto exponent3;
	goto symbol_sign;

exponent3:
	ReadtableNext();
	OUTPUTSTATE("exponent3", c);
	if (c == 0) goto check_float;
	if (checktable_isdigit(c)) goto exponent3;
	goto symbol;

	/* symbol */
symbol:
	OUTPUTSTATE("symbol", c);
	if (first == 0) goto token_symbol;
	goto potential_symbol;

potential:
	ReadtableNext();
	OUTPUTSTATE("potential", c);
potential_symbol:
	if (c == 0) goto token_potential;
	if (checktable_sign(c)) goto potential_sign;
	if (checktable_base(base, c)) goto potential;
	if (checktable_isalpha(c)) goto potential_marker;
	if (checktable_potential(base, c)) goto potential;
	goto token_symbol;

	/* symbol-sign */
symbol_sign:
	OUTPUTSTATE("symbol_sign", c);
	if (first == 0) goto token_symbol;
	goto potential_sign_symbol;

potential_sign:
	ReadtableNext();
	OUTPUTSTATE("potential", c);
potential_sign_symbol:
	if (c == 0) goto token_symbol;
	if (checktable_sign(c)) goto potential_sign;
	if (checktable_base(base, c)) goto potential;
	if (checktable_isalpha(c)) goto potential_marker;
	if (checktable_potential(base, c)) goto potential;
	goto token_symbol;

	/* symbol-marker */
symbol_exponent:
	OUTPUTSTATE("symbol_exponent", c);
	if (first == 0) goto token_symbol;
	goto potential_marker_symbol;

potential_marker:
	ReadtableNext();
	OUTPUTSTATE("potential_marker", c);
potential_marker_symbol:
	if (c == 0) goto token_potential;
	if (checktable_base(base, c)) goto potential;
	if (checktable_isalpha(c)) goto token_symbol;
	if (checktable_sign(c)) goto potential_sign;
	if (checktable_potential(base, c)) goto potential;
	goto token_symbol;

	/* token */
token_digit:
	OUTPUTSTATE("token_digit", '-');
	return TokenType_integer;

token_ratio:
	OUTPUTSTATE("token_ratio", '-');
	return TokenType_ratio;

token_float:
	OUTPUTSTATE("token_float", '-');
	return TokenType_float;

token_dot:
	OUTPUTSTATE("token_dot", '-');
	return TokenType_dot;

token_symbol:
	OUTPUTSTATE("token_symbol", '-');
	return TokenType_symbol;

token_potential:
	OUTPUTSTATE("token_potential", '-');
	return pot? TokenType_potential: TokenType_symbol;

error:
	OUTPUTSTATE("error", '-');
	return TokenType_error;
}


/*
 *  maketoken
 */
static unsigned getreadbase(Execute ptr)
{
	addr one;
	fixnum value;

	GetConst(SPECIAL_READ_BASE, &one);
	getspecialcheck_local(ptr, one, &one);
	Check(GetType(one) != LISPTYPE_FIXNUM, "type error");
	GetFixnum(one, &value);
	if (! isBaseChar(value)) {
		fixnum_heap(&one, (fixnum)value);
		fmte("base ~a must be a number between 2 and 36.", one, NULL);
	}

	return (unsigned)value;
}

static void maketoken_package(Execute ptr, addr *ret, addr queue, addr package)
{
	enum TokenType token;
	unsigned base;
	addr name;

	/* keyword */
	if (package == T)
		GetConst(PACKAGE_KEYWORD, &package);

	if (getescape_readinfo(ptr)) {
		/* escapemonde, make force symbol */
		make_charqueue_heap(queue, &name);
		/* intern package - name */
		intern_package(package, name, ret);
		return;
	}

	base = getreadbase(ptr);
	token = tokentype(base, queue);
	if (token == TokenType_symbol || token == TokenType_potential) {
		make_charqueue_heap(queue, &name);
		/* intern package - symbol */
		intern_package(package, name, ret);
	}
	else {
		fmte("Token-type error", NULL);
		return;
	}
}

static void maketoken_normal(Execute ptr, addr *ret)
{
	unsigned base;
	addr package, queue, name;

	/* table have package */
	getpackage_readinfo(ptr, &package);
	getqueue_readinfo(ptr, &queue);
	if (package != Nil) {
		maketoken_package(ptr, ret, queue, package);
		return;
	}

	/* no package */
	if (getescape_readinfo(ptr)) {
		/* escapemode, make force symbol */
		make_charqueue_heap(queue, &name);
		/* intern name */
		intern_default_package(ptr, name, ret);
		return;
	}

	base = getreadbase(ptr);
	switch (tokentype(base, queue)) {
		case TokenType_symbol:
		case TokenType_potential:
			make_charqueue_heap(queue, &name);
			/* intern *package* - symbol */
			intern_default_package(ptr, name, ret);
			break;

		case TokenType_integer:
			maketoken_integer(ptr->local, queue, base, ret);
			break;

		case TokenType_float:
			maketoken_float(ptr, queue, ret);
			break;

		case TokenType_ratio:
			maketoken_ratio(ptr->local, queue, base, ret);
			break;

		case TokenType_dot:
			if (getdot_readinfo(ptr) == 0)
				fmte("dot no allowed here.", NULL);
			GetConst(SYSTEM_READTABLE_DOT, ret);
			break;

		case TokenType_error:
		default:
			fmte("token error", NULL);
			return;
	}
}

static void maketoken_gensym(Execute ptr, addr *ret)
{
	unsigned base;
	addr queue, symbol, name;

	/* no package */
	getqueue_readinfo(ptr, &queue);
	if (getescape_readinfo(ptr)) {
		/* escapemode, make force symbol */
		make_charqueue_heap(queue, &name);
		symbol_heap(&symbol);
		SetNameSymbol(symbol, name);
		*ret = symbol;
		return;
	}

	base = getreadbase(ptr);
	switch (tokentype(base, queue)) {
		case TokenType_symbol:
		case TokenType_potential:
			make_charqueue_heap(queue, &name);
			symbol_heap(&symbol);
			SetNameSymbol(symbol, name);
			*ret = symbol;
			break;

		default:
			Abort("token error");
			break;
	}
}

static int read_suppress_p(Execute ptr)
{
	addr pos;

	/* *read-suppress */
	GetConst(SPECIAL_READ_SUPPRESS, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	return pos != Nil;
}

static void maketoken(Execute ptr, addr *ret)
{
	/* *read-suppress */
	if (read_suppress_p(ptr)) {
		*ret = Nil;
		return;
	}

	/* make token */
	if (getstate_readinfo(ptr) != ReadInfo_State_Gensym)
		maketoken_normal(ptr, ret);
	else
		maketoken_gensym(ptr, ret);
}


/*
 *  pushchar_readtable
 */
static unicode charmode_readtable(addr pos, unicode c)
{
	switch (*PtrCaseReadtable(pos)) {
		case ReadTable_upcase:
			return toUpperUnicode(c);

		case ReadTable_downcase:
			return toLowerUnicode(c);

		case ReadTable_preserve:
			return c;

		case ReadTable_invert:
			if ('a' <= c && c <= 'z') return c - 'a' + 'A';
			if ('A' <= c && c <= 'Z') return c - 'A' + 'a';
			return c;

		default:
			fmte("Unknown readtable-case type.", NULL);
			break;
	}

	return 0;
}

static int tokenmode_readtable(Execute ptr)
{
	unsigned base;
	size_t size;
	addr queue;

	/* escape */
	if (getescape_readinfo(ptr))
		return TokenType_symbol;

	/* empty (for keyword) */
	getqueue_readinfo(ptr, &queue);
	getsize_charqueue(queue, &size);
	if (size == 0)
		return TokenType_empty;

	base = getreadbase(ptr);
	return tokentype(base, queue);
}

static void setpackage_readtable(Execute ptr)
{
	int type;
	addr queue, package;

	type = tokenmode_readtable(ptr);
	switch (type) {
		case TokenType_empty: /* for keyword */
			setpackage_readinfo(ptr, T);
			break;

		case TokenType_symbol:
		case TokenType_potential:
			getqueue_readinfo(ptr, &queue);
			make_charqueue_heap(queue, &package);
			setpackage_readinfo(ptr, package);
			clear_charqueue(queue);
			break;

		default:
			fmte("Package token type error.", NULL);
			return;
	}
}

static void pushchar_readtable(Execute ptr, addr pos, unicode c, int escape)
{
	unsigned bitescape;
	enum ReadInfo_State bitstate;
	addr queue;

	getqueue_readinfo(ptr, &queue);
	bitescape = getescape_readinfo(ptr);
	bitstate = getstate_readinfo(ptr);
	/* mode0 : readsymbol
	 * modd1 : read comma
	 * mode2 : read comma second
	 * mode3 : readsymol next
	 *
	 * mode0 - aaa
	 * mode1 - :
	 * mode1 - aaa:
	 * mode2 - ::
	 * mode2 - aaa::
	 * mode2 - :aaa
	 * mode2 - ::aaa
	 * mode2 - aaa:bbb
	 * mode2 - aaa::bbb
	 */
	if (escape) {
		if (bitescape == 0)
			setescape_readinfo(ptr, 1);
		if (bitstate == ReadInfo_State_Colon1)
			setstate_readinfo(ptr, ReadInfo_State_Colon2);
		push_charqueue_local(ptr->local, queue, c);
		return;
	}

	switch (bitstate) {
		case ReadInfo_State_First:
			if (c == ':') {
				setstate_readinfo(ptr, ReadInfo_State_Colon1);
				setpackage_readtable(ptr);
				return;
			}
			break;

		case ReadInfo_State_Colon1:
			setstate_readinfo(ptr, ReadInfo_State_Colon2);
			if (c != ':') break;
			return;

		case ReadInfo_State_Colon2:
		case ReadInfo_State_Gensym:
			if (c == ':') {
				fmte("comma error", NULL);
				return;
			}
			break;

		default:
			fmte("mode error", NULL);
			return;
	}

	/* push char */
	if (! escape)
		c = charmode_readtable(pos, c);
	push_charqueue_local(ptr->local, queue, c);
}


/*
 *  readtable
 */
static enum ReadTable_Type readtable_typetable(addr pos, unicode c)
{
	readtype_readtable(pos, c, &pos);
	if (pos == Nil)
		return ReadTable_Type_illegal;
	return ReadTypeStruct(pos)->type;
}

enum ReadTable_Result {
	ReadTable_Result_normal,
	ReadTable_Result_macro,
	ReadTable_Result_eof
};

static enum ReadTable_Result readtable_result(Execute ptr,
		addr *ret, addr stream, addr table)
{
	enum ReadTable_Type type;
	int result;
	unicode x, y, z;

step1:
	result = read_char_stream(stream, &x);
	if (result) goto eof;

	/* step2 */
	type = readtable_typetable(table, x);
	switch (type) {
		case ReadTable_Type_illegal:
			goto illegal_error;

		case ReadTable_Type_whitespace:
			/* step3 */
			goto step1;

		case ReadTable_Type_macro_term:
		case ReadTable_Type_macro_nonterm:
			unread_char_stream(stream, x);
			goto macro; /* return one value */

		case ReadTable_Type_escape_single:
			/* step5 */
			result = read_char_stream(stream, &y);
			if (result) goto error;
			pushchar_readtable(ptr, table, y, 1);
			goto step8;

		case ReadTable_Type_escape_multiple:
			/* step6 */
			goto step9;

		case ReadTable_Type_constituent:
			/* step7 */
			pushchar_readtable(ptr, table, x, 0);
			break;

		default:
			goto error;
	}

step8:
	result = read_char_stream(stream, &y);
	if (result) goto step10;
	type = readtable_typetable(table, y);
	switch (type) {
		case ReadTable_Type_constituent:
		case ReadTable_Type_macro_nonterm:
			pushchar_readtable(ptr, table, y, 0);
			goto step8;

		case ReadTable_Type_escape_single:
			result = read_char_stream(stream, &z);
			if (result) goto error;
			pushchar_readtable(ptr, table, z, 1);
			goto step8;

		case ReadTable_Type_escape_multiple:
			goto step9;

		case ReadTable_Type_illegal:
			goto illegal_error;

		case ReadTable_Type_macro_term:
			unread_char_stream(stream, y);
			goto step10;

		case ReadTable_Type_whitespace:
			if (getpreserving_readinfo(ptr))
				unread_char_stream(stream, y);
			goto step10;

		default:
			goto error;
	}

step9:
	result = read_char_stream(stream, &y);
	if (result) goto error;
	type = readtable_typetable(table, y);
	switch (type) {
		case ReadTable_Type_macro_term:
		case ReadTable_Type_macro_nonterm:
		case ReadTable_Type_constituent:
		case ReadTable_Type_whitespace:
			pushchar_readtable(ptr, table, y, 1);
			goto step9;

		case ReadTable_Type_escape_single:
			result = read_char_stream(stream, &z);
			if (result) goto error;
			pushchar_readtable(ptr, table, z, 1);
			goto step9;

		case ReadTable_Type_escape_multiple:
			goto step8;

		case ReadTable_Type_illegal:
			goto illegal_error;

		default:
			goto error;
	}

step10:
	maketoken(ptr, ret);
	goto final;

illegal_error:
	fmte("Illegal character error", NULL);
	goto error;

error:
	fmte("readtable error", NULL);
	goto eof;

final:
	return ReadTable_Result_normal;
macro:
	return ReadTable_Result_macro;
eof:
	return ReadTable_Result_eof;
}

static int readtable_novalue(Execute ptr, int *result, addr *ret,
		addr stream, addr table)
{
	int check;
	addr pos;
	unicode u;

	/* read */
	clear_readinfo(ptr);
	switch (readtable_result(ptr, &pos, stream, table)) {
		case ReadTable_Result_normal:
			*result = 0;
			*ret = pos;
			return 0;

		case ReadTable_Result_eof:
			*result = 1;
			return 0;

		default:
			break;
	}

	/* macro execute */
	if (read_char_stream(stream, &u)) {
		fmte("eof error", NULL);
		return 1;
	}

	if (macro_character_execute(ptr, &check, &pos, u, stream, table)) {
		return 1;
	}

	if (check) {
		*result = 0;
		*ret = pos;
		return 0;
	}
	else {
		/* return no value */
		*result = -1;
		return 0;
	}
}

static int readtable_front(Execute ptr,
		int *result, addr *ret, addr stream, addr table)
{
	int check;

	for (;;) {
		if (readtable_novalue(ptr, &check, ret, stream, table)) {
			*result = 1;
			return 1;
		}
		if (0 <= check)
			break;
	}
	if (read_suppress_p(ptr))
		*ret = Nil;
	*result = check;
	return 0;
}


/*
 *  read
 */
enum ReadTable_float float_readtable(Execute ptr)
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

_g enum ReadTable_Case readcase_readtable(Execute ptr)
{
	addr pos;
	getreadtable(ptr, &pos);
	return getcase_readtable(pos);
}

_g enum ReadTable_Case getcase_readtable(addr pos)
{
	Check(GetType(pos) != LISPTYPE_READTABLE, "type error");
	return *PtrCaseReadtable(pos);
}

_g void setcase_readtable(addr pos, enum ReadTable_Case mode)
{
	Check(GetType(pos) != LISPTYPE_READTABLE, "type error");
	*PtrCaseReadtable(pos) = mode;
}

_g void getreadtable(Execute ptr, addr *ret)
{
	addr pos;

	GetConst(SPECIAL_READTABLE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	if (GetType(pos) != LISPTYPE_READTABLE)
		TypeError(pos, READTABLE);
	*ret = pos;
}

static int read_call(Execute ptr, addr stream, int *result, addr *ret)
{
	addr table;
	getreadtable(ptr, &table);
	return readtable_front(ptr, result, ret, stream, table);
}

_g int read_stream(Execute ptr, addr stream, int *result, addr *ret)
{
	int check;
	addr control, info;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	pushreadinfo(ptr, &info);
	check = read_call(ptr, stream, result, ret);
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return1(free_check_control(ptr, control, check));
	localhold_end(hold);

	return 0;
}

_g int read_preserving(Execute ptr, addr stream, int *result, addr *ret)
{
	int check;
	addr control, info;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	pushreadinfo(ptr, &info);
	ReadInfoStruct(info)->preserving = 1;
	check = read_call(ptr, stream, result, ret);
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return1(free_check_control(ptr, control, check));
	localhold_end(hold);

	return 0;
}

_g int read_recursive(Execute ptr, addr stream, int *result, addr *ret)
{
	int check;
	addr control, info;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	pushreadinfo_recursive(ptr, &info);
	check = read_call(ptr, stream, result, ret);
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return1(free_check_control(ptr, control, check));
	localhold_end(hold);

	return 0;
}

_g int read_from_string(Execute ptr, int *result, addr *ret, addr pos)
{
	addr stream;
	LocalHold hold;

	open_input_string_stream(&stream, pos);
	hold = LocalHold_local_push(ptr, stream);
	if (read_stream(ptr, stream, result, ret)) return 1;
	localhold_end(hold);
	close_stream(stream);

	return 0;
}

_g int readstring(addr *ret, const char *code)
{
	int result;
	addr stream;

	open_input_char_stream(&stream, code);
	if (read_stream(Execute_Thread, stream, &result, ret))
		fmte("Cannot catch a system signal.", NULL);
	close_stream(stream);

	return result;
}

_g addr readr(const char *code)
{
	addr pos;
	if (readstring(&pos, code))
		return Nil;
	else
		return pos;
}


/*****************************************************************************
 *  macro character
 *****************************************************************************/
/* (defun double-quote-reader (stream character) ...) -> * */
static void function_reader_double_quote(Execute ptr, addr stream, addr code)
{
	int escape;
	unicode u;
	addr queue;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	charqueue_local(local, &queue, 0);
	escape = 0;
	for (;;) {
		if (read_char_stream(stream, &u))
			fmte("The string token must terminate by \".", NULL);
		if (escape) {
			push_charqueue_local(local, queue, u);
			escape = 0;
			continue;
		}
		if (u == '\"') {
			break;
		}
		if (u == '\\') {
			escape = 1;
		}
		else {
			push_charqueue_local(local, queue, u);
		}
	}
	make_charqueue_heap(queue, &queue);
	setresult_control(ptr, queue);
}

static void defun_double_quote_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DOUBLE_QUOTE_READER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_double_quote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun single-quote-reader (stream character) ...) -> * */
static int quote_macro(Execute ptr, int *result, constindex index, addr stream)
{
	int check;
	addr pos, quote;

	/* readtable */
	if (read_recursive(ptr, stream, &check, &pos))
		return 1;
	if (check) {
		*result = 1;
		return 0;
	}

	/* ([index] pos) */
	GetConstant(index, &quote);
	list_heap(&pos, quote, pos, NULL);
	setresult_control(ptr, pos);
	*result = 0;
	return 0;
}

static void function_reader_single_quote(Execute ptr, addr stream, addr code)
{
	int check;

	if (quote_macro(ptr, &check, CONSTANT_COMMON_QUOTE, stream))
		return;
	if (check)
		fmte("After character ' must be an object, but EOF.", NULL);
}

static void defun_single_quote_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SINGLE_QUOTE_READER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_single_quote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-open-reader (stream character) ...) -> * */
static int gensym_check_readlabel(addr label, addr check)
{
	addr value;

	if (gensymp_readlabel(label)) {
		GetReadLabel(label, ReadLabel_Value, &value);
		if (value == check)
			return 1;
	}

	return 0;
}

static void push_replace_readlabel(addr label, addr pos)
{
	addr list;

	GetReadLabel(label, ReadLabel_List, &list);
	cons_heap(&list, pos, list);
	SetReadLabel(label, ReadLabel_List, list);
}

static void queue_readlabel(Execute ptr, addr queue, addr pos)
{
	addr list, label, check;

	if (getreplace_readinfo(ptr, &list)) {
		while (list != Nil) {
			GetCons(list, &label, &list);
			if (gensym_check_readlabel(label, pos)) {
				tailqueue(queue, &check);
				push_replace_readlabel(label, check);
			}
		}
	}
}

static void dotqueue_readlabel(Execute ptr, addr queue, addr pos)
{
	dotqueue(queue, pos);
	queue_readlabel(ptr, queue, pos);
}

static void pushqueue_readlabel(Execute ptr, addr queue, addr pos)
{
	pushqueue_heap(queue, pos);
	queue_readlabel(ptr, queue, pos);
}

static int readdotcons(Execute ptr, addr queue, addr dot, addr pos, int *mode)
{
	addr root;

	switch (*mode) {
		case 0:
			if (dot == pos) {
				rootqueue(queue, &root);
				if (root == Nil) /* (. */
					return 1; /* error */
				*mode = 1; /* (a b . */
				return 2; /* continue */
			}
			return 0;

		case 1:
			if (dot == pos)  /* (a b . . */
				return 1;  /* error */
			dotqueue_readlabel(ptr, queue, pos);
			*mode = 2;  /* (a b . c */
			return 2; /* continue */

		default: /* (a b . c d */
			return 1;  /* error */
	}
}

static int delimited_execute(Execute ptr, addr stream, unicode limit)
{
	int mode, check;
	unicode u;
	addr table, pos, root, dotsym, queue;
	LocalHold hold;

	mode = 0;
	GetConst(SYSTEM_READTABLE_DOT, &dotsym);
	getreadtable(ptr, &table);
	queue_heap(&queue);
	hold = LocalHold_local_push(ptr, queue);
	for (;;) {
		if (read_char_stream(stream, &u))
			fmte("Don't allow end-of-file in the parensis.", NULL);
		if (readtable_typetable(table, u) == ReadTable_Type_whitespace) {
			/* discard character */
			continue;
		}
		if (u == limit) {
			/* discard limit character */
			break;
		}
		unread_char_stream(stream, u);
		if (readtable_novalue(ptr, &check, &pos, stream, table))
			return 1;
		if (0 < check) {
			fmte("read error", NULL);
			return 1;
		}
		if (check < 0)
			continue;

		/* dot */
		check = readdotcons(ptr, queue, dotsym, pos, &mode);
		if (check == 1) {
			fmte("dot no allowed here", NULL);
			return 1;
		}
		if (check == 2)
			continue;
		pushqueue_readlabel(ptr, queue, pos);
	}
	localhold_end(hold);

	if (mode == 1) { /* (a b . ) */
		fmte("dot no allowed here", NULL);
		return 1;
	}

	rootqueue(queue, &root);
	setresult_control(ptr, root);

	return 0;
}

_g int read_delimited_list(Execute ptr, addr stream, unicode limit, int recursive)
{
	int check;
	addr control, info;
	struct readinfo_struct *str;

	/* push */
	push_return_control(ptr, &control);
	/* code */
	if (recursive)
		pushreadinfo_recursive(ptr, &info);
	else
		pushreadinfo(ptr, &info);
	str = ReadInfoStruct(info);
	str->dot = 1;
	check = delimited_execute(ptr, stream, limit);
	return free_check_control(ptr, control, check);
}

static void function_reader_parensis_open(Execute ptr, addr stream, addr code)
{
	(void)read_delimited_list(ptr, stream, ')', 1);
}

static void defun_parensis_open_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_OPEN_READER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_parensis_open);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-close-reader (stream character) ...) -> * */
static void function_reader_parensis_close(Execute ptr, addr stream, addr code)
{
	fmte("unmatch close parenthiesis ).", NULL);
}

static void defun_parensis_close_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_CLOSE_READER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_parensis_close);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun semicolon-reader (stream character) ...) -> * */
static void function_reader_semicolon(Execute ptr, addr stream, addr code)
{
	int result;
	unicode u;

	for (;;) {
		result = read_char_stream(stream, &u);
		if (result || u == '\n') break;
	}
	setvalues_nil_control(ptr);
}

static void defun_semicolon_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SEMICOLON_READER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_semicolon);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun backquote-reader (stream character) ...) -> * */
static int read_backquote(Execute ptr, addr stream, int *result, addr *ret)
{
	int check;
	addr control, info;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	pushreadinfo_recursive(ptr, &info);
	ReadInfoStruct(info)->backquote++;
	check = read_call(ptr, stream, result, ret);
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return1(free_check_control(ptr, control, check));
	localhold_end(hold);

	return 0;
}

static void function_reader_backquote(Execute ptr, addr stream, addr code)
{
	int check;

	if (read_backquote(ptr, stream, &check, &code))
		return;
	if (check)
		fmte("After backquote ` must be an object.", NULL);
	quote_back_heap(&code, code);
	setresult_control(ptr, code);
}

static void defun_backquote_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BACKQUOTE_READER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_backquote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* (defun comma-reader (stream character) ...) -> * */
static int read_comma(Execute ptr, addr stream, int *result, addr *ret)
{
	int check;
	addr control, info;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	pushreadinfo_recursive(ptr, &info);
	ReadInfoStruct(info)->backquote--;
	check = read_call(ptr, stream, result, ret);
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return1(free_check_control(ptr, control, check));
	localhold_end(hold);

	return 0;
}

static void function_reader_comma(Execute ptr, addr stream, addr code)
{
	int check;
	unicode u;

	/* check */
	getreadinfo(ptr, &code);
	if (ReadInfoStruct(code)->backquote == 0)
		fmte("The comma , is not inside backquote.", NULL);

	/* read */
	if (read_char_stream(stream, &u))
		fmte("After comma , must be a character or an object.", NULL);
	if (u == '@') {
		if (read_comma(ptr, stream, &check, &code))
			return;
		if (check)
			fmte("After ,@ must be an object.", NULL);
		quote_atsign_heap(&code, code);
	}
	else if (u == '.') {
		if (read_comma(ptr, stream, &check, &code))
			return;
		if (check)
			fmte("After ,. must be an object.", NULL);
		quote_dot_heap(&code, code);
	}
	else {
		unread_char_stream(stream, u);
		if (read_comma(ptr, stream, &check, &code))
			return;
		if (check)
			fmte("After comma , must be an object.", NULL);
		quote_comma_heap(&code, code);
	}
	setresult_control(ptr, code);
}

static void defun_comma_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_COMMA_READER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_comma);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sharp-reader (stream character) ...) -> * */
static int dispatch_parameter(LocalRoot local, addr stream, addr *ret, unicode *uret)
{
	unicode u;
	addr cons;
	LocalStack stack;

	/* no digit */
	if (read_char_stream(stream, &u))
		return 1;
	if (! isDigitCase(u)) {
		*ret = Nil;
		*uret = u;
		return 0;
	}

	/* parse digit */
	push_local(local, &stack);
	bigcons_local(local, &cons);
	for (;;) {
		push_bigcons(local, cons, 10, (unsigned)(u - '0'));
		if (read_char_stream(stream, &u))
			return 1;
		if (! isDigitCase(u)) {
			*uret = u;
			break;
		}
	}
	integer_cons_alloc(NULL, ret, signplus_bignum, cons);
	rollback_local(local, stack);

	return 0;
}

static void function_reader_sharp(Execute ptr, addr stream, addr code1)
{
	addr arg, pos, code2;
	unicode c1, c2;

	/* #[integer][code] */
	if (dispatch_parameter(ptr->local, stream, &arg, &c2)) {
		fmte("Invalid dispatch character form ~S.", code1, NULL);
		return;
	}
	character_heap(&code2, c2);
	c2 = toUpperUnicode(c2);

	/* macro character */
	getreadtable(ptr, &pos);
	GetDispatchReadtable(pos, &pos);
	GetCharacter(code1, &c1);
	findvalue_character2_hashtable(pos, c1, c2, &pos);
	if (pos == Nil) {
		fmte("There is no macro character ~S-~S.", code1, code2, NULL);
		return;
	}

	(void)funcall_control(ptr, pos, stream, code2, arg, NULL);
}

static void defun_sharp_reader(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SHARP_READER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_reader_sharp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroReader);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
	/* (setq dispatch-function #'sharp-reader) */
	GetConst(SYSTEM_DISPATCH_FUNCTION, &symbol);
	SetFunctionSymbol(symbol, pos);
}

static void make_macro_reader(void)
{
	defun_double_quote_reader(); /* " */
	defun_single_quote_reader(); /* ' */
	defun_parensis_open_reader(); /* ( */
	defun_parensis_close_reader();  /* ) */
	defun_semicolon_reader(); /* ; */
	defun_backquote_reader(); /* ` */
	defun_comma_reader(); /* , */
	defun_sharp_reader(); /* # */
}


/*****************************************************************************
 *  dispatch character
 *****************************************************************************/
/* (defun error-dispatch (stream code arg) ...) -> * */
static void function_dispatch_error(Execute ptr, addr stream, addr code, addr arg)
{
	fmte("don't allow ~S dispatch character.", code, NULL);
}

static void defun_error_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ERROR_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_error);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun equal-dispatch (stream code arg) ...) -> * */
static void read_replace_finalize(Execute ptr)
{
	addr pos, value;

	getdata_control(ptr, &pos);
	GetCons(pos, &pos, &value);
	ReadInfoStruct(pos)->replace = value != Nil? 1: 0;
}

static int read_replace(Execute ptr, addr stream, int *result, addr *ret)
{
	int check;
	addr pos, value, control, code;
	struct readinfo_struct *str;
	LocalHold hold;

	/* readinfo */
	getreadinfo(ptr, &pos);
	str = ReadInfoStruct(pos);
	value = str->replace? T: Nil;
	str->replace = 1;
	hold = LocalHold_array(ptr, 1);
	/* finalize */
	push_finalize_control(ptr, &control);
	cons_local(ptr->local, &pos, pos, value);
	syscall_code(ptr->local, &code, p_read_replace_finalize, pos);
	setfinalize_control(ptr, control, code);
	/* code */
	check = read_recursive(ptr, stream, result, ret);
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return1(free_check_control(ptr, control, check));
	localhold_end(hold);

	return 0;
}

static int find_readlabel(addr key, addr list, addr *ret)
{
	addr label, check;

	while (list != Nil) {
		GetCons(list, &label, &list);
		GetReadLabel(label, ReadLabel_Label, &check);
		if (eql_function(key, check)) {
			if (ret)
				GetReadLabel(label, ReadLabel_Value, ret);
			return 1;
		}
	}

	return 0;
}

static void pushlabel_readinfo(Execute ptr, addr value, addr *ret)
{
	addr cons, label, next;

	getreadinfo(ptr, &cons);
	GetReadInfo(cons, ReadInfo_Label, &cons);
	Check(! consp(cons), "type error");
	GetCar(cons, &next);
	if (find_readlabel(value, next, NULL))
		fmte("The #n= label ~S already exists.", value, NULL);
	readlabel_heap(ptr, &label, value);
	cons_heap(&next, label, next);
	SetCar(cons, next);
	*ret = label;
}

static void replace_cons_readlabel(Execute ptr, addr pos, addr left, addr right)
{
	addr car, cdr;

	GetCons(pos, &car, &cdr);
	if (car == left)
		SetCar(pos, right);
	if (cdr == left)
		SetCdr(pos, right);
}

static void replace_vector_readlabel(Execute ptr, addr pos, addr left, addr right)
{
	addr check;
	size_t size, i;

	lenarray(pos, &size);
	for (i = 0; i < size; i++) {
		getarray(pos, i, &check);
		if (check == left)
			setarray(pos, i, right);
	}
}

static void replace_array_readlabel(Execute ptr, addr pos, addr left, addr right)
{
	addr check;
	struct array_struct *str;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	if (str->type != ARRAY_TYPE_T) return;
	/* general array */
	size = str->size;
	GetArrayInfo(pos, ARRAY_INDEX_MEMORY, &pos);
	for (i = 0; i < size; i++) {
		arraygen_get(pos, i, &check);
		if (check == left)
			arraygen_set(pos, i, right);
	}
}

static void replace_readlabel(Execute ptr, addr replace, addr left, addr right)
{
	switch (GetType(replace)) {
		case LISPTYPE_CONS:
			replace_cons_readlabel(ptr, replace, left, right);
			break;

		case LISPTYPE_VECTOR:
			replace_vector_readlabel(ptr, replace, left, right);
			break;

		case LISPTYPE_ARRAY:
			replace_array_readlabel(ptr, replace, left, right);
			break;

		default:
			break;
	}
}

static void closelabel_readlabel(Execute ptr, addr label, addr pos)
{
	addr gensym, list, replace;

	Check(! gensymp_readlabel(label), "gensymp error");
	/* error #n= #n# */
	GetReadLabel(label, ReadLabel_Value, &gensym);
	if (pos == gensym)
		fmte("The #n= value don't replace self value #n#.", NULL);
	/* replace */
	GetReadLabel(label, ReadLabel_List, &list);
	while (list != Nil) {
		GetCons(list, &replace, &list);
		replace_readlabel(ptr, replace, gensym, pos);
	}
	/* result */
	SetReadLabel(label, ReadLabel_List, Nil);
	SetReadLabel(label, ReadLabel_Value, pos);
	normal_readlabel(label);
}

static void function_dispatch_equal(Execute ptr, addr stream, addr code, addr arg)
{
	int check;
	addr pos, label;

	pushlabel_readinfo(ptr, arg, &label);
	if (read_replace(ptr, stream, &check, &pos))
		return;
	if (check)
		fmte("After dispatch character ~S must be an object.", code, NULL);
	closelabel_readlabel(ptr, label, pos);
	setresult_control(ptr, pos);
}

static void defun_equal_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_EQUAL_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_equal);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun sharp-dispatch (stream code arg) ...) -> * */
static void function_dispatch_sharp(Execute ptr, addr stream, addr code, addr arg)
{
	addr pos;

	getreadinfo(ptr, &pos);
	GetReadInfo(pos, ReadInfo_Label, &pos);
	Check(! consp(pos), "type error");
	GetCar(pos, &pos);
	if (! find_readlabel(arg, pos, &pos))
		fmte("The #n# label ~S is not exist.", arg, NULL);
	setresult_control(ptr, pos);
}

static void defun_sharp_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SHARP_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_sharp);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun single-quote-dispatch (stream code arg) ...) -> * */
static void function_dispatch_single_quote(Execute ptr,
		addr stream, addr code, addr arg)
{
	int check;

	if (quote_macro(ptr, &check, CONSTANT_COMMON_FUNCTION, stream))
		return;
	if (check)
		fmte("After character #' must be a function-designer, but EOF.", NULL);
}

static void defun_single_quote_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_SINGLE_QUOTE_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_single_quote);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-open-dispatch (stream code arg) ...) -> * */
static void make_vector_normal(addr cons, size_t size, addr *ret)
{
	addr vector, pos;
	size_t i, index;

	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		index = size - i - 1;
		GetCons(cons, &pos, &cons);
		setarray(vector, index, pos);
	}
	*ret = vector;
}

static void make_vector_limit(addr cons, size_t size, addr *ret)
{
	addr vector, pos;
	size_t i;

	vector_heap(&vector, size);
	nreverse_list_unsafe(&cons, cons);
	pos = Nil;
	for (i = 0; i < size; i++) {
		if (cons != Nil)
			GetCons(cons, &pos, &cons);
		setarray(vector, i, pos);
	}
	*ret = vector;
}

static int find_vector_eq_unsafe(addr key, addr vector)
{
	addr check;
	size_t size, i;

	lenarray(vector, &size);
	for (i = 0; i < size; i++) {
		getarray(vector, i, &check);
		if (key == check)
			return 1;
	}

	return 0;
}

static void vector_readlabel(Execute ptr, addr pos)
{
	addr list, label, value;

	if (getreplace_readinfo(ptr, &list)) {
		while (list != Nil) {
			GetCons(list, &label, &list);
			if (gensymp_readlabel(label)) {
				GetReadLabel(label, ReadLabel_Value, &value);
				if (find_vector_eq_unsafe(value, pos))
					push_replace_readlabel(label, pos);
			}
		}
	}
}

static void function_dispatch_parensis_open(Execute ptr,
		addr stream, addr code, addr arg)
{
	int check;
	unicode u;
	addr table, root, pos;
	LocalRoot local;
	LocalStack stack;
	size_t size, limit;

	/* parameter */
	if (arg != Nil && GetIndex_integer(arg, &limit))
		fmte("Too large dispatch parameter ~S.", arg, NULL);

	/* read list */
	local = ptr->local;
	push_local(local, &stack);
	getreadtable(ptr, &table);
	root = Nil;
	size = 0;
	for (;;) {
		if (read_char_stream(stream, &u)) {
			fmte("Don't allow end-of-file in the parensis.", NULL);
		}
		if (readtable_typetable(table, u) == ReadTable_Type_whitespace) {
			continue;
		}
		if (u == ')') {
			break;
		}
		unread_char_stream(stream, u);
		if (read_recursive(ptr, stream, &check, &pos))
			return;
		if (check)
			fmte("read error", NULL);
		cons_local(local, &root, pos, root);
		size++;

		/* size check */
		if (arg != Nil && limit < size)
			fmte("Too many vector parameter.", NULL);
	}

	/* make vector */
	if (arg == Nil) {
		make_vector_normal(root, size, &root);
	}
	else {
		if (root == Nil) {
			if (limit == 0)
				vector_heap(&root, 0);
			else
				fmte("The vector don't initialize because body is empty #n().", NULL);
		}
		else {
			make_vector_limit(root, limit, &root);
		}
	}
	vector_readlabel(ptr, root);
	rollback_local(local, stack);
	setresult_control(ptr, root);
}

static void defun_parensis_open_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_OPEN_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_parensis_open);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun parensis-close-dispatch (stream code arg) ...) -> * */
static void function_dispatch_parensis_close(Execute ptr,
		addr stream, addr code, addr arg)
{
	fmte("unmatch close parenthiesis ).", NULL);
}

static void defun_parensis_close_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PARENSIS_CLOSE_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_parensis_close);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun asterisk-dispatch (stream code arg) ...) -> * */
static void asterisk_bitcons(Execute ptr, addr stream, addr code)
{
	unicode c;
	addr pos, cons;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	bitcons_local(local, &cons, 0);
	for (;;) {
		if (read_char_stream(stream, &c))
			break;
		if (c == '0') {
			push_bitcons(local, cons, 0);
		}
		else if (c == '1') {
			push_bitcons(local, cons, 1);
		}
		else {
			unread_char_stream(stream, c);
			break;
		}
	}
	bitmemory_cons_heap(&pos, cons);
	rollback_local(local, stack);

	setresult_control(ptr, pos);
}

static void asterisk_bitvector(Execute ptr, addr stream, size_t size)
{
	int last;
	unicode c;
	addr pos;
	size_t i;

	/* read bit-vector */
	bitmemory_heap(&pos, size);
	last = 0;
	for (i = 0; ; i++) {
		if (size <= i)
			fmte("Too large bit-vector.", NULL);
		if (read_char_stream(stream, &c))
			break;
		if (c == '0') {
			bitmemory_setint(pos, i, 0);
			last = 0;
		}
		else if (c == '1') {
			bitmemory_setint(pos, i, 1);
			last = 1;
		}
		else {
			unread_char_stream(stream, c);
			break;
		}
	}

	/* fill last value */
	for (; i < size; i++)
		bitmemory_setint(pos, i, last);

	/* result */
	setresult_control(ptr, pos);
}

static void function_dispatch_asterisk(Execute ptr, addr stream, addr code, addr arg)
{
	size_t size;

	if (arg == Nil) {
		asterisk_bitcons(ptr, stream, code);
	}
	else {
		if (GetIndex_integer(arg, &size))
			fmte("The index size ~S is too large.", arg, NULL);
		asterisk_bitvector(ptr, stream, size);
	}
}

static void defun_asterisk_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ASTERISK_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_asterisk);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun colon-dispatch (stream code arg) ...) -> * */
static void readtable_colon(Execute ptr, addr *ret, addr stream)
{
	addr table;

	getreadtable(ptr, &table);
	setstate_readinfo(ptr, ReadInfo_State_Gensym);
	switch (readtable_result(ptr, ret, stream, table)) {
		case ReadTable_Result_normal:
			break;

		case ReadTable_Result_eof:
			fmte("After character #: must be an object, but EOF.", NULL);
			break;

		case ReadTable_Result_macro:
			fmte("After character #: don't allow a macro character.", NULL);
			break;

		default:
			fmte("Invalid result.", NULL);
			break;
	}
}

static void function_dispatch_colon(Execute ptr, addr stream, addr code, addr arg)
{
	addr control, pos;

	/* push */
	push_close_control(ptr, &control);
	/* code */
	pushreadinfo_recursive(ptr, &pos);
	readtable_colon(ptr, &pos, stream);
	/* free */
	free_control(ptr, control);
	/* result */
	setresult_control(ptr, pos);
}

static void defun_colon_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_COLON_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_colon);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun less-dispatch (stream code arg) ...) -> * */
static void function_dispatch_less(Execute ptr, addr stream, addr code, addr arg)
{
	fmte("Cannot read #< dispatch character.", NULL);
}

static void defun_less_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_LESS_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_less);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun backslash-dispatch (stream code arg) ...) -> * */
static void function_dispatch_backslash(Execute ptr, addr stream, addr code, addr arg)
{
	int check;
	addr table, pos;

	getreadtable(ptr, &table);
	unread_char_stream(stream, '\\');
	if (read_recursive(ptr, stream, &check, &pos))
		return;
	if (check)
		fmte("Cannot read character name.", NULL);
	if (read_suppress_p(ptr)) {
		setresult_control(ptr, Nil);
		return;
	}
	if (! symbolp(pos))
		fmte("Invalid character type ~S.", pos, NULL);
	if (! find_name_char(&pos, pos))
		fmte("The character name ~S is not found.", pos, NULL);
	setresult_control(ptr, pos);
}

static void defun_backslash_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BACKSLASH_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_backslash);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun or-dispatch (stream code arg) ...) -> (values) */
static void function_dispatch_or(Execute ptr, addr stream, addr code, addr arg)
{
	unicode u;
	size_t count;

	count = 0;
	for (;;) {
		if (read_char_stream(stream, &u))
			break;
		if (u == '#') {
			if (read_char_stream(stream, &u))
				break;
			if (u == '|')
				count++;
		}
		else if (u == '|') {
			if (read_char_stream(stream, &u))
				break;
			if (u == '#') {
				if (count == 0)
					break;
				count--;
			}
		}
	}
	setvalues_nil_control(ptr);
}

static void defun_or_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_OR_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_or);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun plus-dispatch (stream code arg) ...) -> * */
static int read_feature(Execute ptr, addr stream, int *result, addr *ret)
{
	int check;
	addr control, symbol, keyword;
	LocalHold hold;

	/* (let ((*package* (find-package "KEYWORD")))
	 *   (read))
	 */
	/* push */
	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	/* code */
	GetConst(SPECIAL_PACKAGE, &symbol);
	GetConst(PACKAGE_KEYWORD, &keyword);
	pushspecial_control(ptr, symbol, keyword);
	check = read_recursive(ptr, stream, result, ret);
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return1(free_check_control(ptr, control, check));
	localhold_end(hold);

	return 0;
}

static int feature_eq_constant(addr pos, constindex index1, constindex index2)
{
	addr check;

	GetConstant(index1, &check);
	if (check == pos) return 1;
	GetConstant(index2, &check);
	return check == pos;
}
static int feature_not(addr pos)
{
	return feature_eq_constant(pos, CONSTANT_KEYWORD_NOT, CONSTANT_COMMON_NOT);
}
static int feature_and(addr pos)
{
	return feature_eq_constant(pos, CONSTANT_KEYWORD_AND, CONSTANT_COMMON_AND);
}
static int feature_or(addr pos)
{
	return feature_eq_constant(pos, CONSTANT_KEYWORD_OR, CONSTANT_COMMON_OR);
}

static int check_feature(addr list, addr pos);
static int check_feature_cons(addr list, addr cons)
{
	addr car, cdr;

	getcons(cons, &car, &cdr);
	/* not */
	if (feature_not(car)) {
		if (! singlep(cdr))
			fmte("The feature ~S must be a (not x) form.", cons, NULL);
		GetCar(cdr, &car);
		return ! check_feature(list, car);
	}
	/* and */
	if (feature_and(car)) {
		while (cdr != Nil) {
			getcons(cdr, &car, &cdr);
			if (! check_feature(list, car)) return 0;
		}
		return 1;
	}
	/* or */
	if (feature_or(car)) {
		while (cdr != Nil) {
			getcons(cdr, &car, &cdr);
			if (check_feature(list, car)) return 1;
		}
		return 0;
	}
	/* error */
	fmte("Invalid feature operator ~S.", car, NULL);
	return 0;
}

static int check_feature(addr list, addr pos)
{
	if (symbolp(pos)) {
		return find_list_eq_safe(pos, list);
	}
	else if (consp(pos)) {
		return check_feature_cons(list, pos);
	}
	else {
		fmte("Invalid feature ~S.", pos, NULL);
	}

	return 0;
}

static int read_ignore(Execute ptr, addr stream, int *result)
{
	/* (let ((*read-suppress* t)) ...) */
	int check;
	addr control, symbol;

	push_close_control(ptr, &control);
	GetConst(SPECIAL_READ_SUPPRESS, &symbol);
	pushspecial_control(ptr, symbol, T);
	check = read_recursive(ptr, stream, result, &stream);
	return free_check_control(ptr, control, check);
}

static void function_dispatch_plus(Execute ptr, addr stream, addr code, addr arg)
{
	int check;
	addr feature, form, list;
	LocalHold hold;

	/* read feature, read form */
	if (read_feature(ptr, stream, &check, &feature))
		return;
	if (check)
		fmte("After dispatch #+ must be a feature form.", NULL);

	/* check *features* */
	GetConst(SPECIAL_FEATURES, &list);
	getspecialcheck_local(ptr, list, &list);
	hold = LocalHold_local_push(ptr, feature);
	if (check_feature(list, feature)) {
		if (read_recursive(ptr, stream, &check, &form))
			return;
		localhold_end(hold);
		if (check)
			fmte("After dispatch #+feature must be a object.", NULL);
		setresult_control(ptr, form);
	}
	else {
		if (read_ignore(ptr, stream, &check))
			return;
		localhold_end(hold);
		if (check)
			fmte("After dispatch #+feature must be a object.", NULL);
		setvalues_nil_control(ptr);
	}
}

static void defun_plus_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PLUS_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_plus);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun minus-dispatch (stream code arg) ...) -> * */
static void function_dispatch_minus(Execute ptr, addr stream, addr code, addr arg)
{
	int check;
	addr feature, form, list;
	LocalHold hold;

	/* read feature, read form */
	if (read_feature(ptr, stream, &check, &feature))
		return;
	if (check)
		fmte("After dispatch #- must be a feature form.", NULL);

	/* check *features* */
	GetConst(SPECIAL_FEATURES, &list);
	getspecialcheck_local(ptr, list, &list);
	hold = LocalHold_local_push(ptr, feature);
	if (! check_feature(list, feature)) {
		if (read_recursive(ptr, stream, &check, &form))
			return;
		localhold_end(hold);
		if (check)
			fmte("After dispatch #-feature must be a object.", NULL);
		setresult_control(ptr, form);
	}
	else {
		if (read_ignore(ptr, stream, &check))
			return;
		localhold_end(hold);
		if (check)
			fmte("After dispatch #-feature must be a object.", NULL);
		setvalues_nil_control(ptr);
	}
}

static void defun_minus_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_MINUS_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_minus);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun dot-dispatch (stream code arg) ...) -> * */
static void function_dispatch_dot(Execute ptr, addr stream, addr code, addr arg)
{
	int check;
	addr eval;
	LocalHold hold;

	GetConst(SPECIAL_READ_EVAL, &eval);
	getspecialcheck_local(ptr, eval, &eval);
	if (eval == Nil)
		fmte("The dispatch #. don't read when *read-eval* is nil.", NULL);
	if (read_recursive(ptr, stream, &check, &eval))
		return;
	if (check)
		fmte("After dispatch #. must be a object.", NULL);
	if (read_suppress_p(ptr)) {
		setresult_control(ptr, Nil);
		return;
	}

	hold = LocalHold_local_push(ptr, eval);
	if (eval_object(ptr, eval, &eval))
		return;
	localhold_end(hold);
	setresult_control(ptr, eval);
}

static void defun_dot_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_DOT_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_dot);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun radix-dispatch (stream code arg) ...) -> * */
static int dispatch_radix_execute(Execute ptr, addr stream, fixnum base,
		int *result, addr *ret)
{
	int check;
	addr control, symbol, value;
	LocalHold hold;

	/* push */
	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	/* code */
	GetConst(SPECIAL_READ_BASE, &symbol);
	fixnum_heap(&value, base);
	pushspecial_control(ptr, symbol, value);
	check = read_recursive(ptr, stream, result, ret);
	if (*result == 0)
		localhold_set(hold, 0, *ret);
	Return1(free_check_control(ptr, control, check));
	localhold_end(hold);

	return 0;
}

static void dispatch_radix_read(Execute ptr, addr stream, fixnum base)
{
	int check;
	addr pos;

	if (dispatch_radix_execute(ptr, stream, base, &check, &pos))
		return;
	if (check)
		fmte("After radix dispatch #<n>r must be an integer.", NULL);
	if (read_suppress_p(ptr)) {
		setresult_control(ptr, Nil);
		return;
	}
	if (! rationalp(pos))
		fmte("The radix value ~S must be an integer.", pos, NULL);
	setresult_control(ptr, pos);
}

static void function_dispatch_radix(Execute ptr, addr stream, addr code, addr arg)
{
	fixnum value;

	GetFixnum_signed(arg, &value);
	if (! isBaseChar(value)) {
		if (! read_suppress_p(ptr))
			fmte("The radix ~S must be a number between 2 and 36.", arg, NULL);
	}
	dispatch_radix_read(ptr, stream, value);
}

static void defun_radix_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_RADIX_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_radix);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun binary-dispatch (stream code arg) ...) -> * */
static void function_dispatch_binary(Execute ptr, addr stream, addr code, addr arg)
{
	dispatch_radix_read(ptr, stream, 2);
}

static void defun_binary_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_BINARY_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_binary);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun octal-dispatch (stream code arg) ...) -> * */
static void function_dispatch_octal(Execute ptr, addr stream, addr code, addr arg)
{
	dispatch_radix_read(ptr, stream, 8);
}

static void defun_octal_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_OCTAL_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_octal);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun hexadecimal-dispatch (stream code arg) ...) -> * */
static void function_dispatch_hexadecimal(Execute ptr, addr stream, addr code, addr arg)
{
	dispatch_radix_read(ptr, stream, 16);
}

static void defun_hexadecimal_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_HEXADECIMAL_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_hexadecimal);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun complex-dispatch (stream code arg) ...) -> * */
static void function_dispatch_complex(Execute ptr, addr stream, addr code, addr arg)
{
	int check;
	addr form, pos, real, imag;

	if (read_recursive(ptr, stream, &check, &form))
		return;
	if (check)
		fmte("After complex dispatch must be a (real imag) form.", NULL);
	if (read_suppress_p(ptr)) {
		setresult_control(ptr, Nil);
		return;
	}
	pos = form;
	if (! consp(pos)) goto error;
	GetCons(pos, &real, &pos);
	if (! consp(pos)) goto error;
	GetCons(pos, &imag, &pos);
	if (pos != Nil) goto error;
	if (! realp(real)) goto error;
	if (! realp(imag)) goto error;
	complex_heap(&pos, real, imag);
	setresult_control(ptr, pos);
	return;

error:
	fmte("The complex dispatch ~S must be a (real imag) form.", form, NULL);
}

static void defun_complex_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_COMPLEX_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_complex);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun array-dispatch (stream code arg) ...) -> * */
static int find_array_eq_unsafe(addr key, addr array)
{
	addr check;
	size_t size, i;

	array_get_rowlength(array, &size);
	for (i = 0; i < size; i++) {
		(void)array_get_t(array, i, &check);
		if (key == check)
			return 1;
	}

	return 0;
}

static void array_readlabel(Execute ptr, addr pos)
{
	addr list, label, value;

	if (getreplace_readinfo(ptr, &list)) {
		while (list != Nil) {
			GetCons(list, &label, &list);
			if (gensymp_readlabel(label)) {
				GetReadLabel(label, ReadLabel_Value, &value);
				if (find_array_eq_unsafe(value, pos))
					push_replace_readlabel(label, pos);
			}
		}
	}
}

static void function_dispatch_array(Execute ptr, addr stream, addr code, addr arg)
{
	int check, ignore;
	addr form;

	ignore = read_suppress_p(ptr);
	if (arg == Nil && (! ignore))
		fmte("There is no rank parameter at the #<n>a dispatch.", NULL);
	if (read_recursive(ptr, stream, &check, &form))
		return;
	if (ignore) {
		setresult_control(ptr, Nil);
		return;
	}
	if (check)
		fmte("After array dispatch must be an initial-contents form.", NULL);
	array_contents_heap(&form, arg, form);
	array_readlabel(ptr, form);
	setresult_control(ptr, form);
}

static void defun_array_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_ARRAY_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_array);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun pathname-dispatch (stream code arg) ...) -> * */
static void function_dispatch_pathname(Execute ptr, addr stream, addr code, addr arg)
{
	int check;
	addr pos;

	if (read_recursive(ptr, stream, &check, &pos))
		return;
	if (check)
		fmte("After #P must be a pathname-designer.", NULL);
	if (read_suppress_p(ptr)) {
		setresult_control(ptr, Nil);
		return;
	}
	pathname_designer_heap(ptr, pos, &pos);
	setresult_control(ptr, pos);
}

static void defun_pathname_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_PATHNAME_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_pathname);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun structure-dispatch (stream code arg) ...) -> structure-object */
static void function_dispatch_structure(Execute ptr, addr stream, addr code, addr arg)
{
	int check;
	addr pos, rest;
	LocalHold hold;

	if (read_recursive(ptr, stream, &check, &pos))
		return;
	if (check)
		goto error;
	if (read_suppress_p(ptr)) {
		setresult_control(ptr, Nil);
		return;
	}
	if (! consp(pos))
		goto error;
	GetCons(pos, &pos, &rest);

	hold = LocalHold_local_push(ptr, pos);
	if (structure_constructor_common(ptr, pos, rest, &pos))
		return;
	localhold_end(hold);

	setresult_control(ptr, pos);
	return;

error:
	fmte("After #s must be (name key value ...) form.", NULL);
}

static void defun_structure_dispatch(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(SYSTEM_STRUCTURE_DISPATCH, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var3(pos, p_defun_dispatch_structure);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroDispatch);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void make_macro_dispatch(void)
{
	defun_error_dispatch();           /* whitespace */
	defun_equal_dispatch();           /* #= */
	defun_sharp_dispatch();           /* ## */
	defun_single_quote_dispatch();    /* #' */
	defun_parensis_open_dispatch();   /* #( */
	defun_parensis_close_dispatch();  /* #) */
	defun_asterisk_dispatch();        /* #* */
	defun_colon_dispatch();           /* #: */
	defun_less_dispatch();            /* #< */
	defun_backslash_dispatch();       /* #\ */
	defun_or_dispatch();              /* #| */
	defun_plus_dispatch();            /* #+ */
	defun_minus_dispatch();           /* #- */
	defun_dot_dispatch();             /* #. */
	defun_radix_dispatch();           /* #R */
	defun_binary_dispatch();          /* #B */
	defun_octal_dispatch();           /* #O */
	defun_hexadecimal_dispatch();     /* #X */
	defun_complex_dispatch();         /* #C */
	defun_array_dispatch();           /* #A */
	defun_pathname_dispatch();        /* #P */
	defun_structure_dispatch();       /* #S */
}


/*****************************************************************************
 *  build-readtable
 *****************************************************************************/
static void makeconstant(void)
{
	addr symbol, gensym, name;

	/* (setq readtable-dot (make-symbol "READTABLE-DOT")) */
	GetConst(SYSTEM_READTABLE_DOT, &symbol);
	symbol_heap(&gensym);
	GetNameSymbol(symbol, &name);
	SetNameSymbol(gensym, name);
	SetValueSymbol(symbol, gensym);
	SetStatusReadOnly(symbol);
}

_g void build_readtable(void)
{
	addr table, constant;

	/* function */
	make_macro_reader();
	make_macro_dispatch();
	/* readtable */
	makeconstant();
	readtable_heap(&table);
	GetConst(SPECIAL_READTABLE, &constant);
	SetValueSymbol(constant, table);
}

_g void in_package_lisp_package(void)
{
	addr package, symbol;
	Execute ptr;

	ptr = Execute_Thread;
	find_char_package(LISP_PACKAGE, &package);
	GetConst(SPECIAL_PACKAGE, &symbol);
	setspecial_local(ptr, symbol, package);
}


/*
 *  initialize
 */
_g void init_readtable(void)
{
	SetPointerCall(defun, var2, reader_double_quote);
	SetPointerCall(defun, var2, reader_single_quote);
	SetPointerCall(defun, var2, reader_parensis_open);
	SetPointerCall(defun, var2, reader_parensis_close);
	SetPointerCall(defun, var2, reader_semicolon);
	SetPointerCall(defun, var2, reader_backquote);
	SetPointerCall(defun, var2, reader_comma);
	SetPointerCall(defun, var2, reader_sharp);
	SetPointerCall(defun, var3, dispatch_error);
	SetPointerCall(defun, var3, dispatch_equal);
	SetPointerCall(defun, var3, dispatch_sharp);
	SetPointerCall(defun, var3, dispatch_single_quote);
	SetPointerCall(defun, var3, dispatch_parensis_open);
	SetPointerCall(defun, var3, dispatch_parensis_close);
	SetPointerCall(defun, var3, dispatch_asterisk);
	SetPointerCall(defun, var3, dispatch_colon);
	SetPointerCall(defun, var3, dispatch_less);
	SetPointerCall(defun, var3, dispatch_backslash);
	SetPointerCall(defun, var3, dispatch_or);
	SetPointerCall(defun, var3, dispatch_plus);
	SetPointerCall(defun, var3, dispatch_minus);
	SetPointerCall(defun, var3, dispatch_dot);
	SetPointerCall(defun, var3, dispatch_radix);
	SetPointerCall(defun, var3, dispatch_binary);
	SetPointerCall(defun, var3, dispatch_octal);
	SetPointerCall(defun, var3, dispatch_hexadecimal);
	SetPointerCall(defun, var3, dispatch_complex);
	SetPointerCall(defun, var3, dispatch_array);
	SetPointerCall(defun, var3, dispatch_pathname);
	SetPointerCall(defun, var3, dispatch_structure);
	SetPointerType(empty, read_replace_finalize);
	init_chartable();
}

