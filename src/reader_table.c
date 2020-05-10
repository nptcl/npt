#include "character.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "hashtable.h"
#include "heap.h"
#include "gc.h"
#include "package.h"
#include "reader.h"
#include "reader_dispatch.h"
#include "reader_function.h"
#include "reader_table.h"
#include "reader_type.h"
#include "symbol.h"

/*
 *  access
 */
_g void getarray_readtable(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_READTABLE);
	GetArrayReadtable_Low(pos, ret);
}

_g void setarray_readtable(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_READTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArrayReadtable_Low(pos, value);
}

_g void gettable_readtable(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_READTABLE);
	GetTableReadtable_Low(pos, ret);
}

_g void settable_readtable(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_READTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTableReadtable_Low(pos, value);
}

_g void getdispatch_readtable(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_READTABLE);
	GetDispatchReadtable_Low(pos, ret);
}

_g void setdispatch_readtable(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_READTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDispatchReadtable_Low(pos, value);
}

_g void *ptr_readtable(addr pos)
{
	CheckType(pos, LISPTYPE_READTABLE);
	return PtrReadtable_Low(pos);
}

_g enum ReadTable_Case *ptrcase_readtable(addr pos)
{
	CheckType(pos, LISPTYPE_READTABLE);
	return (enum ReadTable_Case *)PtrCaseReadtable_Low(pos);
}


/*
 *  object
 */
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
_g void readtype_readtable(addr pos, unicode c, addr *ret)
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
	push_new_control(ptr, &control);
	Return(funcall_control(ptr, call, stream, code, NULL));
	if (lengthvalues_control(ptr) == 0) {
		*result = 0;
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		*result = 1;
	}
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

_g int macro_character_execute(Execute ptr, int *result, addr *ret,
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
	Return(macro_character_call(ptr, result, ret, call, stream, code));
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

