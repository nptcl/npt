#include "character.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "hashtable.h"
#include "heap.h"
#include "hold.h"
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
void getarray_readtable(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_READTABLE);
	GetArrayReadtable_Low(pos, ret);
}

void setarray_readtable(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_READTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArrayReadtable_Low(pos, value);
}

void gettable_readtable(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_READTABLE);
	GetTableReadtable_Low(pos, ret);
}

void settable_readtable(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_READTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTableReadtable_Low(pos, value);
}

void getdispatch_readtable(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_READTABLE);
	GetDispatchReadtable_Low(pos, ret);
}

void setdispatch_readtable(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_READTABLE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDispatchReadtable_Low(pos, value);
}

void *ptr_readtable(addr pos)
{
	CheckType(pos, LISPTYPE_READTABLE);
	return PtrReadtable_Low(pos);
}

enum ReadTable_Case *ptrcase_readtable(addr pos)
{
	CheckType(pos, LISPTYPE_READTABLE);
	return (enum ReadTable_Case *)PtrCaseReadtable_Low(pos);
}


/*
 *  object
 */
int readtable_heap_(addr *ret)
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
	Return(make_dispatch_readtype_(&one));
	SetDispatchReadtable(pos, one);
	/* result */
	return Result(ret, pos);
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

static int copy_table_readtable_(addr from, addr to)
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
			Return(intern_hashheap_(to, car, &cell));
			SetCdr(cell, cdr);
		}
	}

	return 0;
}

static int copy_dispatch_readtable_(addr from, addr to)
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
			Return(intern_hashheap_(to, car, &cell));
			SetCdr(cell, cdr);
		}
	}

	return 0;
}

int copy_readtable_(addr from, addr to)
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
	Return(copy_table_readtable_(a, b));
	/* dispatch */
	GetDispatchReadtable(from, &a);
	GetDispatchReadtable(to, &b);
	clear_hashtable_heap(b);
	return copy_dispatch_readtable_(a, b);
}

int copy_readtable_heap_(addr from, addr *ret)
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
	Return(copy_table_readtable_(a, b));
	SetTableReadtable(to, b);
	/* dispatch */
	GetDispatchReadtable(from, &a);
	dispatch_readtype_heap(&b);
	Return(copy_dispatch_readtable_(a, b));
	SetDispatchReadtable(to, b);
	/* result */
	return Result(ret, to);
}

int copy_default_readtable_(addr pos)
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
	return default_dispatch_readtype_(one, '#');
}

static int setreadtype_readtable_(addr pos, unicode code, addr type)
{
	addr key;

	if (code < 0x80) {
		GetArrayReadtable(pos, &pos);
		SetArrayA2(pos, code, type);
	}
	else {
		GetTableReadtable(pos, &pos);
		character_heap(&key, code);
		Return(intern_hashheap_(pos, key, &pos));
		SetCdr(pos, type);
	}

	return 0;
}

int make_dispatch_macro_character_(addr pos, addr character, int nonterm)
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
	return setreadtype_readtable_(pos, code, type);
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

int get_default_dispatch_macro_(addr code1, addr code2, addr *ret)
{
	unicode u;

	GetCharacter(code1, &u);
	if (u != '#')
		return fmte_("The character ~S is not dispatch macro.", code1, NULL);
	get_default_dispatch_sharp(code2, ret);

	return 0;
}


/*
 *  macro_character_execute
 */
static int readtype_unicode_readtable_(addr hash, unicode c, addr *ret)
{
	addr pos, value;

	/* find hashtable */
	Return(find_unicode_hashtable_(hash, c, &value));
	if (value != Unbound)
		return Result(ret, value);

	/* new object */
	character_heap(&pos, c);
	make_readtype(&value, ReadTable_Type_constituent, c, 0);
	Return(intern_hashheap_(hash, pos, &pos));
	SetCdr(pos, value);

	return Result(ret, value);
}

int readtype_readtable_(addr pos, unicode c, addr *ret)
{
	if (c < 0x80) {
		GetArrayReadtable(pos, &pos);
		GetArrayA2(pos, c, ret);
	}
	else {
		GetTableReadtable(pos, &pos);
		Return(readtype_unicode_readtable_(pos, c, ret));
	}

	return 0;
}

static int macro_character_call_call_(Execute ptr, LocalHold hold,
		int *result, addr *ret, addr call, addr stream, addr code)
{
	Return(funcall_control(ptr, call, stream, code, NULL));
	if (lengthvalues_control(ptr) == 0) {
		*result = 0;
	}
	else {
		getresult_control(ptr, ret);
		localhold_set(hold, 0, *ret);
		*result = 1;
	}

	return 0;
}

static int macro_character_call(Execute ptr, int *result, addr *ret,
		addr call, addr stream, addr code)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_control(ptr, &control);
	(void)macro_character_call_call_(ptr, hold, result, ret, call, stream, code);
	Return(pop_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

int macro_character_execute(Execute ptr, int *result, addr *ret,
		unicode c, addr stream, addr table)
{
	addr call, code;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	character_heap(&code, c);
	localhold_push(hold, code);

	Return(readtype_readtable_(table, c, &call));
	localhold_push(hold, call);

	if (call == Nil)
		goto error;
	GetReadType(call, &call);
	localhold_push(hold, call);
	if (call == Nil)
		goto error;
	*result = 0;

	Return(macro_character_call(ptr, result, ret, call, stream, code));
	localhold_end(hold);
	return 0;

error:
	*result = 0;
	return fmte_("Character ~S don't have a macro code.", code, NULL);
}

int get_dispatch_macro_character_(addr pos, unicode u1, unicode u2, addr *ret)
{
	addr check;

	CheckType(pos, LISPTYPE_READTABLE);
	/* dispatch check */
	Return(readtype_readtable_(pos, u1, &check));
	if (check == Nil)
		goto error;
	if (! dispatch_readtype(check))
		goto error;
	/* find */
	GetDispatchReadtable(pos, &check);
	u2 = toUpperUnicode(u2);
	return findnil_character2_hashtable_(check, u1, u2, ret);

error:
	character_heap(&check, u1);
	return fmte_("The character ~S is not dispatch macro.", check, NULL);
}

int rem_dispatch_macro_character_(addr pos, unicode u1, unicode u2)
{
	int check;
	addr value, key;

	CheckType(pos, LISPTYPE_READTABLE);
	/* dispatch check */
	Return(readtype_readtable_(pos, u1, &value));
	if (value == Nil)
		goto error;
	if (! dispatch_readtype(value))
		goto error;
	/* delete */
	GetDispatchReadtable(pos, &value);
	u2 = toUpperUnicode(u2);
	Return(findcons_character2_hashtable_(value, u1, u2, &key));
	if (key != Nil) {
		GetCar(key, &key);
		Return(delete_hashtable_(value, key, &check));
	}
	return 0;

error:
	character_heap(&value, u1);
	return fmte_("The character ~S is not dispatch macro.", value, NULL);
}

int set_dispatch_macro_character_(addr pos, unicode u1, unicode u2, addr call)
{
	addr check, cons;

	CheckType(pos, LISPTYPE_READTABLE);
	/* dispatch check */
	Return(readtype_readtable_(pos, u1, &check));
	if (check == Nil)
		goto error;
	if (! dispatch_readtype(check))
		goto error;
	/* intern */
	GetDispatchReadtable(pos, &check);
	u2 = toUpperUnicode(u2);
	Return(findcons_character2_hashtable_(check, u1, u2, &cons));
	if (cons != Nil) {
		SetCdr(cons, call);
	}
	else {
		character2_heap(&cons, u1, u2);
		Return(intern_hashheap_(check, cons, &cons));
		SetCdr(cons, call);
	}
	return 0;

error:
	character_heap(&check, u1);
	return fmte_("The character ~S is not dispatch macro.", check, NULL);
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
void get_default_macro_character(unicode u, addr *ret, int *nonterm)
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

int get_macro_character_(addr pos, unicode u, addr *ret, int *nonterm)
{
	addr type;
	struct readtype_struct *str;

	CheckType(pos, LISPTYPE_READTABLE);
	Return(readtype_readtable_(pos, u, &type));
	if (type == Nil) {
		*ret = Nil;
		*nonterm = 0;
		return 0;
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

	return 0;
}

int set_macro_character_(addr pos, unicode u, int nonterm, addr call)
{
	addr type;
	enum ReadTable_Type value;

	CheckType(pos, LISPTYPE_READTABLE);
	value = nonterm? ReadTable_Type_macro_nonterm: ReadTable_Type_macro_term;
	make_readtype(&type, value, u, 0);
	SetReadType(type, call);
	/* add readtable */
	return setreadtype_readtable_(pos, u, type);
}

static int setreadtype_default_(addr pos,
		unicode u, enum ReadTable_Type type, addr call)
{
	addr one;

	make_readtype(&one, type, u, 0);
	SetReadType(one, call);
	return setreadtype_readtable_(pos, u, one);
}

static int setdispatch_default_(addr pos, unicode u, addr call)
{
	addr one;

	make_readtype(&one, ReadTable_Type_macro_nonterm, u, 1);
	SetReadType(one, call);
	Return(setreadtype_readtable_(pos, u, one));

	GetDispatchReadtable(pos, &one);
	return default_dispatch_readtype_(one, u);
}

int set_syntax_from_default_(unicode u1, unicode u2, addr to)
{
	addr pos;

	Return(delete_readtype_(to, u1));
	if (readtype_whitespace(u2))
		return setreadtype_default_(to, u1, ReadTable_Type_whitespace, Nil);
	if (readtype_constituent(u2))
		return setreadtype_default_(to, u1, ReadTable_Type_constituent, Nil);
	if (u2 == '\\')
		return setreadtype_default_(to, u1, ReadTable_Type_escape_single, Nil);
	if (u2 == '|')
		return setreadtype_default_(to, u1, ReadTable_Type_escape_multiple, Nil);
	if (readtype_termmacro(u2, &pos))
		return setreadtype_default_(to, u1, ReadTable_Type_macro_term, pos);
	if (readtype_sharpmacro(u2, &pos))
		return setdispatch_default_(to, u1, pos);

	/* delete only */
	return 0;
}

static int copy_dispatch_macro_(unicode u1, unicode u2, addr to, addr from)
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
				Return(intern_hashheap_(to, car, &car));
				SetCdr(car, cdr);
			}
		}
	}

	return 0;
}

int set_syntax_from_char_(unicode u1, unicode u2, addr to, addr from)
{
	addr one, type, call;
	struct readtype_struct *str;

	Return(delete_readtype_(to, u1));
	Return(readtype_readtable_(from, u2, &type));
	if (type != Nil) {
		str = ReadTypeStruct(type);
		GetReadType(type, &call);
		/* set-readtype */
		make_readtype(&one, str->type, u1, str->dispatch);
		Return(setreadtype_readtable_(to, u1, one));
		SetReadType(one, call);
		/* set-dispatch */
		if (str->dispatch) {
			GetDispatchReadtable(to, &to);
			GetDispatchReadtable(from, &from);
			Return(copy_dispatch_macro_(u1, u2, to, from));
		}
	}

	return 0;
}

int float_readtable_(Execute ptr, enum ReadTable_float *ret)
{
	addr pos, check;

	GetConst(SPECIAL_READ_DEFAULT_FLOAT_FORMAT, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	GetConst(COMMON_SINGLE_FLOAT, &check);
	if (check == pos)
		return Result(ret, ReadTable_single);
	GetConst(COMMON_DOUBLE_FLOAT, &check);
	if (check == pos)
		return Result(ret, ReadTable_double);
	GetConst(COMMON_LONG_FLOAT, &check);
	if (check == pos)
		return Result(ret, ReadTable_long);
	GetConst(COMMON_SHORT_FLOAT, &check);
	if (check == pos)
		return Result(ret, ReadTable_short);

	*ret = ReadTable_single;
	return fmte_("Invalid *read-default-float-format* value ~S.", pos, NULL);
}

int readcase_readtable_(Execute ptr, enum ReadTable_Case *ret)
{
	addr pos;
	Return(getreadtable_(ptr, &pos));
	return Result(ret, getcase_readtable(pos));
}

enum ReadTable_Case getcase_readtable(addr pos)
{
	Check(GetType(pos) != LISPTYPE_READTABLE, "type error");
	return *PtrCaseReadtable(pos);
}

void setcase_readtable(addr pos, enum ReadTable_Case mode)
{
	Check(GetType(pos) != LISPTYPE_READTABLE, "type error");
	*PtrCaseReadtable(pos) = mode;
}

int getreadtable_(Execute ptr, addr *ret)
{
	addr pos;

	GetConst(SPECIAL_READTABLE, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	if (GetType(pos) != LISPTYPE_READTABLE) {
		*ret = Nil;
		return TypeError_(pos, READTABLE);
	}

	return Result(ret, pos);
}

