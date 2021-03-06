#include <string.h>
#include "character.h"
#include "cons.h"
#include "constant.h"
#include "hashtable.h"
#include "heap.h"
#include "reader_table.h"
#include "reader_type.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  access
 */
void *ptr_readtype(addr pos)
{
	CheckType(pos, LISPSYSTEM_READTYPE);
	return PtrReadType_Low(pos);
}

struct readtype_struct *struct_readtype(addr pos)
{
	CheckType(pos, LISPSYSTEM_READTYPE);
	return ReadTypeStruct_Low(pos);
}

void get_readtype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_READTYPE);
	GetReadType_Low(pos, ret);
}

void set_readtype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_READTYPE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadType_Low(pos, value);
}


/*
 *  readtype
 */
int dispatch_readtype(addr pos)
{
	CheckType(pos, LISPSYSTEM_READTYPE);
	return ReadTypeStruct(pos)->dispatch;
}

void make_readtype(addr *ret,
		enum ReadTable_Type type, unicode code, unsigned dispatch)
{
	addr pos;
	struct readtype_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_READTYPE, 1, sizeoft(struct readtype_struct));
	str = ReadTypeStruct(pos);
	str->type = type;
	str->code = code;
	str->dispatch = dispatch;
	*ret = pos;
}

void copy_readtype(addr *ret, addr copy)
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

void default_array_readtype(addr array)
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

static int dispatch_character_(addr pos, unicode a, unicode b, constindex index)
{
	addr key, value, cons;

	/* hash */
	Check(isLowerCase(b), "case error");
	character2_heap(&key, a, b);
	Return(intern_hashheap_(pos, key, &cons));
	/* value */
	GetConstant(index, &value);
	GetFunctionSymbol(value, &value);
	Check(value == Unbound, "unbound error");
	/* set */
	SetCdr(cons, value);

	return 0;
}

#define DispatchCharacter(a,u,b,c) \
	/**/ Return(dispatch_character_(a,u,b,CONSTANT_SYSTEM_##c##_DISPATCH))

int default_dispatch_readtype_(addr pos, unicode u)
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

	return 0;
}

void array_readtype_heap(addr *ret)
{
	vector2_heap(ret, 0x80);
}

void dispatch_readtype_heap(addr *ret)
{
	addr pos;
	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQUAL);
	*ret = pos;
}

void make_array_readtype(addr *ret)
{
	addr pos;
	array_readtype_heap(&pos);
	default_array_readtype(pos);
	*ret = pos;
}

void make_table_readtype(addr *ret)
{
	addr pos;
	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	*ret = pos;
}

int make_dispatch_readtype_(addr *ret)
{
	addr pos;
	dispatch_readtype_heap(&pos);
	Return(default_dispatch_readtype_(pos, '#'));
	return Result(ret, pos);
}

int readtype_whitespace(unicode u)
{
	if (0x80 <= u) return 0;
	return strchr(Default_WhiteSpace, (int)u) != NULL;
}

int readtype_constituent(unicode u)
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
int readtype_termmacro(unicode u, addr *ret)
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

int readtype_sharpmacro(unicode u, addr *ret)
{
	addr pos;

	if (u == '#') {
		GetConst(SYSTEM_SHARP_READER, &pos);
		GetFunctionSymbol(pos, ret);
		return 1;
	}

	return 0;
}

static int delete_dispatch_macro_(addr pos, unicode u)
{
	int check;
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
			if (refcharacter2a(car) == u) {
				Return(delete_hashtable_(pos, car, &check));
			}
		}
	}

	return 0;
}

int delete_readtype_(addr pos, unicode c)
{
	int check;
	addr value;

	Return(readtype_readtable_(pos, c, &value));
	if (value == Nil)
		return 0;
	if (dispatch_readtype(value)) {
		GetDispatchReadtable(pos, &value);
		Return(delete_dispatch_macro_(value, c));
	}
	/* delete */
	if (c < 0x80) {
		GetArrayReadtable(pos, &pos);
		SetArrayA2(pos, c, Nil);
	}
	else {
		GetTableReadtable(pos, &pos);
		Return(findcons_unicode_hashtable_(pos, c, &value));
		if (value != Nil) {
			GetCar(value, &value);
			Return(delete_hashtable_(pos, value, &check));
		}
	}

	return 0;
}

