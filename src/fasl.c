#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "bignum.h"
#include "build.h"
#include "character.h"
#include "code.h"
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "execute.h"
#include "fasl.h"
#include "file.h"
#include "heap.h"
#include "object.h"
#include "package.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"

typedef int (*faslcode)(Execute, addr, addr *);
static faslcode faslcall[0x0100];

static inline int faslread_check(addr stream, void *ptr, size_t size)
{
	int check;
	size_t result;

	check = readforce_binary_stream(stream, ptr, size, &result);
	if (check) return 1;
	if (size != result) return 1;

	return 0;
}

#define faslread_type(stream, value) faslread_check(stream, &(value), sizeoft(value))

static int faslread_magic(addr stream)
{
	char a[8], b[8];

	/* LISPNAME */
	faslread_check(stream, a, 8);
	memset(b, 0, 8);
	strncpy(b, LISPNAME, 8);
	if (memcpy(a, b, 8) != 0)
		return 1;

	/* FASL0000 */
	faslread_check(stream, a, 8);
	memcpy(b, "FASL\0\0\0\0", 8);
	if (memcpy(a, b, 8) != 0)
		return 1;

	/* OK */
	return 0;
}

static inline void faslwrite_buffer(addr stream, const void *ptr, size_t size)
{
	const byte *data;
	size_t i;

	data = (const byte *)ptr;
	for (i = 0; i < size; i++)
		write_byte_stream(stream, data[i]);
}

static void faslwrite_magic(addr stream)
{
	char a[8];

	memset(a, 0, 8);
	strncpy(a, LISPNAME, 8);
	faslwrite_buffer(stream, a, 8);
	faslwrite_buffer(stream, "FASL\0\0\0\0", 8);
}

static int faslread_header(addr input)
{
	byte buffer[64];
	uint16_t v, a, b, c;

	/* 0: magic number */
	if (faslread_magic(input)) return 1;
	/* 16: endian */
	if (faslread_type(input, v)) return 1;
	if (v != 1) {
		Debug("endian error.");
		return 1;
	}
	/* 18: version */
	if (faslread_type(input, a)) return 1;
	if (faslread_type(input, b)) return 1;
	if (faslread_type(input, c)) return 1;
	if (a != LISP_VERSION_A || b != LISP_VERSION_B || c != LISP_VERSION_C) return 1;
	/* 20: arch */
	if (faslread_type(input, v)) return 1;
#ifdef LISP_64BIT
	if (v != 1) {
		Debug("This fasl file is not 64bit arch.");
		return 1;
	}
#else
	if (v != 0) {
		Debug("This fasl file is not 32bit arch.");
		return 1;
	}
#endif
	/* 22: padding */
	if (faslread_check(input, buffer, 2)) return 1;
	/* 24: end */

	return 0;
}

static void faslwrite_byte16(addr stream, uint16_t v)
{
	faslwrite_buffer(stream, (const void *)&v, sizeoft(v));
}

_g void faslwrite_header(addr stream)
{
	char buffer[64];

	/* 0: magic number */
	faslwrite_magic(stream);
	/* 16: endian */
	faslwrite_byte16(stream, 1);
	/* 18: version */
	faslwrite_byte16(stream, LISP_VERSION_A);
	faslwrite_byte16(stream, LISP_VERSION_B);
	faslwrite_byte16(stream, LISP_VERSION_C);
	/* 20: arch */
#ifdef LISP_64BIT
	faslwrite_byte16(stream, 1);
#else
	faslwrite_byte16(stream, 0);
#endif
	/* 22: padding */
	memset(buffer, 0xFF, 64);
	faslwrite_buffer(stream, (const void *)buffer, 2);
	/* 24: end */
}

#define faslread_nodecl(value, input, msg) { \
	if (faslread_type(input, value)) { \
		Debug("fasl input error: %s" msg); \
		return 1; \
	} \
}
#define faslread_value(type, value, input, msg) type value; \
	faslread_nodecl(value, input, msg)

static int faslread_operator(Execute ptr, addr input, addr *ret)
{
	int check;
	byte c;
	faslcode call;

	check = read_byte_stream(input, &c);

	/* EOF check */
	if (check) {
		Debug("EOF error");
		return 1;
	}
	if (c == 0xFF) {
		Debug("eof operator error");
		return 1;
	}

	/* execute */
	call = faslcall[c];
	if (call == 0) {
		Debug("faslcall error");
		return 1;
	}
	if ((*call)(ptr, input, ret)) {
		Debug("faslcall execute error");
		return 1;
	}

	return 0;
}

static void faslread_result(Execute ptr)
{
	addr pos;

	pos = ptr->control;
	if (pos == Nil) {
		info("control is null.");
		return;
	}
	getvalues_list_control_heap(ptr, &pos);
	if (pos == Nil) {
		info("result: (values)");
		return;
	}
	info_noeol("result: ");
	infoprint(pos);
}

static int faslread_control(Execute ptr, addr input)
{
	int result;
	addr pos, control;

	/* push */
	push_close_control(ptr, &control);
	/* code */
	result = faslread_operator(ptr, input, &pos);
	if (result) {
		Debug("faslread_operator error.");
		exit_code(ptr, LISPCODE_ERROR);
	}
	Check(GetType(pos) != LISPTYPE_CODE, "type code error");
	runcode_control(ptr, pos);
	/* free */
	faslread_result(ptr);
	free_control(ptr, control);

	return 0;
}

static int faslread_execute(Execute ptr, addr input)
{
	byte c;
	int result;

	for (;;) {
		/* EOF check */
		result = read_byte_stream(input, &c);
		if (result) {
			Debug("stream error.");
			return 1;
		}
		if (c == 0xFF)
			break;

		/* do execute */
		result = unread_byte_stream(input, c);
		if (result) {
			Debug("unread_byte_stream error");
			return 1;
		}
		result = faslread_control(ptr, input);
		if (result) {
			Debug("faslread_control error.");
			return 1;
		}
	}

	return 0;
}

_g int faslread_stream(Execute ptr, addr input)
{
	int result;

	result = faslread_header(input);
	if (result) {
		Debug("fasl header error");
		return 1;
	}
	result = faslread_execute(ptr, input);
	if (result) {
		Debug("fasl error");
		return 1;
	}

	return 0;
}


/*
 *  faslcode
 */
/* ARGSUSED0 */
static int nil_fasl(Execute ptr, addr input, addr *ret)
{
	*ret = Nil;
	return 0;
}

/* ARGSUSED0 */
static int t_fasl(Execute ptr, addr input, addr *ret)
{
	*ret = T;
	return 0;
}

/* ARGSUSED0 */
static int unbound_fasl(Execute ptr, addr input, addr *ret)
{
	*ret = Unbound;
	return 0;
}

static int cons_make(Execute ptr,
		addr input, addr *ret, void (*make)(Execute, addr *))
{
	int result;
	addr car, cdr;

	/* car */
	result = faslread_operator(ptr, input, &car);
	if (result) {
		Debug("cons car error");
		return 1;
	}
	/* cdr */
	result = faslread_operator(ptr, input, &cdr);
	if (result) {
		Debug("cons cdr error");
		return 1;
	}

	/* object */
	make(ptr, ret);
	SetCons(*ret, car, cdr);

	return 0;
}

/* ARGSUSED0 */
static void consnil_heap_make(Execute ptr, addr *ret)
{
	consnil_heap(ret);
}

static int cons_fasl(Execute ptr, addr input, addr *ret)
{
	return cons_make(ptr, input, ret, consnil_heap_make);
}

static void consnil_local_make(Execute ptr, addr *ret)
{
	consnil_local(ptr->local, ret);
}

static int consnil_local_fasl(Execute ptr, addr input, addr *ret)
{
	return cons_make(ptr, input, ret, consnil_local_make);
}

/* ARGSUSED0 */
static int fixnum_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(fixnum, value, input, "fixnum");
	fixnum_heap(ret, value);
	return 0;
}

/* character */
/* ARGSUSED0 */
static int char_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "character");
	character_heap(ret, value);
	return 0;
}

/* ARGSUSED0 */
static int wchar_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(unicode, value, input, "character unicode");
	character_heap(ret, value);
	return 0;
}

/* list */
static int list_fasl(Execute ptr, addr input, addr *ret, size_t len)
{
	int result;
	addr car, cdr, next, root;
	size_t i;

	/* nil */
	if (len == 0) {
		*ret = LispRoot(NIL);
		return 0;
	}

	/* list */
	root = 0;
	cdr = 0;
	for (i = 0; i < len; i++) {
		result = faslread_operator(ptr, input, &car);
		if (result) {
			Debug("list car error");
			return 1;
		}

		consnil_heap(&next);
		SetCar(next, car);
		if (i == 0)
			root = next;
		else {
			/* NOSTRICT: cdr */
			SetCdr(cdr, next);
		}
		cdr = next;
	}
	*ret = root;

	return 0;
}

static int list1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "list1");
	return list_fasl(ptr, input, ret, (size_t)value);
}

static int list2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "list2");
	return list_fasl(ptr, input, ret, (size_t)value);
}

static int list4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "list4");
	return list_fasl(ptr, input, ret, (size_t)value);
}

static int list8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "list8");
	return list_fasl(ptr, input, ret, (size_t)value);
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* list* */
static int lista_fasl(Execute ptr, addr input, addr *ret, size_t len)
{
	int result;
	addr car, cdr, next, root;
	size_t i;

	/* error */
	if (len == 0) {
		Debug("fasl input error: lista size must be at least 1.");
		return 1;
	}

	/* list */
	cdr = root = NULL;
	for (i = 1; i <= len; i++) {
		result = faslread_operator(ptr, input, &car);
		if (result) {
			Debug("list car error");
			return 1;
		}

		/* last argument */
		if (i == len) {
			if (i == 1) {
				*ret = car;
				return 0;
			}
			/* NOSTRICT: cdr */
			SetCdr(cdr, car);
			break;
		}

		/* make object */
		consnil_heap(&next);
		SetCar(next, car);
		if (i == 1)
			root = next;
		else
			SetCdr(cdr, next);
		cdr = next;
	}
	*ret = root;

	return 0;
}

static int lista1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "lista1");
	return lista_fasl(ptr, input, ret, (size_t)value);
}

static int lista2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "lista2");
	return lista_fasl(ptr, input, ret, (size_t)value);
}

static int lista4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "lista4");
	return lista_fasl(ptr, input, ret, (size_t)value);
}

static int lista8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "lista8");
	return lista_fasl(ptr, input, ret, (size_t)value);
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* string */
static void string1_stream(addr *ret, addr stream, size_t size)
{
	addr pos;
	unicode *body;
	byte c;
	size_t i;

	strvect_heap(&pos, size);
	GetStringUnicode(pos, &body);
	for (i = 0; i < size; i++) {
		if (read_byte_stream(stream, &c))
			_fmte("stream error", NULL);
		strvect_setc(pos, i, (unicode)c);
	}
	*ret = pos;
}

/* ARGSUSED0 */
static int string1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "string1");
	string1_stream(ret, input, (size_t)value);
	return 0;
}

/* ARGSUSED0 */
static int string2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "string2");
	string1_stream(ret, input, (size_t)value);
	return 0;
}

/* ARGSUSED0 */
static int string4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "strint4");
	string1_stream(ret, input, (size_t)value);
	return 0;
}

/* ARGSUSED0 */
static int string8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "string8");
	string1_stream(ret, input, (size_t)value);
	return 0;
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* wstring */
static void stringu_stream(addr *ret, addr stream, size_t size)
{
	addr pos;
	unicode *body, c;
	size_t i, temp;

	strvect_heap(&pos, size);
	GetStringUnicode(pos, &body);
	for (i = 0; i < size; i++) {
		if (readforce_binary_stream(stream, &c, sizeoft(unicode), &temp))
			_fmte("stream error", NULL);
		string_setc(pos, i, c);
	}
	*ret = pos;
}

/* ARGSUSED0 */
static int wstring1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "wstring1");
	stringu_stream(ret, input, (size_t)value);
	return 0;
}

/* ARGSUSED0 */
static int wstring2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "wstring2");
	stringu_stream(ret, input, (size_t)value);
	return 0;
}

/* ARGSUSED0 */
static int wstring4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "wstring4");
	stringu_stream(ret, input, (size_t)value);
	return 0;
}

/* ARGSUSED0 */
static int wstring8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "wstring8");
	stringu_stream(ret, input, (size_t)value);
	return 0;
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* call */
/* ARGSUSED0 */
static void makenamesymbol(Execute ptr,
		constindex index, addr input, size_t size, addr *ret)
{
	addr name, package;

	string1_stream(&name, input, size);
	GetConstant(index, &package);
	intern_package(package, name, ret);
}

static int call1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "call1");
	makenamesymbol(ptr, CONSTANT_PACKAGE_CODE, input, (size_t)value, ret);
	return 0;
}

static int call2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "call2");
	makenamesymbol(ptr, CONSTANT_PACKAGE_CODE, input, (size_t)value, ret);
	return 0;
}

static int call4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "call4");
	makenamesymbol(ptr, CONSTANT_PACKAGE_CODE, input, (size_t)value, ret);
	return 0;
}

static int call8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "call8");
	makenamesymbol(ptr, CONSTANT_PACKAGE_CODE, input, (size_t)value, ret);
	return 0;
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* wcall */
/* ARGSUSED0 */
static void makenamesymbolu(Execute ptr,
		constindex index, addr input, size_t size, addr *ret)
{
	addr name, package;

	stringu_stream(&name, input, size);
	GetConstant(index, &package);
	intern_package(package, name, ret);
}

static int wcall1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "wcall1");
	makenamesymbolu(ptr, CONSTANT_PACKAGE_CODE, input, (size_t)value, ret);
	return 0;
}

static int wcall2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "wcall2");
	makenamesymbolu(ptr, CONSTANT_PACKAGE_CODE, input, (size_t)value, ret);
	return 0;
}

static int wcall4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "wcall4");
	makenamesymbolu(ptr, CONSTANT_PACKAGE_CODE, input, (size_t)value, ret);
	return 0;
}

static int wcall8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "wcall8");
	makenamesymbolu(ptr, CONSTANT_PACKAGE_CODE, input, (size_t)value, ret);
	return 0;
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* keyword */
static int keyword1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "keyword1");
	makenamesymbol(ptr, CONSTANT_PACKAGE_KEYWORD, input, (size_t)value, ret);
	return 0;
}

static int keyword2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "keyword2");
	makenamesymbol(ptr, CONSTANT_PACKAGE_KEYWORD, input, (size_t)value, ret);
	return 0;
}

static int keyword4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "keyword4");
	makenamesymbol(ptr, CONSTANT_PACKAGE_KEYWORD, input, (size_t)value, ret);
	return 0;
}

static int keyword8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "keyword8");
	makenamesymbol(ptr, CONSTANT_PACKAGE_KEYWORD, input, (size_t)value, ret);
	return 0;
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* wkeyword */
static int wkeyword1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "wkeyword1");
	makenamesymbolu(ptr, CONSTANT_PACKAGE_KEYWORD, input, (size_t)value, ret);
	return 0;
}

static int wkeyword2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "wkeyword2");
	makenamesymbolu(ptr, CONSTANT_PACKAGE_KEYWORD, input, (size_t)value, ret);
	return 0;
}

static int wkeyword4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "wkeyword4");
	makenamesymbolu(ptr, CONSTANT_PACKAGE_KEYWORD, input, (size_t)value, ret);
	return 0;
}

static int wkeyword8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "wkeyword8");
	makenamesymbolu(ptr, CONSTANT_PACKAGE_KEYWORD, input, (size_t)value, ret);
	return 0;
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* gensym */
static void makegensym(Execute ptr, addr input, size_t size, addr *ret)
{
	addr name;

	string1_stream(&name, input, size);
	symbol_heap(ret);
	SetNameSymbol(*ret, name);
}

static int gensym1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "gensym1");
	makegensym(ptr, input, (size_t)value, ret);
	return 0;
}

static int gensym2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "gensym2");
	makegensym(ptr, input, (size_t)value, ret);
	return 0;
}

static int gensym4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "gensym4");
	makegensym(ptr, input, (size_t)value, ret);
	return 0;
}

static int gensym8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "gensym8");
	makegensym(ptr, input, (size_t)value, ret);
	return 0;
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

static void makegensymu(Execute ptr, addr input, size_t size, addr *ret)
{
	addr name;

	stringu_stream(&name, input, size);
	symbol_heap(ret);
	SetNameSymbol(*ret, name);
}

static int wgensym1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "wgensym1");
	makegensymu(ptr, input, (size_t)value, ret);
	return 0;
}

static int wgensym2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "wgensym2");
	makegensymu(ptr, input, (size_t)value, ret);
	return 0;
}

static int wgensym4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "wgensym4");
	makegensymu(ptr, input, (size_t)value, ret);
	return 0;
}

static int wgensym8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "wgensym8");
	makegensymu(ptr, input, (size_t)value, ret);
	return 0;
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* default */
static void makedefaultsymbol(Execute ptr,
		addr input, size_t size, addr *ret)
{
	addr name;
	string1_stream(&name, input, size);
	intern_default_package(ptr, name, ret);
}

static int default1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "default1");
	makedefaultsymbol(ptr, input, (size_t)value, ret);
	return 0;
}

static int default2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "default2");
	makedefaultsymbol(ptr, input, (size_t)value, ret);
	return 0;
}

static int default4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "default4");
	makedefaultsymbol(ptr, input, (size_t)value, ret);
	return 0;
}

static int default8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "default8");
	makedefaultsymbol(ptr, input, (size_t)value, ret);
	return 0;
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* wdefault */
static void makedefaultsymbolu(Execute ptr,
		addr input, size_t size, addr *ret)
{
	addr name;
	stringu_stream(&name, input, size);
	intern_default_package(ptr, name, ret);
}

static int wdefault1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "wdefault1");
	makedefaultsymbolu(ptr, input, (size_t)value, ret);
	return 0;
}

static int wdefault2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "wdefault2");
	makedefaultsymbolu(ptr, input, (size_t)value, ret);
	return 0;
}

static int wdefault4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "wdefault4");
	makedefaultsymbolu(ptr, input, (size_t)value, ret);
	return 0;
}

static int wdefault8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "wdefault8");
	makedefaultsymbolu(ptr, input, (size_t)value, ret);
	return 0;
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* symbol */
static int symbolread_size(void (*readproc)(Execute, size_t *, addr ),
		void (*heapproc)(addr *, addr , size_t),
		Execute ptr,
		addr input,
		addr *ret)
{
	addr package, name;
	LocalRoot local;
	LocalStack stack;
	size_t value;

	/* push */
	local = ptr->local;
	push_local(local, &stack);
	/* package */
	readproc(ptr, &value, input);
	heapproc(&name, input, (size_t)value);
	find_package(name, &package);
	if (package == Nil) {
		Debug("find_package error");
		exit_code(ptr, LISPCODE_ERROR);
	}
	/* name */
	readproc(ptr, &value, input);
	heapproc(&name, input, (size_t)value);
	/* intern */
	intern_package(package, name, ret);
	/* free */
	rollback_local(local, stack);

	return 0;
}

static int symbolascii_size(void (*proc)(Execute, size_t *, addr ),
		Execute ptr,
		addr input,
		addr *ret)
{
	return symbolread_size(proc, string1_stream, ptr, input, ret);
}

static void symbolread_byte(Execute ptr, size_t *value, addr input)
{
	byte temp;

	if (faslread_type(input, temp)) {
		Debug("symbolread_byte error");
		exit_code(ptr, LISPCODE_ERROR);
	}
	*value = (size_t)temp;
}

static void symbolread_byte16(Execute ptr, size_t *value, addr input)
{
	byte16 temp;

	if (faslread_type(input, temp)) {
		Debug("symbolread_byte16 error");
		exit_code(ptr, LISPCODE_ERROR);
	}
	*value = (size_t)temp;
}

static void symbolread_byte32(Execute ptr, size_t *value, addr input)
{
	byte32 temp;

	if (faslread_type(input, temp)) {
		Debug("symbolread_byte32 error");
		exit_code(ptr, LISPCODE_ERROR);
	}
	*value = (size_t)temp;
}

#ifdef LISP_ARCH_64BIT
static void symbolread_byte64(Execute ptr, size_t *value, addr input)
{
	byte64 temp;

	if (faslread_type(input, temp)) {
		Debug("symbolread_byte64 error");
		exit_code(ptr, LISPCODE_ERROR);
	}
	*value = (size_t)temp;
}
#endif

static int symbol1_fasl(Execute ptr, addr input, addr *ret)
{
	return symbolascii_size(symbolread_byte, ptr, input, ret);
}

static int symbol2_fasl(Execute ptr, addr input, addr *ret)
{
	return symbolascii_size(symbolread_byte16, ptr, input, ret);
}

static int symbol4_fasl(Execute ptr, addr input, addr *ret)
{
	return symbolascii_size(symbolread_byte32, ptr, input, ret);
}

static int symbol8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	return symbolascii_size(symbolread_byte64, ptr, input, ret);
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* wsymbol */
static int symbolunicode_size(void (*proc)(Execute, size_t *, addr ),
		Execute ptr,
		addr input,
		addr *ret)
{
	return symbolread_size(proc, stringu_stream, ptr, input, ret);
}

static int wsymbol1_fasl(Execute ptr, addr input, addr *ret)
{
	return symbolunicode_size(symbolread_byte, ptr, input, ret);
}

static int wsymbol2_fasl(Execute ptr, addr input, addr *ret)
{
	return symbolunicode_size(symbolread_byte16, ptr, input, ret);
}

static int wsymbol4_fasl(Execute ptr, addr input, addr *ret)
{
	return symbolunicode_size(symbolread_byte32, ptr, input, ret);
}

static int wsymbol8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	return symbolunicode_size(symbolread_byte64, ptr, input, ret);
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* bignum */
/* ARGSUSED0 */
static int bignum_fasl(Execute ptr,
		addr input, addr *ret, int sign, size_t size)
{
	addr pos;
	fixed value;
	size_t i;

	bignum_heap(&pos, sign, size);
	for (i = 0; i < size; i++) {
		if (faslread_type(input, value)) {
			Debug("bignum_fasl error");
			return 1;
		}
		setfixed_bignum(pos, i, value);
	}
	*ret = pos;

	return 0;
}

static int bignum1_fasl(Execute ptr, addr input, addr *ret)
{
	byte sign, size;
	faslread_nodecl(sign, input, "bignum1 sign");
	faslread_nodecl(size, input, "bignum1 size");
	return bignum_fasl(ptr, input, ret, (int)sign, (size_t)size);
}

static int bignum2_fasl(Execute ptr, addr input, addr *ret)
{
	byte sign;
	byte16 size;
	faslread_nodecl(sign, input, "bignum2 sign");
	faslread_nodecl(size, input, "bignum2 size");
	return bignum_fasl(ptr, input, ret, (int)sign, (size_t)size);
}

static int bignum4_fasl(Execute ptr, addr input, addr *ret)
{
	byte sign;
	byte32 size;
	faslread_nodecl(sign, input, "bignum4 sign");
	faslread_nodecl(size, input, "bignum4 size");
	return bignum_fasl(ptr, input, ret, (int)sign, (size_t)size);
}

static int bignum8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	byte sign;
	byte64 size;
	faslread_nodecl(sign, input, "bignum8 sign");
	faslread_nodecl(size, input, "bignum8 size");
	return bignum_fasl(ptr, input, ret, (int)sign, (size_t)size);
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* float */
/* ARGSUSED0 */
static int float16_error(Execute ptr, addr input, addr *ret)
{
	Debug("float16 is not implemented.");
	return 1;
}

/* ARGSUSED0 */
static int float32_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(float, value, input, "float");
	single_float_heap(ret, value);
	return 0;
}

/* ARGSUSED0 */
static int float64_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(double, value, input, "double");
	double_float_heap(ret, value);
	return 0;
}

/* ARGSUSED0 */
static int float128_error(Execute ptr, addr input, addr *ret)
{
	Debug("float128 is not implemented.");
	return 1;
}

/* vector */
static int vector_make(Execute ptr,
		void (*makearray)(addr *, enum LISPTYPE, size_t),
		addr input, addr *ret, enum LISPTYPE type, size_t len)
{
	int result;
	size_t i;
	addr pos, child;

	makearray(&pos, type, len);
	for (i = 0; i < len; i++) {
		result = faslread_operator(ptr, input, &child);
		if (result) {
			Debug("vector child error");
			return 1;
		}
		setarray(pos, i, child);
	}
	*ret = pos;

	return 0;
}

static void makearray2(addr *pos, enum LISPTYPE type, size_t size)
{
	heap_array2(pos, type, (byte16)size);
}

static void makearray4(addr *pos, enum LISPTYPE type, size_t size)
{
	heap_array4(pos, type, (byte32)size);
}

#ifdef LISP_ARCH_64BIT
static void makearray8(addr *pos, enum LISPTYPE type, size_t size)
{
	heap_array8(pos, type, (size_t)size);
}
#endif

static int vector1_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte, value, input, "vector1");
	return vector_make(ptr, makearray2, input, ret, LISPTYPE_VECTOR, (size_t)value);
}

static int vector2_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte16, value, input, "vector2");
	return vector_make(ptr, makearray2, input, ret, LISPTYPE_VECTOR, (size_t)value);
}

static int vector4_fasl(Execute ptr, addr input, addr *ret)
{
	faslread_value(byte32, value, input, "vector4");
	return vector_make(ptr, makearray4, input, ret, LISPTYPE_VECTOR, (size_t)value);
}

static int vector8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	faslread_value(byte64, value, input, "vector8");
	return vector_make(ptr, makearray8, input, ret, LISPTYPE_VECTOR, value);
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

static void heap_array_code(addr *ret, enum LISPTYPE type, size_t len)
{
	heap_array4(ret, type, (byte32)len);
}

static int code_fasl(Execute ptr, addr input, addr *ret, size_t len)
{
	int result;
	addr code;

	result = vector_make(ptr, heap_array_code, input, &code, LISPTYPE_VECTOR, len);
	if (result) {
		Debug("vector_make error");
		return 1;
	}
	Abort("TODO: old_make_code(code, ret);");

	return result;
}

#define readcode(type, msg) { \
	type __value; \
	if (faslread_type(input, __value)) { \
		Debug("fasl input value error: " msg); \
		return 1; \
	} \
	return code_fasl(ptr, input, ret, (size_t)__value); \
}

static int code1_fasl(Execute ptr, addr input, addr *ret)
{
	readcode(byte, "code1");
}

static int code2_fasl(Execute ptr, addr input, addr *ret)
{
	readcode(byte16, "code2");
}

static int code4_fasl(Execute ptr, addr input, addr *ret)
{
	readcode(byte32, "code4");
}

static int code8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	readcode(byte64, "code8");
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* vectortype */
static int vectortype1_fasl(Execute ptr, addr input, addr *ret)
{
	byte type, value;
	faslread_nodecl(type, input, "vectortype1 type");
	faslread_nodecl(value, input, "vectortype1 length");
	return vector_make(ptr,
			makearray2, input, ret, (enum LISPTYPE)type, (size_t)value);
}

static int vectortype2_fasl(Execute ptr, addr input, addr *ret)
{
	byte type;
	byte16 value;
	faslread_nodecl(type, input, "vectortype2 type");
	faslread_nodecl(value, input, "vectortype2 length");
	return vector_make(ptr,
			makearray2, input, ret, (enum LISPTYPE)type, (size_t)value);
}

static int vectortype4_fasl(Execute ptr, addr input, addr *ret)
{
	byte type;
	byte32 value;
	faslread_nodecl(type, input, "vectortype4 type");
	faslread_nodecl(value, input, "vectortype4 length");
	return vector_make(ptr,
			makearray4, input, ret, (enum LISPTYPE)type, (size_t)value);
}

static int vectortype8_fasl(Execute ptr, addr input, addr *ret)
{
#ifdef LISP_ARCH_64BIT
	byte type;
	byte64 value;
	faslread_nodecl(type, input, "vectortype8 type");
	faslread_nodecl(value, input, "vectortype8 length");
	return vector_make(ptr,
			makearray8, input, ret, (enum LISPTYPE)type, (size_t)value);
#else
	Debug("64bit value can not read in 32bit implementation.");
	return 1;
#endif
}

/* nop (no operator) */
/* ARGSUSED0 */
static int hello_fasl(Execute ptr, addr input, addr *ret)
{
	Info("Hello fasl.");
	*ret = Nil;
	return 0;
}

/* ARGSUSED0 */
static int nop_fasl(Execute ptr, addr input, addr *ret)
{
	*ret = Nil;
	return 0;
}


/*
 *  initialize
 */
_g void init_fasl(void)
{
	memset(faslcall, 0, sizeoft(faslcall));
	faslcall[0x00] = nil_fasl;
	faslcall[0x01] = t_fasl;
	faslcall[0x02] = unbound_fasl;
	faslcall[0x03] = cons_fasl;
	faslcall[0x04] = consnil_local_fasl;
	faslcall[0x05] = fixnum_fasl;
	faslcall[0x06] = char_fasl;
	faslcall[0x07] = wchar_fasl;
	faslcall[0x10] = list1_fasl;
	faslcall[0x11] = list2_fasl;
	faslcall[0x12] = list4_fasl;
	faslcall[0x13] = list8_fasl;
	faslcall[0x14] = lista1_fasl;
	faslcall[0x15] = lista2_fasl;
	faslcall[0x16] = lista4_fasl;
	faslcall[0x17] = lista8_fasl;
	faslcall[0x18] = string1_fasl;
	faslcall[0x19] = string2_fasl;
	faslcall[0x1A] = string4_fasl;
	faslcall[0x1B] = string8_fasl;
	faslcall[0x1C] = wstring1_fasl;
	faslcall[0x1D] = wstring2_fasl;
	faslcall[0x1E] = wstring4_fasl;
	faslcall[0x1F] = wstring8_fasl;
	faslcall[0x20] = call1_fasl;
	faslcall[0x21] = call2_fasl;
	faslcall[0x22] = call4_fasl;
	faslcall[0x23] = call8_fasl;
	faslcall[0x24] = wcall1_fasl;
	faslcall[0x25] = wcall2_fasl;
	faslcall[0x26] = wcall4_fasl;
	faslcall[0x27] = wcall8_fasl;
	faslcall[0x28] = keyword1_fasl;
	faslcall[0x29] = keyword2_fasl;
	faslcall[0x2A] = keyword4_fasl;
	faslcall[0x2B] = keyword8_fasl;
	faslcall[0x2C] = wkeyword1_fasl;
	faslcall[0x2D] = wkeyword2_fasl;
	faslcall[0x2E] = wkeyword4_fasl;
	faslcall[0x2F] = wkeyword8_fasl;
	faslcall[0x30] = gensym1_fasl;
	faslcall[0x31] = gensym2_fasl;
	faslcall[0x32] = gensym4_fasl;
	faslcall[0x33] = gensym8_fasl;
	faslcall[0x34] = wgensym1_fasl;
	faslcall[0x35] = wgensym2_fasl;
	faslcall[0x36] = wgensym4_fasl;
	faslcall[0x37] = wgensym8_fasl;
	faslcall[0x38] = default1_fasl;
	faslcall[0x39] = default2_fasl;
	faslcall[0x3A] = default4_fasl;
	faslcall[0x3B] = default8_fasl;
	faslcall[0x3C] = wdefault1_fasl;
	faslcall[0x3D] = wdefault2_fasl;
	faslcall[0x3E] = wdefault4_fasl;
	faslcall[0x3F] = wdefault8_fasl;
	faslcall[0x40] = symbol1_fasl;
	faslcall[0x41] = symbol2_fasl;
	faslcall[0x42] = symbol4_fasl;
	faslcall[0x43] = symbol8_fasl;
	faslcall[0x44] = wsymbol1_fasl;
	faslcall[0x45] = wsymbol2_fasl;
	faslcall[0x46] = wsymbol4_fasl;
	faslcall[0x47] = wsymbol8_fasl;
	faslcall[0x48] = vector1_fasl;
	faslcall[0x49] = vector2_fasl;
	faslcall[0x4A] = vector4_fasl;
	faslcall[0x4B] = vector8_fasl;
	faslcall[0x4C] = code1_fasl;
	faslcall[0x4D] = code2_fasl;
	faslcall[0x4E] = code4_fasl;
	faslcall[0x4F] = code8_fasl;
	faslcall[0x50] = vectortype1_fasl;
	faslcall[0x51] = vectortype2_fasl;
	faslcall[0x52] = vectortype4_fasl;
	faslcall[0x53] = vectortype8_fasl;
	faslcall[0x58] = bignum1_fasl;
	faslcall[0x59] = bignum2_fasl;
	faslcall[0x5A] = bignum4_fasl;
	faslcall[0x5B] = bignum8_fasl;
	faslcall[0x5C] = float16_error;
	faslcall[0x5D] = float32_fasl;
	faslcall[0x5E] = float64_fasl;
	faslcall[0x5F] = float128_error;
	faslcall[0xFD] = hello_fasl;
	faslcall[0xFE] = nop_fasl;
	faslcall[0xFF] = nop_fasl;
}

