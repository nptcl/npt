#include "fasl.c"
#include "common.h"
#include "constant.h"
#include "degrade.h"
#include "function.h"
#include "pathname.h"

#define TESTFILE "_debug.txt"

#if 0
static addr openwrite(void)
{
	addr name, stream;

	strvect_char_heap(&name, TESTFILE);
	open_output_binary_stream(Execute_Thread, &stream, name);

	return stream;
}

static addr openread(void)
{
	addr name, stream;

	strvect_char_heap(&name, TESTFILE);
	open_input_binary_stream(Execute_Thread, &stream, name);

	return stream;
}

static inline int writetest(addr stream, const void *ptr, size_t size)
{
	int result;
	size_t rsize;

	result = write_binary_stream(stream, ptr, size, &rsize);
	if (result) return 1;
	if (size != rsize) return 1;

	return 0;
}

#define writetype(stream, type, value) { \
	type __temp = (type)(value); \
	if (writetest(stream, (const void *)&__temp, sizeof(type))) goto error; \
}

static addr makefaslheader(void)
{
	int i;
	addr stream;

	stream = openwrite();
	if (writetest(stream, FaslHeader, FaslHeaderSize)) goto error;
	writetype(stream, byte16, 1);
	writetype(stream, byte16, LISP_VERSION_A);
	writetype(stream, byte16, LISP_VERSION_B);
	writetype(stream, byte16, LISP_VERSION_C);
#ifdef LISP_64BIT
	writetype(stream, byte16, 1);
#else
	writetype(stream, byte16, 0);
#endif
	for (i = 0; i < 14; i++) {
		if (write_byte_stream(stream, 0))
			goto error;
	}
	return stream;

error:
	close_stream(stream);
	return NULL;
}

static int test_headercheck(void)
{
	int result;
	addr stream;

	stream = makefaslheader();
	write_byte_stream(stream, 0xFF);
	close_stream(stream);

	stream = openread();
	result = headercheck(stream);
	close_stream(stream);
	test(result == 0, "headercheck1");

	RETURN;
}

static int test_fasloperator(void)
{
	addr stream, pos;

	stream = makefaslheader();
	write_byte_stream(stream, 0xFF);
	close_stream(stream);

	stream = openread();
	test(headercheck(stream) == 0, "fasloperator1");
	test(fasloperator(Execute_Thread, stream, &pos) != 0, "fasloperator2");
	close_stream(stream);

	stream = openwrite();
	close_stream(stream);
	stream = openread();
	test(fasloperator(Execute_Thread, stream, &pos), "fasloperator4");
	close_stream(stream);

	stream = openwrite();
	write_byte_stream(stream, 0xFE);
	close_stream(stream);
	stream = openread();
	test(fasloperator(Execute_Thread, stream, &pos) == 0, "fasloperator5");
	close_stream(stream);

	stream = openwrite();
	write_byte_stream(stream, 0xFC);
	close_stream(stream);
	stream = openread();
	test(fasloperator(Execute_Thread, stream, &pos) != 0, "fasloperator6");
	close_stream(stream);

	RETURN;
}

static int bintest(const void *ptr, size_t len, addr *ret)
{
	int result;
	addr stream, pos;
	size_t i;

	*ret = 0;
	pos = (pbyte)ptr;
	stream = openwrite();
	for (i = 0; i < len; i++) {
		if (write_byte_stream(stream, pos[i])) {
			close_stream(stream);
			return 1;
		}
	}
	close_stream(stream);
	stream = openread();
	result = fasloperator(Execute_Thread, stream, ret);
	close_stream(stream);

	return result;
}

static int test_nil(void)
{
	addr pos;

	test(bintest("\x00", 1, &pos) == 0, "nil1");
	test(pos == Nil, "nil2");

	RETURN;
}

static int test_t(void)
{
	addr pos;

	test(bintest("\x01", 1, &pos) == 0, "t1");
	test(pos == T, "t2");

	RETURN;
}

static int test_unbound(void)
{
	addr pos;

	test(bintest("\x02", 1, &pos) == 0, "unbound1");
	test(pos == Unbound, "unbound2");

	RETURN;
}

static int test_cons(void)
{
	addr pos, left, right;

	test(bintest("\x03\x01\x03\x00\x01", 5, &pos) == 0, "cons1");
	test(pos, "cons2");
	left = right = 0;
	GetCons(pos, &left, &right);
	test(left == T, "cons3");
	test(GetType(right) == LISPTYPE_CONS, "cons4");
	left = 0;
	GetCons(right, &left, &right);
	test(left == Nil, "cons5");
	test(right == T, "cons6");

	RETURN;
}

static int test_fixnum(void)
{
	byte mem[30];
	addr pos;
	fixnum value;

	memset(mem, 0xAA, 30);
	mem[0] = 0x05;
	value = 0x3412;
	memcpy(mem+1, &value, sizeof(fixnum));
	test(bintest(mem, 1+sizeof(fixnum), &pos) == 0, "fixnum1");
	test(pos, "fixnum2");
	value = 0;
	GetFixnum(pos, &value);
	test(value == 0x3412, "fixnum3");

	RETURN;
}

static int test_char(void)
{
	addr pos;
	unicode value;

	test(bintest("\x06\x65", 2, &pos) == 0, "char1");
	test(pos, "char2");
	value = 0;
	GetCharacter(pos, &value);
	test(value == 0x65, "char3");

	RETURN;
}

static int test_wchar(void)
{
	byte mem[30];
	addr pos;
	unicode value;

	memset(mem, 0xAA, 30);
	mem[0] = 0x07;
	value = 0x84332211;
	memcpy(mem+1, &value, sizeof(unicode));
	test(bintest(mem, 1+sizeof(unicode), &pos) == 0, "wchar1");
	test(pos, "wchar2");
	value = 0;
	GetCharacter(pos, &value);
	test(value == 0x84332211, "wchar3");

	RETURN;
}

/* list */
static int test_list1(void)
{
	addr pos, left;
	unicode value;

	test(bintest("\x10\x00", 1+1, &pos) == 0, "list1-1");
	test(pos == Nil, "list1-2");

	test(bintest("\x10\x03\x00\x01\x06\x30", 5+1, &pos) == 0, "list1-3");
	test(GetType(pos) == LISPTYPE_CONS, "list1-4");
	GetCons(pos, &left, &pos);
	test(left == Nil, "list1-5");
	GetCons(pos, &left, &pos);
	test(left == T, "list1-6");
	GetCons(pos, &left, &pos);
	value = 0;
	GetCharacter(left, &value);
	test(value == 0x30, "list1-7");
	test(pos == Nil, "list1-8");

	RETURN;
}

static int test_list2(void)
{
	byte mem[30];
	addr pos, left;
	byte16 size;
	unicode value;

	test(bintest("\x11\x00\x00", 1+sizeof(size), &pos) == 0, "list2-1");
	test(pos == Nil, "list2-2");

	memset(mem, 0xAA, 30);
	memcpy(mem, "\x11\xFF\xFF\x00\x01\x06\x30", 5+sizeof(size));
	size = 3;
	memcpy(mem+1, &size, sizeof(size));
	test(bintest(mem, 5+sizeof(size), &pos) == 0, "list2-3");
	test(GetType(pos) == LISPTYPE_CONS, "list2-4");
	GetCons(pos, &left, &pos);
	test(left == Nil, "list2-5");
	GetCons(pos, &left, &pos);
	test(left == T, "list2-6");
	GetCons(pos, &left, &pos);
	value = 0;
	GetCharacter(left, &value);
	test(value == 0x30, "list2-7");
	test(pos == Nil, "list2-8");

	RETURN;
}

static int test_list4(void)
{
	byte mem[30];
	addr pos, left;
	byte32 size;
	unicode value;

	test(bintest("\x12\x00\x00\x00\x00", 1+sizeof(size), &pos) == 0, "list4-1");
	test(pos == Nil, "list4-2");

	memset(mem, 0xAA, 30);
	memcpy(mem, "\x12\xFF\xFF\xFF\xFF\x00\x01\x06\x30", 5+sizeof(size));
	size = 3;
	memcpy(mem+1, &size, sizeof(size));
	test(bintest(mem, 5+sizeof(size), &pos) == 0, "list4-3");
	test(GetType(pos) == LISPTYPE_CONS, "list4-4");
	GetCons(pos, &left, &pos);
	test(left == Nil, "list4-5");
	GetCons(pos, &left, &pos);
	test(left == T, "list4-6");
	GetCons(pos, &left, &pos);
	value = 0;
	GetCharacter(left, &value);
	test(value == 0x30, "list4-7");
	test(pos == Nil, "list4-8");

	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_list8(void)
{
	byte mem[30];
	addr pos, left;
	byte64 size;
	unicode value;

	test(bintest("\x13\x00\x00\x00\x00\x00\x00\x00\x00",
				1+sizeof(size), &pos) == 0, "list8-1");
	test(pos == Nil, "list8-2");

	memset(mem, 0xAA, 30);
	memcpy(mem, "\x13\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\x00\x01\x06\x30",
			5+sizeof(size));
	size = 3;
	memcpy(mem+1, &size, sizeof(size));
	test(bintest(mem, 5+sizeof(size), &pos) == 0, "list8-3");
	test(GetType(pos) == LISPTYPE_CONS, "list8-4");
	GetCons(pos, &left, &pos);
	test(left == Nil, "list8-5");
	GetCons(pos, &left, &pos);
	test(left == T, "list8-6");
	GetCons(pos, &left, &pos);
	value = 0;
	GetCharacter(left, &value);
	test(value == 0x30, "list8-7");
	test(pos == Nil, "list8-8");

	RETURN;
}
#endif

/* lista */
static int test_lista1(void)
{
	addr pos, left;
	unicode value;

	test(bintest("\x14\x00", 1+1, &pos) != 0, "lista1-1");

	test(bintest("\x14\x03\x00\x01\x06\x30", 5+1, &pos) == 0, "lista1-2");
	test(GetType(pos) == LISPTYPE_CONS, "lista1-3");
	GetCons(pos, &left, &pos);
	test(left == Nil, "lista1-4");
	GetCons(pos, &left, &pos);
	test(left == T, "lista1-5");
	value = 0;
	GetCharacter(pos, &value);
	test(value == 0x30, "lista1-6");

	RETURN;
}

static int test_lista2(void)
{
	byte mem[30];
	addr pos, left;
	byte16 size;
	unicode value;

	test(bintest("\x15\x00\x00", 1+sizeof(size), &pos) != 0, "lista2-1");
	memset(mem, 0xAA, 30);
	memcpy(mem, "\x15\xFF\xFF\x00\x01\x06\x30", 5+sizeof(size));
	size = 3;
	memcpy(mem+1, &size, sizeof(size));
	test(bintest(mem, 5+sizeof(size), &pos) == 0, "lista2-2");
	test(GetType(pos) == LISPTYPE_CONS, "lista2-3");
	GetCons(pos, &left, &pos);
	test(left == Nil, "lista2-4");
	GetCons(pos, &left, &pos);
	test(left == T, "lista2-5");
	value = 0;
	GetCharacter(pos, &value);
	test(value == 0x30, "lista2-6");

	RETURN;
}

static int test_lista4(void)
{
	byte mem[30];
	addr pos, left;
	byte32 size;
	unicode value;

	test(bintest("\x16\x00\x00\x00\x00", 1+sizeof(size), &pos) != 0, "lista4-1");
	memset(mem, 0xAA, 30);
	memcpy(mem, "\x16\xFF\xFF\xFF\xFF\x00\x01\x06\x30", 5+sizeof(size));
	size = 3;
	memcpy(mem+1, &size, sizeof(size));
	test(bintest(mem, 5+sizeof(size), &pos) == 0, "lista4-2");
	test(GetType(pos) == LISPTYPE_CONS, "lista4-3");
	GetCons(pos, &left, &pos);
	test(left == Nil, "lista4-4");
	GetCons(pos, &left, &pos);
	test(left == T, "lista4-5");
	value = 0;
	GetCharacter(pos, &value);
	test(value == 0x30, "lista4-6");

	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_lista8(void)
{
	byte mem[30];
	addr pos, left;
	byte64 size;
	unicode value;

	test(bintest("\x17\x00\x00\x00\x00\x00\x00\x00\x00",
				1+sizeof(size), &pos) != 0, "lista8-1");
	memset(mem, 0xAA, 30);
	memcpy(mem, "\x17\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\x00\x01\x06\x30",
			5+sizeof(size));
	size = 3;
	memcpy(mem+1, &size, sizeof(size));
	test(bintest(mem, 5+sizeof(size), &pos) == 0, "lista8-2");
	test(GetType(pos) == LISPTYPE_CONS, "lista8-3");
	GetCons(pos, &left, &pos);
	test(left == Nil, "lista8-4");
	GetCons(pos, &left, &pos);
	test(left == T, "lista8-5");
	value = 0;
	GetCharacter(pos, &value);
	test(value == 0x30, "lista8-6");

	RETURN;
}
#endif

/* string */
#define makestring_macro() { \
	byte mem[30]; \
	size_t len; \
	memset(mem, 0xAA, 30); \
	mem[0] = type; \
	memcpy(mem+1, &size, sizeof(size)); \
	len = strlen(name); \
	memcpy(mem+1+sizeof(size), name, len+1); \
	return bintest(mem, 1+sizeof(size)+len, ret); \
}
static int makestring1(byte type, byte size, const char *name, addr *ret)
{
	makestring_macro();
}
static int makestring2(byte type, byte16 size, const char *name, addr *ret)
{
	makestring_macro();
}
static int makestring4(byte type, byte32 size, const char *name, addr *ret)
{
	makestring_macro();
}
#ifdef LISP_ARCH_64BIT
static int makestring8(byte type, byte64 size, const char *name, addr *ret)
{
	makestring_macro();
}
#endif

static int test_string1(void)
{
	addr pos;

	test(makestring1(0x18, 4, "abcde", &pos) == 0, "string1-1");
	test(GetType(pos) == LISPTYPE_STRING, "string1-2");
	test(string_compare_char(pos, "abcd") == 0, "string1-3");

	RETURN;
}

static int test_string2(void)
{
	addr pos;

	test(makestring2(0x19, 4, "abcde", &pos) == 0, "string2-1");
	test(GetType(pos) == LISPTYPE_STRING, "string2-2");
	test(string_compare_char(pos, "abcd") == 0, "string2-3");

	RETURN;
}

static int test_string4(void)
{
	addr pos;

	test(makestring4(0x1A, 4, "abcde", &pos) == 0, "string4-1");
	test(GetType(pos) == LISPTYPE_STRING, "string4-2");
	test(string_compare_char(pos, "abcd") == 0, "string4-3");

	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_string8(void)
{
	addr pos;

	test(makestring8(0x1B, 4, "abcde", &pos) == 0, "string8-1");
	test(GetType(pos) == LISPTYPE_STRING, "string8-2");
	test(string_compare_char(pos, "abcd") == 0, "string8-3");

	RETURN;
}
#endif

static void ascii_unicode(const char *ascii, void *dest, size_t size)
{
	pbyte pos;
	size_t i;
	unicode v;

	pos = (pbyte)dest;
	for (i = 0; i < size; i++) {
		v = (unicode)ascii[i];
		memcpy(pos + sizeof(v)*i, &v, sizeof(v));
	}
}

#define makeunicode_macro() { \
	byte mem[100]; \
	size_t len; \
	memset(mem, 0xAA, 100); \
	mem[0] = type; \
	memcpy(mem+1, &size, sizeof(size)); \
	len = strlen(name); \
	ascii_unicode(name, mem + 1 + sizeof(size), len); \
	return bintest(mem, 1+sizeof(size)+len*sizeof(unicode), ret); \
}
static int makeunicode1(byte type, byte size, const char *name, addr *ret)
{
	makeunicode_macro();
}
static int makeunicode2(byte type, byte16 size, const char *name, addr *ret)
{
	makeunicode_macro();
}
static int makeunicode4(byte type, byte32 size, const char *name, addr *ret)
{
	makeunicode_macro();
}
#ifdef LISP_ARCH_64BIT
static int makeunicode8(byte type, byte64 size, const char *name, addr *ret)
{
	makeunicode_macro();
}
#endif

static int test_wstring1(void)
{
	addr pos, comp;

	test(makeunicode1(0x1C, 4, "abcde", &pos) == 0, "wstring1-1");
	test(GetType(pos) == LISPTYPE_STRING, "wstring1-2");
	strvect_char_heap(&comp, "abcd");
	test(string_compare(pos, comp) == 0, "wstring1-3");

	RETURN;
}

static int test_wstring2(void)
{
	addr pos, comp;

	test(makeunicode2(0x1D, 4, "abcde", &pos) == 0, "wstring2-1");
	test(GetType(pos) == LISPTYPE_STRING, "wstring2-2");
	strvect_char_heap(&comp, "abcd");
	test(string_compare(pos, comp) == 0, "wstring2-3");

	RETURN;
}

static int test_wstring4(void)
{
	addr pos, comp;

	test(makeunicode4(0x1E, 4, "abcde", &pos) == 0, "wstring4-1");
	test(GetType(pos) == LISPTYPE_STRING, "wstring4-2");
	strvect_char_heap(&comp, "abcd");
	test(string_compare(pos, comp) == 0, "wstring4-3");

	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_wstring8(void)
{
	addr pos, comp;

	test(makeunicode8(0x1F, 4, "abcde", &pos) == 0, "wstring8-1");
	test(GetType(pos) == LISPTYPE_STRING, "wstring8-2");
	strvect_char_heap(&comp, "abcd");
	test(string_compare(pos, comp) == 0, "wstring8-3");

	RETURN;
}
#endif

/* call */
#define symboltest(package, name, arg) { \
	addr temp; \
	test(GetType(pos) == LISPTYPE_SYMBOL, arg "-2"); \
	GetPackageSymbol(pos, &temp); \
	getname_package(temp, &temp); \
	test(string_compare_char(temp, package) == 0, arg "-3"); \
	GetNameSymbol(pos, &temp); \
	test(string_compare_char(temp, name) == 0, arg "-4"); \
}

static int test_call1(void)
{
	addr pos;
	test(makestring1(0x20, 3, "NOPAA", &pos) == 0, "call1-1");
	symboltest(LISP_CODE, "NOP", "call1");
	GetFunctionSymbol(pos, &pos);
	test(pos != Unbound, "call1-a");
	test(system_function_p(pos), "call1-b");
	RETURN;
}

static int test_call2(void)
{
	addr pos;
	test(makestring2(0x21, 4, "ABCDE", &pos) == 0, "call2-1");
	symboltest(LISP_CODE, "ABCD", "call2");
	RETURN;
}

static int test_call4(void)
{
	addr pos;
	test(makestring4(0x22, 4, "ABCDE", &pos) == 0, "call4-1");
	symboltest(LISP_CODE, "ABCD", "call4");
	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_call8(void)
{
	addr pos;
	test(makestring8(0x23, 4, "ABCDE", &pos) == 0, "call8-1");
	symboltest(LISP_CODE, "ABCD", "call8");
	RETURN;
}
#endif

#define usymboltest(package, name, arg) { \
	addr temp, str; \
	test(GetType(pos) == LISPTYPE_SYMBOL, arg "-2"); \
	GetPackageSymbol(pos, &temp); \
	getname_package(temp, &temp); \
	test(string_compare_char(temp, package) == 0, arg "-3"); \
	GetNameSymbol(pos, &temp); \
	strvect_char_heap(&str, name); \
	test(string_compare(temp, str) == 0, arg "-4"); \
}

static int test_wcall1(void)
{
	addr pos;
	test(makeunicode1(0x24, 3, "NOPAA", &pos) == 0, "wcall1-1");
	usymboltest(LISP_CODE, "NOP", "wcall1");
	GetFunctionSymbol(pos, &pos);
	test(pos != Unbound, "wcall1-a");
	test(system_function_p(pos), "wcall1-b");
	RETURN;
}
static int test_wcall2(void)
{
	addr pos;
	test(makeunicode2(0x25, 4, "ABCDE", &pos) == 0, "wcall2-1");
	usymboltest(LISP_CODE, "ABCD", "wcall2");
	RETURN;
}
static int test_wcall4(void)
{
	addr pos;
	test(makeunicode4(0x26, 4, "ABCDE", &pos) == 0, "wcall4-1");
	usymboltest(LISP_CODE, "ABCD", "wcall4");
	RETURN;
}
#ifdef LISP_ARCH_64BIT
static int test_wcall8(void)
{
	addr pos;
	test(makeunicode8(0x27, 4, "ABCDE", &pos) == 0, "wcall8-1");
	usymboltest(LISP_CODE, "ABCD", "wcall8");
	RETURN;
}
#endif

/* keyword */
static int test_keyword1(void)
{
	addr pos;
	test(makestring1(0x28, 4, "ABCDE", &pos) == 0, "keyword1-1");
	symboltest("KEYWORD", "ABCD", "keyword1");
	RETURN;
}

static int test_keyword2(void)
{
	addr pos;
	test(makestring2(0x29, 4, "ABCDE", &pos) == 0, "keyword2-1");
	symboltest("KEYWORD", "ABCD", "keyword2");
	RETURN;
}

static int test_keyword4(void)
{
	addr pos;
	test(makestring4(0x2A, 4, "ABCDE", &pos) == 0, "keyword4-1");
	symboltest("KEYWORD", "ABCD", "keyword4");
	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_keyword8(void)
{
	addr pos;
	test(makestring8(0x2B, 4, "ABCDE", &pos) == 0, "keyword8-1");
	symboltest("KEYWORD", "ABCD", "keyword8");
	RETURN;
}
#endif

static int test_wkeyword1(void)
{
	addr pos;
	test(makeunicode1(0x2C, 4, "ABCDE", &pos) == 0, "wkeyword1-1");
	usymboltest("KEYWORD", "ABCD", "wkeyword1");
	RETURN;
}
static int test_wkeyword2(void)
{
	addr pos;
	test(makeunicode2(0x2D, 4, "ABCDE", &pos) == 0, "wkeyword2-1");
	usymboltest("KEYWORD", "ABCD", "wkeyword2");
	RETURN;
}
static int test_wkeyword4(void)
{
	addr pos;
	test(makeunicode4(0x2E, 4, "ABCDE", &pos) == 0, "wkeyword4-1");
	usymboltest("KEYWORD", "ABCD", "wkeyword4");
	RETURN;
}
#ifdef LISP_ARCH_64BIT
static int test_wkeyword8(void)
{
	addr pos;
	test(makeunicode8(0x2F, 4, "ABCDE", &pos) == 0, "wkeyword8-1");
	usymboltest("KEYWORD", "ABCD", "wkeyword8");
	RETURN;
}
#endif

/* gensym */
#if 0
static int test_gensym1(void)
{
	addr pos;
	test(makestring1(0x28, 4, "ABCDE", &pos) == 0, "gensym1-1");
	symboltest("KEYWORD", "ABCD", "gensym1");
	RETURN;
}
#endif

/* default */
static int test_default1(void)
{
	addr pos;
	test(makestring1(0x38, 4, "ABCDE", &pos) == 0, "default1-1");
	symboltest(LISPNAME, "ABCD", "default1");
	RETURN;
}

static int test_default2(void)
{
	addr pos;
	test(makestring2(0x39, 4, "ABCDE", &pos) == 0, "default2-1");
	symboltest(LISPNAME, "ABCD", "default2");
	RETURN;
}

static int test_default4(void)
{
	addr pos;
	test(makestring4(0x3A, 4, "ABCDE", &pos) == 0, "default4-1");
	symboltest(LISPNAME, "ABCD", "default4");
	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_default8(void)
{
	addr pos;
	test(makestring8(0x3B, 4, "ABCDE", &pos) == 0, "default8-1");
	symboltest(LISPNAME, "ABCD", "default8");
	RETURN;
}
#endif

static int test_wdefault1(void)
{
	addr pos;
	test(makeunicode1(0x3C, 4, "ABCDE", &pos) == 0, "wdefault1-1");
	usymboltest(LISPNAME, "ABCD", "wdefault1");
	RETURN;
}
static int test_wdefault2(void)
{
	addr pos;
	test(makeunicode2(0x3D, 4, "ABCDE", &pos) == 0, "wdefault2-1");
	usymboltest(LISPNAME, "ABCD", "wdefault2");
	RETURN;
}
static int test_wdefault4(void)
{
	addr pos;
	test(makeunicode4(0x3E, 4, "ABCDE", &pos) == 0, "wdefault4-1");
	usymboltest(LISPNAME, "ABCD", "wdefault4");
	RETURN;
}
#ifdef LISP_ARCH_64BIT
static int test_wdefault8(void)
{
	addr pos;
	test(makeunicode8(0x3F, 4, "ABCDE", &pos) == 0, "wdefault8-1");
	usymboltest(LISPNAME, "ABCD", "wdefault8");
	RETURN;
}
#endif

/* symbol */
#define makesymascii_macro(typesize) { \
	byte mem[100]; \
	typesize len; \
	size_t size; \
	/* type */ \
	mem[0] = type; \
	size = 1; \
	/* package */ \
	len = (typesize)strlen(pack); \
	memcpy(mem + size, &len, sizeof(len)); \
	size += sizeof(len); \
	memcpy(mem + size, pack, len); \
	size += len; \
	/* name */ \
	len = (typesize)strlen(name); \
	memcpy(mem + size, &len, sizeof(len)); \
	size += sizeof(len); \
	memcpy(mem + size, name, len); \
	size += len; \
	return bintest(mem, size, ret); \
}
static int makesymascii1(byte type, const char *pack, const char *name, addr *ret)
{
	makesymascii_macro(byte);
}
static int makesymascii2(byte type, const char *pack, const char *name, addr *ret)
{
	makesymascii_macro(byte16);
}
static int makesymascii4(byte type, const char *pack, const char *name, addr *ret)
{
	makesymascii_macro(byte32);
}
#ifdef LISP_ARCH_64BIT
static int makesymascii8(byte type, const char *pack, const char *name, addr *ret)
{
	makesymascii_macro(byte64);
}
#endif

static int test_symbol1(void)
{
	addr pos;
	test(makesymascii1(0x40, "COMMON-LISP", "ABCDE", &pos) == 0, "symbol1-1");
	symboltest("COMMON-LISP", "ABCDE", "symbol1");
	RETURN;
}

static int test_symbol2(void)
{
	addr pos;
	test(makesymascii2(0x41, "COMMON-LISP", "ABCDE", &pos) == 0, "symbol2-1");
	symboltest("COMMON-LISP", "ABCDE", "symbol2");
	RETURN;
}

static int test_symbol4(void)
{
	addr pos;
	test(makesymascii4(0x42, "COMMON-LISP", "ABCDE", &pos) == 0, "symbol4-1");
	symboltest("COMMON-LISP", "ABCDE", "symbol4");
	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_symbol8(void)
{
	addr pos;
	test(makesymascii8(0x43, "COMMON-LISP", "ABCDE", &pos) == 0, "symbol8-1");
	symboltest("COMMON-LISP", "ABCDE", "symbol8");
	RETURN;
}
#endif

#define makesymunicode_macro(typesize) { \
	byte mem[100]; \
	typesize len; \
	size_t size; \
	/* type */ \
	mem[0] = type; \
	size = 1; \
	/* package */ \
	len = (typesize)strlen(pack); \
	memcpy(mem + size, &len, sizeof(len)); \
	size += sizeof(len); \
	ascii_unicode(pack, mem + size, len); \
	size += len * sizeof(unicode); \
	/* name */ \
	len = (typesize)strlen(name); \
	memcpy(mem + size, &len, sizeof(len)); \
	size += sizeof(len); \
	ascii_unicode(name, mem + size, len); \
	size += len * sizeof(unicode); \
	return bintest(mem, size, ret);  \
}
static int makesymunicode1(byte type, const char *pack, const char *name, addr *ret)
{
	makesymunicode_macro(byte);
}
static int makesymunicode2(byte type, const char *pack, const char *name, addr *ret)
{
	makesymunicode_macro(byte16);
}
static int makesymunicode4(byte type, const char *pack, const char *name, addr *ret)
{
	makesymunicode_macro(byte32);
}
#ifdef LISP_ARCH_64BIT
static int makesymunicode8(byte type, const char *pack, const char *name, addr *ret)
{
	makesymunicode_macro(byte64);
}
#endif

static int test_wsymbol1(void)
{
	addr pos;
	test(makesymunicode1(0x44, "COMMON-LISP", "ABCDE", &pos) == 0, "wsymbol1-1");
	usymboltest("COMMON-LISP", "ABCDE", "wsymbol1");
	RETURN;
}

static int test_wsymbol2(void)
{
	addr pos;
	test(makesymunicode2(0x45, "COMMON-LISP", "ABCDE", &pos) == 0, "wsymbol2-1");
	usymboltest("COMMON-LISP", "ABCDE", "wsymbol2");
	RETURN;
}

static int test_wsymbol4(void)
{
	addr pos;
	test(makesymunicode4(0x46, "COMMON-LISP", "ABCDE", &pos) == 0, "wsymbol4-1");
	usymboltest("COMMON-LISP", "ABCDE", "wsymbol4");
	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_wsymbol8(void)
{
	addr pos;
	test(makesymunicode8(0x47, "COMMON-LISP", "ABCDE", &pos) == 0, "wsymbol8-1");
	usymboltest("COMMON-LISP", "ABCDE", "wsymbol8");
	RETURN;
}
#endif

/* float */
static int test_float32(void)
{
	byte mem[100];
	addr pos;
	single_float value;

	mem[0] = 0x5D;
	value = -12.34f;
	memcpy(mem+1, &value, sizeof(value));
	test(bintest(mem, 1+sizeof(value), &pos) == 0, "float32-1");
	test(GetType(pos) == LISPTYPE_SINGLE_FLOAT, "float32-2");
	value = 0;
	GetSingleFloat(pos, &value);
	test(value == -12.34f, "float32-3");

	RETURN;
}

static int test_float64(void)
{
	byte mem[100];
	addr pos;
	double_float value;

	mem[0] = 0x5E;
	value = -12.34;
	memcpy(mem+1, &value, sizeof(value));
	test(bintest(mem, 1+sizeof(value), &pos) == 0, "float64-1");
	test(GetType(pos) == LISPTYPE_DOUBLE_FLOAT, "float64-2");
	value = 0;
	GetDoubleFloat(pos, &value);
	test(value == -12.34, "float64-3");

	RETURN;
}

/* vector */
#define makevect_macro(type, append) { \
	byte mem[100]; \
	byte first = append; \
	type len; \
	size_t size; \
	memset(mem, 0xAA, 100); \
	len = 5; \
	mem[0] = code; \
	size = 1; \
	if (first) { \
		mem[size] = first; \
		size += sizeof(first); \
	} \
	memcpy(mem + size, &len, sizeof(len)); \
	size += sizeof(len); \
	memcpy(mem + size, "\x03\x00\x01\x18\x05" "Hello\x00\x01\x06\x65", 14); \
	size += 14; \
	return bintest(mem, size, ret); \
}
static int makevect1(byte code, addr *ret)
{
	makevect_macro(byte, 0);
}
static int makevect2(byte code, addr *ret)
{
	makevect_macro(byte16, 0);
}
static int makevect4(byte code, addr *ret)
{
	makevect_macro(byte32, 0);
}
#ifdef LISP_ARCH_64BIT
static int makevect8(byte code, addr *ret)
{
	makevect_macro(byte64, 0);
}
#endif

static int vecttest(addr pos, enum LISPTYPE type)
{
	addr left, right;
	unicode v;

	test(GetType(pos) == type, "vector-2");
	getarray(pos, 0, &right);
	test(GetType(right) == LISPTYPE_CONS, "vector-3");
	GetCons(right, &left, &right);
	test(left == Nil, "vector-4");
	test(right == T, "vector-5");
	getarray(pos, 1, &left);
	test(string_compare_char(left, "Hello") == 0, "vector-6");
	getarray(pos, 2, &left);
	test(left == Nil, "vector-7");
	getarray(pos, 3, &left);
	test(left == T, "vector-8");
	getarray(pos, 4, &left);
	GetCharacter(left, &v);
	test(v == 0x65, "vector-9");

	RETURN;
}

static int test_vector1(void)
{
	addr pos;
	test(makevect1(0x48, &pos) == 0, "vector1");
	if (vecttest(pos, LISPTYPE_VECTOR)) goto error;
	if (GetStatusSize(pos) != LISPSIZE_ARRAY2) goto error;
	RETURN;
}

static int test_vector2(void)
{
	addr pos;
	test(makevect2(0x49, &pos) == 0, "vector2");
	if (vecttest(pos, LISPTYPE_VECTOR)) goto error;
	if (GetStatusSize(pos) != LISPSIZE_ARRAY2) goto error;
	RETURN;
}

static int test_vector4(void)
{
	addr pos;
	test(makevect4(0x4A, &pos) == 0, "vector4");
	if (vecttest(pos, LISPTYPE_VECTOR)) goto error;
	if (GetStatusSize(pos) != LISPSIZE_ARRAY4) goto error;
	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_vector8(void)
{
	addr pos;
	test(makevect8(0x4B, &pos) == 0, "vector8");
	if (vecttest(pos, LISPTYPE_VECTOR)) goto error;
	if (GetStatusSize(pos) != LISPSIZE_ARRAY8) goto error;
	RETURN;
}
#endif

/* code */
static int checkcode(addr root)
{
	addr one, base;
	size_t size;
	calltype *call, callcheck;

	/* type */
	test(GetType(root) == LISPTYPE_CODE, "checkcode1");
	/* length */
	lenarray(root, &size);
	test(size == CODE_ARRAY_SIZE, "checkcode2");
	/* array */
	GetArrayA2(root, CODE_ARRAY_CODE, &one);
	size = 0;
	lenarray(one, &size);
	test(size == 3, "checkcode3");
	GetArrayA2(root, CODE_ARRAY_CALL, &one);
	PosBodyB4(one, (addr *)&call);
	/* NOP */
	internchar(LISP_CODE, "NOP", &base);
	GetFunctionSymbol(base, &base);
	GetCompiled(base, &callcheck);
	test(call[0] == callcheck, "checkcode4");
	/* INFO */
	internchar(LISP_CODE, "INFO", &base);
	GetFunctionSymbol(base, &base);
	GetCompiled(base, &callcheck);
	test(call[1] == callcheck, "checkcode5");
	/* END */
	internchar(LISP_CODE, "END", &base);
	GetFunctionSymbol(base, &base);
	GetCompiled(base, &callcheck);
	test(call[2] == callcheck, "checkcode6");
	/* args */
	GetArrayA2(root, CODE_ARRAY_ARGUMENT, &one);
	GetArrayA4(one, 0, &base);
	test(base == Nil, "checkcode7");
	GetArrayA4(one, 1, &base);
	test(GetType(base) == LISPTYPE_CONS, "checkcode8");
	GetArrayA4(one, 2, &base);
	test(base == Nil, "checkcode9");

	RETURN;
}

static int test_make_code_heap(void)
{
	addr array, root, one, base;

	vector4_heap(&array, 3);

	/* NOP */
	cons_heap(&root);
	internchar(LISP_CODE, "NOP", &one);
	SetCar(root, one);
	SetArrayA4(array, 0, root);

	/* INFO */
	cons_heap(&root);
	internchar(LISP_CODE, "INFO", &one);
	cons_heap(&base);
	SetCons(root, one, base);
	character_heap(&one, 0x65);
	SetCar(base, one);
	SetArrayA4(array, 1, root);

	/* END */
	cons_heap(&root);
	internchar(LISP_CODE, "END", &one);
	SetCar(root, one);
	SetArrayA4(array, 2, root);

	/* test */
	old_make_code(array, &root);
	return checkcode(root);
}

static int makecode(byte first, const void *ptr, size_t len, addr *ret)
{
	byte mem[1000];
	size_t size;

	memset(mem, 0xAA, 1000);
	/* code */
	mem[0] = first;
	size = 1;
	/* size */
	memcpy(mem + size, ptr, len);
	size += len;

	/* (NOP) */
	memcpy(mem + size, "\x10\x01\x20\x03" "NOP", 7);
	size += 7;
	/* (INFO character) */
	memcpy(mem + size, "\x10\x02\x20\x04" "INFO" "\x06\x65", 10);
	size += 10;
	/* (END) */
	memcpy(mem + size, "\x10\x01\x20\x03" "END", 7);
	size += 7;

	return bintest(mem, size, ret);
}

#define makecode_macro(first, type, name) { \
	type v = 0x03; \
	addr pos; \
	test(makecode((first), &v, sizeof(type), &pos) == 0, name); \
	if (checkcode(pos)) goto error; \
}

static int test_code1(void)
{
	makecode_macro(0x4C, byte, "code1");
	RETURN;
}

static int test_code2(void)
{
	makecode_macro(0x4D, byte16, "code2");
	RETURN;
}

static int test_code4(void)
{
	makecode_macro(0x4E, byte32, "code4");
	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_code8(void)
{
	makecode_macro(0x4F, byte64, "code8");
	RETURN;
}
#endif

/* vectortype */
static int makevecttype1(byte code, addr *ret)
{
	makevect_macro(byte, LISPTYPE_SYSTEM);
}
static int makevecttype2(byte code, addr *ret)
{
	makevect_macro(byte16, LISPTYPE_SYSTEM);
}
static int makevecttype4(byte code, addr *ret)
{
	makevect_macro(byte32, LISPTYPE_SYSTEM);
}
#ifdef LISP_ARCH_64BIT
static int makevecttype8(byte code, addr *ret)
{
	makevect_macro(byte64, LISPTYPE_SYSTEM);
}
#endif

static int test_vectortype1(void)
{
	addr pos;
	test(makevecttype1(0x50, &pos) == 0, "vectortype1");
	if (vecttest(pos, LISPTYPE_SYSTEM)) goto error;
	RETURN;
}

static int test_vectortype2(void)
{
	addr pos;
	test(makevecttype2(0x51, &pos) == 0, "vectortype2");
	if (vecttest(pos, LISPTYPE_SYSTEM)) goto error;
	RETURN;
}

static int test_vectortype4(void)
{
	addr pos;
	test(makevecttype4(0x52, &pos) == 0, "vectortype4");
	if (vecttest(pos, LISPTYPE_SYSTEM)) goto error;
	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_vectortype8(void)
{
	addr pos;
	test(makevecttype8(0x53, &pos) == 0, "vectortype8");
	if (vecttest(pos, LISPTYPE_SYSTEM)) goto error;
	RETURN;
}
#endif

/* nop, others */
static int test_error(void)
{
	addr pos;
	test(bintest("\xFC", 1, &pos) != 0, "error1");
	RETURN;
}

static int test_hello(void)
{
	addr pos;
	test(bintest("\xFD", 1, &pos) == 0, "hello1");
	test(pos == Nil, "hello2");
	RETURN;
}

static int test_nop(void)
{
	addr pos;
	test(bintest("\xFE", 1, &pos) == 0, "nop1");
	test(pos == Nil, "nop2");
	RETURN;
}
#endif


/*
 *  Main
 */
static int testbreak_faslcode(void)
{
#if 0
	TestBreak(test_headercheck);
	TestBreak(test_fasloperator);
	TestBreak(test_nil);
	TestBreak(test_t);
	TestBreak(test_unbound);
	TestBreak(test_cons);
	TestBreak(test_fixnum);
	TestBreak(test_char);
	TestBreak(test_wchar);
	TestBreak(test_list1);
	TestBreak(test_list2);
	TestBreak(test_list4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_list8);
#endif
	TestBreak(test_lista1);
	TestBreak(test_lista2);
	TestBreak(test_lista4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_lista8);
#endif
	TestBreak(test_string1);
	TestBreak(test_string2);
	TestBreak(test_string4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_string8);
#endif
	TestBreak(test_wstring1);
	TestBreak(test_wstring2);
	TestBreak(test_wstring4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_wstring8);
#endif
	TestBreak(test_call1);
	TestBreak(test_call2);
	TestBreak(test_call4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_call8);
#endif
	TestBreak(test_wcall1);
	TestBreak(test_wcall2);
	TestBreak(test_wcall4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_wcall8);
#endif
	TestBreak(test_keyword1);
	TestBreak(test_keyword2);
	TestBreak(test_keyword4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_keyword8);
#endif
	TestBreak(test_wkeyword1);
	TestBreak(test_wkeyword2);
	TestBreak(test_wkeyword4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_wkeyword8);
#endif
#if 0
	TestBreak(test_gensym1);
	TestBreak(test_gensym2);
	TestBreak(test_gensym4);
	TestBreak(test_gensym8);
	TestBreak(test_wgensym1);
	TestBreak(test_wgensym2);
	TestBreak(test_wgensym4);
	TestBreak(test_wgensym8);
#endif
	TestBreak(test_default1);
	TestBreak(test_default2);
	TestBreak(test_default4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_default8);
#endif
	TestBreak(test_wdefault1);
	TestBreak(test_wdefault2);
	TestBreak(test_wdefault4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_wdefault8);
#endif
	TestBreak(test_symbol1);
	TestBreak(test_symbol2);
	TestBreak(test_symbol4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_symbol8);
#endif
	TestBreak(test_wsymbol1);
	TestBreak(test_wsymbol2);
	TestBreak(test_wsymbol4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_wsymbol8);
#endif
	TestBreak(test_float32);
	TestBreak(test_float64);
	TestBreak(test_vector1);
	TestBreak(test_vector2);
	TestBreak(test_vector4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_vector8);
#endif
	TestBreak(test_make_code_heap);
	TestBreak(test_code1);
	TestBreak(test_code2);
	TestBreak(test_code4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_code8);
#endif
	TestBreak(test_vectortype1);
	TestBreak(test_vectortype2);
	TestBreak(test_vectortype4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_vectortype8);
#endif
	TestBreak(test_error);
	TestBreak(test_hello);
	TestBreak(test_nop);
#endif

	return 0;
}

int test_fasl(void)
{
	int result;
	lispcode code;
	Execute ptr;

	/* init */
	init_fasl();

	/* test */
	TITLE;
	alloclisp(0, 0);
	ptr = Execute_Thread;
	lisp_info_enable = 0;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_package();
		build_fasl();
		build_code();
		build_pathname();
		build_common();
		lisp_initialize = 1;
		result = testbreak_faslcode();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

