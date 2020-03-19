#include "character_check.c"
#include "constant.h"
#include "degrade.h"

static int test_isbasechar(void)
{
	test(! isbasechar(1), "isbasechar1");
	test(  isbasechar(2), "isbasechar2");
	test(  isbasechar(8), "isbasechar3");
	test(  isbasechar(16), "isbasechar4");
	test(  isbasechar(36), "isbasechar5");
	test(! isbasechar(37), "isbasechar6");

	RETURN;
}

static int test_isuppercase(void)
{
	test(  isuppercase('A'), "isuppercase1");
	test(  isuppercase('C'), "isuppercase2");
	test(  isuppercase('Z'), "isuppercase3");
	test(! isuppercase('a'), "isuppercase4");
	test(! isuppercase('c'), "isuppercase5");
	test(! isuppercase('z'), "isuppercase6");
	test(! isuppercase(' '), "isuppercase7");
	test(! isuppercase('*'), "isuppercase8");

	RETURN;
}

static int test_islowercase(void)
{
	test(  islowercase('a'), "islowercase1");
	test(  islowercase('c'), "islowercase2");
	test(  islowercase('z'), "islowercase3");
	test(! islowercase('A'), "islowercase4");
	test(! islowercase('C'), "islowercase5");
	test(! islowercase('Z'), "islowercase6");
	test(! islowercase(' '), "islowercase7");
	test(! islowercase('*'), "islowercase8");

	RETURN;
}

static int test_isdigitcase(void)
{
	test(  isdigitcase('0'), "isdigitcase1");
	test(  isdigitcase('3'), "isdigitcase2");
	test(  isdigitcase('9'), "isdigitcase3");
	test(! isdigitcase('a'), "isdigitcase4");
	test(! isdigitcase('B'), "isdigitcase5");
	test(! isdigitcase('*'), "isdigitcase6");

	RETURN;
}

static int test_isalphabetic(void)
{
	test(  isalphabetic('a'), "isalphabetic1");
	test(  isalphabetic('B'), "isalphabetic2");
	test(  isalphabetic('z'), "isalphabetic3");
	test(  isalphabetic('Z'), "isalphabetic4");
	test(! isalphabetic('0'), "isalphabetic5");
	test(! isalphabetic('7'), "isalphabetic6");
	test(! isalphabetic('&'), "isalphabetic7");

	RETURN;
}

static int test_isalphanumeric(void)
{
	test(  isalphanumeric('a'), "isalphanumeric1");
	test(  isalphanumeric('B'), "isalphanumeric2");
	test(  isalphanumeric('z'), "isalphanumeric3");
	test(  isalphanumeric('Z'), "isalphanumeric4");
	test(  isalphanumeric('0'), "isalphanumeric5");
	test(  isalphanumeric('7'), "isalphanumeric6");
	test(! isalphanumeric('&'), "isalphanumeric7");

	RETURN;
}

static int test_toupperunicode(void)
{
	test(toupperunicode('a') == 'A', "toupperunicode1");
	test(toupperunicode('c') == 'C', "toupperunicode2");
	test(toupperunicode('z') == 'Z', "toupperunicode3");
	test(toupperunicode('A') == 'A', "toupperunicode4");
	test(toupperunicode('C') == 'C', "toupperunicode5");
	test(toupperunicode('Z') == 'Z', "toupperunicode6");
	test(toupperunicode('0') == '0', "toupperunicode7");
	test(toupperunicode('$') == '$', "toupperunicode8");

	RETURN;
}

static int test_tolowerunicode(void)
{
	test(tolowerunicode('A') == 'a', "tolowerunicode1");
	test(tolowerunicode('C') == 'c', "tolowerunicode2");
	test(tolowerunicode('Z') == 'z', "tolowerunicode3");
	test(tolowerunicode('a') == 'a', "tolowerunicode4");
	test(tolowerunicode('c') == 'c', "tolowerunicode5");
	test(tolowerunicode('z') == 'z', "tolowerunicode6");
	test(tolowerunicode('0') == '0', "tolowerunicode7");
	test(tolowerunicode('$') == '$', "tolowerunicode8");

	RETURN;
}


/*
 *  character type
 */
static int test_issurrogatepair(void)
{
	test(! issurrogatepair('a'), "issurrogatepair1");
	test(! issurrogatepair(0xD7FF), "issurrogatepair2");
	test(  issurrogatepair(0xD800), "issurrogatepair3");
	test(  issurrogatepair(0xDA00), "issurrogatepair4");
	test(  issurrogatepair(0xDFFF), "issurrogatepair5");
	test(! issurrogatepair(0xE000), "issurrogatepair6");
	test(! issurrogatepair(0xFFFF), "issurrogatepair7");
	test(! issurrogatepair(0x010000), "issurrogatepair8");
	test(! issurrogatepair(0x110000), "issurrogatepair9");

	RETURN;
}

static int test_isbaserange(void)
{
	test(  isbaserange(0), "isbaserange1");
	test(  isbaserange('A'), "isbaserange2");
	test(  isbaserange(0xD800), "isbaserange3");
	test(  isbaserange(0x10FFFF), "isbaserange4");
	test(! isbaserange(0x110000), "isbaserange5");
	test(! isbaserange(0x80000000), "isbaserange6");

	RETURN;
}

static int test_isstandardtype(void)
{
	test(  isstandardtype(0x0A), "isstandardtype1");
	test(  isstandardtype(' '), "isstandardtype2");
	test(  isstandardtype('a'), "isstandardtype3");
	test(  isstandardtype('z'), "isstandardtype4");
	test(  isstandardtype('A'), "isstandardtype5");
	test(  isstandardtype('Z'), "isstandardtype6");
	test(  isstandardtype('%'), "isstandardtype7");
	test(! isstandardtype(0), "isstandardtype8");
	test(! isstandardtype(0x0D), "isstandardtype9");
	test(! isstandardtype(0xFF), "isstandardtype10");
	test(! isstandardtype(0x1000), "isstandardtype11");
	test(! isstandardtype(0x1F), "isstandardtype12");
	test(  isstandardtype(0x20), "isstandardtype13");
	test(  isstandardtype(0x7E), "isstandardtype14");
	test(! isstandardtype(0x7F), "isstandardtype15");

	RETURN;
}

static int test_isbasetype(void)
{
	test(  isbasetype('A'), "isbasetype1");
	test(  isbasetype('%'), "isbasetype2");
	test(  isbasetype(0), "isbasetype3");
	test(  isbasetype(0xD7FF), "isbasetype4");
	test(! isbasetype(0xD800), "isbasetype5");
	test(! isbasetype(0xDFFF), "isbasetype6");
	test(  isbasetype(0xE000), "isbasetype7");
	test(  isbasetype(0x10FFFF), "isbasetype8");
	test(! isbasetype(0x110000), "isbasetype9");
	test(! isbasetype(0x80000000), "isbasetype10");

	RETURN;
}

static int test_isextendedtype(void)
{
	test(! isextendedtype('a'), "isextendedtype1");
	test(! isextendedtype(0x7FFFFFFF), "isextendedtype2");
	test(  isextendedtype(0x80000000), "isextendedtype3");
	test(  isextendedtype(0x80001000), "isextendedtype4");

	RETURN;
}


/*
 *  main
 */
static int testbreak_character_check(void)
{
	/* character check */
	TestBreak(test_isbasechar);
	TestBreak(test_isuppercase);
	TestBreak(test_islowercase);
	TestBreak(test_isdigitcase);
	TestBreak(test_isalphabetic);
	TestBreak(test_isalphanumeric);
	TestBreak(test_toupperunicode);
	TestBreak(test_tolowerunicode);

	/* character type */
	TestBreak(test_issurrogatepair);
	TestBreak(test_isbaserange);
	TestBreak(test_isstandardtype);
	TestBreak(test_isbasetype);
	TestBreak(test_isextendedtype);

	return 0;
}

int test_character_check(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	ptr = Execute_Thread;
	lisp_info_enable = 1;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		lisp_initialize = 1;
		result = testbreak_character_check();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}
