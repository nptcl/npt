#include "character_check.c"
#include "degrade.h"

/*
 *  character check
 */
static int test_isbasechar(void)
{
	test(! isbasechar(1), "isbasechar.1");
	test(  isbasechar(2), "isbasechar.2");
	test(  isbasechar(8), "isbasechar.3");
	test(  isbasechar(16), "isbasechar.4");
	test(  isbasechar(36), "isbasechar.5");
	test(! isbasechar(37), "isbasechar.6");

	RETURN;
}

static int test_isuppercase(void)
{
	test(  isuppercase('A'), "isuppercase.1");
	test(  isuppercase('C'), "isuppercase.2");
	test(  isuppercase('Z'), "isuppercase.3");
	test(! isuppercase('a'), "isuppercase.4");
	test(! isuppercase('c'), "isuppercase.5");
	test(! isuppercase('z'), "isuppercase.6");
	test(! isuppercase(' '), "isuppercase.7");
	test(! isuppercase('*'), "isuppercase.8");

	RETURN;
}

static int test_islowercase(void)
{
	test(  islowercase('a'), "islowercase.1");
	test(  islowercase('c'), "islowercase.2");
	test(  islowercase('z'), "islowercase.3");
	test(! islowercase('A'), "islowercase.4");
	test(! islowercase('C'), "islowercase.5");
	test(! islowercase('Z'), "islowercase.6");
	test(! islowercase(' '), "islowercase.7");
	test(! islowercase('*'), "islowercase.8");

	RETURN;
}

static int test_isdigitcase(void)
{
	test(  isdigitcase('0'), "isdigitcase.1");
	test(  isdigitcase('3'), "isdigitcase.2");
	test(  isdigitcase('9'), "isdigitcase.3");
	test(! isdigitcase('a'), "isdigitcase.4");
	test(! isdigitcase('B'), "isdigitcase.5");
	test(! isdigitcase('*'), "isdigitcase.6");

	RETURN;
}

static int test_isalphabetic(void)
{
	test(  isalphabetic('a'), "isalphabetic.1");
	test(  isalphabetic('B'), "isalphabetic.2");
	test(  isalphabetic('z'), "isalphabetic.3");
	test(  isalphabetic('Z'), "isalphabetic.4");
	test(! isalphabetic('0'), "isalphabetic.5");
	test(! isalphabetic('7'), "isalphabetic.6");
	test(! isalphabetic('&'), "isalphabetic.7");

	RETURN;
}

static int test_isalphanumeric(void)
{
	test(  isalphanumeric('a'), "isalphanumeric.1");
	test(  isalphanumeric('B'), "isalphanumeric.2");
	test(  isalphanumeric('z'), "isalphanumeric.3");
	test(  isalphanumeric('Z'), "isalphanumeric.4");
	test(  isalphanumeric('0'), "isalphanumeric.5");
	test(  isalphanumeric('7'), "isalphanumeric.6");
	test(! isalphanumeric('&'), "isalphanumeric.7");

	RETURN;
}

static int test_toupperunicode(void)
{
	test(toupperunicode('a') == 'A', "toupperunicode.1");
	test(toupperunicode('c') == 'C', "toupperunicode.2");
	test(toupperunicode('z') == 'Z', "toupperunicode.3");
	test(toupperunicode('A') == 'A', "toupperunicode.4");
	test(toupperunicode('C') == 'C', "toupperunicode.5");
	test(toupperunicode('Z') == 'Z', "toupperunicode.6");
	test(toupperunicode('0') == '0', "toupperunicode.7");
	test(toupperunicode('$') == '$', "toupperunicode.8");

	RETURN;
}

static int test_tolowerunicode(void)
{
	test(tolowerunicode('A') == 'a', "tolowerunicode.1");
	test(tolowerunicode('C') == 'c', "tolowerunicode.2");
	test(tolowerunicode('Z') == 'z', "tolowerunicode.3");
	test(tolowerunicode('a') == 'a', "tolowerunicode.4");
	test(tolowerunicode('c') == 'c', "tolowerunicode.5");
	test(tolowerunicode('z') == 'z', "tolowerunicode.6");
	test(tolowerunicode('0') == '0', "tolowerunicode.7");
	test(tolowerunicode('$') == '$', "tolowerunicode.8");

	RETURN;
}


/*
 *  character type
 */
static int test_issurrogatepair(void)
{
	test(! issurrogatepair('a'), "issurrogatepair.1");
	test(! issurrogatepair(0xD7FF), "issurrogatepair.2");
	test(  issurrogatepair(0xD800), "issurrogatepair.3");
	test(  issurrogatepair(0xDA00), "issurrogatepair.4");
	test(  issurrogatepair(0xDFFF), "issurrogatepair.5");
	test(! issurrogatepair(0xE000), "issurrogatepair.6");
	test(! issurrogatepair(0xFFFF), "issurrogatepair.7");
	test(! issurrogatepair(0x010000), "issurrogatepair.8");
	test(! issurrogatepair(0x110000), "issurrogatepair.9");

	RETURN;
}

static int test_isbaserange(void)
{
	test(  isbaserange(0), "isbaserange.1");
	test(  isbaserange('A'), "isbaserange.2");
	test(  isbaserange(0xD800), "isbaserange.3");
	test(  isbaserange(0x10FFFF), "isbaserange.4");
	test(! isbaserange(0x110000), "isbaserange.5");
	test(! isbaserange(0x80000000), "isbaserange.6");

	RETURN;
}

static int test_isstandardtype(void)
{
	test(  isstandardtype(0x0A), "isstandardtype.1");
	test(  isstandardtype(' '), "isstandardtype.2");
	test(  isstandardtype('a'), "isstandardtype.3");
	test(  isstandardtype('z'), "isstandardtype.4");
	test(  isstandardtype('A'), "isstandardtype.5");
	test(  isstandardtype('Z'), "isstandardtype.6");
	test(  isstandardtype('%'), "isstandardtype.7");
	test(! isstandardtype(0), "isstandardtype.8");
	test(! isstandardtype(0x0D), "isstandardtype.9");
	test(! isstandardtype(0xFF), "isstandardtype.10");
	test(! isstandardtype(0x1000), "isstandardtype.11");
	test(! isstandardtype(0x1F), "isstandardtype.12");
	test(  isstandardtype(0x20), "isstandardtype.13");
	test(  isstandardtype(0x7E), "isstandardtype.14");
	test(! isstandardtype(0x7F), "isstandardtype.15");

	RETURN;
}

static int test_isbasetype(void)
{
	test(  isbasetype('A'), "isbasetype.1");
	test(  isbasetype('%'), "isbasetype.2");
	test(  isbasetype(0), "isbasetype.3");
	test(  isbasetype(0xD7FF), "isbasetype.4");
	test(! isbasetype(0xD800), "isbasetype.5");
	test(! isbasetype(0xDFFF), "isbasetype.6");
	test(  isbasetype(0xE000), "isbasetype.7");
	test(  isbasetype(0x10FFFF), "isbasetype.8");
	test(! isbasetype(0x110000), "isbasetype.9");
	test(! isbasetype(0x00800000), "isbasetype.10");

	RETURN;
}

static int test_isextendedtype(void)
{
	test(! isextendedtype('a'), "isextendedtype.1");
	test(! isextendedtype(0x007FFFFF), "isextendedtype.2");
	test(  isextendedtype(0x00800000), "isextendedtype.3");
	test(  isextendedtype(0x00801000), "isextendedtype.4");

	RETURN;
}


/*
 *  character_check
 */
int test_character_check(void)
{
	DegradeTitle;

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

