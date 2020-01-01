#include "bigcons.h"
#include "bignum.h"
#include "control.h"
#include "condition.h"
#include "env_time.h"
#include "integer.h"
#include "localtime.h"
#include "object.h"
#include "rational.h"
#include "real_floor.h"
#include "symbol.h"


#define ENCODE_UNIVERSAL_1900  693961
#define ENCODE_UNIVERSAL_1970  719528

/*
 *  decode-universal-time
 */
static void decode_universal_second(addr sec, addr *rsec, addr *rmin, addr *rhour)
{
	size_t v;

	getindex_integer(sec, &v);
	fixnum_heap(rsec, (fixnum)(v % 60));
	v = v / 60;
	fixnum_heap(rmin, (fixnum)(v % 60));
	v = v / 60;
	Check(24 <= v, "hour error");
	fixnum_heap(rhour, (fixnum)v);
}

static void decode_universal_year4(size_t day, int leap, size_t *ryear, size_t *rday)
{
	size_t year1day;

	year1day = 365 + leap;
	if (day < year1day) {
		*ryear = 0;
		*rday = day;
		return;
	}
	day -= year1day;
	*ryear = day / 365 + 1;
	*rday = day % 365;
}

#define DECODE_UNIVERSAL_YEAR4DAY		(365*4 + 1)
static void decode_universal_year100(size_t day, int leap, size_t *ryear, size_t *rday)
{
	size_t year4day, y4, year;

	year4day = DECODE_UNIVERSAL_YEAR4DAY - 1 + leap;
	if (day < year4day) {
		decode_universal_year4(day, leap, ryear, rday);
		return;
	}
	day -= year4day;
	y4 = day / DECODE_UNIVERSAL_YEAR4DAY;
	day = day % DECODE_UNIVERSAL_YEAR4DAY;
	decode_universal_year4(day, 1, &year, rday);
	*ryear = (y4 + 1) * 4 + year;
}

#define DECODE_UNIVERSAL_YEAR100DAY		(365*100 + 25 - 1)
static void decode_universal_year400(size_t day, size_t *ryear, size_t *rday)
{
	size_t year100day, y100, year;

	year100day = DECODE_UNIVERSAL_YEAR100DAY + 1;
	if (day < year100day) {
		decode_universal_year100(day, 1, ryear, rday);
		return;
	}
	day -= year100day;
	y100 = day / DECODE_UNIVERSAL_YEAR100DAY;
	day = day % DECODE_UNIVERSAL_YEAR100DAY;
	decode_universal_year100(day, 0, &year, rday);
	*ryear = (y100 + 1) * 100 + year;
}

#define DECODE_UNIVERSAL_YEAR400DAY		(365*400 + 100 + 1 - 4)
static void decode_universal_year(size_t day, size_t *ryear, size_t *rday)
{
	size_t y400, year;

	y400 = day / DECODE_UNIVERSAL_YEAR400DAY;
	day = day % DECODE_UNIVERSAL_YEAR400DAY;
	decode_universal_year400(day, &year, rday);
	*ryear = y400 * 400 + year;
}

static int encode_universal_leap(size_t year)
{
	return ((year % 4) == 0) && (((year % 100) != 0) || ((year % 400) == 0));
}

static size_t encode_universal_month_day(size_t month, int leap)
{
	static size_t array[12] = {31,28,31,30,31,30,  31,31,30,31,30,31};

	Check(12 <= month, "month error");
	return ((month == 1) && leap)?
		(array[month] + 1):
		array[month];
}

static void decode_universal_month(size_t year, size_t day, addr *rmonth, addr *rday)
{
	int leap;
	size_t i, v;

	leap = encode_universal_leap(year);
	for (i = 0; i < 12; i++) {
		v = encode_universal_month_day(i, leap);
		if (day < v) {
			fixnum_heap(rmonth, (fixnum)(i + 1));
			fixnum_heap(rday, (fixnum)(day + 1));
			return;
		}
		day -= v;
	}

	/* error */
	fmte("decode-universal-month error.", NULL);
	*rmonth = NULL;
	*rday = NULL;
}

static void decode_universal_time_zone(LocalRoot local,
		struct universal_time_struct *u, addr value, addr zone)
{
	addr v, year, month, day, hour, minute, sec, week;
	size_t date, count;

	/* value, zone */
	fixnum_local(local, &v, 60 * 60);
	multi_fixnum_rational_common(local, v, zone, &v);
	floor1_common(local, &v, &week, v);
	minus_ii_real_common(local, value, v, &value);

	/* week */
	fixnum_local(local, &v, 24 * 60 * 60);
	floor2_common(local, &day, &sec, value, v);
	getindex_integer(day, &date);
	fixnum_heap(&week, (fixnum)(date % 7));

	/* day + 1900year */
	date += ENCODE_UNIVERSAL_1900;

	/* date, time */
	decode_universal_second(sec, &sec, &minute, &hour);
	decode_universal_year(date, &count, &date);
	decode_universal_month(count, date, &month, &day);
	make_index_integer_heap(&year, count);

	/* result */
	u->second = sec;
	u->minute = minute;
	u->hour = hour;
	u->date = day;
	u->month = month;
	u->year = year;
	u->week = week;
	u->daylight_p = Nil;
	u->zone = zone;
}

/* zone nil */
static int encode_universal_offset(fixnum *ret)
{
	time_t now, a, b;
	struct tm str;

	now = time(NULL);
	if (gmtime_arch(&str, &now))
		return 1;
	a = mktime(&str);
	if (localtime_arch(&str, &now))
		return 1;
	b = mktime(&str);
	*ret = (fixnum)(a - b);

	return 0;
}

static void decode_universal_default(LocalRoot local,
		struct universal_time_struct *u, addr pos, fixnum offset)
{
	addr zone;
	fixnum_heap(&zone, offset);
	decode_universal_time_zone(local, u, pos, zone);
}

static void decode_universal_struct(LocalRoot local,
		struct universal_time_struct *u, struct tm *str, fixnum offset)
{
	int check;
	addr value;

	/* year */
	fixnum_heap(&value, (fixnum)(str->tm_year + 1900));
	u->year = value;

	/* month */
	fixnum_heap(&value, (fixnum)(str->tm_mon + 1));
	u->month = value;

	/* date */
	fixnum_heap(&value, (fixnum)str->tm_mday);
	u->date = value;

	/* hour */
	fixnum_heap(&value, (fixnum)str->tm_hour);
	u->hour = value;

	/* minute */
	fixnum_heap(&value, (fixnum)str->tm_min);
	u->minute = value;

	/* second */
	fixnum_heap(&value, (fixnum)str->tm_sec);
	u->second = value;

	/* week */
	check = str->tm_wday;
	fixnum_heap(&value, (check == 0)? 6: check - 1);
	u->week = value;

	/* daylight_p */
	u->daylight_p = str->tm_isdst? T: Nil;

	/* zone */
	fixnum_heap(&value, offset);
	u->zone = value;
}

static void decode_universal_diff_value(LocalRoot local, addr *ret)
{
	addr symbol, value;

	/* 1900/01/01 - 1970/01/01 */
	GetConst(SYSTEM_ENCODE_UNIVERSAL_1970, &symbol);
	GetValueSymbol(symbol, &value);
	if (value == Unbound) {
		bigcons_char_local(local, &value, 10, "2208988800");
		bignum_cons_heap(&value, signplus_bignum, value);
		SetValueSymbol(symbol, value);
	}
	*ret = value;
}

static void decode_universal_diff(LocalRoot local, addr value, addr *ret)
{
	addr right;
	decode_universal_diff_value(local, &right);
	minus_ii_real_common(local, value, right, ret);
}

static void decode_universal_time_nil(LocalRoot local,
		struct universal_time_struct *u, addr pos)
{
	size_t value;
	time_t timer;
	fixnum offset;
	struct tm str;

	/* zone offset */
	if (encode_universal_offset(&offset)) {
		offset = 0;
		goto decode_default;
	}
	offset /= 60 * 60;

	/* universal time */
	decode_universal_diff(local, pos, &pos);
	if (GetIndex_integer(pos, &value))
		goto decode_default;

	/* timer */
	timer = (time_t)value;
	if (timer < 0)
		goto decode_default;
	if (localtime_arch(&str, &timer))
		goto decode_default;
	if (str.tm_isdst < 0)
		goto decode_default;

	/* decode struct */
	decode_universal_struct(local, u, &str, offset);
	return;

decode_default:
	decode_universal_default(local, u, pos, offset);
}

_g void decode_universal_time_common(LocalRoot local,
		struct universal_time_struct *u, addr pos, addr zone)
{
	if (zone == Nil)
		decode_universal_time_nil(local, u, pos);
	else
		decode_universal_time_zone(local, u, pos, zone);
}


/*
 *  encode-universal-time
 */
static size_t encode_universal_month(size_t day, size_t month, size_t year)
{
	int leap;
	size_t sum, i;

	/* check */
	leap = encode_universal_leap(year);
	i = encode_universal_month_day(month, leap);
	if (i < day) {
		fmte("Invalid day ~A.", intsizeh(day), NULL);
		return 0;
	}

	/* days */
	sum = 0;
	for (i = 0; i < month; i++)
		sum += encode_universal_month_day(i, leap);

	return sum;
}

static size_t encode_universal_year(size_t year)
{
	size_t year1, y4, y100, y400;

	if (year == 0)
		return 0;
	year1 = year - 1;
	y4 = (year1 / 4) + 1;
	y100 = (year1 / 100) + 1;
	y400 = (year1 / 400) + 1;

	return (year * 365) + y4 - y100 +y400;
}

static size_t encode_universal_day(size_t day, size_t month, size_t year)
{
	return (day - 1)
		+ encode_universal_month(day, month - 1, year)
		+ encode_universal_year(year);
}

static void encode_universal_second_absolute(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour, addr zone, size_t day)
{
	addr left, right;

	/* (* 24 day) -> (* fixnum integer) -> integer */
	fixnum_heap(&left, 24);
	make_index_integer_local(local, &right, day);
	multi_ii_real_common(local, left, right, &right);
	/* (+ hour zone) -> (+ fixnum rational) -> rational */
	if (zone != Unbound)
		plus_fixnum_rational_common(local, hour, zone, &hour);
	/* (+ hour right) -> (+ rational integer) -> rational */
	plus_rational_common(local, hour, right, &right);
	/* (* 60 right) -> (* fixnum rational) -> rational */
	fixnum_heap(&left, 60);
	multi_fixnum_rational_common(local, left, right, &right);
	/* (+ min right) -> (+ fixnum rational) -> rational */
	plus_fixnum_rational_common(local, min, right, &right);
	/* (* 60 right) -> (* fixnum rational) -> rational */
	multi_fixnum_rational_common(local, left, right, &right);
	/* (+ sec right) -> (+ fixnum rational) -> rational */
	plus_fixnum_rational_common(local, sec, right, &right);
	/* (floor right) */
	floor1_common(local, ret, &right, right);
}

static void encode_universal_time_absolute(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour,
		size_t day, size_t month, size_t year, addr zone)
{
	day = encode_universal_day(day, month, year);
	day -= ENCODE_UNIVERSAL_1900;
	encode_universal_second_absolute(local, ret, sec, min, hour, zone, day);
}

/* daylight */
static void encode_universal_diff(LocalRoot local, addr *ret, addr value)
{
	addr right;
	decode_universal_diff_value(local, &right);
	plus_ii_real_common(local, value, right, ret);
}

static int encode_universal_time_standard(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour, size_t day)
{
	size_t value;
	time_t now, a, b;
	struct tm str;

	day -= ENCODE_UNIVERSAL_1970;
	encode_universal_second_absolute(local, &sec, sec, min, hour, Unbound, day);
	/* mktime */
	if (GetIndex_integer(sec, &value))
		return 1;  /* Too large */
	now = (time_t)value;
	if (now < 0)
		return 1;  /* Too large (2038 problem in 32bit arch) */
	if (gmtime_arch(&str, &now))
		return 1;  /* error gmtime */
	a = mktime(&str);
	if (localtime_arch(&str, &now))
		return 1;  /* error gmtime */
	b = mktime(&str);
	now += (a - b);
	if (now < 0)
		return 1;
	value = (size_t)now;
	make_index_integer_local(local, &sec, value);
	encode_universal_diff(local, ret, sec);

	return 0;
}

static void encode_universal_time_offset(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour, size_t day)
{
	fixnum value;
	addr diff;

	day -= ENCODE_UNIVERSAL_1900;
	encode_universal_second_absolute(local, &sec, sec, min, hour, Unbound, day);
	if (encode_universal_offset(&value)) {
		fmte("encode-universal-offset error", NULL);
		return;
	}
	fixnum_heap(&diff, value);
	plus_ii_real_common(local, sec, diff, ret);
}

static void encode_universal_time_daylight(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour,
		size_t day, size_t month, size_t year)
{
	day = encode_universal_day(day, month, year);
	if (encode_universal_time_standard(local, ret, sec, min, hour, day))
		encode_universal_time_offset(local, ret, sec, min, hour, day);
}

static size_t encode_universal_time_year(size_t year, addr year_error)
{
	time_t now;
	struct tm str;
	size_t a, b;

	if (1900 <= year)
		return year;
	if (100 <= year) {
		fmte("Invalid year ~A", year_error, NULL);
		return 0;
	}
	/* 0 - 99 */
	now = time(NULL);
	if (localtime_arch(&str, &now)) {
		fmte("localtime error.", NULL);
		return 0;
	}
	a = (1900 + str.tm_year) / 100;
	b = str.tm_year % 100;
	if (b < 50) {
		if (b + 50 <= year) {
			if (a == 0) {
				fmte("Too small year ~A", year_error, NULL);
				return 0;
			}
			a--;
		}
	}
	else {
		if (year < b - 50)
			a++;
	}

	return 100*a + year;
}

_g void encode_universal_time_common(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour,
		addr day, addr month, addr year, addr zone)
{
	size_t d, m, y;

	getindex_integer(day, &d);
	getindex_integer(month, &m);
	getindex_integer(year, &y);
	y = encode_universal_time_year(y, year);

	if (zone != Unbound)
		encode_universal_time_absolute(local, ret, sec, min, hour, d, m, y, zone);
	else
		encode_universal_time_daylight(local, ret, sec, min, hour, d, m, y);
}


/*
 *  get-universal-time
 */
_g void get_universal_time_common(LocalRoot local, addr *ret)
{
	addr left, right;

	make_index_integer_heap(&left, (size_t)time(NULL));
	decode_universal_diff_value(local, &right);
	plus_ii_real_common(local, left, right, ret);
}


/*
 *  get-decoded-time
 */
_g void get_decoded_time_common(LocalRoot local, struct universal_time_struct *u)
{
	addr pos;
	get_universal_time_common(local, &pos);
	decode_universal_time_common(local, u, pos, Nil);
}


/*
 *  internal-time-units-per-second
 */
#if defined(LISP_WINDOWS)
#include <windows.h>

_g void get_internal_time_units_per_second(fixnum *ret)
{
	*ret = 1000;
}
_g void get_internal_real_time_common(LocalRoot local, addr *ret)
{
#ifdef LISP_64BIT
	integer_fixed_heap(ret, signplus_bignum, (fixed)GetTickCount64());
#else
	ULONGLONG value;
	addr a, b;

	value = GetTickCount64();
	integer_fixed_local(local, &a, signplus_bignum, (fixed)(value >> 32ULL));
	integer_fixed_local(local, &b, signplus_bignum, (fixed)(0xFFFFFFFFULL & value));
	shiftup_bignum_local(local, &a, a, 32);
	plus_ii_real_common(local, a, b, ret);
#endif
}


#elif defined(LISP_POSIX)
#include <sys/time.h>

_g void get_internal_time_units_per_second(fixnum *ret)
{
	struct timeval tv;

	if (gettimeofday(&tv, NULL) == 0)
		*ret = 1000000;
	else
		*ret = 1;
}
_g void get_internal_real_time_common(LocalRoot local, addr *ret)
{
	struct timeval tv;
	addr high, low, value;

	if (gettimeofday(&tv, NULL) == 0) {
		make_index_integer_local(local, &high, (size_t)tv.tv_sec);
		fixnum_local(local, &low, (fixnum)tv.tv_usec);
		fixnum_local(local, &value, 1000000);
		multi_ii_real_common(local, high, value, &high);
		plus_fi_real_common(local, low, high, ret);
	}
	else {
		make_index_integer_heap(ret, (size_t)time(NULL));
	}
}

#else
_g void get_internal_time_units_per_second(fixnum *ret)
{
	*ret = 1;
}
_g void get_internal_real_time_common(LocalRoot local, addr *ret)
{
	make_index_integer_heap(ret, (size_t)time(NULL));
}
#endif

_g void get_internal_run_time_common(addr *ret)
{
	size_t value;
	value = ControlCounter;
	make_index_integer_heap(ret, value);
}

