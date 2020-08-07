#include <errno.h>
#include "bignum.h"
#include "bignum_cons.h"
#include "bignum_object.h"
#include "control_object.h"
#include "control_operator.h"
#include "condition.h"
#include "condition_debugger.h"
#include "env_time.h"
#include "function.h"
#include "hold.h"
#include "integer.h"
#include "localtime.h"
#include "object.h"
#include "pointer.h"
#include "ratio.h"
#include "ratio_multi.h"
#include "rational_multi.h"
#include "rational_plus.h"
#include "real_division.h"
#include "real_floor.h"
#include "real_truncate.h"
#include "restart.h"
#include "strvect.h"
#include "symbol.h"

#define ENCODE_UNIVERSAL_1900	693961
#define ENCODE_UNIVERSAL_1970	719528

#if defined(LISP_POSIX)
#define LISP_SLEEP_INTERVAL		1000000
#define LISP_SLEEP_INTERVAL_F	1.0e6

#elif defined(LISP_WINDOWS)
#define LISP_SLEEP_INTERVAL		1000
#define LISP_SLEEP_INTERVAL_F	1.0e3

#else
#define LISP_SLEEP_INTERVAL		1
#define LISP_SLEEP_INTERVAL_F	1.0e0
#endif

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

static int decode_universal_month_(size_t year, size_t day, addr *rmonth, addr *rday)
{
	int leap;
	size_t i, v;

	leap = encode_universal_leap(year);
	for (i = 0; i < 12; i++) {
		v = encode_universal_month_day(i, leap);
		if (day < v) {
			fixnum_heap(rmonth, (fixnum)(i + 1));
			fixnum_heap(rday, (fixnum)(day + 1));
			return 0;
		}
		day -= v;
	}

	/* error */
	*rmonth = Nil;
	*rday = Nil;
	return fmte_("decode-universal-month error.", NULL);
}

static int decode_universal_time_zone_(LocalRoot local,
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
	Return(decode_universal_month_(count, date, &month, &day));
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

	return 0;
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

static int decode_universal_default_(LocalRoot local,
		struct universal_time_struct *u, addr pos, fixnum offset)
{
	addr zone;
	fixnum_heap(&zone, offset);
	return decode_universal_time_zone_(local, u, pos, zone);
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

static int decode_universal_diff_value_(LocalRoot local, addr *ret)
{
	addr symbol, value;

	/* 1900/01/01 - 1970/01/01 */
	GetConst(SYSTEM_ENCODE_UNIVERSAL_1970, &symbol);
	GetValueSymbol(symbol, &value);
	if (value == Unbound) {
		Return(bigcons_char_local_(local, &value, 10, "2208988800"));
		bignum_cons_heap(&value, signplus_bignum, value);
		Return(setvalue_symbol_(symbol, value));
	}

	return Result(ret, value);
}

static int decode_universal_diff_(LocalRoot local, addr value, addr *ret)
{
	addr right;

	Return(decode_universal_diff_value_(local, &right));
	minus_ii_real_common(local, value, right, ret);

	return 0;
}

static int decode_universal_time_nil_(LocalRoot local,
		struct universal_time_struct *u, addr time)
{
	size_t value;
	time_t timer;
	fixnum offset;
	struct tm str;
	addr pos;

	/* zone offset */
	if (encode_universal_offset(&offset)) {
		offset = 0;
		goto decode_default;
	}
	offset /= 60 * 60;

	/* universal time */
	Return(decode_universal_diff_(local, time, &pos));
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
	return 0;

decode_default:
	return decode_universal_default_(local, u, time, offset);
}

_g int decode_universal_time_common_(LocalRoot local,
		struct universal_time_struct *u, addr pos, addr zone)
{
	if (zone == Nil)
		return decode_universal_time_nil_(local, u, pos);
	else
		return decode_universal_time_zone_(local, u, pos, zone);
}


/*
 *  encode-universal-time
 */
static int encode_universal_month_(size_t day, size_t month, size_t year, size_t *ret)
{
	int leap;
	size_t sum, i;

	/* check */
	leap = encode_universal_leap(year);
	i = encode_universal_month_day(month, leap);
	if (i < day) {
		*ret = 0;
		return fmte_("Invalid day ~A.", intsizeh(day), NULL);
	}

	/* days */
	sum = 0;
	for (i = 0; i < month; i++)
		sum += encode_universal_month_day(i, leap);

	return Result(ret, sum);
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

	return (year * 365) + y4 - y100 + y400;
}

static int encode_universal_day_(size_t day, size_t month, size_t year, size_t *ret)
{
	size_t size;

	Return(encode_universal_month_(day, month - 1, year, &size));
		size += (day - 1) + encode_universal_year(year);

	return Result(ret, size);
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

static int encode_universal_time_absolute_(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour,
		size_t day, size_t month, size_t year, addr zone)
{
	Return(encode_universal_day_(day, month, year, &day));
	day -= ENCODE_UNIVERSAL_1900;
	encode_universal_second_absolute(local, ret, sec, min, hour, zone, day);

	return 0;
}

/* daylight */
static int encode_universal_diff_(LocalRoot local, addr *ret, addr value)
{
	addr right;

	Return(decode_universal_diff_value_(local, &right));
	plus_ii_real_common(local, value, right, ret);

	return 0;
}

static int encode_universal_time_standard_(LocalRoot local, addr *value,
		addr sec, addr min, addr hour, size_t day, int *ret)
{
	size_t size;
	time_t now, a, b;
	struct tm str;

	day -= ENCODE_UNIVERSAL_1970;
	encode_universal_second_absolute(local, &sec, sec, min, hour, Unbound, day);
	/* mktime */
	if (GetIndex_integer(sec, &size))
		return Result(ret, 1);  /* Too large */
	now = (time_t)size;
	if (now < 0)
		return Result(ret, 1);  /* Too large (2038 problem in 32bit arch) */
	if (gmtime_arch(&str, &now))
		return Result(ret, 1);  /* error gmtime */
	a = mktime(&str);
	if (localtime_arch(&str, &now))
		return Result(ret, 1);  /* error localtime */
	b = mktime(&str);
	now += (a - b);
	if (now < 0)
		return Result(ret, 1);
	size = (size_t)now;
	make_index_integer_local(local, &sec, size);
	Return(encode_universal_diff_(local, value, sec));

	return Result(ret, 0);
}

static int encode_universal_time_offset_(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour, size_t day)
{
	fixnum value;
	addr diff;

	day -= ENCODE_UNIVERSAL_1900;
	encode_universal_second_absolute(local, &sec, sec, min, hour, Unbound, day);
	if (encode_universal_offset(&value)) {
		*ret = Nil;
		return fmte_("encode-universal-offset error", NULL);
	}
	fixnum_heap(&diff, value);
	plus_ii_real_common(local, sec, diff, ret);

	return 0;
}

static int encode_universal_time_daylight_(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour,
		size_t day, size_t month, size_t year)
{
	int check;

	Return(encode_universal_day_(day, month, year, &day));
	Return(encode_universal_time_standard_(local, ret, sec, min, hour, day, &check));
	if (check)
		return encode_universal_time_offset_(local, ret, sec, min, hour, day);

	return 0;
}

static int encode_universal_time_year_(size_t year, addr year_error, size_t *ret)
{
	time_t now;
	struct tm str;
	size_t a, b;

	if (1900 <= year)
		return Result(ret, year);
	if (100 <= year) {
		*ret = 0;
		return fmte_("Invalid year ~A", year_error, NULL);
	}
	/* 0 - 99 */
	now = time(NULL);
	if (localtime_arch(&str, &now)) {
		*ret = 0;
		return fmte_("localtime error.", NULL);
	}
	a = (1900 + str.tm_year) / 100;
	b = str.tm_year % 100;
	if (b < 50) {
		if (b + 50 <= year) {
			if (a == 0) {
				*ret = 0;
				return fmte_("Too small year ~A", year_error, NULL);
			}
			a--;
		}
	}
	else {
		if (year < b - 50)
			a++;
	}

	return Result(ret, 100*a + year);
}

_g int encode_universal_time_common_(LocalRoot local, addr *ret,
		addr sec, addr min, addr hour,
		addr day, addr month, addr year, addr zone)
{
	size_t d, m, y;

	getindex_integer(day, &d);
	getindex_integer(month, &m);
	getindex_integer(year, &y);
	Return(encode_universal_time_year_(y, year, &y));

	if (zone != Unbound) {
		return encode_universal_time_absolute_(local, ret,
				sec, min, hour, d, m, y, zone);
	}
	else {
		return encode_universal_time_daylight_(local, ret,
				sec, min, hour, d, m, y);
	}
}


/*
 *  get-universal-time
 */
_g int get_universal_time_common_(LocalRoot local, addr *ret)
{
	addr left, right;

	make_index_integer_heap(&left, (size_t)time(NULL));
	Return(decode_universal_diff_value_(local, &right));
	plus_ii_real_common(local, left, right, ret);

	return 0;
}


/*
 *  get-decoded-time
 */
_g int get_decoded_time_common_(LocalRoot local, struct universal_time_struct *u)
{
	addr pos;
	Return(get_universal_time_common_(local, &pos));
	return decode_universal_time_common_(local, u, pos, Nil);
}


/*
 *  internal-time-units-per-second
 */
#if defined(LISP_WINDOWS)
#include <windows.h>

_g void get_internal_time_units_per_second(fixnum *ret)
{
	*ret = LISP_SLEEP_INTERVAL;
}
_g void get_internal_real_time_common(LocalRoot local, addr *ret)
{
#ifdef LISP_64BIT
	integer_fixed_heap(ret, signplus_bignum, (fixed)GetTickCount64());
#else
	ULONGLONG value;
	addr a, b;

	value = GetTickCount64();
	bignum_value_local(local, &a, signplus_bignum, (fixed)(value >> 32ULL));
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
		*ret = LISP_SLEEP_INTERVAL;
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
		fixnum_local(local, &value, LISP_SLEEP_INTERVAL);
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


/*
 *  sleep
 */
#if defined(LISP_POSIX)
#include <unistd.h>

#ifdef LISP_DEBUG
#define LISP_SLEEP_FIXNUM		3
#else
#define LISP_SLEEP_FIXNUM		86400
#endif

struct sleep_object_struct {
	volatile sig_atomic_t atomic;
};
#define SleepObjectStruct(x) ((struct sleep_object_struct *)PtrBodySS(x))

static void get_sleep_object(Execute ptr, addr *ret)
{
	addr pos;

	GetConst(COMMON_SLEEP, &pos);
	getspecial_local(ptr, pos, &pos);
	Check(pos == Unbound, "unbound error");
	CheckType(pos, LISPSYSTEM_SLEEP);
	*ret = pos;
}

static sig_atomic_t getatomic_sleep_object(Execute ptr)
{
	addr pos;
	get_sleep_object(ptr, &pos);
	return SleepObjectStruct(pos)->atomic;
}

static void setatomic_sleep_object(Execute ptr)
{
	addr pos;
	get_sleep_object(ptr, &pos);
	SleepObjectStruct(pos)->atomic = 1;
}

static void push_sleep_object(Execute ptr)
{
	addr pos, symbol, value;
	LocalRoot local;

	/* object */
	local = ptr->local;
	local_smallsize(local, &pos, LISPSYSTEM_SLEEP,
			1, sizeoft(struct sleep_object_struct));
	SleepObjectStruct(pos)->atomic = 0;

	/* push */
	GetConst(COMMON_SLEEP, &symbol);
	get_internal_real_time_common(local, &value);
	SetArraySS(pos, 0, value);
	pushspecial_control(ptr, symbol, pos);
}

static int sleep_close_object(Execute ptr)
{
	/* do nothing */
	return 0;
}

static void time_sleep_object(Execute ptr, addr *ret)
{
	addr pos;
	get_sleep_object(ptr, &pos);
	GetArraySS(pos, 0, ret);
}

static int sleep_second_common(Execute ptr, fixnum value)
{
	sleep((unsigned int)value);
	return getatomic_sleep_object(ptr);
}

static int sleep_moment_common(Execute ptr, fixnum value)
{
	useconds_t usec;

	/* argument */
	usec = (useconds_t)value;
	if (usec == 0)
		return 0;
	if (LISP_SLEEP_INTERVAL <= usec)
		usec = LISP_SLEEP_INTERVAL - 1;

	/* sleep */
	if (usleep(usec) == -1) {
		if (errno == EINVAL) {
			Abort("usleep error");
		}
	}

	return getatomic_sleep_object(ptr);
}

#elif defined(LISP_WINDOWS)
#include <windows.h>

#ifdef LISP_DEBUG
#define LISP_SLEEP_FIXNUM		3
#else
#define LISP_SLEEP_FIXNUM		86400000
#endif

struct sleep_object_struct {
	HANDLE hEvent;
};
#define SleepObjectStruct(x) ((struct sleep_object_struct *)PtrBodySS(x))

static void get_sleep_object(Execute ptr, addr *ret)
{
	addr pos;

	GetConst(COMMON_SLEEP, &pos);
	getspecial_local(ptr, pos, &pos);
	Check(pos == Unbound, "unbound error");
	CheckType(pos, LISPSYSTEM_SLEEP);
	*ret = pos;
}

static void setatomic_sleep_object(Execute ptr)
{
	addr pos;
	get_sleep_object(ptr, &pos);
	SetEvent(SleepObjectStruct(pos)->hEvent);
}

static void push_sleep_object(Execute ptr)
{
	addr pos, symbol, value;
	LocalRoot local;
	HANDLE handle;

	/* event */
	handle = CreateEvent(NULL, TRUE, FALSE, NULL);
	if (handle == NULL) {
		Abort("CreateEvent error.");
		return;
	}

	/* object */
	local = ptr->local;
	local_smallsize(local, &pos, LISPSYSTEM_SLEEP,
			1, sizeoft(struct sleep_object_struct));
	SleepObjectStruct(pos)->hEvent = handle;

	/* push */
	GetConst(COMMON_SLEEP, &symbol);
	get_internal_real_time_common(local, &value);
	SetArraySS(pos, 0, value);
	pushspecial_control(ptr, symbol, pos);
}

static int sleep_close_object(Execute ptr)
{
	addr pos;

	get_sleep_object(ptr, &pos);
	CloseHandle(SleepObjectStruct(pos)->hEvent);

	return 0;
}

static void time_sleep_object(Execute ptr, addr *ret)
{
	addr pos;
	get_sleep_object(ptr, &pos);
	GetArraySS(pos, 0, ret);
}

static int sleep_moment_common(Execute ptr, fixnum value)
{
	DWORD check;
	HANDLE handle;
	addr pos;

	if (value == 0)
		return 0;
	get_sleep_object(ptr, &pos);
	handle = SleepObjectStruct(pos)->hEvent;
	check = WaitForSingleObject(handle, (DWORD)value);
	if (check == WAIT_TIMEOUT)
		return 0;
	if (check == WAIT_OBJECT_0)
		return 1;

	/* error */
	Abort("WaitForSingleObject error.");
	return 0;
}

static int sleep_second_common(Execute ptr, fixnum value)
{
	return sleep_moment_common(ptr, (fixnum)(value * LISP_SLEEP_INTERVAL));
}

#else

static int sleep_close_object(Execute ptr)
{
	/* do nothing */
	return 0;
}
#endif

static int sleep_continue(Execute ptr)
{
	/* do nothing */
	return 0;
}

#if defined(LISP_POSIX) || defined(LISP_WINDOWS)
static int sleep_integer_common(Execute ptr, addr var)
{
	LocalRoot local;
	addr wait;
	fixnum value;

	local = ptr->local;
	fixnum_heap(&wait, LISP_SLEEP_FIXNUM);
	truncate2_common(local, &var, &wait, var, wait);
	while (plusp_integer(var)) {
		Return(sleep_second_common(ptr, LISP_SLEEP_FIXNUM));
		oneminus_integer_common(local, var, &var);
	}
	GetFixnum(wait, &value);
	return sleep_second_common(ptr, value);
}

static int sleep_execute_common_(Execute ptr, addr var)
{
	addr right;
	LocalRoot local;
	fixnum value;

	fixnum_heap(&right, LISP_SLEEP_INTERVAL);
	local = ptr->local;
	truncate2_common(local, &var, &right, var, right);
	Return(sleep_integer_common(ptr, var));
	GetFixnum(right, &value);
	return sleep_moment_common(ptr, value);
}


/*
 *  restart
 */
static void sleep_execute_diff(Execute ptr, addr var, addr *ret)
{
	addr now, diff;
	LocalRoot local;

	local = ptr->local;
	get_internal_real_time_common(local, &now);
	time_sleep_object(ptr, &diff);

	/* diff */
	minus_ii_real_common(local, now, diff, &now);
	minus_ii_real_common(local, var, now, &var);
	*ret = var;
}

#ifdef LISP_POSIX
static void sleep_signal_handler(int value)
{
	setatomic_sleep_object(Execute_Thread);
}

static int sleep_signal_restart_(Execute ptr, addr var)
{
	int check;

	if (signal(SIGINT, sleep_signal_handler) == SIG_ERR) {
		Abort("signal set error.");
		return 0;
	}
	check = sleep_execute_common_(ptr, var);
	if (signal(SIGINT, SIG_DFL) == SIG_ERR) {
		Abort("signal set default error.");
		return 0;
	}

	return check;
}
#endif

#ifdef LISP_WINDOWS
static int sleep_signal_restart_(Execute ptr, addr var)
{
	return sleep_execute_common_(ptr, var);
}
#endif

static int sleep_execute_restart_(Execute ptr, addr var, addr *ret)
{
	addr condition;
	LocalHold hold;

	if (! sleep_signal_restart_(ptr, var))
		return 0;

	/* diff */
	sleep_execute_diff(ptr, var, &var);
	hold = LocalHold_local_push(ptr, var);
	*ret = var;

	/* invoke-debugger */
	strvect_char_heap(&condition, "Break SIGINT");
	instance_simple_condition(&condition, condition, Nil);
	localhold_push(hold, condition);
	Return(invoke_debugger(ptr, condition));
	localhold_end(hold);

	return 0;
}

static void sleep_make_restart(addr *ret)
{
	static const char *message = "Return to sleep.";
	addr inst, pos;

	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&inst, pos);
	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_sleep_continue);
	setfunction_restart(inst, pos);
	setinteractive_restart(inst, Nil);
	strvect_char_heap(&pos, message);
	setreport_restart(inst, pos);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);
	*ret = inst;
}

static int sleep_break_restart_(Execute ptr, addr restart, addr var, addr *ret)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	push_sleep_object(ptr);
	setprotect_control(ptr, p_sleep_close_object, Nil);
	*ret = Nil;
	Return(restart1r_control(ptr, restart, sleep_execute_restart_, var, ret));
	localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

static int sleep_wait_common_(Execute ptr, addr var)
{
	addr restart;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	localhold_set(hold, 0, var);
	for (;;) {
		sleep_make_restart(&restart);
		localhold_set(hold, 1, restart);
		Return(sleep_break_restart_(ptr, restart, var, &var));
		localhold_set(hold, 0, var);
		if (var == Nil)
			break;
		if (minusp_integer(var))
			break;
	}
	localhold_end(hold);

	return 0;
}
#endif

#if defined(LISP_POSIX) || defined(LISP_WINDOWS)
static void sleep_value_integer(LocalRoot local, addr var, addr *ret)
{
	addr right;
	fixnum_heap(&right, LISP_SLEEP_INTERVAL);
	multi_ii_real_common(local, var, right, ret);
}

static void sleep_value_ratio(LocalRoot local, addr var, addr *ret)
{
	addr right;

	fixnum_heap(&right, LISP_SLEEP_INTERVAL);
	multi_rf_real_common(local, var, right, &var);
	truncate1_common(local, ret, &right, var);
}

static int sleep_value_single_float_(LocalRoot local, addr var, addr *ret)
{
	single_float value, moment;
	addr left, right;

	GetSingleFloat(var, &value);
	value = lisp_truncate1_s(value, &moment);
	moment = (single_float)(moment * LISP_SLEEP_INTERVAL_F);
	Return(bignum_single_float_local_(local, value, &left, NULL));
	Return(bignum_single_float_local_(local, moment, &right, NULL));
	fixnum_heap(&var, LISP_SLEEP_INTERVAL);
	multi_ii_real_common(local, left, var, &left);
	plus_ii_real_common(local, left, right, ret);

	return 0;
}

static int sleep_value_double_float_(LocalRoot local, addr var, addr *ret)
{
	double_float value, moment;
	addr left, right;

	GetDoubleFloat(var, &value);
	value = lisp_truncate1_d(value, &moment);
	moment = (double_float)(moment * LISP_SLEEP_INTERVAL_F);
	Return(bignum_double_float_local_(local, value, &left, NULL));
	Return(bignum_double_float_local_(local, moment, &right, NULL));
	fixnum_heap(&var, LISP_SLEEP_INTERVAL);
	multi_ii_real_common(local, left, var, &left);
	plus_ii_real_common(local, left, right, ret);

	return 0;
}

static int sleep_value_long_float_(LocalRoot local, addr var, addr *ret)
{
	long_float value, moment;
	addr left, right;

	GetLongFloat(var, &value);
	value = lisp_truncate1_l(value, &moment);
	moment = (long_float)(moment * LISP_SLEEP_INTERVAL_F);
	Return(bignum_long_float_local_(local, value, &left, NULL));
	Return(bignum_long_float_local_(local, moment, &right, NULL));
	fixnum_heap(&var, LISP_SLEEP_INTERVAL);
	multi_ii_real_common(local, left, var, &left);
	plus_ii_real_common(local, left, right, ret);

	return 0;
}

static int sleep_value_common_(LocalRoot local, addr var, addr *ret)
{
	switch (GetType(var)) {
		case LISPTYPE_FIXNUM:
		case LISPTYPE_BIGNUM:
			sleep_value_integer(local, var, ret);
			break;

		case LISPTYPE_RATIO:
			sleep_value_ratio(local, var, ret);
			break;

		case LISPTYPE_SINGLE_FLOAT:
			return sleep_value_single_float_(local, var, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return sleep_value_double_float_(local, var, ret);

		case LISPTYPE_LONG_FLOAT:
			return sleep_value_long_float_(local, var, ret);

		default:
			*ret = Nil;
			return fmte_("Invalid value type ~S.", var, NULL);
	}

	return 0;
}
#endif

_g int sleep_common_(Execute ptr, addr var)
{
#if defined(LISP_POSIX) || defined(LISP_WINDOWS)
	Return(sleep_value_common_(ptr->local, var, &var));
	return sleep_wait_common_(ptr, var);
#else
	return fmte_("This implementation is not support SLEEP function.", NULL);
#endif
}


/*
 *  initialize
 */
_g void init_environemnt_time(void)
{
	SetPointerType(empty, sleep_continue);
	SetPointerType(empty, sleep_close_object);
}

