#include <stdio.h>
#include "bignum_object.h"
#include "boole.h"
#include "bytespec.h"
#include "condition.h"
#include "constant.h"
#include "define.h"
#include "execute.h"
#include "heap.h"
#include "integer.h"
#include "md5encode.h"
#include "random.h"
#include "random_state.h"
#include "thread.h"
#include "symbol.h"

#define zeroset(p,s) memset((void *)(p), 0, (size_t)(s))
#define readmd5(m,p,s) read_md5encode((m), (const void *)(p), (size_t)(s))

#define PtrBodyRandomState(x)	((struct random_state *)PtrBodyB2(x))
#define RANDOM_DEVICE "/dev/urandom"
#define RANDOM_DEVICE_SIZE 256

static int InitRandomState = 0;
static mutexlite RandomStateMutex;

struct random_state *struct_random_state(addr pos)
{
	CheckType(pos, LISPTYPE_RANDOM_STATE);
	return PtrBodyRandomState(pos);
}

#if defined LISP_POSIX
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>
#include "arch.h"

/* hostname */
static size_t gethostname_buffer(char *ptr, size_t size)
{
	int result;

	result = gethostname(ptr, size);
	if (result < 0) {
		size--;
		ptr[size] = '\0';
	}
	else {
		size = strlen(ptr);
	}

	return size;
}

#define GETHOSTNAME_SIZE 256
static void read_hostname_seed(struct md5encode *md5)
{
	volatile char buffer[GETHOSTNAME_SIZE];
	size_t size;

	size = gethostname_buffer((char *)buffer, GETHOSTNAME_SIZE);
	readmd5(md5, buffer, size);
	zeroset(buffer, GETHOSTNAME_SIZE);
}

static int read_device_urandom(struct md5encode *md5)
{
	volatile unsigned char buffer[RANDOM_DEVICE_SIZE];
	int file, check;
	size_t size;

	file = open(RANDOM_DEVICE, O_RDONLY | O_NONBLOCK);
	if (file < 0) {
		Debug("file " RANDOM_DEVICE " is not exist.");
		return 1;
	}
	check = readforce_posix(file, (void *)buffer, RANDOM_DEVICE_SIZE, &size);
	if (check) {
		close(file);
		Debug("read error");
		return -1;
	}
	readmd5(md5, buffer, size);
	zeroset(buffer, RANDOM_DEVICE_SIZE);
	if (close(file) < 0) {
		Debug("close error");
		return -1;
	}

	return 0;
}

static void random_seed_os(struct md5encode *md5)
{
	volatile int value;
	volatile pid_t pid;
	volatile struct timeval now;

	/* hostname */
	read_hostname_seed(md5);
	/* time */
	gettimeofday((struct timeval *)&now, NULL);
	readmd5(md5, &now, sizeof(now));
	zeroset(&now, sizeof(now));
	/* process id */
	pid = getpid();
	readmd5(md5, &pid, sizeof(pid));
	zeroset(&pid, sizeof(pid));
	/* thread id */
	value = (int)Index_Thread;
	readmd5(md5, &value, sizeof(value));
	value = 0;
	/* read /dev/urandom */
	if (read_device_urandom(md5))
		Abort("Cannot read " RANDOM_DEVICE ".");
}

#elif defined LISP_WINDOWS
#include <windows.h>
#include <ntsecapi.h>
#include "c99.h"

#define GETHOSTNAME_SIZE 256
#define gethostwin(p,s) GetComputerNameExA(ComputerNameDnsFullyQualified,(p),(s))
static int read_hostname_seed(struct md5encode *md5)
{
	volatile char buffer[GETHOSTNAME_SIZE];
	BOOL result;
	DWORD size;
	char *ptr;
	LocalRoot local;
	LocalStack stack;

	size = GETHOSTNAME_SIZE;
	result = gethostwin((LPSTR)buffer, &size);
	if (result == 0) {
		local = Local_Thread;
		push_local(local, &stack);
		ptr = (char *)lowlevel_local(local, size + 1UL);
		result = gethostwin(ptr, &size);
		readmd5(md5, ptr, size);
		zeroset(buffer, GETHOSTNAME_SIZE);
		rollback_local(local, stack);
		if (result == 0) {
			Debug("GetComputerName error");
			return 1;
		}
	}
	else {
		ptr = (char *)buffer;
		readmd5(md5, ptr, size);
		zeroset(buffer, GETHOSTNAME_SIZE);
	}

	return 0;
}

#define RTLGENRANDOM_SIZE 256
static BOOLEAN rtlgenrandom(PVOID buffer, ULONG length)
{
	typedef BOOLEAN (WINAPI *apicalltype)(PVOID, ULONG);
	HMODULE hModule;
	BOOLEAN result;
	apicalltype call;

	hModule = LoadLibraryA("Advapi32.dll");
	if (hModule == NULL) {
		Debug("LoadLibrary Advapi32 error");
		return FALSE;
	}
	call = (apicalltype)GetProcAddress(hModule, "SystemFunction036");
	if (call == NULL) {
		Debug("GetProcAddress SystemFunction036 error");
		FreeLibrary(hModule);
		return FALSE;
	}
	result = (*call)(buffer, length);
	FreeLibrary(hModule);

	return result;
}

static int read_windows_random(struct md5encode *md5)
{
	volatile unsigned char buffer[RTLGENRANDOM_SIZE];
	BOOLEAN result;

	result = rtlgenrandom((PVOID)buffer, RTLGENRANDOM_SIZE);
	if (result == FALSE) {
		Debug("RtlGenRandom error");
		return 1;
	}
	readmd5(md5, buffer, RTLGENRANDOM_SIZE);
	SecureZeroMemory((PVOID)buffer, RTLGENRANDOM_SIZE);

	return 0;
}

static void random_seed_os(struct md5encode *md5)
{
	volatile DWORD value;
	volatile SYSTEMTIME time;

	/* hostname */
	if (read_hostname_seed(md5))
		Abort("Cannot get hostname.");
	/* time */
	GetSystemTime((LPSYSTEMTIME)&time);
	readmd5(md5, &time, sizeof(time));
	zeroset(&time, sizeof(time));
	/* process id */
	value = GetCurrentProcessId();
	readmd5(md5, &value, sizeof(value));
	value = 0;
	/* thread id */
	value = GetCurrentThreadId();
	readmd5(md5, &value, sizeof(value));
	value = 0;
	/* RtlGenRandom */
	if (read_windows_random(md5))
		Abort("Cannot get random number.");
}
#else
#include <time.h>

static int read_device_urandom(struct md5encode *md5)
{
	volatile unsigned char buffer[RANDOM_DEVICE_SIZE];
	FILE *file;
	size_t size;

	file = fopen(RANDOM_DEVICE, "rb");
	if (file == NULL) {
		/* Device is not exist, OK. */
		return 1;
	}
	size = fread((void *)buffer, RANDOM_DEVICE_SIZE, 1, file);
	if (size == 0) {
		fclose(file);
		Debug("fread error");
		return -1;
	}
	readmd5(md5, buffer, size);
	zeroset(buffer, RANDOM_DEVICE_SIZE);
	if (fclose(file) < 0) {
		Debug("fclose error");
		return -1;
	}

	return 0;
}

static void random_seed_os(struct md5encode *md5)
{
	volatile time_t now;

	/* time */
	time((time_t *)&now);
	readmd5(md5, &now, sizeof(now));
	zeroset(&now, sizeof(now));
	/* read /dev/urandom */
	if (read_device_urandom(md5) < 0)
		Abort("Cannot read random device.");
}
#endif


/*
 *  interface
 */
int init_random_state(void)
{
	if (InitRandomState) {
		Debug("InitRandomState error.");
		return 1;
	}
	if (lispd_make_mutexlite(&RandomStateMutex)) {
		Debug("lispd_make_mutexlite error.");
		return 1;
	}
	InitRandomState = 1;

	return 0;
}

void free_random_state(void)
{
	if (InitRandomState) {
		lispd_destroy_mutexlite(&RandomStateMutex);
		InitRandomState = 0;
	}
}

static void make_random_seed(struct md5encode *md5)
{
	volatile const void *ptr;
	void (*call)(struct md5encode *);
	static volatile int counter = 0;

	/* environment */
	random_seed_os(md5);
	/* counter */
	lispd_lock_mutexlite(&RandomStateMutex);
	counter++;
	lispd_unlock_mutexlite(&RandomStateMutex);
	readmd5(md5, &counter, sizeof(counter));
	/* function pointer */
	call = make_random_seed;
	memcpy((void *)&ptr, (const void *)&call, sizeof(ptr));
	readmd5(md5, &ptr, sizeof(ptr));
	ptr = NULL;
}

void random_state_alloc(LocalRoot local, addr *ret)
{
	addr pos;

	Check(0xFF <= sizeoft(struct random_state), "size error");
	alloc_body2(local, &pos, LISPTYPE_RANDOM_STATE, sizeoft(struct random_state));
	memset(struct_random_state(pos), 0, sizeoft(struct random_state));
	*ret = pos;
}
void random_state_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	random_state_alloc(local, ret);
}
void random_state_heap(addr *ret)
{
	random_state_alloc(NULL, ret);
}

static void make_randomly_seed(struct random_state *ptr)
{
	volatile uint8_t result[MD5ENCODE_SIZE];
	struct md5encode md5;

	clear_md5encode(&md5);
	make_random_seed(&md5);
	calc_md5encode(&md5, (void *)result);
	clear_md5encode(&md5);
	memcpy(ptr, (const void *)result, sizeoft(result));
	zeroset(result, MD5ENCODE_SIZE);
}

void copy_random_state(addr left, addr right)
{
	struct random_state *ptr1, *ptr2;

	CheckType2(left, LISPTYPE_RANDOM_STATE, "type left error");
	CheckType2(right, LISPTYPE_RANDOM_STATE, "type right error");
	ptr1 = struct_random_state(left);
	ptr2 = struct_random_state(right);
	memcpy(ptr1, ptr2, sizeoft(struct random_state));
}

void randomly_random_state(addr left)
{
	struct random_state *ptr;

	CheckType(left, LISPTYPE_RANDOM_STATE);
	Check(sizeof(struct random_state) != MD5ENCODE_SIZE, "size error");
	ptr = struct_random_state(left);
	make_randomly_seed(ptr);
}

int constant_random_state_(Execute ptr, addr left)
{
	addr right;

	Check(ptr == NULL, "execute error");
	GetConst(SPECIAL_RANDOM_STATE, &right);
	Return(getspecialcheck_local_(ptr, right, &right));
	if (GetType(right) != LISPTYPE_RANDOM_STATE)
		return TypeError_(right, RANDOM_STATE);
	copy_random_state(left, right);

	return 0;
}

int make_random_state_heap_(Execute ptr, addr *ret, addr state)
{
	addr pos;

	random_state_heap(&pos);
	if (state == T) {
		randomly_random_state(pos);
	}
	else if (state == Nil) {
		Return(constant_random_state_(ptr, pos));
	}
	else {
		if (GetType(state) != LISPTYPE_RANDOM_STATE) {
			*ret = Nil;
			return TypeError_(state, RANDOM_STATE);
		}
		copy_random_state(pos, state);
	}

	return Result(ret, pos);
}

#ifdef LISP_64BIT
#define RANDOM_STATE_SIZE	2
#define RANDOM_STATE_UNION	u64
#else
#define RANDOM_STATE_SIZE	4
#define RANDOM_STATE_UNION	u32
#endif
void make_bignum_random_state_alloc(LocalRoot local, addr pos, addr *ret)
{
	int i;
	struct random_state *state;

	CheckType(pos, LISPTYPE_RANDOM_STATE);
	state = struct_random_state(pos);
	alloc_bignum(local, &pos, RANDOM_STATE_SIZE);
	for (i = 0; i < RANDOM_STATE_SIZE; i++)
		setfixed_bignum(pos, i, state->seed.RANDOM_STATE_UNION[i]);
	*ret = pos;
}

void make_bignum_random_state_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	make_bignum_random_state_alloc(local, pos, ret);
}

void make_bignum_random_state_heap(addr pos, addr *ret)
{
	make_bignum_random_state_alloc(NULL, pos, ret);
}

int equal_random_state_addr(addr left, addr right)
{
	struct random_state *state1, *state2;

	CheckType(left, LISPTYPE_RANDOM_STATE);
	CheckType(right, LISPTYPE_RANDOM_STATE);
	state1 = struct_random_state(left);
	state2 = struct_random_state(right);

	return random_state_equal(state1, state2);
}


/*
 *  sysctl
 */
int random_state_integer_(addr pos, addr *ret)
{
	addr value;

	if (GetType(pos) != LISPTYPE_RANDOM_STATE) {
		*ret = Nil;
		return TypeError_(pos, RANDOM_STATE);
	}

	make_bignum_random_state_heap(pos, &value);
	return integer_result_heap_(value, ret);
}

int random_state_make_(LocalRoot local, addr pos, addr *ret)
{
	addr spec, ignore, state;
	bigtype *data;
	struct random_state *str;
	size_t size, i;

	if (! integerp(pos)) {
		*ret = Nil;
		return TypeError_(pos, INTEGER);
	}

	/* (ldb (byte 128 0) pos) */
	bytespec_heap(&spec, 128UL, 0UL);
	Return(ldb_common_(local, &pos, spec, pos));

	/* result */
	if (fixnump(pos))
		bignum_fixnum_heap(&pos, pos);
	GetSizeBignum(pos, &size);
	GetRootDataBignum(pos, &ignore, &data);
	random_state_heap(&state);
	str = struct_random_state(state);
	for (i = 0; i < size; i++) {
#ifdef LISP_64BIT
		str->seed.u64[i] = (uint64_t)data[i];
#else
		str->seed.u32[i] = (uint32_t)data[i];
#endif
	}

	return Result(ret, state);
}

