#include "c99.h"
#include "random.h"
#include "random_float.h"

/*
 *  random float
 */
static char *strfloat_front(char *ptr, const char *data)
{
	for (;;) {
		*ptr = *data;
		if (*ptr == 0)
			break;
		data++;
		ptr++;
	}

	return ptr;
}

static char *strfloat_start(char *ptr)
{
	return strfloat_front(ptr, "0x0.");
}

static void strfloat_end(char *ptr)
{
	strfloat_front(ptr, "p0");
}


/*
 *  32bit float
 */
#define RANDOM_PRINTF32		8

static char *strfloat_32bit(char *ptr, uint32_t value)
{
	char buffer[32];
	snprintf(buffer, 32, "%08" PRIX32, value);
	return strfloat_front(ptr, buffer);
}


/* [32bit] (23+1) * 2 = 48 -> 64 -> 2 times */
#define RANDOM_FLOAT32_TIMES	2
#define RANDOM_FLOAT32_BUFFER	(RANDOM_PRINTF32*RANDOM_FLOAT32_TIMES)
#define RANDOM_FLOAT32_DATA		(4+2+1 + RANDOM_FLOAT32_BUFFER)
_g float float_random_32bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	char data[RANDOM_FLOAT32_DATA];
	char *ptr;
	unsigned i;
	uint32_t bit;
	float value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_FLOAT32_TIMES; i++) {
		bit = random_number_32bit(state);
		ptr = strfloat_32bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%A", &value);

	return value;
}


/* [32bit] (52+1) * 2 = 106 -> 128 -> 4 times */
#define RANDOM_DOUBLE32_TIMES	4
#define RANDOM_DOUBLE32_BUFFER	(RANDOM_PRINTF32*RANDOM_DOUBLE32_TIMES)
#define RANDOM_DOUBLE32_DATA	(4+2+1 + RANDOM_DOUBLE32_BUFFER)
_g double double_random_32bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_DOUBLE32_DATA];
	char *ptr;
	uint32_t bit;
	double value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_DOUBLE32_TIMES; i++) {
		bit = random_number_32bit(state);
		ptr = strfloat_32bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%lA", &value);

	return value;
}


/*
 *  IEEE754 binary128
 *    (112+1) * 2 = 226 -> 256 -> 8 times
 *  Intel long-double
 *     (64+0) * 2 = 128 -> 128 -> 4 times
 */
#define RANDOM_LONG32_TIMES		8
#define RANDOM_LONG32_BUFFER	(RANDOM_PRINTF32*RANDOM_LONG32_TIMES)
#define RANDOM_LONG32_DATA		(4+2+1 + RANDOM_LONG32_BUFFER)
_g long double long_random_32bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_LONG32_DATA];
	char *ptr;
	uint32_t bit;
	long double value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_LONG32_TIMES; i++) {
		bit = random_number_32bit(state);
		ptr = strfloat_32bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%LA", &value);

	return value;
}


/*
 *  64bit float
 */
#define RANDOM_PRINTF64		16

static char *strfloat_64bit(char *ptr, uint64_t value)
{
	char buffer[32];
	snprintf(buffer, 32, "%016" PRIX64, value);
	return strfloat_front(ptr, buffer);
}


/* [64bit] (23+1) * 2 = 48 -> 64 -> 1 times */
#define RANDOM_FLOAT64_TIMES	1
#define RANDOM_FLOAT64_BUFFER	(RANDOM_PRINTF64*RANDOM_FLOAT64_TIMES)
#define RANDOM_FLOAT64_DATA		(4+2+1 + RANDOM_FLOAT64_BUFFER)
_g float float_random_64bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	char data[RANDOM_FLOAT64_DATA];
	char *ptr;
	unsigned i;
	uint64_t bit;
	float value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_FLOAT64_TIMES; i++) {
		bit = random_number_64bit(state);
		ptr = strfloat_64bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%A", &value);

	return value;
}


/* [64bit] (52+1) * 2 = 106 -> 128 -> 2 times */
#define RANDOM_DOUBLE64_TIMES	2
#define RANDOM_DOUBLE64_BUFFER	(RANDOM_PRINTF64*RANDOM_DOUBLE64_TIMES)
#define RANDOM_DOUBLE64_DATA	(4+2+1 + RANDOM_DOUBLE64_BUFFER)
_g double double_random_64bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_DOUBLE64_DATA];
	char *ptr;
	uint64_t bit;
	double value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_DOUBLE64_TIMES; i++) {
		bit = random_number_64bit(state);
		ptr = strfloat_64bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%lA", &value);

	return value;
}


/*
 *  IEEE754 binary128
 *    (112+1) * 2 = 226 -> 256 -> 4 times
 *  Intel long-double
 *     (64+0) * 2 = 128 -> 128 -> 2 times
 */
#define RANDOM_LONG64_TIMES		4
#define RANDOM_LONG64_BUFFER	(RANDOM_PRINTF64*RANDOM_LONG64_TIMES)
#define RANDOM_LONG64_DATA		(4+2+1 + RANDOM_LONG64_BUFFER)
_g long double long_random_64bit(struct random_state *state)
{
	/* "0x0.FFFFFFFFp0": 4+2+1 + size*times */
	unsigned i;
	char data[RANDOM_LONG64_DATA];
	char *ptr;
	uint64_t bit;
	long double value;

	ptr = strfloat_start(data);
	for (i = 0; i < RANDOM_LONG64_TIMES; i++) {
		bit = random_number_64bit(state);
		ptr = strfloat_64bit(ptr, bit);
	}
	strfloat_end(ptr);
	sscanc(data, "%LA", &value);

	return value;
}

