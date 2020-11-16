/*
 *  xorshift+
 */
#include "random.h"
#include "md5encode.h"
#include <string.h>
#include <inttypes.h>

#define MASK32BIT 0xFFFFFFFFUL
#define MASK64BIT 0xFFFFFFFFFFFFFFFFULL

static uint32_t xorshift128_32bit(uint32_t *x, uint32_t *y, uint32_t *z, uint32_t *w)
{
	/*
	 *  Xorshift RNGs
	 *  George Marsaglia, The Florida State University,
	 *  Journal of Statistical Software, 2003.
	 *  http://www.jstatsoft.org/v08/i14/paper
	 */
	uint32_t v;

	v = (*x ^ (*x << 11/*a*/));
	*x = *y;
	*y = *z;
	*z = *w;
	*w = (v ^ (v >> 8/*b*/)) ^ (*w ^ (*w >> 19/*c*/));

	return *w;
}

static uint64_t xorshift128plus_64bit(uint64_t *s0, uint64_t *s1)
{
	/*
	 *  Further scramblings of Marsaglia's xorshift generators
	 *  Sebastiano Vigna, Universit`a degli Studi di Milano, Italy,
	 *  arXiv:1404.0390v3 [cs.DS] 23 May 2016.
	 *  https://arxiv.org/abs/1404.0390
	 *  http://vigna.di.unimi.it/ftp/papers/xorshiftplus.pdf
	 *
	 *  Table I.
	 *  a23-b17-c26: s34-r30+64-w61 (failures)
	 *  a23-b18-c5 : s38-r20+70-w65 (weight)
	 */
	uint64_t x, y, z;

	x = *s0;
	y = *s1;
	z = x + y;
	*s0 = y;
	x ^= x << 23/*a*/;
	*s1 = x ^ y ^ (x >> 18/*b*/) ^ (y >> 5/*c*/);

	return z;
}

/* random */
uint32_t random_number_32bit(struct random_state *state)
{
	return xorshift128_32bit(
			&state->seed.u32[0], &state->seed.u32[1],
			&state->seed.u32[2], &state->seed.u32[3]);
}

uint64_t random_number_64bit(struct random_state *state)
{
	return xorshift128plus_64bit(&state->seed.u64[0], &state->seed.u64[1]);
}

/* 0 ... value */
uint32_t random_equal_32bit(struct random_state *state, uint32_t value)
{
	int shift;
	uint32_t check, result;

	/* shift */
	if (value == 0UL) return 0UL;
	check = (value >> 1UL);
	for (shift = 1; check; shift++)
		check >>= 1UL;

	/* generate */
	check = (32 <= shift)? MASK32BIT: (1UL << shift) - 1UL;
	do {
		result = check & random_number_32bit(state);
	} while (value < result);

	return result;
}

uint64_t random_equal_64bit(struct random_state *state, uint64_t value)
{
	int shift;
	uint64_t check, result;

	/* shift */
	if (value == 0ULL) return 0ULL;
	check = (value >> 1ULL);
	for (shift = 1; check; shift++)
		check >>= 1ULL;

	/* generate */
	check = (64 <= shift)? MASK64BIT: (1ULL << shift) - 1ULL;
	do {
		result = check & random_number_64bit(state);
	} while (value < result);

	return result;
}

/* 0 ... value-1 */
uint32_t random_less_32bit(struct random_state *state, uint32_t value)
{
	if (value <= 1UL) return 0UL;
	return random_equal_32bit(state, value - 1UL);
}

uint64_t random_less_64bit(struct random_state *state, uint64_t value)
{
	if (value <= 1UL) return 0UL;
	return random_equal_64bit(state, value - 1UL);
}

/* seed */
void random_seed_buffer(struct random_state *state, const void *ptr, size_t size)
{
	sequence_md5encode(ptr, size, state->seed.u32);
}

void random_seed_string(struct random_state *state, const char *word)
{
	sequence_md5encode(word, strlen(word), state->seed.u32);
}

/* check */
int random_state_equal(struct random_state *a, struct random_state *b)
{
	return memcmp(&(a->seed.u32), &(b->seed.u32),
			(size_t)(sizeof(uint32_t) * 4)) == 0;
}

