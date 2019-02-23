/*
 *  md5encode.h
 *
 *  [RFC1321] The MD5 Message-Digest Algorithm
 *  https://www.ietf.org/rfc/rfc1321.txt
 *
 *  Usage
 *    1. String
 *      uint8_t result[MD5ENCODE_SIZE];
 *      string_md5encode("Hello md5encode", result);
 *
 *    2. Buffer
 *      uint8_t result[MD5ENCODE_SIZE];
 *      sequence_md5encode("Hello md5encode", 15, result);
 *
 *    3. Buffers
 *      uint8_t result[MD5ENCODE_SIZE];
 *      struct md5encode md5;
 *      clear_md5encode(&md5);
 *      read_md5encode(&md5, "Hello", 5);
 *      read_md5encode(&md5, " ", 1);
 *      read_md5encode(&md5, "md5encode", 9);
 *      calc_md5encode(&md5, result);
 */
#ifndef __MD5ENCODE_HEADER__
#define __MD5ENCODE_HEADER__

#include <stdio.h>
#include <stdint.h>

#define MD5ENCODE_SIZE 16

struct md5encode {
	uint32_t a, b, c, d, x[MD5ENCODE_SIZE];
	size_t size;
	int pos;
};

void clear_md5encode(struct md5encode *);
void read_md5encode(struct md5encode *, const void *, size_t);
void calc_md5encode(struct md5encode *, void *);
void sequence_md5encode(const void *, size_t, void *);
void string_md5encode(const char *, void *);

#endif

