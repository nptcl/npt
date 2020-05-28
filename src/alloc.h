#ifndef __ALLOC_HEADER__
#define __ALLOC_HEADER__

#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>

/*
 *  Constant
 */
#define PtrSize					(sizeof(void *))
#define IdxSize					(sizeof(size_t))
#define PtrSizet				((size_t)sizeof(void *))
#define IdxSizet				((size_t)sizeof(size_t))


/*
 *  Low level memory access
 */
#define ClearJmpBuf(x)			memset((x), 0, sizeoft(jmp_buf))
#define CopyJmpBuf(x,y)			memcpy((x), (y), sizeoft(jmp_buf))
#define WtType(p,v)				memcpy((p), &(v), sizeoft(v))
#define RdType(p,v)				memcpy(&(v), (p), sizeoft(v))
#define WtTypePtr(p,v)			memcpy((p), (v), sizeoft(*v))
#define RdTypePtr(p,v)			memcpy((v), (p), sizeoft(*v))
#define WtByte(p,c)				(*(addr)(p) = (c))
#define RdByte(p)				(*(addr)(p))


/*
 *  Alignment
 */
#define PointerMask             (~(uintptr_t)0)
#define Align8Mask				(PointerMask ^ 7UL)
#define Align8Cut(p)			(Align8Mask & (uintptr_t)(p))
#define Align8Out(p)            (((uintptr_t)(p)) & 7UL)
#define Align8Space(p)			((8UL - Align8Out(p)) % 8UL)
#define Align8Inplace(p)		((void *)(((uintptr_t)(p)) + Align8Space(p)))
#define Align8Front(x,y) { \
	uintptr_t __align = Align8Out(x); \
	if (__align) { \
		*(void **)(y) = (void *)(((uintptr_t)(x)) + (8UL - __align)); \
	} else { \
		*(void **)(y) = (void *)(x); \
	} \
}

#define IndexMask               (~(size_t)0)
#define Index8Mask				(IndexMask ^ 7UL)
#define AlignSize8Out(x)		(((size_t)(x)) & 7UL)
#define AlignSize8Cut(p)		(Index8Mask & (size_t)(p))
#define AlignSize8Space(x)		((8UL - AlignSize8Out(x)) % 8UL)
#define AlignSize8Inplace(x)	(((size_t)(x)) + AlignSize8Space(x))
#define AlignSize8Front(x,y) { \
	size_t __align = AlignSize8Out(x); \
	if (__align) { \
		*(y) = (x) + (8UL - __align); \
	} else { \
		*(y) = (x); \
	} \
}

#define CheckAlign8(x,y)		Check(Align8Out(x), (y))
#define CheckAlignSize8(x,y)	Check(AlignSize8Out(x), (y))


/*
 *  allocate
 */
#define sizeoft(n)              ((size_t)sizeof(n))
#define sizeofm(n,s)            (((size_t)(s)) * sizeoft(n))
#define malloctype(n)           ((n *)malloc(sizeoft(n)))
#define mallocsize(n,s)         ((n *)malloc(sizeofm(n, (s))))
#define reallocsize(p,n,s)      ((n *)realloc((p), sizeofm(n, (s))))
#define clearmemory(n,s)		memset((void *)(n), 0, (size_t)(s))
#define clearpoint(n)			clearmemory((n), sizeof(*n))
#define cleartype(n)			clearmemory(&(n), sizeof(n))
#define clearsize(n,s)			clearmemory((n), sizeofm(n[0], (s)))
#define aamemory(n,s)			memset((void *)(n), 0xAA, (size_t)(s))
#define aatype(n)				aamemory(&(n), sizeof(n))


/*
 *  Bit shift
 */
#define GetShiftValue(x,i,s)    ((x) & ((s) << (i)))
#define SetShiftValue(x,i,v,s,t) { \
	if (v) (x) |= (t)((s) << (i)); \
	else (x) &= ~(t)((s) << (i)); \
}
#define GetBitByte(x,i)        GetShiftValue(x,i,1U)
#define SetBitByte(x,i,v)      SetShiftValue(x,i,v,1U,byte)


/*
 *  compare
 */
#define EqualPointer(x,y)			(((void *)(x)) == ((void *)(y)))
#define NotEqualPointer(x,y)		(((void *)(x)) != ((void *)(y)))
#define LessPointer(x,y)			(((void *)(x)) < ((void *)(y)))
#define GreaterPointer(x,y)			(((void *)(x)) > ((void *)(y)))
#define LessEqualPointer(x,y)		(((void *)(x)) <= ((void *)(y)))
#define GreaterEqualPointer(x,y)	(((void *)(x)) >= ((void *)(y)))
#define ComparePointer(x,y)			(((intptr_t)(x)) - ((intptr_t)(y)))

#endif

