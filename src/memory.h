#ifndef __MEMORY_HEADER__
#define __MEMORY_HEADER__

#include <string.h>
#include "alloc.h"
#include "build.h"
#include "thread.h"
#include "typedef.h"
#include "object.h"

#define LISPCLASS_ConsLength      8
#define LISPCLASS_SymbolLength    8
#define LISPCLASS_Size1Length     8
#define LISPCLASS_Size2Length     6
#define LISPCLASS_Size3Length     13
#define LISPCLASS_Length (1 + 1 + LISPCLASS_ConsLength + LISPCLASS_SymbolLength + \
		LISPCLASS_Size1Length + LISPCLASS_Size2Length + LISPCLASS_Size3Length)


/****************************************************************************
 *  Object
 *     [T S C U] [v v v v] ...
 *     (Type Status - -)
 *
 *  Value
 *    PtrValue1A: [T S C U][v - - -]
 *    PtrValue1B: [T S C U][- v - -]
 *    PtrValue2A: [T S C U][v v - -]
 *    PtrValue2B: [T S C U][- - v v]
 *    PtrValue4A: [T S C U][v4-----]
 *    PtrValue4B: [T S C U][- - - -] [v4-----]
 *    PtrValue4C: [T S C U][- - - -] [- - - -][v4-----]
 *    PtrValue4D: [T S C U][- - - -] [- - - -][- - - -] [v4-----]
 *    PtrValue8A: [T S C U][- - - -] [v8--------------]
 *    PtrValue8B: [T S C U][- - - -] [- - - -][- - - -] [v8--------------]
 *    PtrValue8C: [T S C U][- - - -] [- - - -][- - - -] [- - - -][- - - -] [v8]
 *
 *  64bit
 *            A4: [T S C U][a a a a] [s s s s][s s s s]
 *            B4: [T S C U][b b b b] [s s s s][s s s s]
 *            AB: [T S C U][a a b b] [s s s s][s s s s]
 *            A8: [T S C U][- - - -] [s s s s][s s s s] [a a a a][a a a a]
 *            B8: [T S C U][- - - -] [s s s s][s s s s] [b b b b][b b b b]
 *  32bit
 *            A4: [T S C U][a a a a] [- - - -][s s s s]
 *            B4: [T S C U][b b b b] [- - - -][s s s s]
 *            AB: [T S C U][a a b b] [- - - -][s s s s]
 *
 *  ---
 *            SS: [T S C U][a b s s]
 *            A2: [T S C U][a a s s]
 *            B2: [T S C U][b b s s]
 ****************************************************************************/

__extern int LISPCLASS_Array[0x0100];

enum LISPSIZE {
	LISPSIZE_ARRAY2      = 0,
	LISPSIZE_ARRAY4      = 1,
	LISPSIZE_ARRAY8      = 2,
	LISPSIZE_SMALLSIZE   = 3,
	LISPSIZE_ARRAYBODY   = 4,
	LISPSIZE_BODY2       = 5,
	LISPSIZE_BODY4       = 6,
	LISPSIZE_BODY8       = 7,
	LISPSIZE_SIZE
};

enum LISPSTATUS {
	LISPSTATUS_SIZE1     = 0,
	LISPSTATUS_SIZE2     = 1,
	LISPSTATUS_SIZE3     = 2,
	LISPSTATUS_DYNAMIC   = 3,   /* local object */
	LISPSTATUS_READONLY  = 4,   /* can't write */
	LISPSTATUS_SYSTEM    = 5,   /* can't gc */
	LISPSTATUS_FIXED     = 6,   /* can't move */
	LISPSTATUS_GC        = 7,
	LISPSTATUS_SIZE
};

enum LISPCHECK {
	LISPCHECK_SYMBOL	= 0,
	LISPCHECK_LIST		= 1,
	LISPCHECK_ARRAY		= 2,
	LISPCHECK_BODY		= 3,
	LISPCHECK_ARRAYBODY	= 4,
	LISPCHECK_SIZE2		= 5,
	LISPCHECK_SIZE4		= 6,
	LISPCHECK_SIZE8		= 7,
	LISPCHECK_SIZE
};

enum LISPCLASS {
	LISPCLASS_Cons1 = 0, /* cons */
	LISPCLASS_Cons2,
	LISPCLASS_Cons3,
	LISPCLASS_Cons4,
	LISPCLASS_Cons5,
	LISPCLASS_Cons6,
	LISPCLASS_Cons7,
	LISPCLASS_Cons8,
	LISPCLASS_Symbol1,	/* symbol */
	LISPCLASS_Symbol2,
	LISPCLASS_Symbol3,
	LISPCLASS_Symbol4,
	LISPCLASS_Symbol5,
	LISPCLASS_Symbol6,
	LISPCLASS_Symbol7,
	LISPCLASS_Symbol8,
	LISPCLASS_Size11,	/* 16 [size1] */
	LISPCLASS_Size12,	/* 32 */
	LISPCLASS_Size13,	/* 48 */
	LISPCLASS_Size14,	/* 64 */
	LISPCLASS_Size15,	/* 80 */
	LISPCLASS_Size16,	/* 96 */
	LISPCLASS_Size17,	/* 112 */
	LISPCLASS_Size18,	/* 128 */
	LISPCLASS_Size21,	/* 192 [size2] */
	LISPCLASS_Size22,	/* 256 */
	LISPCLASS_Size23,	/* 320 */
	LISPCLASS_Size24,	/* 384 */
	LISPCLASS_Size25,	/* 448 */
	LISPCLASS_Size26,	/* 512 */
	LISPCLASS_Size31,	/* 768 [size3] */
	LISPCLASS_Size32,	/* 1024 */
	LISPCLASS_Size33,	/* 1280 */
	LISPCLASS_Size34,	/* 1536 */
	LISPCLASS_Size35,	/* 1792 */
	LISPCLASS_Size36,	/* 2048 */
	LISPCLASS_Size37,	/* 2304 */
	LISPCLASS_Size38,	/* 2560 */
	LISPCLASS_Size39,	/* 2816 */
	LISPCLASS_Size3A,	/* 3072 */
	LISPCLASS_Size3B,	/* 3328 */
	LISPCLASS_Size3C,	/* 3584 */
	LISPCLASS_Size3D,	/* 3840 */
	LISPCLASS_SizeK,	/* 4K, 8K, ... [Large] */
	LISPCLASS_SizeM,	/* 4M, 8M, ... [Huge] */
	LISPCLASS_SIZE
};


/****************************************************************************
 *  function
 ****************************************************************************/
#define ConsLength              (8UL + PtrSize * 2UL)
#define SymbolLength            (8UL + PtrSize * SYMBOL_INDEX_SIZE)

/* Header */
#define GetType(x)              ((enum LISPTYPE)(RdByte(x)))
#define SetType(x,y)            (WtByte((x), (y)))
#define PtrStatus(x)            (((addr)(x)) + 1UL)
#define GetStatus(x)            (RdByte(PtrStatus(x)))
#define SetStatus(x,y)          (WtByte(PtrStatus(x), (y)))
#define PtrCheck(x)				(((addr)(x)) + 2UL)
#define GetCheck(x)				(RdByte(PtrCheck(x)))
#define SetCheck(x,y)			(WtByte(PtrCheck(x), (y)))
#define PtrUser(x)				(((addr)(x)) + 3UL)
#define GetUser(x)				(RdByte(PtrUser(x)))
#define SetUser(x,y)			(WtByte(PtrUser(x), (y)))

#define BitStatusSize(x)        ((x) & 0x07UL)
#define BitStatusDynamic(x)     GetBitByte((x), LISPSTATUS_DYNAMIC)
#define BitStatusReadOnly(x)    GetBitByte((x), LISPSTATUS_READONLY)
#define BitStatusSystem(x)      GetBitByte((x), LISPSTATUS_SYSTEM)
#define BitStatusFixed(x)       GetBitByte((x), LISPSTATUS_FIXED)
#define BitStatusGc(x)          GetBitByte((x), LISPSTATUS_GC)

#define GetStatusSize(x)        ((enum LISPSIZE)(BitStatusSize(GetStatus(x))))
#define GetStatusDynamic(x)     (BitStatusDynamic(GetStatus(x)))
#define GetStatusReadOnly(x)    (BitStatusReadOnly(GetStatus(x)))
#define GetStatusSystem(x)      (BitStatusSystem(GetStatus(x)))
#define GetStatusFixed(x)       (BitStatusFixed(GetStatus(x)))
#define GetStatusGc(x)          (BitStatusGc(GetStatus(x)))
#define GetStatusValue(x,i)     GetBitByte(*PtrStatus(x),(i))
#define SetStatusValue(x,i,v)   SetBitByte(*PtrStatus(x),(i),(v))
#define SetStatusSize(x,y,z)	SetStatus((x), ((y)|(1U<<(z))))
#define SetStatusReadOnly(x)	SetStatusValue(x, LISPSTATUS_READONLY, 1)
#define ResetStatusReadOnly(x)	SetStatusValue(x, LISPSTATUS_READONLY, 0)

#define GetCheckSymbol(x)		GetBitByte(GetCheck(x), LISPCHECK_SYMBOL)
#define GetCheckList(x)			GetBitByte(GetCheck(x), LISPCHECK_LIST)
#define GetCheckArray(x)		GetBitByte(GetCheck(x), LISPCHECK_ARRAY)
#define GetCheckBody(x)			GetBitByte(GetCheck(x), LISPCHECK_BODY)
#define GetCheckArrayBody(x)	GetBitByte(GetCheck(x), LISPCHECK_ARRAYBODY)
#define GetCheckSize2(x)		GetBitByte(GetCheck(x), LISPCHECK_SIZE2)
#define GetCheckSize4(x)		GetBitByte(GetCheck(x), LISPCHECK_SIZE4)
#define GetCheckSize8(x)		GetBitByte(GetCheck(x), LISPCHECK_SIZE8)
#define GetCheckValue(x,i)		GetBitByte(*PtrCheck(x),(i))
#define SetCheckValue(x,i,v)	SetBitByte(*PtrCheck(x),(i),(v))
#define SetCheck1(x,y)			SetCheck((x), (1U<<(y)))
#define SetCheck2(x,y,z)		SetCheck((x),((1U<<(y))|(1U<<(z))))
#define SetCheck3(x,y,z,w)		SetCheck((x),((1U<<(y))|(1U<<(z))|(1U<<(w))))
#define SetCheck4(x,y,z,w,a)	SetCheck((x),((1U<<(y))|(1U<<(z))|(1U<<(w))|(1U<<(a))))

/* Object Header */
#define PtrByte1A(x)            (((addr)(x)) + 4UL)
#define PtrByte1B(x)            (((addr)(x)) + 5UL)
#define PtrByte2A(x)            (((addr)(x)) + 4UL)
#define PtrByte2B(x)            (((addr)(x)) + 6UL)
#define PtrByte2C(x)            (((addr)(x)) + 8UL)
#define PtrByte4A(x)            (((addr)(x)) + 4UL)
#define PtrByte4B(x)            (((addr)(x)) + 8UL)
#define PtrByte4C(x)            (((addr)(x)) + 12UL)
#define PtrByte4D(x)            (((addr)(x)) + 16UL)
#define PtrValue2A(x)           ((byte16 *)PtrByte2A(x))
#define PtrValue2B(x)           ((byte16 *)PtrByte2B(x))
#define PtrValue2C(x)           ((byte16 *)PtrByte2C(x))
#define PtrValue4A(x)           ((byte32 *)PtrByte4A(x))
#define PtrValue4B(x)           ((byte32 *)PtrByte4B(x))
#define PtrValue4C(x)           ((byte32 *)PtrByte4C(x))
#define PtrValue4D(x)           ((byte32 *)PtrByte4D(x))

#define PtrByte2va(x)			PtrByte1A(x)
#define PtrByte2vb(x)			PtrByte1B(x)
#define PtrByte2V(x)			PtrByte2A(x)
#define PtrByte2L(x)			PtrByte2B(x)
#define PtrByte2P(x)			PtrByte2C(x)
#define PtrValue2va(x)			((byte *)PtrByte2va(x))
#define PtrValue2vb(x)			((byte *)PtrByte2vb(x))
#define PtrValue2V(x)			((byte16 *)PtrByte2V(x))
#define PtrValue2L(x)			((byte16 *)PtrByte2L(x))

#define PtrByte4va(x)			PtrByte2A(x)
#define PtrByte4vb(x)			PtrByte2B(x)
#define PtrByte4V(x)			PtrByte4A(x)
#define PtrByte4P(x)			PtrByte4D(x)
#define PtrValue4va(x)			((byte16 *)PtrByte4va(x))
#define PtrValue4vb(x)			((byte16 *)PtrByte4vb(x))
#define PtrValue4V(x)			((byte32 *)PtrByte4V(x))

/* 64bit */
#ifdef LISP_ARCH_64BIT
#define PtrByte8A(x)            (((addr)(x)) + 8UL)
#define PtrByte8B(x)            (((addr)(x)) + 16UL)
#define PtrByte8C(x)            (((addr)(x)) + 24UL)
#define PtrValue8A(x,t)			((t *)(((addr)(x)) + 8UL))
#define PtrValue8B(x,t)			((t *)(((addr)(x)) + 16UL))
#define PtrValue8C(x,t)			((t *)(((addr)(x)) + 24UL))

#define PtrByte8V(x)			PtrByte8B(x)
#define PtrByte8P(x)			PtrByte8C(x)
#define PtrValue8V(x)			((size_t *)PtrByte8V(x))
#endif

/* Length */
#ifdef LISP_ARCH_64BIT
#define PtrByteL(x)				PtrByte8A(x)
#else
#define PtrByteL(x)				PtrByte4C(x)
#endif
#define PtrValueL(x)			((size_t *)PtrByteL(x))

/* memory object */
#define PtrValueReserved(x)		((size_t *)(8UL + (addr)(x)))
#define GetValueReserved(x,y)	(*(y) = *PtrValueReserved(x))
#define SetValueReserved(x,y)	(*PtrValueReserved(x) = (y))
#define PtrValueSpace(x)		((size_t *)(8UL + (addr)(x)))
#define GetValueSpace(x,y)		(*(y) = *PtrValueReserved(x))
#define SetValueSpace(x,y)		(*PtrValueReserved(x) = (y))
#define GetValueSpace1(x,y)		(*(y) = (size_t)(((addr)(x))[1]))
#define SetValueSpace1(x,y)		(((addr)(x))[1] = (byte)(y))

#define GetSizeReserved(x,y)	(*(y) = *PtrValueReserved(x) + 8UL + IdxSize)
#define SetSizeReserved(x,y)	(*PtrValueReserved(x) = (y) - (8UL + IdxSize))
#define GetSizeSpace(x,y)		(*(y) = *PtrValueReserved(x) + 8UL + IdxSize)
#define SetSizeSpace(x,y)		(*PtrValueReserved(x) = (y) - (8UL + IdxSize))
#define GetSizeSpace1(x,y)		(*(y) = (size_t)((((addr)(x))[1]) + 2UL))
#define SetSizeSpace1(x,y)		(((addr)(x))[1] = (byte)(((size_t)(y)) - 2UL))

#define PtrArrayA2(x)			((addr *)PtrByte2P(x))
#define PtrArraySS(x)			((addr *)PtrByte2P(x))
#define PtrArrayA4(x)			((addr *)PtrByte4P(x))
#define PtrArrayAB(x)			((addr *)PtrByte4P(x))
#define PtrArrayA2i(x,i)        (PtrArrayA2(x) + (i))
#define PtrArraySSi(x,i)        (PtrArraySS(x) + (i))
#define PtrArrayA4i(x,i)        (PtrArrayA4(x) + (i))
#define PtrArrayABi(x,i)        (PtrArrayAB(x) + (i))

#define PtrLenArrayA2(x)        PtrValue2V(x)
#define PtrLenArraySS(x)        PtrValue2va(x)
#define PtrLenArrayA4(x)        PtrValue4V(x)
#define PtrLenArrayAB(x)        PtrValue4va(x)
#define PtrLenBodyB2(x)         PtrValue2V(x)
#define PtrLenBodySS(x)         PtrValue2vb(x)
#define PtrLenBodyB4(x)         PtrValue4V(x)
#define PtrLenBodyAB(x)         PtrValue4vb(x)

#define GetLenArrayA2(x)        (*PtrLenArrayA2(x))
#define GetLenArraySS(x)        (*PtrLenArraySS(x))
#define GetLenArrayA4(x)        (*PtrLenArrayA4(x))
#define GetLenArrayAB(x)        (*PtrLenArrayAB(x))
#define GetLenBodyB2(x)         (*PtrLenBodyB2(x))
#define GetLenBodySS(x)         (*PtrLenBodySS(x))
#define GetLenBodyB4(x)         (*PtrLenBodyB4(x))
#define GetLenBodyAB(x)         (*PtrLenBodyAB(x))

#ifdef LISP_ARCH_64BIT
#define PtrArrayA8(x)			((addr *)PtrByte8P(x))
#define PtrArrayA8i(x,i)		(PtrArrayA8(x) + (i))
#define PtrLenArrayA8(x)        PtrValue8V(x)
#define PtrLenBodyB8(x)         PtrValue8V(x)
#define GetLenArrayA8(x)        (*PtrLenArrayA8(x))
#define GetLenBodyB8(x)         (*PtrLenBodyB8(x))
#endif

#define PtrBodySSa(x,y)			(PtrByte2P(x) + (y)*PtrSize)
#define PtrBodyABa(x,y)			(PtrByte4P(x) + (y)*PtrSize)
#define PtrBodyB2(x)			PtrByte2P(x)
#define PtrBodySS(x)			PtrBodySSa(x, GetLenArraySS(x))
#define PtrBodyB4(x)			PtrByte4P(x)
#define PtrBodyAB(x)			PtrBodyABa(x, GetLenArrayAB(x))

#define RefBodySSa(x,a,t)		(*(t *)PtrBodySSa(x,a))
#define RefBodyABa(x,a,t)		(*(t *)PtrBodyABa(x,a))
#define RefBodyB2(x,t)			(*(t *)PtrBodyB2(x))
#define RefBodySS(x,t)			(*(t *)PtrBodySS(x))
#define RefBodyB4(x,t)			(*(t *)PtrBodyB4(x))
#define RefBodyAB(x,t)			(*(t *)PtrBodyAB(x))

#define RefBodySSai(x,a,t,i)	(((t *)PtrBodySSa(x,a))[i])
#define RefBodyABai(x,a,t,i)	(((t *)PtrBodyABa(x,a))[i])
#define RefBodyB2i(x,t,i)		(((t *)PtrBodyB2(x))[i])
#define RefBodySSi(x,t,i)		(((t *)PtrBodySS(x))[i])
#define RefBodyB4i(x,t,i)		(((t *)PtrBodyB4(x))[i])
#define RefBodyABi(x,t,i)		(((t *)PtrBodyAB(x))[i])

#ifdef LISP_ARCH_64BIT
#define PtrBodyB8(x)			PtrByte8P(x)
#define RefBodyB8(x,t)			(*(t *)PtrBodyB8(x))
#define RefBodyB8i(x,t,i)		(((t *)PtrBodyB8(x))[i])
#endif

#define GetvBodySSa(x,a,t,v)	(*(v) = RefBodySSa(x,a,t))
#define GetvBodyABa(x,a,t,v)	(*(v) = RefBodyABa(x,a,t))
#define GetvBodyB2(x,t,v)		(*(v) = RefBodyB2(x,t))
#define GetvBodySS(x,t,v)		(*(v) = RefBodySS(x,t))
#define GetvBodyB4(x,t,v)		(*(v) = RefBodyB4(x,t))
#define GetvBodyAB(x,t,v)		(*(v) = RefBodyAB(x,t))

#define SetvBodySSa(x,a,t,v)	(RefBodySSa(x,a,t) = (v))
#define SetvBodyABa(x,a,t,v)	(RefBodyABa(x,a,t) = (v))
#define SetvBodyB2(x,t,v)		(RefBodyB2(x,t) = (v))
#define SetvBodySS(x,t,v)		(RefBodySS(x,t) = (v))
#define SetvBodyB4(x,t,v)		(RefBodyB4(x,t) = (v))
#define SetvBodyAB(x,t,v)		(RefBodyAB(x,t) = (v))

#define IncvBodySSa(x,a,t,v)	(RefBodySSa(x,a,t) += (v))
#define IncvBodyABa(x,a,t,v)	(RefBodyABa(x,a,t) += (v))
#define IncvBodyB2(x,t,v)		(RefBodyB2(x,t) += (v))
#define IncvBodySS(x,t,v)		(RefBodySS(x,t) += (v))
#define IncvBodyB4(x,t,v)		(RefBodyB4(x,t) += (v))
#define IncvBodyAB(x,t,v)		(RefBodyAB(x,t) += (v))

#define DecvBodySSa(x,a,t,v)	(RefBodySSa(x,a,t) -= (v))
#define DecvBodyABa(x,a,t,v)	(RefBodyABa(x,a,t) -= (v))
#define DecvBodyB2(x,t,v)		(RefBodyB2(x,t) -= (v))
#define DecvBodySS(x,t,v)		(RefBodySS(x,t) -= (v))
#define DecvBodyB4(x,t,v)		(RefBodyB4(x,t) -= (v))
#define DecvBodyAB(x,t,v)		(RefBodyAB(x,t) -= (v))

#define GetvBodySSai(x,a,t,i,v)	(*(v) = RefBodySSai(x,a,t,i))
#define GetvBodyABai(x,a,t,i,v)	(*(v) = RefBodyABai(x,a,t,i))
#define GetvBodyB2i(x,t,i,v)	(*(v) = RefBodyB2i(x,t,i))
#define GetvBodySSi(x,t,i,v)	(*(v) = RefBodySSi(x,t,i))
#define GetvBodyB4i(x,t,i,v)	(*(v) = RefBodyB4i(x,t,i))
#define GetvBodyABi(x,t,i,v)	(*(v) = RefBodyABi(x,t,i))

#define SetvBodySSai(x,a,t,i,v)	(RefBodySSai(x,a,t,i) = (v))
#define SetvBodyABai(x,a,t,i,v)	(RefBodyABai(x,a,t,i) = (v))
#define SetvBodyB2i(x,t,i,v)	(RefBodyB2i(x,t,i) = (v))
#define SetvBodySSi(x,t,i,v)	(RefBodySSi(x,t,i) = (v))
#define SetvBodyB4i(x,t,i,v)	(RefBodyB4i(x,t,i) = (v))
#define SetvBodyABi(x,t,i,v)	(RefBodyABi(x,t,i) = (v))

#define IncvBodySSai(x,a,t,i,v)	(RefBodySSai(x,a,t,i) += (v))
#define IncvBodyABai(x,a,t,i,v)	(RefBodyABai(x,a,t,i) += (v))
#define IncvBodyB2i(x,t,i,v)	(RefBodyB2i(x,t,i) += (v))
#define IncvBodySSi(x,t,i,v)	(RefBodySSi(x,t,i) += (v))
#define IncvBodyB4i(x,t,i,v)	(RefBodyB4i(x,t,i) += (v))
#define IncvBodyABi(x,t,i,v)	(RefBodyABi(x,t,i) += (v))

#define DecvBodySSai(x,a,t,i,v)	(RefBodySSai(x,a,t,i) -= (v))
#define DecvBodyABai(x,a,t,i,v)	(RefBodyABai(x,a,t,i) -= (v))
#define DecvBodyB2i(x,t,i,v)	(RefBodyB2i(x,t,i) -= (v))
#define DecvBodySSi(x,t,i,v)	(RefBodySSi(x,t,i) -= (v))
#define DecvBodyB4i(x,t,i,v)	(RefBodyB4i(x,t,i) -= (v))
#define DecvBodyABi(x,t,i,v)	(RefBodyABi(x,t,i) -= (v))

#ifdef LISP_ARCH_64BIT
#define GetvBodyB8(x,t,v)		(*(v) = RefBodyB8(x,t))
#define SetvBodyB8(x,t,v)		(RefBodyB8(x,t) = (v))
#define IncvBodyB8(x,t,v)		(RefBodyB8(x,t) += (v))
#define DecvBodyB8(x,t,v)		(RefBodyB8(x,t) -= (v))
#define GetvBodyB8i(x,t,i,v)	(*(v) = RefBodyB8i(x,t,i))
#define SetvBodyB8i(x,t,i,v)	(RefBodyB8i(x,t,i) = (v))
#define IncvBodyB8i(x,t,i,v)	(RefBodyB8i(x,t,i) += (v))
#define DecvBodyB8i(x,t,i,v)	(RefBodyB8i(x,t,i) -= (v))
#endif

#define IsValueSymbol(x)		\
	((x) == LISPTYPE_SYMBOL || (x) == LISPTYPE_NIL || (x) == LISPTYPE_T)
#define IsValueFloat(x)         \
	(LISPTYPE_SHORT_FLOAT <= (x) && (x) <= LISPTYPE_LONG_FLOAT)
#define IsValueByte2(x)			\
	((x) == LISPSIZE_ARRAY2 || (x) == LISPSIZE_BODY2 || (x) == LISPSIZE_SMALLSIZE)

#define IsClassSmall(x)			((x) < LISPCLASS_SizeK)
#define IsClassLarge(x)			(LISPCLASS_SizeK <= (x))
#define IsBoolean(x)			((x) == Nil || (x) == T)
#define IsCons(x)               (GetType(x) == LISPTYPE_CONS)
#define IsList(x)               ((x) == Nil || GetType(x) == LISPTYPE_CONS)
#define IsArray(x)              (GetStatusSize(x) <= LISPSIZE_ARRAYBODY)
#define IsBody(x)               (LISPSIZE_SMALLSIZE <= GetStatusSize(x))
#define IsSymbol(x)             GetCheckSymbol(x)
#define IsFloat(x)              IsValueFloat(GetType(x))

#define MemoryLengthSS(a,b)		( 8UL + (a)*PtrSize + (b))
#define MemoryLengthAB(a,b)		(16UL + (a)*PtrSize + (b))
#define MemoryLengthA2(a)		( 8UL + (a)*PtrSize)
#define MemoryLengthA4(a)		(16UL + (a)*PtrSize)
#define MemoryLengthB2(b)		( 8UL + (b))
#define MemoryLengthB4(b)		(16UL + (b))
#ifdef LISP_ARCH_64BIT
#define MemoryLengthA8(a)		(24UL + (a)*PtrSize)
#define MemoryLengthB8(b)		(24UL + (b))
#endif

#ifdef LISP_DEBUG
#define LenArrayA2(x,len)		lenarrayA2(x,len)
#define LenArraySS(x,len)		lenarraySS(x,len)
#define LenArrayA4(x,len)		lenarrayA4(x,len)
#define LenArrayAB(x,len)		lenarrayAB(x,len)
#define LenBodyB2(x,len)		lenbodyB2(x,len)
#define LenBodySS(x,len)		lenbodySS(x,len)
#define LenBodyB4(x,len)		lenbodyB4(x,len)
#define LenBodyAB(x,len)		lenbodyAB(x,len)

#define LenArrayA2r(x)			lenarrayA2r(x)
#define LenArraySSr(x)			lenarraySSr(x)
#define LenArrayA4r(x)			lenarrayA4r(x)
#define LenArrayABr(x)			lenarrayABr(x)
#define LenBodyB2r(x)			lenbodyB2r(x)
#define LenBodySSr(x)			lenbodySSr(x)
#define LenBodyB4r(x)			lenbodyB4r(x)
#define LenBodyABr(x)			lenbodyABr(x)

#define PosBodySSa(x,a,len)		posbodySSa(x,a,len)
#define PosBodyABa(x,a,len)		posbodyABa(x,a,len)
#define PosBodyB2(x,len)        posbodyB2(x,len)
#define PosBodySS(x,len)        posbodySS(x,len)
#define PosBodyB4(x,len)        posbodyB4(x,len)
#define PosBodyAB(x,len)        posbodyAB(x,len)

#define PosBodySSar(x,a)		posbodySSar(x,a)
#define PosBodyABar(x,a)		posbodyABar(x,a)
#define PosBodyB2r(x)			posbodyB2r(x)
#define PosBodySSr(x)			posbodySSr(x)
#define PosBodyB4r(x)			posbodyB4r(x)
#define PosBodyABr(x)			posbodyABr(x)

#define PosBodyLenSSa(x,a,b,n)	posbodylenSSa(x,a,b,n)
#define PosBodyLenABa(x,a,b,n)	posbodylenABa(x,a,b,n)
#define PosBodyLenB2(x,b,n)     posbodylenB2(x,b,n)
#define PosBodyLenSS(x,b,n)     posbodylenSS(x,b,n)
#define PosBodyLenB4(x,b,n)     posbodylenB4(x,b,n)
#define PosBodyLenAB(x,b,n)     posbodylenAB(x,b,n)

#ifdef LISP_ARCH_64BIT
#define LenArrayA8(x,len)       lenarrayA8(x,len)
#define LenBodyB8(x,len)        lenbodyB8(x,len)
#define PosBodyB8(x,len)        posbodyB8(x,len)
#define PosBodyLenB8(x,b,n)     posbodylenB8(x,b,n)

#define LenArrayA8r(x)			lenarrayA8r(x)
#define LenBodyB8r(x)			lenbodyB8r(x)
#define PosBodyB8r(x)			posbodyB8r(x)
#endif

#else
#define LenArrayA2(x,len)       (*(len) = GetLenArrayA2(x))
#define LenArraySS(x,len)       (*(len) = GetLenArraySS(x))
#define LenArrayA4(x,len)       (*(len) = GetLenArrayA4(x))
#define LenArrayAB(x,len)       (*(len) = GetLenArrayAB(x))
#define LenBodyB2(x,len)        (*(len) = GetLenBodyB2(x))
#define LenBodySS(x,len)        (*(len) = GetLenBodySS(x))
#define LenBodyB4(x,len)        (*(len) = GetLenBodyB4(x))
#define LenBodyAB(x,len)        (*(len) = GetLenBodyAB(x))

#define LenArrayA2r(x)			GetLenArrayA2(x)
#define LenArraySSr(x)			GetLenArraySS(x)
#define LenArrayA4r(x)			GetLenArrayA4(x)
#define LenArrayABr(x)			GetLenArrayAB(x)
#define LenBodyB2r(x)			GetLenBodyB2(x)
#define LenBodySSr(x)			GetLenBodySS(x)
#define LenBodyB4r(x)			GetLenBodyB4(x)
#define LenBodyABr(x)			GetLenBodyAB(x)

#define PosBodySSa(x,a,len)		(*(len) = PtrBodySSa(x,a))
#define PosBodyABa(x,a,len)		(*(len) = PtrBodyABa(x,a))
#define PosBodyB2(x,len)        (*(len) = PtrBodyB2(x))
#define PosBodySS(x,len)        (*(len) = PtrBodySS(x))
#define PosBodyB4(x,len)        (*(len) = PtrBodyB4(x))
#define PosBodyAB(x,len)        (*(len) = PtrBodyAB(x))

#define PosBodySSar(x,a)		PtrBodySSa(x,a)
#define PosBodyABar(x,a)		PtrBodyABa(x,a)
#define PosBodyB2r(x)			PtrBodyB2(x)
#define PosBodySSr(x)			PtrBodySS(x)
#define PosBodyB4r(x)			PtrBodyB4(x)
#define PosBodyABr(x)			PtrBodyAB(x)

#define PosBodyLenSSa(x,a,b,n)	(*(b)=PtrBodySSa(x,a), *(n)=GetLenBodySS(x))
#define PosBodyLenABa(x,a,b,n)	(*(b)=PtrBodyABa(x,a), *(n)=GetLenBodyAB(x))
#define PosBodyLenB2(x,b,n)     (*(b)=PtrBodyB2(x), *(n)=GetLenBodyB2(x))
#define PosBodyLenSS(x,b,n)     (*(b)=PtrBodySS(x), *(n)=GetLenBodySS(x))
#define PosBodyLenB4(x,b,n)     (*(b)=PtrBodyB4(x), *(n)=GetLenBodyB4(x))
#define PosBodyLenAB(x,b,n)     (*(b)=PtrBodyAB(x), *(n)=GetLenBodyAB(x))

#ifdef LISP_ARCH_64BIT
#define LenArrayA8(x,len)       (*(len) = GetLenArrayA8(x))
#define LenBodyB8(x,len)        (*(len) = GetLenBodyB8(x))
#define PosBodyB8(x,len)        (*(len) = PtrBodyB8(x))
#define PosBodyLenB8(x,b,n)     (*(b)=PtrBodyB8(x), *(n)=GetLenBodyB8(x))

#define LenArrayA8r(x)			GetLenArrayA8(x)
#define LenBodyB8r(x)			GetLenBodyB8(x)
#define PosBodyB8r(x)			PtrBodyB8(x)
#endif
#endif

#ifdef LISP_DEBUG
#define GetArrayA2(x,i,v)		getarrayA2(x,i,v)
#define GetArraySS(x,i,v)		getarraySS(x,i,v)
#define GetArrayA4(x,i,v)		getarrayA4(x,i,v)
#define GetArrayAB(x,i,v)		getarrayAB(x,i,v)

#define RefArrayA2(x,i)			refarrayA2(x,i)
#define RefArraySS(x,i)			refarraySS(x,i)
#define RefArrayA4(x,i)			refarrayA4(x,i)
#define RefArrayAB(x,i)			refarrayAB(x,i)

#define SetArrayA2(x,i,v)		setarrayA2(x,i,v)
#define SetArraySS(x,i,v)		setarraySS(x,i,v)
#define SetArrayA4(x,i,v)		setarrayA4(x,i,v)
#define SetArrayAB(x,i,v)		setarrayAB(x,i,v)

#define SetArrayA2_force(x,i,v)	setarrayA2_force(x,i,v)
#define SetArraySS_force(x,i,v)	setarraySS_force(x,i,v)
#define SetArrayA4_force(x,i,v)	setarrayA4_force(x,i,v)
#define SetArrayAB_force(x,i,v)	setarrayAB_force(x,i,v)

#ifdef LISP_ARCH_64BIT
#define GetArrayA8(x,i,v)		getarrayA8(x,i,v)
#define RefArrayA8(x,i)			refarrayA8(x,i)
#define SetArrayA8(x,i,v)       setarrayA8(x,i,v)
#define SetArrayA8_force(x,i,v)	setarrayA8_force(x,i,v)
#endif
#else

#define GetArrayA2(r,i,n)		(*(n) = PtrArrayA2(r)[i])
#define GetArraySS(r,i,n)		(*(n) = PtrArraySS(r)[i])
#define GetArrayA4(r,i,n)		(*(n) = PtrArrayA4(r)[i])
#define GetArrayAB(r,i,n)		(*(n) = PtrArrayAB(r)[i])

#define RefArrayA2(r,i)			(PtrArrayA2(r)[i])
#define RefArraySS(r,i)			(PtrArraySS(r)[i])
#define RefArrayA4(r,i)			(PtrArrayA4(r)[i])
#define RefArrayAB(r,i)			(PtrArrayAB(r)[i])

#define SetArrayA2(x,i,v)       (PtrArrayA2(x)[i] = (v))
#define SetArraySS(x,i,v)       (PtrArraySS(x)[i] = (v))
#define SetArrayA4(x,i,v)       (PtrArrayA4(x)[i] = (v))
#define SetArrayAB(x,i,v)       (PtrArrayAB(x)[i] = (v))

#define SetArrayA2_force(x,i,v)	SetArrayA2(x,i,v)
#define SetArraySS_force(x,i,v)	SetArraySS(x,i,v)
#define SetArrayA4_force(x,i,v)	SetArrayA4(x,i,v)
#define SetArrayAB_force(x,i,v)	SetArrayAB(x,i,v)

#ifdef LISP_ARCH_64BIT
#define GetArrayA8(r,i,n)		(*(n) = PtrArrayA8(r)[i])
#define RefArrayA8(r,i)			(PtrArrayA8(r)[i])
#define SetArrayA8(x,i,v)       (PtrArrayA8(x)[i] = (v))
#define SetArrayA8_force(x,i,v)	SetArrayA8(x,i,v)
#endif
#endif

#define ClearBodyB2(p,b)		memset(PtrByte2P(p), 0, (b))
#define ClearBodySS(p,a,b)		memset(PtrByte2P(p) + (a)*PtrSize, 0, b)
#define ClearBodyB4(p,b)		memset(PtrByte4P(p), 0, (b))
#define ClearBodyAB(p,a,b)		memset(PtrByte4P(p) + (a)*PtrSize, 0, b)
#ifdef LISP_ARCH_64BIT
#define ClearBodyB8(p,b)		memset(PtrByte8P(p), 0, (b))
#endif


/*
 *  type
 */
_g enum LISPTYPE gettype(addr pos);


/*
 *  size class
 */
_g enum LISPCLASS size_class(size_t);
_g size_t size_split(size_t size);
_g void size_and_class(size_t, enum LISPCLASS *, size_t *);
_g size_t getobjectlength(addr pos);
_g size_t getmemorylength(addr pos);
_g int valid_header(addr pos);


/*
 *  Memory Access
 */
_g void lenarrayA2(addr pos, size_t *ret);
_g void lenarraySS(addr pos, size_t *ret);
_g void lenarrayA4(addr pos, size_t *ret);
_g void lenarrayAB(addr pos, size_t *ret);
_g void lenarray(addr pos, size_t *ret);
#ifdef LISP_ARCH_64BIT
_g void lenarrayA8(addr pos, size_t *ret);
#endif

_g size_t lenarrayA2r(addr pos);
_g size_t lenarraySSr(addr pos);
_g size_t lenarrayA4r(addr pos);
_g size_t lenarrayABr(addr pos);
_g size_t lenarrayr(addr pos);
#ifdef LISP_ARCH_64BIT
_g size_t lenarrayA8r(addr pos);
#endif

_g void lenbodyB2(addr pos, size_t *ret);
_g void lenbodySS(addr pos, size_t *ret);
_g void lenbodyB4(addr pos, size_t *ret);
_g void lenbodyAB(addr pos, size_t *ret);
_g void lenbody(addr pos, size_t *ret);
#ifdef LISP_ARCH_64BIT
_g void lenbodyB8(addr pos, size_t *ret);
#endif

_g size_t lenbodyB2r(addr pos);
_g size_t lenbodySSr(addr pos);
_g size_t lenbodyB4r(addr pos);
_g size_t lenbodyABr(addr pos);
_g size_t lenbodyr(addr pos);
#ifdef LISP_ARCH_64BIT
_g size_t lenbodyB8r(addr pos);
#endif

_g void posbodySSa(addr pos, size_t array, addr *ret);
_g void posbodyABa(addr pos, size_t array, addr *ret);
_g void posbodyB2(addr pos, addr *ret);
_g void posbodySS(addr pos, addr *ret);
_g void posbodyB4(addr pos, addr *ret);
_g void posbodyAB(addr pos, addr *ret);
_g void posbody(addr pos, addr *ret);
#ifdef LISP_ARCH_64BIT
_g void posbodyB8(addr pos, addr *ret);
#endif

_g addr posbodySSar(addr pos, size_t array);
_g addr posbodyABar(addr pos, size_t array);
_g addr posbodyB2r(addr pos);
_g addr posbodySSr(addr pos);
_g addr posbodyB4r(addr pos);
_g addr posbodyABr(addr pos);
_g addr posbodyr(addr pos);
#ifdef LISP_ARCH_64BIT
_g addr posbodyB8r(addr pos);
#endif

_g void posbodylenSSa(addr pos, size_t array, addr *body, size_t *len);
_g void posbodylenABa(addr pos, size_t array, addr *body, size_t *len);
_g void posbodylenB2(addr pos, addr *body, size_t *len);
_g void posbodylenSS(addr pos, addr *body, size_t *len);
_g void posbodylenB4(addr pos, addr *body, size_t *len);
_g void posbodylenAB(addr pos, addr *body, size_t *len);
_g void posbodylen(addr pos, addr *body, size_t *len);
#ifdef LISP_ARCH_64BIT
_g void posbodylenB8(addr pos, addr *body, size_t *len);
#endif

_g void getarrayA2(addr pos, size_t index, addr *ret);
_g void getarraySS(addr pos, size_t index, addr *ret);
_g void getarrayA4(addr pos, size_t index, addr *ret);
_g void getarrayAB(addr pos, size_t index, addr *ret);
_g void getarray(addr pos, size_t index, addr *ret);
#ifdef LISP_ARCH_64BIT
_g void getarrayA8(addr pos, size_t index, addr *ret);
#endif

_g addr refarrayA2(addr pos, size_t index);
_g addr refarraySS(addr pos, size_t index);
_g addr refarrayA4(addr pos, size_t index);
_g addr refarrayAB(addr pos, size_t index);
_g addr refarray(addr pos, size_t index);
#ifdef LISP_ARCH_64BIT
_g addr refarrayA8(addr pos, size_t index);
#endif

_g int checkdynamic(addr pos, addr value);
#define CheckDynamic(p,v) { \
	Check(checkdynamic((p), (v)), "dynamic error"); \
}
_g void setarrayA2(addr pos, size_t index, addr ret);
_g void setarraySS(addr pos, size_t index, addr ret);
_g void setarrayA4(addr pos, size_t index, addr ret);
_g void setarrayAB(addr pos, size_t index, addr ret);
_g void setarray(addr pos, size_t index, addr ret);
#ifdef LISP_ARCH_64BIT
_g void setarrayA8(addr pos, size_t index, addr ret);
#endif

_g void setarrayA2_force(addr pos, size_t index, addr ret);
_g void setarraySS_force(addr pos, size_t index, addr ret);
_g void setarrayA4_force(addr pos, size_t index, addr ret);
_g void setarrayAB_force(addr pos, size_t index, addr ret);
#ifdef LISP_ARCH_64BIT
_g void setarrayA8_force(addr pos, size_t index, addr ret);
#endif


/*
 *  Object
 */
_g void nilarray2(addr pos, size_t size);
_g void nilarray4(addr pos, size_t size);
#ifdef LISP_ARCH_64BIT
_g void nilarray8(addr pos, size_t size);
#endif
_g void unboundarray2(addr pos, size_t size);
_g void unboundarray4(addr pos, size_t size);
#ifdef LISP_ARCH_64BIT
_g void unboundarray8(addr pos, size_t size);
#endif

#endif

