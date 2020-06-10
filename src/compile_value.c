#include "bigdata.h"
#include "bignum.h"
#include "compile_stream.h"
#include "compile_type.h"
#include "condition.h"
#include "execute.h"
#include "ratio.h"
#include "stream.h"
#include "typedef.h"

/*
 *  fixnum
 */
static int faslwrite_value_fixnum(Execute ptr, addr stream, addr pos)
{
	fixnum value;

	CheckType(pos, LISPTYPE_FIXNUM);
	faslwrite_type(stream, FaslCode_fixnum);
	GetFixnum(pos, &value);
	faslwrite_buffer(stream, &value, sizeoft(value));

	return 0;
}

_g int faslread_fixnum_code(Execute ptr, addr stream, addr *ret)
{
	fixnum value;

	faslread_buffer(stream, &value, sizeoft(value));
	fixnum_heap(ret, value);

	return 0;
}


/*
 *  bignum
 */
static int faslwrite_value_bignum(Execute ptr, addr stream, addr pos)
{
	int sign;
	fixed *data;
	size_t size, i;

	CheckType(pos, LISPTYPE_BIGNUM);
	faslwrite_type(stream, FaslCode_bignum);
	/* sign */
	GetSignBignum(pos, &sign);
	write_byte_stream(stream, (byte)sign);
	/* size */
	GetSizeBignum(pos, &size);
	faslwrite_buffer(stream, &size, IdxSize);
	/* data */
	GetDataBignum(pos, &data);
	for (i = 0; i < size; i++)
		faslwrite_buffer(stream, &(data[i]), sizeoft(fixed));
	
	return 0;
}

_g int faslread_bignum_code(Execute ptr, addr stream, addr *ret)
{
	byte sign;
	addr pos;
	fixed *data;
	size_t size, i;

	/* sign */
	faslread_byte(stream, &sign);
	/* size */
	faslread_buffer(stream, &size, IdxSize);
	/* data */
	bignum_heap(&pos, (sign != 0), size);
	GetDataBignum(pos, &data);
	for (i = 0; i < size; i++)
		faslread_buffer(stream, &(data[i]), sizeoft(fixed));
	
	return Result(ret, pos);
}


/*
 *  ratio
 */
static int faslwrite_value_ratio(Execute ptr, addr stream, addr pos)
{
	int sign;
	addr numer, denom;

	/* sign */
	CheckType(pos, LISPTYPE_RATIO);
	faslwrite_type(stream, FaslCode_ratio);
	/* sign */
	GetSignRatio(pos, &sign);
	write_byte_stream(stream, (byte)sign);
	/* numer/denom */
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	Return(faslwrite_value_bignum(ptr, stream, numer));
	Return(faslwrite_value_bignum(ptr, stream, denom));

	return 0;
}

_g int faslread_ratio_code(Execute ptr, addr stream, addr *ret)
{
	byte sign, check;
	addr numer, denom;

	/* sign */
	faslread_byte(stream, &sign);
	/* numer */
	faslread_byte(stream, &check);
	if (check != FaslCode_bignum)
		goto error;
	Return(faslread_bignum_code(ptr, stream, &numer));
	/* denom */
	faslread_byte(stream, &check);
	if (check != FaslCode_bignum)
		goto error;
	Return(faslread_bignum_code(ptr, stream, &denom));
	/* result */
	make_ratio_heap(ret, (sign != 0), numer, denom);
	return 0;

error:
	fmte("Invalid fasl format.", NULL);
	return 0;
}


/*
 *  interface
 */
_g int faslwrite_value(Execute ptr, addr stream, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			return faslwrite_value_fixnum(ptr, stream, pos);

		case LISPTYPE_BIGNUM:
			return faslwrite_value_bignum(ptr, stream, pos);

		case LISPTYPE_RATIO:
			return faslwrite_value_ratio(ptr, stream, pos);

		case LISPTYPE_TYPE:
			return faslwrite_value_type(ptr, stream, pos);

		default:
			fmte("Invalid value ~S.", pos, NULL);
			return 0;
	}

	return 0;
}

