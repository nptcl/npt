#include "build.h"
#include "condition.h"
#include "control_object.h"
#include "encode.h"
#include "strvect.h"
#include "unicode.h"

/*
 *  UTF-8
 */
int string8_size_alloc_(LocalRoot local, addr *ret, const char *name, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t allsize;

	if (UTF8_size_strlen((const byte *)name, size, &allsize))
		return fmte_("UTF8 encoding error (length).", NULL);
	strvect_alloc(local, &pos, allsize);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF8_size_makeunicode(destroy, (const byte *)name, size))
		return fmte_("UTF8 encoding error (make).", NULL);
	return Result(ret,  pos);
}
int string8_size_local_(LocalRoot local, addr *ret, const char *name, size_t size)
{
	CheckLocal(local);
	return string8_size_alloc_(local, ret, name, size);
}
int string8_size_heap_(addr *ret, const char *name, size_t size)
{
	return string8_size_alloc_(NULL, ret, name, size);
}

int string8_null_alloc_(LocalRoot local, addr *ret, const char *name)
{
	addr pos;
	unicode *destroy;
	size_t size;

	if (UTF8_null_strlen((const byte *)name, &size))
		return fmte_("UTF8 encoding error (length).", NULL);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF8_null_makeunicode(destroy, (const byte *)name))
		return fmte_("UTF8 encoding error (make).", NULL);
	return Result(ret, pos);
}
int string8_null_local_(LocalRoot local, addr *ret, const char *name)
{
	CheckLocal(local);
	return string8_null_alloc_(local, ret, name);
}
int string8_null_heap_(addr *ret, const char *name)
{
	return string8_null_alloc_(NULL, ret, name);
}


/*
 *  UTF-16
 */
int string16_size_alloc_(LocalRoot local, addr *ret, const byte16 *name, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t allsize;

	if (UTF16_size_strlen(name, size, &allsize))
		return fmte_("UTF16 encoding error (length).", NULL);
	strvect_alloc(local, &pos, allsize);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF16_size_makeunicode(destroy, name, size))
		return fmte_("UTF16 encoding error (make).", NULL);
	return Result(ret, pos);
}
int string16_size_local_(LocalRoot local, addr *ret, const byte16 *name, size_t size)
{
	CheckLocal(local);
	return string16_size_alloc_(local, ret, name, size);
}
int string16_size_heap_(addr *ret, const byte16 *name, size_t size)
{
	return string16_size_alloc_(NULL, ret, name, size);
}

int string16_null_alloc_(LocalRoot local, addr *ret, const byte16 *name)
{
	addr pos;
	unicode *destroy;
	size_t size;

	if (UTF16_null_strlen(name, &size))
		return fmte_("UTF16 encoding error (length).", NULL);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF16_null_makeunicode(destroy, name))
		return fmte_("UTF16 encoding error (make).", NULL);
	return Result(ret, pos);
}
int string16_null_local_(LocalRoot local, addr *ret, const byte16 *name)
{
	CheckLocal(local);
	return string16_null_alloc_(local, ret, name);
}
int string16_null_heap_(addr *ret, const byte16 *name)
{
	return string16_null_alloc_(NULL, ret, name);
}


/*
 *  UTF-32
 */
int string32_size_alloc_(LocalRoot local, addr *ret, const unicode *name, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t allsize;

	if (UTF32_size_strlen(name, size, &allsize))
		return fmte_("UTF32 encoding error (length).", NULL);
	strvect_alloc(local, &pos, allsize);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF32_size_makeunicode(destroy, name, size))
		return fmte_("UTF32 encoding error (make).", NULL);
	return Result(ret, pos);
}

int string32_size_local_(LocalRoot local, addr *ret, const unicode *name, size_t size)
{
	CheckLocal(local);
	return string32_size_alloc_(local, ret, name, size);
}

int string32_size_heap_(addr *ret, const unicode *name, size_t size)
{
	return string32_size_alloc_(NULL, ret, name, size);
}

int string32_null_alloc_(LocalRoot local, addr *ret, const unicode *name)
{
	addr pos;
	unicode *destroy;
	size_t size;

	if (UTF32_null_strlen(name, &size))
		return fmte_("UTF32 encoding error (length).", NULL);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF32_null_makeunicode(destroy, name))
		return fmte_("UTF32 encoding error (make).", NULL);
	return Result(ret, pos);
}

int string32_null_local_(LocalRoot local, addr *ret, const unicode *name)
{
	CheckLocal(local);
	return string32_null_alloc_(local, ret, name);
}

int string32_null_heap_(addr *ret, const unicode *name)
{
	return string32_null_alloc_(NULL, ret, name);
}

