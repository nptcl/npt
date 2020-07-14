#include "build.h"
#include "condition.h"
#include "control_object.h"
#include "encode.h"
#include "strvect.h"

_g int string8_size_alloc_(LocalRoot local, addr *ret, const char *name, size_t size)
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
	strvect_update_character_type(pos);
	return Result(ret,  pos);
}
_g int string8_size_local_(LocalRoot local, addr *ret, const char *name, size_t size)
{
	Check(local == NULL, "local error");
	return string8_size_alloc_(local, ret, name, size);
}
_g int string8_size_heap_(addr *ret, const char *name, size_t size)
{
	return string8_size_alloc_(NULL, ret, name, size);
}

_g int string8_null_alloc_(LocalRoot local, addr *ret, const char *name)
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
	strvect_update_character_type(pos);
	return Result(ret, pos);
}
_g int string8_null_local_(LocalRoot local, addr *ret, const char *name)
{
	Check(local == NULL, "local error");
	return string8_null_alloc_(local, ret, name);
}
_g int string8_null_heap_(addr *ret, const char *name)
{
	return string8_null_alloc_(NULL, ret, name);
}

_g int string16_size_alloc_(LocalRoot local, addr *ret, const byte16 *name, size_t size)
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
	strvect_update_character_type(pos);
	return Result(ret, pos);
}
_g int string16_size_local_(LocalRoot local, addr *ret, const byte16 *name, size_t size)
{
	Check(local == NULL, "local error");
	return string16_size_alloc_(local, ret, name, size);
}
_g int string16_size_heap_(addr *ret, const byte16 *name, size_t size)
{
	return string16_size_alloc_(NULL, ret, name, size);
}

_g int string16_null_alloc_(LocalRoot local, addr *ret, const byte16 *name)
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
	strvect_update_character_type(pos);
	return Result(ret, pos);
}
_g int string16_null_local_(LocalRoot local, addr *ret, const byte16 *name)
{
	Check(local == NULL, "local error");
	return string16_null_alloc_(local, ret, name);
}
_g int string16_null_heap_(addr *ret, const byte16 *name)
{
	return string16_null_alloc_(NULL, ret, name);
}

