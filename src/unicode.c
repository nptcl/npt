#include "build.h"
#include "condition.h"
#include "encode.h"
#include "strvect.h"

_g addr string8_size_allocr(LocalRoot local, const char *name, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t allsize;

	if (UTF8_size_strlen((const byte *)name, size, &allsize))
		_fmte("UTF8 encoding error (length).", NULL);
	strvect_alloc(local, &pos, allsize);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF8_size_makeunicode(destroy, (const byte *)name, size))
		_fmte("UTF8 encoding error (make).", NULL);
	strvect_update_character_type(pos);

	return pos;
}
_g addr string8_size_localr(LocalRoot local, const char *name, size_t size)
{
	Check(local == NULL, "local error");
	return string8_size_allocr(local, name, size);
}
_g addr string8_size_heapr(const char *name, size_t size)
{
	return string8_size_allocr(NULL, name, size);
}
_g void string8_size_alloc(LocalRoot local, addr *ret, const char *name, size_t size)
{
	*ret = string8_size_allocr(local, name, size);
}
_g void string8_size_local(LocalRoot local, addr *ret, const char *name, size_t size)
{
	Check(local == NULL, "local error");
	*ret = string8_size_allocr(local, name, size);
}
_g void string8_size_heap(addr *ret, const char *name, size_t size)
{
	*ret = string8_size_allocr(NULL, name, size);
}

_g addr string8_null_allocr(LocalRoot local, const char *name)
{
	addr pos;
	unicode *destroy;
	size_t size;

	if (UTF8_null_strlen((const byte *)name, &size))
		_fmte("UTF8 encoding error (length).", NULL);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF8_null_makeunicode(destroy, (const byte *)name))
		_fmte("UTF8 encoding error (make).", NULL);
	strvect_update_character_type(pos);

	return pos;
}
_g addr string8_null_localr(LocalRoot local, const char *name)
{
	Check(local == NULL, "local error");
	return string8_null_allocr(local, name);
}
_g addr string8_null_heapr(const char *name)
{
	return string8_null_allocr(NULL, name);
}
_g void string8_null_alloc(LocalRoot local, addr *ret, const char *name)
{
	*ret = string8_null_allocr(local, name);
}
_g void string8_null_local(LocalRoot local, addr *ret, const char *name)
{
	Check(local == NULL, "local error");
	*ret = string8_null_allocr(local, name);
}
_g void string8_null_heap(addr *ret, const char *name)
{
	*ret = string8_null_allocr(NULL, name);
}

_g addr string16_size_allocr(LocalRoot local, const byte16 *name, size_t size)
{
	addr pos;
	unicode *destroy;
	size_t allsize;

	if (UTF16_size_strlen(name, size, &allsize))
		_fmte("UTF16 encoding error (length).", NULL);
	strvect_alloc(local, &pos, allsize);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF16_size_makeunicode(destroy, name, size))
		_fmte("UTF16 encoding error (make).", NULL);
	strvect_update_character_type(pos);

	return pos;
}
_g addr string16_size_localr(LocalRoot local, const byte16 *name, size_t size)
{
	Check(local == NULL, "local error");
	return string16_size_allocr(local, name, size);
}
_g addr string16_size_heapr(const byte16 *name, size_t size)
{
	return string16_size_allocr(NULL, name, size);
}
_g void string16_size_alloc(LocalRoot local, addr *ret, const byte16 *name, size_t size)
{
	*ret = string16_size_allocr(local, name, size);
}
_g void string16_size_local(LocalRoot local, addr *ret, const byte16 *name, size_t size)
{
	Check(local == NULL, "local error");
	*ret = string16_size_allocr(local, name, size);
}
_g void string16_size_heap(addr *ret, const byte16 *name, size_t size)
{
	*ret = string16_size_allocr(NULL, name, size);
}

_g addr string16_null_allocr(LocalRoot local, const byte16 *name)
{
	addr pos;
	unicode *destroy;
	size_t size;

	if (UTF16_null_strlen(name, &size))
		_fmte("UTF16 encoding error (length).", NULL);
	strvect_alloc(local, &pos, size);
	GetStringUnicode(pos, (const unicode **)&destroy);
	if (UTF16_null_makeunicode(destroy, name))
		_fmte("UTF16 encoding error (make).", NULL);
	strvect_update_character_type(pos);

	return pos;
}
_g addr string16_null_localr(LocalRoot local, const byte16 *name)
{
	Check(local == NULL, "local error");
	return string16_null_allocr(local, name);
}
_g addr string16_null_heapr(const byte16 *name)
{
	return string16_null_allocr(NULL, name);
}
_g void string16_null_alloc(LocalRoot local, addr *ret, const byte16 *name)
{
	*ret = string16_null_allocr(local, name);
}
_g void string16_null_local(LocalRoot local, addr *ret, const byte16 *name)
{
	Check(local == NULL, "local error");
	*ret = string16_null_allocr(local, name);
}
_g void string16_null_heap(addr *ret, const byte16 *name)
{
	*ret = string16_null_allocr(NULL, name);
}

