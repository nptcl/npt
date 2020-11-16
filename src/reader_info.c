#include "character_queue.h"
#include "condition.h"
#include "cons.h"
#include "control_object.h"
#include "local.h"
#include "reader_info.h"
#include "symbol.h"
#include "typedef.h"

void get_readinfo(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_READINFO);
	GetReadInfo_Low(pos, index, ret);
}

void set_readinfo(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_READINFO);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadInfo_Low(pos, index, value);
}

struct readinfo_struct *struct_readinfo(addr pos)
{
	CheckType(pos, LISPSYSTEM_READINFO);
	return ReadInfoStruct_Low(pos);
}


/*
 *  readinfo
 */
static void readinfo_local(LocalRoot local, addr *ret)
{
	addr pos, value;
	struct readinfo_struct *str;

	/* readinfo */
	local_smallsize(local, &pos, LISPSYSTEM_READINFO,
			ReadInfo_Size, sizeoft(struct readinfo_struct));
	str = ReadInfoStruct(pos);
	clearpoint(str);
	/* charqueue */
	charqueue_local(local, &value, 0);
	SetReadInfo(pos, ReadInfo_Queue, value);
	/* label */
	consnil_heap(&value);
	SetReadInfo(pos, ReadInfo_Label, value);
	/* result */
	*ret = pos;
}

static void readinfo_symbol(addr *ret)
{
	GetConst(SYSTEM_READINFO_SPECIAL, ret);
}

void getreadinfo(Execute ptr, addr *ret)
{
	addr symbol;

	readinfo_symbol(&symbol);
	getspecial_local(ptr, symbol, ret);
	Check(*ret == Unbound, "unbound error");
	CheckType(*ret, LISPSYSTEM_READINFO);
}

struct readinfo_struct *getreadinfo_struct(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos);
}

void pushreadinfo(Execute ptr, addr *ret)
{
	addr symbol, info;

	readinfo_symbol(&symbol);
	readinfo_local(ptr->local, &info);
	pushspecial_control(ptr, symbol, info);
	*ret = info;
}

int pushreadinfo_recursive_(Execute ptr, addr *ret)
{
	unsigned preserving, replace;
	addr symbol, info, label;
	struct readinfo_struct *str;
	size_t backquote;

	/* outside read */
	readinfo_symbol(&symbol);
	getspecial_local(ptr, symbol, &info);
	if (info == Unbound)
		return fmte_("Outside read don't accept recursive-p parameter.", NULL);
	str = ReadInfoStruct(info);
	preserving = str->preserving;
	replace = str->replace;
	backquote = str->backquote;
	GetReadInfo(info, ReadInfo_Label, &label);

	/* push readinfo */
	readinfo_local(ptr->local, &info);
	str = ReadInfoStruct(info);
	str->preserving = preserving;
	str->replace = replace;
	str->backquote = backquote;
	str->recursive = 1;
	SetReadInfo(info, ReadInfo_Label, label);
	pushspecial_control(ptr, symbol, info);
	return Result(ret, info);
}

void getpackage_readinfo(Execute ptr, addr *ret)
{
	addr pos;
	getreadinfo(ptr, &pos);
	GetReadInfo(pos, ReadInfo_Package, ret);
}

void setpackage_readinfo(Execute ptr, addr value)
{
	addr pos;
	getreadinfo(ptr, &pos);
	SetReadInfo(pos, ReadInfo_Package, value);
}

void getqueue_readinfo(Execute ptr, addr *ret)
{
	addr pos;
	getreadinfo(ptr, &pos);
	GetReadInfo(pos, ReadInfo_Queue, ret);
}

unsigned getpreserving_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->preserving;
}

unsigned getescape_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->escape;
}

void setescape_readinfo(Execute ptr, unsigned value)
{
	addr pos;

	Check(1U < value, "value error");
	getreadinfo(ptr, &pos);
	ReadInfoStruct(pos)->escape = value;
}

unsigned getdot_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->dot;
}

unsigned getreplace_readinfo(Execute ptr, addr *label)
{
	unsigned ret;
	addr pos;

	getreadinfo(ptr, &pos);
	ret = ReadInfoStruct(pos)->replace;
	if (ret) {
		GetReadInfo(pos, ReadInfo_Label, &pos);
		GetCar(pos, label);
	}

	return ret;
}

enum ReadInfo_State getstate_readinfo(Execute ptr)
{
	addr pos;
	getreadinfo(ptr, &pos);
	return ReadInfoStruct(pos)->state;
}

void setstate_readinfo(Execute ptr, enum ReadInfo_State value)
{
	addr pos;
	getreadinfo(ptr, &pos);
	ReadInfoStruct(pos)->state = value;
}

void clear_readinfo(Execute ptr)
{
	addr pos, value;
	struct readinfo_struct *str;

	getreadinfo(ptr, &pos);
	/* package */
	SetReadInfo(pos, ReadInfo_Package, Nil);
	/* queue */
	GetReadInfo(pos, ReadInfo_Queue, &value);
	clear_charqueue(value);
	/* state, escape */
	str = ReadInfoStruct(pos);
	str->escape = 0;
	str->state = ReadInfo_State_First;
}

