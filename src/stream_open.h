#ifndef __STREAM_OPEN_HEADER__
#define __STREAM_OPEN_HEADER__

#include "execute.h"
#include "typedef.h"

#define upgrade_open_element_type_stream_ _n(upgrade_open_element_type_stream_)
#define open_element_stream_ _n(open_element_stream_)
#define open_stream_ _n(open_stream_)

enum Stream_Open_Direction {
	Stream_Open_Direction_Input,
	Stream_Open_Direction_Output,
	Stream_Open_Direction_Io,
	Stream_Open_Direction_Probe
};

enum Stream_Open_Element {
	Stream_Open_Element_Character,
	Stream_Open_Element_Bit,
	Stream_Open_Element_Unsigned8,
	Stream_Open_Element_Unsigned16,
	Stream_Open_Element_Unsigned32,
	Stream_Open_Element_Unsigned64,
	Stream_Open_Element_Signed8,
	Stream_Open_Element_Signed16,
	Stream_Open_Element_Signed32,
	Stream_Open_Element_Signed64
};

enum Stream_Open_IfExists {
	Stream_Open_IfExists_Error,
	Stream_Open_IfExists_NewVersion,
	Stream_Open_IfExists_Rename,
	Stream_Open_IfExists_RenameAndDelete,
	Stream_Open_IfExists_Overwrite,
	Stream_Open_IfExists_Append,
	Stream_Open_IfExists_Supersede,
	Stream_Open_IfExists_Nil
};

enum Stream_Open_IfDoesNot {
	Stream_Open_IfDoesNot_Create,
	Stream_Open_IfDoesNot_Error,
	Stream_Open_IfDoesNot_Nil
};

enum Stream_Open_External {
	Stream_Open_External_Default,
	Stream_Open_External_Ascii,
	Stream_Open_External_Utf8,
	Stream_Open_External_Utf8Bom,
	Stream_Open_External_Utf16,
	Stream_Open_External_Utf16Le,
	Stream_Open_External_Utf16Be,
	Stream_Open_External_Utf16LeBom,
	Stream_Open_External_Utf16BeBom,
	Stream_Open_External_Utf32,
	Stream_Open_External_Utf32Le,
	Stream_Open_External_Utf32Be,
	Stream_Open_External_Utf32LeBom,
	Stream_Open_External_Utf32BeBom
};

int upgrade_open_element_type_stream_(addr var, addr *ret);
int open_element_stream_(Execute ptr, addr value, enum Stream_Open_Element *ret);
int open_stream_(Execute ptr, addr *ret, addr pos,
		enum Stream_Open_Direction direction,
		enum Stream_Open_Element element,
		enum Stream_Open_IfExists exist,
		enum Stream_Open_IfDoesNot doesnot,
		enum Stream_Open_External external);

#endif

