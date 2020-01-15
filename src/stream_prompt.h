#ifndef __STREAM_PROMPT_HEADER__
#define __STREAM_PROMPT_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

enum PromptStreamMode {
	PromptStreamMode_Normal,
	PromptStreamMode_Inspect,
	PromptStreamMode_Step
};

_g void open_prompt_stream(addr *stream);
_g void mode_prompt_stream(Execute ptr, enum PromptStreamMode mode);
_g void init_stream_prompt(void);

#endif

