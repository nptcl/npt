#ifndef __STREAM_FILE_HEADER__
#define __STREAM_FILE_HEADER__

#define init_stream_binary_input _n(init_stream_binary_input)
#define init_stream_binary_output _n(init_stream_binary_output)
#define init_stream_binary_io _n(init_stream_binary_io)
#define init_stream_character_input _n(init_stream_character_input)
#define init_stream_character_output _n(init_stream_character_output)
#define init_stream_character_io _n(init_stream_character_io)
#define init_stream_binchar_input _n(init_stream_binchar_input)
#define init_stream_binchar_output _n(init_stream_binchar_output)
#define init_stream_binchar_io _n(init_stream_binchar_io)
#define init_stream_probe _n(init_stream_probe)

void init_stream_binary_input(void);
void init_stream_binary_output(void);
void init_stream_binary_io(void);
void init_stream_character_input(void);
void init_stream_character_output(void);
void init_stream_character_io(void);
void init_stream_binchar_input(void);
void init_stream_binchar_output(void);
void init_stream_binchar_io(void);
void init_stream_probe(void);

#endif

