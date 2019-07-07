if LISP_DEGRADE
CFLAGS += -Isrc
include src/source.mk
else
include src/release.mk
endif

CFLAGS += $(lisp_optimize)
VPATH = src:test
bin_PROGRAMS = npt
npt_SOURCES = $(source)

