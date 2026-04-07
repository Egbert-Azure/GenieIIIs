PROG	 = sdltrs

SRCS	+= debug.c
SRCS	+= dis.c
SRCS	+= error.c
SRCS	+= main.c
SRCS	+= trs_cassette.c
SRCS	+= trs_clones.c
SRCS	+= trs_cmd_rom.c
SRCS	+= trs_cp500.c
SRCS	+= trs_disk.c
SRCS	+= trs_hard.c
SRCS	+= trs_imp_exp.c
SRCS	+= trs_interrupt.c
SRCS	+= trs_io.c
SRCS	+= trs_memory.c
SRCS	+= trs_mkdisk.c
SRCS	+= trs_options.c
SRCS	+= trs_printer.c
SRCS	+= trs_sdl_gui.c
SRCS	+= trs_sdl_interface.c
SRCS	+= trs_sdl_keyboard.c
SRCS	+= trs_state_save.c
SRCS	+= trs_stringy.c
SRCS	+= trs_uart.c
SRCS	+= z80.c
SRCS	+= PasteManager.c

OBJS	 = ${SRCS:.c=.o}

.PHONY: all bsd clean depend nox os2 sdl sdl2 win32 win64 wsdl2

all:
	@echo "make (bsd | clean | depend | nox | os2 | sdl | sdl2 | win32 | win64 | wsdl2)"

bsd:
	make -f BSDmakefile

clean:
	rm -f ${OBJS} ${PROG}

depend:
	makedepend -Y -- ${CFLAGS} -- ${SRCS} 2>&1 | \
		(egrep -v 'cannot find|not in' || true)

nox:	ENDIAN	 = $(shell echo "ab" | od -x | grep "6261" > /dev/null || echo "-Dbig_endian")
nox:	SDL_INC	?= $(shell sdl-config --cflags)
nox:	SDL_LIB	?= $(shell sdl-config --libs)
nox:	MACROS	+= -DNOX
nox:	READLINE?= -DREADLINE
nox:	${PROG}

os2:	SDL_INC	?= $(shell sdl-config --cflags)
os2:	SDL_LIB	?= $(shell sdl-config --libs)
os2:	MACROS	+= -DNOX
os2:	LDFLAGS	+= -Zomf
os2:	${PROG}

sdl:	ENDIAN	 = $(shell echo "ab" | od -x | grep "6261" > /dev/null || echo "-Dbig_endian")
sdl:	SDL_INC	?= $(shell sdl-config --cflags)
sdl:	SDL_LIB	?= $(shell sdl-config --libs)
sdl:	READLINE?= -DREADLINE
sdl:	X11INC	?= -I/usr/include/X11
sdl:	X11LIB	?= -L/usr/lib/X11 -lX11
sdl:	${PROG}

sdl2:	ENDIAN	 = $(shell echo "ab" | od -x | grep "6261" > /dev/null || echo "-Dbig_endian")
sdl2:	SDL_INC	?= $(shell sdl2-config --cflags)
sdl2:	SDL_LIB	?= $(shell sdl2-config --libs)
sdl2:	MACROS	+= -DSDL2
sdl2:	READLINE?= -DREADLINE
sdl2:	${PROG}

win32:	MINGW	?= \MinGW
win32:	CC	 = ${MINGW}\bin\gcc.exe
win32:	SDL_INC	?= -I${MINGW}\include\SDL
win32:	SDL_LIB	?= -L${MINGW}\lib -lmingw32 -lSDLmain -lSDL
win32:	${PROG}

win64:	MINGW64	?= \MinGW64
win64:	CC	 = ${MINGW64}\bin\gcc.exe
win64:	SDL_INC	?= -I${MINGW64}\include\SDL2
win64:	SDL_LIB	?= -L${MINGW64}\lib -lmingw32 -lSDL2main -lSDL2
win64:	MACROS	+= -DSDL2
win64:	${PROG}

wsdl2:	MINGW	?= \MinGW
wsdl2:	CC	 = ${MINGW}\bin\gcc.exe
wsdl2:	SDL_INC	?= -I${MINGW}\include\SDL2
wsdl2:	SDL_LIB	?= -L${MINGW}\lib -lmingw32 -lSDL2main -lSDL2
wsdl2:	MACROS	+= -DSDL2
wsdl2:	${PROG}

MSYS		:=$(if $(findstring Msys, $(shell uname -a)), Msys)
SSPLIB		:=$(if ${MSYS},-lssp,)
READLINELIBS	 =$(if ${READLINE},-lreadline,)
MACROS		+= -DROM_PATH='"${ROM_PATH}"'
ZBX		?= -DZBX
CFLAGS		?= -g -O2 -Wall
CFLAGS		+= ${SDL_INC} ${X11INC} ${ENDIAN} ${MACROS} ${READLINE} ${ZBX}

${PROG}: ${OBJS}
	${CC} -o ${PROG} ${OBJS} ${SDL_LIB} ${X11LIB} ${LDFLAGS} ${READLINELIBS} ${SSPLIB}
