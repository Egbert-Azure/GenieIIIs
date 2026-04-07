cmake_minimum_required(VERSION 2.8.5...3.13)

project(sdltrs C)

include(GNUInstallDirs)
include(TestBigEndian)

set(CMAKE_C_FLAGS
	"-g -Wall -Wextra -Wno-unused-parameter -Wno-unused-result -O2 ${CMAKE_C_FLAGS}"
)

set(SOURCES
	src/debug.c
	src/dis.c
	src/error.c
	src/main.c
	src/trs_cassette.c
	src/trs_clones.c
	src/trs_cmd_rom.c
	src/trs_cp500.c
	src/trs_disk.c
	src/trs_hard.c
	src/trs_imp_exp.c
	src/trs_interrupt.c
	src/trs_io.c
	src/trs_memory.c
	src/trs_mkdisk.c
	src/trs_options.c
	src/trs_printer.c
	src/trs_sdl_gui.c
	src/trs_sdl_interface.c
	src/trs_sdl_keyboard.c
	src/trs_state_save.c
	src/trs_stringy.c
	src/trs_uart.c
	src/z80.c
	src/PasteManager.c
)

add_executable(sdltrs ${SOURCES})

add_definitions(-DROM_PATH="${ROM_PATH}")
message("-- ROM path set to: '${ROM_PATH}'")

test_big_endian(BIGENDIAN)
if (${BIGENDIAN})
	add_definitions(-Dbig_endian)
endif ()

option(DISKIMG	"Install disk images with utilities"	ON)
option(FASTMOVE	"Fast inaccurate Z80 block moves"	OFF)
option(HTMLDOC	"Install documentation in HTML format"	ON)
option(ICONS	"Install icons and desktop file"	ON)
option(OLDSCAN	"Display Scanlines using old method"	OFF)
option(NOX	"Build SDL 1.2 version without X"	OFF)
option(READLINE	"Readline support for zbx debugger"	ON)
option(SDL1	"Use SDL version 1.2 instead of SDL2"	OFF)
option(ZBX	"Build with integrated Z80 debugger"	ON)

if (FASTMOVE)
	add_definitions(-DFAST_MOVE)
	message("-- Fast inaccurate Z80 block moves")
endif ()

if (OLDSCAN)
	add_definitions(-DOLD_SCANLINES)
	message("-- Display Scanlines using old method")
endif ()

if (READLINE AND ZBX)
	find_path(Readline_ROOT_DIR
		NAMES include/readline/readline.h
	)
	find_path(Readline_INCLUDE_DIR
		NAMES readline/readline.h
		HINTS ${Readline_ROOT_DIR}/include
	)
	find_library(Readline_LIBRARY
		NAMES readline
		HINTS ${Readline_ROOT_DIR}/lib
	)
	if (Readline_LIBRARY)
		find_package(Curses QUIET)
		if (CURSES_LIBRARIES)
			include_directories(${CURSES_INCLUDE_DIR})
			target_link_libraries(sdltrs ${CURSES_LIBRARIES})
		endif ()
		add_definitions(-DREADLINE)
		message("-- Readline support for zbx debugger")
		include_directories(${Readline_INCLUDE_DIR})
		target_link_libraries(sdltrs ${Readline_LIBRARY})
	else ()
		message("-- Readline NOT FOUND")
	endif ()
endif ()

if (SDL1 OR NOX)
	if (NOX)
		add_definitions(-DNOX)
		message("-- Build SDL 1.2 version without X")
	else ()
		find_package(X11 REQUIRED)
		message("-- Use SDL version 1.2 instead of SDL2")
		include_directories(${X11_INCLUDE_DIR})
		target_link_libraries(sdltrs ${X11_LIBRARIES})
	endif ()
	set(SDL_CONFIG "sdl-config")
else ()
	add_definitions(-DSDL2)
	set(SDL_CONFIG "sdl2-config")
endif ()

find_program(SDL "${SDL_CONFIG}")
if (NOT SDL)
	message(FATAL_ERROR "${SDL_CONFIG} NOT FOUND")
else ()
	execute_process(COMMAND ${SDL} --cflags
			OUTPUT_VARIABLE SDL_CFLAGS
			OUTPUT_STRIP_TRAILING_WHITESPACE)
	execute_process(COMMAND ${SDL} --libs
			OUTPUT_VARIABLE SDL_LIBS
			OUTPUT_STRIP_TRAILING_WHITESPACE)
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${SDL_CFLAGS}")
	if (ZBX)
		add_definitions(-DZBX)
		message("-- Build with integrated Z80 debugger")
		if (MINGW OR MSYS OR WIN32)
			string(REPLACE " -mwindows" "" SDL_LIBS "${SDL_LIBS}")
		endif ()
	endif ()
	message("-- Found SDL: ${SDL_LIBS}")
	target_link_libraries(sdltrs ${SDL_LIBS})
endif ()

install(TARGETS sdltrs		DESTINATION ${CMAKE_INSTALL_BINDIR}/)
install(FILES src/sdltrs.1	DESTINATION ${CMAKE_INSTALL_MANDIR}/man1/)
install(FILES LICENSE		DESTINATION ${CMAKE_INSTALL_DOCDIR}/)

if (HTMLDOC)
message("-- Install documentation in HTML format")
install(DIRECTORY html		DESTINATION ${CMAKE_INSTALL_DOCDIR}/)
endif ()

if (DISKIMG)
message("-- Install disk images with utilities")
install(DIRECTORY diskimages/	DESTINATION ${CMAKE_INSTALL_DATADIR}/sdltrs/)
endif ()

if (ICONS)
message("-- Install icons and desktop file")
install(FILES sdltrs.desktop	DESTINATION ${CMAKE_INSTALL_DATADIR}/applications/)
install(FILES icons/sdltrs.png	DESTINATION ${CMAKE_INSTALL_DATADIR}/icons/hicolor/48x48/apps/)
install(FILES icons/sdltrs.svg	DESTINATION ${CMAKE_INSTALL_DATADIR}/icons/hicolor/scalable/apps/)
install(FILES icons/sdltrs.xpm	DESTINATION ${CMAKE_INSTALL_DATADIR}/pixmaps/)
endif ()
