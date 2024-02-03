AUTOMAKE_OPTIONS=foreign

AM_CFLAGS=	-Wall

bin_PROGRAMS=	sdl2trs
dist_man_MANS=	src/sdl2trs.1

sdl2trs_SOURCES=src/debug.c \
		src/dis.c \
		src/error.c \
		src/main.c \
		src/trs_cassette.c \
		src/trs_clones.c \
		src/trs_cmd_rom.c \
		src/trs_cp500.c \
		src/trs_disk.c \
		src/trs_hard.c \
		src/trs_imp_exp.c \
		src/trs_interrupt.c \
		src/trs_io.c \
		src/trs_memory.c \
		src/trs_mkdisk.c \
		src/trs_options.c \
		src/trs_printer.c \
		src/trs_sdl_gui.c \
		src/trs_sdl_interface.c \
		src/trs_sdl_keyboard.c \
		src/trs_state_save.c \
		src/trs_stringy.c \
		src/trs_uart.c \
		src/z80.c \
		src/PasteManager.c

appicondir=	$(datadir)/icons/hicolor/scalable/apps
appicon_DATA=	icons/sdl2trs.svg

diskimagedir=	$(datadir)/@PACKAGE@
diskimage_DATA=	diskimages/*.dsk

desktopdir=	$(datadir)/applications
desktop_DATA=	sdl2trs.desktop

docdir=		$(datadir)/doc/@PACKAGE@
nobase_doc_DATA=html/*.html html/images/*.png

icondir=	$(datadir)/icons/hicolor/48x48/apps
icon_DATA=	icons/sdl2trs.png

pixmapdir=	$(datadir)/pixmaps
pixmap_DATA=	icons/sdl2trs.xpm
