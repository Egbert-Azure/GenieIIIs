project('sdltrs', 'c',
	version: '1.2.34',
	license: 'BSD',
	default_options: [ 'buildtype=debugoptimized', 'c_std=gnu89', 'warning_level=2' ],
	meson_version: '>= 0.36.0'
)

add_project_arguments('-Wno-unused-parameter', '-Wno-unused-result', language: 'c')
add_project_arguments('-DROM_PATH=' + get_option('ROM_PATH'), language: 'c')

sources = files([
	'src/debug.c',
	'src/dis.c',
	'src/error.c',
	'src/main.c',
	'src/trs_cassette.c',
	'src/trs_clones.c',
	'src/trs_cmd_rom.c',
	'src/trs_cp500.c',
	'src/trs_disk.c',
	'src/trs_hard.c',
	'src/trs_imp_exp.c',
	'src/trs_interrupt.c',
	'src/trs_io.c',
	'src/trs_memory.c',
	'src/trs_mkdisk.c',
	'src/trs_options.c',
	'src/trs_printer.c',
	'src/trs_sdl_gui.c',
	'src/trs_sdl_interface.c',
	'src/trs_sdl_keyboard.c',
	'src/trs_state_save.c',
	'src/trs_stringy.c',
	'src/trs_uart.c',
	'src/z80.c',
	'src/PasteManager.c'
])

compiler = meson.get_compiler('c')
endian	 = host_machine.endian()
readline = declare_dependency()
x11	 = declare_dependency()

message('host machine endian: ' + endian)
if endian == 'big'
	add_project_arguments('-Dbig_endian', language : 'c')
endif

message('ROM path set to: ' + get_option('ROM_PATH'))

if get_option('FASTMOVE')
	add_project_arguments('-DFAST_MOVE', language : 'c')
	message('Fast inaccurate Z80 block moves')
endif

if get_option('OLDSCAN')
	add_project_arguments('-DOLD_SCANLINES', language : 'c')
	message('Display Scanlines using old method')
endif

if get_option('NOX')
	sdl = dependency('sdl', required : true)
	add_project_arguments('-DNOX', language : 'c')
	message('Build SDL 1.2 version without X')
else
	if get_option('SDL1')
		sdl = dependency('sdl', required : true)
		x11 = dependency('x11', required : true)
		message('Use SDL version 1.2 instead of SDL2')
	else
		sdl = dependency('sdl2', required : true)
		if sdl.found()
			add_project_arguments('-DSDL2', language : 'c')
		else
			sdl = dependency('sdl', required : true)
			x11 = dependency('x11', required : true)
			message('SDL2 not found, falling back to SDL1')
		endif
	endif
endif

if get_option('ZBX')
	add_project_arguments('-DZBX', language : 'c')
	message('Build with integrated Z80 debugger')
	if get_option('READLINE')
		readline = compiler.find_library('readline', required : false)
		if readline.found()
			add_project_arguments('-DREADLINE', language : 'c')
			message('Readline support for zbx debugger')
		endif
	endif
endif

executable('sdltrs', sources, dependencies : [ readline, sdl, x11 ])
