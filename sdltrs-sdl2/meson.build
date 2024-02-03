project('sdl2trs', 'c',
	version: '1.2.28',
	license: 'BSD',
	default_options: [ 'buildtype=debugoptimized', 'c_std=gnu89', 'warning_level=2' ],
	meson_version: '>= 0.36.0'
)

add_project_arguments('-Wno-unused-parameter', '-Wno-unused-result', language: 'c')

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
sdl	 = dependency('sdl2', required : true)

message('host machine endian: ' + endian)
if endian == 'big'
	add_project_arguments('-Dbig_endian', language : 'c')
endif

if get_option('FASTMOVE')
	add_project_arguments('-DFAST_MOVE', language : 'c')
	message('Fast inaccurate Z80 block moves')
endif

if get_option('OLDSCAN')
	add_project_arguments('-DOLD_SCANLINES', language : 'c')
	message('Display Scanlines using old method')
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

executable('sdl2trs', sources, dependencies : [ readline, sdl ])
