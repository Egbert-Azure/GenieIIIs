**SDL2TRS** needs the development files of `SDL2` and optional
`GNU readline` for the integrated Z80 debugger zbx.

On *Debian* and *Ubuntu* based systems these can be installed with:
```sh
sudo apt install libsdl2-dev libreadline-dev
```

## Windows prerequesits

For *Win32/64* please install [MinGW] or [MinGW-w64] and the [SDL]
development library with the `mingw.tar.gz` file extension.
It is recommended to use [MSYS2] to make things easier:

- Get the latest version of Mingw-w64 via MSYS2, which provides up-to-date native builds of GCC, Mingw-w64, and other helpful C++ tools and libraries. You can use this [link](https://github.com/msys2/msys2-installer/releases/download/2022-06-03/msys2-x86_64-20220603.exe) to the installer.

- Follow the Installation instructions on the MSYS2 website to install Mingw-w64 and install the Mingw-w64 toolchain (`pacman -S --needed base-devel mingw-w64-x86_64-toolchain`) a MSYS2 terminal. Accept the default to install all the members in the `toolchain` group
- Add the path to your Mingw-w64 bin folder to the Windows PATH environment variable by using the following steps: type `settings` to open your Windows Settings. Edit environment variables and
select new to `C:\msys64\mingw64\bin`
- Add SDL2 package for 64 bit system with `pacman -S mingw-w64-x86_64-SDL2`

Visual Studio Creator can be used for coding. Add the extensions by searching for `c++` in the Extensions view (`Ctrl+Shift+X`).

---

To build with autotools:
------------------------

Installation of `aclocal`, `autoconf` and `automake` is required:
```sh
sudo apt install autoconf automake autotools-dev
```
on *Debian* and *Ubuntu* systems.

From the main directory, execute:
```sh
./autogen.sh
```
which will generate the `configure` script.

To configure the build system, execute:
```sh
./configure
```

This should autodetect the configuration but some options may be passed:
```sh
./configure --enable-fastmove
```
to enable faster but not accurate Z80 block moves,
```sh
./configure --enable-oldscan
```
to enable old method to display Scanlines,
```
./configure --enable-zbx
```
to build with the integrated Z80 debugger zbx,
```sh
./configure --enable-zbx --enable-readline
```
to enable `readline` support for the zbx debugger.

Start build of the program in the main directory by executing:
```sh
make
```

---

To build with CMake:
--------------------

From the main directory, execute:
```sh
mkdir -p build && cd build && cmake .. && cmake --build .
```

---

To build with Makefiles:
------------------------

From the `src` directory, execute:
```sh
make sdl2
```
to build the SDL2 version,

```sh
make bsd
```
(or just `make` on *FreeBSD*/*OpenBSD*) to build on BSD with SDL2.

For *Win32/64* please copy the header files of the SDL2 library to
`\MinGW\include\SDL2` (or `\MinGW64\include\SDL2` for [MinGW-w64]),
and libraries to the `\MinGW\lib\` (or `\MinGW64\lib`) directory,
or edit the macros `SDL_INC` and `SDL_LIB` in `Makefile` to point
to the SDL2 installation location:
```sh
mingw32-make win32
```
to build the SDL2 version, or
```sh
mingw32-make win64
```
to build the 64-bit SDL2 version.

---

To build on macOS:
------------------

Download and install [Homebrew] for macOS first.
```sh
brew install autoconf automake libtool llvm readline sdl2
```
should download and install the required packages to build **SDL2TRS**.
In the main directory of the source, execute the following commands:
```sh
./autogen.sh
./configure --enable-readline
make
```
This will build the executable binary.

[Homebrew]: https://brew.sh
[MinGW]: https://osdn.net/projects/mingw/
[MinGW-w64]: http://mingw-w64.org
[MSYS2]: https://www.msys2.org
[SDL]: https://www.libsdl.org
