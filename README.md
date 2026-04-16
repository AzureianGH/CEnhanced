# CHanceC Compiler

`chancec` is the front-end compiler for CHance.

It parses `.ce` source files, runs semantic analysis, emits ChanceCode bytecode (`.ccb`), and can drive backend compilation and linking through the CHance toolchain.

## How to Install
Clone these repositories like:

```bash
git clone https://github.com/Aquanite/CEnhanced.git
git clone https://github.com/Aquanite/ChanceCode.git
git clone https://github.com/Aquanite/CHS.git
git clone https://github.com/Aquanite/CLD.git
```

Then build `chancec` and related tools:

```bash
cd CEnhanced
./install.sh
```
OR with PREFIX on Windows
```
cd CEnhanced
.\install.bat
```
## Language Standards

- H26 specification: [`H26.md`](H26.md)
- H27 specification: [`H27.md`](H27.md)
- H28 draft overlay: [`draftH28.md`](draftH28.md)

H28 is the default language mode.

- Use `-H28` for full current language behavior.
- Use `-H27` for H27 compatibility mode.
- Use `-H26` for H26 compatibility mode.

## Repository Layout

- `src/` - `chancec` compiler source
- `runtime/` - runtime sources and runtime library project
- `tests/` - compiler and language tests
- `build.sh` / `build.bat` - build scripts
- `install.sh` - install script for toolchain artifacts

## Building

From the `CEnhanced` directory:

```bash
./build.sh
```

This builds `chancec` and related toolchain pieces used by this workspace.

For BSlash targets, `chancec` uses CHS as the assembler stage. CHS now embeds the BSlash assembler core internally, so the normal CHance toolchain no longer requires a separate external `bas` executable for BSlash assembly.

## Basic Usage

Compile a source file:

```bash
chancec -x86 hello.ce
```

Stop after bytecode emission:

```bash
chancec -H28 -Sccb hello.ce
```

Compile in H26 mode:

```bash
chancec -H26 -x86 hello.ce
```

Compile from a project file:

```bash
chancec app.ceproj
```

See all flags:

```bash
chancec --help
```

## Notes

- By default (non-freestanding), `chancec` integrates stdlib/runtime libraries during normal builds.
- `.ceproj` can pass extra compiler flags via `args=` / `control_args=`.
