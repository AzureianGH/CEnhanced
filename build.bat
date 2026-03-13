@echo off
setlocal ENABLEEXTENSIONS

REM Usage: build.bat [preset]
REM If a CMake preset is provided, use it.
REM Otherwise auto-select a reasonable default and fall back to local configure+build.

set "PRESET=%~1"
if "%PRESET%"=="" (
	where gcc >nul 2>nul
	if not errorlevel 1 (
		set "PRESET=mingw-release"
	)
)

if not "%PRESET%"=="" (
	echo [Windows] Configuring with CMake preset "%PRESET%"...
	cmake --preset "%PRESET%" -DCHANCE_DEFAULT_RUNTIME="%CHANCE_DEFAULT_RUNTIME%" -DCHANCE_DEFAULT_STDLIB="%CHANCE_DEFAULT_STDLIB%"
	if errorlevel 1 goto FALLBACK

	echo [Windows] Building with CMake preset "%PRESET%"...
	cmake --build --preset "%PRESET%"
	if errorlevel 1 goto FALLBACK

	goto :EOF
)

goto FALLBACK

:FALLBACK
echo [Windows] Preset build failed or not selected. Falling back to local configure+build in .\build ...
if not exist build mkdir build

set "GENERATOR="
set "MAKE_PROGRAM="

where ninja >nul 2>nul
if not errorlevel 1 set "GENERATOR=Ninja"

if "%GENERATOR%"=="" (
	where mingw32-make >nul 2>nul
	if not errorlevel 1 (
		set "GENERATOR=MinGW Makefiles"
		set "MAKE_PROGRAM=mingw32-make"
	)
)

if "%GENERATOR%"=="" (
	where nmake >nul 2>nul
	if not errorlevel 1 set "GENERATOR=NMake Makefiles"
)

if "%GENERATOR%"=="" (
	echo [Windows] Build failed: no generator backend found.
	echo [Windows] Install one of: Ninja, mingw32-make, or nmake.
	exit /b 1
)

where gcc >nul 2>nul
if errorlevel 1 (
	where clang >nul 2>nul
	if errorlevel 1 (
		where cl >nul 2>nul
		if errorlevel 1 (
			echo [Windows] Build failed: no C compiler found in PATH.
			exit /b 1
		)
	)
)

set "MAKE_ARG="
if not "%MAKE_PROGRAM%"=="" set "MAKE_ARG=-DCMAKE_MAKE_PROGRAM=%MAKE_PROGRAM%"

cmake -S . -B build -G "%GENERATOR%" -DCMAKE_BUILD_TYPE=Release %MAKE_ARG% -DCHANCE_DEFAULT_RUNTIME="%CHANCE_DEFAULT_RUNTIME%" -DCHANCE_DEFAULT_STDLIB="%CHANCE_DEFAULT_STDLIB%"
if errorlevel 1 goto ERROR

cmake --build build --config Release
if errorlevel 1 goto ERROR

exit /b %ERRORLEVEL%

:ERROR
echo [Windows] Build failed.
exit /b 1
