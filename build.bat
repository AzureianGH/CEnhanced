@echo off
setlocal ENABLEEXTENSIONS

REM Usage: build.bat [preset]
REM If a CMake preset is provided (default: mingw-release), use it.
REM Otherwise, fall back to a local MinGW Makefiles configure+build in ./build.

set "PRESET=%~1"
if "%PRESET%"=="" set "PRESET=mingw-release"

echo [Windows] Building with CMake preset "%PRESET%"...
cmake --build --preset "%PRESET%"
if errorlevel 1 goto FALLBACK

goto :EOF

:FALLBACK
echo [Windows] Preset build failed. Falling back to MinGW Makefiles in .\build ...
if not exist build mkdir build
cmake -S . -B build -G "MinGW Makefiles" -DCMAKE_BUILD_TYPE=Release
if errorlevel 1 goto ERROR
cmake --build build --config Release
if errorlevel 1 goto ERROR

exit /b %ERRORLEVEL%

:ERROR
echo [Windows] Build failed.
exit /b 1
