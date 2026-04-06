@echo off
setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION

set "SCRIPT_DIR=%~dp0"
if "%SCRIPT_DIR:~-1%"=="\" set "SCRIPT_DIR=%SCRIPT_DIR:~0,-1%"

if not defined PREFIX set "PREFIX=%ProgramFiles%\chance"
set "BIN_DIR=%PREFIX%\bin"
set "SHARE_DIR=%PREFIX%\share\chance"
set "STDLIB_DIR=%SHARE_DIR%\stdlib"
set "RUNTIME_DIR=%SHARE_DIR%\runtime"

if "%~1"=="" (
  set "CHANCECODE_DIR=%SCRIPT_DIR%\..\ChanceCode"
) else (
  set "CHANCECODE_DIR=%~1"
)
if "%~2"=="" (
  set "CLD_DIR=%SCRIPT_DIR%\..\CLD"
) else (
  set "CLD_DIR=%~2"
)
if "%~3"=="" (
  set "CHS_DIR=%SCRIPT_DIR%\..\CHS"
) else (
  set "CHS_DIR=%~3"
)

for %%I in ("%CHANCECODE_DIR%") do set "CHANCECODE_DIR=%%~fI"
for %%I in ("%CLD_DIR%") do set "CLD_DIR=%%~fI"
for %%I in ("%CHS_DIR%") do set "CHS_DIR=%%~fI"

echo.
echo == Build ==
echo Building CHance compiler
set "CHANCE_DEFAULT_RUNTIME=%RUNTIME_DIR%\runtime.cclib"
set "CHANCE_DEFAULT_STDLIB=%STDLIB_DIR%\stdlib.cclib"
call "%SCRIPT_DIR%\build.bat"
if errorlevel 1 goto :error
echo Compiler build complete

if exist "%CHANCECODE_DIR%\build.bat" (
  echo Building ChanceCode
  pushd "%CHANCECODE_DIR%"
  call build.bat
  if errorlevel 1 (
    popd
    goto :error
  )
  popd
  echo ChanceCode build complete
)

set "CHANCEC_BIN=%SCRIPT_DIR%\build\chancec.exe"
if not exist "%CHANCEC_BIN%" set "CHANCEC_BIN=%SCRIPT_DIR%\build\chancec"
if not exist "%CHANCEC_BIN%" (
  echo error: chancec not found in "%SCRIPT_DIR%\build"
  goto :error
)

set "CHANCECODEC_BIN="
if exist "%SCRIPT_DIR%\build\chancecodec.exe" set "CHANCECODEC_BIN=%SCRIPT_DIR%\build\chancecodec.exe"
if "%CHANCECODEC_BIN%"=="" if exist "%SCRIPT_DIR%\build\chancecodec" set "CHANCECODEC_BIN=%SCRIPT_DIR%\build\chancecodec"
if "%CHANCECODEC_BIN%"=="" if exist "%CHANCECODE_DIR%\build\chancecodec.exe" set "CHANCECODEC_BIN=%CHANCECODE_DIR%\build\chancecodec.exe"
if "%CHANCECODEC_BIN%"=="" if exist "%CHANCECODE_DIR%\build\chancecodec" set "CHANCECODEC_BIN=%CHANCECODE_DIR%\build\chancecodec"
if not "%CHANCECODEC_BIN%"=="" echo Using chancecodec for library rebuild: %CHANCECODEC_BIN%

set "CHS_BIN="
if exist "%SCRIPT_DIR%\build\chs.exe" set "CHS_BIN=%SCRIPT_DIR%\build\chs.exe"
if "%CHS_BIN%"=="" if exist "%SCRIPT_DIR%\build\chs" set "CHS_BIN=%SCRIPT_DIR%\build\chs"
if "%CHS_BIN%"=="" if exist "%CHS_DIR%\build\chs.exe" set "CHS_BIN=%CHS_DIR%\build\chs.exe"
if "%CHS_BIN%"=="" if exist "%CHS_DIR%\build\chs" set "CHS_BIN=%CHS_DIR%\build\chs"
if not "%CHS_BIN%"=="" echo Using CHS for assembly: %CHS_BIN%

echo.
echo == Libraries ==
echo Rebuilding runtime and stdlib with built compiler

if not "%CHANCECODEC_BIN%"=="" (
  if not "%CHS_BIN%"=="" (
    pushd "%SCRIPT_DIR%\runtime"
    "%CHANCEC_BIN%" --chancecodec "%CHANCECODEC_BIN%" --chs "%CHS_BIN%" runtime.ceproj
    if errorlevel 1 (
      popd
      goto :error
    )
    popd
    pushd "%SCRIPT_DIR%\src\stdlib"
    "%CHANCEC_BIN%" --chancecodec "%CHANCECODEC_BIN%" --chs "%CHS_BIN%" stdlib.ceproj
    if errorlevel 1 (
      popd
      goto :error
    )
    popd
  ) else (
    pushd "%SCRIPT_DIR%\runtime"
    "%CHANCEC_BIN%" --chancecodec "%CHANCECODEC_BIN%" runtime.ceproj
    if errorlevel 1 (
      popd
      goto :error
    )
    popd
    pushd "%SCRIPT_DIR%\src\stdlib"
    "%CHANCEC_BIN%" --chancecodec "%CHANCECODEC_BIN%" stdlib.ceproj
    if errorlevel 1 (
      popd
      goto :error
    )
    popd
  )
) else (
  if not "%CHS_BIN%"=="" (
    pushd "%SCRIPT_DIR%\runtime"
    "%CHANCEC_BIN%" --chs "%CHS_BIN%" runtime.ceproj
    if errorlevel 1 (
      popd
      goto :error
    )
    popd
    pushd "%SCRIPT_DIR%\src\stdlib"
    "%CHANCEC_BIN%" --chs "%CHS_BIN%" stdlib.ceproj
    if errorlevel 1 (
      popd
      goto :error
    )
    popd
  ) else (
    pushd "%SCRIPT_DIR%\runtime"
    "%CHANCEC_BIN%" runtime.ceproj
    if errorlevel 1 (
      popd
      goto :error
    )
    popd
    pushd "%SCRIPT_DIR%\src\stdlib"
    "%CHANCEC_BIN%" stdlib.ceproj
    if errorlevel 1 (
      popd
      goto :error
    )
    popd
  )
)
echo Library rebuild complete

echo.
echo == Install ==
echo Prefix:  %PREFIX%
echo Bin:     %BIN_DIR%
echo Stdlib:  %STDLIB_DIR%
echo Runtime: %RUNTIME_DIR%
echo Installing artifacts

if not exist "%BIN_DIR%" mkdir "%BIN_DIR%"
if not exist "%STDLIB_DIR%" mkdir "%STDLIB_DIR%"
if not exist "%RUNTIME_DIR%" mkdir "%RUNTIME_DIR%"
if errorlevel 1 goto :error

copy /Y "%CHANCEC_BIN%" "%BIN_DIR%\chancec.exe" >nul
if errorlevel 1 goto :error

if exist "%SCRIPT_DIR%\src\stdlib\stdlib.cclib" (
  copy /Y "%SCRIPT_DIR%\src\stdlib\stdlib.cclib" "%STDLIB_DIR%\stdlib.cclib" >nul
  if errorlevel 1 goto :error
)

if exist "%SCRIPT_DIR%\runtime\runtime.cclib" (
  copy /Y "%SCRIPT_DIR%\runtime\runtime.cclib" "%RUNTIME_DIR%\runtime.cclib" >nul
  if errorlevel 1 goto :error
)

if not "%CHANCECODEC_BIN%"=="" (
  copy /Y "%CHANCECODEC_BIN%" "%BIN_DIR%\chancecodec.exe" >nul
  if errorlevel 1 goto :error
)

if exist "%CLD_DIR%\build\cld.exe" (
  copy /Y "%CLD_DIR%\build\cld.exe" "%BIN_DIR%\cld.exe" >nul
  if errorlevel 1 goto :error
) else if exist "%CLD_DIR%\build\cld" (
  copy /Y "%CLD_DIR%\build\cld" "%BIN_DIR%\cld.exe" >nul
  if errorlevel 1 goto :error
)

if exist "%CHS_DIR%\build\chs.exe" (
  copy /Y "%CHS_DIR%\build\chs.exe" "%BIN_DIR%\chs.exe" >nul
  if errorlevel 1 goto :error
) else if exist "%CHS_DIR%\build\chs" (
  copy /Y "%CHS_DIR%\build\chs" "%BIN_DIR%\chs.exe" >nul
  if errorlevel 1 goto :error
)

REM install path variables, permanent
setx /M CHANCE_PREFIX "%PREFIX%" >nul
setx /M CHANCE_BIN_DIR "%BIN_DIR%" >nul
setx /M CHANCE_SHARE_DIR "%SHARE_DIR%" >nul
setx /M CHANCE_STDLIB_DIR "%STDLIB_DIR%" >nul
setx /M CHANCE_RUNTIME_DIR "%RUNTIME_DIR%" >nul

echo Done
exit /b 0

:error
echo Install failed.
exit /b 1
