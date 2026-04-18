$ErrorActionPreference = 'Stop'

function Write-Section {
    param([string]$Name)
    Write-Host ""
    Write-Host "== $Name =="
}

function Write-Info {
    param([string]$Message)
    Write-Host $Message
}

function Write-Warn {
    param([string]$Message)
    Write-Host "warning: $Message"
}

function Fail {
    param([string]$Message)
    throw $Message
}

function Resolve-AbsolutePath {
    param([Parameter(Mandatory = $true)][string]$Path)

    try {
        return (Resolve-Path -LiteralPath $Path).Path
    }
    catch {
        return [System.IO.Path]::GetFullPath($Path)
    }
}

function Find-Make {
    foreach ($candidate in @('make', 'mingw32-make')) {
        if (Get-Command $candidate -ErrorAction SilentlyContinue) {
            return $candidate
        }
    }
    return $null
}

function Get-PreferredCCompiler {
    if (-not [string]::IsNullOrWhiteSpace($env:CC)) {
        return $env:CC
    }

    if (Get-Command gcc -ErrorAction SilentlyContinue) {
        return 'gcc'
    }

    return $null
}

function Invoke-External {
    param(
        [Parameter(Mandatory = $true)][string]$FilePath,
        [string[]]$Arguments = @(),
        [string]$WorkingDirectory,
        [hashtable]$Environment = @{},
        [switch]$AllowFailure
    )

    $oldValues = @{}
    $hadValue = @{}

    foreach ($key in $Environment.Keys) {
        $envPath = "Env:$key"
        $existing = Get-Item -Path $envPath -ErrorAction SilentlyContinue
        if ($null -ne $existing) {
            $hadValue[$key] = $true
            $oldValues[$key] = $existing.Value
        }
        else {
            $hadValue[$key] = $false
        }
        Set-Item -Path $envPath -Value ([string]$Environment[$key])
    }

    try {
        if ($WorkingDirectory) {
            Push-Location $WorkingDirectory
        }

        & $FilePath @Arguments
        $exitCode = $LASTEXITCODE

        if ($exitCode -ne 0 -and -not $AllowFailure) {
            $joinedArgs = ($Arguments -join ' ')
            Fail ([string]::Format('command failed with exit code {0}: {1} {2}', $exitCode, $FilePath, $joinedArgs))
        }

        return $exitCode
    }
    finally {
        if ($WorkingDirectory) {
            Pop-Location
        }

        foreach ($key in $Environment.Keys) {
            $envPath = "Env:$key"
            if ($hadValue[$key]) {
                Set-Item -Path $envPath -Value $oldValues[$key]
            }
            else {
                Remove-Item -Path $envPath -ErrorAction SilentlyContinue
            }
        }
    }
}

function Get-FirstExistingPath {
    param([string[]]$Candidates)

    foreach ($candidate in $Candidates) {
        if (Test-Path -LiteralPath $candidate) {
            return (Resolve-Path -LiteralPath $candidate).Path
        }
    }

    return $null
}

function New-DirectoriesIfMissing {
    param([string[]]$Paths)

    try {
        foreach ($path in $Paths) {
            if (-not (Test-Path -LiteralPath $path)) {
                New-Item -ItemType Directory -Path $path -Force | Out-Null
            }
        }
        return $true
    }
    catch {
        return $false
    }
}

function Copy-RequiredFile {
    param(
        [Parameter(Mandatory = $true)][string]$Source,
        [Parameter(Mandatory = $true)][string]$Destination
    )

    if (-not (Test-Path -LiteralPath $Source)) {
        Fail "required file not found: $Source"
    }

    Copy-Item -LiteralPath $Source -Destination $Destination -Force
}

function Copy-IfExists {
    param(
        [Parameter(Mandatory = $true)][string]$Source,
        [Parameter(Mandatory = $true)][string]$Destination
    )

    if (Test-Path -LiteralPath $Source) {
        Copy-Item -LiteralPath $Source -Destination $Destination -Force
        return $true
    }

    return $false
}

try {
    $rawArgs = @($args)
    $makeCores = $null
    $positionals = New-Object System.Collections.Generic.List[string]

    for ($i = 0; $i -lt $rawArgs.Count; $i++) {
        $arg = [string]$rawArgs[$i]

        if ($arg -eq '--') {
            for ($j = $i + 1; $j -lt $rawArgs.Count; $j++) {
                $positionals.Add([string]$rawArgs[$j])
            }
            break
        }

        if ($arg.StartsWith('--cores=')) {
            $rawValue = $arg.Substring(8)
            if (-not [int]::TryParse($rawValue, [ref]$makeCores) -or $makeCores -le 0) {
                Fail '--cores must be a positive integer'
            }
            continue
        }

        if ($arg -eq '--cores') {
            if ($i + 1 -ge $rawArgs.Count) {
                Fail '--cores requires a value'
            }
            $i++
            $rawValue = [string]$rawArgs[$i]
            if (-not [int]::TryParse($rawValue, [ref]$makeCores) -or $makeCores -le 0) {
                Fail '--cores must be a positive integer'
            }
            continue
        }

        if ($arg.StartsWith('-')) {
            Fail "unknown option '$arg'"
        }

        $positionals.Add($arg)
    }

    if ($positionals.Count -gt 4) {
        Write-Warn 'extra positional arguments after the first 4 are ignored'
    }

    $scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

    $chanceCodeDir = if ($positionals.Count -ge 1) { $positionals[0] } else { Join-Path $scriptDir '..\ChanceCode' }
    $cldDir = if ($positionals.Count -ge 2) { $positionals[1] } else { Join-Path $scriptDir '..\CLD' }
    $chsDir = if ($positionals.Count -ge 3) { $positionals[2] } else { Join-Path $scriptDir '..\CHS' }
    $cvmDir = if ($positionals.Count -ge 4) { $positionals[3] } else { Join-Path $scriptDir '..\CVM' }

    $chanceCodeDir = Resolve-AbsolutePath $chanceCodeDir
    $cldDir = Resolve-AbsolutePath $cldDir
    $chsDir = Resolve-AbsolutePath $chsDir
    $cvmDir = Resolve-AbsolutePath $cvmDir

    $prefix = $env:PREFIX
    $prefixIsDefault = $false
    if ([string]::IsNullOrWhiteSpace($prefix)) {
        $prefix = Join-Path $env:ProgramFiles 'chance'
        $prefixIsDefault = $true
    }

    $binDir = Join-Path $prefix 'bin'
    $shareDir = Join-Path $prefix 'share\chance'
    $stdlibDir = Join-Path $shareDir 'stdlib'
    $runtimeDir = Join-Path $shareDir 'runtime'

    $defaultRuntime = (Join-Path $runtimeDir 'runtime.cclib').Replace('\\', '/')
    $defaultStdlib = (Join-Path $stdlibDir 'stdlib.cclib').Replace('\\', '/')
    $preferredCCompiler = Get-PreferredCCompiler
    if ($preferredCCompiler) {
        Write-Info "Using C compiler for make projects: $preferredCCompiler"
    }

    Write-Section 'Build'
    Write-Info 'Building CHance compiler'
    Invoke-External -FilePath (Join-Path $scriptDir 'build.bat') -WorkingDirectory $scriptDir -Environment @{
        CHANCE_DEFAULT_RUNTIME = $defaultRuntime
        CHANCE_DEFAULT_STDLIB = $defaultStdlib
    } | Out-Null
    Write-Info 'Compiler build complete'

    $chanceCodeBuildBat = Join-Path $chanceCodeDir 'build.bat'
    if (Test-Path -LiteralPath $chanceCodeBuildBat) {
        Write-Info 'Building ChanceCode'
        Invoke-External -FilePath $chanceCodeBuildBat -WorkingDirectory $chanceCodeDir | Out-Null
        Write-Info 'ChanceCode build complete'
    }

    $cldMakefile = Join-Path $cldDir 'Makefile'
    if (Test-Path -LiteralPath $cldMakefile) {
        $makeBin = Find-Make
        if ($null -eq $makeBin) {
            Fail 'no make tool found (need make or mingw32-make)'
        }
        else {
            Write-Info 'Building CLD'
            $makeArgs = @()
            if ($null -ne $makeCores) {
                $makeArgs += @('-j', [string]$makeCores)
            }
            if ($preferredCCompiler) {
                $makeArgs += "CC=$preferredCCompiler"
            }
            Invoke-External -FilePath $makeBin -Arguments $makeArgs -WorkingDirectory $cldDir | Out-Null
            Write-Info 'CLD build complete'
        }
    }

    $chsMakefile = Join-Path $chsDir 'Makefile'
    if (Test-Path -LiteralPath $chsMakefile) {
        $makeBin = Find-Make
        if ($null -eq $makeBin) {
            Fail 'no make tool found (need make or mingw32-make)'
        }
        else {
            Write-Info 'Building CHS'
            $makeArgs = @()
            if ($null -ne $makeCores) {
                $makeArgs += @('-j', [string]$makeCores)
            }
            if ($preferredCCompiler) {
                $makeArgs += "CC=$preferredCCompiler"
            }
            Invoke-External -FilePath $makeBin -Arguments $makeArgs -WorkingDirectory $chsDir | Out-Null
            Write-Info 'CHS build complete'
        }
    }

    $cvmCmakeLists = Join-Path $cvmDir 'CMakeLists.txt'
    if (Test-Path -LiteralPath $cvmCmakeLists) {
        if (-not (Get-Command cmake -ErrorAction SilentlyContinue)) {
            Fail 'cmake is required to build CVM'
        }

        $cvmCacheFile = Join-Path $cvmDir 'build\CMakeCache.txt'
        $needCvmConfigure = $true
        if ($env:CHANCE_FORCE_CONFIGURE -ne '1' -and (Test-Path -LiteralPath $cvmCacheFile)) {
            $cacheLines = Get-Content -LiteralPath $cvmCacheFile
            $cacheHome = $null
            $cacheChanceCode = $null

            foreach ($line in $cacheLines) {
                if ($line -match '^CMAKE_HOME_DIRECTORY:[^=]*=(.*)$') {
                    $cacheHome = $Matches[1]
                }
                elseif ($line -match '^CHANCECODE_ROOT:[^=]*=(.*)$') {
                    $cacheChanceCode = $Matches[1]
                }
            }

            if ($cacheHome -and $cacheChanceCode) {
                $cacheHomeNorm = $cacheHome.Replace('\', '/').ToLowerInvariant()
                $cvmDirNorm = $cvmDir.Replace('\', '/').ToLowerInvariant()
                $cacheChanceCodeNorm = $cacheChanceCode.Replace('\', '/').ToLowerInvariant()
                $chanceCodeNorm = $chanceCodeDir.Replace('\', '/').ToLowerInvariant()

                if ($cacheHomeNorm -eq $cvmDirNorm -and $cacheChanceCodeNorm -eq $chanceCodeNorm) {
                    $needCvmConfigure = $false
                }
            }
        }

        if ($needCvmConfigure) {
            Write-Info 'Configuring CVM'
            Invoke-External -FilePath 'cmake' -Arguments @('-S', '.', '-B', 'build', "-DCHANCECODE_ROOT=$chanceCodeDir") -WorkingDirectory $cvmDir | Out-Null
        }
        else {
            Write-Info 'Using existing CVM CMake cache (skipping configure)'
        }

        Write-Info 'Building CVM'
        Invoke-External -FilePath 'cmake' -Arguments @('--build', 'build') -WorkingDirectory $cvmDir | Out-Null
        Write-Info 'CVM build complete'
    }

    $chancecBin = Get-FirstExistingPath @(
        (Join-Path $scriptDir 'build\chancec.exe'),
        (Join-Path $scriptDir 'build\chancec'),
        (Join-Path $scriptDir 'build\mingw-release\chancec.exe'),
        (Join-Path $scriptDir 'build\mingw-release\chancec')
    )
    if (-not $chancecBin) {
        Fail "chancec not found in '$scriptDir\build' or '$scriptDir\build\mingw-release'"
    }

    $chancecodecBin = Get-FirstExistingPath @(
        (Join-Path $scriptDir 'build\chancecodec.exe'),
        (Join-Path $scriptDir 'build\chancecodec'),
        (Join-Path $scriptDir 'build\mingw-release\chancecodec.exe'),
        (Join-Path $scriptDir 'build\mingw-release\chancecodec'),
        (Join-Path $chanceCodeDir 'build\chancecodec.exe'),
        (Join-Path $chanceCodeDir 'build\chancecodec')
    )
    if ($chancecodecBin) {
        Write-Info "Using chancecodec for library rebuild: $chancecodecBin"
    }

    $chsBin = Get-FirstExistingPath @(
        (Join-Path $scriptDir 'build\chs.exe'),
        (Join-Path $scriptDir 'build\chs'),
        (Join-Path $scriptDir 'build\mingw-release\chs.exe'),
        (Join-Path $scriptDir 'build\mingw-release\chs'),
        (Join-Path $chsDir 'build\chs.exe'),
        (Join-Path $chsDir 'build\chs'),
        (Join-Path $chsDir 'build\mingw-release\chs.exe'),
        (Join-Path $chsDir 'build\mingw-release\chs')
    )
    if ($chsBin) {
        Write-Info "Using CHS for assembly: $chsBin"
    }

    Write-Section 'Libraries'
    Write-Info 'Rebuilding runtime and stdlib with built compiler'

    $runtimeProjDir = Join-Path $scriptDir 'runtime'
    $stdlibProjDir = Join-Path $scriptDir 'src\stdlib'

    if ($chancecodecBin) {
        if ($chsBin) {
            Invoke-External -FilePath $chancecBin -Arguments @('--chancecodec', $chancecodecBin, '--chs', $chsBin, 'runtime.ceproj') -WorkingDirectory $runtimeProjDir | Out-Null
            Invoke-External -FilePath $chancecBin -Arguments @('--chancecodec', $chancecodecBin, '--chs', $chsBin, 'stdlib.ceproj') -WorkingDirectory $stdlibProjDir | Out-Null
        }
        else {
            Invoke-External -FilePath $chancecBin -Arguments @('--chancecodec', $chancecodecBin, 'runtime.ceproj') -WorkingDirectory $runtimeProjDir | Out-Null
            Invoke-External -FilePath $chancecBin -Arguments @('--chancecodec', $chancecodecBin, 'stdlib.ceproj') -WorkingDirectory $stdlibProjDir | Out-Null
        }
    }
    else {
        if ($chsBin) {
            Invoke-External -FilePath $chancecBin -Arguments @('--chs', $chsBin, 'runtime.ceproj') -WorkingDirectory $runtimeProjDir | Out-Null
            Invoke-External -FilePath $chancecBin -Arguments @('--chs', $chsBin, 'stdlib.ceproj') -WorkingDirectory $stdlibProjDir | Out-Null
        }
        else {
            Invoke-External -FilePath $chancecBin -Arguments @('runtime.ceproj') -WorkingDirectory $runtimeProjDir | Out-Null
            Invoke-External -FilePath $chancecBin -Arguments @('stdlib.ceproj') -WorkingDirectory $stdlibProjDir | Out-Null
        }
    }

    Write-Info 'Library rebuild complete'

    Write-Section 'Install'
    Write-Info "Prefix:  $prefix"
    Write-Info "Bin:     $binDir"
    Write-Info "Stdlib:  $stdlibDir"
    Write-Info "Runtime: $runtimeDir"
    Write-Info 'Installing artifacts'

    $created = New-DirectoriesIfMissing @($binDir, $stdlibDir, $runtimeDir)
    if (-not $created) {
        if ($prefixIsDefault) {
            Write-Warn "cannot create default install directories under '$prefix'; falling back to user-local install root"
            $prefix = Join-Path $env:LOCALAPPDATA 'chance'
            $binDir = Join-Path $prefix 'bin'
            $shareDir = Join-Path $prefix 'share\chance'
            $stdlibDir = Join-Path $shareDir 'stdlib'
            $runtimeDir = Join-Path $shareDir 'runtime'

            if (-not (New-DirectoriesIfMissing @($binDir, $stdlibDir, $runtimeDir))) {
                Fail 'failed to create fallback install directories'
            }

            Write-Info "Prefix:  $prefix"
            Write-Info "Bin:     $binDir"
            Write-Info "Stdlib:  $stdlibDir"
            Write-Info "Runtime: $runtimeDir"
        }
        else {
            Fail 'failed to create install directories'
        }
    }

    Copy-RequiredFile -Source $chancecBin -Destination (Join-Path $binDir 'chancec.exe')
    [void](Copy-IfExists -Source (Join-Path $scriptDir 'src\stdlib\stdlib.cclib') -Destination (Join-Path $stdlibDir 'stdlib.cclib'))
    [void](Copy-IfExists -Source (Join-Path $scriptDir 'runtime\runtime.cclib') -Destination (Join-Path $runtimeDir 'runtime.cclib'))

    if ($chancecodecBin) {
        Copy-RequiredFile -Source $chancecodecBin -Destination (Join-Path $binDir 'chancecodec.exe')
    }

    Write-Info 'Installing CLD'
    $cldBin = Get-FirstExistingPath @(
        (Join-Path $cldDir 'build\cld.exe'),
        (Join-Path $cldDir 'build\mingw-release\cld.exe'),
        (Join-Path $cldDir 'build\cld'),
        (Join-Path $cldDir 'build\mingw-release\cld')
    )
    if ($cldBin) {
        Copy-RequiredFile -Source $cldBin -Destination (Join-Path $binDir 'cld.exe')
    }
    else {
        Fail "CLD binary not found under '$cldDir\build'"
    }

    Write-Info 'Installing CHS'
    $chsInstallBin = Get-FirstExistingPath @(
        (Join-Path $chsDir 'build\chs.exe'),
        (Join-Path $chsDir 'build\mingw-release\chs.exe'),
        (Join-Path $chsDir 'build\chs'),
        (Join-Path $chsDir 'build\mingw-release\chs')
    )
    if ($chsInstallBin) {
        Copy-RequiredFile -Source $chsInstallBin -Destination (Join-Path $binDir 'chs.exe')
    }
    else {
        Fail "CHS binary not found under '$chsDir\build'"
    }

    $cvmExe = Get-FirstExistingPath @(
        (Join-Path $cvmDir 'build\cvm.exe'),
        (Join-Path $cvmDir 'build\cvm'),
        (Join-Path $cvmDir 'build\mingw-release\cvm.exe'),
        (Join-Path $cvmDir 'build\mingw-release\cvm'),
        (Join-Path $cvmDir 'build\Release\cvm.exe'),
        (Join-Path $cvmDir 'build\Debug\cvm.exe'),
        (Join-Path $cvmDir 'build\RelWithDebInfo\cvm.exe'),
        (Join-Path $cvmDir 'build\MinSizeRel\cvm.exe')
    )

    if ($cvmExe) {
        Write-Info 'Installing CVM'
        Copy-RequiredFile -Source $cvmExe -Destination (Join-Path $binDir 'cvm.exe')

        $cvmExeDir = Split-Path -Parent $cvmExe
        [void](Copy-IfExists -Source (Join-Path $cvmExeDir 'libffi-8.dll') -Destination (Join-Path $binDir 'libffi-8.dll'))
        [void](Copy-IfExists -Source (Join-Path $cvmExeDir 'libffi.dll') -Destination (Join-Path $binDir 'libffi.dll'))
    }

    if (Test-Path -LiteralPath (Join-Path $binDir 'cvm.exe')) {
        if (-not (Copy-IfExists -Source (Join-Path $scriptDir 'src\stdlib\stdlib.cclib') -Destination (Join-Path $binDir 'stdlib.cclib'))) {
            Write-Warn "stdlib.cclib not found at '$scriptDir\src\stdlib\stdlib.cclib'"
        }

        if (-not (Copy-IfExists -Source (Join-Path $scriptDir 'runtime\runtime.cclib') -Destination (Join-Path $binDir 'runtime.cclib'))) {
            Write-Warn "runtime.cclib not found at '$scriptDir\runtime\runtime.cclib'"
        }

        if (-not (Copy-IfExists -Source (Join-Path $scriptDir 'src\stdlib\stdlib.ccb') -Destination (Join-Path $binDir 'stdlib.ccb'))) {
            Write-Warn "stdlib.ccb not found at '$scriptDir\src\stdlib\stdlib.ccb'"
        }

        if (-not (Copy-IfExists -Source (Join-Path $scriptDir 'runtime\runtime.ccb') -Destination (Join-Path $binDir 'runtime.ccb'))) {
            Write-Warn "runtime.ccb not found at '$scriptDir\runtime\runtime.ccb'"
        }
    }

    Write-Section 'Install Location'
    Write-Info "Installed prefix: $prefix"
    Write-Info "Installed bin: $binDir"
    Write-Info "Add this bin directory to PATH manually if needed: $binDir"

    Write-Host 'Done'
    exit 0
}
catch {
    Write-Host "error: $($_.Exception.Message)"
    Write-Host 'Install failed.'
    exit 1
}
