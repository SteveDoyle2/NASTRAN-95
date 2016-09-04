@ECHO OFF
SETLOCAL

IF [%NASTRANHOME%] == [] (GOTO HOMERR) ELSE (GOTO NASTRANCMD)

:: Binary and data locations
:: FIXME: Should install the executable in the bin directory
:: FIXME: Should set these values in some external file
:SETPATHS
SET NASTRANEXE=%NASTRANHOME%\source\mds\nastran.exe
SET NASTRANCFG=%NASTRANHOME%\bin\nastran.nml
IF NOT EXIST %NASTRANEXE% GOTO EXERR
IF NOT EXIST %NASTRANCFG% GOTO CFGERR
GOTO:EOF

:: Configure the environment and execute NASA/NASTRAN
:NASTRANCMD
CALL :SETPATHS
%NASTRANEXE% %*
GOTO:EOF

:: Usage and errors

:USAGE
ECHO:
ECHO USAGE: nastran [-o output file] [-l log file] <input>
ECHO:
ECHO   Mandatory argument
ECHO     input : Top level input file with executive and case control
ECHO:
ECHO   Optional arguments
ECHO     -o : Optional output file name, defaults to <input>.out
ECHO     -l : Optional log file name, default to <input>.log
GOTO:EOF

:HOMERR
ECHO:
ECHO ERROR: NASTRANHOME is not set.
ECHO   Set environmental variable NASTRANHOME to the NASTRAN root directory.
GOTO:EOF

:EXERR
ECHO:
ECHO ERROR: The NASTRAN executable does not exist.
ECHO   Verify that %NASTRANEXE% exists and the path is correct.
GOTO:EOF

:CFGERR
ECHO:
ECHO ERROR: The NASTRAN configuration namelist does not exist.
ECHO   Verify that %NASTRANCFG% exists and the path is correct.
GOTO:EOF

ENDLOCAL
