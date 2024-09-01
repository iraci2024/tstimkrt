@echo off
rem **********************************************************************
rem *
rem * SecureBridge for RAD Studio 2009
rem *
rem **********************************************************************

rem --- Win64 compatibility ---
if "%ProgramFiles(x86)%"=="" goto DoWin32
set PROGRAMFILES=%ProgramFiles(x86)%
:DoWin32

set IdeDir="%PROGRAMFILES%\CodeGear\RAD Studio\6.0
call ..\Make.bat Delphi 12 WIN32 %2
rem call ..\Make.bat Delphi 12 CLR
