@echo off
rem **********************************************************************
rem *
rem * SecureBridge
rem *
rem * Tasks:
rem *   Compile SecureBridge packages;
rem *
rem * Command line:
rem *   call ..\Make.bat IDEName IDEVer CLR
rem *   
rem * Parameters:
rem *   IDEName = (Delphi, CBuilder)
rem *   IDEVer = (6, 7, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
rem *   Platform = (CLR, WIN32, WIN64, OSX32, iOSSimulator, iOSDevice, iOSDevice32, iOSDevice64, Android32, Android64, Linux64) WIN32 - default
rem **********************************************************************

rem Prepare ==============================================================
rem ======================================================================
set IDEName=%1
set IDEVer=%2
set Platform=%3
set NeedPause=%4
set PrjNameL=sbridge

pushd

rem Test IDEName
if %IDEName%A==DelphiA goto IDENameOK
if %IDEName%A==CBuilderA goto IDENameOK
echo Command line must be:
echo    call ..\Make.bat IDEName IDEVer
echo    IDEName = (Delphi, CBuilder)
goto Err
:IDENameOK

rem Test IDEVer
if %IDEVer%A==6A goto IDEVerOK
if %IDEVer%A==7A goto IDEVerOK
if %IDEVer%A==9A goto IDEVerOK
if %IDEVer%A==10A goto IDEVerOK
if %IDEVer%A==11A goto IDEVer11
if %IDEVer%A==12A goto IDEVerOK
if %IDEVer%A==14A goto IDEVerOK
if %IDEVer%A==15A goto IDEVerOK
if %IDEVer%A==16A goto IDEVerOK
if %IDEVer%A==17A goto IDEVerOK
if %IDEVer%A==18A goto IDEVerOK
if %IDEVer%A==19A goto IDEVerOK
if %IDEVer%A==20A goto IDEVerOK
if %IDEVer%A==21A goto IDEVerOK
if %IDEVer%A==22A goto IDEVerOK
if %IDEVer%A==23A goto IDEVerOK
if %IDEVer%A==24A goto IDEVerOK
if %IDEVer%A==25A goto IDEVerOK
if %IDEVer%A==26A goto IDEVerOK
if %IDEVer%A==27A goto IDEVerOK
if %IDEVer%A==28A goto IDEVerOK
echo Command line must be:
echo    call ..\Make.bat IDEName IDEVer Platform
echo    IDEVer = (6, 7, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)
goto Err

:IDEVer11:
set PkgVer=105
goto PkgVerOK

:IDEVerOK
set PkgVer=%IDEVer%0

:PkgVerOK

if not %Platform%A==CLRA goto PlatformWin64
goto end
rem set PlatformDir=CLR
rem goto PlatformOK
:PlatformWin64
if not %Platform%A==WIN64A goto PlatformOSX32
set PlatformDir=Win64
goto PlatformOK
:PlatformOSX32
if not %Platform%A==OSX32A goto PlatformOSX64
set PlatformDir=OSX32
set CompilerOptions=-LE. -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Fmx
goto PlatformOK
:PlatformOSX64
if not %Platform%A==OSX64A goto iOSSimulator
set PlatformDir=OSX64
set CompilerOptions=-LE. -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Fmx
goto PlatformOK
:iOSSimulator
if not %Platform%A==iOSSimulatorA goto iOSDevice
set PlatformDir=iOSSimulator
goto PlatformOK
:iOSDevice
if not %Platform%A==iOSDeviceA goto iOSDevice32
set PlatformDir=iOSDevice
goto PlatformOK
:iOSDevice32
if not %Platform%A==iOSDevice32A goto iOSDevice64
set PlatformDir=iOSDevice32
goto PlatformOK
:iOSDevice64
if not %Platform%A==iOSDevice64A goto Android
set PlatformDir=iOSDevice64
goto PlatformOK
:Android
if not %Platform%A==AndroidA goto Android32
set PlatformDir=Android
goto PlatformOK
:Android32
if not %Platform%A==Android32A goto Android64
set PlatformDir=Android32
goto PlatformOK
:Android64
if not %Platform%A==Android64A goto Linux64
set PlatformDir=Android64
goto PlatformOK
:Linux64
if not %Platform%A==Linux64A goto PlatformWin32
set PlatformDir=Linux64
goto PlatformOK
:PlatformWin32
set Platform=WIN32
set PlatformDir=Win32
:PlatformOK

set CompilerOptions=-B -LE. -U..\..\Lib\Delphi%IDEVer%\%PlatformDir% -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl
set CompilerOptionsVCL=-B -LE. -U..\..\Lib\Delphi%IDEVer%\%PlatformDir% -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell

if %IDEVer%A==16A goto NotResetCompileOptions
if %IDEVer%A==17A goto NotResetCompileOptions
if %IDEVer%A==18A goto NotResetCompileOptions
if %IDEVer%A==19A goto NotResetCompileOptions
if %IDEVer%A==20A goto NotResetCompileOptions
if %IDEVer%A==21A goto NotResetCompileOptions
if %IDEVer%A==22A goto NotResetCompileOptions
if %IDEVer%A==23A goto NotResetCompileOptions
if %IDEVer%A==24A goto NotResetCompileOptions
if %IDEVer%A==25A goto NotResetCompileOptions
if %IDEVer%A==26A goto NotResetCompileOptions
if %IDEVer%A==27A goto NotResetCompileOptions
if %IDEVer%A==28A goto NotResetCompileOptions
set PlatformDir=.
set CompilerOptions=-B -LE. -U..
set CompilerOptionsVCL=-B -LE. -U..
:NotResetCompileOptions

if %IDEName%A==CBuilderA goto CBuilder
if %Platform%A==CLRA goto Delphi8

rem Compile ==============================================================
if not %Platform%A==WIN32A goto Win64Compiler
set Compiler=%IdeDir%\Bin\dcc32.exe"
goto CompilerOK
:Win64Compiler
if not %Platform%A==WIN64A goto OSX32Compiler
set Compiler=%IdeDir%\Bin\dcc64.exe"
goto CompilerOK
:OSX32Compiler
if not %Platform%A==OSX32A goto OSX64Compiler
set Compiler=%IdeDir%\Bin\dccosx.exe"
goto CompilerOK
:OSX64Compiler
if not %Platform%A==OSX64A goto iOSSimulator
set Compiler=%IdeDir%\Bin\dccosx64.exe"
goto CompilerOK
:iOSSimulator
if not %Platform%A==iOSSimulatorA goto iOSDevice
set Compiler=%IdeDir%\Bin\dccios32.exe"
goto CompilerOK
:iOSDevice
if not %Platform%A==iOSDeviceA goto iOSDevice32
set Compiler=%IdeDir%\Bin\dcciosarm.exe"
goto CompilerOK
:iOSDevice32
if not %Platform%A==iOSDevice32A goto iOSDevice64
set Compiler=%IdeDir%\Bin\dcciosarm.exe"
goto CompilerOK
:iOSDevice64
if not %Platform%A==iOSDevice64A goto Android
set Compiler=%IdeDir%\Bin\dcciosarm64.exe"
goto CompilerOK
:Android
if not %Platform%A==AndroidA goto Android32
set Compiler=%IdeDir%\Bin\dccaarm.exe"
goto CompilerOK
:Android32
if not %Platform%A==Android32A goto Android64
set Compiler=%IdeDir%\Bin\dccaarm.exe"
goto CompilerOK
:Android64
if not %Platform%A==Android64A goto Linux64
set Compiler=%IdeDir%\Bin\dccaarm64.exe"
goto CompilerOK
:Linux64
if not %Platform%A==Linux64A goto InvalidPlatform
set Compiler=%IdeDir%\Bin\dcclinux64.exe"
set CompilerOptions=%CompilerOptionsLinux64%
:CompilerOK

rem Compile SecureBridge packages ===========================================
%Compiler% %CompilerOptions% %PrjNameL%%PkgVer%.dpk
@if errorlevel 1 goto Err

if %Platform%A==WIN64A goto SkipDcl
if %Platform%A==OSX32A goto SkipDcl
if %Platform%A==iOSSimulatorA goto SkipDcl
if %Platform%A==iOSDeviceA goto SkipDcl
if %Platform%A==iOSDevice32A goto SkipDcl
if %Platform%A==iOSDevice64A goto SkipDcl
if %Platform%A==AndroidA goto SkipDcl
if %Platform%A==Android32A goto SkipDcl
if %Platform%A==Android64A goto SkipDcl
if %Platform%A==Linux64A goto SkipDcl
%Compiler% %CompilerOptionsVCL% dcl%PrjNameL%%PkgVer%.dpk
@if errorlevel 1 goto Err
:SkipDcl

rem Copy files ===========================================================
rem ======================================================================

if exist *.bpl        move *.bpl               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.dcu        move *.dcu               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.dcu     move ..\*.dcu            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.dcp        move *.dcp               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

copy ..\*.res            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

rem MAC OS X files ===
if not %Platform%A==OSX32A goto SkipOSX32Lib
if exist *.dylib      move *.dylib             ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.bpi        move *.bpi               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipOSX32Lib

rem iOS Simulator files ===
if not %Platform%A==iOSSimulatorA goto SkipiOSSimulatorLib
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipiOSSimulatorLib

rem iOS Device files ===
if not %Platform%A==iOSDeviceA goto SkipiOSDeviceLib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipiOSDeviceLib 

rem iOS Device32 files ===
if not %Platform%A==iOSDevice32A goto SkipiOSDevice32Lib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipiOSDevice32Lib 

rem iOS Device64 files ===
if not %Platform%A==iOSDevice64A goto SkipiOSDevice64Lib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipiOSDevice64Lib 

rem Android files ===
if not %Platform%A==AndroidA goto SkipAndroidLib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipAndroidLib

rem Android32 files ===
if not %Platform%A==Android32A goto SkipAndroid32Lib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipAndroid32Lib

rem Android64 files ===
if not %Platform%A==Android64A goto SkipAndroid64Lib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipAndroid64Lib

rem Linux64 files ===
if not %Platform%A==Linux64A goto SkipLinux64Lib
if exist *.o          move *.o                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.a          move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
:SkipLinux64Lib

rem CBuilder files ===
if %Platform%A==CLRA goto SkipD10BCCLib
if exist  *.bpi       move *.bpi               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist  *.lib       move *.lib               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist  *.a         move *.a                 ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist  *.hpp       move *.hpp               ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%
if exist  ..\*.hpp    move ..\*.hpp            ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%
:SkipD10BCCLib

goto end

:Delphi8
rem Compile Delphi8 ======================================================
rem Compile SecureBridge packages ===========================================
%IdeDir%\Bin\dccil.exe" -LE. Devart.SecureBridge.dpk
@if errorlevel 1 goto Err

%IdeDir%\Bin\dccil.exe" -LE. Devart.SecureBridge.Design.dpk
@if errorlevel 1 goto Err

rem Copy files ===========================================================
rem ======================================================================

if exist *.dll      move *.dll               ..\..\Bin\%IDEName%%IDEVer%
if exist *.pdb      move *.pdb               ..\..\Bin\%IDEName%%IDEVer%

if exist *.dcpil    move *.dcpil             ..\..\Lib\%IDEName%%IDEVer%
if exist *.dcuil    move *.dcuil             ..\..\Lib\%IDEName%%IDEVer%

copy ..\*.res            ..\..\Lib\%IDEName%%IDEVer%

goto end

:CBuilder
rem Compile ==============================================================
rem Compile SecureBridge packages ===========================================
%IdeDir%\Bin\bpr2mak.exe" -t..\sbridge.bmk %PrjNameL%%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f %PrjNameL%%PkgVer%.mak
@if errorlevel 1 goto Err

%IdeDir%\Bin\bpr2mak.exe" -t..\sbridge.bmk dcl%PrjNameL%%PkgVer%.bpk
@if errorlevel 1 goto Err
%IdeDir%\Bin\make.exe" -f dcl%PrjNameL%%PkgVer%.mak
@if errorlevel 1 goto Err

rem Copy files ===========================================================
rem ======================================================================

if exist *.bpl        move *.bpl               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.tds        move *.tds               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%
if exist *.mak        move *.mak               ..\..\Bin\%IDEName%%IDEVer%\%PlatformDir%

if exist *.dcu        move *.dcu               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.dcu     move ..\*.dcu            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.bpi        move *.bpi               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.lib        move *.lib               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.obj        move *.obj               ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%
if exist *.hpp        move *.hpp               ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%
if exist ..\*.hpp     move ..\*.hpp            ..\..\Include\%IDEName%%IDEVer%\%PlatformDir%

copy ..\*.res            ..\..\Lib\%IDEName%%IDEVer%\%PlatformDir%

goto end

:InvalidPlatform
echo Invalid Platform

:Err
if %NeedPause%A == NA goto end
if %NeedPause%A == NOA goto end
pause

:end
popd