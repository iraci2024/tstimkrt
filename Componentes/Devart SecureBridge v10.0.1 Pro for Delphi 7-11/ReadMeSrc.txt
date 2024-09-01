SecureBridge Components Source Code
Copyright 2007-2021, Devart. All Rights Reserved

There are two ways to compile and install SecureBridge for Windows manually.

I. Using IDE

Delphi and C++Builder for Win32
--------------------------------

Run your IDE and walk through the following steps:
  1) Compile SecureBridge run-time package (sbridgeXX.dpk)
  2) Compile and install SecureBridge design-time package (dclsbridgeXX.dpk)

You can find these packages in 
  Source\Delphi6\*.dpk - for Delphi 6 
  Source\Delphi7\*.dpk - for Delphi 7 
  Source\Delphi10\*.dpk - for BDS 2006
  Source\Delphi11\*.dpk - for RAD Studio 2007
  Source\Delphi12\*.dpk - for RAD Studio 2009
  Source\Delphi14\*.dpk - for RAD Studio 2010
  Source\Delphi15\*.dpk - for RAD Studio XE
  Source\Delphi16\*.dpk - for RAD Studio XE2
  Source\Delphi17\*.dpk - for RAD Studio XE3
  Source\Delphi18\*.dpk - for RAD Studio XE4
  Source\Delphi19\*.dpk - for RAD Studio XE5
  Source\Delphi20\*.dpk - for RAD Studio XE6
  Source\Delphi21\*.dpk - for RAD Studio XE7
  Source\Delphi22\*.dpk - for RAD Studio XE8
  Source\Delphi23\*.dpk - for RAD Studio 10 Seattle
  Source\Delphi24\*.dpk - for RAD Studio 10.1 Berlin
  Source\Delphi25\*.dpk - for RAD Studio 10.2 Tokyo
  Source\Delphi26\*.dpk - for RAD Studio 10.3 Rio
  Source\Delphi27\*.dpk - for RAD Studio 10.4 Sydney
  Source\CBuilder6\*.bpk - for C++Builder 6

To compile SecureBridge-based applications, add SecureBridge Source directory path 
to the "Library Path".

SecureBridge Library uses freeware units developed by Hagen Reddmann, 
in which hash algorithms and symmetric algorithms for data encryption are implemented. 
We express gratitude for that to him.

II. Using make-files

Delphi and C++Builder for Win32
--------------------------------

  1) Go to one of the following folders (let's denote this folder %MakePath%):
     Source\Delphi6 - for Delphi 6
     Source\Delphi7 - for Delphi 7
     Source\Delphi10 - for BDS 2006
     Source\Delphi11 - for RAD Studio 2007
     Source\Delphi12 - for RAD Studio 2009
     Source\Delphi14 - for RAD Studio 2010
     Source\Delphi15 - for RAD Studio XE
     Source\Delphi16 - for RAD Studio XE2
     Source\Delphi17 - for RAD Studio XE3
     Source\Delphi18 - for RAD Studio XE4
     Source\Delphi19 - for RAD Studio XE5
     Source\Delphi20 - for RAD Studio XE6
     Source\Delphi21 - for RAD Studio XE7
     Source\Delphi22 - for RAD Studio XE8
     Source\Delphi23 - for RAD Studio 10 Seattle
     Source\Delphi24 - for RAD Studio 10.1 Berlin
     Source\Delphi25 - for RAD Studio 10.2 Tokyo
     Source\Delphi26 - for RAD Studio 10.3 Rio
     Source\Delphi27 - for RAD Studio 10.4 Sydney
     Source\CBuilder6 - for C++Builder 6

  2) Find in the 'Make.bat' line containing 

     set IdeDir="%PROGRAMFILES%\Borland\Delphi7

     and make sure that the path to the IDE is set correctly (it must include 
     the opening quotation mark and omit the closing quotation mark).

  3) Run 'Make.bat'. Binaries will be copied to %MakePath%\SecureBridge sub folder
  4) Copy %MakePath%\SecureBridge\*.bpl files to a folder that is included in the
     PATH environment variable
  5) Run IDE and add dclsbridgeXX.bpl via Component->Install Packages... menu 
  6) To compile SecureBridge based application add the SecureBridge Source directory 
     path to the "Library Path" list
