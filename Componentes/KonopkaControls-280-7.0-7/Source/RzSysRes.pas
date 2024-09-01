{===============================================================================
  RzSysRes Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2021 by Embarcadero Technologies, Inc.  All Rights Reserved.


  Description
  ------------------------------------------------------------------------------
  This is an interface unit to the RSRC32.DLL which provides a thunking layer to
  the Free System Resource values under Win32, specifically Win95 and Win98.
  Note that this unit is only valid under Win32 and that the RSRC32.DLL is
  not available on WinNT systems.


  Modification History
  ------------------------------------------------------------------------------
  7.0.0  (15 March 2021)
    * Removed old conditional defines. All code designed only for RAD Studio 10.4+.
    * Added High-DPI support for all elements in all controls
    * Added improvments and optimizations in code
  ------------------------------------------------------------------------------
  6.5.0  (28 Nov 2020)
    * Removed old conditional defines. All code designed for RAD Studio 10+.
    * Updated all uses clauses to reference complete unit scope names.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * No changes.
===============================================================================}

{$I RzComps.inc}

unit RzSysRes;

interface

type
  {$Z4}
  TFreeSystemResources = ( gfsr_SystemResources, gfsr_GDIResources, gfsr_UserResources );


function GetFreeSystemResources( ResType: TFreeSystemResources ): Integer;

implementation

uses
  Winapi.Windows,
  System.SysUtils;

type
  TFcnGetFreeSysRes = function( ResType: TFreeSystemResources ): Integer; stdcall;


var
  SysResModule: THandle = 0;
  FcnGetFreeSysRes: TFcnGetFreeSysRes = nil;


function GetFreeSystemResources( ResType: TFreeSystemResources ): Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
  begin
    { Thunking down to 16-bit is only valid on Win95/Win98 }
    if SysResModule = 0 then
      SysResModule := LoadLibrary( 'RSRC32.DLL' );

    if @FcnGetFreeSysRes = nil then
      @FcnGetFreeSysRes := GetProcAddress( SysResModule,
                                           '_MyGetFreeSystemResources32@4' );

    if @FcnGetFreeSysRes <> nil then
      Result := FcnGetFreeSysRes( ResType )
    else
      Result := 0;
  end
  else
    Result := 0;
end;



initialization

finalization
  if SysResModule <> 0 then
    FreeLibrary( SysResModule );        { Be sure to release library when done }

end.
