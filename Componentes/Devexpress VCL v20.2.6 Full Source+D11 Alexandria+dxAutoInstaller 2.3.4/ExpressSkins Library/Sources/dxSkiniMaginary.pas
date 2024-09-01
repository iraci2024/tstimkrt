{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressSkins Library }
{ }
{ Copyright (c) 2006-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSSKINS AND ALL ACCOMPANYING }
{ VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxSkiniMaginary;

{$I cxVer.inc}

interface

uses
  Classes, dxCore, dxCoreGraphics, dxGDIPlusApi, cxLookAndFeelPainters,
  dxSkinsCore, dxSkinsLookAndFeelPainter;

{$HPPEMIT '#ifdef USEPACKAGES'}
{$IF DEFINED(VER210)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS14.bpi"'}
{$ELSEIF DEFINED(VER220)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS15.bpi"'}
{$ELSEIF DEFINED(VER230)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS16.bpi"'}
{$ELSEIF DEFINED(VER240)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS17.bpi"'}
{$ELSEIF DEFINED(VER250)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS18.bpi"'}
{$ELSEIF DEFINED(VER260)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS19.bpi"'}
{$ELSEIF DEFINED(VER270)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS20.bpi"'}
{$ELSEIF DEFINED(VER280)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS21.bpi"'}
{$ELSEIF DEFINED(VER290)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS22.bpi"'}
{$ELSEIF DEFINED(VER300)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS23.bpi"'}
{$ELSEIF DEFINED(VER310)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS24.bpi"'}
{$ELSEIF DEFINED(VER320)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS25.bpi"'}
{$ELSEIF DEFINED(VER330)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS26.bpi"'}
{$ELSEIF DEFINED(VER340)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS27.bpi"'}
{$ELSEIF DEFINED(VER350)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS28.bpi"'}
{$ELSE}
Unsupported
{$IFEND}
{$HPPEMIT '#else'}
{$IFDEF WIN64}
{$IF DEFINED(VER210)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS14.a"'}
{$ELSEIF DEFINED(VER220)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS15.a"'}
{$ELSEIF DEFINED(VER230)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS16.a"'}
{$ELSEIF DEFINED(VER240)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS17.a"'}
{$ELSEIF DEFINED(VER250)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS18.a"'}
{$ELSEIF DEFINED(VER260)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS19.a"'}
{$ELSEIF DEFINED(VER270)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS20.a"'}
{$ELSEIF DEFINED(VER280)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS21.a"'}
{$ELSEIF DEFINED(VER290)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS22.a"'}
{$ELSEIF DEFINED(VER300)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS23.a"'}
{$ELSEIF DEFINED(VER310)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS24.a"'}
{$ELSEIF DEFINED(VER320)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS25.a"'}
{$ELSEIF DEFINED(VER330)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS26.a"'}
{$ELSEIF DEFINED(VER340)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS27.a"'}
{$ELSEIF DEFINED(VER350)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS28.a"'}
{$ELSE}
  Unsupported
{$IFEND}
{$ELSE}
{$IF DEFINED(VER210)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS14.lib"'}
{$ELSEIF DEFINED(VER220)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS15.lib"'}
{$ELSEIF DEFINED(VER230)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS16.lib"'}
{$ELSEIF DEFINED(VER240)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS17.lib"'}
{$ELSEIF DEFINED(VER250)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS18.lib"'}
{$ELSEIF DEFINED(VER260)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS19.lib"'}
{$ELSEIF DEFINED(VER270)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS20.lib"'}
{$ELSEIF DEFINED(VER280)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS21.lib"'}
{$ELSEIF DEFINED(VER290)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS22.lib"'}
{$ELSEIF DEFINED(VER300)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS23.lib"'}
{$ELSEIF DEFINED(VER310)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS24.lib"'}
{$ELSEIF DEFINED(VER320)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS25.lib"'}
{$ELSEIF DEFINED(VER330)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS26.lib"'}
{$ELSEIF DEFINED(VER340)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS27.lib"'}
{$ELSEIF DEFINED(VER350)}
{$HPPEMIT '#pragma link "dxSkiniMaginaryRS28.lib"'}
{$ELSE}
  Unsupported
{$IFEND}
{$ENDIF}
{$HPPEMIT '#endif}
  type

{ TdxSkiniMaginaryPainter }

  TdxSkiniMaginaryPainter = class(TdxSkinLookAndFeelPainter)public
function LookAndFeelName: string;
override;
end;

implementation

{$R dxSkiniMaginary.res}

const
  SkinsCount = 1;
  SkinNames: array [0 .. SkinsCount - 1] of string = ('iMaginary');
  SkinPainters: array [0 .. SkinsCount - 1] of TdxSkinLookAndFeelPainterClass =
    (TdxSkiniMaginaryPainter);

  { TdxSkiniMaginaryPainter }

function TdxSkiniMaginaryPainter.LookAndFeelName: string;
begin
  Result := 'iMaginary';
end;

//

procedure RegisterPainters;
var
  I: Integer;
begin
  if CheckGdiPlus then
  begin
    for I := 0 to SkinsCount - 1 do
      cxLookAndFeelPaintersManager.Register(SkinPainters[I].Create(SkinNames[I],
        HInstance));
  end;
end;

procedure UnregisterPainters;
var
  I: Integer;
begin
  if cxLookAndFeelPaintersManager <> nil then
  begin
    for I := 0 to SkinsCount - 1 do
      cxLookAndFeelPaintersManager.Unregister(SkinNames[I]);
  end;
end;

{$IFNDEF DXSKINDYNAMICLOADING}

initialization

dxUnitsLoader.AddUnit(@RegisterPainters, @UnregisterPainters);

finalization

dxUnitsLoader.RemoveUnit(@UnregisterPainters);
{$ENDIF}

end.
