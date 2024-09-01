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

unit dxSkinOffice2019White;

{$I cxVer.inc}

interface

uses
  Classes, dxCore, dxCoreGraphics, dxGDIPlusApi, cxLookAndFeelPainters,
  dxSkinsCore, dxSkinsLookAndFeelPainter;

{$HPPEMIT '#ifdef USEPACKAGES'}
{$IF DEFINED(VER210)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS14.bpi"'}
{$ELSEIF DEFINED(VER220)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS15.bpi"'}
{$ELSEIF DEFINED(VER230)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS16.bpi"'}
{$ELSEIF DEFINED(VER240)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS17.bpi"'}
{$ELSEIF DEFINED(VER250)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS18.bpi"'}
{$ELSEIF DEFINED(VER260)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS19.bpi"'}
{$ELSEIF DEFINED(VER270)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS20.bpi"'}
{$ELSEIF DEFINED(VER280)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS21.bpi"'}
{$ELSEIF DEFINED(VER290)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS22.bpi"'}
{$ELSEIF DEFINED(VER300)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS23.bpi"'}
{$ELSEIF DEFINED(VER310)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS24.bpi"'}
{$ELSEIF DEFINED(VER320)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS25.bpi"'}
{$ELSEIF DEFINED(VER330)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS26.bpi"'}
{$ELSEIF DEFINED(VER340)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS27.bpi"'}
{$ELSEIF DEFINED(VER350)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS28.bpi"'}
{$ELSE}
Unsupported
{$IFEND}
{$HPPEMIT '#else'}
{$IFDEF WIN64}
{$IF DEFINED(VER210)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS14.a"'}
{$ELSEIF DEFINED(VER220)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS15.a"'}
{$ELSEIF DEFINED(VER230)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS16.a"'}
{$ELSEIF DEFINED(VER240)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS17.a"'}
{$ELSEIF DEFINED(VER250)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS18.a"'}
{$ELSEIF DEFINED(VER260)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS19.a"'}
{$ELSEIF DEFINED(VER270)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS20.a"'}
{$ELSEIF DEFINED(VER280)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS21.a"'}
{$ELSEIF DEFINED(VER290)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS22.a"'}
{$ELSEIF DEFINED(VER300)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS23.a"'}
{$ELSEIF DEFINED(VER310)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS24.a"'}
{$ELSEIF DEFINED(VER320)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS25.a"'}
{$ELSEIF DEFINED(VER330)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS26.a"'}
{$ELSEIF DEFINED(VER340)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS27.a"'}
{$ELSEIF DEFINED(VER350)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS28.a"'}
{$ELSE}
  Unsupported
{$IFEND}
{$ELSE}
{$IF DEFINED(VER210)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS14.lib"'}
{$ELSEIF DEFINED(VER220)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS15.lib"'}
{$ELSEIF DEFINED(VER230)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS16.lib"'}
{$ELSEIF DEFINED(VER240)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS17.lib"'}
{$ELSEIF DEFINED(VER250)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS18.lib"'}
{$ELSEIF DEFINED(VER260)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS19.lib"'}
{$ELSEIF DEFINED(VER270)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS20.lib"'}
{$ELSEIF DEFINED(VER280)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS21.lib"'}
{$ELSEIF DEFINED(VER290)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS22.lib"'}
{$ELSEIF DEFINED(VER300)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS23.lib"'}
{$ELSEIF DEFINED(VER310)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS24.lib"'}
{$ELSEIF DEFINED(VER320)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS25.lib"'}
{$ELSEIF DEFINED(VER330)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS26.lib"'}
{$ELSEIF DEFINED(VER340)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS27.lib"'}
{$ELSEIF DEFINED(VER350)}
{$HPPEMIT '#pragma link "dxSkinOffice2019WhiteRS28.lib"'}
{$ELSE}
  Unsupported
{$IFEND}
{$ENDIF}
{$HPPEMIT '#endif}
  type

{ TdxSkinOffice2019WhitePainter }

  TdxSkinOffice2019WhitePainter = class(TdxSkinLookAndFeelPainter)public
function LookAndFeelName: string;
override;
end;

implementation

{$R dxSkinOffice2019White.res}

const
  SkinsCount = 1;
  SkinNames: array [0 .. SkinsCount - 1] of string = ('Office2019White');
  SkinPainters: array [0 .. SkinsCount - 1] of TdxSkinLookAndFeelPainterClass =
    (TdxSkinOffice2019WhitePainter);

  { TdxSkinOffice2019WhitePainter }

function TdxSkinOffice2019WhitePainter.LookAndFeelName: string;
begin
  Result := 'Office2019White';
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
