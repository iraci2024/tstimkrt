{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressSpreadSheet }
{ }
{ Copyright (c) 2001-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetClasses;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI22}
  System.Hash,
{$ENDIF}
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  AnsiStrings,
  Windows, SysUtils, Classes, Generics.Defaults, Generics.Collections, Graphics,
  dxCore, dxCoreClasses, cxGraphics, dxGDIPlusAPI, dxGDIPlusClasses, dxHash,
  dxHashUtils,
  dxSpreadSheetTypes, cxClasses, cxFormats, dxSpreadSheetCoreStrs,
  dxSpreadSheetGraphics, cxVariants, dxSpreadSheetCoreStyles;

type
  EdxSpreadSheetError = class(EdxException);

  { Aliases }

  TdxSpreadSheetBorders = dxSpreadSheetCoreStyles.TdxSpreadSheetBorders;
  TdxSpreadSheetBordersHandle = dxSpreadSheetCoreStyles.
    TdxSpreadSheetBordersHandle;
  TdxSpreadSheetBrushHandle = dxSpreadSheetCoreStyles.TdxSpreadSheetBrushHandle;
  TdxSpreadSheetBrushes = dxSpreadSheetCoreStyles.TdxSpreadSheetBrushes;
  TdxSpreadSheetCellStyleHandle = dxSpreadSheetCoreStyles.
    TdxSpreadSheetCellStyleHandle;
  TdxSpreadSheetCellStyles = dxSpreadSheetCoreStyles.TdxSpreadSheetCellStyles;
  TdxSpreadSheetCustomBrush = dxSpreadSheetCoreStyles.TdxSpreadSheetCustomBrush;
  TdxSpreadSheetCustomDataFormat = dxSpreadSheetCoreStyles.
    TdxSpreadSheetCustomDataFormat;
  TdxSpreadSheetCustomFont = dxSpreadSheetCoreStyles.TdxSpreadSheetCustomFont;
  TdxSpreadSheetFont = dxSpreadSheetCoreStyles.TdxSpreadSheetFont;
  TdxSpreadSheetFontHandle = dxSpreadSheetCoreStyles.TdxSpreadSheetFontHandle;
  TdxSpreadSheetFonts = dxSpreadSheetCoreStyles.TdxSpreadSheetFonts;
  TdxSpreadSheetFormats = dxSpreadSheetCoreStyles.TdxSpreadSheetFormats;
  TdxSpreadSheetPredefinedFormats = dxSpreadSheetCoreStyles.
    TdxSpreadSheetPredefinedFormats;
  TdxSpreadSheetSharedString = dxSpreadSheetCoreStyles.
    TdxSpreadSheetSharedString;

  { TdxSpreadSheetCustomFormatSettings }

  TdxSpreadSheetCustomFormatSettings = class
  protected
    function GetLocaleID: Integer; virtual;
  public
    Data: TFormatSettings;
    DateTimeSystem: TdxSpreadSheetDateTimeSystem;

    constructor Create;
    procedure Assign(ASource: TdxSpreadSheetCustomFormatSettings); virtual;
    procedure UpdateSettings; virtual;
  end;

  { TdxSpreadSheetFormatSettings }

  TdxSpreadSheetFormatSettings = class(TdxSpreadSheetCustomFormatSettings)
  strict private
    function GetDecimalSeparator: Char; inline;
    function GetListSeparator: Char; inline;
    procedure SetDecimalSeparator(const AValue: Char); inline;
    procedure SetListSeparator(const AValue: Char); inline;
  protected
    procedure UpdateBreakChars;
    procedure UpdateOperations;
  public
    ArraySeparator: Char;
    BreakChars: string;
    CurrencyFormat: string;
    Operations: TdxSpreadSheetOperationStrings;
    R1C1Reference: Boolean;

    procedure Assign(ASource: TdxSpreadSheetCustomFormatSettings); override;
    function ExpandExternalLinks: Boolean; virtual;
    function GetFunctionName(const AName: Pointer): string; virtual;
    procedure UpdateSettings; override;

    property DecimalSeparator: Char read GetDecimalSeparator
      write SetDecimalSeparator;
    property ListSeparator: Char read GetListSeparator write SetListSeparator;
  end;

  { IdxSpreadSheetObjectLink }

  IdxSpreadSheetObjectLink = interface
    ['{9C19004A-C805-4E44-9991-77FE1C44C0A4}']
    function GetObject: TObject;
    procedure ReleaseObject;
  end;

  { TdxSpreadSheetObjectLink }

  TdxSpreadSheetObjectLink = class(TInterfacedObject, IdxSpreadSheetObjectLink)
  strict private
    FObject: TObject;
  public
    constructor Create(AObject: TObject);
    // IdxSpreadSheetObjectLink
    function GetObject: TObject;
    procedure ReleaseObject;
  end;

  { TdxSpreadSheetInvalidObject }

  TdxSpreadSheetInvalidObject = class(TcxIUnknownObject,
    IdxSpreadSheetObjectLink)
  strict private
    // IdxSpreadSheetObjectLink
    function GetObject: TObject;
    procedure ReleaseObject;
  public
    class procedure AssignTo(var AObject); inline;
    class function Instance: TObject;
    class function IsInvalid(const AObject: TObject): Boolean; inline;
    class function IsLive(const AObject: TObject): Boolean; inline;
  end;

function dxFloatToStr(const AValue: Double;
  const AFormatSettings: TFormatSettings): string;
function dxTryStrToFloat(const AValue: string; var AFloatValue: Double;
  const AFormatSettings: TFormatSettings): Boolean;
function dxTryStrToNumeric(const AStr: string; out ANumeric: Double): Boolean;

implementation

uses
  Math, StrUtils, dxTypeHelpers, dxSpreadSheetUtils, dxSpreadSheetNumberFormat,
  dxDPIAwareUtils, dxStringHelper;

var
  dxSpreadSheetInvalidObject: TObject;

function dxFloatToStr(const AValue: Double;
  const AFormatSettings: TFormatSettings): string;

  function IsExponentialFormatStringShouldBeUsed(const AAbsValue
    : Extended): Boolean;
  begin
    if AAbsValue < 1 then
      Result := (AAbsValue > 0) and (AAbsValue < 1E-10) or (AAbsValue < 1E-08)
        and (Length(FormatFloat('0.############', AAbsValue,
        AFormatSettings)) > 9 + 2)
    else
      Result := AAbsValue > 1E11;
  end;

  function GetFormatString(const AAbsValue: Extended): string;
  begin
    if IsExponentialFormatStringShouldBeUsed(AAbsValue) then
      Result := '0.#####E+00'
    else
      Result := '0.' + DupeString('#',
        Max(0, 10 - Length(FormatFloat('0', AAbsValue, AFormatSettings))));
  end;

begin
  Result := FormatFloat(GetFormatString(Abs(AValue)), AValue, AFormatSettings);
end;

function dxTryStrToFloat(const AValue: string; var AFloatValue: Double;
  const AFormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToFloat(AValue, AFloatValue, AFormatSettings);
end;

function dxTryStrToNumeric(const AStr: string; out ANumeric: Double): Boolean;
var
  ADate: TDateTime;
begin
  Result := TryStrToFloat(AStr, ANumeric);
  if not Result then
  begin
    Result := TryStrToDateTime(AStr, ADate);
    if Result then
      ANumeric := ADate;
  end;
end;

{ TdxSpreadSheetCustomFormatSettings }

constructor TdxSpreadSheetCustomFormatSettings.Create;
begin
  inherited Create;
  UpdateSettings;
end;

procedure TdxSpreadSheetCustomFormatSettings.Assign
  (ASource: TdxSpreadSheetCustomFormatSettings);
begin
  Data := ASource.Data;
  DateTimeSystem := ASource.DateTimeSystem;
end;

procedure TdxSpreadSheetCustomFormatSettings.UpdateSettings;
begin
  dxGetLocaleFormatSettings(GetLocaleID, Data);
  DateTimeSystem := dts1900;
end;

function TdxSpreadSheetCustomFormatSettings.GetLocaleID: Integer;
begin
  Result := dxGetInvariantLocaleID;
end;

{ TdxSpreadSheetFormatSettings }

procedure TdxSpreadSheetFormatSettings.Assign
  (ASource: TdxSpreadSheetCustomFormatSettings);
begin
  inherited Assign(ASource);
  if ASource is TdxSpreadSheetFormatSettings then
  begin
    ArraySeparator := TdxSpreadSheetFormatSettings(ASource).ArraySeparator;
    BreakChars := TdxSpreadSheetFormatSettings(ASource).BreakChars;
    CurrencyFormat := TdxSpreadSheetFormatSettings(ASource).CurrencyFormat;
    Operations := TdxSpreadSheetFormatSettings(ASource).Operations;
    R1C1Reference := TdxSpreadSheetFormatSettings(ASource).R1C1Reference;
  end;
end;

function TdxSpreadSheetFormatSettings.ExpandExternalLinks: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetFormatSettings.GetFunctionName
  (const AName: Pointer): string;
begin
  Result := dxSpreadSheetUpperCase(LoadResString(AName));
end;

procedure TdxSpreadSheetFormatSettings.UpdateSettings;
begin
  inherited UpdateSettings;
  CurrencyFormat := Data.CurrencyString;
  ArraySeparator := ';';
  R1C1Reference := False;
  UpdateOperations;
end;

procedure TdxSpreadSheetFormatSettings.UpdateBreakChars;
var
  I: TdxSpreadSheetFormulaOperation;
  S: TStringBuilder;
begin
  S := TdxStringBuilderManager.Get(32);
  try
    S.Append(dxTab);
    S.Append(dxLF);
    S.Append(dxCR);
    S.Append(dxAbsoluteReferenceChar);
    S.Append(dxStringMarkChar);
    S.Append(dxRefSeparator);
    S.Append(dxAreaSeparator);
    S.Append(dxStringMarkChar2);
    S.Append(dxLeftParenthesis);
    S.Append(dxRightParenthesis);
    S.Append(dxLeftArrayParenthesis);
    S.Append(dxRightArrayParenthesis);
    S.Append(ArraySeparator);
    S.Append(ListSeparator);

    if not R1C1Reference then
    begin
      S.Append(dxReferenceLeftParenthesis);
      S.Append(dxReferenceRightParenthesis);
    end;

    for I := Low(Operations) to High(Operations) do
    begin
      if Length(Operations[I]) = 1 then
        S.Append(Operations[I]);
    end;

    BreakChars := S.ToString;
  finally
    TdxStringBuilderManager.Release(S);
  end;
end;

procedure TdxSpreadSheetFormatSettings.UpdateOperations;
begin
  Operations := dxDefaultOperations;
  Operations[opRange] := dxAreaSeparator;
  UpdateBreakChars;
end;

function TdxSpreadSheetFormatSettings.GetDecimalSeparator: Char;
begin
  Result := Data.DecimalSeparator;
end;

function TdxSpreadSheetFormatSettings.GetListSeparator: Char;
begin
  Result := Data.ListSeparator;
end;

procedure TdxSpreadSheetFormatSettings.SetDecimalSeparator(const AValue: Char);
begin
  Data.DecimalSeparator := AValue;
end;

procedure TdxSpreadSheetFormatSettings.SetListSeparator(const AValue: Char);
begin
  Data.ListSeparator := AValue;
end;

{ TdxSpreadSheetObjectLink }

constructor TdxSpreadSheetObjectLink.Create(AObject: TObject);
begin
  FObject := AObject;
end;

function TdxSpreadSheetObjectLink.GetObject: TObject;
begin
  Result := FObject;
end;

procedure TdxSpreadSheetObjectLink.ReleaseObject;
begin
  TdxSpreadSheetInvalidObject.AssignTo(FObject);
end;

{ TdxSpreadSheetInvalidObject }

class procedure TdxSpreadSheetInvalidObject.AssignTo(var AObject);
begin
  TObject(AObject) := Instance;
end;

class function TdxSpreadSheetInvalidObject.Instance: TObject;
begin
  Result := dxSpreadSheetInvalidObject;
end;

class function TdxSpreadSheetInvalidObject.IsInvalid(const AObject
  : TObject): Boolean;
begin
  Result := AObject = Instance;
end;

class function TdxSpreadSheetInvalidObject.IsLive(const AObject
  : TObject): Boolean;
begin
  Result := (AObject <> nil) and (AObject <> Instance);
end;

function TdxSpreadSheetInvalidObject.GetObject: TObject;
begin
  Result := Self;
end;

procedure TdxSpreadSheetInvalidObject.ReleaseObject;
begin
  // do nothing
end;

initialization

dxSpreadSheetInvalidObject := TdxSpreadSheetInvalidObject.Create;

finalization

FreeAndNil(dxSpreadSheetInvalidObject);

end.
