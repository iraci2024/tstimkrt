{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressEditors }
{ }
{ Copyright (c) 1998-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL }
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

unit dxMeasurementUnitEdit; // for internal use

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
  Classes, SysUtils, Controls, Variants, dxCore, cxSpinEdit, cxEdit;

type

  TdxMeasurementUnitEditIncrementValueEvent = procedure(Sender: TObject;
    AButton: TcxSpinEditButton; var AValue: Variant; var AHandled: Boolean)
    of object;

  { TdxMeasurementUnitEditProperties }

  TdxMeasurementUnitEditProperties = class(TcxCustomSpinEditProperties)
  private
    FOnIncrementValue: TdxMeasurementUnitEditIncrementValueEvent;
  protected
    function CheckValueBounds(const Value: Variant): Variant; override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function DoGetNewValue(Sender: TObject; AButton: TcxSpinEditButton;
      var AValue: Variant): Boolean; virtual;
    function IsDisplayValueNumeric: Boolean; override;
    function PrepareValue(const AValue: Variant): Variant; override;
  public
    constructor Create(AOwner: TPersistent); override;
    function IsEditValueValid(var AEditValue: TcxEditValue;
      AEditFocused: Boolean): Boolean; override;
    procedure ValidateDisplayValue(var ADisplayValue: TcxEditValue;
      var AErrorText: TCaption; var AError: Boolean;
      AEdit: TcxCustomEdit); override;
  published
    property ValidateOnEnter;
    property ValidationOptions default [evoShowErrorIcon, evoAllowLoseFocus];

    property OnChange;
    property OnIncrementValue: TdxMeasurementUnitEditIncrementValueEvent
      read FOnIncrementValue write FOnIncrementValue;
    property OnEditValueChanged;
    property OnValidate;
  end;

  { TdxMeasurementUnitEdit }

  TdxMeasurementUnitEdit = class(TcxCustomSpinEdit)
  private
    function GetActiveProperties: TdxMeasurementUnitEditProperties;
    function GetProperties: TdxMeasurementUnitEditProperties;
    procedure SetProperties(const Value: TdxMeasurementUnitEditProperties);
  protected
    procedure CheckEditorValueBounds; override;
    function GetNewValue(AButton: TcxSpinEditButton; out AValue: Variant)
      : Boolean; override;
    function GetValue: Variant; override;
    function IsValidChar(AChar: Char): Boolean; override;
    procedure SetValue(const Value: Variant); override;
    procedure Initialize; override;
    property Value;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue;
      out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    property ActiveProperties: TdxMeasurementUnitEditProperties
      read GetActiveProperties;
  published
    property Properties: TdxMeasurementUnitEditProperties read GetProperties
      write SetProperties;

    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property ParentColor;
    property TabOrder;
  end;

  { TdxMeasurementUnitEditHelper }

  TdxMeasurementUnitEditHelper = class
  private
    FDefaultDescription: string;
    FDescription: string;
    FIncrement: Currency;
    FPossibleDescriptions: TStringList;
    FMaxPrecision: Integer;
    FMinValue: Currency;
    FMaxValue: Currency;
    function IsValidValue(AValue: Extended): Boolean;
    function PossibleDescriptionIndexOf(const ADescription: string): Integer;
    procedure SetDefaultDescription(const AValue: string);
    procedure SetDescription(const AValue: string);
    function TryGetValue(AText: string; out AValue: Currency): Boolean;
  protected
    class procedure ExtractActualValueAndDescriptionStrings(const AText: string;
      out AValueStr, ADescription: string);
    function GetCheckableCandidate(const ADescription: string): string; virtual;
  public
    constructor Create(const ADescription: string; AIncrement: Currency;
      AMaxPrecision: Integer; AMinValue, AMaxValue: Currency;
      const ADefaultDescription: string = '');
    destructor Destroy; override;
    function CorrectRange(const AValue: Variant): Variant;
    function GetDescriptionFromText(const AText: string): string;
    function GetValueFromText(const AText: string;
      ACorrectRange: Boolean = True): Variant;
    function GetTextFromValue(const AValue: Variant): string;
    function IncrementValue(AButton: TcxSpinEditButton;
      var AText: Variant): Boolean;
    function IsPossibleDescription(const ADescription: string): Boolean;

    procedure AddPossibleDescription(const ADescription: string);
    procedure ClearPossibleDescriptions;
    procedure RemovePossibleDescription(const ADescription: string);

    property DefaultDescription: string read FDefaultDescription
      write SetDefaultDescription;
    property Description: string read FDescription write SetDescription;
    property Increment: Currency read FIncrement write FIncrement;
    property MaxPrecision: Integer read FMaxPrecision write FMaxPrecision;
    property MaxValue: Currency read FMaxValue write FMaxValue;
    property MinValue: Currency read FMinValue write FMinValue;
  end;

implementation

uses
  Math, Windows;

{ TdxMeasurementUnitEditProperties }

constructor TdxMeasurementUnitEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  ValidationOptions := [evoShowErrorIcon, evoAllowLoseFocus];
end;

function TdxMeasurementUnitEditProperties.IsEditValueValid(var AEditValue
  : TcxEditValue; AEditFocused: Boolean): Boolean;
begin
  Result := True;
end;

procedure TdxMeasurementUnitEditProperties.ValidateDisplayValue
  (var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean; AEdit: TcxCustomEdit);
var
  AIsUserErrorDisplayValue: Boolean;
begin
  DoValidate(ADisplayValue, AErrorText, AError, AEdit,
    AIsUserErrorDisplayValue);
end;

function TdxMeasurementUnitEditProperties.CheckValueBounds
  (const Value: Variant): Variant;
begin
  Result := Value;
end;

procedure TdxMeasurementUnitEditProperties.DoAssign
  (AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TdxMeasurementUnitEditProperties then
    Self.FOnIncrementValue := TdxMeasurementUnitEditProperties(AProperties)
      .OnIncrementValue;
end;

function TdxMeasurementUnitEditProperties.DoGetNewValue(Sender: TObject;
  AButton: TcxSpinEditButton; var AValue: Variant): Boolean;
begin
  Result := False;
  if Assigned(FOnIncrementValue) then
    FOnIncrementValue(Sender, AButton, AValue, Result);
end;

function TdxMeasurementUnitEditProperties.IsDisplayValueNumeric: Boolean;
begin
  Result := False;
end;

function TdxMeasurementUnitEditProperties.PrepareValue
  (const AValue: Variant): Variant;
begin
  Result := AValue;
end;

{ TdxMeasurementUnitEdit }

class function TdxMeasurementUnitEdit.GetPropertiesClass
  : TcxCustomEditPropertiesClass;
begin
  Result := TdxMeasurementUnitEditProperties;
end;

procedure TdxMeasurementUnitEdit.PrepareEditValue(const ADisplayValue
  : TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean);
begin
  EditValue := ADisplayValue;
end;

procedure TdxMeasurementUnitEdit.CheckEditorValueBounds;
begin
end;

function TdxMeasurementUnitEdit.GetNewValue(AButton: TcxSpinEditButton;
  out AValue: Variant): Boolean;
begin
  AValue := DisplayValue;
  Result := ActiveProperties.DoGetNewValue(Self, AButton, AValue)
end;

function TdxMeasurementUnitEdit.GetValue: Variant;
begin
  Result := InternalEditValue;
end;

procedure TdxMeasurementUnitEdit.Initialize;
begin
  inherited Initialize;
  InternalEditValue := Null;
end;

function TdxMeasurementUnitEdit.IsValidChar(AChar: Char): Boolean;
begin
  Result := (Ord(AChar) >= 32) or (AChar = Char(#8));
end;

procedure TdxMeasurementUnitEdit.SetValue(const Value: Variant);
begin
  InternalEditValue := Value;
end;

function TdxMeasurementUnitEdit.GetActiveProperties
  : TdxMeasurementUnitEditProperties;
begin
  Result := TdxMeasurementUnitEditProperties(inherited ActiveProperties);
end;

function TdxMeasurementUnitEdit.GetProperties: TdxMeasurementUnitEditProperties;
begin
  Result := TdxMeasurementUnitEditProperties(inherited Properties);
end;

procedure TdxMeasurementUnitEdit.SetProperties(const Value
  : TdxMeasurementUnitEditProperties);
begin
  inherited Properties := Value;
end;

{ TdxMeasurementUnitEditHelper }

constructor TdxMeasurementUnitEditHelper.Create(const ADescription: string;
  AIncrement: Currency; AMaxPrecision: Integer; AMinValue, AMaxValue: Currency;
  const ADefaultDescription: string = '');
begin
  inherited Create;
  FDescription := ADescription;
  FDefaultDescription := ADefaultDescription;
  FIncrement := AIncrement;
  FMaxPrecision := AMaxPrecision;
  FMaxValue := AMaxValue;
  FMinValue := AMinValue;
  FPossibleDescriptions := TStringList.Create;
end;

destructor TdxMeasurementUnitEditHelper.Destroy;
begin
  FreeAndNil(FPossibleDescriptions);
  inherited Destroy;
end;

procedure TdxMeasurementUnitEditHelper.AddPossibleDescription
  (const ADescription: string);
var
  ACandidate: string;
begin
  ACandidate := AnsiUpperCase(Trim(ADescription));
  if (ACandidate <> '') and (PossibleDescriptionIndexOf(ACandidate) < 0) then
    FPossibleDescriptions.Add(ADescription);
end;

procedure TdxMeasurementUnitEditHelper.ClearPossibleDescriptions;
begin
  FPossibleDescriptions.Clear;
end;

procedure TdxMeasurementUnitEditHelper.RemovePossibleDescription
  (const ADescription: string);
var
  ACandidate: string;
  AIndex: Integer;
begin
  ACandidate := AnsiUpperCase(Trim(ADescription));
  AIndex := PossibleDescriptionIndexOf(ACandidate);
  while AIndex >= 0 do
  begin
    FPossibleDescriptions.Delete(AIndex);
    AIndex := PossibleDescriptionIndexOf(ACandidate);
  end;
end;

function TdxMeasurementUnitEditHelper.CorrectRange(const AValue
  : Variant): Variant;
begin
  Result := AValue;
  if not VarIsNull(AValue) then
    if AValue > MaxValue then
      Result := MaxValue
    else if AValue < MinValue then
      Result := MinValue;
end;

function TdxMeasurementUnitEditHelper.GetTextFromValue
  (const AValue: Variant): string;

  function GetFormatString: string;
  var
    I: Integer;
    ADecimalFormat: string;
  begin
    ADecimalFormat := '';
    for I := 0 to MaxPrecision - 1 do
      if ADecimalFormat = '' then
        ADecimalFormat := '.#'
      else
        ADecimalFormat := ADecimalFormat + '#';
    Result := '0' + ADecimalFormat;
  end;

begin
  Result := '';
  if not VarIsNull(AValue) then
    if Description = '' then
      Result := FormatFloat(GetFormatString, AValue)
    else
      Result := FormatFloat(GetFormatString, AValue) + Description;
end;

class procedure TdxMeasurementUnitEditHelper.
  ExtractActualValueAndDescriptionStrings(const AText: string;
  out AValueStr, ADescription: string);
var
  I, P: Integer;
begin
  AValueStr := '';
  ADescription := Trim(AText);
  P := Length(ADescription);
  for I := 1 to Length(ADescription) do
{$IFDEF DELPHI15}
    if not CharInSet(ADescription[I], ['0' .. '9']) and
      not CharInSet(ADescription[I], [FormatSettings.DecimalSeparator, '-'])
    then
{$ELSE}
    if not CharInSet(ADescription[I], ['0' .. '9']) and
      not CharInSet(ADescription[I], [SysUtils.DecimalSeparator, '-']) then
{$ENDIF}
    begin
      P := I - 1;
      Break;
    end;
  if P > 0 then
  begin
    AValueStr := Copy(ADescription, 1, P);
    Delete(ADescription, 1, P);
  end;

end;

function TdxMeasurementUnitEditHelper.GetCheckableCandidate(const ADescription
  : string): string;
begin
  Result := AnsiUpperCase(Trim(ADescription));
end;

function TdxMeasurementUnitEditHelper.GetDescriptionFromText
  (const AText: string): string;
var
  AValueStr: string;
begin
  ExtractActualValueAndDescriptionStrings(AText, AValueStr, Result);
end;

function TdxMeasurementUnitEditHelper.GetValueFromText(const AText: string;
  ACorrectRange: Boolean = True): Variant;
var
  AValue: Currency;
begin
  if TryGetValue(AText, AValue) and
    (not ACorrectRange or (CorrectRange(AValue) = AValue)) then
    Result := AValue
  else
    Result := Null;
end;

function TdxMeasurementUnitEditHelper.IncrementValue(AButton: TcxSpinEditButton;
  var AText: Variant): Boolean;

  function GetIncrement(const AValue: Currency): Currency;
  var
    AIncrement: Currency;
  begin
    if AButton = sebBackward then
      AIncrement := -Increment
    else
      AIncrement := Increment;
    Result := RoundTo(Trunc((AValue + AIncrement) / Increment) * Increment -
      AValue, -MaxPrecision);
    if (Abs(Result) > Increment) and not SameValue(Abs(Result), Increment) then
      Result := RoundTo(Result - AIncrement, -MaxPrecision);
  end;

var
  AValue: Variant;
  ANewValue: Currency;
  AValueStr, AActualDescription: string;
  E: Extended;
begin
  if Increment = 0 then
    Exit(True);
  ExtractActualValueAndDescriptionStrings(VarToStr(AText), AValueStr,
    AActualDescription);
  if Trim(VarToStr(AText)) = '' then
    AActualDescription := Description;
  if TryStrToFloat(AValueStr, E) and IsValidValue(E) then
    AValue := E
  else
    AValue := CorrectRange(0);
  ANewValue := AValue + GetIncrement(AValue);
  if ANewValue > MaxValue then
    ANewValue := MaxValue
  else if ANewValue < MinValue then
    ANewValue := MinValue;
  if (Description <> AActualDescription) and
    IsPossibleDescription(AActualDescription) then
    Description := AActualDescription;
  AText := GetTextFromValue(ANewValue);
  Result := True;
end;

function TdxMeasurementUnitEditHelper.IsPossibleDescription(const ADescription
  : string): Boolean;
var
  ACandidate: string;
begin
  Result := True;
  ACandidate := GetCheckableCandidate(ADescription);
  if not(((ACandidate = '') and (DefaultDescription <> '')) or
    (ACandidate = AnsiUpperCase(Trim(Description)))) then
    Result := PossibleDescriptionIndexOf(ACandidate) >= 0;
end;

function TdxMeasurementUnitEditHelper.IsValidValue(AValue: Extended): Boolean;
var
  AMinValue, AMaxValue: Extended;
begin
  AMinValue := MinCurrency;
  AMaxValue := MaxCurrency;
  Result := (AValue > AMinValue) and (AValue < AMaxValue);
end;

function TdxMeasurementUnitEditHelper.PossibleDescriptionIndexOf
  (const ADescription: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FPossibleDescriptions.Count - 1 do
    if ADescription = AnsiUpperCase(Trim(FPossibleDescriptions[I])) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TdxMeasurementUnitEditHelper.SetDefaultDescription
  (const AValue: string);
begin
  if Trim(AValue) = '' then
    FDefaultDescription := ''
  else
    FDefaultDescription := AValue
end;

procedure TdxMeasurementUnitEditHelper.SetDescription(const AValue: string);
begin
  if Trim(AValue) = '' then
    FDescription := ''
  else
    FDescription := AValue;
end;

function TdxMeasurementUnitEditHelper.TryGetValue(AText: string;
  out AValue: Currency): Boolean;
var
  AValueStr, AActualDescription: string;
  E: Extended;
begin
  ExtractActualValueAndDescriptionStrings(AText, AValueStr, AActualDescription);
  Result := TryStrToFloat(AValueStr, E) and IsValidValue(E) and
    IsPossibleDescription(AActualDescription);
  if Result then
  begin
    AValue := E;
    RoundTo(AValue, -MaxPrecision);
  end;
end;

initialization

GetRegisteredEditProperties.Register(TdxMeasurementUnitEditProperties, '');

finalization

GetRegisteredEditProperties.Unregister(TdxMeasurementUnitEditProperties);

end.
