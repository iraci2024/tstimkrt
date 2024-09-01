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

unit dxSpreadSheetCoreReferences;

{$I cxVer.Inc}

interface

uses
  Types, Math, SysUtils, dxCore;

const
  dxMaxColumnNameLength = 3;
  dxMaxReferenceLength = 32;

type

  { TdxSpreadSheetReference }

  TdxSpreadSheetReference = record
    Value: Int64;

    function GetFlags: Integer; inline;
    function GetIsAbsolute: Boolean; inline;
    function GetIsAllItems: Boolean; inline;
    function GetIsError: Boolean; inline;
    function GetOffset: Integer; inline;
    procedure SetFlags(AValue: Integer); inline;
    procedure SetIsAbsolute(AValue: Boolean); inline;
    procedure SetIsAllItems(AValue: Boolean); inline;
    procedure SetIsError(AValue: Boolean); inline;
    procedure SetOffset(AValue: Integer); inline;
  public
    constructor Create(const AValue: Int64); overload;
    constructor Create(AIndex: Integer; AIsAbsolute: Boolean); overload;
    constructor Create(AIndex, AOrigin, AMaxIndex: Integer); overload;
    function ActualValue(const AOrigin: Integer): Integer; inline;
    procedure SetActualValue(const AOrigin, AValue: Integer);
    class function AllItems: TdxSpreadSheetReference; static;
    class function Invalid: TdxSpreadSheetReference; static;
    class operator Equal(const V1, V2: TdxSpreadSheetReference): Boolean;
    class operator NotEqual(const V1, V2: TdxSpreadSheetReference): Boolean;
    procedure Move(ADelta: Integer); inline;
    procedure Reset;
    function UpdateAreaReference(const AOrigin, AIndex1, AIndex2,
      ANewIndex1: Integer; var APoint2: TdxSpreadSheetReference;
      AMoveReference, AMoveOrigin: Boolean): Boolean; inline;
    function UpdateReference(const AOrigin, AIndex1, AIndex2,
      ANewIndex1: Integer; AMoveReference, AMoveOrigin: Boolean)
      : Boolean; inline;

    property Flags: Integer read GetFlags write SetFlags;
    property IsAbsolute: Boolean read GetIsAbsolute write SetIsAbsolute;
    property IsAllItems: Boolean read GetIsAllItems write SetIsAllItems;
    property IsError: Boolean read GetIsError write SetIsError;
    property Offset: Integer read GetOffset write SetOffset;
  end;

  { TdxSpreadSheetReferenceParser }

  TdxSpreadSheetReferenceParser = class
  strict private
  const
{$REGION 'Internal Types'}
    ciNumber = 0;
    ciAlpha = 1;
    ciRowRef = 2;
    ciColumnRef = 3;
    ciPlus = 4;
    ciMinus = 5;
    ciDollar = 6;
    ciLeftParenthesis = 7;
    ciRightParenthesis = 8;
    ciBreakChar = 9;

    StateError = -1;
    StateFinish = 1;
    StateR1C1Initial = 0;
    StateR1C1Row = 2;
    StateR1C1RowAbsoluteIndex = 3;
    StateR1C1RowRelative = 4;
    StateR1C1RowRelativeIndex = 5;
    StateR1C1RowRelativeNegative = 6;
    StateR1C1Column = 8;
    StateR1C1ColumnAbsoluteIndex = 9;
    StateR1C1ColumnRelative = 10;
    StateR1C1ColumnRelativeIndex = 11;
    StateR1C1ColumnRelativeNegative = 12;

    StateA1Initial = 14;
    StateA1ColumnAbsoluteIndex = 16;
    StateA1ColumnRelativeIndex = 17;
    StateA1ColumnIndex2 = 18;
    StateA1ColumnIndex3 = 19;
    StateA1RowAbsoluteIndex = 21;
    StateA1RowRelativeIndex = 22;

    StateMachine: array [0 .. 22, 0 .. 9] of ShortInt = (
      { 0-9 A-Z  R   C   +   -   $   [   ]  #0 }
      (-1, -1, 2, 8, -1, -1, -1, -1, -1, -1), // StateR1C1Initial
      (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1), // StateFinish
      (3, -1, -1, 8, 1, 1, -1, 4, -1, 1), // StateR1C1Row
      (3, -1, -1, 8, 1, 1, -1, -1, -1, 1), // StateR1C1RowAbsoluteIndex
      (5, -1, -1, -1, 5, 6, -1, -1, 7, -1), // StateR1C1RowRelative
      (5, -1, -1, -1, -1, -1, -1, -1, 7, -1), // StateR1C1RowRelativeIndex
      (5, -1, -1, -1, -1, -1, -1, -1, 7, -1), // StateR1C1RowRelativeNegative
      (-1, -1, -1, 8, 1, 1, -1, -1, -1, 1), (9, -1, -1, -1, 1, 1, -1, 10, -1,
      1), // StateR1C1Column
      (9, -1, -1, -1, 1, 1, -1, -1, -1, 1), // StateR1C1ColumnAbsoluteIndex
      (11, -1, -1, -1, 11, 12, -1, -1, -1, -1), // StateR1C1ColumnRelative
      (11, -1, -1, -1, -1, -1, -1, -1, 13, -1), // StateR1C1ColumnRelativeIndex
      (11, -1, -1, -1, -1, -1, -1, -1, 13, -1),
      // StateR1C1ColumnRelativeNegative
      (-1, -1, -1, -1, 1, 1, -1, -1, -1, 1),

      (22, 17, -1, -1, -1, -1, 15, -1, -1, -1), // StateA1Initial
      (21, 16, -1, -1, -1, -1, -1, -1, -1, -1), (22, 18, -1, -1, -1, -1, 20, -1,
      -1, 1), // StateA1ColumnAbsoluteIndex
      (22, 18, -1, -1, -1, -1, 20, -1, -1, 1), // StateA1ColumnRelativeIndex
      (22, 19, -1, -1, -1, -1, 20, -1, -1, 1), // StateA1ColumnIndex2
      (22, -1, -1, -1, -1, -1, 20, -1, -1, 1), // StateA1ColumnIndex3
      (21, -1, -1, -1, -1, -1, -1, -1, -1, 1), (21, -1, -1, -1, -1, -1, -1, -1,
      -1, 1), // StateA1RowAbsoluteIndex
      (22, -1, -1, -1, -1, -1, -1, -1, -1, 1) // StateA1RowRelativeIndex
      );
{$ENDREGION}
  protected type
    TGetCharIndexFunc = function(const C: Char): Integer;
  strict private
    class function GetA1CharIndex(const C: Char): Integer; static;
    class function GetR1C1CharIndex(const C: Char): Integer; static;
  protected
    class function ParseCore(const S: string; AInitialState: Integer;
      AGetCharIndexFunc: TGetCharIndexFunc; var AStartPos: Integer;
      AFinishPos: Integer; out ARow, AColumn: TdxSpreadSheetReference): Boolean;
  public
    class function Parse(const AUpCaseString: string;
      AR1C1ReferenceStyle: Boolean; var AStartPos: Integer; AFinishPos: Integer;
      out ARow, AColumn: TdxSpreadSheetReference): Boolean; inline;
    class function ParseA1(const AUpCaseString: string; var AStartPos: Integer;
      AFinishPos: Integer; out ARow, AColumn: TdxSpreadSheetReference): Boolean;
    class function ParseR1C1(const AUpCaseString: string;
      var AStartPos: Integer; AFinishPos: Integer;
      out ARow, AColumn: TdxSpreadSheetReference): Boolean;
  end;

  { TdxSpreadSheetReferencePath }

  TdxSpreadSheetReferencePath = class
  strict private
    FColumn: Integer;
    FNext: TdxSpreadSheetReferencePath;
    FRow: Integer;
    FSheet: TObject;
  public
    constructor Create(ARow, AColumn: Integer; ASheet: TObject);
    destructor Destroy; override;
    procedure Add(ARow, AColumn: Integer; ASheet: TObject);
    procedure Remove(ARow, AColumn: Integer; ASheet: TObject);
    function ToString: string; override;

    property Column: Integer read FColumn;
    property Next: TdxSpreadSheetReferencePath read FNext;
    property Row: Integer read FRow;
    property Sheet: TObject read FSheet;
  end;

  { TdxSpreadSheetColumnReferences }

  TdxSpreadSheetColumnReferences = class
  protected
    class procedure NameByIndex(const ABuffer: TStringBuilder;
      AIndex: Integer); overload;
  public
    class function IndexByName(const AName: string;
      R1C1ReferenceStyle: Boolean = False): Integer;
    class function NameByIndex(const AIndex: Integer;
      R1C1ReferenceStyle: Boolean = False): string; overload;
    class function TryIndexByName(const AName: string; out AIndex: Integer;
      R1C1ReferenceStyle: Boolean = False): Boolean;
  end;

  { TdxSpreadSheetReferenceAsString }

  TdxSpreadSheetReferenceAsString = class
  public
    class function Build(const ALink: TObject;
      const ARow, AColumn: TdxSpreadSheetReference;
      const ARowOrigin, AColumnOrigin: Integer; R1C1ReferenceStyle: Boolean)
      : string; overload; inline;
    class function Build(const AReference: string;
      const ARow, AColumn: TdxSpreadSheetReference;
      const ARowOrigin, AColumnOrigin: Integer; R1C1ReferenceStyle: Boolean)
      : string; overload; inline;
    class function Build(ABuffer: TStringBuilder; const AReference: string;
      const ARow, AColumn: TdxSpreadSheetReference;
      const ARowOrigin, AColumnOrigin: Integer; R1C1ReferenceStyle: Boolean)
      : string; overload;
    class function Build(ABuffer: TStringBuilder; const AReference: string;
      const AArea: TRect; const ARowOrigin, AColumnOrigin: Integer;
      R1C1ReferenceStyle: Boolean): string; overload;

    class function BuildArea(const ALink1: TObject;
      const ARow1, AColumn1: TdxSpreadSheetReference; const ALink2: TObject;
      const ARow2, AColumn2: TdxSpreadSheetReference;
      const ARowOrigin, AColumnOrigin: Integer; R1C1ReferenceStyle: Boolean)
      : string; overload; inline;
    class procedure BuildArea(ABuffer: TStringBuilder; const ALink1: TObject;
      const ARow1, AColumn1: TdxSpreadSheetReference; const ALink2: TObject;
      const ARow2, AColumn2: TdxSpreadSheetReference;
      const ARowOrigin, AColumnOrigin: Integer; R1C1ReferenceStyle: Boolean);
      overload; inline;
    class procedure BuildArea(ABuffer: TStringBuilder;
      const AReference1: string; const ARow1, AColumn1: TdxSpreadSheetReference;
      const AReference2: string; const ARow2, AColumn2: TdxSpreadSheetReference;
      const ARowOrigin, AColumnOrigin: Integer; R1C1ReferenceStyle: Boolean);
      overload; inline;

    class function BuildLinkReference(const ALink: TObject): string; inline;
    class function BuildViewReference(const AView: TObject): string; overload;
    class function BuildViewReference(const AViewName: string;
      AEscape: TdxDefaultBoolean = bDefault): string; overload;

    class function CheckIfViewNameMustBeEscaped(const AName,
      ABreakChars: string): Boolean;
  end;

function dxTryStringToReference(const S: string; R1C1ReferenceStyle: Boolean;
  out ARow, AColumn: Integer): Boolean; overload;
function dxTryStringToReference(const S: string; R1C1ReferenceStyle: Boolean;
  out ARow, AColumn: TdxSpreadSheetReference): Boolean; overload;
function dxTryStringToReferenceArea(const S: string; out AArea: TRect)
  : Boolean; overload;
function dxTryStringToReferenceArea(const S: string;
  R1C1ReferenceStyle: Boolean; out AArea: TRect): Boolean; overload;

implementation

uses
  dxStringHelper,
  dxSpreadSheetCoreStrs,
  dxSpreadSheetTypes;

type
  TInt64 = record
    Hi, Low: Integer;
  end;

const
  dxRefAbsolute = 1;
  dxRefAllItems = 2;
  dxRefError = 4;

function dxTryStringToReference(const S: string; R1C1ReferenceStyle: Boolean;
  out ARow, AColumn: TdxSpreadSheetReference): Boolean;
var
  AStartPos: Integer;
  AFinishPos: Integer;
begin
  AStartPos := 1;
  AFinishPos := Length(S);
  Result := TdxSpreadSheetReferenceParser.Parse(UpperCase(S),
    R1C1ReferenceStyle, AStartPos, AFinishPos, ARow, AColumn) and
    (AStartPos > AFinishPos);
end;

function dxTryStringToReference(const S: string; R1C1ReferenceStyle: Boolean;
  out ARow, AColumn: Integer): Boolean; overload;
var
  R, C: TdxSpreadSheetReference;
begin
  Result := dxTryStringToReference(S, R1C1ReferenceStyle, R, C);
  if Result then
  begin
    if R.IsAllItems then
      ARow := MaxInt
    else
      ARow := R.Offset;

    if C.IsAllItems then
      AColumn := MaxInt
    else
      AColumn := C.Offset;
  end;
end;

function dxTryStringToReferenceArea(const S: string;
  R1C1ReferenceStyle: Boolean; out AArea: TRect): Boolean;
var
  AAreaSeparatorPosition: Integer;
begin
  AAreaSeparatorPosition := Pos(dxAreaSeparator, S);
  if AAreaSeparatorPosition > 0 then
  begin
    Result := dxTryStringToReference(Copy(S, 1, AAreaSeparatorPosition - 1),
      R1C1ReferenceStyle, AArea.Top, AArea.Left) and
      dxTryStringToReference(Copy(S, AAreaSeparatorPosition + 1, MaxInt),
      R1C1ReferenceStyle, AArea.Bottom, AArea.Right);

    if Result then
    begin
      if AArea.Left = MaxInt then
        AArea.Left := 0;
      if AArea.Top = MaxInt then
        AArea.Top := 0;
      if AArea.Right < 0 then
        AArea.Right := MaxInt;
    end;
  end
  else
  begin
    Result := dxTryStringToReference(S, R1C1ReferenceStyle, AArea.Top,
      AArea.Left);
    if Result then
      AArea.BottomRight := AArea.TopLeft;
  end;
end;

function dxTryStringToReferenceArea(const S: string; out AArea: TRect): Boolean;
begin
  Result := dxTryStringToReferenceArea(S, False, AArea) or
    dxTryStringToReferenceArea(S, True, AArea);
end;

{ TdxSpreadSheetReference }

constructor TdxSpreadSheetReference.Create(const AValue: Int64);
begin
  Value := AValue;
end;

constructor TdxSpreadSheetReference.Create(AIndex: Integer;
  AIsAbsolute: Boolean);
begin
  Reset;
  Offset := AIndex;
  IsAbsolute := AIsAbsolute;
end;

constructor TdxSpreadSheetReference.Create(AIndex, AOrigin, AMaxIndex: Integer);
begin
  Reset;
  IsAbsolute := AOrigin < 0;
  if IsAbsolute then
    Offset := AIndex
  else
    Offset := AIndex - AOrigin;

  if Offset >= AMaxIndex then
    IsAllItems := True;
end;

function TdxSpreadSheetReference.ActualValue(const AOrigin: Integer): Integer;
begin
  if IsError then
    Result := -1
  else if IsAllItems then
    Result := MaxInt
  else if IsAbsolute then
    Result := Offset
  else
    Result := Offset + AOrigin;
end;

procedure TdxSpreadSheetReference.SetActualValue(const AOrigin,
  AValue: Integer);
begin
  if IsAbsolute then
    Offset := AValue
  else
    Offset := AValue - AOrigin;

  IsError := IsError or (ActualValue(AOrigin) < 0);
end;

class function TdxSpreadSheetReference.AllItems: TdxSpreadSheetReference;
begin
  Result.Reset;
  Result.IsAllItems := True;
end;

class function TdxSpreadSheetReference.Invalid: TdxSpreadSheetReference;
begin
  Result.Reset;
  Result.Offset := MinInt;
end;

class operator TdxSpreadSheetReference.Equal(const V1,
  V2: TdxSpreadSheetReference): Boolean;
begin
  Result := V1.Value = V2.Value;
end;

class operator TdxSpreadSheetReference.NotEqual(const V1,
  V2: TdxSpreadSheetReference): Boolean;
begin
  Result := not(V1 = V2);
end;

procedure TdxSpreadSheetReference.Move(ADelta: Integer);
begin
  if IsAbsolute or IsError or IsAllItems then
    Exit;
  Offset := Offset + ADelta;
end;

procedure TdxSpreadSheetReference.Reset;
begin
  Value := 0;
end;

function TdxSpreadSheetReference.UpdateAreaReference(const AOrigin, AIndex1,
  AIndex2, ANewIndex1: Integer; var APoint2: TdxSpreadSheetReference;
  AMoveReference, AMoveOrigin: Boolean): Boolean;
var
  ADestOrigin, ARef1, ARef2, ADelta, AOffset1, AOffset2: Integer;
begin
  Result := False;
  if IsError or IsAllItems or APoint2.IsError or APoint2.IsAllItems then
    Exit;

  if ANewIndex1 >= 0 then
    ADelta := ANewIndex1 - AIndex1
  else
    ADelta := AIndex2 - AIndex1 + 1;

  ADestOrigin := AOrigin;
  if (AIndex1 <= AOrigin) and AMoveOrigin then
    ADestOrigin := ADestOrigin - ADelta * (Ord(ANewIndex1 < 0) * 2 - 1);

  AOffset1 := Offset;
  AOffset2 := APoint2.Offset;

  ARef1 := ActualValue(AOrigin);
  ARef2 := APoint2.ActualValue(AOrigin);

  if AMoveReference then
  begin
    if ANewIndex1 >= 0 then
    begin
      if ARef1 >= AIndex1 then
        Inc(ARef1, ADelta);
      if ARef2 >= AIndex1 then
        Inc(ARef2, ADelta);
    end
    else
    begin
      IsError := (AIndex1 <= ARef1) and (AIndex2 >= ARef2);

      if ARef1 > AIndex2 then
        Dec(ARef1, ADelta)
      else if ARef1 >= AIndex1 then
        ARef1 := AIndex1;

      if ARef2 >= AIndex2 then
        Dec(ARef2, ADelta)
      else if AIndex1 < ARef2 then
        ARef2 := AIndex1;
    end;
  end;

  SetActualValue(ADestOrigin, ARef1);
  APoint2.SetActualValue(ADestOrigin, ARef2);

  IsError := IsError or APoint2.IsError or
    (APoint2.ActualValue(AOrigin) < ActualValue(AOrigin));
  APoint2.IsError := IsError;
  Result := IsError or (AOffset1 <> Offset) or (AOffset2 <> APoint2.Offset);
end;

function TdxSpreadSheetReference.UpdateReference(const AOrigin, AIndex1,
  AIndex2, ANewIndex1: Integer; AMoveReference, AMoveOrigin: Boolean): Boolean;
var
  ADestOrigin, ARef, ADelta, AOffset: Integer;
begin
  Result := False;
  if IsError or IsAllItems or (AIndex1 = ANewIndex1) then
    Exit;

  if ANewIndex1 < 0 then
    ADelta := AIndex2 - AIndex1 + 1
  else
    ADelta := ANewIndex1 - AIndex1;

  ADestOrigin := AOrigin;
  if (AIndex1 <= AOrigin) and AMoveOrigin then
    ADestOrigin := ADestOrigin - ADelta * (Ord(ANewIndex1 < 0) * 2 - 1);

  ARef := ActualValue(AOrigin);
  AOffset := Offset;
  if AMoveReference then
  begin
    if ANewIndex1 < 0 then
    begin
      IsError := InRange(ARef, AIndex1, AIndex2);
      if ARef >= AIndex2 then
        Dec(ARef, ADelta)
      else if ARef >= AIndex1 then
        ARef := AIndex1;
    end
    else
    begin
      if ARef >= AIndex1 then
        Inc(ARef, ADelta);
    end;
  end;
  SetActualValue(ADestOrigin, ARef);
  Result := IsError or (AOffset <> Offset);
end;

function TdxSpreadSheetReference.GetFlags: Integer;
begin
  Result := TInt64(Value).Hi;
end;

function TdxSpreadSheetReference.GetIsAbsolute: Boolean;
begin
  Result := Flags and dxRefAbsolute = dxRefAbsolute;
end;

function TdxSpreadSheetReference.GetIsAllItems: Boolean;
begin
  Result := Flags and dxRefAllItems = dxRefAllItems;
end;

function TdxSpreadSheetReference.GetIsError: Boolean;
begin
  Result := Flags and dxRefError = dxRefError;
end;

function TdxSpreadSheetReference.GetOffset: Integer;
begin
  Result := TInt64(Value).Low;
end;

procedure TdxSpreadSheetReference.SetFlags(AValue: Integer);
begin
  TInt64(Value).Hi := AValue;
end;

procedure TdxSpreadSheetReference.SetIsAbsolute(AValue: Boolean);
begin
  if AValue then
    Flags := Flags or dxRefAbsolute
  else
    Flags := Flags and not dxRefAbsolute;
end;

procedure TdxSpreadSheetReference.SetIsAllItems(AValue: Boolean);
begin
  if AValue then
    Flags := Flags or dxRefAllItems
  else
    Flags := Flags and not dxRefAllItems;
end;

procedure TdxSpreadSheetReference.SetIsError(AValue: Boolean);
begin
  if AValue then
    Flags := Flags or dxRefError
  else
    Flags := Flags and not dxRefError;
end;

procedure TdxSpreadSheetReference.SetOffset(AValue: Integer);
begin
  TInt64(Value).Low := AValue;
  IsAllItems := AValue = MaxInt;
end;

{ TdxSpreadSheetReferenceParser }

class function TdxSpreadSheetReferenceParser.Parse(const AUpCaseString: string;
  AR1C1ReferenceStyle: Boolean; var AStartPos: Integer; AFinishPos: Integer;
  out ARow, AColumn: TdxSpreadSheetReference): Boolean;
begin
  if AR1C1ReferenceStyle then
    Result := ParseR1C1(AUpCaseString, AStartPos, AFinishPos, ARow, AColumn)
  else
    Result := ParseA1(AUpCaseString, AStartPos, AFinishPos, ARow, AColumn);
end;

class function TdxSpreadSheetReferenceParser.ParseA1(const AUpCaseString
  : string; var AStartPos: Integer; AFinishPos: Integer;
  out ARow, AColumn: TdxSpreadSheetReference): Boolean;
begin
  Result := ParseCore(AUpCaseString, StateA1Initial, GetA1CharIndex, AStartPos,
    AFinishPos, ARow, AColumn);
  if Result then
  begin
    if not AColumn.IsAllItems then
      AColumn.Offset := AColumn.Offset - 1;
    if not ARow.IsAllItems then
      ARow.Offset := ARow.Offset - 1;
  end;
end;

class function TdxSpreadSheetReferenceParser.ParseR1C1(const AUpCaseString
  : string; var AStartPos: Integer; AFinishPos: Integer;
  out ARow, AColumn: TdxSpreadSheetReference): Boolean;
begin
  Result := ParseCore(AUpCaseString, StateR1C1Initial, GetR1C1CharIndex,
    AStartPos, AFinishPos, ARow, AColumn);
  if Result then
  begin
    if AColumn.IsAbsolute and not AColumn.IsAllItems then
      AColumn.Offset := AColumn.Offset - 1;
    if ARow.IsAbsolute and not ARow.IsAllItems then
      ARow.Offset := ARow.Offset - 1;
  end;
end;

class function TdxSpreadSheetReferenceParser.ParseCore(const S: string;
  AInitialState: Integer; AGetCharIndexFunc: TGetCharIndexFunc;
  var AStartPos: Integer; AFinishPos: Integer;
  out ARow, AColumn: TdxSpreadSheetReference): Boolean;
var
  AColumnIndex: Integer;
  AColumnIndexIsNegative: Boolean;
  AColumnIsAbsolute: Boolean;
  AColumnIsAll: Boolean;
  ARowIndex: Integer;
  ARowIndexIsNegative: Boolean;
  ARowIsAbsolute: Boolean;
  ARowIsAll: Boolean;

  function NextState(AChar: Char; AState: Integer): Integer;
  var
    ACharIndex: Integer;
  begin
    ACharIndex := AGetCharIndexFunc(AChar);
    Result := StateMachine[AState, ACharIndex];

    case Result of
      StateError, StateFinish:
        Exit;
      StateA1RowAbsoluteIndex, StateA1RowRelativeIndex,
        StateR1C1RowAbsoluteIndex, StateR1C1RowRelativeIndex:
        ARowIndex := ARowIndex * 10 + (Ord(AChar) - Ord('0'));
      StateR1C1ColumnAbsoluteIndex, StateR1C1ColumnRelativeIndex:
        AColumnIndex := AColumnIndex * 10 + (Ord(AChar) - Ord('0'));
      StateA1ColumnAbsoluteIndex, StateA1ColumnRelativeIndex,
        StateA1ColumnIndex2, StateA1ColumnIndex3:
        AColumnIndex := AColumnIndex * 26 + (Ord(AChar) - Ord('A') + 1);
      StateR1C1RowRelativeNegative:
        ARowIndexIsNegative := True;
      StateR1C1ColumnRelativeNegative:
        AColumnIndexIsNegative := True;
    end;

    case Result of
      StateA1ColumnAbsoluteIndex, StateR1C1ColumnAbsoluteIndex:
        AColumnIsAbsolute := True;
      StateA1ColumnRelativeIndex, StateR1C1ColumnRelative:
        AColumnIsAbsolute := False;
      StateA1RowAbsoluteIndex, StateR1C1RowAbsoluteIndex:
        ARowIsAbsolute := True;
      StateA1RowRelativeIndex, StateR1C1RowRelative:
        ARowIsAbsolute := False;
    end;

    case Result of
      StateA1ColumnAbsoluteIndex, StateA1ColumnRelativeIndex, StateR1C1Column,
        StateR1C1ColumnAbsoluteIndex, StateR1C1ColumnRelativeIndex:
        AColumnIsAll := False;

      StateA1RowAbsoluteIndex, StateA1RowRelativeIndex, StateR1C1Row,
        StateR1C1RowRelativeIndex, StateR1C1RowAbsoluteIndex:
        ARowIsAll := False;
    end;
  end;

var
  APosition: Integer;
  AState: Integer;
begin
  AColumnIndex := 0;
  AColumnIndexIsNegative := False;
  AColumnIsAbsolute := False;
  AColumnIsAll := True;
  ARowIndex := 0;
  ARowIndexIsNegative := False;
  ARowIsAbsolute := False;
  ARowIsAll := True;

  AState := AInitialState;
  APosition := AStartPos;
  while APosition <= AFinishPos do
  begin
    AState := NextState(S[APosition], AState);
    if AState = StateError then
      Exit(False);
    if AState = StateFinish then
      Break;
    Inc(APosition);
  end;

  if AState <> StateFinish then
    AState := NextState(#0, AState);

  Result := AState = StateFinish;
  if Result then
  begin
    if ARowIndexIsNegative then
      ARowIndex := -ARowIndex;
    if AColumnIndexIsNegative then
      AColumnIndex := -AColumnIndex;

    ARow.Reset;
    ARow.Offset := ARowIndex;
    ARow.IsAbsolute := ARowIsAbsolute;
    ARow.IsAllItems := ARowIsAll;

    AColumn.Reset;
    AColumn.Offset := AColumnIndex;
    AColumn.IsAbsolute := AColumnIsAbsolute;
    AColumn.IsAllItems := AColumnIsAll;

    AStartPos := APosition;
  end;
end;

class function TdxSpreadSheetReferenceParser.GetA1CharIndex
  (const C: Char): Integer;
begin
  if dxCharInSet(C, ['0' .. '9']) then
    Exit(ciNumber);
  if dxCharInSet(C, ['A' .. 'Z']) then
    Exit(ciAlpha);
  if Ord(C) = Ord(dxAbsoluteReferenceChar) then
    Exit(ciDollar);
  Result := ciBreakChar;
end;

class function TdxSpreadSheetReferenceParser.GetR1C1CharIndex
  (const C: Char): Integer;
begin
  if dxCharInSet(C, ['0' .. '9']) then
    Exit(ciNumber);
  if Ord(C) = Ord('+') then
    Exit(ciPlus);
  if Ord(C) = Ord('-') then
    Exit(ciMinus);
  if Ord(C) = Ord(dxRCRowReferenceChar) then
    Exit(ciRowRef);
  if Ord(C) = Ord(dxRCColumnReferenceChar) then
    Exit(ciColumnRef);
  if Ord(C) = Ord(dxReferenceLeftParenthesis) then
    Exit(ciLeftParenthesis);
  if Ord(C) = Ord(dxReferenceRightParenthesis) then
    Exit(ciRightParenthesis);
  if dxCharInSet(C, ['A' .. 'Z']) then
    Exit(ciAlpha);
  Result := ciBreakChar;
end;

{ TdxSpreadSheetReferencePath }

constructor TdxSpreadSheetReferencePath.Create(ARow, AColumn: Integer;
  ASheet: TObject);
begin
  FColumn := AColumn;
  FRow := ARow;
  FSheet := ASheet;
end;

destructor TdxSpreadSheetReferencePath.Destroy;
begin
  FreeAndNil(FNext);
  inherited Destroy;
end;

procedure TdxSpreadSheetReferencePath.Add(ARow, AColumn: Integer;
  ASheet: TObject);
var
  ALast, AItem: TdxSpreadSheetReferencePath;
begin
  AItem := TdxSpreadSheetReferencePath.Create(ARow, AColumn, ASheet);
  ALast := Self;
  while ALast.Next <> nil do
    ALast := ALast.Next;
  ALast.FNext := AItem;
end;

procedure TdxSpreadSheetReferencePath.Remove(ARow, AColumn: Integer;
  ASheet: TObject);
var
  ABeforeLast, ALast: TdxSpreadSheetReferencePath;
begin
  ALast := Self;
  ABeforeLast := nil;
  while ALast.Next <> nil do
  begin
    ABeforeLast := ALast;
    ALast := ALast.Next;
  end;
  if (ALast.Row = ARow) and (ALast.Column = AColumn) and (ALast.Sheet = ASheet)
  then
  begin
    if ABeforeLast <> nil then
      ABeforeLast.FNext := nil;
    ALast.Free;
  end;
end;

function TdxSpreadSheetReferencePath.ToString: string;
begin
  Result := TdxSpreadSheetReferenceAsString.BuildViewReference(Sheet) +
    TdxSpreadSheetColumnReferences.NameByIndex(Column) + IntToStr(Row + 1);
end;

{ TdxSpreadSheetColumnReferences }

class function TdxSpreadSheetColumnReferences.IndexByName(const AName: string;
  R1C1ReferenceStyle: Boolean = False): Integer;
begin
  if not TryIndexByName(AName, Result, R1C1ReferenceStyle) then
    Result := -1;
end;

class function TdxSpreadSheetColumnReferences.TryIndexByName
  (const AName: string; out AIndex: Integer;
  R1C1ReferenceStyle: Boolean): Boolean;
var
  ARow, AColumn: TdxSpreadSheetReference;
begin
  if R1C1ReferenceStyle then
    Result := TryStrToInt(AName, AIndex)
  else
  begin
    Result := dxTryStringToReference(AName, False, ARow, AColumn) and
      ARow.IsAllItems and not AColumn.IsAllItems;
    if Result then
      AIndex := AColumn.Offset;
  end;
end;

class function TdxSpreadSheetColumnReferences.NameByIndex(const AIndex: Integer;
  R1C1ReferenceStyle: Boolean = False): string;
var
  ABuffer: TStringBuilder;
begin
  if R1C1ReferenceStyle then
    Exit(IntToStr(AIndex + 1));

  ABuffer := TdxStringBuilderManager.Get(dxMaxColumnNameLength);
  try
    NameByIndex(ABuffer, AIndex);
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

class procedure TdxSpreadSheetColumnReferences.NameByIndex
  (const ABuffer: TStringBuilder; AIndex: Integer);
begin
  if AIndex >= 26 then
    NameByIndex(ABuffer, AIndex div 26 - 1);
  ABuffer.Append(Char(Byte('A') + AIndex mod 26));
end;

{ TdxSpreadSheetReferenceAsString }

class function TdxSpreadSheetReferenceAsString.Build(const ALink: TObject;
  const ARow, AColumn: TdxSpreadSheetReference;
  const ARowOrigin, AColumnOrigin: Integer;
  R1C1ReferenceStyle: Boolean): string;
begin
  Result := Build(BuildLinkReference(ALink), ARow, AColumn, ARowOrigin,
    AColumnOrigin, R1C1ReferenceStyle);
end;

class function TdxSpreadSheetReferenceAsString.Build(const AReference: string;
  const ARow, AColumn: TdxSpreadSheetReference;
  const ARowOrigin, AColumnOrigin: Integer;
  R1C1ReferenceStyle: Boolean): string;
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get(dxMaxReferenceLength);
  try
    Build(ABuffer, AReference, ARow, AColumn, ARowOrigin, AColumnOrigin,
      R1C1ReferenceStyle);
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

class function TdxSpreadSheetReferenceAsString.Build(ABuffer: TStringBuilder;
  const AReference: string; const ARow, AColumn: TdxSpreadSheetReference;
  const ARowOrigin, AColumnOrigin: Integer;
  R1C1ReferenceStyle: Boolean): string;

  procedure R1C1PartToString(const APartPrefix: string;
    const AReference: TdxSpreadSheetReference; const AOrigin: Integer);
  begin
    if not AReference.IsAllItems then
    begin
      ABuffer.Append(APartPrefix);
      if AReference.IsAbsolute then
        ABuffer.Append(AReference.ActualValue(AOrigin) + 1)
      else if AReference.Offset <> 0 then
      begin
        ABuffer.Append(dxReferenceLeftParenthesis);
        ABuffer.Append(AReference.Offset);
        ABuffer.Append(dxReferenceRightParenthesis);
      end;
    end;
  end;

begin
  ABuffer.Append(AReference);

  if (ARow.ActualValue(ARowOrigin) < 0) or
    (AColumn.ActualValue(AColumnOrigin) < 0) then
  begin
    ABuffer.Append(serRefError);
    Exit;
  end;

  if R1C1ReferenceStyle then
  begin
    R1C1PartToString(dxRCRowReferenceChar, ARow, ARowOrigin);
    R1C1PartToString(dxRCColumnReferenceChar, AColumn, AColumnOrigin);
  end
  else
  begin
    if not AColumn.IsAllItems then
    begin
      if AColumn.IsAbsolute then
        ABuffer.Append(dxAbsoluteReferenceChar);
      TdxSpreadSheetColumnReferences.NameByIndex(ABuffer,
        AColumn.ActualValue(AColumnOrigin));
    end;

    if not ARow.IsAllItems then
    begin
      if ARow.IsAbsolute then
        ABuffer.Append(dxAbsoluteReferenceChar);
      ABuffer.Append(ARow.ActualValue(ARowOrigin) + 1);
    end;
  end;
end;

class function TdxSpreadSheetReferenceAsString.Build(ABuffer: TStringBuilder;
  const AReference: string; const AArea: TRect;
  const ARowOrigin, AColumnOrigin: Integer;
  R1C1ReferenceStyle: Boolean): string;
var
  AColumn1: TdxSpreadSheetReference;
  AColumn2: TdxSpreadSheetReference;
  ARow1: TdxSpreadSheetReference;
  ARow2: TdxSpreadSheetReference;
begin
  ARow1 := TdxSpreadSheetReference.Create(AArea.Top, ARowOrigin,
    dxSpreadSheetMaxRowIndex);
  AColumn1 := TdxSpreadSheetReference.Create(AArea.Left, AColumnOrigin,
    dxSpreadSheetMaxColumnIndex);
  if (AArea.Left = AArea.Right) and (AArea.Top = AArea.Bottom) then
    Build(ABuffer, AReference, ARow1, AColumn1, ARowOrigin, AColumnOrigin,
      R1C1ReferenceStyle)
  else
  begin
    ARow2 := TdxSpreadSheetReference.Create(AArea.Bottom, ARowOrigin,
      dxSpreadSheetMaxRowIndex);
    AColumn2 := TdxSpreadSheetReference.Create(AArea.Right, AColumnOrigin,
      dxSpreadSheetMaxColumnIndex);

    if ARow2.IsAllItems and (ARow1.ActualValue(ARowOrigin) = 0) then
      ARow1.IsAllItems := True;
    if AColumn2.IsAllItems and (AColumn1.ActualValue(AColumnOrigin) = 0) then
      AColumn1.IsAllItems := True;
    if ARow1.IsAllItems and ARow2.IsAllItems and AColumn1.IsAllItems and
      AColumn2.IsAllItems then
    begin
      ARow1.IsAllItems := False;
      ARow2.IsAllItems := False;
      ARow2.SetActualValue(0, dxSpreadSheetMaxRowIndex);
      AColumn1.IsAllItems := False;
      AColumn2.IsAllItems := False;
      AColumn2.SetActualValue(0, dxSpreadSheetMaxColumnIndex);
    end;

    BuildArea(ABuffer, AReference, ARow1, AColumn1, '', ARow2, AColumn2,
      ARowOrigin, AColumnOrigin, R1C1ReferenceStyle);
  end;
end;

class function TdxSpreadSheetReferenceAsString.BuildArea(const ALink1: TObject;
  const ARow1, AColumn1: TdxSpreadSheetReference; const ALink2: TObject;
  const ARow2, AColumn2: TdxSpreadSheetReference;
  const ARowOrigin, AColumnOrigin: Integer;
  R1C1ReferenceStyle: Boolean): string;
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get(dxMaxReferenceLength);
  try
    BuildArea(ABuffer, ALink1, ARow1, AColumn1, ALink2, ARow2, AColumn2,
      ARowOrigin, AColumnOrigin, R1C1ReferenceStyle);
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

class procedure TdxSpreadSheetReferenceAsString.BuildArea
  (ABuffer: TStringBuilder; const ALink1: TObject;
  const ARow1, AColumn1: TdxSpreadSheetReference; const ALink2: TObject;
  const ARow2, AColumn2: TdxSpreadSheetReference;
  const ARowOrigin, AColumnOrigin: Integer; R1C1ReferenceStyle: Boolean);
begin
  BuildArea(ABuffer, BuildLinkReference(ALink1), ARow1, AColumn1,
    BuildLinkReference(ALink2), ARow2, AColumn2, ARowOrigin, AColumnOrigin,
    R1C1ReferenceStyle);
end;

class procedure TdxSpreadSheetReferenceAsString.BuildArea
  (ABuffer: TStringBuilder; const AReference1: string;
  const ARow1, AColumn1: TdxSpreadSheetReference; const AReference2: string;
  const ARow2, AColumn2: TdxSpreadSheetReference;
  const ARowOrigin, AColumnOrigin: Integer; R1C1ReferenceStyle: Boolean);
begin
  if R1C1ReferenceStyle then
  begin
    if (ARow1 = ARow2) and (AColumn1 = AColumn2) and
      ((AReference2 = '') or (AReference1 = AReference2)) then
    begin
      Build(ABuffer, AReference1, ARow1, AColumn1, ARowOrigin, AColumnOrigin,
        R1C1ReferenceStyle);
      Exit;
    end;
  end;

  Build(ABuffer, AReference1, ARow1, AColumn1, ARowOrigin, AColumnOrigin,
    R1C1ReferenceStyle);
  ABuffer.Append(dxAreaSeparator);
  Build(ABuffer, AReference2, ARow2, AColumn2, ARowOrigin, AColumnOrigin,
    R1C1ReferenceStyle);
end;

class function TdxSpreadSheetReferenceAsString.BuildLinkReference
  (const ALink: TObject): string;
begin
  if ALink <> nil then
    Result := ALink.ToString
  else
    Result := '';
end;

class function TdxSpreadSheetReferenceAsString.BuildViewReference
  (const AView: TObject): string;
var
  AIntf: IdxSpreadSheetViewCaption;
begin
  if Supports(AView, IdxSpreadSheetViewCaption, AIntf) then
    Result := BuildViewReference(AIntf.GetCaption,
      dxBooleanToDefaultBoolean(AIntf.IsCaptionMustBeEscaped))
  else
    Result := '';
end;

class function TdxSpreadSheetReferenceAsString.BuildViewReference
  (const AViewName: string; AEscape: TdxDefaultBoolean = bDefault): string;
var
  ABuffer: TStringBuilder;
begin
  if AViewName <> '' then
  begin
    ABuffer := TdxStringBuilderManager.Get(Length(AViewName) + 3);
    try
      if dxDefaultBooleanToBoolean(AEscape, True) then
      begin
        ABuffer.Append(dxStringMarkChar2);
        ABuffer.Append(AViewName);
        ABuffer.Append(dxStringMarkChar2);
      end
      else
        ABuffer.Append(AViewName);

      ABuffer.Append(dxRefSeparator);
      Result := ABuffer.ToString;
    finally
      TdxStringBuilderManager.Release(ABuffer);
    end;
  end
  else
    Result := '';
end;

class function TdxSpreadSheetReferenceAsString.CheckIfViewNameMustBeEscaped
  (const AName, ABreakChars: string): Boolean;
var
  L: Integer;
  I: Integer;
begin
  L := Length(AName);
  if L > 0 then
  begin
    if not dxCharIsAlpha(AName[1]) then
      Exit(True);
    for I := 1 to L do
    begin
      if Pos(AName[I], ABreakChars) > 0 then
        Exit(True);
    end;
  end;
  Result := False;
end;

end.
