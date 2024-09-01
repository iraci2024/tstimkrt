{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library classes }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
{ ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE }
{ PROGRAM ONLY. }
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

unit dxTextViewInfoCache; // for internal use

interface

{$I cxVer.inc}
{$UNDEF DXLOGGING}

uses
  Types, Windows, dxCore, dxCoreClasses, dxFontHelpers;

type
  PdxIntegerArray = ^TdxIntegerArray;
  TdxIntegerArray = array [0 .. 0] of Integer;

  PdxWordArray = ^TdxWordArray;
  TdxWordArray = array [0 .. 0] of Word;

  IntPtr = PINT;

  { TdxTextViewInfo }

  TdxTextViewInfo = class
  strict private
    FCharacterWidths: PdxIntegerArray;
    FGlyphCount: Integer;
    FGlyphs: PdxWordArray;
    FSize: TSize;
  public
    destructor Destroy; override;

    property CharacterWidths: PdxIntegerArray read FCharacterWidths
      write FCharacterWidths;
    property Glyphs: PdxWordArray read FGlyphs write FGlyphs;
    property GlyphCount: Integer read FGlyphCount write FGlyphCount;
    property Size: TSize read FSize write FSize;
  end;

  { TdxTextViewInfoCache }

  TdxTextViewInfoCache = class
{$REGION 'internal types'}
  strict protected
  type
    TItem = class
    strict private
      FHash: Cardinal;
      FText: string;
      FFontInfo: TdxFontInfo;
      FTextViewInfo: TdxTextViewInfo;
      FNext: TItem;
    public
      constructor Create(const AText: string; AFontInfo: TdxFontInfo;
        ATextInfo: TdxTextViewInfo);
      destructor Destroy; override;
      function Equals(AEntry: TItem): Boolean; reintroduce; overload;
      function Equals(AHash: Cardinal; const AText: string;
        AFontInfo: TdxFontInfo): Boolean; reintroduce; overload;
      class function CalcHash(const AText: string; AFontInfo: TdxFontInfo)
        : Cardinal; static;

      property Hash: Cardinal read FHash;
      property Text: string read FText;
      property FontInfo: TdxFontInfo read FFontInfo;
      property TextViewInfo: TdxTextViewInfo read FTextViewInfo;
      property Next: TItem read FNext write FNext;
    end;

    TItemArray = array of TItem;
{$ENDREGION}
  strict private
    FCount: Integer;
    FTable: TItemArray;
    FTableSize: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure AddTextViewInfo(const AText: string; AFontInfo: TdxFontInfo;
      ATextInfo: TdxTextViewInfo);
    function TryGetTextViewInfo(const AText: string; AFontInfo: TdxFontInfo)
      : TdxTextViewInfo;

    property Count: Integer read FCount;
  end;

  { TdxCustomBoxMeasurer }

  TdxCustomBoxMeasurer = class(TcxIUnknownObject)
  strict private
    FCaretPosBufferSize: Integer;
    FCaretPosBuffer: Pointer;
    FTextViewInfoCache: TdxTextViewInfoCache;
  strict protected
    procedure AdjustCharacterBoundsForLigature(const ACharacterBounds
      : TArray<TRect>; AFrom, ATo: Integer);
    procedure EstimateCaretPositionsForLigatures(const ACharacterBounds
      : TArray<TRect>);
    function CalculateCharactersBounds(ACaret: PINT; ALength: Integer;
      const ABounds: TRect): TArray<TRect>;
    function CreateTextViewInfoCore(ADC: HDC; const AText: string;
      AFontInfo: TdxFontInfo): TdxTextViewInfo; virtual;
    function MeasureCharactersBoundsSlow(ADC: HDC; const AText: string;
      var AGcpResults: TGCPResultsW): Integer;
    function MeasureWithGetCharacterPlacementSlow(ADC: HDC; const AText: string;
      var AGcpResults: TGCPResultsW): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function FindLengthToFit(ADC: HDC; const AText: string;
      AFontInfo: TdxFontInfo; AMaxWidth: Integer;
      ARestoreFont: Boolean = False): Integer;
    function CreateTextViewInfo(ADC: HDC; const AText: string;
      AFontInfo: TdxFontInfo): TdxTextViewInfo; overload; virtual; abstract;
    function CreateTextViewInfo(const AText: string; AFontInfo: TdxFontInfo)
      : TdxTextViewInfo; overload; virtual; abstract;
    function GetCaretPosBuffer(AItemsCount: Integer): Pointer;
    function MeasureText(const AText: string; AFontInfo: TdxFontInfo): TSize;
      overload; virtual;
    function MeasureText(ADC: HDC; const AText: string; AFontInfo: TdxFontInfo)
      : TSize; overload; virtual;
    function MeasureCharactersBounds(ADC: HDC; const AText: string;
      AFontInfo: TdxFontInfo; const ABounds: TRect): TArray<TRect>;
      overload; virtual;

    property TextViewInfoCache: TdxTextViewInfoCache read FTextViewInfoCache;
  end;

implementation

uses
  RTLConsts, SysUtils, Math, Classes, dxTypeHelpers, dxHash;

{ TdxTextViewInfo }

destructor TdxTextViewInfo.Destroy;
begin
  FreeMem(FGlyphs);
  FreeMem(FCharacterWidths);
  inherited Destroy;
end;

{ TdxTextViewInfoCache.TItem }

constructor TdxTextViewInfoCache.TItem.Create(const AText: string;
  AFontInfo: TdxFontInfo; ATextInfo: TdxTextViewInfo);
begin
  inherited Create;
  FHash := CalcHash(AText, AFontInfo);
  FText := AText;
  FFontInfo := AFontInfo;
  FTextViewInfo := ATextInfo;
end;

destructor TdxTextViewInfoCache.TItem.Destroy;
begin
  FTextViewInfo.Free;
  inherited Destroy;
end;

const
  DefTableSize = 33703;

class function TdxTextViewInfoCache.TItem.CalcHash(const AText: string;
  AFontInfo: TdxFontInfo): Cardinal;
var
  C: NativeUInt;
begin
  C := NativeUInt(AFontInfo) shr 3;
  C := ((C shl 5) + C) and $FFFFFFFF;
  Result := TdxStringHash.Murmur2(AText) xor C;
end;

function TdxTextViewInfoCache.TItem.Equals(AEntry: TItem): Boolean;
begin
  Result := (Hash = AEntry.Hash) and (FontInfo = AEntry.FontInfo) and
    (Text = AEntry.Text);
end;

function TdxTextViewInfoCache.TItem.Equals(AHash: Cardinal; const AText: string;
  AFontInfo: TdxFontInfo): Boolean;
begin
  Result := (Hash = AHash) and (FontInfo = AFontInfo) and (Text = AText);
end;

{ TdxTextViewInfoCache }

constructor TdxTextViewInfoCache.Create;
begin
  inherited Create;
  FTableSize := DefTableSize;
  SetLength(FTable, FTableSize);
end;

destructor TdxTextViewInfoCache.Destroy;
begin
  Clear;
  FTable := nil;
  inherited Destroy;
end;

procedure TdxTextViewInfoCache.AddTextViewInfo(const AText: string;
  AFontInfo: TdxFontInfo; ATextInfo: TdxTextViewInfo);
var
  AEntry, ATestEntry: TItem;
  AIndex: Cardinal;
begin
  AEntry := TItem.Create(AText, AFontInfo, ATextInfo);
  AIndex := AEntry.Hash mod FTableSize;
  ATestEntry := FTable[AIndex];
  if ATestEntry = nil then
  begin
    FTable[AIndex] := AEntry;
    Inc(FCount);
    Exit;
  end;

  repeat
    if ATestEntry.Next = nil then
    begin
      ATestEntry.Next := AEntry;
      Inc(FCount);
      Break;
    end;
    ATestEntry := ATestEntry.Next;
  until False;
end;

function TdxTextViewInfoCache.TryGetTextViewInfo(const AText: string;
  AFontInfo: TdxFontInfo): TdxTextViewInfo;
var
  AEntry: TItem;
  AIndex: Integer;
  AHash: Cardinal;
begin
  AHash := TItem.CalcHash(AText, AFontInfo);
  AIndex := AHash mod FTableSize;
  AEntry := FTable[AIndex];
  if AEntry = nil then
    Exit(nil);

  repeat
    if AEntry.Equals(AHash, AText, AFontInfo) then
      Exit(AEntry.TextViewInfo);

    AEntry := AEntry.Next;
    if AEntry = nil then
      Exit(nil);
  until False;
end;

procedure TdxTextViewInfoCache.Clear;
var
  I: Integer;
  AEntry, ATemp: TItem;
begin
  for I := 0 to FTableSize - 1 do
  begin
    AEntry := FTable[I];;
    while AEntry <> nil do
    begin
      ATemp := AEntry;
      AEntry := ATemp.Next;
      ATemp.Free;
    end;
    FTable[I] := nil;
  end;
  FCount := 0;
end;

{ TdxCustomBoxMeasurer }

constructor TdxCustomBoxMeasurer.Create;
const
  dxInitialBufferItemCount = 64;
begin
  inherited Create;
  GetCaretPosBuffer(dxInitialBufferItemCount);
  FTextViewInfoCache := TdxTextViewInfoCache.Create;
end;

destructor TdxCustomBoxMeasurer.Destroy;
begin
  FreeAndNil(FTextViewInfoCache);
  FreeMem(FCaretPosBuffer, FCaretPosBufferSize);
  inherited Destroy;
end;

function TdxCustomBoxMeasurer.MeasureText(const AText: string;
  AFontInfo: TdxFontInfo): TSize;
var
  ATextViewInfo: TdxTextViewInfo;
begin
  ATextViewInfo := TextViewInfoCache.TryGetTextViewInfo(AText, AFontInfo);
  if ATextViewInfo = nil then
  begin
    ATextViewInfo := CreateTextViewInfo(AText, AFontInfo);
    TextViewInfoCache.AddTextViewInfo(AText, AFontInfo, ATextViewInfo);
  end;
  Result := ATextViewInfo.Size;
end;

function TdxCustomBoxMeasurer.MeasureText(ADC: HDC; const AText: string;
  AFontInfo: TdxFontInfo): TSize;
var
  ATextViewInfo: TdxTextViewInfo;
begin
  ATextViewInfo := TextViewInfoCache.TryGetTextViewInfo(AText, AFontInfo);
  if ATextViewInfo = nil then
  begin
    ATextViewInfo := CreateTextViewInfo(ADC, AText, AFontInfo);
    TextViewInfoCache.AddTextViewInfo(AText, AFontInfo, ATextViewInfo);
  end;
  Result := ATextViewInfo.Size;
end;

function TdxCustomBoxMeasurer.MeasureCharactersBounds(ADC: HDC;
  const AText: string; AFontInfo: TdxFontInfo; const ABounds: TRect)
  : TArray<TRect>;
var
  AGcpResults: TGCPResultsW;
  ASize: Cardinal;
  ALength: Integer;
  AOldFont: THandle;
begin
  ALength := Length(AText);
  AOldFont := SelectObject(ADC, AFontInfo.GdiFontHandle);
  try
    AGcpResults.lStructSize := SizeOf(GCP_RESULTS);
    AGcpResults.lpOutString := nil;
    AGcpResults.lpOrder := nil;
    AGcpResults.lpDx := nil;
    AGcpResults.lpCaretPos := GetCaretPosBuffer(ALength);
    AGcpResults.lpClass := nil;
    AGcpResults.lpGlyphs := nil;
    AGcpResults.nGlyphs := ALength;
    AGcpResults.nMaxFit := 0;
    ASize := GetCharacterPlacement(ADC, PChar(AText), ALength, 0, AGcpResults,
      GCP_USEKERNING or GCP_LIGATE);
    if (ASize = 0) and (ALength > 0) then
      MeasureCharactersBoundsSlow(ADC, AText, AGcpResults);
    Result := CalculateCharactersBounds(AGcpResults.lpCaretPos,
      ALength, ABounds);
  finally
    SelectObject(ADC, AOldFont);
  end;
end;

function TdxCustomBoxMeasurer.MeasureCharactersBoundsSlow(ADC: HDC;
  const AText: string; var AGcpResults: TGCPResultsW): Integer;
var
  I, AAdd, AStep, ALength, AExtraLength: Integer;
begin
  ALength := Length(AText);
  AStep := (ALength + 1) div 2;
  AAdd := AStep;
  for I := 0 to 2 do
  begin
    AExtraLength := ALength + AAdd;
    AGcpResults.nGlyphs := AExtraLength;
    AGcpResults.lpCaretPos := GetCaretPosBuffer(AExtraLength);
    Result := GetCharacterPlacement(ADC, PChar(AText), ALength, 0, AGcpResults,
      GCP_USEKERNING or GCP_LIGATE);
    if Result <> 0 then
      Exit;
    Inc(AAdd, AStep);
  end;
end;

procedure TdxCustomBoxMeasurer.AdjustCharacterBoundsForLigature
  (const ACharacterBounds: TArray<TRect>; AFrom, ATo: Integer);
var
  I, ACount: Integer;
  ABounds: TArray<TRect>;
begin
  ACount := ATo - AFrom + 1;
  ABounds := ACharacterBounds[ATo].SplitHorizontally(ACount);
  for I := 0 to ACount - 1 do
    ACharacterBounds[I + AFrom] := ABounds[I];
end;

function TdxCustomBoxMeasurer.CalculateCharactersBounds(ACaret: PINT;
  ALength: Integer; const ABounds: TRect): TArray<TRect>;
var
  I, APrevPos, ANextPos: Integer;
  ANeedProcessLigatures: Boolean;
begin
  ANeedProcessLigatures := False;
  SetLength(Result, ALength);
  APrevPos := ACaret^;
  for I := 0 to ALength - 2 do
  begin
    Inc(ACaret);
    ANextPos := ACaret^;
    Result[I].InitSize(ABounds.Left + APrevPos, ABounds.Top,
      ANextPos - APrevPos, ABounds.Height);
    ANeedProcessLigatures := (APrevPos = ANextPos) or ANeedProcessLigatures;
    APrevPos := ANextPos;
  end;
  if ALength > 0 then
    Result[ALength - 1].InitSize(ABounds.Left + APrevPos, ABounds.Top,
      ABounds.Width - APrevPos, ABounds.Height);
  if ANeedProcessLigatures then
    EstimateCaretPositionsForLigatures(Result);
end;

function TdxCustomBoxMeasurer.CreateTextViewInfoCore(ADC: HDC;
  const AText: string; AFontInfo: TdxFontInfo): TdxTextViewInfo;
var
  AGcpResults: TGCPResultsW;
  ASize: Cardinal;
  ALength, AWidth, AHeight, ACaretBasedWidth: Integer;
begin
  ALength := Length(AText);
  ZeroMemory(@AGcpResults, SizeOf(TGCPResults));
  AGcpResults.lStructSize := SizeOf(TGCPResults);

  AGcpResults.lpCaretPos := GetCaretPosBuffer(ALength);
  GetMem(AGcpResults.lpDx, SizeOf(Integer) * ALength);

  if not AFontInfo.UseGetGlyphIndices then
  begin
    GetMem(AGcpResults.lpGlyphs, SizeOf(Word) * ALength);
    if ALength > 0 then
      PWord(AGcpResults.lpGlyphs)^ := 0;
    AGcpResults.nGlyphs := ALength;
  end;

  ASize := GetCharacterPlacement(ADC, PChar(AText), ALength, 0, AGcpResults,
    GCP_USEKERNING or GCP_LIGATE);
  if (ASize = 0) and (ALength > 0) then
    ASize := MeasureWithGetCharacterPlacementSlow(ADC, AText, AGcpResults);
  AWidth := LongRec(ASize).Lo;
  AHeight := LongRec(ASize).Hi;
  if Length(AText) > 0 then
  begin
    ACaretBasedWidth := PdxIntegerArray(AGcpResults.lpCaretPos)[ALength - 1];
    if ACaretBasedWidth > $FFFF then
      AWidth := ACaretBasedWidth + PdxIntegerArray(AGcpResults.lpDx)
        [ALength - 1];
  end;

  if AFontInfo.UseGetGlyphIndices then
  begin
    if AGcpResults.lpGlyphs = nil then
      GetMem(AGcpResults.lpGlyphs, SizeOf(Word) * ALength);
    GetGlyphIndices(ADC, PChar(AText), ALength,
      Pointer(AGcpResults.lpGlyphs), 0);
  end;

  Result := TdxTextViewInfo.Create;
  Result.Size := TSize.Create(AWidth, AHeight);
  Result.Glyphs := Pointer(AGcpResults.lpGlyphs);
  Result.GlyphCount := AGcpResults.nGlyphs;
  Result.CharacterWidths := Pointer(AGcpResults.lpDx);
end;

function TdxCustomBoxMeasurer.MeasureWithGetCharacterPlacementSlow(ADC: HDC;
  const AText: string; var AGcpResults: TGCPResultsW): Integer;
var
  I, AAdd, AStep, ALength, AExtraLength: Integer;
begin
  ALength := Length(AText);
  AStep := (ALength + 1) div 2;
  AAdd := AStep;
  for I := 0 to 2 do
  begin
    AExtraLength := ALength + AAdd;
    AGcpResults.nGlyphs := AExtraLength;
    ReallocMem(AGcpResults.lpDx, SizeOf(Integer) * AExtraLength);
    ReallocMem(AGcpResults.lpGlyphs, SizeOf(Word) * AExtraLength);
    if ALength > 0 then
      PWord(AGcpResults.lpGlyphs)^ := 0;
    Result := GetCharacterPlacement(ADC, PChar(AText), ALength, 0, AGcpResults,
      GCP_USEKERNING or GCP_LIGATE);
    if Result <> 0 then
      Exit;
    Inc(AAdd, AStep);
  end;
end;

function TdxCustomBoxMeasurer.GetCaretPosBuffer(AItemsCount: Integer): Pointer;
var
  ASize: Integer;
begin
  ASize := SizeOf(Integer) * AItemsCount;
  if ASize > FCaretPosBufferSize then
  begin
    ReallocMem(FCaretPosBuffer, ASize);
    FCaretPosBufferSize := ASize;
  end;
  Result := FCaretPosBuffer;
end;

procedure TdxCustomBoxMeasurer.EstimateCaretPositionsForLigatures
  (const ACharacterBounds: TArray<TRect>);
var
  ABounds: TRect;
  I, AFrom, ACount: Integer;
begin
  ACount := Length(ACharacterBounds);
  AFrom := MaxInt;
  for I := 0 to ACount - 1 do
  begin
    ABounds := ACharacterBounds[I];
    if (ABounds.Right - ABounds.Left) = 0 then
    begin
      if AFrom = MaxInt then
        AFrom := I;
    end
    else if AFrom < I then
    begin
      AdjustCharacterBoundsForLigature(ACharacterBounds, AFrom, I);
      AFrom := MaxInt;
    end;
  end;
end;

function TdxCustomBoxMeasurer.FindLengthToFit(ADC: HDC; const AText: string;
  AFontInfo: TdxFontInfo; AMaxWidth: Integer;
  ARestoreFont: Boolean = False): Integer;
var
  AGcpResults: TGCPResults;
  AOldFont: HFONT;
begin
  AOldFont := SelectObject(ADC, AFontInfo.GdiFontHandle);
  ZeroMemory(@AGcpResults, SizeOf(AGcpResults));
  AGcpResults.lStructSize := SizeOf(AGcpResults);
  AGcpResults.nGlyphs := Length(AText) * 4;
  GetCharacterPlacement(ADC, PChar(AText), Length(AText), AMaxWidth,
    AGcpResults, GCP_USEKERNING or GCP_LIGATE or GCP_MAXEXTENT);
  Result := AGcpResults.nMaxFit;
  if ARestoreFont then
    SelectObject(ADC, AOldFont);
end;

end.
