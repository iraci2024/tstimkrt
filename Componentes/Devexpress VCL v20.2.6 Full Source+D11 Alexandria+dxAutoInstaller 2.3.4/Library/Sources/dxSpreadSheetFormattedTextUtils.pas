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

unit dxSpreadSheetFormattedTextUtils;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, Math, Classes, Types, SysUtils, Graphics,
  Generics.Defaults, Generics.Collections,
  StrUtils, Variants,
  // CX
  cxClasses, dxCore, cxGraphics, dxCoreClasses, cxFormats, cxControls,
  cxGeometry, cxLookAndFeelPainters,
  dxGDIPlusClasses, cxDrawTextUtils, dxStringHelper,
  // FormatedTextUtils
  dxFormattedText, dxFormattedTextConverterRTF,
  // SpreadSheet
  dxSpreadSheetCore, dxSpreadSheetCoreStyles, dxSpreadSheetTypes,
  dxSpreadSheetClasses, dxSpreadSheetUtils,
  dxSpreadSheetStrs, dxSpreadSheetGraphics, dxSpreadSheetNumberFormat,
  dxSpreadSheetPrinting, dxSpreadSheetHyperlinks,
  dxSpreadSheetProtection;

type

  { TdxSpreadSheetFormattedText }

  TdxSpreadSheetFormattedText = class
  public
    class function CreateFontHandle(AFontTable: TdxSpreadSheetFonts;
      const AStyle: TdxFormattedTextCharacterProperties)
      : TdxSpreadSheetFontHandle;
    class function FromFormattedText(AText: TdxFormattedText;
      ADefaultFont: TdxSpreadSheetFontHandle; AFontTable: TdxSpreadSheetFonts)
      : TdxSpreadSheetFormattedSharedString; overload;
    class function FromFormattedText(AText: TdxFormattedText;
      ADefaultFont: TFont; AFontTable: TdxSpreadSheetFonts)
      : TdxSpreadSheetFormattedSharedString; overload;
    class function FromRtf(const ARtfString: string; ADefaultFont: TFont;
      AFontTable: TdxSpreadSheetFonts): TdxSpreadSheetFormattedSharedString;
    class procedure ToFormattedText(AText: TdxFormattedText;
      AString: TdxSpreadSheetFormattedSharedString;
      ADefaultFont: TdxSpreadSheetFontHandle);
  end;

  { TdxSpreadSheetFormattedTextService }

  TdxSpreadSheetFormattedTextService = class(TdxSpreadSheetTextService)
  strict private
    class function CalculateTextSize(ACanvas: TcxCanvas;
      ACell: TdxSpreadSheetCell; ADisplayWidth: Integer): TSize;
    class function CreateFormattedText: TdxFormattedText;
    class function GetCache(ACell: TdxSpreadSheetCell)
      : TdxSpreadSheetFormattedSharedStringCache;
    class function GetFontClone(ACell: TdxSpreadSheetCell): TFont;
    class function GetFontTable(ACell: TdxSpreadSheetCell): TdxSpreadSheetFonts;
    class function GetFormattedText(ACell: TdxSpreadSheetCell)
      : TdxFormattedText; overload;
    class function GetFormattedText(ACell: TdxSpreadSheetCell;
      ACanvas: TcxCanvas; const ASize: TSize; AFlags: Cardinal)
      : TdxFormattedText; overload;
    class function HasFormatting(AString
      : TdxSpreadSheetFormattedSharedString): Boolean;
    class function IsRTFSignature(const S: string): Boolean; inline;
  protected
    class procedure CalculateTextBounds(ACanvas: TcxCanvas;
      ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo;
      var ATextBounds: TRect); override;
    class procedure DrawValue(ACanvas: TcxCanvas;
      ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo;
      ABounds: TRect); override;
    class procedure ApplyDefaultStyle(ACell: TdxSpreadSheetCell;
      const AEditValue: string); override;
  public
    class procedure CalculateSize(ACell: TdxSpreadSheetCell; ACanvas: TcxCanvas;
      const ABounds: TRect; AIsMerged: Boolean;
      AWidth, AHeight: PInteger); override;
    class function IsFormattedEditValue(const AEditValue: string): Boolean;
      override;
    class function IsFormattedTextValue(ACell: TdxSpreadSheetCell): Boolean;
      override;
    class function IsRTFSupported: Boolean; override;
    class function ForceSetAsRTF(ACell: TdxSpreadSheetCell;
      const AEditValue: string): Boolean; override;
    class function GetAsRTF(ACell: TdxSpreadSheetCell; var AValue: string;
      ARichEditCompatibility: Boolean): Boolean; override;
    class function SetAsRTF(ACell: TdxSpreadSheetCell; const AEditValue: string)
      : Boolean; override;
    class function SetAsRTFValue(ACell: TdxSpreadSheetCell;
      const AEditValue: string; AForce: Boolean): Boolean; // for internal use
  end;

implementation

uses
  dxDPIAwareUtils, dxSpreadSheetStyles, dxCoreGraphics, dxDrawRichTextUtils;

type
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxFormattedTextAccess = class(TdxFormattedText);
  TdxFormattedTextRunAccess = class(TdxFormattedTextRun);
  TdxSpreadSheetCellFontAccess = class(TdxSpreadSheetCellFont);

  { TdxSpreadSheetFormattedText }

class function TdxSpreadSheetFormattedText.CreateFontHandle
  (AFontTable: TdxSpreadSheetFonts;
  const AStyle: TdxFormattedTextCharacterProperties): TdxSpreadSheetFontHandle;
begin
  Result := AFontTable.CreateFont;
  Result.Color := AStyle.FontColor;
  Result.Name := AStyle.FontName;
  Result.Size := AStyle.FontSize;
  Result.Style := AStyle.FontStyle;
  Result.Script := TdxSpreadSheetFontScript(AStyle.CharacterFormattingScript);
  Result := AFontTable.AddFont(Result);
end;

class procedure TdxSpreadSheetFormattedText.ToFormattedText
  (AText: TdxFormattedText; AString: TdxSpreadSheetFormattedSharedString;
  ADefaultFont: TdxSpreadSheetFontHandle);

  procedure AppendRun(ARun: TdxFormattedTextRun; APosition: Integer);
  var
    ARunPrev: TdxFormattedTextRunAccess;
  begin
    TdxFormattedTextRunAccess(ARun).FTextStart := PChar(AText.Text) +
      APosition - 1;
    if AText.Runs.Count > 0 then
    begin
      ARunPrev := TdxFormattedTextRunAccess(AText.Runs.Last);
      ARunPrev.FTextLength := dxGetStringLength(ARunPrev.FTextStart,
        TdxFormattedTextRunAccess(ARun).FTextStart);
    end;
    AText.Runs.Add(ARun);
  end;

  procedure AppendFont(AFont, ANextFont: TdxSpreadSheetFontHandle;
    APosition: Integer);
  const
    FontScriptToRunClass: array [TdxSpreadSheetFontScript]
      of TdxFormattedTextRunClass = (nil, TdxFormattedTextSupRun,
      TdxFormattedTextSubRun);
    FontStyleToRunClass: array [TFontStyle] of TdxFormattedTextRunClass =
      (TdxFormattedTextBoldRun, TdxFormattedTextItalicRun,
      TdxFormattedTextUnderlineRun, TdxFormattedTextStrikeoutRun);
  var
    AFontStyle: TFontStyle;
  begin
    if AFont.Name <> ANextFont.Name then
    begin
      if AFont.Name <> ADefaultFont.Name then
        AppendRun(TdxFormattedTextFontRun.Create(traClose, ''), APosition);
      if ANextFont.Name <> ADefaultFont.Name then
        AppendRun(TdxFormattedTextFontRun.Create(traOpen, ANextFont.Name),
          APosition);
    end;

    if AFont.Size <> ANextFont.Size then
    begin
      if AFont.Size <> ADefaultFont.Size then
        AppendRun(TdxFormattedTextSizeRun.Create(traClose, 0), APosition);
      if ANextFont.Size <> ADefaultFont.Size then
        AppendRun(TdxFormattedTextSizeRun.Create(traOpen, ANextFont.Size),
          APosition);
    end;

    if AFont.Script <> ANextFont.Script then
    begin
      if (AFont.Script <> ADefaultFont.Script) and (AFont.Script <> fsNone) then
        AppendRun(FontScriptToRunClass[AFont.Script].Create(traClose),
          APosition);
      if (ANextFont.Script <> ADefaultFont.Script) and
        (ANextFont.Script <> fsNone) then
        AppendRun(FontScriptToRunClass[ANextFont.Script].Create(traOpen),
          APosition);
    end;

    if AFont.Color <> ANextFont.Color then
    begin
      if AFont.Color <> ADefaultFont.Color then
        AppendRun(TdxFormattedTextColorRun.Create(traClose, 0), APosition);
      if ANextFont.Color <> ADefaultFont.Color then
        AppendRun(TdxFormattedTextColorRun.Create(traOpen, ANextFont.Color),
          APosition);
    end;

    if AFont.Style <> ANextFont.Style then
      for AFontStyle := Low(AFontStyle) to High(AFontStyle) do
      begin
        if (AFontStyle in AFont.Style) <> (AFontStyle in ANextFont.Style) then
        begin
          if AFontStyle in AFont.Style then
            AppendRun(FontStyleToRunClass[AFontStyle].Create(traClose),
              APosition);
          if AFontStyle in ANextFont.Style then
            AppendRun(FontStyleToRunClass[AFontStyle].Create(traOpen),
              APosition);
        end;
      end;
  end;

var
  AFontHandle: TdxSpreadSheetFontHandle;
  APrevFont: TdxSpreadSheetFontHandle;
  ARun: TdxSpreadSheetFormattedSharedStringRun;
  I: Integer;
begin
  AText.Text := AString.Value;
  if AString.Runs.Count > 0 then
  begin
    APrevFont := ADefaultFont;
    AppendRun(TdxFormattedTextNoCodeRun.Create(traOpen), 1);
    for I := 0 to AString.Runs.Count - 1 do
    begin
      ARun := AString.Runs.Items[I];
      AFontHandle := ARun.FontHandle;
      if AFontHandle = nil then
        AFontHandle := ADefaultFont;
      AppendFont(APrevFont, AFontHandle, ARun.StartIndex);
      APrevFont := AFontHandle;
    end;
    AppendFont(APrevFont, ADefaultFont, Length(AText.Text) + 1);
    AppendRun(TdxFormattedTextNoCodeRun.Create(traClose),
      Length(AText.Text) + 1);
    TdxFormattedTextAccess(AText).CreateInternalRuns
      (ADefaultFont.GraphicObject);
  end;
end;

class function TdxSpreadSheetFormattedText.FromFormattedText
  (AText: TdxFormattedText; ADefaultFont: TdxSpreadSheetFontHandle;
  AFontTable: TdxSpreadSheetFonts): TdxSpreadSheetFormattedSharedString;
begin
  Result := FromFormattedText(AText, ADefaultFont.GraphicObject, AFontTable);
end;

class function TdxSpreadSheetFormattedText.FromFormattedText
  (AText: TdxFormattedText; ADefaultFont: TFont;
  AFontTable: TdxSpreadSheetFonts): TdxSpreadSheetFormattedSharedString;

  procedure AddRun(APosition: Integer;
    AString: TdxSpreadSheetFormattedSharedString;
    const AStyle: TdxFormattedTextCharacterProperties);
  var
    ARun: TdxSpreadSheetFormattedSharedStringRun;
  begin
    if AString.Runs.Count > 0 then
    begin
      ARun := TdxSpreadSheetFormattedSharedStringRun(AString.Runs.Last);
      if ARun.StartIndex = APosition then
      begin
        ARun.FontHandle := CreateFontHandle(AFontTable, AStyle);
        Exit;
      end;
    end;
    if APosition <= Length(AString.Value) then
      AString.Runs.Add(APosition, CreateFontHandle(AFontTable, AStyle));
  end;

var
  APosition: Integer;
  ARun: TdxFormattedTextRun;
  AStack: TdxFormattedTextRunStack;
  AStyle: TdxFormattedTextCharacterProperties;
  I: Integer;
begin
  Result := TdxSpreadSheetFormattedSharedString.CreateObject
    (AText.GetDisplayText);

  AStack := TdxFormattedTextRunStack.Create;
  try
    APosition := 1;
    for I := 0 to AText.Runs.Count - 1 do
    begin
      ARun := AText.Runs[I];
      AStyle.Initialize(ADefaultFont);
      AStack.ProcessRun(ARun);
      AStack.CalculateStyle(AStyle);
      AddRun(APosition, Result, AStyle);
      Inc(APosition, ARun.TextLength);
    end;
  finally
    AStack.Free;
  end;
end;

class function TdxSpreadSheetFormattedText.FromRtf(const ARtfString: string;
  ADefaultFont: TFont; AFontTable: TdxSpreadSheetFonts)
  : TdxSpreadSheetFormattedSharedString;
var
  AFormattedText: TdxFormattedText;
begin
  AFormattedText := TdxFormattedText.Create;
  try
    TdxFormattedTextConverterRTF.Import(AFormattedText, ARtfString,
      ADefaultFont, True);
    Result := FromFormattedText(AFormattedText, ADefaultFont, AFontTable);
  finally
    AFormattedText.Free;
  end;
end;

{ TdxSpreadSheetFormattedTextService }

class procedure TdxSpreadSheetFormattedTextService.CalculateSize
  (ACell: TdxSpreadSheetCell; ACanvas: TcxCanvas; const ABounds: TRect;
  AIsMerged: Boolean; AWidth, AHeight: PInteger);
var
  ASize: TSize;
  AValueWidth: Integer;
begin
  if IsFormattedTextValue(ACell) then
  begin
    if (AHeight <> nil) and (AWidth = nil) then
      AValueWidth := cxRectWidth(ABounds)
    else
      AValueWidth := MaxInt;

    ASize := CalculateTextSize(ACanvas, ACell, AValueWidth);
    if AWidth <> nil then
      AWidth^ := ASize.cx;
    if AHeight <> nil then
      AHeight^ := ASize.cy;
  end
  else
    inherited;
end;

class procedure TdxSpreadSheetFormattedTextService.ApplyDefaultStyle
  (ACell: TdxSpreadSheetCell; const AEditValue: string);
var
  ADefaultFont: TFont;
  AFormattedSharedString: TdxSpreadSheetFormattedSharedString;
begin
  if IsRTFSignature(AEditValue) then
  begin
    ADefaultFont := GetFontClone(ACell);
    try
      AFormattedSharedString := TdxSpreadSheetFormattedText.FromRtf(AEditValue,
        ADefaultFont, GetFontTable(ACell));
      try
        if AFormattedSharedString.Runs.Count > 0 then
          TdxSpreadSheetCellFontAccess(ACell.Style.Font).Handle :=
            AFormattedSharedString.Runs[0].FontHandle;
      finally
        AFormattedSharedString.Free;
      end;
    finally
      ADefaultFont.Free;
    end;
  end;
end;

class procedure TdxSpreadSheetFormattedTextService.CalculateTextBounds
  (ACanvas: TcxCanvas; ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo;
  var ATextBounds: TRect);
var
  ASize: TSize;
begin
  if IsFormattedTextValue(ACell.Cell) then
  begin
    ASize := CalculateTextSize(ACanvas, ACell.Cell, cxRectWidth(ATextBounds));
    if ACell.AlignVert <> taTop then
    begin
      if ACell.AlignVert = taBottom then
        OffsetRect(ATextBounds, 0, (cxRectHeight(ATextBounds) - ASize.cy));
      if ACell.AlignVert = taCenterY then
        OffsetRect(ATextBounds, 0,
          (cxRectHeight(ATextBounds) - ASize.cy) div 2);
    end;
    ATextBounds := cxRectSetSize(ATextBounds, ASize);
  end
  else
    inherited;
end;

class procedure TdxSpreadSheetFormattedTextService.DrawValue(ACanvas: TcxCanvas;
  ACell: TdxSpreadSheetTableViewCustomDataCellViewInfo; ABounds: TRect);
begin
  if IsFormattedTextValue(ACell.Cell) then
    GetFormattedText(ACell.Cell, ACanvas, cxSize(ABounds),
      ACell.GetTextOutFormat).Draw(ACanvas.Canvas, ABounds.TopLeft)
  else
    inherited;
end;

class function TdxSpreadSheetFormattedTextService.ForceSetAsRTF
  (ACell: TdxSpreadSheetCell; const AEditValue: string): Boolean;
begin
  Result := SetAsRTFValue(ACell, AEditValue, True);
end;

class function TdxSpreadSheetFormattedTextService.GetAsRTF
  (ACell: TdxSpreadSheetCell; var AValue: string;
  ARichEditCompatibility: Boolean): Boolean;
begin
  Result := IsFormattedTextValue(ACell);
  if Result then
    AValue := TdxFormattedTextConverterRTF.Export(GetFormattedText(ACell),
      ACell.StyleHandle.Font.GraphicObject, ARichEditCompatibility);
end;

class function TdxSpreadSheetFormattedTextService.IsFormattedEditValue
  (const AEditValue: string): Boolean;
var
  AText: TdxFormattedText;
begin
  Result := IsRTFSignature(AEditValue);
  if Result then
  begin
    AText := CreateFormattedText;
    try
      TdxFormattedTextConverterRTF.Import(AText, AEditValue, nil, True);
      Result := AText.HasFormatting;
    finally
      AText.Free;
    end;
  end;
end;

class function TdxSpreadSheetFormattedTextService.IsFormattedTextValue
  (ACell: TdxSpreadSheetCell): Boolean;
begin
  Result := (ACell <> nil) and (ACell.DataType = cdtString) and
    (ACell.AsSharedString is TdxSpreadSheetFormattedSharedString);
end;

class function TdxSpreadSheetFormattedTextService.IsRTFSupported: Boolean;
begin
  Result := True;
end;

class function TdxSpreadSheetFormattedTextService.SetAsRTF
  (ACell: TdxSpreadSheetCell; const AEditValue: string): Boolean;
begin
  Result := SetAsRTFValue(ACell, AEditValue, False);
end;

class function TdxSpreadSheetFormattedTextService.SetAsRTFValue
  (ACell: TdxSpreadSheetCell; const AEditValue: string;
  AForce: Boolean): Boolean;
var
  ACache: TdxSpreadSheetFormattedSharedStringCache;
  ADefaultFont: TFont;
  AFormattedSharedString: TdxSpreadSheetFormattedSharedString;
  AFormattedText: TdxFormattedText;
  ASharedString: TdxSpreadSheetSharedString;
begin
  Result := IsRTFSignature(AEditValue);
  if Result then
  begin
    ACache := GetCache(ACell);
    ACache.RemoveItems(ACell.AsSharedString);

    ADefaultFont := GetFontClone(ACell);
    try
      AFormattedText := CreateFormattedText;
      TdxFormattedTextConverterRTF.Import(AFormattedText, AEditValue,
        ADefaultFont, True);
      AFormattedSharedString := TdxSpreadSheetFormattedText.FromFormattedText
        (AFormattedText, ADefaultFont, GetFontTable(ACell));
      Result := AForce or HasFormatting(AFormattedSharedString);
      if Result then
      begin
        ASharedString := TdxCustomSpreadSheetAccess(ACell.SpreadSheet)
          .StringTable.Add(AFormattedSharedString);
        if not dxAreFontsEqual(ADefaultFont,
          ACell.StyleHandle.Font.GraphicObject) then
          ACell.Style.Font.Assign(ADefaultFont);
        ACache.AddRender(ASharedString, ACell.StyleHandle.Font, AFormattedText);
        ACell.AsSharedString := ASharedString; // must be last
      end
      else
      begin
        AFormattedSharedString.Free;
        AFormattedText.Free;
      end;
    finally
      ADefaultFont.Free;
    end;
  end;
end;

class function TdxSpreadSheetFormattedTextService.CalculateTextSize
  (ACanvas: TcxCanvas; ACell: TdxSpreadSheetCell;
  ADisplayWidth: Integer): TSize;
begin
  Result := GetFormattedText(ACell, ACanvas, cxSize(ADisplayWidth, MaxInt),
    IfThen(ACell.Style.WordWrap, CXTO_WORDBREAK)).TextSize;
end;

class function TdxSpreadSheetFormattedTextService.CreateFormattedText
  : TdxFormattedText;
begin
  Result := TdxFormattedText.Create;
  Result.UseOfficeFonts := True;
end;

class function TdxSpreadSheetFormattedTextService.GetCache
  (ACell: TdxSpreadSheetCell): TdxSpreadSheetFormattedSharedStringCache;
begin
  Result := TdxCustomSpreadSheetAccess(ACell.SpreadSheet)
    .FormattedSharedStringCache;
end;

class function TdxSpreadSheetFormattedTextService.GetFontClone
  (ACell: TdxSpreadSheetCell): TFont;
begin
  Result := TFont.Create;
  Result.Assign(ACell.StyleHandle.Font.GraphicObject);
end;

class function TdxSpreadSheetFormattedTextService.GetFontTable
  (ACell: TdxSpreadSheetCell): TdxSpreadSheetFonts;
begin
  Result := TdxCustomSpreadSheetAccess(ACell.SpreadSheet).CellStyles.Fonts;
end;

class function TdxSpreadSheetFormattedTextService.GetFormattedText
  (ACell: TdxSpreadSheetCell): TdxFormattedText;
var
  ACache: TdxSpreadSheetFormattedSharedStringCache;
  AFontHandle: TdxSpreadSheetFontHandle;
  AString: TdxSpreadSheetFormattedSharedString;
begin
  ACache := GetCache(ACell);
  AString := TdxSpreadSheetFormattedSharedString(ACell.AsSharedString);
  AFontHandle := ACell.StyleHandle.Font;
  if not ACache.TryGetRender(AString, AFontHandle, TObject(Result)) then
  begin
    Result := CreateFormattedText;
    TdxSpreadSheetFormattedText.ToFormattedText(Result, AString, AFontHandle);
    ACache.AddRender(AString, AFontHandle, Result);
  end;
end;

class function TdxSpreadSheetFormattedTextService.GetFormattedText
  (ACell: TdxSpreadSheetCell; ACanvas: TcxCanvas; const ASize: TSize;
  AFlags: Cardinal): TdxFormattedText;
begin
  Result := GetFormattedText(ACell);
  Result.CalculateLayout(ACanvas.Canvas, ACell.StyleHandle.Font.GraphicObject,
    cxRect(ASize), AFlags, dxDefaultScaleFactor);
end;

class function TdxSpreadSheetFormattedTextService.HasFormatting
  (AString: TdxSpreadSheetFormattedSharedString): Boolean;
begin
  Result := (AString.Runs.Count > 1) or (AString.Runs.Count > 0) and
    (AString.Runs[0].StartIndex > 1);
end;

class function TdxSpreadSheetFormattedTextService.IsRTFSignature
  (const S: string): Boolean;
begin
  Result := dxIsRichText(S);
end;

initialization

TdxSpreadSheetFormattedTextService.Register;

finalization

TdxSpreadSheetFormattedTextService.UnRegister;

end.
