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

unit dxSpreadSheetFormatHTML;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics, Generics.Defaults,
  Generics.Collections,
  dxCore, dxCoreGraphics, dxHashUtils, cxGraphics, dxGDIPlusClasses, cxClasses,
  dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetUtils,
  dxSpreadSheetTextFileFormatCore, dxSpreadSheetGraphics,
  dxSpreadSheetTypes, dxSpreadSheetHyperlinks, dxSpreadSheetCoreStyles,
  dxSpreadSheetStyles;

type

  { TdxSpreadSheetHTMLFormat }

  TdxSpreadSheetHTMLFormat = class(TdxSpreadSheetCustomFormat)
  strict private
    class var FCellAutoHeight: Boolean;
  public
    class function CanReadFromStream(AStream: TStream): Boolean; override;
    class function GetDescription: string; override;
    class function GetExt: string; override;
    class function GetReader: TdxSpreadSheetCustomReaderClass; override;
    class function GetWriter: TdxSpreadSheetCustomWriterClass; override;
    //
    class property CellAutoHeight: Boolean read FCellAutoHeight
      write FCellAutoHeight;
  end;

  { TdxSpreadSheetHTMFormat }

  TdxSpreadSheetHTMFormat = class(TdxSpreadSheetHTMLFormat)
  public
    class function GetExt: string; override;
  end;

  { TdxSpreadSheetHTMLFontStyleMap }

  TdxSpreadSheetHTMLFontStyleMap = class
    (TDictionary<TdxSpreadSheetFontHandle, string>)
  protected
    procedure KeyNotify(const Key: TdxSpreadSheetFontHandle;
      Action: TCollectionNotification); override;
  end;

  { TdxSpreadSheetHTMLStyleMap }

  TdxSpreadSheetHTMLStyleMap = class
    (TDictionary<TdxSpreadSheetCellStyleHandle, string>)
  protected
    procedure KeyNotify(const Key: TdxSpreadSheetCellStyleHandle;
      Action: TCollectionNotification); override;
  end;

  { TdxSpreadSheetHTMLFormatWriter }

  TdxSpreadSheetHTMLFormatWriter = class(TdxSpreadSheetCustomTextFormatWriter)
  strict private
    FFontMap: TdxSpreadSheetHTMLFontStyleMap;
    FGraphicCount: Integer;
    FStyleMap: TdxSpreadSheetHTMLStyleMap;
    FStylesStream: TMemoryStream;
    FStylesWriter: TStreamWriter;
  protected
    FStylesPositionInStream: Int64;
    FUseInlineStyles: Boolean;

    function CreateProgressHelper
      : TdxSpreadSheetCustomFilerProgressHelper; override;
    function CreateTableViewWriter(AView: TdxSpreadSheetTableView;
      const AArea: TRect): TdxSpreadSheetCustomFilerSubTask; override;
    function GetEncoding: TEncoding; override;

    procedure WriteDocumentFooter; override;
    procedure WriteDocumentHeader; override;
    procedure WriteStandardStyles; virtual;

    property FontMap: TdxSpreadSheetHTMLFontStyleMap read FFontMap;
    property StyleMap: TdxSpreadSheetHTMLStyleMap read FStyleMap;
    property StylesWriter: TStreamWriter read FStylesWriter;
    property UseInlineStyles: Boolean read FUseInlineStyles;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
      override;
    destructor Destroy; override;
    function GetImageFileName(out ARootPath, ASubFileName: string): Boolean;
    function RegisterFont(AFont: TdxSpreadSheetFontHandle): string; virtual;
    function RegisterStyle(AStyle: TdxSpreadSheetCellStyle): string; virtual;
    procedure WriteData; override;
  end;

  { TdxSpreadSheetHTMLFormatCustomWriter }

  TdxSpreadSheetHTMLFormatCustomWriter = class(TdxSpreadSheetCustomFilerSubTask)
  strict private
    FStreamWriter: TStreamWriter;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomFiler;
      AStreamWriter: TStreamWriter);
    //
    property StreamWriter: TStreamWriter read FStreamWriter;
  end;

  { TdxSpreadSheetHTMLFormatDisplayStyleCache }

  TdxSpreadSheetHTMLFormatDisplayStyleCache = class
    (TdxValueCacheManager<Int64, TdxSpreadSheetTableViewCellDisplayStyle>)
  protected
    procedure DoRemove(const Value
      : TdxSpreadSheetTableViewCellDisplayStyle); override;
  public
    constructor Create;
    procedure Add(ARowIndex, AColumnIndex: Integer;
      AStyle: TdxSpreadSheetTableViewCellDisplayStyle);
    function Find(ARowIndex, AColumnIndex: Integer;
      out AStyle: TdxSpreadSheetTableViewCellDisplayStyle): Boolean;
  end;

  { TdxSpreadSheetHTMLFormatTableViewWriter }

  TdxSpreadSheetHTMLFormatTableViewWriter = class
    (TdxSpreadSheetCustomTextFormatTableViewWriter)
  strict private
    FDisplayStyleCache: TdxSpreadSheetHTMLFormatDisplayStyleCache;
    FFloatContainers: TList<TPair<TdxSpreadSheetContainer, TRect>>;
    FHyperlinkCells: TDictionary<TdxSpreadSheetCell, TdxSpreadSheetHyperlink>;
    FInscribedContainers
      : TDictionary<TdxSpreadSheetCell, TdxSpreadSheetContainer>;

    function GetCellStyle(ARowIndex, AColumnIndex: Integer;
      ACell: TdxSpreadSheetCell; ACellIsAssigned: Boolean)
      : TdxSpreadSheetTableViewCellDisplayStyle;
    function GetOwner: TdxSpreadSheetHTMLFormatWriter; inline;
    function IsInscribedContainer(AContainer: TdxSpreadSheetContainer): Boolean;
  protected type
    TCreateWriterProc = reference to function(AWriter: TStreamWriter)
      : TdxSpreadSheetHTMLFormatCustomWriter;
  protected
    FCurrentRowSize: Integer;

    function CalculateActualCellStyle(ARowIndex, AColumnIndex: Integer;
      ACell: TdxSpreadSheetCell): TdxSpreadSheetTableViewCellDisplayStyle;
    procedure ExtendDimensionsByContainers(var R: TRect);
    procedure PrepareContainerImage(const AFileName: string;
      var AContainerBounds: TRect; AContainer: TdxSpreadSheetContainer);
    procedure PrepareContainers; virtual;
    procedure PrepareHyperlinks; virtual;

    procedure WriteCell(ARowIndex, AColumnIndex: Integer;
      ACell: TdxSpreadSheetCell); override;
    procedure WriteCellContent(ACell: TdxSpreadSheetCell);
    procedure WriteCellTagBody(ARowIndex, AColumnIndex: Integer;
      ACell: TdxSpreadSheetCell; AMergedCell: TdxSpreadSheetMergedCell);
    procedure WriteCellTagFooter;
    procedure WriteCellTagHeader(ARowIndex, AColumnIndex: Integer;
      ACell: TdxSpreadSheetCell; AMergedCell: TdxSpreadSheetMergedCell);
    procedure WriteContainer(AContainer: TdxSpreadSheetContainer;
      AContainerBounds: TRect); virtual;
    procedure WriteContainers; virtual;
    procedure WriteFormattedText(AText: TdxSpreadSheetFormattedSharedString);
    procedure WriteHyperlink(AHyperlink: TdxSpreadSheetHyperlink;
      ABody: string = '');
    procedure WriteInlineCellStyle(ABuffer: TStringBuilder;
      ACellStyle: TdxSpreadSheetCellDisplayStyle);
    procedure WriteInlineFontStyle(ABuffer: TStringBuilder;
      AStyle: TdxSpreadSheetFontHandle); overload;
    function WriteInlineFontStyle(AStyle: TdxSpreadSheetFontHandle)
      : string; overload;
    procedure WriteInlineStyle(ABuffer: TStringBuilder;
      ACreateWriterProc: TCreateWriterProc);
    procedure WriteRow(ARowIndex: Integer;
      ARow: TdxSpreadSheetTableRow); override;
    procedure WriteRowFooter(ARowIndex: Integer;
      ARow: TdxSpreadSheetTableRow); virtual;
    procedure WriteRowHeader(ARowIndex: Integer;
      ARow: TdxSpreadSheetTableRow); virtual;
    procedure WriteTableFooter; virtual;
    procedure WriteTableHeader; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomTextFormatWriter;
      AView: TdxSpreadSheetTableView; const AArea: TRect); override;
    destructor Destroy; override;
    procedure Execute; override;
    //
    property FloatContainers: TList < TPair < TdxSpreadSheetContainer,
      TRect >> read FFloatContainers;
    property HyperlinkCells
      : TDictionary<TdxSpreadSheetCell, TdxSpreadSheetHyperlink>
      read FHyperlinkCells;
    property InscribedContainers
      : TDictionary<TdxSpreadSheetCell, TdxSpreadSheetContainer>
      read FInscribedContainers;
    property Owner: TdxSpreadSheetHTMLFormatWriter read GetOwner;
  end;

  { TdxSpreadSheetHTMLFormatFontStyleWriter }

  TdxSpreadSheetHTMLFormatFontStyleWriter = class
    (TdxSpreadSheetHTMLFormatCustomWriter)
  strict private
    FFontStyle: TdxSpreadSheetFontHandle;

    function GetContentStyle: TcxViewParams;
  protected
    procedure WriteFont; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomFiler;
      AWriter: TStreamWriter; AFontStyle: TdxSpreadSheetFontHandle);
    procedure Execute; override;
    //
    property ContentStyle: TcxViewParams read GetContentStyle;
  end;

  { TdxSpreadSheetHTMLFormatCellStyleWriter }

  TdxSpreadSheetHTMLFormatCellStyleWriter = class
    (TdxSpreadSheetHTMLFormatFontStyleWriter)
  strict private
    FStyle: TdxSpreadSheetCellStyleHandle;
  protected
    procedure WriteBorders; virtual;
    procedure WriteBrush; virtual;
    procedure WriteTextAlignment; virtual;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomFiler;
      AWriter: TStreamWriter; AStyle: TdxSpreadSheetCellStyleHandle);
    procedure Execute; override;
    //
    property Style: TdxSpreadSheetCellStyleHandle read FStyle;
  end;

implementation

uses
  Math, StrUtils, RTLConsts, dxColorPicker, dxCoreClasses, cxGeometry,
  dxSpreadSheetCoreHelpers,
  dxSpreadSheetStrs, dxStringHelper;

const
  dxBodyMargin = 8;

type
  TdxSpreadSheetCellStylesAccess = class(TdxSpreadSheetCellStyles);
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetTableColumnsAccess = class(TdxSpreadSheetTableColumns);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetTableViewInfoAccess = class(TdxSpreadSheetTableViewInfo);

function ConvertSpecialCharacters(const AString: string): string;
var
  ABuilder: TStringBuilder;
  AFinishIndex: Integer;
  AStartIndex: Integer;
begin
  ABuilder := TdxStringBuilderManager.Get(MulDiv(Length(AString), 5, 4));
  try
    AStartIndex := 1;
    AFinishIndex := Length(AString);

    while (AStartIndex <= AFinishIndex) and (AString[AStartIndex] = ' ') do
    begin
      ABuilder.Append('&nbsp;');
      Inc(AStartIndex);
    end;

    while (AStartIndex <= AFinishIndex) and (AString[AFinishIndex] = ' ') do
      Dec(AFinishIndex);

    while AStartIndex <= AFinishIndex do
    begin
      case AString[AStartIndex] of
        '<':
          ABuilder.Append('&lt;');
        '>':
          ABuilder.Append('&gt;');
        '&':
          ABuilder.Append('&amp;');
        '"':
          ABuilder.Append('&quot;');
        #10:
          ABuilder.Append('<br/>');
        #13:
          begin
            ABuilder.Append('<br/>');
            if (AStartIndex + 1 <= AFinishIndex) and
              (AString[AStartIndex + 1] = #10) then
              Inc(AStartIndex);
          end;
      else
        ABuilder.Append(AString[AStartIndex]);
      end;
      Inc(AStartIndex);
    end;

    Inc(AFinishIndex);
    while AFinishIndex <= Length(AString) do
    begin
      ABuilder.Append('&nbsp;');
      Inc(AFinishIndex);
    end;

    Result := ABuilder.ToString;
  finally
    TdxStringBuilderManager.Release(ABuilder);
  end;
end;

function GetHTMLColor(AColor: TColor): string;
begin
  if AColor = clNone then
    Result := 'transparent'
  else
    Result := '#' + TdxColorHelper.AlphaColorToHexCode
      (dxColorToAlphaColor(AColor), False, False);
end;

procedure StreamInsertData(AStream: TStream; AData: TMemoryStream;
  const AInsertPosition: Int64;
  AProgressHelper: TdxSpreadSheetCustomFilerProgressHelper);
const
  TempBufferSize = 1048576; // 1 MB
var
  ABytesToRead: Integer;
  ADataSize: Integer;
  ALimitPos, ATempPos, ANewTempPos: Int64;
  AStreamSize: Int64;
  ATempBuffer: PByte;
begin
  ADataSize := AData.Size;
  if ADataSize > 0 then
  begin
    AProgressHelper.BeginStage(100);
    try
      ATempBuffer := AllocMem(TempBufferSize);
      try
        AStreamSize := AStream.Size;
        ATempPos := AStreamSize;
        ALimitPos := AInsertPosition;
        repeat
          ANewTempPos := Max(ALimitPos, ATempPos - TempBufferSize);
          ABytesToRead := ATempPos - ANewTempPos;
          if ABytesToRead > 0 then
          begin
            AStream.Position := ANewTempPos;
            AStream.ReadBuffer(ATempBuffer^, ABytesToRead);
            AStream.Position := ANewTempPos + ADataSize;
            AStream.WriteBuffer(ATempBuffer^, ABytesToRead);
            AProgressHelper.SetTaskNumber
              (Trunc(100 * (AStreamSize - ANewTempPos) / AStreamSize));
            ATempPos := ANewTempPos;
          end;
        until ATempPos = ALimitPos;
      finally
        FreeMem(ATempBuffer, TempBufferSize);
      end;
      AStream.Position := AInsertPosition;
      AStream.WriteBuffer(AData.Memory^, ADataSize);
    finally
      AProgressHelper.EndStage;
    end;
  end;
end;

{ TdxSpreadSheetHTMLFormat }

class function TdxSpreadSheetHTMLFormat.CanReadFromStream
  (AStream: TStream): Boolean;
begin
  Result := False;
end;

class function TdxSpreadSheetHTMLFormat.GetDescription: string;
begin
  Result := 'Web Page';
end;

class function TdxSpreadSheetHTMLFormat.GetExt: string;
begin
  Result := '.html';
end;

class function TdxSpreadSheetHTMLFormat.GetReader
  : TdxSpreadSheetCustomReaderClass;
begin
  Result := nil;
end;

class function TdxSpreadSheetHTMLFormat.GetWriter
  : TdxSpreadSheetCustomWriterClass;
begin
  Result := TdxSpreadSheetHTMLFormatWriter;
end;

{ TdxSpreadSheetHTMFormat }

class function TdxSpreadSheetHTMFormat.GetExt: string;
begin
  Result := '.htm';
end;

{ TdxSpreadSheetHTMLFontStyleMap }

procedure TdxSpreadSheetHTMLFontStyleMap.KeyNotify
  (const Key: TdxSpreadSheetFontHandle; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Key.AddRef;
    cnRemoved, cnExtracted:
      Key.Release;
  end;
end;

{ TdxSpreadSheetHTMLStyleMap }

procedure TdxSpreadSheetHTMLStyleMap.KeyNotify
  (const Key: TdxSpreadSheetCellStyleHandle; Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Key.AddRef;
    cnRemoved, cnExtracted:
      Key.Release;
  end;
end;

{ TdxSpreadSheetHTMLFormatWriter }

constructor TdxSpreadSheetHTMLFormatWriter.Create(AOwner: TdxCustomSpreadSheet;
  AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  WriteEncodingPreamble := False;
  FStyleMap := TdxSpreadSheetHTMLStyleMap.Create;
  FFontMap := TdxSpreadSheetHTMLFontStyleMap.Create;
end;

destructor TdxSpreadSheetHTMLFormatWriter.Destroy;
begin
  FreeAndNil(FStyleMap);
  FreeAndNil(FFontMap);
  inherited Destroy;
end;

function TdxSpreadSheetHTMLFormatWriter.GetImageFileName(out ARootPath,
  ASubFileName: string): Boolean;
begin
  Result := Stream is TFileStream;
  if Result then
  begin
    ARootPath := ExtractFilePath(TFileStream(Stream).FileName);
    if ARootPath = '' then
      ARootPath := IncludeTrailingPathDelimiter(GetCurrentDir);
    ASubFileName := ChangeFileExt(ExtractFileName(TFileStream(Stream).FileName),
      '.images') + PathDelim + IntToStr(FGraphicCount) + '.png';
    Inc(FGraphicCount);
  end;
end;

procedure TdxSpreadSheetHTMLFormatWriter.WriteData;
begin
  FStylesStream := TMemoryStream.Create;
  FStylesWriter := TdxSpreadSheetStreamWriter.Create(FStylesStream,
    Encoding, False);
  try
    inherited WriteData;
    StreamInsertData(Stream, FStylesStream, FStylesPositionInStream,
      ProgressHelper);
  finally
    FreeAndNil(FStylesStream);
    FreeAndNil(FStylesWriter);
  end;
end;

function TdxSpreadSheetHTMLFormatWriter.CreateProgressHelper
  : TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, 3);
end;

function TdxSpreadSheetHTMLFormatWriter.CreateTableViewWriter
  (AView: TdxSpreadSheetTableView; const AArea: TRect)
  : TdxSpreadSheetCustomFilerSubTask;
begin
  Result := TdxSpreadSheetHTMLFormatTableViewWriter.Create(Self, AView, AArea);
end;

function TdxSpreadSheetHTMLFormatWriter.GetEncoding: TEncoding;
begin
  Result := TEncoding.UTF8;
end;

function TdxSpreadSheetHTMLFormatWriter.RegisterFont
  (AFont: TdxSpreadSheetFontHandle): string;
begin
  if not FontMap.TryGetValue(AFont, Result) then
  begin
    Result := 'Font_' + IntToStr(FontMap.Count);

    StylesWriter.WriteLine('.' + Result);
    StylesWriter.WriteLine('{');
    ExecuteSubTask(TdxSpreadSheetHTMLFormatFontStyleWriter.Create(Self,
      StylesWriter, AFont));
    StylesWriter.WriteLine('}');

    FontMap.Add(AFont, Result);
  end;
end;

function TdxSpreadSheetHTMLFormatWriter.RegisterStyle
  (AStyle: TdxSpreadSheetCellStyle): string;
begin
  if not StyleMap.TryGetValue(AStyle.Handle, Result) then
  begin
    Result := 'CellStyle_' + IntToStr(StyleMap.Count);

    StylesWriter.WriteLine('.' + Result);
    StylesWriter.WriteLine('{');
    ExecuteSubTask(TdxSpreadSheetHTMLFormatCellStyleWriter.Create(Self,
      StylesWriter, AStyle.Handle));
    StylesWriter.WriteLine('}');

    StyleMap.Add(AStyle.Handle, Result);
  end;
end;

procedure TdxSpreadSheetHTMLFormatWriter.WriteDocumentFooter;
begin
  StreamWriter.WriteLine('</body>');
  StreamWriter.WriteLine('</html>');
end;

procedure TdxSpreadSheetHTMLFormatWriter.WriteDocumentHeader;
begin
  StreamWriter.WriteLine
    ('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">');
  StreamWriter.WriteLine('<html>');
  StreamWriter.WriteLine('<head>');
  StreamWriter.WriteLine
    ('<meta http-equiv="content-type" content="text/html" charset="utf-8">');

  StreamWriter.Write('<title>');
  StreamWriter.Write(SpreadSheet.ActiveSheetAsTable.Caption);
  StreamWriter.WriteLine('</title>');

  StreamWriter.WriteLine('<style type="text/css"><!--');
  WriteStandardStyles;
  FStylesPositionInStream := StreamWriter.BaseStream.Position;
  StreamWriter.WriteLine('--></style>');

  StreamWriter.WriteLine('</head>');
  StreamWriter.WriteLine('<body>');
end;

procedure TdxSpreadSheetHTMLFormatWriter.WriteStandardStyles;
begin
  StreamWriter.WriteLine
    ('div.cell { width: 100%; overflow: hidden; position: relative;}');
  StreamWriter.WriteLine
    ('table { border-collapse: collapse; table-layout: fixed; }');
  StreamWriter.WriteLine('table td { overflow: hidden; padding: 0px;}');
  StreamWriter.WriteLine('body { margin: %dpx; }', [dxBodyMargin]);
end;

{ TdxSpreadSheetHTMLFormatCustomWriter }

constructor TdxSpreadSheetHTMLFormatCustomWriter.Create
  (AOwner: TdxSpreadSheetCustomFiler; AStreamWriter: TStreamWriter);
begin
  inherited Create(AOwner);
  FStreamWriter := AStreamWriter;
end;

{ TdxSpreadSheetHTMLFormatDisplayStyleCache }

constructor TdxSpreadSheetHTMLFormatDisplayStyleCache.Create;
begin
  inherited Create(dxSpreadSheetMaxColumnCount * 3);
end;

procedure TdxSpreadSheetHTMLFormatDisplayStyleCache.DoRemove
  (const Value: TdxSpreadSheetTableViewCellDisplayStyle);
begin
  Value.Free;
end;

procedure TdxSpreadSheetHTMLFormatDisplayStyleCache.Add(ARowIndex,
  AColumnIndex: Integer; AStyle: TdxSpreadSheetTableViewCellDisplayStyle);
begin
  inherited Add(dxMakeInt64(ARowIndex, AColumnIndex), AStyle);
end;

function TdxSpreadSheetHTMLFormatDisplayStyleCache.Find(ARowIndex,
  AColumnIndex: Integer;
  out AStyle: TdxSpreadSheetTableViewCellDisplayStyle): Boolean;
begin
  Result := Get(dxMakeInt64(ARowIndex, AColumnIndex), AStyle);
end;

{ TdxSpreadSheetHTMLFormatTableViewWriter }

constructor TdxSpreadSheetHTMLFormatTableViewWriter.Create
  (AOwner: TdxSpreadSheetCustomTextFormatWriter; AView: TdxSpreadSheetTableView;
  const AArea: TRect);
begin
  inherited Create(AOwner, AView, AArea);
  FEnumBlankCellsInTheEndOfLine := True;
  FFloatContainers := TList < TPair < TdxSpreadSheetContainer, TRect >>.Create;
  FInscribedContainers :=
    TDictionary<TdxSpreadSheetCell, TdxSpreadSheetContainer>.Create;
  FDisplayStyleCache := TdxSpreadSheetHTMLFormatDisplayStyleCache.Create;
  FHyperlinkCells := TDictionary<TdxSpreadSheetCell,
    TdxSpreadSheetHyperlink>.Create;
end;

destructor TdxSpreadSheetHTMLFormatTableViewWriter.Destroy;
begin
  FreeAndNil(FInscribedContainers);
  FreeAndNil(FDisplayStyleCache);
  FreeAndNil(FFloatContainers);
  FreeAndNil(FHyperlinkCells);
  inherited Destroy;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.Execute;
begin
  PrepareContainers;
  PrepareHyperlinks;
  WriteTableHeader;
  inherited Execute;
  WriteTableFooter;
  WriteContainers;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.ExtendDimensionsByContainers
  (var R: TRect);
var
  AColumnIndex: Integer;
  ARowIndex: Integer;
  ATableView: IdxSpreadSheetTableView;
  I: Integer;
begin
  if Supports(View, IdxSpreadSheetTableView, ATableView) then
    for I := 0 to FloatContainers.Count - 1 do
      if ATableView.GetCellAtAbsolutePoint(FloatContainers[I].Value.BottomRight,
        ARowIndex, AColumnIndex) then
      begin
        R.Right := Max(R.Right, AColumnIndex);
        R.Bottom := Max(R.Bottom, ARowIndex);
      end;
end;

function TdxSpreadSheetHTMLFormatTableViewWriter.CalculateActualCellStyle
  (ARowIndex, AColumnIndex: Integer; ACell: TdxSpreadSheetCell)
  : TdxSpreadSheetTableViewCellDisplayStyle;

  function GetNeighborCellStyle(ARow, AColumn: Integer; ASide: TcxBorder)
    : TdxSpreadSheetCellStyle;
  begin
    case ASide of
      bLeft:
        Dec(AColumn);
      bTop:
        Dec(ARow);
      bRight:
        Inc(AColumn);
    else
      Inc(ARow);
    end;
    Result := GetCellStyle(ARow, AColumn, nil, False);
  end;

var
  ABorder: TcxBorder;
  ABorderColor: TColor;
  ABorderStyle: TdxSpreadSheetCellBorderStyle;
begin
  Result := TdxSpreadSheetTableViewCellDisplayStyle.Create(View);
  Result.Handle := GetCellStyle(ARowIndex, AColumnIndex, ACell, True).Handle;
  Result.BeginUpdate;
  try
    for ABorder := bRight to bBottom do
    begin
      dxSpreadSheetMergeBorderStyle(ABorder, Result.Handle,
        GetNeighborCellStyle(ARowIndex, AColumnIndex, ABorder).Handle,
        ABorderColor, ABorderStyle);
      Result.Borders[ABorder].Color := ABorderColor;
      Result.Borders[ABorder].Style := ABorderStyle;
    end;
  finally
    Result.EndUpdate;
  end;
end;

function TdxSpreadSheetHTMLFormatTableViewWriter.GetCellStyle(ARowIndex,
  AColumnIndex: Integer; ACell: TdxSpreadSheetCell; ACellIsAssigned: Boolean)
  : TdxSpreadSheetTableViewCellDisplayStyle;
begin
  if not FDisplayStyleCache.Find(ARowIndex, AColumnIndex, Result) then
  begin
    if not ACellIsAssigned then
      ACell := View.Cells[ARowIndex, AColumnIndex];
    Result := TdxSpreadSheetTableViewCellDisplayStyle.Create(View,
      TdxSpreadSheetTableViewAccess(View).GetCellStyleHandle(ARowIndex,
      AColumnIndex, ACell));
    View.ConditionalFormatting.CalculateStyle(Result, ARowIndex,
      AColumnIndex, ACell);
    FDisplayStyleCache.Add(ARowIndex, AColumnIndex, Result);
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.PrepareContainerImage
  (const AFileName: string; var AContainerBounds: TRect;
  AContainer: TdxSpreadSheetContainer);

  function CreateContainerImage(var AContainerBounds: TRect): TdxPNGImage;
  var
    ABitmap: TcxBitmap32;
    ACanDrawSelection: Boolean;
    AViewInfo: TdxSpreadSheetContainerViewInfo;
  begin
    AViewInfo := TdxSpreadSheetContainerAccess(AContainer).CreateViewInfo;
    try
      AViewInfo.SetBounds(AContainerBounds, AContainerBounds);
      AViewInfo.Calculate;

      ABitmap := TcxBitmap32.CreateSize(AViewInfo.RealDrawingBounds, True);
      try
        ACanDrawSelection := AViewInfo.CanDrawSelection;
        try
          AViewInfo.CanDrawSelection := False;
          ABitmap.cxCanvas.WindowOrg := AViewInfo.RealDrawingBounds.TopLeft;
          AViewInfo.Draw(ABitmap.cxCanvas, dsFirst);
          AViewInfo.Draw(ABitmap.cxCanvas, dsSecond);
        finally
          AViewInfo.CanDrawSelection := ACanDrawSelection;
        end;

        Result := TdxPNGImage.CreateFromBitmap(ABitmap);
      finally
        ABitmap.Free;
      end;

      AContainerBounds := AViewInfo.RealDrawingBounds;
    finally
      AViewInfo.Free;
    end;
  end;

var
  AImage: TdxPNGImage;
begin
  AImage := CreateContainerImage(AContainerBounds);
  try
    ForceDirectories(ExtractFilePath(AFileName));
    AImage.SaveToFile(AFileName);
  finally
    AImage.Free;
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.PrepareContainers;
var
  AContainer: TdxSpreadSheetContainerAccess;
  I: Integer;
begin
  if StreamWriter.BaseStream is TFileStream then
  begin
    for I := 0 to View.Containers.Count - 1 do
    begin
      AContainer := TdxSpreadSheetContainerAccess(View.Containers[I]);
      if AContainer.Visible then
      begin
        if IsInscribedContainer(AContainer) then
          InscribedContainers.AddOrSetValue(AContainer.AnchorPoint1.Cell,
            AContainer)
        else
          FloatContainers.Add(TPair<TdxSpreadSheetContainer, TRect>.Create
            (AContainer, AContainer.Calculator.CalculateBounds));
      end;
    end;
    ExtendDimensionsByContainers(FArea);
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.PrepareHyperlinks;
var
  ACell: TdxSpreadSheetCell;
  AHyperlink: TdxSpreadSheetHyperlink;
  I: Integer;
begin
  for I := 0 to View.Hyperlinks.Count - 1 do
  begin
    AHyperlink := View.Hyperlinks[I];
    if AHyperlink.IsAreaCorrect and (AHyperlink.ValueType <> hvtReference) then
    begin
      ACell := View.CreateCell(AHyperlink.Area.Top, AHyperlink.Area.Left);
      if not HyperlinkCells.ContainsKey(ACell) then
        HyperlinkCells.Add(ACell, AHyperlink);
    end;
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteCell(ARowIndex,
  AColumnIndex: Integer; ACell: TdxSpreadSheetCell);
var
  AMergedCell: TdxSpreadSheetMergedCell;
begin
  AMergedCell := View.MergedCells.FindCell(ARowIndex, AColumnIndex);
  if (AMergedCell = nil) or (AMergedCell.Area.Left = AColumnIndex) and
    (AMergedCell.Area.Top = ARowIndex) then
  begin
    WriteCellTagHeader(ARowIndex, AColumnIndex, ACell, AMergedCell);
    WriteCellTagBody(ARowIndex, AColumnIndex, ACell, AMergedCell);
    WriteCellTagFooter;
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteCellTagBody(ARowIndex,
  AColumnIndex: Integer; ACell: TdxSpreadSheetCell;
  AMergedCell: TdxSpreadSheetMergedCell);

  function GetInscribedContainer: TdxSpreadSheetContainer;
  begin
    if not InscribedContainers.TryGetValue(ACell, Result) then
      Result := nil;
  end;

  function IsSingeLineCell(AMergedCell: TdxSpreadSheetMergedCell): Boolean;
  begin
    Result := (AMergedCell = nil) or
      (dxSpreadSheetAreaHeight(AMergedCell.Area) = 1);
  end;

  procedure WriteDivHeader(AInscribedContainer: TdxSpreadSheetContainer);
  var
    AInlineStyles: TStringBuilder;
  begin
    AInlineStyles := TdxStringBuilderManager.Get;
    try
      if ACell.Style.AlignHorzIndent > 0 then
      begin
        if ACell.Style.AlignHorz in [ssahLeft, ssahDistributed] then
          AInlineStyles.AppendFormat('padding-left: %dpx; ',
            [ACell.Style.AlignHorzIndent]);
        if ACell.Style.AlignHorz in [ssahRight, ssahDistributed] then
          AInlineStyles.AppendFormat('padding-right: %dpx; ',
            [ACell.Style.AlignHorzIndent]);
      end;

      if AInscribedContainer <> nil then
        AInlineStyles.AppendFormat('min-height: %dpx; ', [FCurrentRowSize]);

      if not TdxSpreadSheetHTMLFormat.CellAutoHeight and
        IsSingeLineCell(AMergedCell) then
        AInlineStyles.AppendFormat('max-height: %dpx; ', [FCurrentRowSize]);

      StreamWriter.Write('<div class="cell"');
      if AInlineStyles.Length > 0 then
      begin
        StreamWriter.Write(' style="');
        StreamWriter.Write(AInlineStyles.ToString);
        StreamWriter.Write('"');
      end;
      StreamWriter.Write('>');
    finally
      TdxStringBuilderManager.Release(AInlineStyles);
    end;
  end;

  procedure WriteDivFooter;
  begin
    StreamWriter.Write('</div>');
  end;

var
  AContainer: TdxSpreadSheetContainerAccess;
begin
  if ACell <> nil then
  begin
    AContainer := TdxSpreadSheetContainerAccess(GetInscribedContainer);
    if not ACell.IsEmpty or (AContainer <> nil) then
    begin
      WriteDivHeader(AContainer);

      if ACell.Style.WordWrap then
        WriteCellContent(ACell)
      else
      begin
        StreamWriter.Write('<nobr>');
        WriteCellContent(ACell);
        StreamWriter.Write('</nobr>');
      end;

      if AContainer <> nil then
        WriteContainer(AContainer,
          cxRectSetOrigin(AContainer.Calculator.CalculateBounds,
          AContainer.AnchorPoint1.Offset));

      WriteDivFooter;
    end;
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteCellTagFooter;
begin
  StreamWriter.WriteLine('</td>');
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteCellTagHeader(ARowIndex,
  AColumnIndex: Integer; ACell: TdxSpreadSheetCell;
  AMergedCell: TdxSpreadSheetMergedCell);
var
  ACellStyle: TdxSpreadSheetCellDisplayStyle;
  AInlineStyles: TStringBuilder;
begin
  StreamWriter.Write('<td');

  AInlineStyles := TdxStringBuilderManager.Get;
  try
    ACellStyle := CalculateActualCellStyle(ARowIndex, AColumnIndex, ACell);
    try
      if Owner.UseInlineStyles then
        WriteInlineCellStyle(AInlineStyles, ACellStyle)
      else
      begin
        StreamWriter.Write(' class="');
        StreamWriter.Write(Owner.RegisterStyle(ACellStyle));
        StreamWriter.Write('"');
      end;
    finally
      ACellStyle.Free;
    end;

    if AMergedCell <> nil then
    begin
      if dxSpreadSheetAreaWidth(AMergedCell.Area) > 1 then
        StreamWriter.Write(' colspan="%d"',
          [dxSpreadSheetAreaWidth(AMergedCell.Area)]);
      if dxSpreadSheetAreaHeight(AMergedCell.Area) > 1 then
        StreamWriter.Write(' rowspan="%d"',
          [dxSpreadSheetAreaHeight(AMergedCell.Area)]);
    end;

    if (ACell <> nil) and (ACell.Style.AlignHorz = ssahGeneral) and ACell.IsNumericValue
    then
      AInlineStyles.Append('text-align: right;');

    if AInlineStyles.Length > 0 then
    begin
      StreamWriter.Write(' style="');
      StreamWriter.Write(AInlineStyles.ToString);
      StreamWriter.Write('"');
    end;
  finally
    TdxStringBuilderManager.Release(AInlineStyles);
  end;

  StreamWriter.Write('>');
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteCellContent
  (ACell: TdxSpreadSheetCell);
var
  AHyperlink: TdxSpreadSheetHyperlink;
begin
  if HyperlinkCells.TryGetValue(ACell, AHyperlink) then
    WriteHyperlink(AHyperlink)
  else if ACell.AsSharedString is TdxSpreadSheetFormattedSharedString then
    WriteFormattedText(TdxSpreadSheetFormattedSharedString
      (ACell.AsSharedString))
  else
    StreamWriter.Write(ConvertSpecialCharacters(ACell.DisplayText));
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteContainer
  (AContainer: TdxSpreadSheetContainer; AContainerBounds: TRect);
var
  AFileName: string;
  ARootPath: string;
begin
  if Owner.GetImageFileName(ARootPath, AFileName) then
  begin
    PrepareContainerImage(ARootPath + AFileName, AContainerBounds, AContainer);
    WriteHyperlink(AContainer.Hyperlink,
      Format('<img src="%s" style="position: absolute; left: %dpx; top: %dpx">',
      [dxReplacePathDelimiter(AFileName, PathDelim, '/'), AContainerBounds.Left,
      AContainerBounds.Top]));
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteContainers;
var
  I: Integer;
begin
  ProgressHelper.BeginStage(FloatContainers.Count);
  try
    for I := 0 to FloatContainers.Count - 1 do
    begin
      with FloatContainers[I] do
        WriteContainer(Key, cxRectOffset(Value, dxBodyMargin, dxBodyMargin));
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteTableFooter;
begin
  StreamWriter.WriteLine('</table>');
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteTableHeader;

  function GetBackgroundColor: TColor;
  begin
    Result := cxGetActualColor
      (View.CellStyles.DefaultStyle.Brush.BackgroundColor,
      SpreadSheet.Styles.GetContentStyle(nil).Color);
  end;

  function GetTotalWidth(const AColumnWidths: array of Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to Length(AColumnWidths) - 1 do
      Inc(Result, AColumnWidths[I]);
  end;

  procedure DoWriteTableHeader(const AColumnWidths: array of Integer);
  var
    I: Integer;
  begin
    StreamWriter.WriteLine
      ('<table border="0" cellspacing="0" width="%dpx" bgcolor="%s">',
      [GetTotalWidth(AColumnWidths), GetHTMLColor(GetBackgroundColor)]);
    for I := 0 to Length(AColumnWidths) - 1 do
      StreamWriter.WriteLine('<col width="%dpx"/>', [AColumnWidths[I]]);
  end;

var
  AColumnWidths: array of Integer;
  I: Integer;
begin
  SetLength(AColumnWidths, Area.Right + 1);
  for I := 0 to Area.Right do
    AColumnWidths[I] := TdxSpreadSheetTableColumnsAccess(View.Columns)
      .GetItemSize(I);
  DoWriteTableHeader(AColumnWidths);
  SetLength(AColumnWidths, 0);
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteFormattedText
  (AText: TdxSpreadSheetFormattedSharedString);

  function GetRunLength(AStartIndex, ARunIndex: Integer): Integer;
  begin
    if ARunIndex + 1 < AText.Runs.Count then
      Result := AText.Runs[ARunIndex + 1].StartIndex - AStartIndex
    else
      Result := Length(AText.Value) - AStartIndex + 1;
  end;

var
  ALength: Integer;
  ARun: TdxSpreadSheetFormattedSharedStringRun;
  I: Integer;
begin
  ALength := GetRunLength(1, -1);
  if ALength > 0 then
    StreamWriter.Write(ConvertSpecialCharacters(Copy(AText.Value, 1, ALength)));

  for I := 0 to AText.Runs.Count - 1 do
  begin
    ARun := AText.Runs[I];
    ALength := GetRunLength(ARun.StartIndex, I);
    if ALength > 0 then
    begin
      StreamWriter.Write('<span');

      if Owner.UseInlineStyles then
      begin
        StreamWriter.Write(' style="');
        StreamWriter.Write(WriteInlineFontStyle(ARun.FontHandle));
        StreamWriter.Write('"');
      end
      else
      begin
        StreamWriter.Write(' class="');
        StreamWriter.Write(Owner.RegisterFont(ARun.FontHandle));
        StreamWriter.Write('"');
      end;

      StreamWriter.Write('>');
      StreamWriter.Write(ConvertSpecialCharacters(Copy(AText.Value,
        ARun.StartIndex, ALength)));
      StreamWriter.Write('</span>');
    end;
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteHyperlink
  (AHyperlink: TdxSpreadSheetHyperlink; ABody: string = '');
var
  AHint: string;
begin
  if (AHyperlink = nil) or (AHyperlink.ValueType = hvtReference) then
  begin
    StreamWriter.Write(ABody);
    Exit;
  end;

  if havScreenTip in AHyperlink.AssignedValues then
    AHint := AHyperlink.ScreenTip
  else
    AHint := Format(cxGetResourceString(@sdxDefaultHyperlinkShortScreenTip),
      [AHyperlink.Value]);

  if ABody = '' then
    ABody := ConvertSpecialCharacters(AHyperlink.DisplayText);

  StreamWriter.Write('<a href="');
  StreamWriter.Write(AHyperlink.Value);
  StreamWriter.Write('" target="_blank" title="');
  StreamWriter.Write(ConvertSpecialCharacters(AHint));
  StreamWriter.Write('">');
  StreamWriter.Write(ABody);
  StreamWriter.Write('</a>');
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteInlineCellStyle
  (ABuffer: TStringBuilder; ACellStyle: TdxSpreadSheetCellDisplayStyle);
begin
  WriteInlineStyle(ABuffer,
    function(AWriter: TStreamWriter): TdxSpreadSheetHTMLFormatCustomWriter
    begin
      Result := TdxSpreadSheetHTMLFormatCellStyleWriter.Create(Owner, AWriter,
        ACellStyle.Handle);
    end);
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteInlineFontStyle
  (ABuffer: TStringBuilder; AStyle: TdxSpreadSheetFontHandle);
begin
  WriteInlineStyle(ABuffer,
    function(AWriter: TStreamWriter): TdxSpreadSheetHTMLFormatCustomWriter
    begin
      Result := TdxSpreadSheetHTMLFormatFontStyleWriter.Create(Owner,
        AWriter, AStyle);
    end);
end;

function TdxSpreadSheetHTMLFormatTableViewWriter.WriteInlineFontStyle
  (AStyle: TdxSpreadSheetFontHandle): string;
var
  ABuffer: TStringBuilder;
begin
  ABuffer := TdxStringBuilderManager.Get;
  try
    WriteInlineFontStyle(ABuffer, AStyle);
    Result := ABuffer.ToString;
  finally
    TdxStringBuilderManager.Release(ABuffer);
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteInlineStyle
  (ABuffer: TStringBuilder; ACreateWriterProc: TCreateWriterProc);
var
  ALines: TStringList;
  AStream: TMemoryStream;
  AStreamWriter: TStreamWriter;
  I: Integer;
begin
  AStream := TMemoryStream.Create;
  try
    AStreamWriter := TStreamWriter.Create(AStream, Owner.Encoding);
    try
      ExecuteSubTask(ACreateWriterProc(AStreamWriter));
    finally
      AStreamWriter.Free;
    end;
    AStream.Position := 0;

    ALines := TStringList.Create;
    try
      ALines.LoadFromStream(AStream, Owner.Encoding);
      for I := 0 to ALines.Count - 1 do
      begin
        ABuffer.Append(ALines[I]);
        ABuffer.Append(' ');
      end;
    finally
      ALines.Free;
    end;
  finally
    AStream.Free;
  end;
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteRow(ARowIndex: Integer;
ARow: TdxSpreadSheetTableRow);
begin
  if ARow <> nil then
    FCurrentRowSize := ARow.Size
  else
    FCurrentRowSize := View.Rows.DefaultSize;

  WriteRowHeader(ARowIndex, ARow);
  inherited WriteRow(ARowIndex, ARow);
  WriteRowFooter(ARowIndex, ARow);
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteRowFooter
  (ARowIndex: Integer; ARow: TdxSpreadSheetTableRow);
begin
  StreamWriter.Write('</tr>');
end;

procedure TdxSpreadSheetHTMLFormatTableViewWriter.WriteRowHeader
  (ARowIndex: Integer; ARow: TdxSpreadSheetTableRow);
begin
  StreamWriter.WriteLine('<tr height="%dpx">', [FCurrentRowSize])
end;

function TdxSpreadSheetHTMLFormatTableViewWriter.GetOwner
  : TdxSpreadSheetHTMLFormatWriter;
begin
  Result := TdxSpreadSheetHTMLFormatWriter(inherited Owner);
end;

function TdxSpreadSheetHTMLFormatTableViewWriter.IsInscribedContainer
  (AContainer: TdxSpreadSheetContainer): Boolean;

  function Check(AItem1, AItem2: TdxSpreadSheetTableItem;
  AOffset: Integer): Boolean;
  begin
    Result := (AItem1 = AItem2) or (AOffset = 0) and
      (AItem1.Index + 1 = AItem2.Index);
  end;

begin
  Result := (AContainer.AnchorPoint1.Cell <> nil) and
    (AContainer.AnchorPoint2.Cell <> nil) and
    Check(AContainer.AnchorPoint1.Cell.Column,
    AContainer.AnchorPoint2.Cell.Column, AContainer.AnchorPoint2.Offset.X) and
    Check(AContainer.AnchorPoint1.Cell.Row, AContainer.AnchorPoint2.Cell.Row,
    AContainer.AnchorPoint2.Offset.Y);
end;

{ TdxSpreadSheetHTMLFormatFontStyleWriter }

constructor TdxSpreadSheetHTMLFormatFontStyleWriter.Create
  (AOwner: TdxSpreadSheetCustomFiler; AWriter: TStreamWriter;
AFontStyle: TdxSpreadSheetFontHandle);
begin
  inherited Create(AOwner, AWriter);
  FFontStyle := AFontStyle;
end;

procedure TdxSpreadSheetHTMLFormatFontStyleWriter.Execute;
begin
  WriteFont;
end;

procedure TdxSpreadSheetHTMLFormatFontStyleWriter.WriteFont;
const
  VertAlign: array [TdxSpreadSheetFontScript] of string = ('baseline',
    'super', 'sub');
var
  ASize: Integer;
  ATextColor: TColor;
begin
  ATextColor := FFontStyle.Color;
  if not cxColorIsValid(ATextColor) then
    ATextColor := ContentStyle.TextColor;
  ASize := FFontStyle.Size;
  if FFontStyle.Script <> fsNone then
  begin
    StreamWriter.WriteLine('vertical-align: %s;',
      [VertAlign[FFontStyle.Script]]);
    ASize := MulDiv(ASize, 50, 100);
  end;
  StreamWriter.WriteLine('color: %s;', [GetHTMLColor(ATextColor)]);
  StreamWriter.WriteLine('font-family: %s;', [FFontStyle.Name]);
  StreamWriter.WriteLine('font-size: %dpt;', [ASize]);
  StreamWriter.WriteLine('mso-font-charset: %d;', [FFontStyle.Charset]);

  if fsBold in FFontStyle.Style then
    StreamWriter.WriteLine('font-weight: bold;');
  if fsItalic in FFontStyle.Style then
    StreamWriter.WriteLine('font-style: italic;');
  if fsUnderline in FFontStyle.Style then
    StreamWriter.WriteLine('text-decoration: underline;')
  else if fsStrikeOut in FFontStyle.Style then
    StreamWriter.WriteLine('text-decoration: line-through;');
end;

function TdxSpreadSheetHTMLFormatFontStyleWriter.GetContentStyle: TcxViewParams;
begin
  Result := SpreadSheet.Styles.GetContentStyle(nil);
end;

{ TdxSpreadSheetHTMLFormatCellStyleWriter }

constructor TdxSpreadSheetHTMLFormatCellStyleWriter.Create
  (AOwner: TdxSpreadSheetCustomFiler; AWriter: TStreamWriter;
AStyle: TdxSpreadSheetCellStyleHandle);
begin
  inherited Create(AOwner, AWriter, AStyle.Font);
  FStyle := AStyle;
end;

procedure TdxSpreadSheetHTMLFormatCellStyleWriter.Execute;
begin
  inherited;
  WriteBorders;
  WriteBrush;
  WriteTextAlignment;
end;

procedure TdxSpreadSheetHTMLFormatCellStyleWriter.WriteBorders;
const
  BorderStyle: array [TdxSpreadSheetCellBorderStyle] of string = ('solid',
    'dotted', 'dotted', 'dashed', 'dashed', 'dashed', 'solid', 'dashed',
    'solid', 'dashed', 'dashed', 'solid', 'solid', 'double', 'solid');
  SideName: array [TcxBorder] of string = ('left', 'top', 'right', 'bottom');
var
  ABorderColor: TColor;
  ADefaultBorderColor: TColor;
  ASide: TcxBorder;
begin
  if SpreadSheet.ActiveSheetAsTable.Options.ActualGridLines then
    ADefaultBorderColor := cxGetActualColor
      (SpreadSheet.OptionsView.GridLineColor, clBtnShadow)
  else
    ADefaultBorderColor := clNone;

  for ASide := Low(TcxBorder) to High(TcxBorder) do
  begin
    ABorderColor := Style.Borders.BorderColor[ASide];
    if Style.Borders.BorderStyle[ASide] = sscbsDefault then
      ABorderColor := cxGetActualColor(ABorderColor, ADefaultBorderColor)
    else
      ABorderColor := cxGetActualColor(ABorderColor, clBlack);

    StreamWriter.WriteLine('border-%s: %dpx %s %s;',
      [SideName[ASide], IfThen(ABorderColor <> clNone,
      dxSpreadSheetBorderStyleThickness[Style.Borders.BorderStyle[ASide]]),
      BorderStyle[Style.Borders.BorderStyle[ASide]],
      GetHTMLColor(ABorderColor)]);
  end;
end;

procedure TdxSpreadSheetHTMLFormatCellStyleWriter.WriteBrush;
begin
  if Style.Brush.Style = sscfsSolid then
    StreamWriter.Write('background-color: %s;',
      [GetHTMLColor(cxGetActualColor(Style.Brush.BackgroundColor,
      ContentStyle.Color))]);
end;

procedure TdxSpreadSheetHTMLFormatCellStyleWriter.WriteTextAlignment;
const
  AlignHorzMap: array [TdxSpreadSheetDataAlignHorz] of string = ('left', 'left',
    'center', 'right', 'justify', 'justify', 'justify');
  AlignVertMap: array [TdxSpreadSheetDataAlignVert] of string = ('top',
    'middle', 'bottom', 'middle', 'middle');
begin
  StreamWriter.WriteLine('text-align: %s;', [AlignHorzMap[Style.AlignHorz]]);
  if Style.AlignHorzIndent > 0 then
  begin
    if Style.AlignHorz in [ssahLeft, ssahDistributed] then
      StreamWriter.WriteLine('padding-left: %dpx;', [Style.AlignHorzIndent]);
    if Style.AlignHorz in [ssahRight, ssahDistributed] then
      StreamWriter.WriteLine('padding-right: %dpx;', [Style.AlignHorzIndent]);
  end;
  StreamWriter.WriteLine('vertical-align: %s;',
    [AlignVertMap[Style.AlignVert]]);
end;

initialization

TdxSpreadSheetHTMFormat.Register;
TdxSpreadSheetHTMLFormat.Register;

finalization

TdxSpreadSheetHTMLFormat.Unregister;
TdxSpreadSheetHTMFormat.Unregister;

end.
