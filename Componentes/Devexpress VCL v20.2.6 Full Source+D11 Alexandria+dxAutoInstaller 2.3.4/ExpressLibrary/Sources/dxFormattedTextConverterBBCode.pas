{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library controls }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
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
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
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

unit dxFormattedTextConverterBBCode;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Graphics, Classes, Generics.Collections, SysUtils,
  dxFormattedText;

type
  TdxBBCode = (bbcNoCode, bbcBold, bbcItalic, bbcUnderline, bbcStrikeout,
    bbcColor, bbcURL, bbcBackgroundColor, bbcSize, bbcSup, bbcSub, bbcFont,
    bbcNoParse, bbcImg);

  TdxBBCodeTag = record
    Action: TdxFormattedTextRunAction;
    Argument: string;
    BBCode: TdxBBCode;
    BeginPosition: PChar;
    EndPosition: PChar;
  end;

  { TdxBBCodeStack }

  TdxBBCodeStack = class
  strict private
    FList: TStringList;

    function GetCount: Integer;
    function GetItem(Index: Integer): TdxBBCode;
  public
    constructor Create;
    destructor Destroy; override;

    function Exists(ACode: TdxBBCode): Boolean;
    procedure ExcludeCode(ACode: TdxBBCode);
    procedure IncludeCode(ACode: TdxBBCode; const AValue: string = '');

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxBBCode read GetItem; default;
  end;

  { TdxBBCodeRunFactory }

  TdxBBCodeRunConstructorFunc = function(const ATag: TdxBBCodeTag;
    const AData: Pointer): TdxFormattedTextRun;

  TdxBBCodeRunFactory = class
  strict private
    FData: array [TdxBBCode] of TPair<TdxBBCodeRunConstructorFunc, Pointer>;
  public
    procedure Add(ACode: TdxBBCode;
      AConstructorFunc: TdxBBCodeRunConstructorFunc; AData: Pointer = nil);
    function CreateRun(const ATag: TdxBBCodeTag): TdxFormattedTextRun;
  end;

  { TdxBBCodeParser }

  TdxBBCodeParser = class
  strict private
    FBBCodeStack: TList<TdxBBCode>;
    FTags: TList<TdxBBCodeTag>;

    procedure AddTag(ABBCode: TdxBBCode; ABeginPosition, AEndPosition: PChar;
      AAction: TdxFormattedTextRunAction; const AArgument: string);
    function IsTagValid(AIndex: Integer): Boolean;
    function TextToBBCode(const AText: string; out ACode: TdxBBCode): Boolean;

    // Run Constructors
    class function CreateColorBasedRun(const ATag: TdxBBCodeTag;
      const AData: Pointer): TdxFormattedTextRun; static;
    class function CreateFontNameRun(const ATag: TdxBBCodeTag;
      const AData: Pointer): TdxFormattedTextRun; static;
    class function CreateFontSizeRun(const ATag: TdxBBCodeTag;
      const AData: Pointer): TdxFormattedTextRun; static;
    class function CreateHyperlinkRun(const ATag: TdxBBCodeTag;
      const AData: Pointer): TdxFormattedTextRun; static;
    class function CreateSimpleRun(const ATag: TdxBBCodeTag;
      const AData: Pointer): TdxFormattedTextRun; static;
  protected
    FHasAccelCharTag: Boolean;
    FRunConstructorFactory: TdxBBCodeRunFactory;
    FTextToBBCode: TDictionary<string, TdxBBCode>;

    procedure AddRun(ARuns: TdxFormattedTextRuns; ATagIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure PopulateTags(ABuffer: PChar; ABufferSize: Integer;
      AShowAccelChar: Boolean);
    procedure Process(AFormattedText: TdxFormattedText);
  end;

  { TdxFormattedTextConverterBBCode }

  TdxRunCodeStringDictionary = TDictionary<string, TdxBBCode>;

  TdxFormattedTextConverterBBCode = class(TdxFormattedTextConverter)
  public
    class procedure AddTextTagFromRun(ARunCodeString
      : TdxRunCodeStringDictionary; ARun: TdxFormattedTextRun;
      ABuffer: TStringBuilder);
    class procedure AddTextFromRun(ARunCodeString: TdxRunCodeStringDictionary;
      ARun: TdxFormattedTextRun; ABuffer: TStringBuilder);
    class procedure PopulateDictionary(ARunCodeString
      : TdxRunCodeStringDictionary);
    class function CanImport(const ASource: string): Boolean; override;
    class procedure Import(ATarget: TdxFormattedText; const ASource: string;
      ADefaultFont: TFont = nil); override;
    class function Export(ASource: TdxFormattedText; ADefaultFont: TFont = nil)
      : string; override;
  end;

function dxBBCodeToString(const Value: TdxBBCode): string;

implementation

uses
  dxCoreGraphics;

const
  BBCodeToText: array [TdxBBCode] of string = ('', 'B', 'I', 'U', 'S', 'COLOR',
    'URL', 'BACKCOLOR', 'SIZE', 'SUP', 'SUB', 'FONT', 'NOPARSE', 'IMG');

type
  TdxFormattedTextRunAccess = class(TdxFormattedTextRun);
  TdxFormattedTextAccess = class(TdxFormattedText);

function dxBBCodeToString(const Value: TdxBBCode): string;
begin
  Result := BBCodeToText[Value];
end;

{ TdxBBCodeStack }

constructor TdxBBCodeStack.Create;
begin
  inherited;
  FList := TStringList.Create;
end;

destructor TdxBBCodeStack.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TdxBBCodeStack.Exists(ACode: TdxBBCode): Boolean;
begin
  Result := FList.IndexOfObject(TObject(ACode)) >= 0;
end;

procedure TdxBBCodeStack.ExcludeCode(ACode: TdxBBCode);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I] = ACode then
    begin
      FList.Delete(I);
      Break;
    end;
end;

procedure TdxBBCodeStack.IncludeCode(ACode: TdxBBCode; const AValue: string);
begin
  FList.AddObject(AValue, TObject(ACode));
end;

function TdxBBCodeStack.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxBBCodeStack.GetItem(Index: Integer): TdxBBCode;
begin
  Result := TdxBBCode(FList.Objects[Index]);
end;

{ TdxBBCodeRunFactory }

procedure TdxBBCodeRunFactory.Add(ACode: TdxBBCode;
  AConstructorFunc: TdxBBCodeRunConstructorFunc; AData: Pointer);
begin
  FData[ACode] := TPair<TdxBBCodeRunConstructorFunc, Pointer>.Create
    (AConstructorFunc, AData);
end;

function TdxBBCodeRunFactory.CreateRun(const ATag: TdxBBCodeTag)
  : TdxFormattedTextRun;
begin
  Result := FData[ATag.BBCode].Key(ATag, FData[ATag.BBCode].Value);
end;

{ TdxBBCodeParser }

constructor TdxBBCodeParser.Create;
var
  ACode: TdxBBCode;
begin
  inherited Create;
  FTags := TList<TdxBBCodeTag>.Create;
  FBBCodeStack := TList<TdxBBCode>.Create;

  FRunConstructorFactory := TdxBBCodeRunFactory.Create;
  FRunConstructorFactory.Add(bbcBackgroundColor, CreateColorBasedRun,
    TdxFormattedTextBackgroundColorRun);
  FRunConstructorFactory.Add(bbcBold, CreateSimpleRun, TdxFormattedTextBoldRun);
  FRunConstructorFactory.Add(bbcColor, CreateColorBasedRun,
    TdxFormattedTextColorRun);
  FRunConstructorFactory.Add(bbcFont, CreateFontNameRun);
  FRunConstructorFactory.Add(bbcItalic, CreateSimpleRun,
    TdxFormattedTextItalicRun);
  FRunConstructorFactory.Add(bbcNoCode, CreateSimpleRun,
    TdxFormattedTextNoCodeRun);
  FRunConstructorFactory.Add(bbcNoParse, CreateSimpleRun,
    TdxFormattedTextNoParseRun);
  FRunConstructorFactory.Add(bbcSize, CreateFontSizeRun);
  FRunConstructorFactory.Add(bbcStrikeout, CreateSimpleRun,
    TdxFormattedTextStrikeoutRun);
  FRunConstructorFactory.Add(bbcSub, CreateSimpleRun, TdxFormattedTextSubRun);
  FRunConstructorFactory.Add(bbcSup, CreateSimpleRun, TdxFormattedTextSupRun);
  FRunConstructorFactory.Add(bbcUnderline, CreateSimpleRun,
    TdxFormattedTextUnderlineRun);
  FRunConstructorFactory.Add(bbcURL, CreateHyperlinkRun);
  FRunConstructorFactory.Add(bbcImg, CreateSimpleRun, TdxFormattedTextImgRun);

  FTextToBBCode := TDictionary<string, TdxBBCode>.Create
    (Ord(High(TdxBBCode)) + 1);
  for ACode := Low(ACode) to High(ACode) do
    FTextToBBCode.Add(BBCodeToText[ACode], ACode);
end;

destructor TdxBBCodeParser.Destroy;
begin
  FreeAndNil(FTextToBBCode);
  FreeAndNil(FRunConstructorFactory);
  FreeAndNil(FBBCodeStack);
  FreeAndNil(FTags);
  inherited;
end;

procedure TdxBBCodeParser.Process(AFormattedText: TdxFormattedText);
var
  I: Integer;
begin
  AFormattedText.Runs.Clear;
  PopulateTags(PChar(AFormattedText.Text), Length(AFormattedText.Text),
    AFormattedText.ShowAccelChar);
  TdxFormattedTextAccess(AFormattedText).FHasAccelCharRun := FHasAccelCharTag;
  for I := 0 to FTags.Count - 2 do
    AddRun(AFormattedText.Runs, I);
end;

procedure TdxBBCodeParser.AddRun(ARuns: TdxFormattedTextRuns;
  ATagIndex: Integer);
var
  ARun: TdxFormattedTextRunAccess;
  ATag: TdxBBCodeTag;
begin
  ATag := FTags[ATagIndex];
  ARun := TdxFormattedTextRunAccess(FRunConstructorFactory.CreateRun(ATag));
  ARun.FTextStart := ATag.EndPosition;
  ARun.FTextLength := dxGetStringLength(ATag.EndPosition,
    FTags[ATagIndex + 1].BeginPosition);
  ARuns.Add(ARun);
end;

procedure TdxBBCodeParser.AddTag(ABBCode: TdxBBCode;
  ABeginPosition, AEndPosition: PChar; AAction: TdxFormattedTextRunAction;
  const AArgument: string);
var
  ATag: TdxBBCodeTag;
begin
  ATag.BBCode := ABBCode;
  ATag.BeginPosition := ABeginPosition;
  ATag.EndPosition := AEndPosition;
  ATag.Action := AAction;
  ATag.Argument := AArgument;
  FTags.Add(ATag);
end;

function TdxBBCodeParser.IsTagValid(AIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := (FBBCodeStack.IndexOf(FTags[AIndex].BBCode) > -1) or
    (FTags[AIndex].Action <> traClose);
  if Result and (FTags[AIndex].Action = traOpen) then
  begin
    Result := False;
    for I := AIndex + 1 to FTags.Count - 1 do
      if (FTags[I].BBCode = FTags[AIndex].BBCode) and
        (FTags[I].Action = traClose) then
      begin
        Result := True;
        Break;
      end;
  end;
end;

function TdxBBCodeParser.TextToBBCode(const AText: string;
  out ACode: TdxBBCode): Boolean;
begin
  Result := FTextToBBCode.TryGetValue(UpperCase(AText), ACode);
end;

procedure TdxBBCodeParser.PopulateTags(ABuffer: PChar; ABufferSize: Integer;
  AShowAccelChar: Boolean);

  procedure MoveToNextSymbol;
  begin
    if ABufferSize > 0 then
    begin
      Dec(ABufferSize);
      Inc(ABuffer);
    end;
  end;

  function GetCodeAction(var ACode: TdxBBCode): TdxFormattedTextRunAction;
  const
    CodeMap: array [Boolean] of TdxFormattedTextRunAction = (traOpen, traClose);
  begin
    Result := CodeMap[ABuffer^ = '/'];
  end;

  function GetCode: TdxBBCode;
  var
    ATagStart: PChar;
  begin
    Result := bbcNoCode;
    ATagStart := ABuffer;
    while ABufferSize > 0 do
    begin
      if ((ABuffer^ = '=') or (ABuffer^ = ']')) and
        TextToBBCode(dxGetString(ATagStart, ABuffer), Result) then
        Break;
      MoveToNextSymbol;
    end;
  end;

  function GetArgument: string;
  var
    AArgumentStart: PChar;
  begin
    Result := '';
    AArgumentStart := ABuffer;
    while ABufferSize > 0 do
    begin
      if ABuffer^ = ']' then
      begin
        Result := dxGetString(AArgumentStart + 1, ABuffer);
        Break;
      end;
      MoveToNextSymbol;
    end;
  end;

  procedure ProcessNoParseTags(ACodeAction: TdxFormattedTextRunAction;
    var ATagIndex: Integer);
  begin
    if (ACodeAction = traOpen) and (ATagIndex = -1) then
      ATagIndex := FTags.Count - 1
    else if (ACodeAction = traClose) and (ATagIndex > -1) then
    begin
      FTags.DeleteRange(ATagIndex + 1, FTags.Count - ATagIndex - 2);
      ATagIndex := -1;
    end;
  end;

  procedure ProcessAccelChar(var APreviousAccelCharTagIndex: Integer;
    var AAccelCharProcessing: Boolean);
  var
    ABBCodeTag: TdxBBCodeTag;
  begin
    if not AAccelCharProcessing and (ABufferSize < 2) then
    begin
      AddTag(bbcNoCode, ABuffer, ABuffer + 1, traOpen, '');
      AddTag(bbcNoCode, ABuffer + 1, ABuffer + 1, traClose, '');
    end
    else
    begin
      if not AAccelCharProcessing then
        AddTag(bbcUnderline, ABuffer, ABuffer + 1, traOpen, '')
      else
        AddTag(bbcUnderline, ABuffer + 1, ABuffer + 1, traClose, '');

      if APreviousAccelCharTagIndex > -1 then
      begin
        ABBCodeTag := FTags[APreviousAccelCharTagIndex];
        ABBCodeTag.BBCode := bbcNoCode;
        FTags[APreviousAccelCharTagIndex] := ABBCodeTag;
      end;
      APreviousAccelCharTagIndex := FTags.Count - 1;
      AAccelCharProcessing := not AAccelCharProcessing;
    end;
  end;

var
  ACode: TdxBBCode;
  ABeginTagPosition: PChar;
  ABeginTagBufferSize: Integer;
  ACodeAction: TdxFormattedTextRunAction;
  AIndex: Integer;
  ANoParseTagIndex: Integer;
  AImgTagIndex: Integer;
  AArgument: string;
  AAccelCharProcessing: Boolean;
  APreviousAccelCharOpenTagIndex, APreviousAccelCharCloseTagIndex: Integer;
begin
  APreviousAccelCharOpenTagIndex := -1;
  APreviousAccelCharCloseTagIndex := -1;
  ANoParseTagIndex := -1;
  AAccelCharProcessing := False;
  AddTag(bbcNoCode, ABuffer, ABuffer, traOpen, '');
  while ABufferSize > 0 do
  begin
    if ABuffer^ = '[' then
    begin
      ABeginTagPosition := ABuffer;
      ABeginTagBufferSize := ABufferSize;
      MoveToNextSymbol;
      ACodeAction := GetCodeAction(ACode);
      if ACodeAction = traClose then
        MoveToNextSymbol;
      ACode := GetCode;
      AArgument := GetArgument;
      if ACode <> bbcNoCode then
      begin
        AddTag(ACode, ABeginTagPosition, ABuffer + 1, ACodeAction, AArgument);
        case ACode of
          bbcNoParse:
            ProcessNoParseTags(ACodeAction, ANoParseTagIndex);
          bbcImg:
            ProcessNoParseTags(ACodeAction, AImgTagIndex);
        end;
      end
      else
      begin
        ABuffer := ABeginTagPosition;
        ABufferSize := ABeginTagBufferSize;
      end;
    end
    else if AShowAccelChar and ((ABuffer^ = '&') or AAccelCharProcessing) then
    begin
      if ABuffer^ = '&' then
        ProcessAccelChar(APreviousAccelCharOpenTagIndex, AAccelCharProcessing)
      else if AAccelCharProcessing then
      begin
        ProcessAccelChar(APreviousAccelCharCloseTagIndex, AAccelCharProcessing);
        FHasAccelCharTag := True;
      end;
    end;
    MoveToNextSymbol;
  end;
  AddTag(bbcNoCode, ABuffer, ABuffer, traClose, '');

  AIndex := 0;
  while AIndex < FTags.Count do
  begin
    if not IsTagValid(AIndex) then
      FTags.Delete(AIndex)
    else
    begin
      case FTags[AIndex].Action of
        traOpen:
          FBBCodeStack.Add(FTags[AIndex].BBCode);
        traClose:
          FBBCodeStack.Extract(FTags[AIndex].BBCode);
      end;
      Inc(AIndex);
    end;
  end;

end;

class function TdxBBCodeParser.CreateColorBasedRun(const ATag: TdxBBCodeTag;
  const AData: Pointer): TdxFormattedTextRun;

  function DecodeColor(const AArgument: string): TColor;
  var
    AColor: TColor;
  begin
    Result := clDefault;
    if AArgument <> '' then
    begin
      if IdentToColor('cl' + AArgument, Integer(AColor)) then
        Result := AColor
      else if not TryStrToInt(AArgument, Integer(Result)) then
        Result := TdxAlphaColors.ToColor(TdxAlphaColors.FromHtml(AArgument));
    end;
  end;

begin
  Result := TdxFormattedTextColorRunClass(AData).Create(ATag.Action,
    DecodeColor(ATag.Argument));
end;

class function TdxBBCodeParser.CreateFontNameRun(const ATag: TdxBBCodeTag;
  const AData: Pointer): TdxFormattedTextRun;
begin
  Result := TdxFormattedTextFontRun.Create(ATag.Action, ATag.Argument);
end;

class function TdxBBCodeParser.CreateFontSizeRun(const ATag: TdxBBCodeTag;
  const AData: Pointer): TdxFormattedTextRun;
begin
  Result := TdxFormattedTextSizeRun.Create(ATag.Action,
    StrToIntDef(ATag.Argument, 0));
end;

class function TdxBBCodeParser.CreateHyperlinkRun(const ATag: TdxBBCodeTag;
  const AData: Pointer): TdxFormattedTextRun;
begin
  Result := TdxFormattedTextURLRun.Create(ATag.Action, ATag.Argument);
end;

class function TdxBBCodeParser.CreateSimpleRun(const ATag: TdxBBCodeTag;
  const AData: Pointer): TdxFormattedTextRun;
begin
  Result := TdxFormattedTextRunClass(AData).Create(ATag.Action);
end;

{ TdxFormattedTextConverterBBCode }

class procedure TdxFormattedTextConverterBBCode.AddTextTagFromRun
  (ARunCodeString: TdxRunCodeStringDictionary; ARun: TdxFormattedTextRun;
  ABuffer: TStringBuilder);
var
  ABBCode: TdxBBCode;
begin
  ABBCode := ARunCodeString[ARun.ClassName];
  if ABBCode <> bbcNoCode then
  begin
    ABuffer.Append('[');
    if ARun.Action = traClose then
      ABuffer.Append('/');
    ABuffer.Append(dxBBCodeToString(ABBCode));
    if (ARun.Action = traOpen) and
      (ABBCode in [bbcColor, bbcBackgroundColor, bbcURL, bbcSize, bbcFont]) then
    begin
      ABuffer.Append('=');
      case ABBCode of
        bbcColor, bbcBackgroundColor:
          ABuffer.Append(Integer(TdxFormattedTextColorRun(ARun).Color));
        bbcURL:
          ABuffer.Append(TdxFormattedTextURLRun(ARun).Hyperlink);
        bbcSize:
          ABuffer.Append(TdxFormattedTextSizeRun(ARun).Size);
      else // bbcFont
        ABuffer.Append(TdxFormattedTextFontRun(ARun).FontName);
      end;
    end;
    ABuffer.Append(']');
  end;
end;

class procedure TdxFormattedTextConverterBBCode.AddTextFromRun
  (ARunCodeString: TdxRunCodeStringDictionary; ARun: TdxFormattedTextRun;
  ABuffer: TStringBuilder);
begin
  AddTextTagFromRun(ARunCodeString, ARun, ABuffer);
  ABuffer.Append(dxGetString(ARun.TextStart, ARun.TextLength));
end;

class procedure TdxFormattedTextConverterBBCode.PopulateDictionary
  (ARunCodeString: TdxRunCodeStringDictionary);
begin
  ARunCodeString.Add('TdxFormattedTextNoCodeRun', bbcNoCode);
  ARunCodeString.Add('TdxFormattedTextNoParseRun', bbcNoParse);
  ARunCodeString.Add('TdxFormattedTextBoldRun', bbcBold);
  ARunCodeString.Add('TdxFormattedTextItalicRun', bbcItalic);
  ARunCodeString.Add('TdxFormattedTextUnderlineRun', bbcUnderline);
  ARunCodeString.Add('TdxFormattedTextStrikeoutRun', bbcStrikeout);
  ARunCodeString.Add('TdxFormattedTextColorRun', bbcColor);
  ARunCodeString.Add('TdxFormattedTextURLRun', bbcURL);
  ARunCodeString.Add('TdxFormattedTextBackgroundColorRun', bbcBackgroundColor);
  ARunCodeString.Add('TdxFormattedTextSizeRun', bbcSize);
  ARunCodeString.Add('TdxFormattedTextSupRun', bbcSup);
  ARunCodeString.Add('TdxFormattedTextSubRun', bbcSub);
  ARunCodeString.Add('TdxFormattedTextFontRun', bbcFont);
end;

class function TdxFormattedTextConverterBBCode.CanImport(const ASource
  : string): Boolean;
begin
  Result := True;
end;

class procedure TdxFormattedTextConverterBBCode.Import
  (ATarget: TdxFormattedText; const ASource: string; ADefaultFont: TFont);
var
  AParser: TdxBBCodeParser;
begin
  AParser := TdxBBCodeParser.Create;
  try
    ATarget.Text := ASource;
    AParser.Process(ATarget);
    AdjustInternalRuns(ATarget, ADefaultFont);
  finally
    AParser.Free;
  end;
end;

class function TdxFormattedTextConverterBBCode.Export(ASource: TdxFormattedText;
  ADefaultFont: TFont): string;
var
  ARunCodeString: TDictionary<string, TdxBBCode>;
  I: Integer;
  ABuffer: TStringBuilder;
begin
  ABuffer := TStringBuilder.Create(2 * Length(ASource.Runs[0].Text));
  try
    ARunCodeString := TdxRunCodeStringDictionary.Create;
    try
      PopulateDictionary(ARunCodeString);
      for I := 0 to ASource.Runs.Count - 1 do
        AddTextFromRun(ARunCodeString, ASource.Runs[I], ABuffer);
    finally
      ARunCodeString.Free;
    end;
    Result := ABuffer.ToString;
  finally
    ABuffer.Free;
  end;
end;

initialization

TdxFormattedTextConverters.Register(TdxFormattedTextConverterBBCode);

finalization

TdxFormattedTextConverters.Unregister(TdxFormattedTextConverterBBCode);

end.
