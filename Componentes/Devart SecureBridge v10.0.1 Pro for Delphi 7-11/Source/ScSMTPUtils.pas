
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSMTPUtils;

interface

uses
  Classes, SysUtils, StrUtils,
  ScTypes, ScCLRClasses, ScFunctions, ScUtils, ScConsts,
  ScCoders;

type
  TScAlternateViewCollection = class;
  TScAttachmentCollection = class;
  TScLinkedResourceCollection = class;
  TScHeaderList = class;

  TScCustomAttachment = class(TCollectionItem)
  protected
    FContentDescription: string;
    FContentID: string;
    FContentTransferEncoding: string;
    FContentType: string;
    FSpecialHeaders: TScHeaderList;
    FParentIndex: integer;

    function GetCharset: string;
    procedure SetCharset(const Value: string);
    procedure SetContentType(const Value: string);
    function GetContentName: string;
    procedure SetContentName(const Value: string);
    procedure SetSpecialHeaders(Value: TScHeaderList);

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property ParentIndex: integer read FParentIndex write FParentIndex;
  published
    property ContentCharset: string read GetCharset write SetCharset;
    property ContentDescription: string read FContentDescription write FContentDescription;
    property ContentID: string read FContentID write FContentID;
    property ContentTransferEncoding: string read FContentTransferEncoding write FContentTransferEncoding;
    property ContentType: string read FContentType write SetContentType;
    property ContentName: string read GetContentName write SetContentName;

    property SpecialHeaders: TScHeaderList read FSpecialHeaders write SetSpecialHeaders;
  end;

  TScAlternateView = class(TScCustomAttachment)
  protected
    FBody: TStrings;
    FLinkedResources: TScLinkedResourceCollection;

    procedure SetBody(Value: TStrings);

  public
    constructor Create(Collection: TScAlternateViewCollection; ABody: TStrings = nil); reintroduce;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

  published
    property Body: TStrings read FBody write SetBody;
    property LinkedResources: TScLinkedResourceCollection read FLinkedResources;
  end;

  TScTextAttachment = TScAlternateView;

  TScDataAttachment = class(TScCustomAttachment)
  protected
    FStoredInMemory: boolean;
    FFileIsTemp: boolean;
    FPathName: string;
    FTempDirectory: string;

    FDataStream: TStream;
    FBeforeOpenPosition: NativeInt;

    procedure SetFileName(const Value: string); virtual;
    procedure SetTempDirectory(const Value: string);
    procedure SetPathName(const Value: string);

  public
    constructor Create(Collection: TOwnedCollection; const AFileName: string); reintroduce; overload;
    constructor Create(Collection: TOwnedCollection; CopyFrom: TStream); reintroduce; overload;
    destructor Destroy; override;

    procedure Init(const Str: string);
    function ToString: string; {$IFDEF VER12P} override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}

    function OpenStream(ForWriting: boolean = False): TStream;
    procedure CloseStream;

    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);

  published
    property PathName: string read FPathName write SetPathName;
    property TempDirectory: string read FTempDirectory write SetTempDirectory;
  end;

  TScAttachment = class(TScDataAttachment)
  private
    FContentDisposition: string;

    procedure SetContentDisposition(const Value: string);
    function GetFileName: string;

  protected
    procedure SetFileName(const Value: string); override;

  public
    constructor Create(Collection: TScAttachmentCollection; const AFileName: string); reintroduce; overload;
    constructor Create(Collection: TScAttachmentCollection; CopyFrom: TStream); reintroduce; overload;
    procedure Assign(Source: TPersistent); override;

  published
    property ContentDisposition: string read FContentDisposition write SetContentDisposition;
    property FileName: string read GetFileName write SetFileName;
  end;

  TScLinkedResource = class(TScDataAttachment)
  public
    constructor Create(Collection: TScLinkedResourceCollection; const AFileName: string); reintroduce; overload;
    constructor Create(Collection: TScLinkedResourceCollection; CopyFrom: TStream); reintroduce; overload;
  end;

  TScAlternateViewCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: integer): TScAlternateView;
    procedure SetItem(Index: integer; Value: TScAlternateView);

  public
    constructor Create(AOwner: TPersistent); reintroduce;

    property Items[Index: integer]: TScAlternateView read GetItem write SetItem; default;
  end;

  TScAttachmentCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: integer): TScAttachment;
    procedure SetItem(Index: integer; Value: TScAttachment);

  public
    constructor Create(AOwner: TPersistent); reintroduce;

    property Items[Index: integer]: TScAttachment read GetItem write SetItem; default;
  end;

  TScLinkedResourceCollection = class(TOwnedCollection)
  protected
    function GetItem(Index: integer): TScLinkedResource;
    procedure SetItem(Index: integer; Value: TScLinkedResource);

  public
    constructor Create(AOwner: TPersistent); reintroduce;

    property Items[Index: integer]: TScLinkedResource read GetItem write SetItem; default;
  end;

  TScHeaderList = class(TStrValueStringList)
  private
    SB: StringBuilder;

    function GetString(Index: integer): string;

    function GetText: string;
    procedure SetText(const Value: string);

    function SaveToString(const Separator: string): string;
  protected
    procedure SetName(const Key: string; const Value: string); override;
  public
    constructor Create;
    destructor Destroy; override;

    function ToString: string; {$IFDEF VER12P}override;{$ELSE}{$IFDEF FPC}reintroduce;{$ENDIF}virtual;{$ENDIF}

    property Strings[Index: Integer]: string read GetString; default;
    property Text: string read GetText write SetText;
  end;

type
  TScHeaderQuotingType = (qtPlain, qtRFC822, qtMIME, qtHTTP);

function MediaTypeMatches(const Value, MediaType: string): boolean;
function ParseHeaderItem(const Header: string): string;
function ReplaceHeaderParam(Header: string; const ParamName, ParamValue: string;
  out OldValue: string; QuotingType: TScHeaderQuotingType = qtMIME): string;
function ParseHeaderParam(const Header: string; const SubItem: string;
  QuotingType: TScHeaderQuotingType = qtMIME): string;
function EncodeHeader(const Header, Specials: string; const HeaderEncoding: char;
  const Charset: string): string;
function DecodeHeader(const Header: string): string;

implementation

uses
  ScMailMessage;

const
  HeaderSeparator = ': ';

  SPECIALS_SET: array[TScHeaderQuotingType] of string = (
    {Plain} '',
    {Inet } '()<>@,;:\"./', {Internet Text Message}
    {MIME } '()<>@,;:\"/[]?=',
    {HTTP } '()<>@,;:\"/[]?={} '#9
  );

function FindMIMEType(const FileName: string): string;
var
  Ext: string;
  i: integer;
begin
  Ext := LowerCase(ExtractFileExt(FileName));

  for i := Low(MIME_TYPES) to High(MIME_TYPES) do begin
    if MIME_TYPES[i][0] = Ext then begin
      Result := MIME_TYPES[i][1];
      Exit;
    end;
  end;

  Result := 'application/octet-stream';
end;

function MediaTypeMatches(const Value, MediaType: string): boolean;
begin
  if Pos('/', MediaType) > 0 then
    Result := AnsiSameText(Value, MediaType)
  else begin
    Result := Length(Value) >= Length(MediaType) + 1{'/'};
    if Result then
      Result := AnsiSameText(Copy(Value, 1, Length(MediaType) + 1{'/'}), MediaType + '/');
  end;
end;

function ParseHeaderItem(const Header: string): string;
begin
  Result := Trim(GetFirstWord(Header, ';'));
end;

function ReplaceHeaderParam(Header: string; const ParamName, ParamValue: string;
  out OldValue: string; QuotingType: TScHeaderQuotingType = qtMIME): string;

  function QuoteIfNeed(const Str, Specials: string): string;
  var
    NeedQuote: boolean;
    Escapes: string;
    i, j, Offset: integer;
  begin
    if (Str = '') or ((Length(Str) >= 2) and (Str[1] = '"') and (Str[Length(Str)] = '"')) then begin
      Result := Str;
      Exit;
    end;

    if QuotingType in [qtRFC822, qtMIME] then
      Escapes := '"\'#13
    else
      Escapes := '"\';

    SetLength(Result, Length(Str) * 2);
    Offset := 0;
    NeedQuote := False;

    for i := 1 to Length(Str) do begin
      for j := 1 to Length(Escapes) do begin
        if Str[i] = Escapes[j] then begin
          NeedQuote := True;
          Inc(Offset);
          Result[Offset] := '\';
          break;
        end;
      end;

      if not NeedQuote then begin
        for j := 1 to Length(Specials) do begin
          if Str[i] = Specials[j] then begin
            NeedQuote := True;
            break;
          end;
        end;
      end;

      Inc(Offset);
      Result[Offset] := Str[I];
    end;

    SetLength(Result, Offset);
    if NeedQuote then
      Result := '"' + Result + '"';
  end;

var
  Name, Value, Specials: string;
  IsQuoted, WasFound: boolean;
  i: integer;
begin
  if (Header = '') or (ParamName = '') then begin
    OldValue := '';
    Result := Header;
    Exit;
  end;

  // Ex: Content-type: text/plain; charset=us-ascii
  Result := ExtractFirstWord(Header, ';');

  SetLength(Specials, 33);
  for i := 0 to 32 do
    Specials[i + 1] := Chr(i);

  Specials := Specials + SPECIALS_SET[QuotingType] + #127;
  WasFound := False;

  while Header <> '' do begin
    Header := TrimLeft(Header);
    if Header = '' then
      break;

    Name := Trim(ExtractFirstWord(Header, '='));
    Header := TrimLeft(Header);
    if Header = '' then
      break;

    IsQuoted := Header[1] = '"';

    if IsQuoted then begin
      Value := '';

      i := 2;
      while i <= Length(Header) do begin
        if Header[i] = '\' then
          Inc(i)
        else
        if Header[i] = '"' then begin
          Value := Copy(Header, 1, i);
          Header := Copy(Header, i + 1, MaxInt);
          break;
        end;

        Inc(i);
      end;

      ExtractFirstWord(Header, ';');
    end
    else begin
      i := PosOfAnyFromSet(Specials, Header);
      if i <> 0 then begin
        Value := Copy(Header, 1, i - 1);
        if Header[i] = ';' then
          Inc(i);
        Delete(Header, 1, i - 1);
      end
      else begin
        Value := Header;
        Header := '';
      end;
    end;

    if (Name <> '') and (Value <> '') then begin
      if (ParamName <> '') and AnsiSameText(Name, ParamName) then begin
        WasFound := True;
        if ParamValue <> '' then
          Result := Result + '; ' + Name + '=' + QuoteIfNeed(ParamValue, Specials);
        // else - delete param
        OldValue := Value;
      end
      else
        Result := Result + '; ' + Name + '=' + Value;
    end;
  end;

  if not WasFound then begin
    if (ParamName <> '') and (ParamValue <> '') then
      Result := Result + '; ' + ParamName + '=' + QuoteIfNeed(ParamValue, Specials);
    OldValue := '';
  end;
end;

function ParseHeaderParam(const Header: string; const SubItem: string;
  QuotingType: TScHeaderQuotingType = qtMIME): string;
begin
  ReplaceHeaderParam(Header, SubItem, '', Result, QuotingType);
end;

function EncodeHeader(const Header, Specials: string; const HeaderEncoding: char;
  const Charset: string): string;
const
  SPACES = [Ord(' '), 9, 13, 10];
var
  Coder: TScCoder;
  BeginEncode, EndEncode: string;
  SpecialsChars: TBytes;
  Data: TBytes;
  EncStr: string;

  procedure EncodeData(StartPos, EndPos: integer);
  const
    BLOCK_LEN = 75;
  var
    TmpBuf: TBytes;
    BufLen: integer;
  begin
    SetLength(TmpBuf, 0);
    EncStr := EncStr + BeginEncode;
    if EndPos > Length(Data) then
      EndPos := Length(Data) + 1;

    if HeaderEncoding = 'Q' then begin // Quoted-printable
      TmpBuf := Coder.Encode(Data, StartPos, EndPos - StartPos);
      EncStr := EncStr + Encoding.ASCII.GetString(TmpBuf);
    end
    else begin
    // if HeaderEncoding = 'B' then // Base64
      BufLen := ((BLOCK_LEN - (Length(BeginEncode) + Length(EndEncode))) div 4) * 3;
      while StartPos < EndPos do begin
        if BufLen > EndPos - StartPos then
          BufLen := EndPos - StartPos;
        TmpBuf := Coder.Encode(Data, StartPos, BufLen);
        EncStr := EncStr + Encoding.ASCII.GetString(TmpBuf);
        Inc(StartPos, BufLen);
      end;
    end;

    EncStr := EncStr + EndEncode;
  end;

var
  NoEncodeChars: TBytes;
  NeedEncode: boolean;
  P, SpacePos, DataPos, EncodePos: integer;
  DataLen: integer;
  i: integer;
begin
  if Header = '' then begin
    Result := '';
    Exit;
  end;

  SetLength(SpecialsChars, 0);
  Data := EncodingByCharset(Charset).GetBytes(Header);
  SpecialsChars := Encoding.ASCII.GetBytes(Specials);
  BeginEncode := '=?' + Charset + '?' + HeaderEncoding + '?';
  EndEncode := '?=';

  if HeaderEncoding = '8' then begin
    Result := Encoding.ASCII.GetString(Data);
    Exit;
  end
  else
  if HeaderEncoding = 'Q' then begin
    Coder := TScCoderQuotedPrintableSpecial.Create;
    TScCoderQuotedPrintableSpecial(Coder).Init(Specials + '=?_', BeginEncode, EndEncode);
  end
  else
  // if HeaderEncoding = 'B' then
    Coder := TScBase64Coder.Create;

  SetLength(NoEncodeChars, 95);
  for i := 0 to 94 do
    NoEncodeChars[i] := i + 32; // 32..126

  if DecodeHeader(Header) <> Header then begin
    // remove '='
    P := Ord('=') - 32; // posintion of '=' in NoEncodeChars
    Move(NoEncodeChars[P + 1], NoEncodeChars[P], Length(NoEncodeChars) - 2);
    SetLength(NoEncodeChars, Length(NoEncodeChars) - 1);
  end;

  try
    DataLen := Length(Data);
    P := 0;
    EncStr := '';
    EncodePos := -1;
    while P < DataLen do begin
      SpacePos := P;
      while (P < DataLen) and (Data[P] in SPACES) do
        Inc(P);

      DataPos := P;
      NeedEncode := False;
      while (P < DataLen) and not (Data[P] in SPACES) do begin
        if not ByteInSet(Data[P], NoEncodeChars) or ByteInSet(Data[P], SpecialsChars) then
          NeedEncode := True;

        Inc(P);
      end;

      if NeedEncode then begin
        if EncodePos = -1 then begin
          EncStr := EncStr + Encoding.Default.GetString(Data, SpacePos, DataPos - SpacePos);
          EncodePos := DataPos;
        end;
      end
      else begin
        if EncodePos <> -1 then begin
          EncodeData(EncodePos, SpacePos);
          EncodePos := -1;
        end;

        EncStr := EncStr + Encoding.Default.GetString(Data, SpacePos, P - SpacePos);
      end;
    end;

    if EncodePos <> -1 then
      EncodeData(EncodePos, P);
  finally
    Coder.Free;
  end;

  Result := EncStr;
end;

function DecodeHeader(const Header: string): string;

  function ParseEncoding(const Header: string; const StartPos: integer;
    var LexemStartPos, LexemEndPos: integer;
    var CharsetStr, EncodingStr, DataStr: string): boolean;
  var
    CharsetPos, EncodingPos, DataStartPos, DataEndPos: integer;
  begin
    Result := False;

    // '=?ISO-8859-1?Q?=E4?='
    CharsetPos := PosEx('=?', Header, StartPos);
    if (CharsetPos = 0) or (CharsetPos > LexemEndPos) then
      Exit;
    Inc(CharsetPos, 2);

    EncodingPos := PosEx('?', Header, CharsetPos);
    if (EncodingPos = 0) or (EncodingPos > LexemEndPos) then
      Exit;
    Inc(EncodingPos);

    DataStartPos := PosEx('?', Header, EncodingPos);
    if (DataStartPos = 0) or (DataStartPos > LexemEndPos) then
      Exit;
    Inc(DataStartPos);

    DataEndPos := PosEx('?=', Header, DataStartPos);
    if (DataEndPos = 0) or (DataEndPos > LexemEndPos) then
      Exit;
    Inc(DataEndPos);

    LexemStartPos := CharsetPos - 2;
    LexemEndPos := DataEndPos;
    CharsetStr := Copy(Header, CharsetPos, EncodingPos - CharsetPos - 1);
    EncodingStr := Copy(Header, EncodingPos, DataStartPos - EncodingPos - 1);
    DataStr := Copy(Header, DataStartPos, DataEndPos - DataStartPos - 1);
    Result := True;
  end;

var
  Coder: TScCoder;
  Charset, EncodingStr, DataStr, Str: string;
  Buf: TBytes;
  StartPos, PrevStartPos, SpacePos, EncodePos: integer;
  WasEncoded, WasDecoded: boolean;
  DataLen: integer;
begin
  Result := Header;
  DataLen := Length(Result);
  SetLength(Buf, 0);

  StartPos := 1;
  PrevStartPos := StartPos;
  WasEncoded := False;

  Coder := nil;
  try
    while StartPos <= DataLen do begin
      // Exit, if only spaces
      StartPos := PosOfAnyExceptSet(LWS, Result, StartPos);
      if StartPos = 0 then
        Break;

      SpacePos := PosOfAnyFromSet(LWS, Result, StartPos);
      if SpacePos <> 0 then
        Dec(SpacePos)
      else
        SpacePos := DataLen;

      if ParseEncoding(Result, StartPos, EncodePos, SpacePos, Charset, EncodingStr, DataStr) then begin
        if EncodingStr = 'B' then begin
          if not (Coder is TScBase64Coder) then begin
            FreeAndNil(Coder);
            Coder := TScBase64Coder.Create;
          end;

          Buf := Coder.Decode(Encoding.ASCII.GetBytes(DataStr));
          WasDecoded := True;
        end
        else
        if EncodingStr = 'Q' then begin
          if not (Coder is TScCoderQuotedPrintableSpecial) then begin
            FreeAndNil(Coder);
            Coder := TScCoderQuotedPrintableSpecial.Create;
          end;

          Buf := Coder.Decode(Encoding.ASCII.GetBytes(DataStr));
          WasDecoded := True;
        end
        else
        if EncodingStr = '8' then begin
          Buf := Encoding.ASCII.GetBytes(DataStr);
          WasDecoded := True;
        end
        else
          WasDecoded := False;

        if WasDecoded then begin
          Str := EncodingByCharset(Charset).GetString(Buf);

          if WasEncoded then begin
            Result := Copy(Result, 1, PrevStartPos - 1) + Str + Copy(Result, SpacePos + 1, MaxInt);
            StartPos := PrevStartPos + Length(Str);
          end
          else begin
            Result := Copy(Result, 1, EncodePos - 1) + Str + Copy(Result, SpacePos + 1, MaxInt);
            StartPos := EncodePos + Length(Str);
          end;
          DataLen := Length(Result);
        end
        else
          StartPos := SpacePos + 1;

        WasEncoded := True;
        PrevStartPos := StartPos;
      end
      else begin
        StartPos := PosOfAnyFromSet(LWS, Result, StartPos);
        if StartPos = 0 then
          Break;

        WasEncoded := False;
      end;
    end;
  finally
    Coder.Free;
  end;
end;

{ TScCustomAttachment }

constructor TScCustomAttachment.Create(Collection: TCollection);
begin
  inherited;

  FSpecialHeaders := TScHeaderList.Create;
  FParentIndex := -1;
end;

destructor TScCustomAttachment.Destroy;
begin
  FSpecialHeaders.Free;

  inherited;
end;

procedure TScCustomAttachment.Assign(Source: TPersistent);
var
  Src: TScCustomAttachment;
begin
  if Source is TScCustomAttachment then begin
    Src := TScCustomAttachment(Source);

    FContentDescription := Src.FContentDescription;
    FContentID := Src.FContentID;
    FContentTransferEncoding := Src.FContentTransferEncoding;
    FContentType := Src.FContentType;

    FSpecialHeaders.Assign(Src.FSpecialHeaders);
  end
  else
    inherited Assign(Source);
end;

function TScCustomAttachment.GetCharset: string;
begin
  Result := ParseHeaderParam(FContentType, 'charset');
end;

procedure TScCustomAttachment.SetCharset(const Value: string);
var
  Tmp: string;
begin
  if (FContentType = '') and (Value = '') then
    Exit;

  if FContentType = '' then
    FContentType := 'text/plain';

  FContentType := ReplaceHeaderParam(FContentType, 'charset', Value, Tmp, qtMIME);
end;

procedure TScCustomAttachment.SetContentType(const Value: string);
var
  OldCharset, OldName: string;
  NewCharset, NewName: string;
  Tmp: string;
begin
  OldCharset := ParseHeaderParam(FContentType, 'charset');
  OldName := ParseHeaderParam(FContentType, 'name');

  NewCharset := ParseHeaderParam(Value, 'charset');
  NewName := ParseHeaderParam(Value, 'name');

  FContentType := Value;
  if (NewCharset = '') and (OldCharset <> '') then
    FContentType := ReplaceHeaderParam(FContentType, 'charset', OldCharset, Tmp, qtMIME);

  if (NewName = '') and (OldName <> '') then
    FContentType := ReplaceHeaderParam(FContentType, 'name', OldName, Tmp, qtMIME);
end;

function TScCustomAttachment.GetContentName: string;
begin
  Result := ParseHeaderParam(FContentType, 'name');
end;

procedure TScCustomAttachment.SetContentName(const Value: string);
var
  Tmp: string;
begin
  if (FContentType = '') and (Value = '') then
    Exit;

  if FContentType = '' then
    FContentType := 'text/plain';

  FContentType := ReplaceHeaderParam(FContentType, 'name', Value, Tmp, qtMIME);
end;

procedure TScCustomAttachment.SetSpecialHeaders(Value: TScHeaderList);
begin
  FSpecialHeaders.Assign(Value);
end;

{ TScAlternateView }

constructor TScAlternateView.Create(Collection: TScAlternateViewCollection; ABody: TStrings = nil);
const
  HTML_LINE1 = '<!DOCTYPE html';
  HTML_LINE2 = '<html';
var
  s: string;
  i: integer;
begin
  inherited Create(Collection);

  FBody := TStringList.Create;
  TStringList(FBody).Duplicates := dupAccept;
  if Assigned(ABody) then
    FBody.Assign(ABody);

  for i := 0 to FBody.Count - 1 do begin
    s := Trim(FBody.Strings[i]);
    if s <> '' then begin
      if (Copy(s, 1, Length(HTML_LINE1)) = HTML_LINE1) or (Copy(s, 1, Length(HTML_LINE2)) = HTML_LINE2) then
        SetContentType('text/html');

      Break;
    end;
  end;

  FLinkedResources := TScLinkedResourceCollection.Create(Self);
end;

destructor TScAlternateView.Destroy;
begin
  FBody.Free;
  FLinkedResources.Free;

  inherited;
end;

procedure TScAlternateView.Assign(Source: TPersistent);
begin
  if Source is TScAlternateView then
    FBody.Assign(TScAlternateView(Source).FBody);

  inherited Assign(Source);
end;

procedure TScAlternateView.SetBody(Value: TStrings);
begin
  FBody.Assign(Value);
end;

{ TScDataAttachment }

constructor TScDataAttachment.Create(Collection: TOwnedCollection; const AFileName: string);
var
  fn: string;
begin
  inherited Create(Collection);

  FStoredInMemory := False;
  FPathName := AFileName;
  FFileIsTemp := False;

  fn := ExtractFileName(AFileName);
  if fn <> '' then begin
    SetFileName(fn);
    SetContentType(FindMIMEType(fn));
  end;
end;

constructor TScDataAttachment.Create(Collection: TOwnedCollection; CopyFrom: TStream);
var
  StreamSize: NativeInt;
begin
  inherited Create(Collection);

  FStoredInMemory := True;

  if Assigned(CopyFrom) then begin
    FDataStream := TMemoryStream.Create;
    StreamSize := CopyFrom.Size - CopyFrom.Position;
    if StreamSize > 0 then
      FDataStream.CopyFrom(CopyFrom, StreamSize);
  end;
end;

destructor TScDataAttachment.Destroy;
begin
  if FFileIsTemp and (FPathName <> '') then
    SysUtils.DeleteFile(FPathName);

  FDataStream.Free;

  inherited;
end;

procedure TScDataAttachment.Init(const Str: string);
var
  TempStream: TStream;
  Buffer: TBytes;
begin
  TempStream := OpenStream(True);
  try
    if Str <> '' then begin
      SetLength(Buffer, 0);
      Buffer := Encoding.Default.GetBytes(Str);
      TempStream.Write(Buffer[0], Length(Buffer));
    end;
  finally
    CloseStream;
  end;
end;

function TScDataAttachment.ToString: string;
var
  TempStream: TStream;
  Buffer: TBytes;
  BufSize: integer;
begin
  if not FStoredInMemory and (FPathName = '') then begin
    Result := '';
    Exit;
  end;

  TempStream := OpenStream;
  try
    if TempStream.Size = 0 then begin
      Result := '';
      Exit;
    end;

    SetLength(Buffer, TempStream.Size);
    BufSize := TempStream.Read(Buffer[0], Length(Buffer));
    Result := Encoding.Default.GetString(Buffer, 0, BufSize);
  finally
    CloseStream;
  end;
end;

function TScDataAttachment.OpenStream(ForWriting: boolean = False): TStream;
begin
  if FStoredInMemory then begin
    if FDataStream = nil then
      FDataStream := TMemoryStream.Create;

    if ForWriting then begin
      FBeforeOpenPosition := 0;
      FDataStream.Size := 0;
    end
    else begin
      FBeforeOpenPosition := FDataStream.Position;
      FDataStream.Position := 0;
    end;

    Result := FDataStream;
  end
  else begin
    FreeAndNil(FDataStream);

    if ForWriting then begin
      if FPathName = '' then begin
        FPathName := GetUniqueFileName(FTempDirectory);
        FFileIsTemp := True;
      end;

      FDataStream := TFileStream.Create(FPathName, fmCreate or fmOpenReadWrite or fmShareDenyWrite);
    end
    else
      FDataStream := TFileStream.Create(FPathName, fmOpenRead or fmShareDenyWrite);

    Result := FDataStream;
  end;
end;

procedure TScDataAttachment.CloseStream;
begin
  if FStoredInMemory then
    FDataStream.Position := FBeforeOpenPosition
  else
    FreeAndNil(FDataStream);
end;

procedure TScDataAttachment.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  if FileName = FPathName then
    Exit;

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TScDataAttachment.LoadFromStream(Stream: TStream);
var
  TempStream: TStream;
begin
  TempStream := OpenStream(True);
  try
    TempStream.CopyFrom(Stream, 0);
  finally
    CloseStream;
  end;
end;

procedure TScDataAttachment.SaveToFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmOpenReadWrite or fmShareDenyWrite);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TScDataAttachment.SaveToStream(Stream: TStream);
var
  TempStream: TStream;
begin
  TempStream := OpenStream;
  try
    Stream.CopyFrom(TempStream, 0);
  finally
    CloseStream;
  end;
end;

procedure TScDataAttachment.SetFileName(const Value: string);
begin
  // none
end;

procedure TScDataAttachment.SetTempDirectory(const Value: string);
begin
  if Value <> FTempDirectory then
    FTempDirectory := ExcludeTrailingBackslash(Value);
end;

procedure TScDataAttachment.SetPathName(const Value: string);
begin
  if FPathName <> Value then begin
    if FFileIsTemp and (FPathName <> '') then
      SysUtils.DeleteFile(FPathName);

    FPathName := Value;
    FFileIsTemp := False;

    if FPathName <> '' then begin
      FreeAndNil(FDataStream);
      FStoredInMemory := False;
    end
    else
      FStoredInMemory := True;
  end;
end;

{ TScAttachment }

constructor TScAttachment.Create(Collection: TScAttachmentCollection; const AFileName: string);
begin
  inherited Create(Collection, AFileName);
end;

constructor TScAttachment.Create(Collection: TScAttachmentCollection; CopyFrom: TStream);
begin
  inherited Create(Collection, CopyFrom);
end;

procedure TScAttachment.Assign(Source: TPersistent);
begin
  if Source is TScAttachment then begin
    FContentDisposition := TScAttachment(Source).FContentDisposition;
  end;

  inherited Assign(Source);
end;

procedure TScAttachment.SetContentDisposition(const Value: string);
var
  OldFileName, NewFileName: string;
  Tmp: string;
begin
  OldFileName := ParseHeaderParam(FContentDisposition, 'filename');
  NewFileName := ParseHeaderParam(Value, 'filename');

  FContentDisposition := Value;
  if (NewFileName = '') and (OldFileName <> '') then begin
    NewFileName := OldFileName;
    FContentDisposition := ReplaceHeaderParam(FContentDisposition, 'filename', NewFileName, Tmp, qtMIME);
  end;
end;

function TScAttachment.GetFileName: string;
begin
  Result := ParseHeaderParam(FContentDisposition, 'filename');
  Result := DecodeHeader(Result);
end;

procedure TScAttachment.SetFileName(const Value: string);
var
  FilenameStr, Tmp: string;
  MailMessage: TScMailMessage;
  HeadersCharset: string;
  HeadersTransferEncoding: char;
begin
  if (FContentDisposition = '') and (Value = '') then
    Exit;

  if FContentDisposition = '' then
    FContentDisposition := 'attachment';

  FilenameStr := Value;
  if FilenameStr <> '' then begin
    MailMessage := nil;
    if Collection is TScAttachmentCollection then begin
      if TScAttachmentCollection(Collection).Owner is TScMailMessage then
        MailMessage := TScMailMessage(TScAttachmentCollection(Collection).Owner);
    end;

    if MailMessage <> nil then begin
      HeadersCharset := MailMessage.HeadersCharset;
      HeadersTransferEncoding := MailMessage.HeadersTransferEncoding;
    end
    else begin
      HeadersCharset := '';
      HeadersTransferEncoding := 'B';
    end;

    if HeadersCharset = '' then
      HeadersCharset := 'us-ascii';
    if HeadersTransferEncoding = '' then
      HeadersTransferEncoding := 'B';

    FilenameStr := EncodeHeader(ExtractFileName(FilenameStr), '', HeadersTransferEncoding, HeadersCharset);
  end;

  FContentDisposition := ReplaceHeaderParam(FContentDisposition, 'filename', FilenameStr, Tmp, qtMIME);
end;

{ TScLinkedResource }

constructor TScLinkedResource.Create(Collection: TScLinkedResourceCollection; const AFileName: string);
begin
  inherited Create(Collection, AFileName);
end;

constructor TScLinkedResource.Create(Collection: TScLinkedResourceCollection; CopyFrom: TStream);
begin
  inherited Create(Collection, CopyFrom);
end;

{ TScAlternateViewCollection }

constructor TScAlternateViewCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScAlternateView);
end;

function TScAlternateViewCollection.GetItem(Index: integer): TScAlternateView;
begin
  Result := inherited GetItem(Index) as TScAlternateView;
end;

procedure TScAlternateViewCollection.SetItem(Index: integer; Value: TScAlternateView);
begin
  inherited SetItem(Index, Value);
end;

{ TScAttachmentCollection }

constructor TScAttachmentCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScAttachment);
end;

function TScAttachmentCollection.GetItem(Index: integer): TScAttachment;
begin
  Result := inherited GetItem(Index) as TScAttachment;
end;

procedure TScAttachmentCollection.SetItem(Index: integer; Value: TScAttachment);
begin
  inherited SetItem(Index, Value);
end;

{ TScLinkedResourceCollection }

constructor TScLinkedResourceCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScLinkedResource);
end;

function TScLinkedResourceCollection.GetItem(Index: integer): TScLinkedResource;
begin
  Result := inherited GetItem(Index) as TScLinkedResource;
end;

procedure TScLinkedResourceCollection.SetItem(Index: integer; Value: TScLinkedResource);
begin
  inherited SetItem(Index, Value);
end;

{ TScHeaderList }

constructor TScHeaderList.Create;
begin
  inherited Create;
  SB := StringBuilder.Create(1024);
end;

destructor TScHeaderList.Destroy;
begin
  SB.Free;
  inherited;
end;

function TScHeaderList.SaveToString(const Separator: string): string;
var
  i: integer;
begin
  SB.Length := 0;

  for i := 0 to Count - 1 do begin
    SB.Append(Keys[i]);
    SB.Append(Separator);
    SB.Append(Values[i]);
    SB.Append(CRLF);
  end;

  Result := SB.ToString;
end;

function TScHeaderList.GetText: string;
begin
  Result := SaveToString('=');
end;

procedure TScHeaderList.SetText(const Value: string);
var
  List: TStringList;
  i: integer;
begin
  Clear;
  List := TStringList.Create;
  try
    List.Text := Value;

    for i := 0 to List.Count - 1 do
      if List.Names[i] <> '' then
        Add(List.Names[i], List.Values[List.Names[i]])
      else
        Add(List[i], '');
  finally
    List.Free;
  end;
end;

function TScHeaderList.GetString(Index: integer): string;
begin
  Result := Keys[Index] + HeaderSeparator + Values[Index];
end;

procedure TScHeaderList.SetName(const Key: string; const Value: string);
var
  Index: integer;
begin
  if Find(Key, Index) then begin
    if Value = '' then
      Delete(Index)
    else
      SetValue(Index, Value);
  end
  else
    if Value <> '' then
      Insert(Count, Key, Value);
end;

function TScHeaderList.ToString: string;
begin
  Result := SaveToString(HeaderSeparator);
end;

end.

