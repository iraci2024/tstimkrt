
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScMailMessage;

interface

uses
  Classes, SysUtils,
  ScTypes, ScCLRClasses, ScFunctions, ScUtils, ScConsts,
  ScSMTPUtils;

type
  TScMailAddressItem = class(TCollectionItem)
  private
    function GetConvertedAddress: string;
  protected
    FAddress: string;
    FDisplayName: string;

    function GetHost: string;
    procedure SetHost(const Value: string);
    function GetUser: string;
    procedure SetUser(const Value: string);
    function GetAsString: string;
    procedure SetAsString(Value: string);
  public
    procedure Assign(Source: TPersistent); override;

    property Address: string read FAddress write FAddress;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Host: string read GetHost write SetHost;
    property User: string read GetUser write SetUser;
    property AsString: string read GetAsString write SetAsString;
  end;

  TScMailAddressList = class(TOwnedCollection)
  protected
    function GetItem(Index: integer): TScMailAddressItem;
    procedure SetItem(Index: integer; Value: TScMailAddressItem);
    function GetAsString: string;
    procedure SetAsString(Value: string);
  public
    constructor Create(AOwner: TPersistent); reintroduce;
    function Add: TScMailAddressItem; reintroduce;

    procedure GetDomains(AStrings: TStrings);

    property Items[Index: integer]: TScMailAddressItem read GetItem write SetItem; default;
    property AsString: string read GetAsString write SetAsString;
  end;

  TScMimeBoundary = class
  private
    FBoundaryList: TIntValueStringList;
    function GetBoundary: string;
    function GetParentIndex: integer;

  protected
    class procedure GenerateGlobalBoundary;

  public
    constructor Create;
    destructor Destroy; override;

    class function GenerateBoundary: string;

    procedure Assign(Source: TScMimeBoundary);
    procedure Push(const Boundary: string; ParentIndex: integer);
    procedure Pop;
    procedure Clear;
    function Count: integer;

    property Boundary: string read GetBoundary;
    property ParentIndex: integer read GetParentIndex;
  end;

  TScMailPriority = (mpNormal, mpLowest, mpLow, mpHigh, mpHighest);
  TScMailEncoding = (meDefault, mePlainText, meMIME);

  TScMailMessage = class(TPersistent)
  protected
    FFrom: TScMailAddressItem;
    FTo: TScMailAddressList;
    FCC: TScMailAddressList;
    FBcc: TScMailAddressList;
    FReplyTo: TScMailAddressList;
    FSender: TScMailAddressItem;
    FReturnReceiptTo: TScMailAddressItem;

    FHeaders: TScHeaderList;
    FSpecialHeaders: TScHeaderList;
    FGeneratedHeaders: TScHeaderList;
    FMimeBoundary: TScMimeBoundary;

    FBody: TStrings;
    FAlternateViews: TScAlternateViewCollection;
    FAttachments: TScAttachmentCollection;

    FHeadersCharset: string;
    FHeadersTransferEncoding: char;
    FSubjectCharset: string;
    FSubjectTransferEncoding: char;

    FEncoding: TScMailEncoding;
    FPriority: TScMailPriority;
    FInSavingToFile: boolean;

    function GetFrom: TScMailAddressItem;
    function GetTo: TScMailAddressList;
    function GetCC: TScMailAddressList;
    function GetBcc: TScMailAddressList;
    function GetReplyTo: TScMailAddressList;
    function GetSender: TScMailAddressItem;
    function GetReturnReceiptTo: TScMailAddressItem;

    procedure SetFrom(Value: TScMailAddressItem);
    procedure SetTo(Value: TScMailAddressList);
    procedure SetCC(Value: TScMailAddressList);
    procedure SetBcc(Value: TScMailAddressList);
    procedure SetReplyTo(Value: TScMailAddressList);
    procedure SetSender(Value: TScMailAddressItem);
    procedure SetReturnReceiptTo(Value: TScMailAddressItem);

    function GetGeneratedHeaders: TScHeaderList;
    procedure SetHeaders(Value: TScHeaderList);
    procedure SetSpecialHeaders(Value: TScHeaderList);
    procedure SetBody(Value: TStrings);

    function GetHeadersCharset: string;
    function GetHeadersTransferEncoding: char;
    function GetSubjectCharset: string;
    function GetSubjectTransferEncoding: char;
    function GetContentDisposition: string;
    procedure SetContentDisposition(const Value: string);
    function GetContentTransferEncoding: string;
    procedure SetContentTransferEncoding(const Value: string);
    function GetContentCharset: string;
    procedure SetContentCharset(const Value: string);
    function GetContentType: string;
    procedure SetContentType(const Value: string);
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    function GetEncoding: TScMailEncoding;
    procedure SetEncoding(const Value: TScMailEncoding);
    function GetInReplyTo: string;
    procedure SetInReplyTo(const Value: string);
    function GetMessageId: string;
    procedure SetMessageId(const Value: string);
    function GetOrganization: string;
    procedure SetOrganization(const Value: string);
    function GetPriority: TScMailPriority;
    function GetReferences: string;
    procedure SetReferences(const Value: string);
    function GetSubject: string;
    procedure SetSubject(const Value: string);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure GenerateHeader;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property AlternateViews: TScAlternateViewCollection read FAlternateViews;
    property Attachments: TScAttachmentCollection read FAttachments;
    property Bcc: TScMailAddressList read GetBcc write SetBcc;
    property Body: TStrings read FBody write SetBody;
    property CC: TScMailAddressList read GetCC write SetCC;
    property ContentCharset: string read GetContentCharset write SetContentCharset;
    property ContentDisposition: string read GetContentDisposition write SetContentDisposition;
    property ContentTransferEncoding: string read GetContentTransferEncoding write SetContentTransferEncoding;
    property ContentType: string read GetContentType write SetContentType;
    property Date: TDateTime read GetDate write SetDate;
    property Encoding: TScMailEncoding read GetEncoding write SetEncoding;
    property From: TScMailAddressItem read GetFrom write SetFrom;
    property Headers: TScHeaderList read FHeaders write SetHeaders;
    property HeadersCharset: string read GetHeadersCharset write FHeadersCharset;
    property HeadersTransferEncoding: char read GetHeadersTransferEncoding write FHeadersTransferEncoding;
    property GeneratedHeaders: TScHeaderList read GetGeneratedHeaders;
    property SpecialHeaders: TScHeaderList read FSpecialHeaders write SetSpecialHeaders;
    property InReplyTo: string read GetInReplyTo write SetInReplyTo;
    property MessageId: string read GetMessageId write SetMessageId;
    property MimeBoundary: TScMimeBoundary read FMimeBoundary;
    property Organization: string read GetOrganization write SetOrganization;
    property Priority: TScMailPriority read GetPriority write FPriority;
    property References: string read GetReferences write SetReferences;
    property ReplyTo: TScMailAddressList read GetReplyTo write SetReplyTo;
    property ReturnReceiptTo: TScMailAddressItem read GetReturnReceiptTo write SetReturnReceiptTo;
    property Sender: TScMailAddressItem read GetSender write SetSender;
    property Subject: string read GetSubject write SetSubject;
    property SubjectCharset: string read GetSubjectCharset write FSubjectCharset;
    property SubjectTransferEncoding: char read GetSubjectTransferEncoding write FSubjectTransferEncoding;
    property ToAddress: TScMailAddressList read GetTo write SetTo;
  end;

implementation

const
  IETF_TEXT =       'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!#$%&''*+-/=?_`{}|~';
  IETF_TEXT_SPACE = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!#$%&''*+-/=?_`{}|~ ';
  IETF_QUOTABLE = '\"';
  ADDRESS_SPECIAL_SYMBOLS = '()[]<>:;.,@\"';

const
  PRIORITY_NAMES: array[TScMailPriority] of string =
    ('normal', 'non-urgent', 'non-urgent', 'urgent', 'urgent');
  IMPORTANCE_NAMES: array[TScMailPriority] of string =
    ('normal', 'low', 'low', 'high', 'high');

const
  GLOBAL_BOUNDARY_LEN = 8;
var
  GLOBAL_BOUNDARY_STR: string = '';

function IETFQuote(const Str: string): string;
var
  StrLen, QuoteLen: integer;
  i, j, Idx: integer;
begin
  QuoteLen := Length(IETF_QUOTABLE);
  StrLen := Length(Str);
  SetLength(Result, StrLen * 2 + 2);
  Result[1] := '"';
  Idx := 2;

  for i := 1 to StrLen do begin
    for j := 1 to QuoteLen do begin
      if Str[i] = IETF_QUOTABLE[j] then begin
        Result[Idx] := '\';
        Inc(Idx);
        Break;
      end;
    end;

    Result[Idx] := Str[i];
    Inc(Idx);
  end;

  Result[Idx] := '"';
  SetLength(Result, Idx);
end;

function EncodeMailAddress(MailAddress: TScMailAddressItem; const HeaderEncoding: char;
  const Charset: string): string;
var
  NeedEncode: boolean;
  Name: string;
  i: integer;
begin
  if MailAddress.DisplayName <> '' then begin
    NeedEncode := False;
    for i := 1 to Length(MailAddress.DisplayName) do begin
      if (MailAddress.DisplayName[i] < #32) or (MailAddress.DisplayName[i] >= #127) then begin
        NeedEncode := True;
        Break;
      end;
    end;

    if NeedEncode then
      Name := EncodeHeader(MailAddress.DisplayName, ADDRESS_SPECIAL_SYMBOLS, HeaderEncoding, Charset)
    else begin
      Name := '"';
      for i := 1 to Length(MailAddress.DisplayName) do begin
        // Quote '\', '"'
        if (MailAddress.DisplayName[i] = '\') or (MailAddress.DisplayName[i] = '"') then
          Name := Name + '\';
        Name := Name + MailAddress.DisplayName[i];
      end;
      Name := Name + '"';
    end;

    Result := Name + ' <' + MailAddress.Address + '>';
  end
  else
    Result := MailAddress.Address;
end;

function EncodeMailAddresses(MailAddressList: TScMailAddressList; const HeaderEncoding: char;
  const Charset: string; CanUseAddressForName: boolean): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to MailAddressList.Count - 1 do begin
    if i > 0 then
      Result := Result + ', ';

    if CanUseAddressForName and (MailAddressList[i].DisplayName = '') then
      MailAddressList[i].DisplayName := MailAddressList[i].Address;

    Result := Result + EncodeMailAddress(MailAddressList[i], HeaderEncoding, Charset);
  end;
end;

procedure DecodeMailAddresses(const MailAddresses: string; MailAddressList: TScMailAddressList);
var
  i: integer;
begin
  MailAddressList.AsString := MailAddresses;

  for i := 0 to MailAddressList.Count - 1 do
    MailAddressList[i].DisplayName := UnquoteStr(DecodeHeader(MailAddressList[i].DisplayName));
end;

{ TScMailAddressItem }

procedure TScMailAddressItem.Assign(Source: TPersistent);
begin
  if Source is TScMailAddressItem then begin
    FAddress := TScMailAddressItem(Source).FAddress;
    FDisplayName := TScMailAddressItem(Source).FDisplayName;
  end
  else
    inherited Assign(Source);
end;

function TScMailAddressItem.GetHost: string;
var
  i: integer;
begin
  Result := '';
  for i := Length(FAddress) downto 1 do begin
    if FAddress[i] = '@' then begin
      Result := Copy(FAddress, i + 1, MaxInt);
      Break;
    end;
  end;
end;

procedure TScMailAddressItem.SetHost(const Value: string);
var
  S: string;
  AtPos: integer;
begin
  S := FAddress;
  AtPos := Pos('@', S);
  if AtPos > 0 then
    Delete(S, AtPos, Length(S));
  FAddress := S + '@' + Value;
end;

function TScMailAddressItem.GetUser: string;
var
  i: integer;
begin
  Result := '';
  for i := Length(FAddress) downto 1 do begin
    if FAddress[i] = '@' then begin
      Result := Copy(FAddress, 1, i - 1);
      Break;
    end;
  end;
end;

procedure TScMailAddressItem.SetUser(const Value: string);
var
  S: string;
  AtPos: integer;
begin
  S := FAddress;
  AtPos := Pos('@', S);
  if AtPos > 0 then
    Delete(S, 1, AtPos);
  FAddress := Value + '@' + S;
end;

function TScMailAddressItem.GetConvertedAddress: string;
var
  TempAddress, CurHost: string;
  i: integer;
begin
  if FAddress = '' then begin
    if FDisplayName <> '' then
      Result := '<>'
    else
      Result := '';
    Exit;
  end;

  TempAddress := FAddress;
  CurHost := '';
  for i := Length(FAddress) downto 1 do begin
    if FAddress[i] = '@' then begin
      TempAddress := Copy(FAddress, 1, i - 1);
      CurHost := Copy(FAddress, i, MaxInt);
      Break;
    end;
  end;

  i := PosOfAnyExceptSet(IETF_TEXT, TempAddress);
  if (i = 0) or (TempAddress[i] = '.') then begin
    if FDisplayName <> '' then
      Result := '<' + TempAddress + CurHost + '>'
    else
      Result := TempAddress + CurHost;
  end
  else begin
    TempAddress := IETFQuote(TempAddress);
    Result := '<' + TempAddress + CurHost + '>';
  end;
end;

function TScMailAddressItem.GetAsString: string;
var
  i: integer;
begin
  if (FDisplayName <> '') and not AnsiSameText(FAddress, FDisplayName) then begin
    i := PosOfAnyExceptSet(IETF_TEXT_SPACE, FDisplayName);

    if i > 0 then
      Result := IETFQuote(FDisplayName) + ' ' + GetConvertedAddress
    else
      Result := FDisplayName + ' ' + GetConvertedAddress;
  end
  else
    Result := GetConvertedAddress;
end;

procedure TScMailAddressItem.SetAsString(Value: string);
var
  AfterAt, InQuote, InAddress, HasAddress: boolean;
  BracketCount, FirstPos: integer;
begin
  FAddress := '';
  FDisplayName := '';

  Value := Trim(Value);
  if Value = '' then
    Exit;

  if Pos('<', Value) > 0 then
    FirstPos := PosOfAnyFromSet('("< ' + TAB, Value)
  else
    FirstPos := PosOfAnyFromSet('(" @' + TAB, Value);

  if FirstPos <> 0 then begin
    BracketCount := 0;
    AfterAt := False;
    HasAddress := False;
    InAddress := False;
    InQuote := False;

    repeat
      case Value[FirstPos] of
        ' ', TAB: begin
          if FirstPos = 1 then
            Delete(Value, 1, 1)
          else begin
            if AfterAt then
              FAddress := FAddress + Trim(Copy(Value, 1, FirstPos - 1))
            else
              FDisplayName := FDisplayName + Copy(Value, 1, FirstPos);
            Delete(Value, 1, FirstPos);
          end;
        end;

        '(': begin
          Inc(BracketCount);
          if FirstPos > 1 then begin
            if InAddress then
              FAddress := FAddress + Trim(Copy(Value, 1, FirstPos - 1))
            else
            if BracketCount = 1 then
              FDisplayName := FDisplayName + Copy(Value, 1, FirstPos - 1);
            Delete(Value, 1, FirstPos);
          end
          else
            Delete(Value, 1, 1);
        end;

        ')': begin
          Dec(BracketCount);
          Delete(Value, 1, FirstPos);
        end;

        '"': begin
          if InQuote then begin
            if HasAddress then
              FAddress := FAddress + Trim(Copy(Value, 1, FirstPos - 1))
            else
              FDisplayName := FDisplayName + Trim(Copy(Value, 1, FirstPos - 1));

            Delete(Value, 1, FirstPos);
            InQuote := False;
          end
          else begin
            InQuote := True;
            Delete(Value, 1, 1);
          end;
        end;

        '<': begin
          if FirstPos > 1 then
            FDisplayName := FDisplayName + Copy(Value, 1, FirstPos - 1);

          FDisplayName := Trim(FDisplayName);
          InAddress := True;
          HasAddress := True;
          Delete(Value, 1, FirstPos);
        end;

        '>': begin
          InAddress := False;
          AfterAt := False;
          FAddress := FAddress + Trim(Copy(Value, 1, FirstPos - 1));
          Delete(Value, 1, FirstPos);
        end;

        '@': begin
          AfterAt := True;
          if InAddress then begin
            FAddress := FAddress + Copy(Value, 1, FirstPos);
            Delete(Value, 1, FirstPos);
          end
          else begin
            if HasAddress then begin
              FDisplayName := FDisplayName + Value;
              Exit;
            end;

            InAddress := True;
            FAddress := FAddress + Copy(Value, 1, FirstPos);
            Delete(Value, 1, FirstPos);
          end;
        end;

        '.': begin
          if HasAddress then begin
            FAddress := FAddress + Trim(Copy(Value, 1, FirstPos - 1)) + '.';
            Value := TrimLeft(Copy(Value, FirstPos + 1, MaxInt));
          end
          else begin
            FAddress := FAddress + Copy(Value, 1, FirstPos);
            Delete(Value, 1, FirstPos);
          end;
        end;

        '\': begin
          if InQuote then begin
            if InAddress then
              FAddress := FAddress + Copy(Value, 1, FirstPos - 1) + Value[FirstPos + 1]
            else
              FDisplayName := FDisplayName + Copy(Value, 1, FirstPos - 1) + Value[FirstPos + 1];
          end;
          Delete(Value, 1, FirstPos + 1);
        end;
      end;

      if BracketCount > 0 then
        FirstPos := PosOfAnyFromSet('()\', Value)
      else
      if InQuote then
        FirstPos := LastDelimiter('"\', Value)
      else
      if AfterAt then begin
        if HasAddress then
          FirstPos := PosOfAnyFromSet('.>(', Value)
        else
          FirstPos := PosOfAnyFromSet('.( ', Value);
      end
      else
      if InAddress then
        FirstPos := PosOfAnyFromSet('"(@>', Value)
      else
        FirstPos := PosOfAnyFromSet('("< @' + TAB, Value);
    until FirstPos = 0;

    if InAddress and not HasAddress then
      FAddress := FAddress + Trim(Value);
  end
  else
    FAddress := Value;
end;

{ TScMailAddressList }

constructor TScMailAddressList.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScMailAddressItem);
end;

function TScMailAddressList.Add: TScMailAddressItem;
begin
  Result := TScMailAddressItem(inherited Add);
end;

function TScMailAddressList.GetItem(Index: integer): TScMailAddressItem;
begin
  Result := TScMailAddressItem(inherited Items[Index]);
end;

procedure TScMailAddressList.SetItem(Index: integer; Value: TScMailAddressItem);
begin
  inherited SetItem(Index, Value);
end;

function TScMailAddressList.GetAsString: string;
var
  Idx: integer;
begin
  Result := '';
  for Idx := 0 to Count - 1 do begin
    if Result = '' then
      Result := GetItem(Idx).AsString
    else
      Result := Result + ', ' + GetItem(Idx).AsString;
  end;
end;

procedure TScMailAddressList.SetAsString(Value: string);
var
  MailAddress: TScMailAddressItem;
  StartPos: integer;
  BracketCount: integer;
  InQuote: boolean;
  Temp: string;
begin
  Clear;
  if Trim(Value) = '' then
    Exit;

  StartPos := PosOfAnyFromSet(':;(", ' + TAB, Value);
  if StartPos = 0 then begin
    MailAddress := Add;
    MailAddress.AsString := TrimLeft(Value);
  end
  else begin
    Temp := '';
    BracketCount := 0;
    InQuote := False;

    repeat
      case Value[StartPos] of
        ' ', TAB: begin
          if StartPos = 1 then begin
            Temp := Temp + Value[StartPos];
            System.Delete(Value, 1, 1);
          end
          else begin
            Temp := Temp + Copy(Value, 1, StartPos);
            System.Delete(Value, 1, StartPos);
          end;
        end;

        ':': begin
          System.Delete(Value, 1, StartPos);
          Temp := '';
        end;

        ';': begin
          Temp := Temp + Copy(Value, 1, StartPos - 1);

          if Trim(Temp) <> '' then begin
            MailAddress := Add;
            MailAddress.AsString := TrimLeft(Temp);
            Temp := '';
          end;

          System.Delete(Value, 1, StartPos);
        end;

        '(': begin
          Inc(BracketCount);
          Temp := Temp + Copy(Value, 1, StartPos);
          System.Delete(Value, 1, StartPos);
        end;

        ')': begin
          Dec(BracketCount);
          Temp := Temp + Copy(Value, 1, StartPos);
          System.Delete(Value, 1, StartPos);
        end;

        '"': begin
          Temp := Temp + Copy(Value, 1, StartPos);
          System.Delete(Value, 1, StartPos);
          InQuote := not InQuote;
        end;

        ',': begin
          Temp := Temp + Copy(Value, 1, StartPos - 1);
          MailAddress := Add;
          MailAddress.AsString := Temp;

          Temp := Trim(MailAddress.AsString);
          if (Temp = '') or (Temp = '<>') then
            FreeAndNil(MailAddress);
          Temp := '';
          System.Delete(Value, 1, StartPos);
        end;

        '\': begin
          Temp := Temp + Copy(Value, 1, StartPos + 1);
          System.Delete(Value, 1, StartPos + 1);
        end;
      end;

      if BracketCount > 0 then
        StartPos := PosOfAnyFromSet('(\)', Value)
      else
      if InQuote then
        StartPos := PosOfAnyFromSet('"\', Value)
      else
        StartPos := PosOfAnyFromSet(':;(", ' + TAB, Value);
    until StartPos = 0;

    if (Trim(Temp) <> '') or (Trim(Value) <> '') then begin
      Temp := Temp + Value;
      MailAddress := Add;
      MailAddress.AsString := TrimLeft(Temp);

      Temp := Trim(MailAddress.AsString);
      if (Temp = '') or (Temp = '<>') then
        FreeAndNil(MailAddress);
    end;
  end;
end;

procedure TScMailAddressList.GetDomains(AStrings: TStrings);
var
  Host: string;
  i: integer;
begin
  if Assigned(AStrings) then begin
    AStrings.BeginUpdate;
    try
      AStrings.Clear;
      for i := 0 to Count - 1 do begin
        Host := LowerCase(Items[i].Host);
        if AStrings.IndexOf(Host) = -1 then
          AStrings.Add(Host);
      end;
    finally
      AStrings.EndUpdate;
    end;
  end;
end;

{ TScMimeBoundary }

constructor TScMimeBoundary.Create;
begin
  inherited;

  FBoundaryList := TIntValueStringList.Create;
end;

destructor TScMimeBoundary.Destroy;
begin
  FBoundaryList.Free;

  inherited;
end;

class procedure TScMimeBoundary.GenerateGlobalBoundary;
var
  d: double;
  Val, i: integer;
begin
  if GLOBAL_BOUNDARY_STR = '' then begin
    if RandSeed = 0 then
      Randomize;

    SetLength(GLOBAL_BOUNDARY_STR, GLOBAL_BOUNDARY_LEN);
    for i := 1 to 8 do begin
      // Digits (48-57), upper-case letters (65..90). In all 36 possible symbols.
      d := (Random * 35) + 1.5; // [1.5 .. 36.5]
      Val := Trunc(d) + 47;  // -> [48 .. 83]
      if Val > 57 then
        Inc(Val, 7); // to upper-case
      GLOBAL_BOUNDARY_STR[i] := Chr(Val);
    end;
  end;
end;

class function TScMimeBoundary.GenerateBoundary: string;
const
  BOUNDARY_LEN = 30 + GLOBAL_BOUNDARY_LEN; //'--=_24*.8*_'
var
  d: double;
  Val, i: integer;
begin
  if RandSeed = 0 then
    Randomize;

  GenerateGlobalBoundary;

  SetLength(Result, BOUNDARY_LEN);
  for i := 5 to BOUNDARY_LEN - GLOBAL_BOUNDARY_LEN - 1 do begin
    // Digits (48-57), upper-case letters (65..90). In all 36 possible symbols.
    d := (Random * 35) + 1.5; // [1.5 .. 36.5]
    Val := Trunc(d) + 47;  // -> [48 .. 83]
    if Val > 57 then
      Inc(Val, 7); // to upper-case
    Result[i] := Chr(Val);
  end;

  Result[1] := '-';
  Result[2] := '-';
  Result[3] := '=';
  Result[4] := '_';
  Result[29] := '.';
  Result[BOUNDARY_LEN] := '_';
  Move(GLOBAL_BOUNDARY_STR[1], Result[30], GLOBAL_BOUNDARY_LEN * sizeof(char));
end;

procedure TScMimeBoundary.Assign(Source: TScMimeBoundary);
begin
  FBoundaryList.Assign(Source.FBoundaryList);
end;

procedure TScMimeBoundary.Clear;
begin
  FBoundaryList.Clear;
end;

function TScMimeBoundary.Count: integer;
begin
  Result := FBoundaryList.Count;
end;

function TScMimeBoundary.GetBoundary: string;
begin
  if FBoundaryList.Count > 0 then
    Result := FBoundaryList[0]
  else
    Result := '';
end;

function TScMimeBoundary.GetParentIndex: integer;
begin
  if FBoundaryList.Count > 0 then
    Result := FBoundaryList.Values[0]
  else
    Result := -1;
end;

procedure TScMimeBoundary.Pop;
begin
  if FBoundaryList.Count > 0 then
    FBoundaryList.Delete(0);
end;

procedure TScMimeBoundary.Push(const Boundary: string; ParentIndex: integer);
begin
  FBoundaryList.Insert(0, Boundary, ParentIndex);
end;

{ TScMailMessage }

constructor TScMailMessage.Create;
begin
  inherited;

  FFrom := TScMailAddressItem.Create(nil);
  FTo := TScMailAddressList.Create(Self);
  FCC := TScMailAddressList.Create(Self);
  FBcc := TScMailAddressList.Create(Self);
  FReplyTo := TScMailAddressList.Create(Self);
  FSender := TScMailAddressItem.Create(nil);
  FReturnReceiptTo := TScMailAddressItem.Create(nil);

  FAlternateViews := TScAlternateViewCollection.Create(Self);
  FAttachments := TScAttachmentCollection.Create(Self);
  FBody := TStringList.Create;
  TStringList(FBody).Duplicates := dupAccept;

  FHeaders := TScHeaderList.Create;
  FSpecialHeaders := TScHeaderList.Create;
  FGeneratedHeaders := TScHeaderList.Create;
  FMimeBoundary := TScMimeBoundary.Create;

  FPriority := mpNormal;
  FEncoding := meDefault;

  FHeadersTransferEncoding := 'B';
  FSubjectTransferEncoding := 'B';
end;

destructor TScMailMessage.Destroy;
begin
  FFrom.Free;
  FTo.Free;
  FCC.Free;
  FBcc.Free;
  FReplyTo.Free;
  FSender.Free;
  FReturnReceiptTo.Free;

  FAlternateViews.Free;
  FAttachments.Free;
  FBody.Free;

  FHeaders.Free;
  FSpecialHeaders.Free;
  FGeneratedHeaders.Free;
  FMimeBoundary.Free;

  inherited;
end;

procedure TScMailMessage.AssignTo(Dest: TPersistent);
var
  Dst: TScMailMessage;
begin
  if IsClass(Dest, TScMailMessage) then begin
    Dst := TScMailMessage(Dest);

    Dst.FFrom.Assign(FFrom);
    Dst.FTo.Assign(FTo);
    Dst.FCC.Assign(FCC);
    Dst.FBcc.Assign(FBcc);
    Dst.FReplyTo.Assign(FReplyTo);
    Dst.FSender.Assign(FSender);
    Dst.FReturnReceiptTo.Assign(FReturnReceiptTo);

    Dst.FAlternateViews.Assign(FAlternateViews);
    Dst.FAttachments.Assign(FAttachments);
    Dst.FBody.Assign(FBody);
    Dst.FHeaders.Assign(FHeaders);
    Dst.FSpecialHeaders.Assign(FSpecialHeaders);
    Dst.FMimeBoundary.Assign(FMimeBoundary);

    Dst.FHeadersCharset := FHeadersCharset;
    Dst.FHeadersTransferEncoding := FHeadersTransferEncoding;
    Dst.FSubjectCharset := FSubjectCharset;
    Dst.FSubjectTransferEncoding := FSubjectTransferEncoding;

    Dst.FEncoding := FEncoding;
    Dst.FPriority := FPriority;
  end
  else
    inherited;
end;

procedure TScMailMessage.Clear;
begin
  // Clear Header
  FFrom.AsString := '';
  FTo.Clear;
  FCC.Clear;
  FBcc.Clear;
  FReplyTo.Clear;
  FSender.AsString := '';
  FReturnReceiptTo.AsString := '';

  FHeaders.Clear;
  FSpecialHeaders.Clear;
  FMimeBoundary.Clear;
  FGeneratedHeaders.Clear;

  FPriority := mpNormal;
  FEncoding := meDefault;
  FInSavingToFile := False;

  // Clear Body
  FAlternateViews.Clear;
  FAttachments.Clear;
  FBody.Clear;
end;

procedure TScMailMessage.GenerateHeader;

  procedure SetContentTransferEncoding(Attachment: TScCustomAttachment);
  var
    CTEStr: string;
  begin
    CTEStr := LowerCase(Attachment.ContentTransferEncoding);
    if CTEStr <> '' then begin
      if FEncoding = meMIME then begin
        if (CTEStr <> '7bit') and (CTEStr <> '8bit') and (CTEStr <> 'binary') and
           (CTEStr <> 'base64') and (CTEStr <> 'binhex40') and (CTEStr <> 'quoted-printable')
        then
          Attachment.ContentTransferEncoding := 'base64';
      end
      else
      if (CTEStr <> 'xxe') and (CTEStr <> 'uue') then
        Attachment.ContentTransferEncoding := 'UUE';
    end;
  end;

var
  CTEStr, ContentTypeStr, BoundaryStr, TmpStr: string;
  EncReturnReceiptTo: string;
  i: integer;
begin
  if Encoding = meDefault then
    if (AlternateViews.Count = 0) and (Attachments.Count = 0) then
      Encoding := mePlainText
    else
      Encoding := meMIME;

  for i := 0 to AlternateViews.Count - 1 do
    SetContentTransferEncoding(AlternateViews[i]);

  for i := 0 to Attachments.Count - 1 do
    SetContentTransferEncoding(Attachments[i]);

  FGeneratedHeaders.Assign(FHeaders);

  if FFrom.DisplayName = '' then
    FFrom.DisplayName := FFrom.Address;
  FGeneratedHeaders.Names['From'] := EncodeMailAddress(FFrom, HeadersTransferEncoding, HeadersCharset);

  FGeneratedHeaders.Names['Subject'] := EncodeHeader(FHeaders.Names['Subject'], '', SubjectTransferEncoding, SubjectCharset);

  if FTo.Count > 0 then
    FGeneratedHeaders.Names['To'] := EncodeMailAddresses(FTo, HeadersTransferEncoding, HeadersCharset, False);

  if FCC.Count > 0 then
    FGeneratedHeaders.Names['Cc'] := EncodeMailAddresses(FCC, HeadersTransferEncoding, HeadersCharset, False);

  if FInSavingToFile then begin
    if FBcc.Count > 0 then
      FGeneratedHeaders.Names['Bcc'] := EncodeMailAddresses(FBcc, HeadersTransferEncoding, HeadersCharset, False);
  end
  else begin
    FGeneratedHeaders.Names['Bcc'] := '';
    FGeneratedHeaders.Names['Message-Id'] := '';
  end;

  if (FEncoding = meMIME) and (Attachments.Count = 1) and (AlternateViews.Count = 0) and (FBody.Count = 0) then begin // has only attachment
    FGeneratedHeaders.Names['Content-Type'] := '';
    FGeneratedHeaders.Names['Content-Transfer-Encoding'] := '';
    FGeneratedHeaders.Names['Content-Disposition'] := '';
  end
  else begin
    BoundaryStr := '';
    ContentTypeStr := FGeneratedHeaders.Names['Content-Type'];

    if FEncoding = meMIME then begin
      FMimeBoundary.Clear;
      // BoundaryStr := ParseHeaderParam(ContentTypeStr, 'boundary');
      //if BoundaryStr = '' then
      BoundaryStr := TScMimeBoundary.GenerateBoundary;
      FMimeBoundary.Push(BoundaryStr, -1); // -1 is top level

      if Attachments.Count > 0 then
        ContentTypeStr := 'multipart/mixed'
      else
      if AlternateViews.Count > 0 then
        ContentTypeStr := 'multipart/alternative'
      else
      if ContentTypeStr = '' then
        ContentTypeStr := 'text/plain; charset=us-ascii';
    end
    else
    if ContentTypeStr = '' then
      ContentTypeStr := 'text/plain; charset=us-ascii';

    if ((Attachments.Count > 0) or (AlternateViews.Count > 0)) and (BoundaryStr <> '') then
      FGeneratedHeaders.Names['Content-Type'] := ReplaceHeaderParam(ContentTypeStr, 'boundary', BoundaryStr, TmpStr, qtMIME)
    else
      FGeneratedHeaders.Names['Content-Type'] := ContentTypeStr;

    // Do not support attachments in encoded body
    CTEStr := LowerCase(FGeneratedHeaders.Names['Content-Transfer-Encoding']);
    if ((Attachments.Count > 0) or (AlternateViews.Count > 0)) and
      (CTEStr <> '') and (CTEStr <> '7bit') and (CTEStr <> '8bit') and (CTEStr <> 'binary')
    then
      FGeneratedHeaders.Names['Content-Transfer-Encoding'] := '';
  end;

  if FHeaders.Names['Date'] = '' then
    FGeneratedHeaders.Names['Date'] := LocalDateTimeToGMT(Now);

  if FHeaders.Names['In-Reply-To'] = '' then begin
    if FHeaders.Names['Message-ID'] <> '' then
      FGeneratedHeaders.Names['In-Reply-To'] := FHeaders.Names['Message-ID']
    else
    if FSpecialHeaders.Names['Message-ID'] <> '' then
      FGeneratedHeaders.Names['In-Reply-To'] := FSpecialHeaders.Names['Message-ID'];
  end;

  if FSender.AsString <> '' then
    FGeneratedHeaders.Names['Sender'] := FSender.AsString;

  if FReplyTo.Count > 0 then
    FGeneratedHeaders.Names['Reply-To'] := EncodeMailAddresses(FReplyTo, HeadersTransferEncoding, HeadersCharset, False);

  FGeneratedHeaders.Names['Organization'] := EncodeHeader(FHeaders.Names['Organization'], '', SubjectTransferEncoding, SubjectCharset);

  if FReturnReceiptTo.AsString <> '' then begin
    EncReturnReceiptTo := EncodeMailAddress(FReturnReceiptTo, HeadersTransferEncoding, HeadersCharset);
    FGeneratedHeaders.Names['Disposition-Notification-To'] := EncReturnReceiptTo;
    FGeneratedHeaders.Names['Return-Receipt-To'] := EncReturnReceiptTo;
  end;

  if GetPriority <> mpNormal then begin
    FGeneratedHeaders.Names['Importance'] := IMPORTANCE_NAMES[FPriority];
    FGeneratedHeaders.Names['Priority'] := PRIORITY_NAMES[FPriority];
    FGeneratedHeaders.Names['X-Priority'] := IntToStr(Ord(FPriority) + 1);
  end
  else begin
    FGeneratedHeaders.Names['Importance'] := '';
    FGeneratedHeaders.Names['Priority'] := '';
    FGeneratedHeaders.Names['X-Priority'] := '';
  end;

  if FSpecialHeaders.Count > 0 then
    FGeneratedHeaders.AddStrings(FSpecialHeaders);
end;

function TScMailMessage.GetGeneratedHeaders: TScHeaderList;
begin
  GenerateHeader;
  Result := FGeneratedHeaders;
end;

function TScMailMessage.GetHeadersCharset: string;
begin
  if FHeadersCharset = '' then
    FHeadersCharset := ParseHeaderParam(FHeaders.Names['Content-Type'], 'charset');
  if FHeadersCharset = '' then
    FHeadersCharset := 'us-ascii';

  Result := FHeadersCharset;
end;

function TScMailMessage.GetHeadersTransferEncoding: char;
begin
  if FHeadersTransferEncoding = '' then
    FHeadersTransferEncoding := 'B';
  Result := FHeadersTransferEncoding;
end;

function TScMailMessage.GetSubjectCharset: string;
begin
  if FSubjectCharset = '' then
    FSubjectCharset := ParseHeaderParam(FHeaders.Names['Content-Type'], 'charset');
  if FSubjectCharset = '' then
    FSubjectCharset := 'us-ascii';

  Result := FSubjectCharset;
end;

function TScMailMessage.GetSubjectTransferEncoding: char;
begin
  if FSubjectTransferEncoding = '' then
    FSubjectTransferEncoding := 'B';
  Result := FSubjectTransferEncoding;
end;

function TScMailMessage.GetContentCharset: string;
begin
  Result := ParseHeaderParam(FHeaders.Names['Content-Type'], 'charset');
end;

procedure TScMailMessage.SetContentCharset(const Value: string);
var
  ContentTypeStr, Tmp: string;
begin
  ContentTypeStr := FHeaders.Names['Content-Type'];
  if (ContentTypeStr = '') and (Value = '') then
    Exit;

  if ContentTypeStr = '' then
    ContentTypeStr := 'text/plain';

  FHeaders.Names['Content-Type'] := ReplaceHeaderParam(ContentTypeStr, 'charset', Value, Tmp, qtMIME);
end;

function TScMailMessage.GetContentType: string;
begin
  Result := FHeaders.Names['Content-Type'];
end;

procedure TScMailMessage.SetContentType(const Value: string);
var
  ContentTypeStr, Tmp: string;
  OldCharset, OldBoundary: string;
begin
  if Value = '' then begin
    FHeaders.Names['Content-Type'] := '';
    Exit;
  end;

  // Content-type: text/plain; charset=us-ascii; boundary="abcd"
  ContentTypeStr := FHeaders.Names['Content-Type'];
  OldCharset := ParseHeaderParam(ContentTypeStr, 'charset');
  OldBoundary := ParseHeaderParam(ContentTypeStr, 'boundary');

  ContentTypeStr := Value;

  if ParseHeaderParam(ContentTypeStr, 'charset') = '' then begin
    if (OldCharset = '') and MediaTypeMatches(ParseHeaderItem(ContentTypeStr), 'text') then
      OldCharset := 'us-ascii';

    if OldCharset <> '' then
      ContentTypeStr := ReplaceHeaderParam(ContentTypeStr, 'charset', OldCharset, Tmp, qtMIME);
  end;

  if ParseHeaderParam(ContentTypeStr, 'boundary') = '' then
    if OldBoundary <> '' then
      ContentTypeStr := ReplaceHeaderParam(ContentTypeStr, 'boundary', OldBoundary, Tmp, qtMIME);

  FHeaders.Names['Content-Type'] := ContentTypeStr;
end;

function TScMailMessage.GetContentDisposition: string;
begin
  Result := FHeaders.Names['Content-Disposition'];
end;

procedure TScMailMessage.SetContentDisposition(const Value: string);
begin
  FHeaders.Names['Content-Disposition'] := Value;
end;

function TScMailMessage.GetContentTransferEncoding: string;
begin
  Result := FHeaders.Names['Content-Transfer-Encoding'];
end;

procedure TScMailMessage.SetContentTransferEncoding(const Value: string);
begin
  FHeaders.Names['Content-Transfer-Encoding'] := Value;
end;

function TScMailMessage.GetDate: TDateTime;
begin
  Result := GMTToLocalDateTime(FHeaders.Names['Date']);
end;

procedure TScMailMessage.SetDate(const Value: TDateTime);
begin
  FHeaders.Names['Date'] := LocalDateTimeToGMT(Value);
end;

function TScMailMessage.GetEncoding: TScMailEncoding;
begin
  if FHeaders.Names['MIME-Version'] <> '' then
    FEncoding := meMIME;

  Result := FEncoding;
end;

procedure TScMailMessage.SetEncoding(const Value: TScMailEncoding);
begin
  FEncoding := Value;
  if Value = meMIME then
    FHeaders.Names['MIME-Version'] := '1.0'
  else
    FHeaders.Names['MIME-Version'] := '';
end;

function TScMailMessage.GetInReplyTo: string;
begin
  Result := IncludeBrackets(FHeaders.Names['In-Reply-To']);
end;

procedure TScMailMessage.SetInReplyTo(const Value: string);
begin
  FHeaders.Names['In-Reply-To'] := IncludeBrackets(Value);
end;

function TScMailMessage.GetMessageId: string;
begin
  Result := IncludeBrackets(FHeaders.Names['Message-Id']);
end;

procedure TScMailMessage.SetMessageId(const Value: string);
begin
  FHeaders.Names['Message-Id'] := IncludeBrackets(Value);
end;

function TScMailMessage.GetOrganization: string;
begin
  Result := FHeaders.Names['Organization'];
end;

procedure TScMailMessage.SetOrganization(const Value: string);
begin
  FHeaders.Names['Organization'] := Value;
end;

function TScMailMessage.GetReferences: string;
begin
  Result := FHeaders.Names['References'];
end;

procedure TScMailMessage.SetReferences(const Value: string);
begin
  FHeaders.Names['References'] := Value;
end;

function TScMailMessage.GetSubject: string;
begin
  Result := DecodeHeader(FHeaders.Names['Subject']);
end;

procedure TScMailMessage.SetSubject(const Value: string);
begin
  FHeaders.Names['Subject'] := Value;
end;

function TScMailMessage.GetPriority: TScMailPriority;
var
  PriorityStr: string;
  PrNo: integer;
begin
  if FHeaders.IndexOf('X-Priority') >= 0 then
    PriorityStr := FHeaders.Names['X-Priority']
  else
  if FHeaders.IndexOf('Priority') >= 0 then
    PriorityStr := FHeaders.Names['Priority']
  else
  if FHeaders.IndexOf('Importance') >= 0 then
    PriorityStr := FHeaders.Names['Importance']
  else
  if FHeaders.IndexOf('X-MSMail-Priority') >= 0 then
    PriorityStr := FHeaders.Names['X-MSMail-Priority']
  else
    PriorityStr := '';

  if PriorityStr <> '' then begin
    PriorityStr := LowerCase(PriorityStr);

    if (Pos('low', PriorityStr) <> 0) or (Pos('non-urgent', PriorityStr) <> 0) then
      FPriority := mpLowest
    else
    if (Pos('high', PriorityStr) <> 0) or (Pos('urgent', PriorityStr) <> 0) then
      FPriority := mpHighest
    else begin
      PrNo := StrToIntDef(GetFirstWord(Trim(PriorityStr), ' '), 3);
      if (PrNo < 1) or (PrNo > 5) then
        PrNo := 3;

      case PrNo of
        1:
          FPriority := mpLowest;
        2:
          FPriority := mpLow;
        3:
          FPriority := mpNormal;
        4:
          FPriority := mpHigh;
        5:
          FPriority := mpHighest;
      end;
    end;
  end;

  Result := FPriority;
end;

function TScMailMessage.GetFrom: TScMailAddressItem;
begin
  if FFrom.AsString = '' then begin
    FFrom.AsString := FHeaders.Names['From'];
    FFrom.DisplayName := UnquoteStr(DecodeHeader(FFrom.DisplayName));
  end;

  Result := FFrom;
end;

function TScMailMessage.GetTo: TScMailAddressList;
var
  ToStr: string;
begin
  if FTo.Count = 0 then begin
    ToStr := FHeaders.Names['To'];
    if ToStr <> '' then
      DecodeMailAddresses(ToStr, FTo);
  end;

  Result := FTo;
end;

function TScMailMessage.GetCC: TScMailAddressList;
var
  CcStr: string;
begin
  if FCC.Count = 0 then begin
    CcStr := FHeaders.Names['Cc'];
    if CcStr <> '' then
      DecodeMailAddresses(CcStr, FCC);
  end;

  Result := FCC;
end;

function TScMailMessage.GetBcc: TScMailAddressList;
var
  BccStr: string;
begin
  if FBcc.Count = 0 then begin
    BccStr := FHeaders.Names['Bcc'];
    if BccStr <> '' then
      DecodeMailAddresses(BccStr, FBcc);
  end;

  Result := FBcc;
end;

function TScMailMessage.GetReplyTo: TScMailAddressList;
var
  ReplyToStr: string;
begin
  if FReplyTo.Count = 0 then begin
    ReplyToStr := FHeaders.Names['Reply-To'];
    if ReplyToStr <> '' then
      DecodeMailAddresses(ReplyToStr, FReplyTo);
  end;

  Result := FReplyTo;
end;

function TScMailMessage.GetSender: TScMailAddressItem;
begin
  if FSender.AsString = '' then
    FSender.AsString := FHeaders.Names['Sender'];

  Result := FSender;
end;

function TScMailMessage.GetReturnReceiptTo: TScMailAddressItem;
begin
  if FReturnReceiptTo.AsString = '' then
    FReturnReceiptTo.AsString := FHeaders.Names['Disposition-Notification-To'];
  if FReturnReceiptTo.AsString = '' then
    FReturnReceiptTo.AsString := FHeaders.Names['Return-Receipt-To'];

  Result := FReturnReceiptTo;
end;

procedure TScMailMessage.SetFrom(Value: TScMailAddressItem);
begin
  FFrom.Assign(Value);
end;

procedure TScMailMessage.SetTo(Value: TScMailAddressList);
begin
  FTo.Assign(Value);
end;

procedure TScMailMessage.SetCC(Value: TScMailAddressList);
begin
  FCC.Assign(Value);
end;

procedure TScMailMessage.SetBcc(Value: TScMailAddressList);
begin
  FBcc.Assign(Value);
end;

procedure TScMailMessage.SetReplyTo(Value: TScMailAddressList);
begin
  FReplyTo.Assign(Value);
end;

procedure TScMailMessage.SetSender(Value: TScMailAddressItem);
begin
  FSender.Assign(Value);
end;

procedure TScMailMessage.SetReturnReceiptTo(Value: TScMailAddressItem);
begin
  FReturnReceiptTo.Assign(Value);
end;

procedure TScMailMessage.SetHeaders(Value: TScHeaderList);
begin
  FHeaders.Assign(Value);
end;

procedure TScMailMessage.SetSpecialHeaders(Value: TScHeaderList);
begin
  FSpecialHeaders.Assign(Value);
end;

procedure TScMailMessage.SetBody(Value: TStrings);
begin
  FBody.Assign(Value);
end;

end.

