unit sgcWebSocket_Server_Base_WebBrokerBridge;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  // webbroker
  HTTPApp, WebReq;

Type
  TsgcWebRequest = Class(TWebRequest)
  protected
    FContentStream: TStream;
    function GetRawContent: TBytes; override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Integer; override;
  public
    function ReadClient(var Buffer; Count: Integer): Integer; override;
    function ReadString(Count: Integer): string; override;
    function TranslateURI(const URI: string): string; override;
  End;

  TsgcWebResponse = Class(TWebResponse)
  protected
    FSent: Boolean;
    function GetDateVariable(Index: Integer): TDateTime; override;
    procedure SetDateVariable(Index: Integer; const Value: TDateTime); override;
    function GetLogMessage: string; override;
    procedure SetLogMessage(const Value: string); override;
  public
    procedure SendResponse; override;
    procedure SendRedirect(const URI: string); override;
    function Sent: Boolean; override;
  End;

  TsgcWebBrokerBridgeRequestHandler = class(TWebRequestHandler)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  INDEX_RESP_Version = 0;
  INDEX_RESP_ReasonString = 1;
  INDEX_RESP_Server = 2;
  INDEX_RESP_WWWAuthenticate = 3;
  INDEX_RESP_Realm = 4;
  INDEX_RESP_Allow = 5;
  INDEX_RESP_Location = 6;
  INDEX_RESP_ContentEncoding = 7;
  INDEX_RESP_ContentType = 8;
  INDEX_RESP_ContentVersion = 9;
  INDEX_RESP_DerivedFrom = 10;
  INDEX_RESP_Title = 11;

  INDEX_RESP_ContentLength = 0;

  INDEX_RESP_Date = 0;
  INDEX_RESP_Expires = 1;
  INDEX_RESP_LastModified = 2;

  INDEX_Method           = 0;
  INDEX_ProtocolVersion  = 1;
  INDEX_URL              = 2;
  INDEX_Query            = 3;
  INDEX_PathInfo         = 4;
  INDEX_PathTranslated   = 5;
  INDEX_CacheControl     = 6;
  INDEX_Date             = 7;
  INDEX_Accept           = 8;
  INDEX_From             = 9;
  INDEX_Host             = 10;
  INDEX_IfModifiedSince  = 11;
  INDEX_Referer          = 12;
  INDEX_UserAgent        = 13;
  INDEX_ContentEncoding  = 14;
  INDEX_ContentType      = 15;
  INDEX_ContentLength    = 16;
  INDEX_ContentVersion   = 17;
  INDEX_DerivedFrom      = 18;
  INDEX_Expires          = 19;
  INDEX_Title            = 20;
  INDEX_RemoteAddr       = 21;
  INDEX_RemoteHost       = 22;
  INDEX_ScriptName       = 23;
  INDEX_ServerPort       = 24;
  INDEX_Content          = 25;
  INDEX_Connection       = 26;
  INDEX_Cookie           = 27;
  INDEX_Authorization    = 28;

implementation

uses
  sgcBase_Helpers;

constructor TsgcWebBrokerBridgeRequestHandler.Create(AOwner: TComponent);
begin
  inherited;
  Classes.ApplicationHandleException := HandleException;
end;

destructor TsgcWebBrokerBridgeRequestHandler.Destroy;
begin
  Classes.ApplicationHandleException := nil;
  inherited;
end;

function TsgcWebRequest.GetDateVariable(Index: Integer): TDateTime;
var
  vText: string;
begin
  vText := string(GetStringVariable(Index));
  if Length(vText) > 0 then
    Result := ParseDate(vText)
  else
    Result := -1;
end;

function TsgcWebRequest.GetIntegerVariable(Index: Integer): Integer;
begin
  Result := StrToIntDef(string(GetStringVariable(Index)), -1);
end;

function TsgcWebRequest.GetRawContent: TBytes;
var
  vPos: Int64;
begin
  vPos := FContentStream.Position;
  FContentStream.Position := 0;
  try
    SetLength(Result, FContentStream.Size);
    FContentStream.Read(Result, 0, Length(Result));
  finally
    FContentStream.Position := vPos;
  end;
end;

function TsgcWebRequest.ReadClient(var Buffer; Count: Integer): Integer;
begin
  Result := FContentStream.Read(Buffer, Count);
  if Result < 0 then
    Result := 0;
end;

function TsgcWebRequest.ReadString(Count: Integer): string;
var
  oStream: TsgcStringStream;
begin
  oStream := TsgcStringStream.Create;
  Try
    oStream.CopyFrom(FContentStream, Count);
    Result := oStream.DataString;
  Finally
    sgcFree(oStream);
  End;
end;

function TsgcWebRequest.TranslateURI(const URI: string): string;
begin
  Result := URI;
end;

function TsgcWebResponse.GetDateVariable(Index: Integer): TDateTime;
begin
  // not implemented
  result := 0;
end;

function TsgcWebResponse.GetLogMessage: string;
begin
  // not implemented
  result := '';
end;

procedure TsgcWebResponse.SendRedirect(const URI: string);
begin
  FSent := True;
end;

procedure TsgcWebResponse.SendResponse;
begin
  FSent := True;
end;

function TsgcWebResponse.Sent: Boolean;
begin
  Result := FSent;
end;

procedure TsgcWebResponse.SetDateVariable(Index: Integer; const Value:
    TDateTime);
begin
  // not implemented
end;

procedure TsgcWebResponse.SetLogMessage(const Value: string);
begin
  // not implemented
end;

end.
