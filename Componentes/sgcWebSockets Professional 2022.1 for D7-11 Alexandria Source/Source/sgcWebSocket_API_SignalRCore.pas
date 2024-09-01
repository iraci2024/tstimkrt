{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_SignalRCore;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils, StrUtils,
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Types, sgcJSON;

type

  TsgcWSSignalRCoreAuthentication = (srcaRequestToken, srcaSetToken);

  TsgcWS_API_SignalRCore = class;

  TSignalRCore_Connect = record
    ConnectionId: String;
    _RawMessage: String;
  end;

  TSignalRCore_Invocation = record
    Target: String;
    Arguments: String;
    InvocationId: String;
    Headers: String;
    _RawMessage: String;
  end;

  TSignalRCore_StreamInvocation = TSignalRCore_Invocation;

  TSignalRCore_StreamItem = record
    InvocationId: String;
    Item: String;
    Headers: String;
    _RawMessage: String;
  end;

  TSignalRCore_Completion = record
    InvocationId: String;
    Result: String;
    Error: String;
    Headers: String;
    _RawMessage: String;
  end;

  TSignalRCore_CancelInvocation = record
    InvocationId: String;
    Headers: String;
    _RawMessage: String;
  end;

  TSignalRCore_Error = record
    Error: String;
    _RawMessage: String;
  end;

  TSignalRCore_Close = record
    Error: String;
    _RawMessage: String;
  end;

  TSignalRCore_WaitForCompletion = record
    InvocationId: String;
    Waiting: Boolean;
    Completion: TSignalRCore_Completion;
  end;

  TsgcWSSignalRCoreBeforeConnectEvent = procedure(Sender: TObject;
    var ConnectionId: String) of object;
  TsgcWSSignalRCoreConnectEvent = procedure(Sender: TObject;
    Connect: TSignalRCore_Connect) of object;
  TsgcWSSignalRCoreInvocationEvent = procedure(Sender: TObject;
    Invocation: TSignalRCore_Invocation) of object;
  TsgcWSSignalRCoreStreamInvocationEvent = procedure(Sender: TObject;
    StreamInvocation: TSignalRCore_StreamInvocation) of object;
  TsgcWSSignalRCoreStreamItemEvent = procedure(Sender: TObject;
    StreamItem: TSignalRCore_StreamItem; var Cancel: Boolean) of object;
  TsgcWSSignalRCoreCompletionEvent = procedure(Sender: TObject;
    Completion: TSignalRCore_Completion) of object;
  TsgcWSSignalRCoreCancelInvocationEvent = procedure(Sender: TObject;
    CancelInvocation: TSignalRCore_CancelInvocation) of object;
  TsgcWSSignalRCoreKeepAliveEvent = procedure(Sender: TObject) of object;
  TsgcWSSignalRCoreErrorEvent = procedure(Sender: TObject;
    Error: TSignalRCore_Error) of object;
  TsgcWSSignalRCoreCloseEvent = procedure(Sender: TObject;
    Close: TSignalRCore_Close) of object;
  TsgcWSSignalRCoreDisconnectEvent = procedure(Sender: TObject;
    CloseCode: Integer; CloseReason: string) of object;

  TThreadInvoke = class(TThread)
  private
    FArguments: String;
    FCompletion: TSignalRCore_Completion;
    FInvocationId: String;
    FSignalRCore: TsgcWS_API_SignalRCore;
    FTarget: String;
    FTimeout: Integer;
    FMessage: TwsSignalRCoreMessages;
  protected
    procedure Execute; override;
  public
    procedure Invoke(aSignalRCore: TsgcWS_API_SignalRCore;
      aTarget, aArguments, aInvocationId: String; aTimeout: Integer = 10000);
    procedure InvokeStream(aSignalRCore: TsgcWS_API_SignalRCore;
      aTarget, aArguments, aInvocationId: String; aTimeout: Integer = 10000);
  public
    property Completion: TSignalRCore_Completion read FCompletion
      write FCompletion;
  end;

  TsgcWSSignalRCore_Authentication_RequestToken = class(TPersistent)
  private
    FPostFieldPassword: string;
    FPostFieldToken: String;
    FURL: String;
    FPostFieldUsername: String;
    FQueryFieldToken: String;
  public
    constructor Create; virtual;
    procedure Assign(aSource: TPersistent); override;
  published
    property PostFieldPassword: string read FPostFieldPassword write
        FPostFieldPassword;
    property PostFieldToken: String read FPostFieldToken write FPostFieldToken;
    property URL: String read FURL write FURL;
    property PostFieldUsername: String read FPostFieldUsername write
        FPostFieldUsername;
    property QueryFieldToken: String read FQueryFieldToken write FQueryFieldToken;
  end;

  TsgcWSSignalRCore_Authentication_SetToken = class(TPersistent)
  private
    FToken: String;
  public
    constructor Create; virtual;
    procedure Assign(aSource: TPersistent); override;
  published
    property Token: String read FToken write FToken;
  end;

  TsgcWSSignalRCore_Authentication = class(TPersistent)
  private
    FAuthentication: TsgcWSSignalRCoreAuthentication;
    FRequestToken: TsgcWSSignalRCore_Authentication_RequestToken;
    FEnabled: Boolean;
    FPassword: String;
    FSetToken: TsgcWSSignalRCore_Authentication_SetToken;
    FUsername: String;
    procedure SetRequestToken(const Value:
        TsgcWSSignalRCore_Authentication_RequestToken);
    procedure SetSetToken(const Value: TsgcWSSignalRCore_Authentication_SetToken);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Authentication: TsgcWSSignalRCoreAuthentication
      read FAuthentication write FAuthentication;
    property RequestToken: TsgcWSSignalRCore_Authentication_RequestToken read
        FRequestToken write SetRequestToken;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Password: String read FPassword write FPassword;
    property SetToken: TsgcWSSignalRCore_Authentication_SetToken read FSetToken
        write SetSetToken;
    property Username: String read FUsername write FUsername;
  end;

  TsgcWSSignalRCore_Options = class(TPersistent)
  private
    FAuthentication: TsgcWSSignalRCore_Authentication;
    FHub: String;
    FProtocol: TwsSignalRCoreProtocols;
    FVersion: TwsSignalRCoreProtocolVersion;
    procedure SetAuthentication(const Value: TsgcWSSignalRCore_Authentication);
  protected
    function GetHubString: String;
    function GetProtocolString: String;
    function GetVersionInteger: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Authentication: TsgcWSSignalRCore_Authentication
      read FAuthentication write SetAuthentication;
    property Hub: String read FHub write FHub;
    property Protocol: TwsSignalRCoreProtocols read FProtocol write FProtocol;
    property Version: TwsSignalRCoreProtocolVersion read FVersion
      write FVersion;
  end;

  TsgcWS_API_SignalRCore = class(TsgcWSAPI_client)
    { from TsgcWSComponent_Base }
  protected
    procedure DoNotifyConnect(aConnection: TsgcWSConnection); override;
    procedure DoNotifyMessage(aConnection: TsgcWSConnection); override;
    procedure DoNotifyError(aConnection: TsgcWSConnection); override;
    procedure DoNotifyException(aConnection: TsgcWSConnection); override;
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSAPI }
  protected
    procedure DoBeforeConnect; override;
    { from TsgcWSAPI }

    { from TsgcWSAPI_client }
  protected
    function DoKeepAlive: Boolean; override;
    { from TsgcWSAPI_client }

    { json }
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
    procedure SetSignalRCore(const Value: TsgcWSSignalRCore_Options);
  protected
    function GetJSONValue(const aNode: String): Variant;
  protected
    property JSON: TsgcJSON read GetJSON write FJSON;
    { json }

    { properties }
  private
    FSignalRCore: TsgcWSSignalRCore_Options;
  public
    property SignalRCore: TsgcWSSignalRCore_Options read FSignalRCore
      write SetSignalRCore;
    { properties }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { methods }
  private
    FFirstMessage: Boolean;
    FNegotiate: Boolean;
    FConnectionId: String;
    FAuthBearerToken: String;
    FThreadInvoke: TThreadInvoke;
  private
    function GetArgumentsFromArrayOfConst(aArguments: Array of Const): String;
  private
    function DoHTTP(const aURL: String; const aPost: TStringList = nil): String;
    procedure DoAuthenticationRequestToken;
    procedure DoAuthenticationSetToken;
    procedure DoAuthentication;
    procedure DoHTTPNegotiation;
    procedure DoWebSocketURL;
  private
    function GetMessageType: TwsSignalRCoreMessages;
  protected
    procedure DoPing; virtual;
    procedure DoClose(const aReason: String); virtual;
    procedure DoHandshakeRequest; virtual;
    procedure DoInvoke(const aTarget: String; const aArguments: String = '[]';
      const aInvocationId: String = ''); virtual;
    function DoInvokeAndWait(const aTarget, aArguments, aInvocationId: String;
      out Completion: TSignalRCore_Completion; const aTimeout: Integer = 10000)
      : Boolean; virtual;
    procedure DoInvokeStream(const aTarget: String;
      const aArguments: String = '[]';
      const aInvocationId: String = '0'); virtual;
    function DoInvokeStreamAndWait(const aTarget, aArguments,
      aInvocationId: String; out Completion: TSignalRCore_Completion;
      const aTimeout: Integer = 10000): Boolean; virtual;
    procedure DoCancelInvocation(const aInvocationId: String); virtual;
  protected
    procedure DoReceiveFirstMessage; virtual;
    procedure DoReceiveCloseMessage; virtual;
    procedure DoReceiveInvocationMessage; virtual;
    procedure DoReceiveStreamInvocationMessage; virtual;
    procedure DoReceiveStreamItem; virtual;
    procedure DoReceiveCompletionMessage; virtual;
    procedure DoReceiveCancelInvocationMessage; virtual;
  public
    procedure WriteData(const aText: String);
  public
    procedure Invoke(const aTarget: String; const aArguments: Array of Const;
      const aInvocationId: String = '');
    function InvokeAndWait(const aTarget: String; aArguments: Array of Const;
      aInvocationId: String; out Completion: TSignalRCore_Completion;
      const aTimeout: Integer = 10000): Boolean;
  public
    procedure CancelInvocation(const aInvocationId: String);
  public
    procedure InvokeStream(const aTarget: String;
      const aArguments: Array of Const; const aInvocationId: String);
    function InvokeStreamAndWait(const aTarget: String;
      const aArguments: Array of Const; const aInvocationId: String;
      out Completion: TSignalRCore_Completion;
      const aTimeout: Integer = 10000): Boolean;
  public
    procedure StreamItem(const aInvocationId, aItem: String);
  public
    procedure CompletionResult(const aInvocationId, aResult: String);
    procedure CompletionError(const aInvocationId, aError: String);
  public
    procedure Ping;
  public
    procedure Close(const aReason: String = '');
  public
    procedure ReConnect(const aConnectionId: String);
    { methods }

    { events }
  private
    FOnSignalRCoreBeforeConnect: TsgcWSSignalRCoreBeforeConnectEvent;
    FOnSignalRCoreCancelInvocation: TsgcWSSignalRCoreCancelInvocationEvent;
    FOnSignalRCoreClose: TsgcWSSignalRCoreCloseEvent;
    FOnSignalRCoreCompletion: TsgcWSSignalRCoreCompletionEvent;
    FOnSignalRCoreConnect: TsgcWSSignalRCoreConnectEvent;
    FOnSignalRCoreDisconnect: TsgcWSSignalRCoreDisconnectEvent;
    FOnSignalRCoreError: TsgcWSSignalRCoreErrorEvent;
    FOnSignalRCoreInvocation: TsgcWSSignalRCoreInvocationEvent;
    FOnSignalRCoreKeepAlive: TsgcWSSignalRCoreKeepAliveEvent;
    FOnSignalRCoreStreamInvocation: TsgcWSSignalRCoreStreamInvocationEvent;
    FOnSignalRCoreStreamItem: TsgcWSSignalRCoreStreamItemEvent;
  protected
    procedure DoEventSignalRCoreBeforeConnect(var ConnectionId
      : String); virtual;
    procedure DoEventSignalRCoreConnect
      (aConnect: TSignalRCore_Connect); virtual;
    procedure DoEventSignalRCoreInvocation(aInvocation
      : TSignalRCore_Invocation); virtual;
    procedure DoEventSignalRCoreStreamInvocation(aStreamInvocation
      : TSignalRCore_StreamInvocation); virtual;
    procedure DoEventSignalRCoreStreamItem(aStreamItem
      : TSignalRCore_StreamItem); virtual;
    procedure DoEventSignalRCoreCompletion(aCompletion
      : TSignalRCore_Completion); virtual;
    procedure DoEventSignalRCoreKeepAlive; virtual;
    procedure DoEventSignalRCoreCancelInvocation(aCancelInvocation
      : TSignalRCore_CancelInvocation); virtual;
    procedure DoEventSignalRCoreError(aError: TSignalRCore_Error); virtual;
    procedure DoEventSignalRCoreDisconnect(const aCloseCode: Integer;
      const aCloseReason: string); virtual;
    procedure DoEventSignalRCoreClose(aClose: TSignalRCore_Close); virtual;
  protected
    property OnSignalRCoreBeforeConnect: TsgcWSSignalRCoreBeforeConnectEvent
      read FOnSignalRCoreBeforeConnect write FOnSignalRCoreBeforeConnect;
    property OnSignalRCoreConnect: TsgcWSSignalRCoreConnectEvent
      read FOnSignalRCoreConnect write FOnSignalRCoreConnect;
    property OnSignalRCoreInvocation: TsgcWSSignalRCoreInvocationEvent
      read FOnSignalRCoreInvocation write FOnSignalRCoreInvocation;
    property OnSignalRCoreStreamInvocation
      : TsgcWSSignalRCoreStreamInvocationEvent
      read FOnSignalRCoreStreamInvocation write FOnSignalRCoreStreamInvocation;
    property OnSignalRCoreStreamItem: TsgcWSSignalRCoreStreamItemEvent
      read FOnSignalRCoreStreamItem write FOnSignalRCoreStreamItem;
    property OnSignalRCoreCompletion: TsgcWSSignalRCoreCompletionEvent
      read FOnSignalRCoreCompletion write FOnSignalRCoreCompletion;
    property OnSignalRCoreCancelInvocation
      : TsgcWSSignalRCoreCancelInvocationEvent
      read FOnSignalRCoreCancelInvocation write FOnSignalRCoreCancelInvocation;
    property OnSignalRCoreKeepAlive: TsgcWSSignalRCoreKeepAliveEvent
      read FOnSignalRCoreKeepAlive write FOnSignalRCoreKeepAlive;
    property OnSignalRCoreError: TsgcWSSignalRCoreErrorEvent
      read FOnSignalRCoreError write FOnSignalRCoreError;
    property OnSignalRCoreClose: TsgcWSSignalRCoreCloseEvent
      read FOnSignalRCoreClose write FOnSignalRCoreClose;
    property OnSignalRCoreDisconnect: TsgcWSSignalRCoreDisconnectEvent
      read FOnSignalRCoreDisconnect write FOnSignalRCoreDisconnect;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  // indy
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  // sgc
  sgcBase_Const, sgcWebSocket_Helpers, sgcWebSocket_Const,
  sgcHTTP_Client, sgcBase_Helpers;

const
  CS_SIGNALR_CORE_SEPARATOR = chr(30);

const
  CS_SIGNALRCORE_PROTOCOL = 'protocol';
  CS_SIGNALRCORE_VERSION = 'version';
  CS_SIGNALRCORE_TYPE = 'type';
  CS_SIGNALRCORE_ERROR = 'error';
  CS_SIGNALRCORE_TARGET = 'target';
  CS_SIGNALRCORE_INVOCATION_ID = 'invocationId';
  CS_SIGNALRCORE_ARGUMENTS = 'arguments';
  CS_SIGNALRCORE_ITEM = 'item';
  CS_SIGNALRCORE_RESULT = 'result';
  CS_SIGNALRCORE_HEADERS = 'headers';

function URIEncode(const aValue: String): string;
var
  i, j: Integer;
  oBytes: TIdbytes;
begin
  Result := '';
{$IFDEF NEXTGEN}
  oBytes := TIdbytes(TEncoding.UTF8.GetBytes(aValue));
  for i := 0 to high(oBytes) do
  begin
{$ELSE}
{$IFDEF INDY10_5_5}
  oBytes := ToBytes(aValue,
    {$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF});
{$ELSE}
    oBytes := ToBytes(UTF8Encode(aValue));
{$ENDIF}
  for i := 0 to length(oBytes) - 1 do
  begin
{$ENDIF}
    j := ord(oBytes[i]);
    case j of
      ord('0') .. ord('9'), ord('a') .. ord('z'), ord('A') .. ord('Z'),
        ord('_'), ord('-'), ord('.'), ord('~'):
        Result := Result + WideChar(oBytes[i]);
    else
      Result := Result + '%' + IntToHex(oBytes[i], 2);
    end;
  end;
end;

constructor TsgcWS_API_SignalRCore.Create(aOwner: TComponent);
begin
  inherited;
  FSignalRCore := TsgcWSSignalRCore_Options.Create;
  FNegotiate := True;
end;

destructor TsgcWS_API_SignalRCore.Destroy;
begin
  sgcFree(FJSON);
  sgcFree(FSignalRCore);
  inherited;
end;

procedure TsgcWS_API_SignalRCore.CancelInvocation(const aInvocationId: String);
begin
  DoCancelInvocation(aInvocationId);
end;

procedure TsgcWS_API_SignalRCore.Close(const aReason: String = '');
begin
  DoClose(aReason);
end;

procedure TsgcWS_API_SignalRCore.CompletionError(const aInvocationId,
  aError: String);
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair(CS_SIGNALRCORE_TYPE, ord(srcmCompletion));
    oJSON.AddPair(CS_SIGNALRCORE_INVOCATION_ID, aInvocationId);
    oJSON.AddPair(CS_SIGNALRCORE_ERROR, aError);
    WriteData(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_SignalRCore.CompletionResult(const aInvocationId,
  aResult: String);
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair(CS_SIGNALRCORE_TYPE, ord(srcmCompletion));
    oJSON.AddPair(CS_SIGNALRCORE_INVOCATION_ID, aInvocationId);
    oJSON.AddPair(CS_SIGNALRCORE_RESULT, aResult);
    WriteData(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_SignalRCore.DoAuthentication;
begin
  FAuthBearerToken := '';
  case SignalRCore.Authentication.Authentication of
    srcaRequestToken:
      DoAuthenticationRequestToken;
    srcaSetToken:
      DoAuthenticationSetToken;
  end;
end;

procedure TsgcWS_API_SignalRCore.DoAuthenticationRequestToken;
var
  oJSON: TsgcJSON;
  vResponse: String;
  oData: TStringList;
  oError: TSignalRCore_Error;
begin
  oData := TStringList.Create;
  Try
    oData.Add(SignalRCore.Authentication.RequestToken.PostFieldUsername + '=' +
      SignalRCore.Authentication.Username);
    oData.Add(SignalRCore.Authentication.RequestToken.PostFieldPassword + '=' +
      SignalRCore.Authentication.Password);
    vResponse := DoHTTP(SignalRCore.Authentication.RequestToken.URL, oData);
  Finally
    sgcFree(oData);
  End;

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(vResponse);
    if oJSON.Node[SignalRCore.Authentication.RequestToken.PostFieldToken] <> nil then
      FAuthBearerToken := oJSON.Node[SignalRCore.Authentication.RequestToken.PostFieldToken].Value
    else
    begin
      oError._RawMessage := '';
      oError.Error := vResponse;

      DoEventSignalRCoreError(oError);
      Abort;
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_SignalRCore.DoAuthenticationSetToken;
begin
  FAuthBearerToken := SignalRCore.Authentication.SetToken.Token;
end;

procedure TsgcWS_API_SignalRCore.ReConnect(const aConnectionId: String);
begin
  FConnectionId := aConnectionId;
  FNegotiate := False;

  Client.Active := True;
end;

procedure TsgcWS_API_SignalRCore.DoBeforeConnect;
begin
  if SignalRCore.Authentication.Enabled then
    DoAuthentication;
  if FNegotiate then
    DoHTTPNegotiation;
  DoWebSocketURL;
  inherited;
end;

procedure TsgcWS_API_SignalRCore.DoCancelInvocation(const aInvocationId
  : String);
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair(CS_SIGNALRCORE_TYPE, ord(srcmCancelInvocation));
    oJSON.AddPair(CS_SIGNALRCORE_INVOCATION_ID, aInvocationId);
    WriteData(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_SignalRCore.DoClose(const aReason: String);
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair(CS_SIGNALRCORE_TYPE, ord(srcmClose));
    if aReason <> '' then
      oJSON.AddPair(CS_SIGNALRCORE_ERROR, aReason);
    WriteData(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_SignalRCore.DoEventSignalRCoreBeforeConnect
  (var ConnectionId: String);
begin
  if Assigned(FOnSignalRCoreBeforeConnect) then
    FOnSignalRCoreBeforeConnect(self, ConnectionId);
end;

procedure TsgcWS_API_SignalRCore.DoEventSignalRCoreCancelInvocation
  (aCancelInvocation: TSignalRCore_CancelInvocation);
begin
  if Assigned(FOnSignalRCoreCancelInvocation) then
    FOnSignalRCoreCancelInvocation(self, aCancelInvocation);
end;

procedure TsgcWS_API_SignalRCore.DoEventSignalRCoreClose
  (aClose: TSignalRCore_Close);
begin
  if Assigned(FOnSignalRCoreClose) then
    FOnSignalRCoreClose(self, aClose);
end;

procedure TsgcWS_API_SignalRCore.DoEventSignalRCoreCompletion
  (aCompletion: TSignalRCore_Completion);
begin
  if Assigned(FOnSignalRCoreCompletion) then
    FOnSignalRCoreCompletion(self, aCompletion);
end;

procedure TsgcWS_API_SignalRCore.DoEventSignalRCoreConnect
  (aConnect: TSignalRCore_Connect);
begin
  if Assigned(FOnSignalRCoreConnect) then
    FOnSignalRCoreConnect(self, aConnect);
end;

procedure TsgcWS_API_SignalRCore.DoEventSignalRCoreDisconnect(const aCloseCode
  : Integer; const aCloseReason: string);
begin
  if Assigned(FOnSignalRCoreDisconnect) then
    FOnSignalRCoreDisconnect(self, aCloseCode, aCloseReason);
end;

procedure TsgcWS_API_SignalRCore.DoEventSignalRCoreError
  (aError: TSignalRCore_Error);
begin
  if Assigned(FOnSignalRCoreError) then
    FOnSignalRCoreError(self, aError);
end;

procedure TsgcWS_API_SignalRCore.DoEventSignalRCoreKeepAlive;
begin
  if Assigned(FOnSignalRCoreKeepAlive) then
    FOnSignalRCoreKeepAlive(self);
end;

procedure TsgcWS_API_SignalRCore.DoEventSignalRCoreInvocation
  (aInvocation: TSignalRCore_Invocation);
begin
  if Assigned(FOnSignalRCoreInvocation) then
    FOnSignalRCoreInvocation(self, aInvocation);
end;

procedure TsgcWS_API_SignalRCore.DoEventSignalRCoreStreamInvocation
  (aStreamInvocation: TSignalRCore_StreamInvocation);
begin
  if Assigned(FOnSignalRCoreStreamInvocation) then
    FOnSignalRCoreStreamInvocation(self, aStreamInvocation);
end;

procedure TsgcWS_API_SignalRCore.DoEventSignalRCoreStreamItem
  (aStreamItem: TSignalRCore_StreamItem);
var
  vCancel: Boolean;
begin
  vCancel := False;
  if Assigned(FOnSignalRCoreStreamItem) then
  begin
    FOnSignalRCoreStreamItem(self, aStreamItem, vCancel);
    if vCancel then
      DoCancelInvocation(aStreamItem.InvocationId);
  end;
end;

procedure TsgcWS_API_SignalRCore.DoReceiveFirstMessage;
var
  oError: TSignalRCore_Error;
  oConnect: TSignalRCore_Connect;
begin
  if JSON.Node['error'] <> nil then
  begin
    oError._RawMessage := JSON.Text;
    oError.Error := JSON.Node['error'].Value;
    DoEventSignalRCoreError(oError);
  end
  else
  begin
    oConnect._RawMessage := JSON.Text;
    oConnect.ConnectionId := FConnectionId;
    DoEventSignalRCoreConnect(oConnect);
  end;
end;

function TsgcWS_API_SignalRCore.DoHTTP(const aURL: String;
  const aPost: TStringList = nil): String;
var
  oHTTP: TsgcIdHTTP;
  oStreamSource, oStreamResponse: TStringStream;
  vURL: String;
begin
  oHTTP := TsgcIdHTTP.Create(nil);
  oHTTP.HandleRedirects := True;
  oStreamSource := TStringStream.Create('');

  oStreamResponse := TStringStream.Create('');
  Try
    if LeftStr(UpperCase(aURL), 4) = 'HTTP' then
      vURL := aURL
    else
    begin
      vURL := 'http://';
      if Client.TLS then
        vURL := 'https://';
      vURL := vURL + Client.Host + ':' + IntToStr(Client.Port);
      vURL := vURL + aURL;
    end;

    if Assigned(Client) then
      oHTTP.TLSOptions.Assign(Client.TLSOptions);
    oHTTP.ReadTimeout := 10000;
    oHTTP.Request.UserAgent := CS_APPLICATION_NAME + ' ' + CS_VERSION;
    if SignalRCore.Authentication.Enabled then
    begin
      case SignalRCore.Authentication.Authentication of
        srcaRequestToken, srcaSetToken:
          begin
            if FAuthBearerToken <> '' then
              oHTTP.Request.CustomHeaders.Add('Authorization: Bearer ' + FAuthBearerToken);
          end;
      end;
    end;
    if Assigned(aPost) then
      oHTTP.Post(vURL, aPost, oStreamResponse)
    else
      oHTTP.Post(vURL, oStreamSource, oStreamResponse);
    Result := oStreamResponse.DataString
  Finally
    sgcFree(oStreamSource);
    sgcFree(oStreamResponse);
    sgcFree(oHTTP);
  End;
end;

procedure TsgcWS_API_SignalRCore.DoHTTPNegotiation;
var
  oJSON: TsgcJSON;
  vURL: String;
  vResponse: String;
begin
  vURL := SignalRCore.GetHubString;
  if RightStr(vURL, 1) <> '/' then
    vURL := vURL + '/';
  vURL := vURL + 'negotiate';

  vResponse := DoHTTP(vURL);

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(vResponse);
    if oJSON.Node['connectionId'] <> nil then
      FConnectionId := oJSON.Node['connectionId'].Value;
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_SignalRCore.DoNotifyConnect(aConnection: TsgcWSConnection);
begin
  FFirstMessage := True;
  DoHandshakeRequest;
  inherited;
end;

procedure TsgcWS_API_SignalRCore.DoNotifyDisconnect
  (aConnection: TsgcWSConnection);
begin
  inherited;
  DoEventSignalRCoreDisconnect(aConnection.CloseCode, aConnection.CloseReason);

  FNegotiate := True;
  FConnectionId := '';
  FAuthBearerToken := '';
end;

procedure TsgcWS_API_SignalRCore.DoNotifyError(aConnection: TsgcWSConnection);
var
  oError: TSignalRCore_Error;
begin
  oError._RawMessage := '';
  oError.Error := aConnection.MsgError;

  DoEventSignalRCoreError(oError);
end;

procedure TsgcWS_API_SignalRCore.DoNotifyException
  (aConnection: TsgcWSConnection);
var
  oError: TSignalRCore_Error;
begin
  oError._RawMessage := '';
  oError.Error := aConnection.MsgException;

  DoEventSignalRCoreError(oError);
end;

procedure TsgcWS_API_SignalRCore.DoNotifyMessage(aConnection: TsgcWSConnection);
var
  i: Integer;
  vMessage: String;
  oMessages: TsgcDelimitedStringList;
begin
  if not Assigned(aConnection) then
    exit;

  oMessages := TsgcDelimitedStringList.Create;
  Try
    oMessages.Delimiter := CS_SIGNALR_CORE_SEPARATOR;
    oMessages.DelimitedText := aConnection.MsgReceived;
    for i := 0 to oMessages.Count - 1 do
    begin
      vMessage := oMessages[i];
      if vMessage <> '' then
      begin
        JSON.Read(vMessage);
        case GetMessageType of
          srcmUndefined:
            begin
              if FFirstMessage then
              begin
                FFirstMessage := False;
                DoReceiveFirstMessage;
              end
              else
                inherited;
            end;
          srcmInvocation:
            DoReceiveInvocationMessage;
          srcmStreamItem:
            DoReceiveStreamItem;
          srcmCompletion:
            DoReceiveCompletionMessage;
          srcmStreamInvocation:
            DoReceiveStreamInvocationMessage;
          srcmCancelInvocation:
            DoReceiveCancelInvocationMessage;
          srcmPing:
            DoEventSignalRCoreKeepAlive;
          srcmClose:
            DoReceiveCloseMessage;
        end;
      end;
    end;
  Finally
    sgcFree(oMessages);
  end;
end;

procedure TsgcWS_API_SignalRCore.DoHandshakeRequest;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair(CS_SIGNALRCORE_PROTOCOL, SignalRCore.GetProtocolString);
    oJSON.AddPair(CS_SIGNALRCORE_VERSION, SignalRCore.GetVersionInteger);
    WriteData(oJSON.Text)
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcWS_API_SignalRCore.DoKeepAlive: Boolean;
begin
  Result := True;

  DoPing;
end;

procedure TsgcWS_API_SignalRCore.DoPing;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair(CS_SIGNALRCORE_TYPE, ord(srcmPing));
    WriteData(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;

end;

procedure TsgcWS_API_SignalRCore.DoReceiveCancelInvocationMessage;
var
  oCancelInvocation: TSignalRCore_CancelInvocation;
begin
  oCancelInvocation.InvocationId := GetJSONValue(CS_SIGNALRCORE_INVOCATION_ID);
  oCancelInvocation.Headers := GetJSONValue(CS_SIGNALRCORE_HEADERS);
  oCancelInvocation._RawMessage := JSON.Text;

  DoEventSignalRCoreCancelInvocation(oCancelInvocation);
end;

procedure TsgcWS_API_SignalRCore.DoReceiveCloseMessage;
var
  oClose: TSignalRCore_Close;
begin
  oClose.Error := GetJSONValue('error');
  oClose._RawMessage := JSON.Text;

  DoEventSignalRCoreClose(oClose);
end;

procedure TsgcWS_API_SignalRCore.DoReceiveCompletionMessage;
var
  oCompletion: TSignalRCore_Completion;
begin
  oCompletion.InvocationId := GetJSONValue(CS_SIGNALRCORE_INVOCATION_ID);
  oCompletion.Result := GetJSONValue(CS_SIGNALRCORE_RESULT);
  oCompletion.Error := GetJSONValue(CS_SIGNALRCORE_ERROR);
  oCompletion.Headers := GetJSONValue(CS_SIGNALRCORE_HEADERS);

  if Assigned(FThreadInvoke) then
  begin
    FThreadInvoke.Terminate;
    FThreadInvoke.Completion := oCompletion;
  end
  else
  begin
    DoEventSignalRCoreCompletion(oCompletion);
  end;
end;

procedure TsgcWS_API_SignalRCore.DoReceiveInvocationMessage;
var
  oInvocation: TSignalRCore_Invocation;
begin
  oInvocation.Target := GetJSONValue(CS_SIGNALRCORE_TARGET);
  oInvocation.Arguments := GetJSONValue(CS_SIGNALRCORE_ARGUMENTS);
  oInvocation.InvocationId := GetJSONValue(CS_SIGNALRCORE_INVOCATION_ID);
  oInvocation.Headers := GetJSONValue(CS_SIGNALRCORE_HEADERS);
  oInvocation._RawMessage := JSON.Text;

  DoEventSignalRCoreInvocation(oInvocation);
end;

procedure TsgcWS_API_SignalRCore.DoReceiveStreamInvocationMessage;
var
  oStreamInvocation: TSignalRCore_StreamInvocation;
begin
  oStreamInvocation.Target := GetJSONValue(CS_SIGNALRCORE_TARGET);
  oStreamInvocation.Arguments := GetJSONValue(CS_SIGNALRCORE_ARGUMENTS);
  oStreamInvocation.InvocationId := GetJSONValue(CS_SIGNALRCORE_INVOCATION_ID);
  oStreamInvocation.Headers := GetJSONValue(CS_SIGNALRCORE_HEADERS);
  oStreamInvocation._RawMessage := JSON.Text;

  DoEventSignalRCoreStreamInvocation(oStreamInvocation);
end;

procedure TsgcWS_API_SignalRCore.DoReceiveStreamItem;
var
  oStreamItem: TSignalRCore_StreamItem;
begin
  oStreamItem.InvocationId := GetJSONValue(CS_SIGNALRCORE_INVOCATION_ID);
  oStreamItem.Item := GetJSONValue(CS_SIGNALRCORE_ITEM);
  oStreamItem.Headers := GetJSONValue(CS_SIGNALRCORE_HEADERS);
  oStreamItem._RawMessage := JSON.Text;

  DoEventSignalRCoreStreamItem(oStreamItem);
end;

procedure TsgcWS_API_SignalRCore.DoWebSocketURL;
var
  vURL: String;
begin
  if SignalRCore.Hub <> '' then
  begin
    if Assigned(Client) then
    begin
      vURL := 'ws';
      if Client.TLS then
        vURL := vURL + 's';
      vURL := vURL + '://' + Client.Host + ':' + IntToStr(Client.Port);
      vURL := vURL + SignalRCore.GetHubString;

      DoEventSignalRCoreBeforeConnect(FConnectionId);

      if FConnectionId <> '' then
        vURL := vURL + '?id=' + FConnectionId;
      if SignalRCore.Authentication.Enabled then
      begin
        case SignalRCore.Authentication.Authentication of
          srcaRequestToken, srcaSetToken:
            begin
              if FConnectionId = '' then
                vURL := vURL + '?'
              else
                vURL := vURL + '&';
              vURL := vURL + SignalRCore.Authentication.RequestToken.QueryFieldToken + '=' + FAuthBearerToken;
            end;
        end;
      end;
      Client.URL := vURL;
    end;
  end;
end;

function TsgcWS_API_SignalRCore.GetArgumentsFromArrayOfConst
  (aArguments: Array of Const): String;

  function GetQuotedStr(const aText: String): String;
  begin
    Result := aText;
    if LeftStr(Result, 1) <> '"' then
      Result := '"' + Result;
    if RightStr(Result, 1) <> '"' then
      Result := Result + '"';
  end;

var
  i: Integer;
  vFS: TFormatSettings;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := ',';

  Result := '';
  for i := Low(aArguments) to High(aArguments) do
  begin
    if Result <> '' then
      Result := Result + ',';
    case aArguments[i].vtype of
{$IFNDEF NEXTGEN}
      vtString:
        Result := Result + GetQuotedStr(String(aArguments[i].VString^));
      vtObject:
        Result := Result + GetQuotedStr(aArguments[i].VObject.ClassName);
{$ENDIF}
      vtInteger:
        Result := Result + IntToStr(aArguments[i].vinteger);
      vtBoolean:
        begin
          if aArguments[i].VBoolean = False then
            Result := Result + 'false'
          else
            Result := Result + 'true';
        end;
{$IFDEF NEXTGEN}
{$IFDEF D10_1}
      vtChar:
        Result := Result + GetQuotedStr(String(aArguments[i].VChar));
      vtPChar:
        Result := Result + GetQuotedStr(String(aArguments[i].VPChar));
      vtAnsiString:
        Result := Result + GetQuotedStr(String(aArguments[i].VAnsiString));
{$ENDIF}
{$ENDIF}
      vtClass:
        Result := Result + GetQuotedStr(aArguments[i].VClass.ClassName);
      vtWideChar:
        Result := Result + GetQuotedStr(aArguments[i].VWidechar);
      vtWideString:
        Result := Result + GetQuotedStr(String(aArguments[i].VWideString));
{$IFDEF D2009}
      vtUnicodeString:
        Result := Result + GetQuotedStr(String(aArguments[i].VUnicodeString));
{$ENDIF}
      vtVariant:
        Result := Result + GetQuotedStr(String(aArguments[i].VVariant^));
      vtExtended:
        Result := Result + FloatToStr(aArguments[i].VExtended^, vFS);
      vtCurrency:
        Result := Result + CurrToStr(aArguments[i].VCurrency^, vFS);
    end;
  end;
  if Result <> '' then
    Result := '[' + Result + ']';
end;

function TsgcWS_API_SignalRCore.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(nil);
  Result := FJSON;
end;

function TsgcWS_API_SignalRCore.GetJSONValue(const aNode: String): Variant;
begin
  Result := '';
  if JSON.Node[aNode] <> nil then
    Result := JSON.Node[aNode].Value
end;

function TsgcWS_API_SignalRCore.GetMessageType: TwsSignalRCoreMessages;
var
  vMessageType: Integer;
begin
  Result := srcmUndefined;
  if JSON.Node['type'] <> nil then
  begin
    vMessageType := JSON.Node['type'].Value;
    case vMessageType of
      1:
        Result := srcmInvocation;
      2:
        Result := srcmStreamItem;
      3:
        Result := srcmCompletion;
      4:
        Result := srcmStreamInvocation;
      5:
        Result := srcmCancelInvocation;
      6:
        Result := srcmPing;
      7:
        Result := srcmClose;
    end;
  end;
end;

procedure TsgcWS_API_SignalRCore.DoInvoke(const aTarget: String;
  const aArguments: String = '[]'; const aInvocationId: String = '');
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair(CS_SIGNALRCORE_TYPE, ord(srcmInvocation));
    oJSON.AddPair(CS_SIGNALRCORE_TARGET, aTarget);
    if aArguments <> '' then
      oJSON.AddArray(CS_SIGNALRCORE_ARGUMENTS, aArguments);
    if aInvocationId <> '' then
      oJSON.AddPair(CS_SIGNALRCORE_INVOCATION_ID, aInvocationId);
    WriteData(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_SignalRCore.Invoke(const aTarget: String;
  const aArguments: Array of Const; const aInvocationId: String = '');
var
  vArguments: String;
begin
  vArguments := GetArgumentsFromArrayOfConst(aArguments);
  DoInvoke(aTarget, vArguments, aInvocationId);
end;

function TsgcWS_API_SignalRCore.DoInvokeAndWait(const aTarget, aArguments,
  aInvocationId: String; out Completion: TSignalRCore_Completion;
  const aTimeout: Integer = 10000): Boolean;
begin
  Result := False;

  FThreadInvoke := TThreadInvoke.Create(True);
  Try
    FThreadInvoke.Invoke(self, aTarget, aArguments, aInvocationId, aTimeout);
    FThreadInvoke.WaitFor;

    if FThreadInvoke.Completion.InvocationId = '' then
    begin
      CancelInvocation(aInvocationId);

      Completion.InvocationId := aInvocationId;
      Completion.Error := S_ERROR_SIGNALR_TIMEOUT_INVOCATION;
    end
    else
    begin
      if FThreadInvoke.Completion.Error = '' then
        Result := True;
      Completion := FThreadInvoke.Completion;
    end;
  Finally
    sgcFree(FThreadInvoke);
  End;
end;

function TsgcWS_API_SignalRCore.InvokeAndWait(const aTarget: String;
  aArguments: Array of Const; aInvocationId: String;
  out Completion: TSignalRCore_Completion;
  const aTimeout: Integer = 10000): Boolean;
var
  vArguments: String;
begin
  vArguments := GetArgumentsFromArrayOfConst(aArguments);
  Result := DoInvokeAndWait(aTarget, vArguments, aInvocationId, Completion,
    aTimeout);
end;

procedure TsgcWS_API_SignalRCore.DoInvokeStream(const aTarget: String;
  const aArguments: String = '[]'; const aInvocationId: String = '0');
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair(CS_SIGNALRCORE_TYPE, ord(srcmStreamInvocation));
    oJSON.AddPair(CS_SIGNALRCORE_TARGET, aTarget);
    oJSON.AddArray(CS_SIGNALRCORE_ARGUMENTS, aArguments);
    oJSON.AddPair(CS_SIGNALRCORE_INVOCATION_ID, aInvocationId);
    WriteData(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_SignalRCore.InvokeStream(const aTarget: String;
  const aArguments: Array of Const; const aInvocationId: String);
var
  vArguments: String;
begin
  vArguments := GetArgumentsFromArrayOfConst(aArguments);
  DoInvokeStream(aTarget, vArguments, aInvocationId);
end;

function TsgcWS_API_SignalRCore.DoInvokeStreamAndWait(const aTarget, aArguments,
  aInvocationId: String; out Completion: TSignalRCore_Completion;
  const aTimeout: Integer = 10000): Boolean;
begin
  Result := False;

  FThreadInvoke := TThreadInvoke.Create(True);
  Try
    FThreadInvoke.InvokeStream(self, aTarget, aArguments, aInvocationId,
      aTimeout);
    FThreadInvoke.WaitFor;

    if FThreadInvoke.Completion.InvocationId = '' then
    begin
      CancelInvocation(aInvocationId);

      Completion.InvocationId := aInvocationId;
      Completion.Error := S_ERROR_SIGNALR_TIMEOUT_INVOCATION;
    end
    else
    begin
      if FThreadInvoke.Completion.Error = '' then
        Result := True;
      Completion := FThreadInvoke.Completion;
    end;
  Finally
    sgcFree(FThreadInvoke);
  End;
end;

function TsgcWS_API_SignalRCore.InvokeStreamAndWait(const aTarget: String;
  const aArguments: Array of Const; const aInvocationId: String;
  out Completion: TSignalRCore_Completion;
  const aTimeout: Integer = 10000): Boolean;
var
  vArguments: String;
begin
  vArguments := GetArgumentsFromArrayOfConst(aArguments);
  Result := DoInvokeStreamAndWait(aTarget, vArguments, aInvocationId,
    Completion, aTimeout);
end;

procedure TsgcWS_API_SignalRCore.Ping;
begin
  DoPing;
end;

procedure TsgcWS_API_SignalRCore.SetSignalRCore(const Value
  : TsgcWSSignalRCore_Options);
begin
  FSignalRCore.Assign(Value);
end;

procedure TsgcWS_API_SignalRCore.StreamItem(const aInvocationId, aItem: String);
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair(CS_SIGNALRCORE_TYPE, ord(srcmStreamItem));
    oJSON.AddPair(CS_SIGNALRCORE_INVOCATION_ID, aInvocationId);
    oJSON.AddPair(CS_SIGNALRCORE_ITEM, aItem);
    WriteData(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_SignalRCore.WriteData(const aText: String);
begin
  FClient.WriteData(aText + CS_SIGNALR_CORE_SEPARATOR);
end;

constructor TsgcWSSignalRCore_Options.Create;
begin
  inherited;
  FAuthentication := TsgcWSSignalRCore_Authentication.Create;
  Protocol := srcpJSON;
  Version := srcv1_0;
end;

destructor TsgcWSSignalRCore_Options.Destroy;
begin
  sgcFree(FAuthentication);
  inherited;
end;

procedure TsgcWSSignalRCore_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSignalRCore_Options then
  begin
    Hub := TsgcWSSignalRCore_Options(aSource).Hub;
    Protocol := TsgcWSSignalRCore_Options(aSource).Protocol;
    Version := TsgcWSSignalRCore_Options(aSource).Version;
    Authentication := TsgcWSSignalRCore_Options(aSource).Authentication;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSSignalRCore_Options.GetHubString: String;
begin
  Result := FHub;
  if Result <> '' then
  begin
    if LeftStr(Result, 1) <> '/' then
      Result := '/' + Result;
  end;
end;

function TsgcWSSignalRCore_Options.GetProtocolString: String;
begin
  case Protocol of
    srcpJSON:
      Result := 'json';
  end;
end;

function TsgcWSSignalRCore_Options.GetVersionInteger: Integer;
begin
  Result := 0;
  case Version of
    srcv1_0:
      Result := 1;
  end;
end;

procedure TsgcWSSignalRCore_Options.SetAuthentication
  (const Value: TsgcWSSignalRCore_Authentication);
begin
  FAuthentication.Assign(Value);
end;

procedure TThreadInvoke.Execute;
var
  vStart: Cardinal;
begin
  inherited;
  case FMessage of
    srcmInvocation:
      FSignalRCore.DoInvoke(FTarget, FArguments, FInvocationId);
    srcmStreamInvocation:
      FSignalRCore.DoInvokeStream(FTarget, FArguments, FInvocationId);
  else
    Terminate;
  end;

  vStart := sgcGetTicks;
  while not Terminated do
  begin
    // ... wait response
    if sgcGetTickDiff(vStart, sgcGetTicks) < Cardinal(FTimeout) then
      sleep(1)
    else
      Terminate;
  end;
end;

procedure TThreadInvoke.Invoke(aSignalRCore: TsgcWS_API_SignalRCore;
  aTarget, aArguments, aInvocationId: String; aTimeout: Integer = 10000);
begin
  FSignalRCore := aSignalRCore;

  FMessage := srcmInvocation;
  FTarget := aTarget;
  FArguments := aArguments;
  FInvocationId := aInvocationId;
  FTimeout := aTimeout;

{$IFDEF D2010}
  Start;
{$ELSE}
  Resume;
{$ENDIF}
end;

procedure TThreadInvoke.InvokeStream(aSignalRCore: TsgcWS_API_SignalRCore;
  aTarget, aArguments, aInvocationId: String; aTimeout: Integer = 10000);
begin
  FSignalRCore := aSignalRCore;

  FMessage := srcmStreamInvocation;
  FTarget := aTarget;
  FArguments := aArguments;
  FInvocationId := aInvocationId;
  FTimeout := aTimeout;

{$IFDEF D2010}
  Start;
{$ELSE}
  Resume;
{$ENDIF}
end;

constructor TsgcWSSignalRCore_Authentication.Create;
begin
  inherited;
  FAuthentication := srcaRequestToken;
  FRequestToken := TsgcWSSignalRCore_Authentication_RequestToken.Create;
  FSetToken := TsgcWSSignalRCore_Authentication_SetToken.Create;
  FEnabled := False;
end;

destructor TsgcWSSignalRCore_Authentication.Destroy;
begin
  sgcFree(FSetToken);
  sgcFree(FRequestToken);
  inherited;
end;

procedure TsgcWSSignalRCore_Authentication.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSignalRCore_Authentication then
  begin
    Enabled := TsgcWSSignalRCore_Authentication(aSource).Enabled;
    Username := TsgcWSSignalRCore_Authentication(aSource).Username;
    Password := TsgcWSSignalRCore_Authentication(aSource).Password;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSSignalRCore_Authentication.SetRequestToken(const Value:
    TsgcWSSignalRCore_Authentication_RequestToken);
begin
  FRequestToken.Assign(Value);
end;

procedure TsgcWSSignalRCore_Authentication.SetSetToken(const Value:
    TsgcWSSignalRCore_Authentication_SetToken);
begin
  FSetToken.Assign(Value);
end;

constructor TsgcWSSignalRCore_Authentication_RequestToken.Create;
begin
  inherited;
  FPostFieldUsername := 'username';
  FPostFieldPassword := 'password';
  FPostFieldToken := 'token';
  FQueryFieldToken := 'access_token';
  FURL := '/account/token';
end;

procedure TsgcWSSignalRCore_Authentication_RequestToken.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSignalRCore_Authentication_RequestToken then
  begin
    PostFieldUsername := TsgcWSSignalRCore_Authentication_RequestToken(aSource)
      .PostFieldUsername;
    PostFieldPassword := TsgcWSSignalRCore_Authentication_RequestToken(aSource)
      .PostFieldPassword;
    PostFieldToken := TsgcWSSignalRCore_Authentication_RequestToken(aSource)
      .PostFieldToken;
    URL := TsgcWSSignalRCore_Authentication_RequestToken(aSource).URL;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSSignalRCore_Authentication_SetToken.Create;
begin
  inherited;
  FToken := '';
end;

procedure TsgcWSSignalRCore_Authentication_SetToken.Assign(aSource:
    TPersistent);
begin
  if aSource is TsgcWSSignalRCore_Authentication_SetToken then
  begin
    Token := TsgcWSSignalRCore_Authentication_SetToken(aSource).Token;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
