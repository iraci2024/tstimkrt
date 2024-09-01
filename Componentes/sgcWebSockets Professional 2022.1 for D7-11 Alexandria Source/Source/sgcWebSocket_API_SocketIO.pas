{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

{ ***************************************************************************
  socket.io protocol

  message types:

  0: open (Sent from the server when a new transport is opened (recheck))
  1: close (Request the close of this transport but does not shutdown the connection itself.)
  2: ping (Sent by the client. Server should answer with a pong packet containing the same data)
    example
      client sends: 2probe
      server sends: 3probe
  3: pong (Sent by the server to respond to ping packets.)
  4: string message (actual message, client and server should call their callbacks with the data.)
    example:
      42/chat,["join","{room:1{"]

      4 is the message packet type in the engine.io protocol
      2 is the EVENT type in the socket.io protocol
      /chat is the data which is processed by socket.io
      socket.io will fire the “join” event
      will pass "room: 1" data. It is possible to omit namespace only when it is /.
  5: upgrade (Before engine.io switches a transport, it tests, if server and client can communicate over this transport. If this test succeed, the client sends an upgrade packets which requests the server to flush its cache on the old transport and switch to the new transport.)
  6: noop (A noop packet. Used primarily to force a poll cycle when an incoming websocket connection is received.)


  WebSocket Messages:

  2probe                                        => Engine.IO probe request
  3probe                                        => Engine.IO probe response
  5                                             => Engine.IO "upgrade" packet type
  42["hello"]
  42["world"]
  40/admin,                                     => request access to the admin namespace (Socket.IO "CONNECT" packet)
  40/admin,{"sid":"-G5j-67EZFp-q59rADQM"{       => grant access to the admin namespace
  42/admin,1["tellme"]                          => Socket.IO "EVENT" packet with acknowledgement
  461-/admin,1[{"_placeholder":true,"num":0{]   => Socket.IO "BINARY_ACK" packet with a placeholder
  <binary>                                      => the binary attachment (sent in the following frame)
  ... after a while without message
  2                                             => Engine.IO "ping" packet type
  3                                             => Engine.IO "pong" packet type
  1                                             => Engine.IO "close" packet type

  *************************************************************************** }

unit sgcWebSocket_API_SocketIO;

interface

{$I sgcVer.inc}

{$IFDEF SGC_APIS}

uses
  Classes, SysUtils, StrUtils,
  // indy
  {$IFDEF SGC_INDY}sgcIdSSL{$ELSE}IdSSL{$ENDIF},
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Types;

Type
  TsgcWSSocketIOHTTPRequest = class
  private
    FHeaders: TStringList;
    function GetHeaders: TStringList;
  public
    property Headers: TStringList read GetHeaders write FHeaders;
  public
    destructor Destroy; override;
  end;

  TsgcWSOnHTTPConnectionSSL = procedure(Sender: TObject; aSSLHandler:
      TIdSSLIOHandlerSocketBase) of object;
  TsgcWSOnSocketIOHTTPRequest = procedure(Sender: TObject; aRequest: TsgcWSSocketIOHTTPRequest) of object;
  TsgcWSOnSocketIOAfterConnect = procedure(Sender: TObject) of object;

  TsgcWSSocketIO_Options = class(TPersistent)
  private
    FHandShakeTimestamp: Boolean;
    FAPI: TwsSocketIOAPI;
    FBase64: Boolean;
    FEncodeParameters: Boolean;
    FHandShakeCustomURL: String;
    FNamespace: string;
    FParameters: string;
    FPolling: Boolean;
    FUserAgent: String;
    FPath: String;
    function GetNamespace: string;
    function GetParameters: string;
  public
    constructor Create; virtual;
    procedure Assign(aSource: TPersistent); override;
  published
    property HandShakeTimestamp: Boolean read FHandShakeTimestamp write
      FHandShakeTimestamp;
    property API: TwsSocketIOAPI read FAPI write FAPI;
    property Base64: Boolean read FBase64 write FBase64;
    property EncodeParameters: Boolean read FEncodeParameters write
        FEncodeParameters;
    property HandShakeCustomURL
      : string read FHandShakeCustomURL write FHandShakeCustomURL;
    property Namespace: string read GetNamespace write FNamespace;
    property Parameters: string read GetParameters write FParameters;
    property Polling: Boolean read FPolling write FPolling;
    property UserAgent: String read FUserAgent write FUserAgent;
    property Path: String read FPath write FPath;
  end;

  TsgcWS_API_SocketIO = class(TsgcWSAPI_client)
    { from TsgcWSComponent_Base }
  protected
    FWSConnection: TsgcWSConnection;
  protected
    procedure DoNotifyConnect(aConnection: TsgcWSConnection); override;
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSAPI }
  protected
    function IsInternalMessage(const aMessage: String): Boolean; override;
    procedure DoBeforeConnect; override;
    { from TsgcWSAPI }

    { from TsgcWSAPI_client }
  protected
    function DoKeepAlive: Boolean; override;
    { from TsgcWSAPI_client }

    { options }
  private
    FSocketIO: TsgcWSSocketIO_Options;
  public
    property SocketIO: TsgcWSSocketIO_Options read FSocketIO write FSocketIO;
    { options }

    { helpers }
  private
    FFirstMessage: Boolean;
  private
    function GetProtocolHTTP: string;
    function GetTimeStamp: string;
    procedure DoSendNameSpace; virtual;
    { helpers }

    { session }
  private
    function DoGetSessionId(aText: String): Boolean;
  private
    function DoSessionResponse(aStream: TStringStream): Boolean;
    function DoGetSession: Boolean;
  private
    function GetAPI_URL: string;
    procedure DoSetURL;
  protected
    function DoGetSessionURL: string; virtual;
    { session }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { properties }
  private
    FIO_CloseTimeout: Integer;
    FIO_HeartBeatTimeout: Integer;
    FIO_SessionId: String;
  public
    property IO_CloseTimeout: Integer read FIO_CloseTimeout;
    property IO_HeartBeatTimeout: Integer read FIO_HeartBeatTimeout;
    property IO_SessionId: String read FIO_SessionId;

    { properties }

    { methods }
  private
    procedure DoClientException(E: Exception);
  private
    procedure DoPing;
  public
    procedure Ping;
    { methods }

    { events }
  private
    FOnAfterConnect: TsgcWSOnSocketIOAfterConnect;
    FOnHTTPConnectionSSL: TsgcWSOnHTTPConnectionSSL;
    FOnHTTPRequest: TsgcWSOnSocketIOHTTPRequest;
  public
    property OnAfterConnect: TsgcWSOnSocketIOAfterConnect read FOnAfterConnect
        write FOnAfterConnect;
    property OnHTTPConnectionSSL: TsgcWSOnHTTPConnectionSSL read
        FOnHTTPConnectionSSL write FOnHTTPConnectionSSL;
    property OnHTTPRequest: TsgcWSOnSocketIOHTTPRequest read FOnHTTPRequest write
        FOnHTTPRequest;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  // sgc
  sgcWebSocket_Helpers, sgcWebSocket_Const, sgcJSON, sgcBase_Const,
  sgcBase_Helpers, sgcHTTP_Client, sgcWebSocket_Client;


type
  TsgcWSComponent_WSClient_Hack = class(TsgcWSComponent_WSClient);

constructor TsgcWSSocketIO_Options.Create;
begin
  inherited;
  HandShakeTimestamp := True;
  HandShakeCustomURL := '';
  Namespace := '';
  API := ioAPI4;
  Polling := True;
  UserAgent := CS_USER_AGENT;
  Parameters := '';
  EncodeParameters := False;
  Path := '';
end;

procedure TsgcWSSocketIO_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSocketIO_Options then
  begin
    HandShakeTimestamp := TsgcWSSocketIO_Options(aSource).HandShakeTimestamp;
    API := TsgcWSSocketIO_Options(aSource).API;
    HandShakeCustomURL := TsgcWSSocketIO_Options(aSource).HandShakeCustomURL;
    Namespace := TsgcWSSocketIO_Options(aSource).Namespace;
    Polling := TsgcWSSocketIO_Options(aSource).Polling;
    UserAgent := TsgcWSSocketIO_Options(aSource).UserAgent;
    Parameters := TsgcWSSocketIO_Options(aSource).Parameters;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSSocketIO_Options.GetNamespace: string;
begin
  if FNamespace <> '' then
  begin
    if LeftStr(FNamespace, 1) <> '/' then
      FNamespace := '/' + FNamespace;
  end;
  Result := FNamespace;
end;

function TsgcWSSocketIO_Options.GetParameters: string;
begin
  if FParameters <> '' then
  begin
    if LeftStr(FParameters, 1) = '/' then
      FParameters := MidStr(FParameters, 2, Length(FParameters) - 1);
    if LeftStr(FParameters, 1) = '?' then
      FParameters := MidStr(FParameters, 2, Length(FParameters) - 1);
    if LeftStr(FParameters, 1) <> '&' then
      FParameters := '&' + FParameters;
    if RightStr(FParameters, 1) = '/' then
      FParameters := MidStr(FParameters, 1, Length(FParameters) - 1);
  end;
  Result := FParameters;
end;

constructor TsgcWS_API_SocketIO.Create(aOwner: TComponent);
begin
  inherited;
  RawMessages := True;
  FSocketIO := TsgcWSSocketIO_Options.Create;
end;

destructor TsgcWS_API_SocketIO.Destroy;
begin
  sgcFree(FSocketIO);
  inherited;
end;

procedure TsgcWS_API_SocketIO.DoBeforeConnect;
begin
  inherited;
  if SocketIO.Polling then
  begin
    if DoGetSession then
      DoSetURL
    else
      DoClientException(Exception.Create(S_UNABLE_GET_SESSION_SOCKETIO));
  end
  else
    DoSetURL;
end;

function TsgcWS_API_SocketIO.DoGetSession: Boolean;
var
  oHTTP: TsgcIdHTTP;
  oStream: TStringStream;
  oRequest: TsgcWSSocketIOHTTPRequest;
begin
  result := False;

  oHTTP := TsgcIdHTTP.Create(nil);
  oStream := TStringStream.Create('');
  Try
    Try
      if Assigned(Client) then
        oHTTP.TLSOptions.Assign(Client.TLSOptions);
      oHTTP.ReadTimeout := 10000;
      oHTTP.Request.UserAgent := SocketIO.UserAgent;
      if Assigned(FOnHTTPRequest) then
      begin
        oRequest := TsgcWSSocketIOHTTPRequest.Create;
        Try
          FOnHTTPRequest(self, oRequest);
          oHTTP.request.CustomHeaders.Text := oRequest.Headers.Text;
        Finally
          sgcFree(oRequest);
        End;
      end;
      if Client.InheritsFrom(TsgcWSClient) then
        oHTTP.Proxy.Assign(TsgcWSClient(Client).Proxy);

      oHTTP.Get(DoGetSessionURL, oStream);

      result := DoSessionResponse(oStream);
    Except
      On E: Exception do
        DoClientException(E);
    End;
  Finally
    sgcFree(oStream);
    sgcFree(oHTTP);
  End;
end;

function TsgcWS_API_SocketIO.DoGetSessionId(aText: String): Boolean;
var
  oJSON: IsgcJSON;
begin
  result := (SocketIO.Polling = False) or (sgcContainsText(aText, 'websocket'));
  if result then
  begin
    oJSON := GetJSONInstance;
    Try
      oJSON.Read(AnsiMidStr(aText,
          AnsiPos('{', aText), Length(aText)));
      if oJSON.Node['sid'] <> nil then
        FIO_SessionId := oJSON.Node['sid'].Value;
      TryStrToInt(oJSON.Node['pingInterval'].Value, FIO_HeartBeatTimeout);
      TryStrToInt(oJSON.Node['pingTimeout'].Value, FIO_CloseTimeout);

      case SocketIO.API of
        ioAPI0, ioAPI1, ioAPI2:
        begin
          if FIO_HeartBeatTimeOut > 1000 then
          begin
            if not Client.HeartBeat.Enabled then
            begin
              Client.HeartBeat.Interval := Trunc(FIO_HeartBeatTimeOut / 1000);
              if FIO_CloseTimeout > 1000 then
                Client.HeartBeat.Timeout := Trunc(FIO_CloseTimeOut / 1000);
              Client.HeartBeat.Enabled := True;
              TsgcWSComponent_WSClient_Hack(Client).DoStartHeartBeat;
            end;
          end;
        end
        else
          Client.HeartBeat.Enabled := False;
      end;
      begin

      end;
    Finally
      FreeJSONInstance(oJSON);
    End;
  end;
end;

function TsgcWS_API_SocketIO.DoGetSessionURL: string;
begin
  if SocketIO.HandShakeCustomURL = '' then
  begin
    result := GetProtocolHTTP;

    case SocketIO.API of
      ioAPI0:
        begin
          result := result + FClient.Host + ':' + IntToStr(FClient.Port) +
            CS_SOCKETIO_URL_API + SocketIO.Path + GetAPI_URL;
          if SocketIO.HandShakeTimestamp then
            result := result + '/?t=' + GetTimeStamp;
          result := result + SocketIO.HandShakeCustomURL;
        end;
      ioAPI1, ioAPI2, ioAPI3, ioAPI4:
        begin
          result := result + FClient.Host + ':' + IntToStr(FClient.Port)
            + CS_SOCKETIO_URL_API + SocketIO.Path + GetAPI_URL +
            '&transport=polling&b64=true&t=' + GetTimeStamp;
        end;
    end;
  end
  else
    result := SocketIO.HandShakeCustomURL;
end;

function TsgcWS_API_SocketIO.DoKeepAlive: Boolean;
begin
  Result := True;

  DoPing;
end;

procedure TsgcWS_API_SocketIO.DoSendNameSpace;
begin
  if SocketIO.Namespace <> '' then
    FClient.WriteData('40' + SocketIO.Namespace)
  else if (SocketIO.API = ioAPI3) or (SocketIO.API = ioAPI4) then
    FClient.WriteData('40');
end;

procedure TsgcWS_API_SocketIO.DoNotifyConnect(aConnection: TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;

  case SocketIO.API of
    ioAPI1, ioAPI2, ioAPI3, ioAPI4:
      begin
        FFirstMessage := True;
        FClient.WriteData('2probe');
      end;
  end;
end;

procedure TsgcWS_API_SocketIO.DoNotifyDisconnect(aConnection: TsgcWSConnection);
begin
  inherited;
  FWSConnection := nil;
end;

procedure TsgcWS_API_SocketIO.DoClientException(E: Exception);
begin
  if Assigned(FClient) then
    TsgcWSComponent_WSClient_Hack(FClient).DoException(FWSConnection, E.Message, E);
end;

procedure TsgcWS_API_SocketIO.DoPing;
begin
  case SocketIO.API of
    ioAPI0:
      FClient.WriteData('2::');
    ioAPI1, ioAPI2:
      FClient.WriteData('2');
    ioAPI3, ioAPI4:
      // API 3.*+ doesn't send ping to server
  end;
  if Assigned(FWSConnection) then
    FWSConnection.LastPing := Now
end;

procedure TsgcWS_API_SocketIO.DoSetURL;
var
  vParameters: string;
  vURL: String;
begin
  vParameters := '';
  case SocketIO.API of
    ioAPI0:
      vParameters := vParameters + CS_SOCKETIO_URL_API + GetAPI_URL + '/websocket/' +
        IO_SessionId;
    ioAPI1, ioAPI2, ioAPI3, ioAPI4:
      begin
        vParameters := vParameters + CS_SOCKETIO_URL_API + SocketIO.Path + GetAPI_URL +
          '&transport=websocket';
        if SocketIO.Polling then
          vParameters := vParameters + '&sid=' + IO_SessionId;
        if SocketIO.Base64 then
          vParameters := vParameters + '&b64=true';
      end;
  end;
  if SocketIO.Parameters <> '' then
    vParameters := vParameters + SocketIO.Parameters;
  if SocketIO.EncodeParameters then
    vParameters := URIEncode(vParameters);
  vURL := 'ws';
  if FClient.TLS then
    vURL := vURL + 's';
  vURL := vURL + '://' + FClient.Host + ':' + IntToStr(FClient.Port) + vParameters;
  FClient.URL := vURL;
end;

function TsgcWS_API_SocketIO.DoSessionResponse(aStream: TStringStream): Boolean;
var
  oList: TsgcDelimitedStringList;
begin
  result := False;

  case SocketIO.API of
    ioAPI0:
      begin
        oList := TsgcDelimitedStringList.Create;
        Try
          oList.Delimiter := ':';
          oList.DelimitedText := aStream.DataString;
          if oList.count > 3 then
          begin
            result := sgcContainsText(oList[3], 'websocket');
            if result then
            begin
              FIO_SessionId := oList[0];
              TryStrToInt(oList[1], FIO_HeartBeatTimeout);
              TryStrToInt(oList[2], FIO_CloseTimeout);
            end;
          end;
        Finally
          sgcFree(oList);
        End;
      end;
    ioAPI1, ioAPI2, ioAPI3, ioAPI4:
      result := DoGetSessionId(aStream.DataString);
  end;
end;

function TsgcWS_API_SocketIO.GetProtocolHTTP: string;
begin
  if FClient.TLS then
    result := 'https://'
  else
    result := 'http://';
end;

function TsgcWS_API_SocketIO.GetAPI_URL: string;
begin
  case SocketIO.API of
    ioAPI0: result := CS_SOCKETIO_URL_API0;
    ioAPI1, ioAPI2: result := CS_SOCKETIO_URL_API1_2;
    ioAPI3: result := CS_SOCKETIO_URL_API3;
    ioAPI4: result := CS_SOCKETIO_URL_API4;
  end;
end;

function TsgcWS_API_SocketIO.GetTimeStamp: string;
begin
  case SocketIO.API of
    ioAPI0:
      result := IntToStr(Round(Now - 25569) * 86400);
    ioAPI1, ioAPI2, ioAPI3, ioAPI4:
      result := IntToStr(Round(Now - 25569) * 86400) + '-0';
  end;
end;

function TsgcWS_API_SocketIO.IsInternalMessage(const aMessage: String): Boolean;
begin
  Result := False;
  if LeftStr(aMessage, 1) = '0' then
    DoGetSessionId(aMessage)
  else if aMessage = '2' then
    FClient.WriteData('3')
  else if ((aMessage = '3probe') or (aMessage = '3')) then
  begin
    FWSConnection.LastPong := Now;

    Result := True;
    FClient.WriteData('5');
    if FFirstMessage then
    begin
      FFirstMessage := False;
      DoSendNameSpace;
      if Assigned(FOnAfterConnect) then
        FOnAfterConnect(self);
    end;
  end;
end;

procedure TsgcWS_API_SocketIO.Ping;
begin
  DoPing;
end;

destructor TsgcWSSocketIOHTTPRequest.Destroy;
begin
  sgcFree(FHeaders);
  inherited;
end;

function TsgcWSSocketIOHTTPRequest.GetHeaders: TStringList;
begin
  if not Assigned(FHeaders) then
    FHeaders := TStringList.Create;
  Result := FHeaders;
end;

{$ENDIF}

end.
