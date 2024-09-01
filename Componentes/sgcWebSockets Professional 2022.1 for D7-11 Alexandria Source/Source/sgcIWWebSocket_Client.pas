{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcIWWebSocket_Client;

interface

{$I sgcVer.inc}

{$IFDEF IWIX}

uses
  Classes, SysUtils, Contnrs, Types, StrUtils,
  IWControl, IWRenderContext, IWHTMLTag, IWScriptEvents, {$IFNDEF IWXI}SWSystem,{$ENDIF}
  IWXMLTag, IWBaseInterfaces, IWApplication, {$IFNDEF IWXI}IWVCLClasses,{$ENDIF}
  sgcWebSocket_Classes, sgcWebSocket_Client, sgcWebSocket_Types;

type
  TsgcIWWSClient = Class;

  TOnMessageEvent = procedure(Sender: TObject; const Text: string) of object;

  TsgcIWWSProtocol = class(TComponent)

  { from TComponent }
    procedure Loaded; override;  
    procedure Notification(AComponent : TComponent; Operation : TOperation);
        override;
  { from TComponent }

  { client }
  private
    FClient: TsgcIWWSClient;
  protected
    procedure SetClient(const Value: TsgcIWWSClient); virtual;
  public
    property Client: TsgcIWWSClient read FClient write SetClient;
  { client }

  { protocol }
  protected
    FProtocol: string;
  public
    property Protocol: string read FProtocol;
  { protocol }

  { events }
  private
    FOnAsyncConnect: TNotifyEvent;
    FOnAsyncDisconnect: TNotifyEvent;
    FOnAsyncError: TOnMessageEvent;
    FOnAsyncMessage: TOnMessageEvent;
  protected
    procedure DoNotifyConnect; virtual;
    procedure DoNotifyDisconnect; virtual;
    procedure DoNotifyError(const aMessage: String); virtual;
    procedure DoNotifyMessage(const aMessage: String); virtual;
  public
    property OnAsyncConnect: TNotifyEvent read FOnAsyncConnect write
        FOnAsyncConnect;
    property OnAsyncDisconnect: TNotifyEvent read FOnAsyncDisconnect write
        FOnAsyncDisconnect;
    property OnAsyncError: TOnMessageEvent read FOnAsyncError write FOnAsyncError;
    property OnAsyncMessage: TOnMessageEvent read FOnAsyncMessage write
        FOnAsyncMessage;
  { events }

  { version }
  private
    function GetVersion: String;  
  public
    property Version: String read GetVersion;
  end;

  TsgcIWTransports = class(TPersistent)
  private
    FEmulation: Boolean;
    FWebSockets: Boolean;
  published
    property Emulation: Boolean read FEmulation write FEmulation;
    property WebSockets: Boolean read FWebSockets write FWebSockets;
  end;


  TsgcIWWSClient = class(TIWCustomControl, IIWSubmitControl)

  { from sgcWebSocket_Client }
  private
    FWSClient: TsgcWSClient;
    function GetWSClient: TsgcWSClient;
  private
    procedure DoEmulationEvent(AParams: TStringList);
  protected
    procedure OnEmulateConnectEvent(Connection: TsgcWSConnection);
    procedure OnEmulateDisconnectEvent(Connection: TsgcWSConnection; Code: Integer);
    procedure OnEmulateErrorEvent(Connection: TsgcWSConnection; const Error:
        string);
    procedure OnEmulateMessageEvent(Connection: TsgcWSConnection; const Text:
        string);
  protected
    property WSClient: TsgcWSClient read GetWSClient;
  { from sgcWebSocket_Client }

  { protocol }
  private
    FProtocolObjectList: TObjectList;
  protected
    FProtocol: String;
    function GetProtocols: String;
    procedure RegisterProtocol(aObject: TsgcIWWSProtocol); virtual;
    procedure UnRegisterProtocol(aObject: TsgcIWWSProtocol); virtual;
  public
    property Protocol: String read FProtocol;
  { protocol }

  private
    FSubmitParam: String;
  private
    FAction: Integer;
    FHost: String;
    FJSOpen: TStringList;
    FJSMessage: TStringList;
    FJSClose: TStringList;
    FJSError: TStringList;
    FOnAsyncDisconnect: TNotifyEvent;
    FOnAsyncMessage: TOnMessageEvent;
    FOnAsyncConnect: TNotifyEvent;
    FOnAsyncError: TOnMessageEvent;
    FOnAsyncEmulation: TNotifyEvent;
    FPort: Integer;
    FWriteText: String;
    FTLS: Boolean;
    FConnected: Boolean;
    FTransport: TwsTransports;
    FTransports: TsgcIWTransports;
    FURL: String;
    FParameters: String;
  private
    procedure DoCloseEvent(AParams: TStringList);
    procedure DoOpenEvent(AParams: TStringList);
    procedure DoMessageEvent(aParams: TStringList);
    procedure DoErrorEvent(aParams: TStringList);
    procedure SetJSClose(const Value: TStringList);
    procedure SetJSMessage(const Value: TStringList);
    procedure SetJSOpen(const Value: TStringList);
    procedure SetJSError(const Value: TStringList);
  private
    procedure DoNoWebsocketSupportEvent(AParams: TStringList);
  private
    function GetConnected: Boolean;
    function GetTransports: TsgcIWTransports;
    function GetVersion: String;
    procedure SetTransports(const Value: TsgcIWTransports);
    procedure SetTLS(const Value: Boolean);
    procedure SetURL(const Value: String);
  protected
    FOnSubmit: TNotifyEvent;
    procedure Dispose(ADispose: Boolean); override;
    procedure DoSubmit;
    procedure HookEvents(AContext: TIWPageContext40; AScriptEvents:
        TIWScriptEvents); override;
    procedure InitControl; override;
    procedure OnItemsChange(ASender : TObject);
    {$IFDEF IWXI}
    function RenderAsync(AContext: TIWCompContext): TIWXMLTag; override;
    procedure RenderScripts(AComponentContext: TIWCompContext); override;
    {$ELSE}
    function RenderAsync(AContext: TIWBaseHTMLComponentContext): TIWXMLTag;
        override;
    procedure RenderScripts(AComponentContext: TIWBaseHTMLComponentContext);
        override;
    {$ENDIF}
    procedure Submit(const AValue: string); override;
  public
    function GetSubmitParam: String;
    {$IFDEF IWXI}
    function RenderHTML(AContext: TIWCompContext): TIWHTMLTag; override;
    {$ELSE}
    function RenderHTML(AContext: TIWBaseHTMLComponentContext): TIWHTMLTag;
        override;
    {$ENDIF}
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure Open;
    procedure Close;
    procedure WriteData(const aText: string);
  public
    property Connected: Boolean read GetConnected;
  public
    property JSOpen: TStringList read FJSOpen write SetJSOpen;
    property JSMessage: TStringList read FJSMessage write SetJSMessage;
    property JSClose: TStringList read FJSClose write SetJSClose;
    property JSError: TStringList read FJSError write SetJSError;
  public
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property TLS: Boolean read FTLS write SetTLS;
    property URL: String read FURL write SetURL;
    property Parameters: String read FParameters write FParameters;
  public
    property Transport: TwsTransports read FTransport;
    property Transports: TsgcIWTransports read GetTransports write SetTransports;
  public
    property OnAsyncDisconnect: TNotifyEvent read FOnAsyncDisconnect write
        FOnAsyncDisconnect;
    property OnAsyncMessage: TOnMessageEvent read FOnAsyncMessage write
        FOnAsyncMessage;
    property OnAsyncConnect: TNotifyEvent read FOnAsyncConnect write
        FOnAsyncConnect;
    property OnSubmit: TNotifyEvent read FOnSubmit write FOnSubmit;
    property OnAsyncError: TOnMessageEvent read FOnAsyncError write FOnAsyncError;
    property OnAsyncEmulation: TNotifyEvent read FOnAsyncEmulation write FOnAsyncEmulation;
  public
    property Version: String read GetVersion;
  end;
{$ENDIF}

implementation

{$IFDEF IWIX}

uses sgcBase_Const, sgcWebSocket_Const, sgcWebSocket_Helpers;

constructor TsgcIWWSClient.Create(aOwner: TComponent);
begin
  inherited;
  FProtocolObjectList := TObjectList.Create;
  FProtocolObjectList.OwnsObjects := False;

  Transports.WebSockets := True;
  FTransport := wsWebSockets;
end;

destructor TsgcIWWSClient.Destroy;
begin
  sgcFree(FJSOpen);
  sgcFree(FJSMessage);
  sgcFree(FJSClose);
  sgcFree(FJSError);
  sgcFree(FProtocolObjectList);
  sgcFree(FTransports);
  inherited;
end;

procedure TsgcIWWSClient.Close;
begin
  if Connected then
  begin
    FAction := 2;
    FConnected := False;
    Invalidate;
  end;
end;

procedure TsgcIWWSClient.Dispose(ADispose: Boolean);
begin
  sgcFree(FJSOpen);
  sgcFree(FJSMessage);
  sgcFree(FJSClose);
  sgcFree(FJSError);
  inherited;
end;

procedure TsgcIWWSClient.Open;
begin
  if not Connected then
  begin
    FAction := 1;
    Invalidate;
  end;
end;

procedure TsgcIWWSClient.DoCloseEvent(AParams: TStringList);
var
  i: Integer;
begin
  FConnected := False;
  if Protocol <> '' then
  begin
    for i := 0 to FProtocolObjectList.Count - 1 do
    begin
      if TsgcIWWSProtocol(FProtocolObjectList[i]).Protocol = Protocol then
        TsgcIWWSProtocol(FProtocolObjectList[i]).DoNotifyDisconnect;
    end;
  end
  else
  begin
    if Assigned(FOnAsyncDisconnect) then
      FOnAsyncDisconnect(Self);
  end;
end;

procedure TsgcIWWSClient.DoNoWebsocketSupportEvent(AParams: TStringList);
begin
  FConnected := False;
  FTransport := wsEmulation;
end;

procedure TsgcIWWSClient.DoErrorEvent(aParams: TStringList);
var
  i: Integer;
begin
  FConnected := True;
  if Protocol <> '' then
  begin
    for i := 0 to FProtocolObjectList.Count - 1 do
    begin
      if TsgcIWWSProtocol(FProtocolObjectList[i]).Protocol = Protocol then
        TsgcIWWSProtocol(FProtocolObjectList[i]).DoNotifyError(aParams.Values['x']);
    end;
  end
  else
  begin
    if Assigned(FOnAsyncError) then
      FOnAsyncError(Self, aParams.Values['x']);
  end;
end;

procedure TsgcIWWSClient.DoOpenEvent(AParams: TStringList);
var
  i: Integer;
begin
  FConnected := True;
  if Protocol <> '' then
  begin
    for i := 0 to FProtocolObjectList.Count - 1 do
    begin
      if TsgcIWWSProtocol(FProtocolObjectList[i]).Protocol = Protocol then
        TsgcIWWSProtocol(FProtocolObjectList[i]).DoNotifyConnect;
    end;
  end
  else
  begin
    if Assigned(FOnAsyncConnect) then
      FOnAsyncConnect(Self);
  end;
end;

procedure TsgcIWWSClient.DoMessageEvent(aParams: TStringList);
var
  i: Integer;
begin
  FConnected := True;
  if Protocol <> '' then
  begin
    for i := 0 to FProtocolObjectList.Count - 1 do
    begin
      if TsgcIWWSProtocol(FProtocolObjectList[i]).Protocol = Protocol then
        TsgcIWWSProtocol(FProtocolObjectList[i]).DoNotifyMessage(aParams.Values['x']);
    end;
  end
  else
  begin
    if Assigned(FOnAsyncMessage) then
      FOnAsyncMessage(Self, aParams.Values['x']);
  end;
end;

procedure TsgcIWWSClient.DoSubmit;
begin
  if Assigned(OnSubmit) then
    OnSubmit(Self);
end;

procedure TsgcIWWSClient.DoEmulationEvent(AParams: TStringList);
begin
  if Assigned(FOnAsyncEmulation) then FOnAsyncEmulation(self);
  Invalidate;
end;

function TsgcIWWSClient.GetConnected: Boolean;
begin
  if FTransport = wsEmulation then
    Result := WSClient.Active
  else
    Result := FConnected;
end;

function TsgcIWWSClient.GetProtocols: String;
var
  i: Integer;
  oList: TStringList;
begin
  Result := '';
  oList := TStringList.Create;
  oList.Delimiter := CS_DELIMITER;
  oList.Sorted := True;
  Try
    for i := 0 to FProtocolObjectList.Count - 1 do
      oList.Add(TsgcIWWSProtocol(FProtocolObjectList[i]).Protocol);
    result := oList.CommaText;
  Finally
    sgcFree(oList);
  End;
end;

function TsgcIWWSClient.GetTransports: TsgcIWTransports;
begin
  if not Assigned(FTransports) then
    FTransports := TsgcIWTransports.Create;
  Result := FTransports;
end;

function TsgcIWWSClient.GetSubmitParam: String;
begin
  Result := FSubmitParam;
end;

function TsgcIWWSClient.GetVersion: String;
begin
  Result := CS_VERSION;
end;

function TsgcIWWSClient.GetWSClient: TsgcWSClient;
begin
  if not Assigned(FWSClient) then
  begin
    FWSClient := TsgcWSClient.Create(self);

    FWSClient.Host := Host;
    FWSClient.Port := Port;
    FWSClient.TLS := TLS;

    FWSClient.OnConnect := OnEmulateConnectEvent;
    FWSClient.OnMessage := OnEmulateMessageEvent;
    FWSClient.OnDisconnect := OnEmulateDisconnectEvent;
    FWSClient.OnError := OnEmulateErrorEvent;

  end;
  Result := FWSClient;
end;

procedure TsgcIWWSClient.HookEvents(AContext: TIWPageContext40; AScriptEvents:
    TIWScriptEvents);
begin
  inherited HookEvents(AContext, AScriptEvents);
  if Transports.WebSockets then
  begin
    AScriptEvents.HookEvent('OnWSOpen' + HTMLNAME, 'if (document.getElementById(''' + 'WS' + HTMLNAME + ''')) {false} else return true;');
    AScriptEvents.HookEvent('OnWSMessage' + HTMLNAME, 'if (document.getElementById(''' + 'WS' + HTMLNAME + ''')) {false} else return true;');
    AScriptEvents.HookEvent('OnWSClose' + HTMLNAME, 'if (document.getElementById(''' + 'WS' + HTMLNAME + ''')) {false} else return true;');
    AScriptEvents.HookEvent('OnWSError' + HTMLNAME, 'if (document.getElementById(''' + 'WS' + HTMLNAME + ''')) {false} else return true;');
  end;
  if Transports.Emulation then
    AScriptEvents.HookEvent('OnWSEmulation' + HTMLNAME, 'if (document.getElementById(''' + 'WS' + HTMLNAME + ''')) {false} else return true;');
end;

procedure TsgcIWWSClient.InitControl;
begin
  inherited;
  FNeedsFormTag := True;
  FJSOpen := TStringList.Create;
  FJSOpen.OnChange := OnItemsChange;
  FJSMessage := TStringList.Create;
  FJSMessage.OnChange := OnItemsChange;
  FJSClose := TStringList.Create;
  FJSClose.OnChange := OnItemsChange;
  FJSError := TStringList.Create;
  FJSError.OnChange := OnItemsChange;
end;

procedure TsgcIWWSClient.OnEmulateConnectEvent(Connection: TsgcWSConnection);
begin
  DoOpenEvent(nil);
end;

procedure TsgcIWWSClient.OnEmulateDisconnectEvent(Connection: TsgcWSConnection;
    Code: Integer);
begin
  DoCloseEvent(nil);
end;

procedure TsgcIWWSClient.OnEmulateErrorEvent(Connection: TsgcWSConnection;
    const Error: string);
var
  oList: TStringList;
begin
  oList := TStringList.Create;
  Try
    oList.Add('x=' + Error);
    DoErrorEvent(oList);
  Finally
    sgcFree(oList);
  End;
end;

procedure TsgcIWWSClient.OnEmulateMessageEvent(Connection: TsgcWSConnection;
    const Text: string);
var
  oList: TStringList;
begin
  oList := TStringList.Create;
  Try
    oList.Add('x=' + Text);
    DoMessageEvent(oList);
  Finally
    sgcFree(oList);
  End;
end;

procedure TsgcIWWSClient.OnItemsChange(ASender : TObject);
begin
  DoRefreshControl := True;
  Invalidate;
end;

procedure TsgcIWWSClient.RegisterProtocol(aObject: TsgcIWWSProtocol);
begin
  FProtocolObjectList.Add(aObject);
end;

{$IFDEF IWXI}
function TsgcIWWSClient.RenderAsync(AContext: TIWCompContext): TIWXMLTag;
{$ELSE}
function TsgcIWWSClient.RenderAsync(AContext: TIWBaseHTMLComponentContext):
    TIWXMLTag;
{$ENDIF}
begin
  case FAction of
    1:  begin
          case FTransport of
            wsWebSockets:
              AContext.WebApplication.CallBackResponse.AddJavaScriptToExecute('WS' + HTMLNAME + '_create();');
            wsEmulation:
              begin
                AContext.WebApplication.CallBackResponse.AddJavaScriptToExecute('window.' + HTMLName + '.startTimer();');
                if not WSClient.Active then
                  WSClient.Active := False;
                WSClient.Active := True;
              end;
          end;
        end;
    2:  begin
          Case FTransport of
            wsWebSockets:
              AContext.WebApplication.CallBackResponse.AddJavaScriptToExecute('WS' + HTMLNAME + '_close();');
            wsEmulation:
              begin
                AContext.WebApplication.CallBackResponse.AddJavaScriptToExecute('window.' + HTMLName + '.stopTimer();');
                WSClient.Active := False;
              end;
          End;
        end;
    3:  begin
          if FWriteText <> '' then
          begin
            case FTransport of
              wsWebSockets:
                AContext.WebApplication.CallBackResponse.AddJavaScriptToExecute('WS' + HTMLNAME + '_send("' + StringReplace(FWriteText, '"', '\"', [rfReplaceAll]) + '")');
              wsEmulation:
                begin
                  if WSClient.Active then
                    WSClient.WriteData(FWriteText);
                end;
            end;
            FWriteText := '';
          end;
        end;
  end;
  FAction := 0;

  Result := nil;
end;


{$IFDEF IWXI}
function TsgcIWWSClient.RenderHTML(AContext: TIWCompContext): TIWHTMLTag;
{$ELSE}
function TsgcIWWSClient.RenderHTML(AContext: TIWBaseHTMLComponentContext):
    TIWHTMLTag;
{$ENDIF}
var
  vScript: string;
  vWS: string;
  vURL: string;
begin
  AContext.WebApplication.RegisterCallBack('OnWSOpen' + HTMLNAME, DoOpenEvent);
  AContext.WebApplication.RegisterCallBack('OnWSClose' + HTMLNAME, DoCloseEvent);
  AContext.WebApplication.RegisterCallBack('OnWSMessage' + HTMLNAME, DoMessageEvent);
  AContext.WebApplication.RegisterCallBack('OnWSError' + HTMLNAME, DoErrorEvent);

  AContext.WebApplication.RegisterCallBack('OnWSNoWebsocketSupport' + HTMLNAME, DoNoWebsocketSupportEvent);

  Result := TIWHTMLTag.CreateTag('TEXTAREA');
  Result.AddStringParam('NAME', HTMLName);
  Result.AddStringParam('style', 'visibility:hidden;');


  vScript :=  'var WS' + HTMLNAME + ';' + #13#10 +
              'function WS' + HTMLNAME + '_onopen() {' + #13#10 +
              JSOpen.Text +
              '} ' + #13#10 +
              'function WS' + HTMLNAME + '_onmessage(text) {' + #13#10 +
              JSMessage.Text +
              '} ' + #13#10 +
              'function WS' + HTMLNAME + '_onclose() {' + #13#10 +
              JSClose.Text +
              '} ' + #13#10 +
              'function WS' + HTMLNAME + '_onerror(text) {' + #13#10 +
              JSError.Text +
              '} ' + #13#10;

  // ... ssl
  vWS := 'ws';
  if TLS then
    vWS := 'wss';
  // ... url
  vURL := vWS + '://' + Host + ':' + IntToStr(Port);
  if Parameters <> '' then
    vURL := vURL + Parameters;

  // ... protocol
  FProtocol := GetProtocols;

  vScript := vScript +
              'function WS' + HTMLNAME + '_create() {' + #13#10;
  // ... websockets
  if Protocol <> '' then
    vScript := vScript + 'WS' + HTMLNAME + ' = new WebSocket("' + vURL + '", "' + Protocol + '"); ' + #13#10
  else
    vScript := vScript + 'WS' + HTMLNAME + ' = new WebSocket("' + vURL + '"); ' + #13#10;
  // ... OnOpen
  vScript := vScript + 'WS' + HTMLNAME + '.onopen = function() { ' + #13#10;
  if JSOpen.Count > 0 then
    vScript := vScript + 'WS' + HTMLNAME + '_onopen(); ' + #13#10;
  vScript := vScript + 'processAjaxEvent(null, null,"OnWSOpen' + HTMLNAME + '", false, null, true); ' + #13#10;
  vScript := vScript + 'return false; } ' + #13#10;
  // ... OnClose
  vScript := vScript + 'WS' + HTMLNAME + '.onclose = function() { ' + #13#10;
  if JSClose.Count > 0 then
    vScript := vScript + 'WS' + HTMLNAME + '_onclose(); ' + #13#10;
  vScript := vScript + 'processAjaxEvent(null, null,"OnWSClose' + HTMLNAME + '", false, null, true); ' + #13#10;
  vScript := vScript + 'return false; } ' + #13#10;
  // ... OnMessage
  vScript := vScript + 'WS' + HTMLNAME + '.onmessage = function(evt) { ' + #13#10 ;
  if JSMessage.Count > 0 then
    vScript := vScript + 'WS' + HTMLNAME + '_onmessage(evt.data); ' + #13#10;
  vScript := vScript + 'SendRequest("&x=" + evt.data, null, "OnWSMessage' + HTMLNAME + '", false, null, ' + {$IFDEF IWXV}'null'{$ELSE}'true'{$ENDIF} + '); return true; ' + #13#10;
  vScript := vScript + '} ' + #13#10;
  // ... OnError
  vScript := vScript + 'WS' + HTMLNAME + '.onerror = function(evt) { ' + #13#10;
  if JSError.Count > 0 then
    vScript := vScript + 'WS' + HTMLNAME + '_onerror(evt.data); ' + #13#10;
  vScript := vScript + 'SendRequest("&x=" + evt.data, null, "OnWSError' + HTMLNAME + '", false, null, ' + {$IFDEF IWXV}'null'{$ELSE}'true'{$ENDIF} + '); return true; ' + #13#10;
  vScript := vScript + '} ' + #13#10 +
              '}' + #13#10;

  // ... send message
  vScript := vScript +
              'function WS' + HTMLNAME + '_send(Text) {' + #13#10 +
                'WS' + HTMLNAME + '.send(Text); ' + #13#10 +
              '} ' + #13#10;

  // ... close websocket
  vScript := vScript +
              'function WS' + HTMLNAME + '_close() {' + #13#10 +
                'WS' + HTMLNAME + '.close(); ' + #13#10 +
              '} ' + #13#10;

  // ... add script
  {$IFDEF IWXI}
  AContext.AddToJavaScriptOnce(vScript);
  {$ELSE}
  TIWComponent40Context(AContext).AddToJavaScriptOnce(vScript);
  {$ENDIF}
end;

{$IFDEF IWXI}
procedure TsgcIWWSClient.RenderScripts(AComponentContext: TIWCompContext);
{$ELSE}
procedure TsgcIWWSClient.RenderScripts(AComponentContext:
    TIWBaseHTMLComponentContext);
{$ENDIF}
begin
  if Transports.Emulation then
  begin
    AComponentContext.WebApplication.RegisterCallBack('OnWSEmulation' + HTMLNAME, DoEmulationEvent);
    TIWPageContext40(AComponentContext.PageContext).AddToInitProc(
        'function WSTest() {' + #13#10 +
          'if ("WebSocket" in window) {' + #13#10 +
          '} else {' + #13#10 +
            'window.' + HTMLName + ' = new IWTimer(''true'', 5000, "OnWSEmulation' + HTMLNAME + '");' + #13#10 +
            'processAjaxEvent(null, null, "OnWSNoWebsocketSupport' + HTMLNAME + '", false, null, true);  ' + #13#10 +
          '}' + #13#10 +
        '}' + #13#10 +
        'WSTest();');
  end;
end;

procedure TsgcIWWSClient.WriteData(const aText: string);
begin
  if FWriteText <> aText then
  begin
    FAction := 3;
    FWriteText := aText;
    Invalidate;
  end;
end;

procedure TsgcIWWSClient.SetJSClose(const Value: TStringList);
begin
  FJSClose.Assign(Value);
  Invalidate;
end;

procedure TsgcIWWSClient.SetJSError(const Value: TStringList);
begin
  FJSError.Assign(Value);
  Invalidate;
end;

procedure TsgcIWWSClient.SetJSMessage(const Value: TStringList);
begin
  FJSMessage.Assign(Value);
  Invalidate;
end;

procedure TsgcIWWSClient.SetJSOpen(const Value: TStringList);
begin
  FJSOpen.Assign(Value);
  Invalidate;
end;

procedure TsgcIWWSClient.SetTransports(const Value: TsgcIWTransports);
begin
  FTransports.Assign(Value);
end;

procedure TsgcIWWSClient.SetTLS(const Value: Boolean);
begin
  FTLS := Value;
end;

procedure TsgcIWWSClient.SetURL(const Value: String);
var
  vProtocol, vHost, vPort, vPath, vQuery: String;
begin
  FURL := Value;

  ParseURL(Value, vProtocol, vHost, vPort, vPath, vQuery);

  Host := vHost;
  if vProtocol = 'wss' then
  begin
    TLS := True;
    if vPort <> '' then
      Port := StrToInt(vPort)
    else
      Port := 443;
  end
  else
  begin
    TLS := False;
    if vPort <> '' then
      Port := StrToInt(vPort)
    else
      Port := 80;
  end;
  Parameters := vPath;
  if vQuery <> '' then
  begin
    if (RightStr(Parameters, 1) = '/') and (Length(Parameters) > 1) then
      Parameters := LeftStr(Parameters, Length(Parameters) - 1);

    Parameters := Parameters + '?' + vQuery;
  end;
end;

procedure TsgcIWWSClient.Submit(const AValue: string);
begin
  FSubmitParam := AValue;
  DoSubmit;
end;

procedure TsgcIWWSClient.UnRegisterProtocol(aObject: TsgcIWWSProtocol);
begin
  FProtocolObjectList.Extract(aObject);
end;

procedure TsgcIWWSProtocol.DoNotifyConnect;
begin
  if Assigned(FOnAsyncConnect) then FOnAsyncConnect(self);
end;

procedure TsgcIWWSProtocol.DoNotifyDisconnect;
begin
  if Assigned(FOnAsyncDisconnect) then FOnAsyncDisconnect(self);
end;

procedure TsgcIWWSProtocol.DoNotifyError(const aMessage: String);
begin
  if Assigned(FOnAsyncError) then FOnAsyncError(self, aMessage);
end;

procedure TsgcIWWSProtocol.DoNotifyMessage(const aMessage: String);
begin
  if Assigned(FOnAsyncMessage) then FOnAsyncMessage(self, aMessage);
end;

function TsgcIWWSProtocol.GetVersion: String;
begin
  Result := CS_VERSION;
end;

procedure TsgcIWWSProtocol.Loaded;
begin
  inherited;
  if Client <> FClient then
    Client := FClient;
end;

procedure TsgcIWWSProtocol.Notification(AComponent : TComponent; Operation :
    TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

procedure TsgcIWWSProtocol.SetClient(const Value: TsgcIWWSClient);
begin
  if Assigned(FClient) then
    FClient.RemoveFreeNotification(self);

  if [csDesigning, csLoading] * ComponentState = [] then
  begin
    if Assigned(FClient) then
    begin
      if not Assigned(Value) then
      begin
        FClient.UnRegisterProtocol(self);
        FClient := Value;
      end
      else if FClient <> Value then
      begin
        FClient := Value;
        FClient.RegisterProtocol(self);
      end;
    end
    else
    begin
      FClient := Value;
      if Assigned(Value) then
        FClient.RegisterProtocol(self);
    end;
  end
  else
    FClient := Value;

  if Assigned(FClient) then
    FClient.FreeNotification(self);
end;

{$ENDIF}

end.
