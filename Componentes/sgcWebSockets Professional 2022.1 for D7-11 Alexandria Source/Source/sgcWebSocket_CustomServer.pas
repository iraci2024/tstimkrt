{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}


unit sgcWebSocket_CustomServer;

interface

{$I sgcVer.inc}

uses
  Classes,
  // indy
  {$IFDEF SGC_INDY}sgcIdHTTPServer{$ELSE}IdHTTPServer{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdTCPServer{$ELSE}IdTCPServer{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdSSL{$ELSE}IdSSL{$ENDIF};

type
  TsgcWSQuerySSLPortEvent = procedure(aPort: Integer; var aSSL: Boolean) of object;

  TsgcWSCustomServer = class(TIdTCPServer)
  { events }
  private
    FOnStartup: TNotifyEvent;
    FOnShutdown: TNotifyEvent;
  protected
    procedure Startup; override;
    procedure Shutdown; override;
  public
    property OnStartup: TNotifyEvent read FOnStartup write FOnStartup;
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
  { events }

  { ssl }
  protected
    FOnQuerySSLPort: TsgcWSQuerySSLPortEvent;
    procedure DoConnect(AContext: TIdContext); override;
    function DoQuerySSLPort(aPort: Integer): Boolean; virtual;
  public
    property OnQuerySSLPort: TsgcWSQuerySSLPortEvent read FOnQuerySSLPort write
        FOnQuerySSLPort;
  { ssl }
  end;

  TsgcWSHTTPCustomServer = class(TIdHTTPServer)
  { events }
  private
    FOnStartup: TNotifyEvent;
    FOnShutdown: TNotifyEvent;
  protected
    procedure Startup; override;
    procedure Shutdown; override;
  public
    property OnStartup: TNotifyEvent read FOnStartup write FOnStartup;
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
  { events }

  { http }
  protected
    function DoExecute(AContext: TIdContext): boolean; override;
  public
    procedure DoExecuteHTTP(AContext: TIdContext);
  { http }

  { ssl }
  protected
    FOnQueryHTTPSSLPort: TsgcWSQuerySSLPortEvent;
    procedure DoConnect(AContext: TIdContext); override;
    function DoQuerySSLPort(aPort: Integer): Boolean; reintroduce; virtual;
  public
    property OnExecute;
  public
    property OnQuerySSLPort: TsgcWSQuerySSLPortEvent read FOnQueryHTTPSSLPort write
        FOnQueryHTTPSSLPort;
  { ssl }
  end;

implementation

procedure TsgcWSHTTPCustomServer.DoConnect(AContext: TIdContext);
begin
// ... customized DoConnect
//  inherited;
  if AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase then
  begin
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough :=
      not DoQuerySSLPort(AContext.Connection.Socket.Binding.Port);
  end;
  if Assigned(OnConnect) then OnConnect(AContext);
end;

function TsgcWSHTTPCustomServer.DoExecute(AContext: TIdContext): boolean;
begin
  if Assigned(FOnExecute) then
  begin
    FOnExecute(AContext);
    result := AContext.Connection.Connected;
  end
  else
    result := inherited DoExecute(AContext);
end;

procedure TsgcWSHTTPCustomServer.DoExecuteHTTP(AContext: TIdContext);
begin
  inherited DoExecute(AContext);
end;

function TsgcWSHTTPCustomServer.DoQuerySSLPort(aPort: Integer): Boolean;
begin
  Result := not Assigned(FOnQueryHTTPSSLPort);
  if not Result then FOnQueryHTTPSSLPort(aPort, Result);
end;

procedure TsgcWSHTTPCustomServer.Shutdown;
begin
  inherited;
  if Assigned(FOnShutdown) then
    FOnShutdown(self);
end;

procedure TsgcWSHTTPCustomServer.Startup;
begin
  inherited;
  if Assigned(FOnStartup) then
    FOnStartup(self);
end;

procedure TsgcWSCustomServer.DoConnect(AContext: TIdContext);
begin
// ... customized DoConnect
//  inherited;
  if AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase then
  begin
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough :=
      not DoQuerySSLPort(AContext.Connection.Socket.Binding.Port);
  end;
  if Assigned(OnConnect) then OnConnect(AContext);
end;

function TsgcWSCustomServer.DoQuerySSLPort(aPort: Integer): Boolean;
begin
  Result := not Assigned(FOnQuerySSLPort);
  if not Result then FOnQuerySSLPort(aPort, Result);
end;

procedure TsgcWSCustomServer.Shutdown;
begin
  inherited;
  if Assigned(FOnShutdown) then
    FOnShutdown(self);
end;

procedure TsgcWSCustomServer.Startup;
begin
  inherited;
  if Assigned(FOnStartup) then
    FOnStartup(self);
end;


end.

