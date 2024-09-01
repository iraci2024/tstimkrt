
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScSecureSocket;
{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRFunctions, CRVio, CRVioTcp,
  TdsUtils, TdsBridge,
  TdsLayers, TdsSSLTypes, TdsSocketController, TdsSSLExtensions;
{$ELSE}
  ScCLRClasses, ScTypes, ScFunctions, ScVio, ScVioTcp,
  ScUtils, ScBridge,
  ScLayers, ScSSLTypes, ScSocketController, ScSSLExtensions;
{$ENDIF}

type
  TCustomSecureConnection = class
  protected
    FOptions: TScSecurityOptions;
    FLastException: Exception;
    FIsSecure: boolean;
    FOnAsyncReceive: TNotifyEvent;
    FController: TSecureController;

    function GetSessionInfo: TScSSLSessionInfo;
    procedure SetOptions(Value: TScSecurityOptions);
    procedure SetIsSecure(Value: boolean);
    procedure SetSecure; virtual; abstract;
    procedure SetOnAsyncReceive(Value: TNotifyEvent); virtual; abstract;

    function GetAvailable: integer; virtual; abstract;
    function GetConnected: boolean; virtual; abstract;
    function GetHasData: boolean; virtual; abstract;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect; virtual; abstract;
    procedure QueueRenegotiate;

    function Send(const Buffer: TValueArr; Offset, Count: integer): integer; virtual; abstract;
    function Read(const Buffer: TValueArr; Offset, Count: integer): integer; virtual;
    function ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer; virtual; abstract;
    function WaitForData(MillisecondsTimeout: integer): boolean; virtual; abstract;

    procedure ShutDown; virtual; abstract;
    procedure Close; virtual; abstract;

    function GetLastException: Exception; virtual; abstract;

    property SessionInfo: TScSSLSessionInfo read GetSessionInfo;
    property IsSecure: boolean read FIsSecure write SetIsSecure;
    property Options: TScSecurityOptions read FOptions write SetOptions;

    property Available: integer read GetAvailable;
    property Connected: boolean read GetConnected;
    property HasData: boolean read GetHasData;

    property OnAsyncReceive: TNotifyEvent read FOnAsyncReceive write SetOnAsyncReceive;
  end;

  // Implements the Berkeley sockets interface and optionally encrypts/decrypts transmitted data.
  TSecureSocket = class(TCustomSecureConnection)
  private
    FVio: TCRVio;
    FIsOwner: boolean;
    FIsShutdown: boolean;
    FTimeout: integer;

  protected
    procedure SetTimeout(Value: integer);
    procedure SetSecure; override;

    function GetOnClose: TNotifyEvent;
    procedure SetOnClose(Value: TNotifyEvent);
    procedure SetOnAsyncReceive(Value: TNotifyEvent); override;

    function GetAvailable: integer; override;
    function GetConnected: boolean; override;
    function GetHasData: boolean; override;

  public
    constructor Create(HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
      const ProviderName: string; const Host: string; Port: integer;
      IPVersion: TIPVersion); overload;
    constructor Create(Vio: TCRVioTcp; IsOwner: boolean); overload;
    destructor Destroy; override;

    procedure Bind(const BindAddress: string);

    procedure Connect; override;

    function Send(const Buffer: TValueArr; Offset, Count: integer): integer; override;
    function Read(const Buffer: TValueArr; Offset, Count: integer): integer; override;
    function ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer; override;
    function WaitForData(MillisecondsTimeout: integer): boolean; override;

    procedure ShutDown; override;
    procedure Close; override;

    procedure SetSocketOption(OptionLevel: integer; OptionName: integer; OptionValue: integer);

    function GetLastException: Exception; override;
    function GetLocalIP: string;
    function GetLocalPort: integer;
    function GetRemoteIP: string;
    function GetRemotePort: integer;

    property Timeout: integer read FTimeout write SetTimeout;

    property OnClose: TNotifyEvent read GetOnClose write SetOnClose;
  end;

  TSecureConnection = class(TCustomSecureConnection)
  private
    FOnSendDataToTarget: TScSendDataEvent;
    FOnReceiveDataFromSource: TScReceiveDataEvent;
    FOnConnect: TNotifyEvent;
    FOnClose: TNotifyEvent;

    FConnected: boolean;

  protected
    procedure SetSecure; override;
    procedure SetOnAsyncReceive(Value: TNotifyEvent); override;

    function GetAvailable: integer; override;
    function GetConnected: boolean; override;
    function GetHasData: boolean; override;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect; override;

    function Send(const Buffer: TValueArr; Offset, Count: integer): integer; override;
    function ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer; override;
    function WaitForData(MillisecondsTimeout: integer): boolean; override;

    procedure ShutDown; override;
    procedure Close; override;

    function GetLastException: Exception; override;

    property OnSendDataToTarget: TScSendDataEvent read FOnSendDataToTarget write FOnSendDataToTarget;
    property OnReceiveDataFromSource: TScReceiveDataEvent read FOnReceiveDataFromSource write FOnReceiveDataFromSource;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, WinSock,
{$ENDIF}
{$IFDEF POSIX}
  Posix.NetinetIn, Posix.SysSocket,
{$ENDIF}
{$IFDEF UNIX}
  Sockets,
{$ENDIF}
{$IFNDEF SBRIDGE}
  CRVioHttp, TdsSSLConsts,
  TdsClientHandshakeLayer, TdsServerHandshakeLayer;
{$ELSE}
  ScVioHttp, ScConsts,
  ScClientHandshakeLayer, ScServerHandshakeLayer;
{$ENDIF}

{ TCustomSecureConnection }

constructor TCustomSecureConnection.Create;
begin
  inherited Create;

  FOptions := TScSecurityOptions.Create;
  FIsSecure := False;
  FController := nil;
end;

destructor TCustomSecureConnection.Destroy;
begin
  FController.Free;
  FLastException.Free;
  FOptions.Free;

  inherited;
end;

function TCustomSecureConnection.Read(const Buffer: TValueArr; Offset, Count: integer): integer;
var
  ReadCount: integer;
begin
  Result := 0;

  while Count > 0 do begin
    ReadCount := ReadNoWait(Buffer, Offset, Count);
    if ReadCount <= 0 then
      Exit;

    Inc(Offset, ReadCount);
    Inc(Result, ReadCount);
    Dec(Count, ReadCount);
  end;
end;

procedure TCustomSecureConnection.QueueRenegotiate;
begin
  if not Connected or (FController = nil) then
    raise EScError.Create(seConnectionNotSecure);

  FController.QueueRenegotiate;
end;

function TCustomSecureConnection.GetSessionInfo: TScSSLSessionInfo;
begin
  if FController <> nil then
    Result := FController.SessionInfo
  else
    Result := nil;
end;

procedure TCustomSecureConnection.SetIsSecure(Value: boolean);
begin
  if FIsSecure <> Value then begin
    if Value and Connected then
      SetSecure;

    FIsSecure := Value;
  end;
end;

procedure TCustomSecureConnection.SetOptions(Value: TScSecurityOptions);
begin
  FOptions.Assign(Value);
end;

{ TSecureSocket }

constructor TSecureSocket.Create(HttpOptions: THttpOptions; ProxyOptions: TProxyOptions;
  const ProviderName: string; const Host: string; Port: integer; IPVersion: TIPVersion);
begin
  inherited Create;

  if (HttpOptions <> nil) and HttpOptions.Enabled then
    FVio := TCRVioHttp.Create(nil, HttpOptions, ProxyOptions, Host, Port, IPVersion)
  else
    FVio := TCRVioTcp.Create(ProxyOptions, ProviderName, Host, Port, IPVersion);

  FIsOwner := True;
  FIsShutdown := False;
end;

constructor TSecureSocket.Create(Vio: TCRVioTcp; IsOwner: boolean);
begin
  inherited Create;

  Assert(Vio <> nil);
  FVio := Vio;
  FIsOwner := IsOwner;
  FIsShutdown := False;
end;

destructor TSecureSocket.Destroy;
begin
  Close;

  if FController <> nil then
    TSecureSocketController(FController).ClearVio;

  inherited;

  if FIsOwner then
    FVio.Free;
end;

function TSecureSocket.GetLastException: Exception;
begin
  if FLastException = nil then
    if not FIsSecure then begin
      if FVio.LastError <> '' then
        FLastException := SocketException.Create(FVio.LastError, FVio.LastErrorCode);
    end
    else
      if FController <> nil then
        FLastException := CloneException(FController.GetLastException);

  Result := FLastException;
end;

procedure TSecureSocket.Close;
begin
  FreeAndNil(FLastException);

  try
    if FController <> nil then
      FController.Close
    else
    if FVio <> nil then
      FVio.Close;
  except
    on E: Exception do begin
      FLastException := CloneException(E);
      raise;
    end;
  end;
end;

procedure TSecureSocket.Bind(const BindAddress: string);
begin
  if (BindAddress <> '') and (FVio is TCRVioTcp) then begin
    TCRVioTcp(FVio).BindAddress := BindAddress;
    TCRVioTcp(FVio).Bind;
  end;
end;

procedure TSecureSocket.Connect;
begin
  FreeAndNil(FController);
  FreeAndNil(FLastException);

  FVio.Connect;

  if FIsSecure then
    SetSecure;
end;

procedure TSecureSocket.SetSecure;
begin
  if FController <> nil then
    FController.ReStart
  else begin
    FreeAndNil(FLastException);
    FController := TSecureSocketController.Create(FVio, FOptions);
    try
      FVio.NonBlocking := False;
      FController.AfterReceiveAppData := FOnAsyncReceive;
      FController.StartTLS;
    except
      on E: Exception do begin
        FLastException := CloneException(E);
        FreeAndNil(FController);
        raise;
      end;
    end;
  end;
end;

function TSecureSocket.Send(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if not FIsSecure then begin
    Result := FVio.Write(Buffer, Offset, Count);
    if FVio.LastError <> '' then
      raise SocketException.Create(FVio.LastError, FVio.LastErrorCode);
  end
  else begin
    FreeAndNil(FLastException);

    if not Connected or (FController = nil) then
      raise SocketException.Create(SSocketErr);

    try
       Result := FController.WriteData(Buffer, Offset, Count);
    except
      on E: Exception do begin
        FLastException := CloneException(E);
        raise;
      end;
    end;
  end;
end;

function TSecureSocket.Read(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if not FIsSecure then begin
    Result := FVio.ReadNoWait(Buffer, Offset, Count);
    if (Result <= 0) and (FVio.LastError <> '') then
      raise SocketException.Create(FVio.LastError, FVio.LastErrorCode);
  end
  else begin
    FreeAndNil(FLastException);

    try
      if (not Connected and FIsShutdown) or (FController = nil) then
        raise SocketException.Create(SSocketErr);

      Result := TSecureSocketController(FController).Read(Buffer, Offset, Count);
      if Result = -1 then
        FIsShutdown := True;
    except
      on E: Exception do begin
        FLastException := CloneException(E);
        raise;
      end;
    end;
  end;
end;

function TSecureSocket.ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if not FIsSecure then begin
    Result := FVio.ReadNoWait(Buffer, Offset, Count);
    if (Result <= 0) and (FVio.LastError <> '') then
      raise SocketException.Create(FVio.LastError, FVio.LastErrorCode);
  end
  else begin
    FreeAndNil(FLastException);

    try
      if FController <> nil then
        Result := TSecureSocketController(FController).ReadNoWait(Buffer, Offset, Count)
      else
        Result := 0;
    except
      on E: Exception do begin
        FLastException := CloneException(E);
        raise;
      end;
    end;
  end;
end;

function TSecureSocket.WaitForData(MillisecondsTimeout: integer): boolean;
begin
  if not FIsSecure then
    Result := FVio.WaitForData(MillisecondsTimeout)
  else
  if FController <> nil then
    Result := FController.WaitForData(1, cardinal(MillisecondsTimeout))
  else
    Result := False;
end;

procedure TSecureSocket.ShutDown;
begin
  try
    if not Connected then
      Exit
    else
    if not FIsSecure then begin
      if FVio is TCRVioTcp then
        TCRVioTcp(FVio).ShutDown(SD_BOTH);
    end
    else
      if FController <> nil then
        FController.Shutdown;
  except
  end;
end;

function TSecureSocket.GetConnected: boolean;
begin
  Result := FVio.Connected;
end;

function TSecureSocket.GetAvailable: integer;
begin
  if not FIsSecure then
    Result := FVio.Available
  else begin
    if FController = nil then
      raise SocketException.Create(SSocketNotConnected);

    Result := FController.Available;
  end;
end;

function TSecureSocket.GetHasData: boolean;
begin
  if FController <> nil then
    Result := FController.Available > 0
  else
    Result := FVio.Connected;
end;

procedure TSecureSocket.SetSocketOption(OptionLevel: integer; OptionName: integer; OptionValue: integer);
begin
  if FVio is TCRVioTcp then
    TCRVioTcp(FVio).SetSocketOption(OptionLevel, OptionName, OptionValue);
end;

procedure TSecureSocket.SetTimeout(Value: integer);
begin
  if Value <> FTimeout then begin
    if Value = 0 then
      FTimeout := MaxInt
    else
      FTimeout := Value;

    if (Int64(FTimeout) * 1000) > MaxInt then
      FOptions.Timeout := MaxInt
    else
      FOptions.Timeout := FTimeout * 1000;

    FVio.Timeout := Value;
    FVio.SendTimeout := Value;
  end;
end;

function TSecureSocket.GetLocalIP: string;
begin
  if FVio is TCRVioTcp then
    Result := TCRVioTcp(FVio).GetLocalIP
  else
    Result := '';
end;

function TSecureSocket.GetLocalPort: integer;
begin
  if FVio is TCRVioTcp then
    Result := TCRVioTcp(FVio).GetLocalPort
  else
    Result := 0;
end;

function TSecureSocket.GetRemoteIP: string;
begin
  if FVio is TCRVioTcp then
    Result := TCRVioTcp(FVio).GetRemoteIP
  else
    Result := '';
end;

function TSecureSocket.GetRemotePort: integer;
begin
  if FVio is TCRVioTcp then
    Result := TCRVioTcp(FVio).GetRemotePort
  else
    Result := 0;
end;

function TSecureSocket.GetOnClose: TNotifyEvent;
begin
  Result := FVio.OnClose;
end;

procedure TSecureSocket.SetOnClose(Value: TNotifyEvent);
begin
  FVio.OnClose := Value;
end;

procedure TSecureSocket.SetOnAsyncReceive(Value: TNotifyEvent);
begin
  FOnAsyncReceive := Value;

  if FController <> nil then
    FController.AfterReceiveAppData := FOnAsyncReceive
  else
  if FVio <> nil then begin
    FVio.OnAsyncReceive := FOnAsyncReceive;
    FVio.NonBlocking := Assigned(FOnAsyncReceive);
  end;
end;

{ TSecureConnection }

constructor TSecureConnection.Create;
begin
  inherited Create;
end;

destructor TSecureConnection.Destroy;
begin
  Close;

  inherited;
end;

function TSecureConnection.GetLastException: Exception;
begin
  if FLastException = nil then
    if FController <> nil then
      FLastException := CloneException(FController.GetLastException);

  Result := FLastException;
end;

procedure TSecureConnection.Close;
begin
  FreeAndNil(FLastException);

  FConnected := False;

  try
    if FController <> nil then
      FController.Close
    else
    if Assigned(OnClose) then
      OnClose(Self);
  except
    on E: Exception do begin
      FLastException := CloneException(E);
      raise;
    end;
  end;
end;

procedure TSecureConnection.Connect;
begin
  FreeAndNil(FController);
  FreeAndNil(FLastException);

  if Assigned(OnConnect) then
    OnConnect(Self);

  FConnected := True;

  if FIsSecure then
    SetSecure;
end;

procedure TSecureConnection.SetSecure;
var
  HandshakeLayerClass: THandshakeLayerClass;
begin
  if not Assigned(FOnSendDataToTarget) then
    raise EScError.CreateFmt(SEventHandlerNotDefined, ['OnSendDataToTarget'], seEventHandlerNotDefined);

  if not Assigned(FOnReceiveDataFromSource) then
    raise EScError.CreateFmt(SEventHandlerNotDefined, ['OnReceiveDataFromSource'], seEventHandlerNotDefined);

  if FController <> nil then
    FController.ReStart
  else begin
    FreeAndNil(FLastException);

    if FOptions.Entity = ceClient then
      HandshakeLayerClass := TClientHandshakeLayer
    else
      HandshakeLayerClass := TServerHandshakeLayer;

    FController := TSecureController.Create(FOptions, HandshakeLayerClass);
    try
      FController.OnSendDataToTarget := FOnSendDataToTarget;
      FController.OnReceiveDataFromSource := FOnReceiveDataFromSource;
      FController.OnClose := FOnClose;

      FController.AfterReceiveAppData := FOnAsyncReceive;
      FController.StartTLS;
    except
      on E: Exception do begin
        FLastException := CloneException(E);
        FreeAndNil(FController);
        raise;
      end;
    end;
  end;
end;

function TSecureConnection.Send(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if not FIsSecure then begin
    if not Assigned(FOnSendDataToTarget) then
      raise EScError.CreateFmt(SEventHandlerNotDefined, ['OnSendDataToTarget'], seEventHandlerNotDefined);

    FOnSendDataToTarget(Buffer, Offset, Count);
    Result := Count;
  end
  else begin
    FreeAndNil(FLastException);

    if FController = nil then
      raise SocketException.Create(SSocketErr);

    try
      Result := FController.WriteData(Buffer, Offset, Count);
    except
      on E: Exception do begin
        FLastException := CloneException(E);
        raise;
      end;
    end;
  end;
end;

function TSecureConnection.ReadNoWait(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if not FIsSecure then begin
    if not Assigned(FOnReceiveDataFromSource) then
      raise EScError.CreateFmt(SEventHandlerNotDefined, ['OnReceiveDataFromSource'], seEventHandlerNotDefined);

    Result := FOnReceiveDataFromSource(Buffer, Offset, Count);
  end
  else begin
    FreeAndNil(FLastException);

    try
      if FController <> nil then
        Result := FController.ReadDataSync(Buffer, Offset, Count)
      else
        Result := 0;
    except
      on E: Exception do begin
        FLastException := CloneException(E);
        raise;
      end;
    end;
  end;
end;

function TSecureConnection.WaitForData(MillisecondsTimeout: integer): boolean;
begin
  if FController <> nil then
    Result := FController.WaitForData(1, cardinal(MillisecondsTimeout))
  else
    Result := False;
end;

procedure TSecureConnection.ShutDown;
begin
  try
    if FController <> nil then
      FController.Shutdown;
  except
  end;
end;

function TSecureConnection.GetAvailable: integer;
begin
  if FController <> nil then
    Result := FController.Available
  else
    Result := 0;
end;

function TSecureConnection.GetConnected: boolean;
begin
  Result := FConnected;
end;

function TSecureConnection.GetHasData: boolean;
begin
  if FController <> nil then
    Result := FController.Available > 0
  else
    Result := False;
end;

procedure TSecureConnection.SetOnAsyncReceive(Value: TNotifyEvent);
begin
  FOnAsyncReceive := Value;

  if FController <> nil then
    FController.AfterReceiveAppData := FOnAsyncReceive;
end;

end.

