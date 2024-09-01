unit ScIndy;

interface

uses
  Classes, SysUtils,
  IdGlobal, IdComponent, IdIOHandler, IdException,
  ScUtils, ScSSHClient, ScSSHChannel;

type
  // Wrapper for connetion Indy with TScSSHClient
  TScIdIOHandler = class(TIdIOHandler)
  private
    FClient: TScSSHClient;
    FSSHChannel: TScSSHChannel;
    procedure SetClient(Value: TScSSHClient);
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;

  public
    destructor Destroy; override;
    procedure Open; override;
    procedure Close; override;
    procedure ConnectClient(const AHost: string; const APort: Integer; const ABoundIP: string;
     const ABoundPort: Integer; const ABoundPortMin: Integer; const ABoundPortMax: Integer;
     const ATimeout: Integer = IdTimeoutDefault); override;
    function Connected: Boolean; override;
    function Readable(AMSec: integer = IdTimeoutDefault): boolean; override;
    function Recv(var ABuf; ALen: Integer): Integer; override;
    function Send(var ABuf; ALen: Integer): Integer; override;

  published
    property Client: TScSSHClient read FClient write SetClient;
  end;

implementation

uses
  ScConsts, ScSSHUtils;

destructor TScIdIOHandler.Destroy;
begin
  FreeAndNil(FSSHChannel);

  inherited;
end;

procedure TScIdIOHandler.ConnectClient(const AHost: string;
  const APort: Integer; const ABoundIP: string; const ABoundPort,
  ABoundPortMin, ABoundPortMax: Integer; const ATimeout: Integer = IdTimeoutDefault);
begin
  inherited ConnectClient(AHost, APort, ABoundIP, ABoundPort, ABoundPortMin, ABoundPortMax, ATimeout);

  if Client = nil then
    raise EScError.Create(SClientNotDefined);

  // Connect
  DoStatus(hsConnecting, []);
  Client.Connected := True;

  if FSSHChannel = nil then
    FSSHChannel := TScSSHChannel.Create(nil);

  if ATimeout < 0 then
    FSSHChannel.Timeout := MaxInt
  else
    FSSHChannel.Timeout := ATimeout;

  FSSHChannel.Client := Client;
  FSSHChannel.Direct := True;
  FSSHChannel.DestHost := AHost;
  FSSHChannel.DestPort := APort;
  FSSHChannel.Connect;
end;

procedure TScIdIOHandler.Open;
begin
  inherited Open;

  if not Assigned(Client) then
    raise EIdConnectException.Create(SClientNotDefined);
  Client.Connected := True;
end;

procedure TScIdIOHandler.Close;
begin
  inherited Close;

  if Assigned(FSSHChannel) then begin
    FSSHChannel.Disconnect;
  end;
end;

function TScIdIOHandler.Connected: Boolean;
begin
  Result := FSSHChannel <> nil;
  if Result then
    Result := FSSHChannel.Connected;
end;

function TScIdIOHandler.Readable(AMSec: integer): boolean;
begin
  Result := FSSHChannel <> nil;

  if Result then begin
    if AMSec = IdTimeoutInfinite then
      AMSec := MaxInt;

    Result := TScSSHChannelUtils.Readable(FSSHChannel, 1, AMSec);
  end;
end;

function TScIdIOHandler.Recv(var ABuf; ALen: Integer): Integer;
begin
  if FSSHChannel <> nil then
    Result := FSSHChannel.ReadBuffer(ABuf, ALen)
  else
    raise EIdException.Create(SChannelNotConnected);
end;

function TScIdIOHandler.Send(var ABuf; ALen: Integer): Integer;
begin
  if FSSHChannel <> nil then
    Result := FSSHChannel.WriteBuffer(ABuf, ALen)
  else
    raise EIdException.Create(SChannelNotConnected);
end;

procedure TScIdIOHandler.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Component = FClient) and (Operation = opRemove) then
    Client := nil;

  inherited;
end;

procedure TScIdIOHandler.SetClient(Value: TScSSHClient);
begin
  if Value <> FClient then begin
    if FClient <> nil then
      FClient.RemoveFreeNotification(Self);

    Close;
    FClient := Value;

    if Value <> nil then
      FClient.FreeNotification(Self);
  end;
end;

end.

