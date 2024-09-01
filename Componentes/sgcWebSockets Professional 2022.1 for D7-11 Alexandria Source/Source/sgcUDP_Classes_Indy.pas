{ ***************************************************************************
  sgcUDP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcUDP_Classes_Indy;

interface

{$I sgcVer.inc}

{$IFDEF SGC_UDP}

uses
  Classes, SysUtils, SyncObjs,
  // indy
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSL{$ELSE}IdSSL{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSocketHandle{$ELSE}IdSocketHandle{$ENDIF},
  // websocket
  sgcUDP_Classes, sgcWebSocket_Classes_Indy, sgcWebSocket_Types;

type

  TsgcUDPSocket_Indy = class(TsgcUDPSocket)
    { socket }
  private
    FSocket: TIdSocketHandle;
  protected
    property Socket: TIdSocketHandle read FSocket write FSocket;
  protected
    function GetActive: Boolean; override;
    { socket }

    { fields }
  protected
    function GetIP: String; override;
    function GetPort: Integer; override;
    function GetLocalIP: String; override;
    function GetLocalPort: Integer; override;
    function GetIPVersion: TwsIPVersion; override;
    { fields }

    { disconnect }
  private
    procedure DoBeforeDisconnectEvent; virtual;
  public
    procedure DisconnectPeer; virtual;
    { disconnect }

    { write data }
  protected
    procedure DoWriteBytes(const aBuffer: TIdBytes); virtual;
  protected
    { write data }

    { constructor / destructor }
  public
    destructor Destroy; override;
    { constructor / destructor }

    { event }
  private
    FOnBeforeDisconnect: TNotifyEvent;
    FOnRead: TNotifyEvent;
  public
    property OnBeforeDisconnect: TNotifyEvent read FOnBeforeDisconnect
      write FOnBeforeDisconnect;
    property OnRead: TNotifyEvent read FOnRead write FOnRead;
    { event }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_UDP}

uses
  sgcBase_Helpers;

procedure TsgcUDPSocket_Indy.DoWriteBytes(const aBuffer: TIdBytes);
begin
  inherited;

  SendBytes := SendBytes + Length(aBuffer);
end;

function TsgcUDPSocket_Indy.GetIP: String;
begin
  if FIP = '' then
  begin
    if Assigned(FSocket) then
      FIP := Socket.PeerIP;
  end;

  Result := FIP;
end;

function TsgcUDPSocket_Indy.GetLocalIP: String;
begin
  if FLocalIP = '' then
  begin
    if Assigned(FSocket) then
      FLocalIP := Socket.IP;
  end;

  Result := FLocalIP;
end;

function TsgcUDPSocket_Indy.GetLocalPort: Integer;
begin
  if FLocalPort = 0 then
  begin
    if Assigned(FSocket) then
      FLocalPort := Socket.Port;
  end;

  Result := FLocalPort;
end;

function TsgcUDPSocket_Indy.GetPort: Integer;
begin
  if FPort = 0 then
  begin
    if Assigned(FSocket) then
      FPort := Socket.PeerPort;
  end;

  Result := FPort;
end;

destructor TsgcUDPSocket_Indy.Destroy;
begin
  FSocket := nil;
  inherited;
end;

procedure TsgcUDPSocket_Indy.DisconnectPeer;
begin
  DoBeforeDisconnectEvent;

  if Assigned(FSocket) then
  begin
    Disconnected := True;

    Socket.CloseSocket;
{$IFDEF SGC_DEBUG}
    DoLog(self, self, 'DisconnectPeer');
{$ENDIF}
  end;
end;

procedure TsgcUDPSocket_Indy.DoBeforeDisconnectEvent;
begin
  if Assigned(FOnBeforeDisconnect) then
    FOnBeforeDisconnect(self);
end;

function TsgcUDPSocket_Indy.GetActive: Boolean;
begin
  Result := Assigned(Socket);
end;

function TsgcUDPSocket_Indy.GetIPVersion: TwsIPVersion;
begin
  if FIPVersion = ipV4 then
  begin
    if Assigned(FSocket) then
    begin
      case Socket.IPVersion of
        Id_IPv4: FIPVersion := ipV4;
        Id_IPv6: FIPVersion := ipV6;
      end;
    end;
  end;
  Result := FIPVersion;
end;

{$ENDIF}

end.
