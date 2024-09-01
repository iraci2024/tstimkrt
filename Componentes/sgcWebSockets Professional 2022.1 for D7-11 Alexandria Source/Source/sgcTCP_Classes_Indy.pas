{***************************************************************************
 sgcTCP component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcTCP_Classes_Indy;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, SyncObjs,
  // indy
  {$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdSSL{$ELSE}IdSSL{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdTCPConnection{$ELSE}IdTCPConnection{$ENDIF},
  {$IFDEF SGC_INDY}sgcIdComponent{$ELSE}IdComponent{$ENDIF},
  // sgcTCP
  sgcTCP_Classes, sgcWebSocket_Classes_Indy, sgcWebSocket_Types;

type

  TsgcTCPConnection_Indy = class(TsgcTCPConnection)
    { TCPConnection }
  private
    FTCPConnection: TIdTCPConnection;
    function GetTCPConnection: TIdTCPConnection;
    procedure SetTCPConnection(const Value: TIdTCPConnection);
  protected
    property TCPConnection: TIdTCPConnection read GetTCPConnection
      write SetTCPConnection;
  protected
    function ExistsTCPConnection: Boolean; virtual;
    function GetActive: Boolean; override;
    { TCPConnection }

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
  protected
    procedure DoClearBuffer; virtual;
  public
    procedure DisconnectPeer(aCallOnDisconnectIfClosed: Boolean = False); virtual;
    { disconnect }

    { write data }
  protected
    procedure DoWriteBytes(const aBuffer: TIdBytes); virtual;
    procedure DoWriteLn(const aText: String = ''); virtual;
  protected
    procedure DoWriteBufferOpen; virtual;
    procedure DoWriteBufferFlush; virtual;
    procedure DoWriteBufferClose; virtual;
    { write data }

    { read data }
  protected
    procedure DoReadBytes(var aBuffer: TIdBytes; aCount: Integer); virtual;
    function GetReadBufferSize: Integer; virtual;
    { read data }

    { constructor / destructor }
  public
    destructor Destroy; override;
    { constructor / destructor }

    { event }
  private
    FOnBeforeDisconnect: TNotifyEvent;
  public
    property OnBeforeDisconnect: TNotifyEvent read FOnBeforeDisconnect
      write FOnBeforeDisconnect;
    { event }
  end;

implementation

uses
  sgcBase_Helpers;

procedure TsgcTCPConnection_Indy.DoReadBytes(var aBuffer: TIdBytes;
  aCount: Integer);
begin
  if Assigned(TCPConnection) then
  begin
    if Assigned(TCPConnection.IOHandler) then
    begin
      TCPConnection.IOHandler.ReadBytes(aBuffer, aCount);
      RecBytes := RecBytes + aCount;
    end;
  end;
end;

procedure TsgcTCPConnection_Indy.DoWriteBytes(const aBuffer: TIdBytes);
begin
  inherited;
  TCPConnection.IOHandler.Write(aBuffer);
  SendBytes := SendBytes + Length(aBuffer);
end;

function TsgcTCPConnection_Indy.GetTCPConnection: TIdTCPConnection;
begin
  Result := FTCPConnection;
end;

function TsgcTCPConnection_Indy.GetIP: String;
begin
  if (FIP = '') and Assigned(TCPConnection) then
  begin
    Try
      if Assigned(TCPConnection.Socket) then
        FIP := TCPConnection.Socket.Binding.PeerIP;
    except
      // nothing
    end;
  end;
  Result := FIP;
end;

function TsgcTCPConnection_Indy.GetLocalIP: String;
begin
  if (FLocalIP = '') and Assigned(TCPConnection) then
  begin
    Try
      if Assigned(TCPConnection.Socket) then
        FLocalIP := TCPConnection.Socket.Binding.IP;
    except
      // nothing
    end;
  end;
  Result := FLocalIP;
end;

function TsgcTCPConnection_Indy.GetLocalPort: Integer;
begin
  if (FLocalPort = 0) and Assigned(TCPConnection) then
  begin
    Try
      if Assigned(TCPConnection.Socket) then
        FLocalPort := TCPConnection.Socket.Binding.Port;
    except
      // nothing
    end;
  end;
  Result := FLocalPort;
end;

function TsgcTCPConnection_Indy.GetPort: Integer;
begin
  if (FPort = 0) and Assigned(TCPConnection) then
  begin
    if Assigned(TCPConnection.Socket) then
    begin
      Try
        FPort := TCPConnection.Socket.Binding.PeerPort;
      Except
        // nothing
      End;
    end;
  end;

  Result := FPort;
end;

procedure TsgcTCPConnection_Indy.SetTCPConnection
  (const Value: TIdTCPConnection);
begin
  if Assigned(self) then
  begin
    FTCPConnection := Value;
    if Value <> nil then
    begin
      GetIP;
      GetPort;
    end;
  end;
end;

destructor TsgcTCPConnection_Indy.Destroy;
begin
  FTCPConnection := nil;
  inherited;
end;

procedure TsgcTCPConnection_Indy.DisconnectPeer(aCallOnDisconnectIfClosed:
    Boolean = False);
begin
  DoBeforeDisconnectEvent;

  if Assigned(TCPConnection) then
  begin
    if Assigned(TCPConnection.IOHandler) then
    begin
      Disconnected := True;

      TCPConnection.IOHandler.WriteBufferClear;
      TCPConnection.IOHandler.InputBuffer.Clear;
{$IFNDEF INDY10_6_2_D10_4} // from this indy version, setting passtrhough := True, locks the thread
{$IFNDEF NEXTGEN}
          if TCPConnection.IOHandler is TIdSSLIOHandlerSocketBase then
            TIdSSLIOHandlerSocketBase(TCPConnection.IOHandler).PassThrough := True;
          // don't read ssl messages if disconnected
{$ENDIF}
{$ENDIF}
      // ... if IOHandler is already closed, notify the disconnection
      // ... otherwise OnDisconnect event maybe it's not called
      if aCallOnDisconnectIfClosed and (TCPConnection.IOHandler.Opened = False) then
        TCPConnection.OnStatus(nil, hsDisconnected, '')
      else
        TCPConnection.Disconnect(False);
{$IFDEF SGC_DEBUG}
      DoLog(self, self, 'DisconnectPeer');
{$ENDIF}
    end;
  end;
end;

procedure TsgcTCPConnection_Indy.DoBeforeDisconnectEvent;
begin
  if Assigned(FOnBeforeDisconnect) then
    FOnBeforeDisconnect(self);
end;

procedure TsgcTCPConnection_Indy.DoClearBuffer;
begin
  inherited;
  if Assigned(TCPConnection) then
  begin
    if not TCPConnection.IOHandler.InputBufferIsEmpty then
      TCPConnection.IOHandler.InputBuffer.Clear;
  end;
end;

procedure TsgcTCPConnection_Indy.DoWriteBufferClose;
begin
  inherited;
  TCPConnection.IOHandler.WriteBufferClose;
end;

procedure TsgcTCPConnection_Indy.DoWriteBufferFlush;
begin
  inherited;
  TCPConnection.IOHandler.WriteBufferFlush;
end;

procedure TsgcTCPConnection_Indy.DoWriteBufferOpen;
begin
  inherited;
  TCPConnection.IOHandler.WriteBufferOpen;
end;

procedure TsgcTCPConnection_Indy.DoWriteLn(const aText: String = '');
begin
  inherited;
  if aText <> '' then
    TCPConnection.IOHandler.WriteLn(aText)
  else
    TCPConnection.IOHandler.WriteLn;
end;

function TsgcTCPConnection_Indy.ExistsTCPConnection: Boolean;
begin
  Result := Assigned(FTCPConnection);
end;

function TsgcTCPConnection_Indy.GetActive: Boolean;
begin
  Result := False;
  if Assigned(TCPConnection) then
  begin
    Try
      if TCPConnection.IOHandler <> nil then
        Result := TCPConnection.Connected;
    Except
      // nothing
    End;
  end;
end;

function TsgcTCPConnection_Indy.GetIPVersion: TwsIPVersion;
begin
  if (FIPVersion = ipV4) and Assigned(TCPConnection) then
  begin
    Try
      if Assigned(TCPConnection.Socket) then
      begin
        case TCPConnection.Socket.Binding.IPVersion of
          Id_IPv4: FIPVersion := ipV4;
          Id_IPv6: FIPVersion := ipV6;
        end;
      end;
    except
      // nothing
    end;
  end;
  Result := FIPVersion;
end;

function TsgcTCPConnection_Indy.GetReadBufferSize: Integer;
begin
  Result := TCPConnection.IOHandler.InputBuffer.size;
end;

end.
