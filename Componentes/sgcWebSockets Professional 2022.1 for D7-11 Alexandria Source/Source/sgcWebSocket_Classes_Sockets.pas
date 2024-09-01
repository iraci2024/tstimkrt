{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Classes_Sockets;

interface

{$I sgcVer.inc}

{$IFDEF SGC_SOCKETS}

uses
  Classes, SysUtils,
  // indy
  {$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Classes_Indy, sgcWebSocket_Helpers,
  sgcWebSocket_Sockets_Base, sgcWebSocket_Types, sgcWebSocket_Server_Base,
  sgcWebSocket_Sockets_HTTP_Server;

type
  TsgcWSConnection_Sockets = class(TsgcWSConnectionServer_Base)
    { TCPConnection }
  protected
    function ExistsTCPConnection: Boolean; override;
    function GetActive: Boolean; override;
    { TCPConnection }

    { fields }
  protected
    function GetIP: String; override;
    function GetPort: Integer; override;
    function GetLocalIP: String; override;
    function GetLocalPort: Integer; override;
  { fields }

  { disconnect }
  protected
    procedure DoClearBuffer; override;
  public
    procedure DisconnectPeer; override;
  { disconnect }

  { write data }
  protected
    procedure DoWriteBytes(aBuffer: TIdBytes); override;
    procedure DoWriteLn(const aText: String = ''); override;
  protected
    procedure DoWriteBufferOpen; override;
    procedure DoWriteBufferFlush; override;
    procedure DoWriteBufferClose; override;
  { write data }

  { read data }
  protected
    function GetPayloadReadLength: Integer; override;
    procedure DoReadBytes(var aBuffer: TIdBytes; aCount: Integer); override;
    function GetReadBufferSize: Integer; override;
  protected
    procedure ReadData(aErrorIfNotMasked: Boolean = False); override;
  { read data }

  { properties }
  protected
    FBuffer: TsgcArrayOfBytes;
    FSocketConnection: IsgcSocketsHttpConnection;
  { properties }

    { events }
  private
    FOnDisconnect: TsgcWSDisconnectEvent;
  protected
    procedure DoEventDisconnect; virtual;
  public
    property OnDisconnect: TsgcWSDisconnectEvent read FOnDisconnect write FOnDisconnect;
    { events }
  end;

  TsgcWSHandShake_Sockets = class(TsgcWSHandShakeServer_Base)

  end;

  TsgcWSComponent_Server_Sockets = class(TsgcWSComponent_Server)

  End;

{$ENDIF}

implementation

{$IFDEF SGC_SOCKETS}

uses
  sgcWebSocket_Const;

procedure TsgcWSConnection_Sockets.DisconnectPeer;
begin
  inherited;
  if ExistsTCPConnection then
    FSocketConnection.Disconnect;
end;

procedure TsgcWSConnection_Sockets.DoClearBuffer;
begin
  inherited;
  if Assigned(FBuffer) then
    SetLength(FBuffer, 0);
end;

procedure TsgcWSConnection_Sockets.DoEventDisconnect;
begin
  FSocketConnection := nil;

  if Assigned(FOnDisconnect) then
    FOnDisconnect(self, 0);
end;

procedure TsgcWSConnection_Sockets.DoReadBytes(var aBuffer: TIdBytes; aCount:
    Integer);
begin
  setLength(aBuffer, aCount);
  aBuffer := TIdBytes(Copy(FBuffer, 0, aCount));

  FBuffer := Copy(FBuffer, aCount, Length(FBuffer));

  RecBytes := RecBytes + aCount;
end;

procedure TsgcWSConnection_Sockets.DoWriteBufferClose;
begin
  inherited;
end;

procedure TsgcWSConnection_Sockets.DoWriteBufferFlush;
begin
  inherited;
end;

procedure TsgcWSConnection_Sockets.DoWriteBufferOpen;
begin
  inherited;
end;

procedure TsgcWSConnection_Sockets.DoWriteBytes(aBuffer: TIdBytes);
begin
  inherited;
  if ExistsTCPConnection then
  begin
    FSocketConnection.SendBytes(TBytes(aBuffer), 0, Length(aBuffer));
    SendBytes := SendBytes + Length(aBuffer);
  end;
end;

procedure TsgcWSConnection_Sockets.DoWriteLn(const aText: String = '');
var
  oStream: TsgcStringStream;
begin
  inherited;
  oStream := TsgcStringStream.Create(aText);
  Try
    DoWriteBytes(TIdBytes(oStream.Bytes));
  Finally
    sgcFree(oStream);
  End;
end;

function TsgcWSConnection_Sockets.ExistsTCPConnection: Boolean;
begin
  Result := Assigned(FSocketConnection);
end;

function TsgcWSConnection_Sockets.GetActive: Boolean;
begin
  Result := True; // todo
end;

function TsgcWSConnection_Sockets.GetIP: String;
begin
  if (FIP = '') and (ExistsTCPConnection = True) then
    FIP := FSocketConnection.PeerAddr;

  result := FIP;
end;

function TsgcWSConnection_Sockets.GetLocalIP: String;
begin
  if (FLocalIP = '') and (ExistsTCPConnection = True) then
    FLocalIP := FSocketConnection.LocalAddr;

  result := FLocalIP;
end;

function TsgcWSConnection_Sockets.GetLocalPort: Integer;
begin
  if (FLocalPort = 0) and (ExistsTCPConnection = True) then
    FLocalPort := FSocketConnection.LocalPort;

  result := FLocalPort;
end;

function TsgcWSConnection_Sockets.GetPayloadReadLength: Integer;
begin
  Result := Length(FBuffer);
end;

function TsgcWSConnection_Sockets.GetPort: Integer;
begin
  if (FPort = 0) and (ExistsTCPConnection = True) then
    FPort := FSocketConnection.PeerPort;

  result := FPort;
end;

function TsgcWSConnection_Sockets.GetReadBufferSize: Integer;
begin
  Result := 0;
  if Assigned(FBuffer) then
    Result := Length(FBuffer);
end;

procedure TsgcWSConnection_Sockets.ReadData(aErrorIfNotMasked: Boolean = False);
begin
  if not Enabled then
  begin
    DoHandshake;
    FState := 0;
  end
  else
  begin
    while (Length(FBuffer) > 0) and (Disconnected = False) and (FMustDisconnect = False) do
    begin
      case Transport of
        trpTCP:
          DoReadData_TCP;
      else
        case Specification of
          spRFC6455:
            DoReadData_RFC6455(aErrorIfNotMasked);
          spHixie76:
            DoReadData_Hixie76;
        end;
      end;
    end;
  end;
end;

{$ENDIF}

end.
