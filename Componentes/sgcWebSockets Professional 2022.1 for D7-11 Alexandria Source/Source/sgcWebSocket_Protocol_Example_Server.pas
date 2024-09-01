{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_Example_Server;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Protocol_Base_Server, Classes, sgcWebSocket_Classes;

type
  TsgcWSProtocol_Example_Server = class(TsgcWSProtocol_Server_Base)
  { from TsgcWSComponent }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection; const Text: string);
        override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection; Code: Integer);
        override;
  { from TsgcWSComponent }
  public
    constructor Create(aOwner: TComponent); override;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

constructor TsgcWSProtocol_Example_Server.Create(aOwner: TComponent);
begin
  inherited;
  // ... here add your protocol name
  FProtocol := 'MyProtocol';
end;

procedure TsgcWSProtocol_Example_Server.DoEventConnect(aConnection:
    TsgcWSConnection);
begin
  inherited;
  // ... add your own code when a client connects to server
end;

procedure TsgcWSProtocol_Example_Server.DoEventDisconnect(aConnection:
    TsgcWSConnection; Code: Integer);
begin
  // ... add your own code when a client disconnects from server
  inherited;
end;

procedure TsgcWSProtocol_Example_Server.DoEventMessage(aConnection:
    TsgcWSConnection; const Text: string);
begin
  inherited;
  // ... process messages received from clients
  // ... you can answer to client using WriteData(aConnection.Guid, 'your message') method
  // ... you can send a message to all clients using BroadCast('your message') method
end;

{$ENDIF}

end.
