{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_Example_Client;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Protocol_Base_Client, Classes, sgcWebSocket_Classes;

type
  TsgcWSProtocol_Example_Client = class(TsgcWSProtocol_Client_Base)
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

constructor TsgcWSProtocol_Example_Client.Create(aOwner: TComponent);
begin
  inherited;
  // ... here add your protocol name
  FProtocol := 'MyProtocol';
end;

procedure TsgcWSProtocol_Example_Client.DoEventConnect(aConnection:
    TsgcWSConnection);
begin
  inherited;
  // ... add your own code when client connects to server
end;

procedure TsgcWSProtocol_Example_Client.DoEventDisconnect(aConnection:
    TsgcWSConnection; Code: Integer);
begin
  // ... add your own code when client disconnects from server
  inherited;
end;

procedure TsgcWSProtocol_Example_Client.DoEventMessage(aConnection:
    TsgcWSConnection; const Text: string);
begin
  // ... process messages received from server
  // ... you can send a message to server using WriteData('your message') method
end;

{$ENDIF}

end.
