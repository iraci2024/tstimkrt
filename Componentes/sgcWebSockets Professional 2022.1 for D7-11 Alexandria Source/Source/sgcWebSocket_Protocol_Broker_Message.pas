{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_Broker_Message;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils, StrUtils;

type
  TsgcWSBrokerMessage = class(TComponent)
  private
    FConnectionId: String;
    FID: String;
    FText: String;
    FThreadId: Cardinal;
  public
    procedure Read(const aMessage: String);
    function Write: string;
  public
    property ID: String read FID write FID;
    property Text: String read FText write FText;
  public
    property ConnectionId: String read FConnectionId write FConnectionId;
    property ThreadId: Cardinal read FThreadId write FThreadId;
  end;

  TsgcWSBrokerBinary = class(TComponent)
  private
    FConnectionId: String;  
    FID: String;
    FThreadId: Cardinal;
  public
    procedure Read(var aStream: TMemoryStream);
    procedure Write(var aData: TMemoryStream);
  public
    property ID: String read FID write FID;
  public
    property ConnectionId: String read FConnectionId write FConnectionId;
    property ThreadId: Cardinal read FThreadId write FThreadId;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcWebSocket_Helpers;

procedure TsgcWSBrokerMessage.Read(const aMessage: String);
var
  n: integer;
begin
  n := StrToInt(LeftStr(aMessage, 3));

  ID := MidStr(aMessage, 4, n);
  Text := MidStr(aMessage, n + 4, Length(aMessage) - n - 3);
end;

function TsgcWSBrokerMessage.Write: string;
begin
  result := Format('%.3d', [Length(ID)]) + ID + Text;
end;

procedure TsgcWSBrokerBinary.Read(var aStream: TMemoryStream);
begin
  sgcWSStreamRead(aStream, FID);
end;

procedure TsgcWSBrokerBinary.Write(var aData: TMemoryStream);
begin
  sgcWSStreamWrite(ID, aData);
end;

{$ENDIF}

end.
