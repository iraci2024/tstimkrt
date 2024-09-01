{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_STOMP_Message;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  // sgc
  sgcSTOMP;

type
  TsgcWSSTOMPMessage = class(TsgcSTOMP)
  private
    FThreadId: Cardinal;
    FConnectionId: String;
  public
    property ThreadId: Cardinal read FThreadId write FThreadId;
    property ConnectionId: String read FConnectionId write FConnectionId;
  end;

{$ENDIF}

implementation

end.
