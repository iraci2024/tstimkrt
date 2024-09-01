{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcIWWebSocket_Protocol_Base;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

{$IFDEF IWIX}

uses
  Classes, SysUtils,
  sgcWebSocket_Protocol_Base_Message,
  sgcIWWebSocket_Client, sgcWebSocket_Classes;

type


  TsgcIWWSProtocol_Base = class(TsgcIWWSProtocol)
  { subscription }
  private
    FOnAsyncSubscription: TOnMessageEvent;
    FOnAsyncUnSubscription: TOnMessageEvent;
  public
    procedure Subscribe(const aChannel: String; const aGuid: String = ''); virtual;
        abstract;
    procedure UnSubscribe(const aChannel: String; const aGuid: String = '');
        virtual; abstract;
  { subscription }

  { send message }
  public
    procedure WriteData(const aText: String); virtual;
  { send message }

  { events }
  protected
    procedure DoNotifySubscription(const aChannel: String); virtual;
    procedure DoNotifyUnSubscription(const aChannel: String); virtual;
  public
    property OnAsyncSubscription: TOnMessageEvent read FOnAsyncSubscription write
        FOnAsyncSubscription;
    property OnAsyncUnSubscription: TOnMessageEvent read FOnAsyncUnSubscription
        write FOnAsyncUnSubscription;
  End;

{$ENDIF}

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

{$IFDEF IWIX}

procedure TsgcIWWSProtocol_Base.DoNotifySubscription(const aChannel: String);
begin
  if Assigned(FOnAsyncSubscription) then FOnAsyncSubscription(self, aChannel);  
end;

procedure TsgcIWWSProtocol_Base.DoNotifyUnSubscription(const aChannel: String);
begin
  if Assigned(FOnAsyncUnSubscription) then FOnAsyncUnSubscription(self, aChannel);
end;

procedure TsgcIWWSProtocol_Base.WriteData(const aText: String);
begin
  if Assigned(Client) then
    Client.WriteData(aText);
end;

{$ENDIF}

{$ENDIF}

end.
