{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcIWWebSocket_Protocol_sgc_Client;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

{$IFDEF IWIX}

uses
  Classes,
  sgcWebSocket_Protocol_sgc_Message,
  sgcIWWebSocket_Protocol_Base, sgcWebSocket_Classes;

type
  TsgcIWWSProtocol_sgc_Client = class(TsgcIWWSProtocol_Base)
  { WSMessage }
  protected
    FWSMessage: TsgcWSMessage;
    function GetWSMessage: TsgcWSMessage;
  protected
    property WSMessage: TsgcWSMessage read GetWSMessage write FWSMessage;
  { WSMessage }

  { Subscriptions }
  public
    procedure Subscribe(const aChannel: String; const aGuid: String = ''); override;
    procedure UnSubscribe(const aChannel: String; const aGuid: String = '');
        override;
  { Subscriptions }

  { notifiers }
  protected
    procedure DoNotifyMessage(const aMessage: String); override;
  { notifiers }

  public
    constructor Create(aOwner: TComponent); override;

  { WriteData }
  public
    procedure WriteData(const aText: String); overload; override;
    procedure WriteData(const aGuid, aChannel, aText: String); reintroduce;
        overload;
  { WriteData }
  end;

{$ENDIF}

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

{$IFDEF IWIX}

uses sgcWebSocket_Const;

constructor TsgcIWWSProtocol_sgc_Client.Create(aOwner: TComponent);
begin
  inherited;
  FProtocol := CS_PROTOCOL_SGC;
end;

procedure TsgcIWWSProtocol_sgc_Client.DoNotifyMessage(const aMessage: String);
begin
  WSMessage.Read(aMessage);

  if WSMessage.Method = CS_SGC_MESSAGE then
    DoNotifyMessage(WSMessage.Text)
  else if WSMessage.Method = CS_SGC_SUBSCRIBE then
    DoNotifySubscription(WSMessage.Channel)
  else if WSMessage.Method = CS_SGC_UNSUBSCRIBE then
    DoNotifyUnSubscription(WSMessage.Channel)
  else
    inherited DoNotifyMessage(aMessage);
end;

function TsgcIWWSProtocol_sgc_Client.GetWSMessage: TsgcWSMessage;
begin
  if not Assigned(FWSMessage) then
    FWSMessage := TsgcWSMessage.Create(self);
  Result := FWSMessage;
end;

procedure TsgcIWWSProtocol_sgc_Client.Subscribe(const aChannel: String; const
    aGuid: String = '');
begin
  WSMessage.Method := CS_SGC_SUBSCRIBE;
  WSMessage.Channel := aChannel;
  WSMessage.Guid := aGuid;
  inherited WriteData(WSMessage.Write);
end;

procedure TsgcIWWSProtocol_sgc_Client.UnSubscribe(const aChannel: String; const
    aGuid: String = '');
begin
  WSMessage.Method := CS_SGC_UNSUBSCRIBE;
  WSMessage.Channel := aChannel;
  WSMessage.Guid := aGuid;
  inherited WriteData(WSMessage.Write);
end;

procedure TsgcIWWSProtocol_sgc_Client.WriteData(const aText: String);
begin
  WSMessage.Text := aText;
  WSMessage.Method := CS_SGC_MESSAGE;
  WSMessage.Channel := '';
  WSMessage.Guid := '';
  inherited WriteData(WSMessage.Write);
end;

procedure TsgcIWWSProtocol_sgc_Client.WriteData(const aGuid, aChannel, aText:
    String);
begin
  WSMessage.Text := aText;
  WSMessage.Method := CS_SGC_MESSAGE;
  WSMessage.Channel := aChannel;
  WSMessage.Guid := aGuid;
  inherited WriteData(WSMessage.Write);
end;

{$ENDIF}

{$ENDIF}

end.
