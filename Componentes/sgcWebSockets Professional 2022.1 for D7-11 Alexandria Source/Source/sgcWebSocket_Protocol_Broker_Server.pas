{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_Broker_Server;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  {$IFDEF DXE3}System.Types, {$ELSE}Types, {$ENDIF}
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Broker_Message,
  sgcWebSocket_Types, sgcWebSocket_Classes_SyncObjs;


type
  TsgcWSProtocol_Broker_Server = class(TsgcWSProtocol_Server)
  { protocol }
  private
    FProtocolObjectList: {$IFDEF NEXTGEN}TsgcWSProtocols{$ELSE}TObjectList{$ENDIF};
  protected
    function ProtocolRegistered(aProtocolName: String; aProtocol: TsgcWSProtocol):
        Boolean;
    function GetProtocols: String;
  public
    procedure DoRegisterProtocol(aObject: TsgcWSProtocol);
    procedure DoUnRegisterProtocol(aObject: TsgcWSProtocol);
  public
    procedure RegisterProtocol(aObject: TsgcWSProtocol);
    procedure UnRegisterProtocol(aObject: TsgcWSProtocol);
  { protocol }
  { message }
  private
    FWSBrokerMessageId: String;
  protected
    function GetWSBrokerMessageByConnection(aConnection: TsgcWSConnection):
        TsgcWSBrokerMessage; virtual;
  { message }

  { binary }
  private
    FWSBrokerBinaryId: String;
  protected
    function GetWSBrokerBinaryByConnection(aConnection: TsgcWSConnection):
        TsgcWSBrokerBinary; virtual;
  { binary }
  { from TsgcWSComponent }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection; const Text: string);
        override;
    procedure DoEventBinary(const aConnection: TsgcWSConnection; Data:
        TMemoryStream); override;
    procedure DoEventFragmented(const aConnection: TsgcWSConnection; Data:
        TMemoryStream; OpCode: TOpCode; Fin: Boolean); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection; Code: Integer);
        override;
  { from TsgcWSComponent }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcWebSocket_Helpers, sgcWebSocket_Protocol_Base_Server,
  sgcWebSocket_Protocol_Base_Message, sgcBase_Helpers;

const
  CS_PROTOCOL_LIST = 0;

type
  TsgcWSProtocol_Broker = Class(TsgcWSProtocol)

  End;

function TsgcWSProtocol_Broker_Server.ProtocolRegistered(aProtocolName: String;
    aProtocol: TsgcWSProtocol): Boolean;
begin
  result := UpperCase(aProtocol.Protocol) = UpperCase(aProtocolName);
  if not result then
    result := UpperCase(aProtocol.GUID + '.' + aProtocol.Protocol) = UpperCase(aProtocolName);
end;

function TsgcWSProtocol_Broker_Server.GetProtocols: String;
var
  i: Integer;
  oList: TStringList;
begin
  Result := '';
  oList := TStringList.Create;
  oList.Delimiter := CS_DELIMITER;
  oList.Sorted := True;
  Try
    for i := 0 to FProtocolObjectList.Count - 1 do
    begin
      if TsgcWSProtocol(FProtocolObjectList[i]).Guid <> '' then
        oList.Add(
          TsgcWSProtocol(FProtocolObjectList[i]).Guid + '.' +
          TsgcWSProtocol(FProtocolObjectList[i]).Protocol)
      else
        oList.Add(
          TsgcWSProtocol(FProtocolObjectList[i]).Protocol);
    end;
    result := oList.CommaText;
  Finally
    sgcFree(oList);
  End;
end;

procedure TsgcWSProtocol_Broker_Server.DoRegisterProtocol(aObject:
    TsgcWSProtocol);
begin
  DoEnterCS(CS_PROTOCOL_LIST);
  Try
    FProtocolObjectList.Add(aObject);
  Finally
    DoLeaveCS(CS_PROTOCOL_LIST);
  End;
end;

procedure TsgcWSProtocol_Broker_Server.DoUnRegisterProtocol(aObject:
    TsgcWSProtocol);
begin
  DoEnterCS(CS_PROTOCOL_LIST);
  Try
    FProtocolObjectList.Extract(aObject);
  Finally
    DoLeaveCS(CS_PROTOCOL_LIST);
  End;
end;

procedure TsgcWSProtocol_Broker_Server.DoEventConnect(aConnection:
    TsgcWSConnection);
var
  i: Integer;
begin
  Try
    // ... initialize connections
    for i := FProtocolObjectList.Count - 1 Downto 0 do
      TsgcWSProtocol_Broker(FProtocolObjectList[i]).DoInitialize(aConnection);

    inherited;
  Except
    On E: Exception do
      DoError(aConnection, E.Message);
  End;
end;

procedure TsgcWSProtocol_Broker_Server.DoEventMessage(aConnection:
    TsgcWSConnection; const Text: string);
var
  i: Integer;
  vText: string;
  oBroker: TsgcWSBrokerMessage;
begin
  if Text = '' then exit;

  oBroker := GetWSBrokerMessageByConnection(aConnection);
  Try
    oBroker.Read(Text);

    vText := oBroker.Text;

    if oBroker.ID <> '' then
    begin
      for i := FProtocolObjectList.Count - 1 Downto 0 do
      begin
        if ProtocolRegistered(oBroker.ID, TsgcWSProtocol(FProtocolObjectList[i])) then
          TsgcWSProtocol_Broker(FProtocolObjectList[i]).DoEventMessage(
            aConnection, vText);
      end;
    end
    else
      inherited;
  Except
    On E: Exception do
      DoError(aConnection, Format(S_ERROR_READING_BROKER_MESSAGE, [E.Message]));
  End;
end;

procedure TsgcWSProtocol_Broker_Server.DoEventBinary(const aConnection:
    TsgcWSConnection; Data: TMemoryStream);
var
  i: Integer;
  oBroker: TsgcWSBrokerBinary;
begin
  if Data.Size = 0 then exit;

  oBroker := GetWSBrokerBinaryByConnection(aConnection);

  Try
    oBroker.Read(Data);

    if oBroker.ID <> '' then
    begin
      for i := FProtocolObjectList.Count - 1 Downto 0 do
      begin
        if ProtocolRegistered(oBroker.ID, TsgcWSProtocol(FProtocolObjectList[i])) then
          TsgcWSProtocol_Broker(FProtocolObjectList[i]).DoEventBinary(
            aConnection, Data);
      end;
    end
    else
      inherited;
  Except
    On E: Exception do
      DoError(aConnection, Format(S_ERROR_READING_BROKER_MESSAGE, [E.Message]));
  End;
end;

procedure TsgcWSProtocol_Broker_Server.DoEventFragmented(const aConnection:
    TsgcWSConnection; Data: TMemoryStream; OpCode: TOpCode; Fin: Boolean);
var
  i: Integer;
  oBroker: TsgcWSBrokerBinary;
begin
  if Data.Size = 0 then exit;

  oBroker := GetWSBrokerBinaryByConnection(aConnection);
  Try
    oBroker.Read(Data);

    if oBroker.ID <> '' then
    begin
      for i := FProtocolObjectList.Count - 1 Downto 0 do
      begin
        if ProtocolRegistered(oBroker.ID, TsgcWSProtocol(FProtocolObjectList[i])) then
          TsgcWSProtocol_Broker(FProtocolObjectList[i]).DoEventFragmented(
            aConnection, Data, OpCode, Fin);
      end;
    end
    else
      inherited;
  Except
    On E: Exception do
      DoError(aConnection, Format(S_ERROR_READING_BROKER_MESSAGE, [E.Message]));
  End;
end;

procedure TsgcWSProtocol_Broker_Server.DoEventDisconnect(aConnection:
    TsgcWSConnection; Code: Integer);
var
  i: Integer;
  vDoInherited: Boolean;
begin
  vDoInherited := True;
  Try
    for i := FProtocolObjectList.Count - 1 Downto 0 do
      TsgcWSProtocol_Broker(FProtocolObjectList[i]).DoFinalize(aConnection);

    inherited;
    vDoInherited := False;

    for i := FProtocolObjectList.Count - 1 Downto 0 do
      TsgcWSProtocol_Broker(FProtocolObjectList[i]).DoClear(aConnection);
  Except
    On E: Exception do
    begin
      if vDoInherited then
        inherited;
      DoError(aConnection, E.Message);
    end;
  End;
end;

constructor TsgcWSProtocol_Broker_Server.Create(aOwner: TComponent);
begin
  inherited;
  FWSBrokerMessageId := NewGuid;
  FWSBrokerBinaryId := NewGuid;
  NotifyEvents := neNoSync;
  FProtocolObjectList := {$IFDEF NEXTGEN}TsgcWSProtocols{$ELSE}TObjectList{$ENDIF}.Create;
  FProtocolObjectList.OwnsObjects := False;
  FProtocol := CS_PROTOCOL_BROKER;
end;

destructor TsgcWSProtocol_Broker_Server.Destroy;
begin
  sgcFree(FProtocolObjectList);
  inherited;
end;

function TsgcWSProtocol_Broker_Server.GetWSBrokerMessageByConnection(
    aConnection: TsgcWSConnection): TsgcWSBrokerMessage;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSBrokerMessageId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSBrokerMessage.Create(nil);
      AddProtocolDataItem(aConnection, FWSBrokerMessageId, oItem);
    end;

    result := TsgcWSBrokerMessage(oItem.WSMessage);
  end;
end;

function TsgcWSProtocol_Broker_Server.GetWSBrokerBinaryByConnection(
    aConnection: TsgcWSConnection): TsgcWSBrokerBinary;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSBrokerBinaryId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSBrokerBinary.Create(nil);
      AddProtocolDataItem(aConnection, FWSBrokerBinaryId, oItem);
    end;

    result := TsgcWSBrokerBinary(oItem.WSMessage);
  end;
end;

procedure TsgcWSProtocol_Broker_Server.RegisterProtocol(aObject:
    TsgcWSProtocol);
begin
  TsgcWSProtocol_Server_Base(aObject).Broker := self;
  TsgcWSProtocol_Server_Base(aObject).Server := Server;
end;

procedure TsgcWSProtocol_Broker_Server.UnRegisterProtocol(aObject:
    TsgcWSProtocol);
begin
  TsgcWSProtocol_Server_Base(aObject).Broker := nil;
  TsgcWSProtocol_Server_Base(aObject).Server := nil;
end;

{$ENDIF}

end.

