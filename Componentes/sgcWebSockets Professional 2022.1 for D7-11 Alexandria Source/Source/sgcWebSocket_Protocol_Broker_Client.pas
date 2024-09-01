{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_Broker_Client;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils, Types,
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}  
  {$IFDEF NEXTGEN} System.Generics.Collections, {$ELSE} Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Broker_Message,
  sgcWebSocket_Types, sgcWebSocket_Classes_SyncObjs;

type
  TsgcWSProtocol_Broker_Client = class(TsgcWSProtocol_Client)
    { protocol }
  private
    FWSConnection: TsgcWSConnection;
  private
    FProtocolObjectList: {$IFDEF NEXTGEN} TsgcWSProtocols {$ELSE} TObjectList
{$ENDIF};
  protected
    function ProtocolRegistered(const aProtocolName: String;
      aProtocol: TsgcWSProtocol): Boolean;
    function GetProtocols: String;
  public
    procedure DoRegisterProtocol(aObject: TsgcWSProtocol);
    procedure DoUnRegisterProtocol(aObject: TsgcWSProtocol);
  public
    procedure RegisterProtocol(aObject: TsgcWSProtocol);
    procedure UnRegisterProtocol(aObject: TsgcWSProtocol);
    { protocol }

  { message read }
  private
    FWSBrokerMessageReadId: String;
  protected
    function GetWSBrokerMessageReadByConnection(aConnection: TsgcWSConnection):
        TsgcWSBrokerMessage; virtual;
  { message read }

  { message write }
  private
    FWSBrokerMessageWriteId: String;
    FWSBrokerMessageWrite: TsgcWSBrokerMessage;
    function GetWSBrokerMessageWrite: TsgcWSBrokerMessage;
  protected
    function GetWSBrokerMessageWriteByConnection(aConnection: TsgcWSConnection):
        TsgcWSBrokerMessage; virtual;
  public
    property WSBrokerMessageWrite: TsgcWSBrokerMessage read GetWSBrokerMessageWrite
        write FWSBrokerMessageWrite;
  { message write }

  { binary read }
  private
    FWSBrokerBinaryReadId: String;
  protected
    function GetWSBrokerBinaryReadByConnection(aConnection: TsgcWSConnection):
        TsgcWSBrokerBinary; virtual;
  { binary read }

  { binary write }
  private
    FWSBrokerBinaryWriteId: String;
    FWSBrokerBinaryWrite: TsgcWSBrokerBinary;
  protected
    function GetWSBrokerBinaryWrite: TsgcWSBrokerBinary;
    function GetWSBrokerBinaryWriteByConnection(aConnection: TsgcWSConnection):
        TsgcWSBrokerBinary; virtual;
  public
    property WSBrokerBinaryWrite: TsgcWSBrokerBinary read GetWSBrokerBinaryWrite
        write FWSBrokerBinaryWrite;
  { binary write }

    { from TsgcWSComponent }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
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
  sgcBase_Helpers,
  sgcWebSocket_Const, sgcWebSocket_Helpers, sgcWebSocket_Protocol_Base_Client;

const
  CS_PROTOCOL_LIST = 0;

type
  TsgcWSProtocol_Broker = Class(TsgcWSProtocol)

  End;

function TsgcWSProtocol_Broker_Client.ProtocolRegistered
  (const aProtocolName: String; aProtocol: TsgcWSProtocol): Boolean;
begin
  Result := UpperCase(aProtocol.Protocol) = UpperCase(aProtocolName);
  if not Result then
    Result := UpperCase(aProtocol.Guid + '.' + aProtocol.Protocol) = UpperCase
      (aProtocolName);
end;

function TsgcWSProtocol_Broker_Client.GetProtocols: String;
var
  i: Integer;
  oList: TStringList;
begin
  Result := '';
  oList := TStringList.Create;
  oList.Delimiter := CS_DELIMITER;
  oList.Sorted := True;
  Try
    DoEnterCS(CS_PROTOCOL_LIST);
    Try
      for i := 0 to FProtocolObjectList.Count - 1 do
      begin
        if TsgcWSProtocol(FProtocolObjectList[i]).Guid <> '' then
          oList.Add(TsgcWSProtocol(FProtocolObjectList[i])
              .Guid + '.' + TsgcWSProtocol(FProtocolObjectList[i]).Protocol)
        else
          oList.Add(TsgcWSProtocol(FProtocolObjectList[i]).Protocol);
      end;
    Finally
      DoLeaveCS(CS_PROTOCOL_LIST);
    End;
    Result := oList.CommaText;
  Finally
    sgcFree(oList);
  End;
end;

procedure TsgcWSProtocol_Broker_Client.DoRegisterProtocol(aObject:
    TsgcWSProtocol);
begin
  DoEnterCS(CS_PROTOCOL_LIST);
  Try
    FProtocolObjectList.Add(aObject);
  Finally
    DoLeaveCS(CS_PROTOCOL_LIST);
  End;
end;

procedure TsgcWSProtocol_Broker_Client.DoUnRegisterProtocol(aObject:
    TsgcWSProtocol);
begin
  DoEnterCS(CS_PROTOCOL_LIST);
  Try
    FProtocolObjectList.Extract(aObject);
  Finally
    DoLeaveCS(CS_PROTOCOL_LIST);
  End;
end;

function TsgcWSProtocol_Broker_Client.GetWSBrokerMessageWrite:
    TsgcWSBrokerMessage;
begin
  result := GetWSBrokerMessageWriteByConnection(FWSConnection);

  if not Assigned(result) then
  begin
    if not Assigned(FWSBrokerMessageWrite) then
      FWSBrokerMessageWrite := TsgcWSBrokerMessage.Create(self);
    Result := FWSBrokerMessageWrite;
  end;
end;

function TsgcWSProtocol_Broker_Client.GetWSBrokerBinaryWrite:
    TsgcWSBrokerBinary;
begin
  result := GetWSBrokerBinaryWriteByConnection(FWSConnection);

  if not Assigned(result) then
  begin
    if not Assigned(FWSBrokerBinaryWrite) then
      FWSBrokerBinaryWrite := TsgcWSBrokerBinary.Create(self);
    Result := FWSBrokerBinaryWrite;
  end;
end;

procedure TsgcWSProtocol_Broker_Client.DoEventConnect
  (aConnection: TsgcWSConnection);
var
  i: Integer;
begin
  FWSConnection := aConnection;

  Try
    for i := FProtocolObjectList.Count - 1 Downto 0 do
      TsgcWSProtocol_Broker(FProtocolObjectList[i]).DoInitialize(aConnection);
    inherited;
  Except
    On E: Exception do
      DoError(aConnection, E.Message);
  End;
end;

procedure TsgcWSProtocol_Broker_Client.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
var
  i: Integer;
  vText: String;
  oBroker: TsgcWSBrokerMessage;
begin
  oBroker := GetWSBrokerMessageReadByConnection(aConnection);

  Try
    oBroker.Read(Text);

    vText := oBroker.Text;

    if oBroker.ID <> '' then
    begin
      for i := FProtocolObjectList.Count - 1 Downto 0 do
      begin
        if ProtocolRegistered(oBroker.ID,
          TsgcWSProtocol(FProtocolObjectList[i])) then
          TsgcWSProtocol_Broker(FProtocolObjectList[i]).DoEventMessage
            (aConnection, vText);
      end;
    end
    else
      inherited;
  Except
    On E: Exception do
      DoError(aConnection, Format(S_ERROR_READING_BROKER_MESSAGE, [E.Message]));
  End;
end;

procedure TsgcWSProtocol_Broker_Client.DoEventBinary(const aConnection:
    TsgcWSConnection; Data: TMemoryStream);
var
  i: Integer;
  oBroker: TsgcWSBrokerBinary;
begin
  oBroker := GetWSBrokerBinaryReadByConnection(aConnection);

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

procedure TsgcWSProtocol_Broker_Client.DoEventFragmented(const aConnection:
    TsgcWSConnection; Data: TMemoryStream; OpCode: TOpCode; Fin: Boolean);
var
  i: Integer;
  oBroker: TsgcWSBrokerBinary;
begin
  oBroker := GetWSBrokerBinaryReadByConnection(aConnection);

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

procedure TsgcWSProtocol_Broker_Client.DoEventDisconnect(aConnection:
    TsgcWSConnection; Code: Integer);
var
  i: Integer;
  vDoInherited: Boolean;
begin
  vDoInherited := True;
  Try
    DoEnterCS(CS_PROTOCOL_LIST);
    Try
      for i := FProtocolObjectList.Count - 1 Downto 0 do
        TsgcWSProtocol_Broker(FProtocolObjectList[i]).DoFinalize(aConnection);
    Finally
      DoLeaveCS(CS_PROTOCOL_LIST);
    End;

    inherited;
    vDoInherited := False;

    DoEnterCS(CS_PROTOCOL_LIST);
    Try
      for i := FProtocolObjectList.Count - 1 Downto 0 do
        TsgcWSProtocol_Broker(FProtocolObjectList[i]).DoClear(aConnection);
    Finally
      DoLeaveCS(CS_PROTOCOL_LIST);
    End;

    FWSConnection := nil;
  Except
    On E: Exception do
    begin
      FWSConnection := nil;

      if vDoInherited then
        inherited;
      DoError(aConnection, E.Message);
    end;
  End;
end;

constructor TsgcWSProtocol_Broker_Client.Create(aOwner: TComponent);
begin
  inherited;
  FWSBrokerMessageReadId := NewGuid;
  FWSBrokerMessageWriteId := NewGuid;
  FWSBrokerBinaryReadId := NewGuid;
  FWSBrokerBinaryWriteId := NewGuid;
  NotifyEvents := neNoSync;
  FProtocolObjectList := {$IFDEF NEXTGEN} TsgcWSProtocols {$ELSE} TObjectList
{$ENDIF}.Create;
  FProtocolObjectList.OwnsObjects := False;
  FProtocol := CS_PROTOCOL_BROKER;
end;

destructor TsgcWSProtocol_Broker_Client.Destroy;
begin
  sgcFree(FProtocolObjectList);
  inherited;
end;

function TsgcWSProtocol_Broker_Client.GetWSBrokerMessageReadByConnection(
    aConnection: TsgcWSConnection): TsgcWSBrokerMessage;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSBrokerMessageReadId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSBrokerMessage.Create(nil);
      AddProtocolDataItem(aConnection, FWSBrokerMessageReadId, oItem);
    end;

    result := TsgcWSBrokerMessage(oItem.WSMessage);
  end;
end;

function TsgcWSProtocol_Broker_Client.GetWSBrokerBinaryReadByConnection(
    aConnection: TsgcWSConnection): TsgcWSBrokerBinary;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSBrokerBinaryReadId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSBrokerBinary.Create(nil);
      AddProtocolDataItem(aConnection, FWSBrokerBinaryReadId, oItem);
    end;

    result := TsgcWSBrokerBinary(oItem.WSMessage);
  end;
end;

function TsgcWSProtocol_Broker_Client.GetWSBrokerBinaryWriteByConnection(
    aConnection: TsgcWSConnection): TsgcWSBrokerBinary;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSBrokerBinaryWriteId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSBrokerBinary.Create(nil);
      AddProtocolDataItem(aConnection, FWSBrokerBinaryWriteId, oItem);
    end;

    result := TsgcWSBrokerBinary(oItem.WSMessage);
  end;
end;

function TsgcWSProtocol_Broker_Client.GetWSBrokerMessageWriteByConnection(
    aConnection: TsgcWSConnection): TsgcWSBrokerMessage;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSBrokerMessageWriteId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSBrokerMessage.Create(nil);
      AddProtocolDataItem(aConnection, FWSBrokerMessageWriteId, oItem);
    end;

    result := TsgcWSBrokerMessage(oItem.WSMessage);
  end;
end;

procedure TsgcWSProtocol_Broker_Client.RegisterProtocol(aObject:
    TsgcWSProtocol);
begin
  TsgcWSProtocol_Client_Base(aObject).Broker := self;
  TsgcWSProtocol_Client_Base(aObject).Client := Client;
end;

procedure TsgcWSProtocol_Broker_Client.UnRegisterProtocol(aObject:
    TsgcWSProtocol);
begin
  TsgcWSProtocol_Client_Base(aObject).Broker := nil;
  TsgcWSProtocol_Client_Base(aObject).Client := nil;
end;

{$ENDIF}

end.
