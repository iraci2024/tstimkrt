{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_LoadBalancer_Client;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
  sgcWebSocket_Client, sgcWebSocket_Types, sgcWebSocket_LoadBalancer_Message,
  sgcWebSocket_Classes;

type
  TOnWriteDataText = procedure(const aGuid, aText: String) of object;
  TOnWriteDataBinary = procedure(const aGuid: String; aStream: TStream) of object;
  TOnBroadCastText = procedure(const aText, aChannel, aProtocol, aExclude, aInclude:
      String) of object;
  TOnBroadCastBinary = procedure(aStream: TStream; const aChannel, aProtocol, aExclude, aInclude:
      String; aSize: Integer; aStreaming: TwsStreaming) of object;

  TsgcWSLoadBalancerClient = class(TsgcWSClient)
  { from TsgcWSClient }
    procedure OnClientMessageEvent(aConnection: TsgcWSConnection; const Text:
        string); override;
    procedure OnClientBinaryEvent(aConnection: TsgcWSConnection; const aStream:
        TMemoryStream); override;
    procedure OnClientFragmentedEvent(aConnection: TsgcWSConnection; const aData:
        TMemoryStream; const aOpCode: TOpCode; const aContinuation: Boolean);
        override;
  { from TsgcWSClient }

  { message text }
  private
    FMessageWriteText: TsgcWSLoadBalancerMessage;
    FMessageReadText: TsgcWSLoadBalancerMessage;
  protected
    function GetMessageWriteText: TsgcWSLoadBalancerMessage;
    function GetMessageReadText: TsgcWSLoadBalancerMessage;
  public
    property MessageWriteText: TsgcWSLoadBalancerMessage read GetMessageWriteText
        write FMessageWriteText;
    property MessageReadText: TsgcWSLoadBalancerMessage read GetMessageReadText
        write FMessageReadText;
  { message text }

  { message binary }
  private
    FMessageWriteBinary: TsgcWSLoadBalancerBinary;
    FMessageReadBinary: TsgcWSLoadBalancerBinary;
  protected
    function GetMessageWriteBinary: TsgcWSLoadBalancerBinary;
    function GetMessageReadBinary: TsgcWSLoadBalancerBinary;
  public
    property MessageWriteBinary: TsgcWSLoadBalancerBinary read GetMessageWriteBinary
        write FMessageWriteBinary;
    property MessageReadBinary: TsgcWSLoadBalancerBinary read GetMessageReadBinary
        write FMessageReadBinary;
  { message binary }

  { constructor }
  public
    constructor Create(aOwner: TComponent); override;
  { constructor }

  { writedata }
  public
    procedure WriteData(const aGuid, aMessage: string); overload;
    procedure WriteData(const aGuid: String; aStream: TStream; aSize: Integer = 0;
        const aStreaming: TwsStreaming = stmNone); overload;
  { writedata }

  { broadcast }
  public
    procedure Broadcast(const aMessage: string; const aChannel: string = ''; const
        aProtocol: string = ''; const Exclude: String = ''; const Include: String =
        ''); overload;
    procedure Broadcast(aStream: TStream; const aChannel: string = ''; const
        aProtocol: string = ''; const Exclude: String = ''; const Include: String =
        ''; const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone); overload;
  { broadcast }

  { events }
  private
    FOnWriteDataText: TOnWriteDataText;
    FOnBroadCastText: TOnBroadCastText;
    FOnBroadCastBinary: TOnBroadCastBinary;
    FOnWriteDataBinary: TOnWriteDataBinary;
  public
    property OnWriteDataText: TOnWriteDataText read FOnWriteDataText write
        FOnWriteDataText;
    property OnBroadCastText: TOnBroadCastText read FOnBroadCastText write
        FOnBroadCastText;
    property OnBroadCastBinary: TOnBroadCastBinary read FOnBroadCastBinary write
        FOnBroadCastBinary;
    property OnWriteDataBinary: TOnWriteDataBinary read FOnWriteDataBinary write
        FOnWriteDataBinary;
  published
    property OnConnect;
    property OnDisconnect;
  { events }
  end;

implementation

uses sgcBase_Helpers, sgcWebSocket_Const;

const
  CS_MESSAGE_WRITE_TEXT = 0;
  CS_MESSAGE_WRITE_BINARY = 1;
  CS_MESSAGE_READ_BINARY = 2;
  CS_MESSAGE_READ_TEXT = 3;

constructor TsgcWSLoadBalancerClient.Create(aOwner: TComponent);
begin
  inherited;
  NotifyEvents := neNoSync;
  Options.Parameters := CS_LB_CLIENT;
end;

procedure TsgcWSLoadBalancerClient.Broadcast(const aMessage: string; const
    aChannel: string = ''; const aProtocol: string = ''; const Exclude: String
    = ''; const Include: String = '');
var
  vText: String;
begin
  DoEnterCS(CS_MESSAGE_WRITE_TEXT);
  Try
    vText := '';
    MessageWriteText.Clear;

    MessageWriteText.Text := aMessage;
    MessageWriteText.Channel := aChannel;
    MessageWriteText.Protocol := aProtocol;
    MessageWriteText.Exclude := Exclude;
    MessageWriteText.Include := Include;
    vText := MessageWriteText.Write;

    WriteData(vText);
  Finally
    DoLeaveCS(CS_MESSAGE_WRITE_TEXT);
  End;
end;

procedure TsgcWSLoadBalancerClient.Broadcast(aStream: TStream; const aChannel:
    string = ''; const aProtocol: string = ''; const Exclude: String = '';
    const Include: String = ''; const aSize: Integer = 0; const aStreaming:
    TwsStreaming = stmNone);
begin
  DoEnterCS(CS_MESSAGE_WRITE_BINARY);
  Try
    MessageWriteBinary.Clear;

    MessageWriteBinary.Channel := aChannel;
    MessageWriteBinary.Protocol := aProtocol;
    MessageWriteBinary.Exclude := Exclude;
    MessageWriteBinary.Include := Include;
    MessageWriteBinary.Size := aSize;
    MessageWriteBinary.Streaming := aStreaming;
    MessageWriteBinary.Write(TMemoryStream(aStream));

    WriteData(aStream);
  Finally
    DoLeaveCS(CS_MESSAGE_WRITE_BINARY);
  End;
end;

function TsgcWSLoadBalancerClient.GetMessageReadBinary:
    TsgcWSLoadBalancerBinary;
begin
  if not Assigned(FMessageReadBinary) then
    FMessageReadBinary := TsgcWSLoadBalancerBinary.Create(self);
  Result := FMessageReadBinary;
end;

function TsgcWSLoadBalancerClient.GetMessageWriteText:
    TsgcWSLoadBalancerMessage;
begin
  if not Assigned(FMessageWriteText) then
    FMessageWriteText := TsgcWSLoadBalancerMessage.Create(self);
  Result := FMessageWriteText;
end;

function TsgcWSLoadBalancerClient.GetMessageReadText: TsgcWSLoadBalancerMessage;
begin
  if not Assigned(FMessageReadText) then
    FMessageReadText := TsgcWSLoadBalancerMessage.Create(self);
  Result := FMessageReadText;
end;

function TsgcWSLoadBalancerClient.GetMessageWriteBinary:
    TsgcWSLoadBalancerBinary;
begin
  if not Assigned(FMessageWriteBinary) then
    FMessageWriteBinary := TsgcWSLoadBalancerBinary.Create(self);
  Result := FMessageWriteBinary;
end;

procedure TsgcWSLoadBalancerClient.OnClientBinaryEvent(aConnection:
    TsgcWSConnection; const aStream: TMemoryStream);
var
  oStream: TMemoryStream;
  vChannel: string;
  vExclude: string;
  vGuid: string;
  vInclude: string;
  vProtocol: string;
  vSize: Integer;
  vStreaming: TwsStreaming;
begin
  DoEnterCS(CS_MESSAGE_READ_BINARY);
  Try
    oStream := TMemoryStream.Create;
    Try
      oStream.CopyFrom(aStream, aStream.size);
      oStream.Position := 0;

      MessageReadBinary.Clear;
      MessageReadBinary.Read(oStream);

      vGuid := MessageReadBinary.Guid;
      if vGuid <> '' then
      begin
        if Assigned(FOnWriteDataBinary) then
          FOnWriteDataBinary(vGuid, oStream);
      end
      else
      begin
        vChannel := MessageReadBinary.Channel;
        vProtocol := MessageReadBinary.Protocol;
        vExclude := MessageReadBinary.Exclude;
        vInclude := MessageReadBinary.Include;
        vSize := MessageReadBinary.Size;
        vStreaming := MessageReadBinary.Streaming;
        if Assigned(FOnBroadCastBinary) then
          FOnBroadCastBinary(oStream, vChannel, vProtocol, vExclude, vInclude, vSize, vStreaming);
      end;
    Finally
      sgcFree(oStream);
    End;
  Finally
    DoLeaveCS(CS_MESSAGE_READ_BINARY);
  End;
end;

procedure TsgcWSLoadBalancerClient.OnClientFragmentedEvent(aConnection:
    TsgcWSConnection; const aData: TMemoryStream; const aOpCode: TOpCode; const
    aContinuation: Boolean);
var
  oStream: TsgcStringStream;
begin
  if aOpCode = opText then
  begin
    oStream := TsgcStringStream.Create('');
    Try
      oStream.CopyFrom(aData, aData.Size);
      oStream.Position := 0;

      OnClientMessageEvent(aConnection, oStream.DataString);
    Finally
      sgcFree(oStream);
    End;
  end
  else if aOpCode = opBinary then
    OnClientBinaryEvent(aConnection, aData);
end;

procedure TsgcWSLoadBalancerClient.OnClientMessageEvent(aConnection:
    TsgcWSConnection; const Text: string);
var
  vChannel: string;
  vExclude: string;
  vGuid: string;
  vInclude: string;
  vProtocol: string;
  vText: String;
begin
  DoEnterCS(CS_MESSAGE_READ_TEXT);
  Try
    MessageReadText.Clear;
    MessageReadText.Read(Text);

    vGuid := MessageReadText.Guid;
    vText := MessageReadText.Text;
    if vGuid <> '' then
    begin
      if Assigned(FOnWriteDataText) then
        FOnWriteDataText(vGuid, vText);
    end
    else
    begin
      vChannel := MessageReadText.Channel;
      vProtocol := MessageReadText.Protocol;
      vExclude := MessageReadText.Exclude;
      vInclude := MessageReadText.Include;
      if Assigned(FOnBroadCastText) then
        FOnBroadCastText(vText, vChannel, vProtocol, vExclude, vInclude);
    end;
  Finally
    DoLeaveCS(CS_MESSAGE_READ_TEXT);
  End;
end;

procedure TsgcWSLoadBalancerClient.WriteData(const aGuid, aMessage: string);
var
  vText: String;
begin
  DoEnterCS(CS_MESSAGE_WRITE_TEXT);
  Try
    vText := '';
    MessageWriteText.Clear;

    MessageWriteText.Guid := aGuid;
    MessageWriteText.Text := aMessage;
    vText := MessageWriteText.Write;

    WriteData(vText);
  Finally
    DoLeaveCS(CS_MESSAGE_WRITE_TEXT);
  End;
end;

procedure TsgcWSLoadBalancerClient.WriteData(const aGuid: String; aStream:
    TStream; aSize: Integer = 0;const aStreaming: TwsStreaming = stmNone);
begin
  DoEnterCS(CS_MESSAGE_WRITE_BINARY);
  Try
    MessageWriteBinary.Clear;

    MessageWriteBinary.Guid := aGuid;
    MessageWriteBinary.Size := aSize;
    MessageWriteBinary.Streaming := aStreaming;
    MessageWriteBinary.Write(TMemoryStream(aStream));

    WriteData(aStream);
  Finally
    DoLeaveCS(CS_MESSAGE_WRITE_BINARY);
  End;

end;

end.
