{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_LoadBalancer_Message;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
  sgcWebSocket_Types;

type
  TsgcWSLoadBalancerMessage = class(TComponent)
  private
    FChannel: string;
    FExclude: string;
    FGuid: String;
    FInclude: string;
    FProtocol: String;
    FText: String;
  public
    procedure Clear;
    procedure Read(const aMessage: String);
    function Write: string;
  public
    property Channel: string read FChannel write FChannel;
    property Exclude: string read FExclude write FExclude;
    property Guid: String read FGuid write FGuid;
    property Include: string read FInclude write FInclude;
    property Protocol: String read FProtocol write FProtocol;
    property Text: String read FText write FText;
  end;

  TsgcWSLoadBalancerBinary = class(TComponent)
  private
    FChannel: string;
    FExclude: string;
    FGuid: String;
    FInclude: string;
    FProtocol: String;
    FSize: Integer;
    FStreaming: TwsStreaming;
  public
    procedure Clear;
    procedure Read(var aStream: TMemoryStream);
    procedure Write(var aData: TMemoryStream);
  public
    property Channel: string read FChannel write FChannel;
    property Exclude: string read FExclude write FExclude;
    property Guid: String read FGuid write FGuid;
    property Include: string read FInclude write FInclude;
    property Protocol: String read FProtocol write FProtocol;
    property Size: Integer read FSize write FSize;
    property Streaming: TwsStreaming read FStreaming write FStreaming;
  end;

  TsgcWSLoadBalancerClientConnection = class(TComponent)
  private
    FActive: Boolean;
    FChannel: string;
    FIP: string;
    FGuid: String;
    FProtocol: String;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    procedure Clear;
    procedure Read(const aMessage: String);
    function Write: string;
  public
    property Active: Boolean read FActive write FActive;
    property Channel: string read FChannel write FChannel;
    property IP: string read FIP write FIP;
    property Guid: String read FGuid write FGuid;
    property Protocol: String read FProtocol write FProtocol;
  end;

  TsgcWSLoadBalancerServerBinding = class(TComponent)
  private
    FHost: string;
    FPort: Integer;
    FSSL: Boolean;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    procedure Clear;
    procedure Read(const aMessage: String);
    function Write: string;
  public
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property SSL: Boolean read FSSL write FSSL;
  end;

  TsgcWSLoadBalancerServerData = class(TComponent)
  private
    FGuid: String;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    procedure Clear;
    procedure Read(const aMessage: String);
    function Write: string;
  public
    property Guid: String read FGuid write FGuid;
  end;

implementation

uses
  sgcWebSocket_Const, sgcWebSocket_Helpers, sgcJSON;

procedure TsgcWSLoadBalancerMessage.Clear;
begin
  Guid := '';
  Channel := '';
  Protocol := '';
  Include := '';
  Exclude := '';
  Text := '';
end;

procedure TsgcWSLoadBalancerMessage.Read(const aMessage: String);
var
  n: integer;
  oJSON: IsgcJSON;
begin
  n := StrToInt(LeftStr(aMessage, 3));

  oJSON := GetJSONInstance;
  Try
    oJSON.Read(MidStr(aMessage, 4, n));
    Guid := oJSON.Node[CS_GUID].Value;
    Channel := oJSON.Node[CS_CHANNEL].Value;
    Protocol := oJSON.Node[CS_PROTOCOL].Value;
    Include := oJSON.Node[CS_INCLUDE].Value;
    Exclude := oJSON.Node[CS_EXCLUDE].Value;
  Finally
    FreeJSONInstance(oJSON);
  End;

  Text := MidStr(aMessage, n + 4, Length(aMessage) - n - 3);
end;

function TsgcWSLoadBalancerMessage.Write: string;
var
  oJSON: IsgcJSON;
  vText: String;
begin
  vText := '';

  oJSON := GetJSONInstance;
  Try
    oJSON.AddPair(CS_GUID, Guid);
    oJSON.AddPair(CS_CHANNEL, Channel);
    oJSON.AddPair(CS_PROTOCOL, Protocol);
    oJSON.AddPair(CS_INCLUDE, Include);
    oJSON.AddPair(CS_EXCLUDE, Exclude);
    vText := oJSON.Text;
  Finally
    FreeJSONInstance(oJSON);
  End;

  result := Format('%.3d', [Length(vText)]) + vText + Text;
end;

procedure TsgcWSLoadBalancerBinary.Clear;
begin
  Guid := '';
  Channel := '';
  Protocol := '';
  Include := '';
  Exclude := '';
  Size := 0;
  Streaming := stmNone;
end;

procedure TsgcWSLoadBalancerBinary.Read(var aStream: TMemoryStream);
begin
  sgcWSStreamRead(aStream, FGuid);
end;

procedure TsgcWSLoadBalancerBinary.Write(var aData: TMemoryStream);
begin
  sgcWSStreamWrite(Guid, aData);
end;

procedure TsgcWSLoadBalancerClientConnection.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSLoadBalancerClientConnection then
  begin
    Active := TsgcWSLoadBalancerClientConnection(aSource).Active;
    Guid := TsgcWSLoadBalancerClientConnection(aSource).Guid;
    IP := TsgcWSLoadBalancerClientConnection(aSource).IP;
    Channel := TsgcWSLoadBalancerClientConnection(aSource).Channel;
    Protocol := TsgcWSLoadBalancerClientConnection(aSource).Protocol;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSLoadBalancerClientConnection.Clear;
begin
  Active := False;
  Guid := '';
  IP := '';
  Channel := '';
  Protocol := '';
end;

procedure TsgcWSLoadBalancerClientConnection.Read(const aMessage: String);
var
  oJSON: IsgcJSON;
begin
  oJSON := GetJSONInstance;
  Try
    oJSON.Read(aMessage);
    Active := oJSON.Node[CS_ACTIVE].Value;
    Guid := oJSON.Node[CS_GUID].Value;
    IP := oJSON.Node[CS_IP].Value;
    Channel := oJSON.Node[CS_CHANNEL].Value;
    Protocol := oJSON.Node[CS_PROTOCOL].Value;
  Finally
    FreeJSONInstance(oJSON);
  End;
end;

function TsgcWSLoadBalancerClientConnection.Write: string;
var
  oJSON: IsgcJSON;
begin
  oJSON := GetJSONInstance;
  Try
    oJSON.AddPair(CS_METHOD, CS_LB_CLIENT_CONNECTION);
    oJSON.AddPair(CS_ACTIVE, Active);
    oJSON.AddPair(CS_GUID, Guid);
    oJSON.AddPair(CS_IP, IP);
    oJSON.AddPair(CS_CHANNEL, Channel);
    oJSON.AddPair(CS_PROTOCOL, Protocol);
    result := oJSON.Text;
  Finally
    FreeJSONInstance(oJSON);
  End;
end;

procedure TsgcWSLoadBalancerServerBinding.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSLoadBalancerServerBinding then
  begin
    Port := TsgcWSLoadBalancerServerBinding(aSource).Port;
    Host := TsgcWSLoadBalancerServerBinding(aSource).Host;
    SSL := TsgcWSLoadBalancerServerBinding(aSource).SSL;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSLoadBalancerServerBinding.Clear;
begin
  Host := '';
  Port := 0;
  SSL := False;
end;

procedure TsgcWSLoadBalancerServerBinding.Read(const aMessage: String);
var
  oJSON: IsgcJSON;
begin
  oJSON := GetJSONInstance;
  Try
    oJSON.Read(aMessage);
    Host := oJSON.Node[CS_HOST].Value;
    Port := oJSON.Node[CS_PORT].Value;
    SSL := oJSON.Node[CS_SSL].Value;
  Finally
    FreeJSONInstance(oJSON);
  End;
end;

function TsgcWSLoadBalancerServerBinding.Write: string;
var
  oJSON: IsgcJSON;
begin
  oJSON := GetJSONInstance;
  Try
    oJSON.AddPair(CS_METHOD, CS_LB_SERVER_BINDING);
    oJSON.AddPair(CS_HOST, Host);
    oJSON.AddPair(CS_PORT, Port);
    oJSON.AddPair(CS_SSL, SSL);
    result := oJSON.Text;
  Finally
    FreeJSONInstance(oJSON);
  End;
end;

procedure TsgcWSLoadBalancerServerData.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSLoadBalancerServerData then
    Guid := TsgcWSLoadBalancerServerData(aSource).Guid
  else
    inherited Assign(aSource);
end;

procedure TsgcWSLoadBalancerServerData.Clear;
begin
  Guid := '';
end;

procedure TsgcWSLoadBalancerServerData.Read(const aMessage: String);
var
  oJSON: IsgcJSON;
begin
  oJSON := GetJSONInstance;
  Try
    oJSON.Read(aMessage);
    Guid := oJSON.Node[CS_GUID].Value;
  Finally
    FreeJSONInstance(oJSON);
  End;
end;

function TsgcWSLoadBalancerServerData.Write: string;
var
  oJSON: IsgcJSON;
begin
  oJSON := GetJSONInstance;
  Try
    oJSON.AddPair(CS_METHOD, CS_LB_SERVER_DATA);
    oJSON.AddPair(CS_GUID, Guid);
    result := oJSON.Text;
  Finally
    FreeJSONInstance(oJSON);
  End;
end;

end.
