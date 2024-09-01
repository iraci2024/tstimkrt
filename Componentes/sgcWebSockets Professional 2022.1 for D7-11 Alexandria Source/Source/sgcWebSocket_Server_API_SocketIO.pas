{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Server_API_SocketIO;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Classes_SyncObjs;

type
  TsgcWSOnServerSocketIOJSONMessage = procedure(Sender: TObject; const
      aConnection: TsgcWSConnection; const aHeader: string; const aJSON: string;
      const aRawMessage: string) of object;

  TsgcWSSocketIO_Options = class(TPersistent)
  private
    FPingInterval: Integer;
    FPingTimeout: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property PingInterval: Integer read FPingInterval write FPingInterval;
    property PingTimeout: Integer read FPingTimeout write FPingTimeout;
  end;

  TsgcWSServer_API_SocketIO = class(TsgcWSAPI_server)
    { from TsgcWSComponent }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent }

    { from TsgcWSAPI_server }
  protected
    function DoHTTPRequestApi(const aRequest: TsgcHTTPRequest; const aResponse:
        TsgcHTTPResponse): Boolean; override;
    { from TsgcWSAPI_server }

    { sessions }
  private
    FSessions: TsgcThreadSafeStringList;
    function GetSessions: TsgcThreadSafeStringList;
  protected
    property Sessions: TsgcThreadSafeStringList read GetSessions
      write FSessions;
    { sessions }

    { properties }
  private
    FSocketIO: TsgcWSSocketIO_Options;
    procedure SetSocketIO(const Value: TsgcWSSocketIO_Options);
  public
    property SocketIO: TsgcWSSocketIO_Options read FSocketIO write SetSocketIO;
    { properties }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    { read }
  protected
    procedure DoReadJSONMessage(const aConnection: TsgcWSConnection; const aText:
        string); virtual;
    { read }

    { events }
  private
    FOnSocketIOJSONMessage: TsgcWSOnServerSocketIOJSONMessage;
  protected
    procedure DoSocketIOJSONMessageEvent(const aConnection: TsgcWSConnection; const
        aHeader, aJSON, aRawMessage: string); virtual;
  protected
    property OnSocketIOJSONMessage: TsgcWSOnServerSocketIOJSONMessage read
        FOnSocketIOJSONMessage write FOnSocketIOJSONMessage;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  sgcWebSocket_Const, sgcBase_Helpers, sgcWebSocket_Helpers;

constructor TsgcWSServer_API_SocketIO.Create(aOwner: TComponent);
begin
  inherited;
  FSocketIO := TsgcWSSocketIO_Options.Create;
end;

destructor TsgcWSServer_API_SocketIO.Destroy;
begin
  sgcFree(FSessions);
  sgcFree(FSocketIO);
  inherited;
end;

procedure TsgcWSServer_API_SocketIO.DoEventConnect
  (aConnection: TsgcWSConnection);
begin
  inherited;
end;

procedure TsgcWSServer_API_SocketIO.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
begin
  inherited;
end;

procedure TsgcWSServer_API_SocketIO.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
begin
  if Text = '2probe' then
    aConnection.WriteData('3probe')
  else if Text = '2' then
    aConnection.WriteData('3')
  else if LeftStr(Text, 2) = '42' then
    DoReadJSONMessage(aConnection, Text);
end;

function TsgcWSServer_API_SocketIO.DoHTTPRequestApi(const aRequest:
    TsgcHTTPRequest; const aResponse: TsgcHTTPResponse): Boolean;
var
  i: Integer;
  oList: TStringList;
  vSID, vMessage: string;
begin
  result := aRequest.Document = '/socket.io/';
  if result then
  begin
    vSID := '';

    oList := TStringList.Create;
    Try
      oList.Delimiter := '&';
      oList.DelimitedText := aRequest.QueryParams;
      for i := 0 to oList.Count - 1 do
      begin
        if LeftStr(oList[i], 4) = 'sid=' then
        begin
          vSID := MidStr(oList[i], 5, Length(oList[i]));
          if Sessions.IndexOf(vSID) = -1 then
            raise Exception.Create('SID not found: ' + vSID);
          break;
        end;
      end;
    Finally
      sgcFree(oList);
    End;

    aResponse.Code := 200;
    aResponse.ContentType := 'text/plain; charset=UTF-8';
    if vSID = '' then
    begin
      vSID := LeftStr(NewGuid, 20);
      vMessage :=
        Format('{"sid":"%s","upgrades":["websocket"],"pingInterval":%d,"pingTimeout":%d}',
        [vSID, SocketIO.PingInterval, SocketIO.PingTimeout]);
      aResponse.Content := IntToStr(Length(vMessage) + 1) + ':0' +
        vMessage + '2:40';
      Sessions.Add(vSID);
    end
    else
      aResponse.Content := '1:6';
  end;
end;

procedure TsgcWSServer_API_SocketIO.DoReadJSONMessage(const aConnection:
    TsgcWSConnection; const aText: string);
var
  i: Integer;
  vHeader: string;
  vJSON: string;
begin
  i := Pos('[', aText);
  if i > 0 then
  begin
    vHeader := LeftStr(aText, i - 1);
    vJSON := MidStr(aText, i, Length(aText));

    DoSocketIOJSONMessageEvent(aConnection, vHeader, vJSON, aText);
  end;
end;

procedure TsgcWSServer_API_SocketIO.DoSocketIOJSONMessageEvent(const
    aConnection: TsgcWSConnection; const aHeader, aJSON, aRawMessage: string);
begin
  if Assigned(FOnSocketIOJSONMessage) then
    FOnSocketIOJSONMessage(self, aConnection, aHeader, aJSON, aRawMessage);
end;

function TsgcWSServer_API_SocketIO.GetSessions: TsgcThreadSafeStringList;
begin
  if not Assigned(FSessions) then
    FSessions := TsgcThreadSafeStringList.Create;
  result := FSessions;
end;

procedure TsgcWSServer_API_SocketIO.SetSocketIO(const Value
  : TsgcWSSocketIO_Options);
begin
  if Assigned(FSocketIO) then
    FSocketIO.Assign(Value);
end;

constructor TsgcWSSocketIO_Options.Create;
begin
  inherited;
  PingInterval := 25000;
  PingTimeout := 5000;
end;

destructor TsgcWSSocketIO_Options.Destroy;
begin
  inherited;
end;

procedure TsgcWSSocketIO_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSocketIO_Options then
  begin
    PingInterval := TsgcWSSocketIO_Options(aSource).PingInterval;
    PingTimeout := TsgcWSSocketIO_Options(aSource).PingTimeout;
  end
  else
    inherited Assign(aSource);
end;

initialization

Classes.RegisterClass(TsgcWSServer_API_SocketIO);
{$ENDIF}

end.
