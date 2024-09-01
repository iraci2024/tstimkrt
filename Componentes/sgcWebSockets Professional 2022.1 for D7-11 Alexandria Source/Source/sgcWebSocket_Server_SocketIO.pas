{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Server_SocketIO;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  // indy
  {$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
  // websocket
  sgcWebSocket_Server, sgcWebSocket_Types, sgcWebSocket_Classes;

type
  TsgcIO_Session = class(TComponent)
  private
    FIO_Session: String;
    function GetSession: String;
  public
    property Session: String read GetSession;
  end;

  TsgcWSServer_SocketIO = class(TsgcWSHTTPServer)
    { from TsgcWSServer_Base }
  protected
    procedure DoPing; override;
  protected
    procedure DoNotifyConnect(aConnection: TsgcWSConnection); override;
  protected
    function DoBuiltInLibraries(aConnection: TsgcWSConnectionServer;
      const aText: String; aDisconnect: Boolean): Boolean; override;
    { from TsgcWSServer_Base }

    { sessions }
  private
    FIO_SessionList: TsgcThreadList;
  protected
    function IO_Session_Create: String; virtual;
    function IO_Session_Delete(const aSession: String): Boolean; virtual;
  protected
    function GetIO_SessionList: TsgcThreadList;
  public
    property IO_SessionList
      : TsgcThreadList read GetIO_SessionList write FIO_SessionList;
    { sessions }

    { messages }
  protected
    function GetMessageHeartBeat: String; virtual;
    { messages }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    { constructor / destructor }

    { properties }
  private
    FIO_API: TwsSocketIOAPI;
  public
    property IO_API: TwsSocketIOAPI read FIO_API write FIO_API;
    { properties }
  end;

implementation

uses
  sgcWebSocket_Const, sgcWebSocket_Helpers;

constructor TsgcWSServer_SocketIO.Create(aOwner: TComponent);
begin
  inherited;
  IO_API := ioAPI0;
end;

function TsgcWSServer_SocketIO.DoBuiltInLibraries
  (aConnection: TsgcWSConnectionServer; const aText: String;
  aDisconnect: Boolean): Boolean;
begin
  Result := inherited DoBuiltInLibraries(aConnection, aText, aDisconnect);

  // session IO
  if not Result then
  begin
    case IO_API of
      ioAPI0:
        begin
          Result := sgcMatchesMask(aText, 'GET ' + CS_SOCKETIO_URL_API0 + '*');
          if Result then
            aConnection.SendResponseHTTP
              (IO_Session_Create + ':60:60:websocket', 'text/html');
        end;
      ioAPI1:
        begin
          Result := sgcMatchesMask(aText, 'GET ' + CS_SOCKETIO_URL_API1_2 + '*');
          if Result then
            aConnection.SendResponseHTTP('{"sid":"' + IO_Session_Create +
                '","upgrades":["websocket"],"pingInterval":25000,"pingTimeout":60000}'
                , 'text/html');
        end
      else
        Result := False;
    end;
  end;
end;

procedure TsgcWSServer_SocketIO.DoNotifyConnect(aConnection: TsgcWSConnection);
var
  i: Integer;
  vSID: string;
  oList: TsgcDelimitedStringList;
begin
  // ... initialization
  vSID := '';

  // ... search sid value
  case IO_API of
    ioAPI0:
      begin
        oList := TsgcDelimitedStringList.Create;
        Try
          oList.Delimiter := '/';
          oList.DelimitedText := TsgcWSConnectionServer(aConnection).URL;
          if oList.Count > 4 then
            vSID := oList[4];
        Finally
          sgcFree(oList);
        End;
      end;
    ioAPI1:
      begin
        oList := TsgcDelimitedStringList.Create;
        Try
          oList.Delimiter := '&';
          oList.DelimitedText := TsgcWSConnectionServer(aConnection).URL;
          for i := 0 to oList.Count - 1 do
          begin
            if LeftStr(UpperCase(oList[i]), 3) = 'SID' then
            begin
              vSID := MidStr(oList[i], 5, Length(oList[i]));
              break;
            end;
          end;
        Finally
          sgcFree(oList);
        End;
      end;
  end;

  // ... delete sid
  if not IO_Session_Delete(vSID) then
    raise TsgcWSException.Create(S_ERROR_SOCKETIO_SID);

  inherited;
end;

procedure TsgcWSServer_SocketIO.DoPing;
begin
  Broadcast(GetMessageHeartBeat);
end;

function TsgcWSServer_SocketIO.GetIO_SessionList: TsgcThreadList;
begin
  if not Assigned(FIO_SessionList) then
    FIO_SessionList := TsgcThreadList.Create;
  Result := FIO_SessionList;
end;

function TsgcWSServer_SocketIO.GetMessageHeartBeat: String;
begin
  case IO_API of
    ioAPI0:
      result := '2::';
    ioAPI1:
      result := '2';
  end;
end;

function TsgcWSServer_SocketIO.IO_Session_Create: String;
var
  oList: TList;
  oSession: TsgcIO_Session;
begin
  Result := '';

  oList := IO_SessionList.LockList;
  Try
    oSession := TsgcIO_Session.Create(nil);
    Result := oSession.Session;

    oList.Add(oSession);
  Finally
    IO_SessionList.UnlockList;
  End;
end;

function TsgcWSServer_SocketIO.IO_Session_Delete(const aSession: String)
  : Boolean;
var
  i: Integer;
  oList: TList;
  oObject: TObject;
begin
  Result := False;

  oList := IO_SessionList.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcIO_Session(oList[i]).Session = aSession then
      begin
        oObject := TObject(oList.Items[0]);
        sgcFree(oObject);
        oList.Delete(0);

        Result := True;

        break;
      end;
    end;
  Finally
    IO_SessionList.UnlockList;
  End;
end;

function TsgcIO_Session.GetSession: String;
begin
  if FIO_Session = '' then
    FIO_Session := LeftStr(NewGuid, 20);
  Result := FIO_Session;
end;

end.
