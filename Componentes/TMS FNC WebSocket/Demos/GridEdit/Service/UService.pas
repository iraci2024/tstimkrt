unit UService;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Types,
  VCL.TMSFNCWebSocketServer, VCL.TMSFNCWebSocketCommon;

type
  TGridService = class(TService)
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
  private
    FGridValues: TStringDynArray;
    FServer: TTMSFNCWebSocketServer;
    procedure HandshakeResponseSent(Sender: TObject; AConnection: TTMSFNCWebSocketServerConnection);
    procedure MessageReceived(Sender: TObject; AConnection: TTMSFNCWebSocketConnection; const aMessage: string);
    procedure GetSSLPassword(Sender: TObject; var APassword: string);
  public
    function GetServiceController: TServiceController; override;
  end;

var
  GridService: TGridService;

implementation

uses
  JSON;

const
  GRID_ROWS = 50;
  GRID_COLUMNS = 30;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  GridService.Controller(CtrlCode);
end;

function TGridService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TGridService.GetSSLPassword(Sender: TObject; var APassword: string);
begin
  APassword := '';
end;

procedure TGridService.HandshakeResponseSent(Sender: TObject;
  AConnection: TTMSFNCWebSocketServerConnection);
var
  jobj, jdata: TJSONObject;
  jarr: TJSONArray;
  I: Integer;
begin
  jobj := TJSONObject.Create;
  try
    jdata := TJSONObject.Create;

    jarr := TJSONArray.Create;
    for I := 0 to (GRID_ROWS * GRID_COLUMNS) - 1 do
      jarr.Add(FGridValues[I]);

    jdata.AddPair('rows', TJSONNumber.Create(GRID_ROWS));
    jdata.AddPair('cols', TJSONNumber.Create(GRID_COLUMNS));
    jdata.AddPair('content', jarr);

    jobj.AddPair('message', 'table_content');
    jobj.AddPair('data', jdata);

    AConnection.Send(jobj.ToJSON);
  finally
    jobj.Free;
  end;
end;

procedure TGridService.MessageReceived(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection; const aMessage: string);
var
  obj, data: TJSONObject;
  msg: string;
  row, col: Integer;
begin
  obj := TJSONObject.ParseJSONValue(AMessage) as TJSONObject;
  try
    obj.TryGetValue<string>('message', msg);
    if msg = 'update_cell' then
    begin
      obj.TryGetValue<TJSONObject>('data', data);

      if Assigned(data) then
      begin
        data.TryGetValue<string>('value', msg);
        data.TryGetValue<Integer>('row', row);
        data.TryGetValue<Integer>('col', col);

        if Length(msg) > 15 then
          SetLength(msg, 15);
        FGridValues[(row * GRID_COLUMNS) + col] := msg;
      end;

      FServer.SendMessageTo(AMessage, function(AClientConnection: TTMSFNCWebSocketServerConnection): Boolean
      begin
        Result := AConnection <> AClientConnection;
      end);
    end;
  finally
    obj.Free
  end;
end;

procedure TGridService.ServiceStart(Sender: TService; var Started: Boolean);
var
  I, c: Integer;
begin
  if Assigned(FServer) then
    FreeAndNil(FServer);

  //Make sure it's always empty when server is started
  c := GRID_ROWS * GRID_COLUMNS;
  SetLength(FGridValues, c);
  for I := 0 to c - 1 do
    FGridValues[I] := '';

  FServer := TTMSFNCWebSocketServer.Create;
  FServer.Port := 8960;
  FServer.CertificateFile := '';
  FServer.CertificateKeyfile := '';
  FServer.RootCertificateFile := '';
  FServer.UseSSL := True;
  FServer.OnMessageReceived := MessageReceived;
  FServer.OnHandshakeResponseSent := HandshakeResponseSent;
  FServer.OnGetSSLPassword := GetSSLPassword;
  FServer.Active := True;
  Started := True;
end;

procedure TGridService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  if Assigned(FServer) then
    FreeAndNil(FServer);
end;

end.
