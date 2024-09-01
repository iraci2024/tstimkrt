unit UMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, WEBLib.WebSocketClient, Vcl.Controls, Vcl.Grids,
  WEBLib.Grids, WEBLib.ExtCtrls, Vcl.StdCtrls, WEBLib.StdCtrls, Types,
  jsdelphisystem;

type
  TServerMessage = record
    message: string;
    data: JSValue;
  end;

  TCellUpdateMessage = record
    row: Integer;
    col: Integer;
    value: string;
  end;

  TTableContentMessage = record
    rows: Integer;
    cols: Integer;
    content: TStringDynArray;
  end;

  TForm1 = class(TWebForm)
    grid: TWebStringGrid;
    WebSocketClient1: TWebSocketClient;
    pnl: TWebPanel;
    connBtn: TWebButton;
    lblTitle: TWebLabel;
    lblDescr: TWebLabel;
    procedure gridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure connBtnClick(Sender: TObject);
    procedure WebSocketClient1Connect(Sender: TObject);
    procedure WebSocketClient1Disconnect(Sender: TObject);
    procedure WebSocketClient1DataReceived(Sender: TObject; Origin: string;
      SocketData: TJSObjectRecord);
    procedure gridValidateEdit(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

type
  TWebSocketClientOpen = class(TWebSocketClient);

{$R *.dfm}

procedure TForm1.connBtnClick(Sender: TObject);
begin
  connBtn.Enabled := False;

  if connBtn.Caption = 'Connect' then
    WebSocketClient1.Connect
  else
    WebSocketClient1.Disconnect;
end;

procedure TForm1.WebSocketClient1Connect(Sender: TObject);
begin
  connBtn.Enabled := True;
  connBtn.Caption := 'Disconnect';
  grid.Enabled := True;
end;

procedure TForm1.WebSocketClient1DataReceived(Sender: TObject; Origin: string;
  SocketData: TJSObjectRecord);
var
  val: JSValue;
  sMsg: TServerMessage;
  cData: TCellUpdateMessage;
  I, J: Integer;
  tData: TTableContentMessage;
begin
  val := TJSJSON.parse(SocketData.jsobject.toString);
  sMsg := TServerMessage(val);

  if sMsg.message = 'update_cell' then
  begin
    cData := TCellUpdateMessage(sMsg.data);
    grid.Cells[cData.col, cData.row] := cData.value;
  end
  else if sMsg.message = 'table_content' then
  begin
    tData := TTableContentMessage(sMsg.data);

    grid.RowCount := tData.rows;
    grid.ColCount := tData.cols;

    grid.BeginUpdate;
    for I := 0 to tData.rows - 1 do
    begin
      for J := 0 to tData.cols - 1 do
        grid.Cells[J, I] := tData.content[(I * 30) + J];
    end;
    grid.EndUpdate;
  end;
end;

procedure TForm1.WebSocketClient1Disconnect(Sender: TObject);
begin
  grid.Enabled := False;
  connBtn.Caption := 'Connect';
  connBtn.Enabled := True;
  ShowMessage('Disconnected from the server');
end;

procedure TForm1.gridSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
var
  msg: TServerMessage;
  data: TCellUpdateMessage;
begin
  msg.message := 'update_cell';
  data.row := ARow;
  data.col := ACol;
  data.value := Value;
  msg.data := data;

  WebSocketClient1.Send(TJSJSON.stringify(msg));
end;

procedure TForm1.gridValidateEdit(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
  if Length(Value) > 15 then
    SetLength(Value, 15);
end;

end.