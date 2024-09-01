unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.TMSFNCWebSocketServer, FMX.TMSFNCWebSocketCommon,
  FMX.TMSFNCCustomComponent;

type
  TForm2 = class(TForm)
    startBtn: TButton;
    logMem: TMemo;
    TMSFNCWebSocketServer1: TTMSFNCWebSocketServer;
    procedure startBtnClick(Sender: TObject);
    procedure TMSFNCWebSocketServer1Connect(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection);
    procedure TMSFNCWebSocketServer1Disconnect(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection);
    procedure TMSFNCWebSocketServer1MessageReceived(Sender: TObject;
      AConnection: TTMSFNCWebSocketConnection; const aMessage: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.startBtnClick(Sender: TObject);
begin
  if startBtn.Text = 'Start' then
  begin
    TMSFNCWebSocketServer1.Active := True;
    startBtn.Text := 'Stop';
  end
  else
  begin
    TMSFNCWebSocketServer1.Active := False;
    startBtn.Text := 'Start';
  end;
end;

procedure TForm2.TMSFNCWebSocketServer1Connect(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection);
begin
  logMem.Lines.Add('A client connected');
end;

procedure TForm2.TMSFNCWebSocketServer1Disconnect(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection);
begin
  logMem.Lines.Add('A client disconnected');
end;

procedure TForm2.TMSFNCWebSocketServer1MessageReceived(Sender: TObject;
  AConnection: TTMSFNCWebSocketConnection; const aMessage: string);
begin
  TMSFNCWebSocketServer1.SendMessageTo(AMessage, function(AClientConnection: TTMSFNCWebSocketServerConnection): Boolean
    begin
      Result := AClientConnection <> AConnection;
    end);
end;

end.
