unit UMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls, Vcl.Controls,
  WEBLib.WebSocketClient, WEBLib.Chatbox, WEBLib.Buttons, WEBLib.ExtCtrls,
  WEBLib.WebCtrls, Vcl.Imaging.pngimage;

type
  TSocketMessage = record
    user: string;
    message: string;
    image: string;
    pdf: string;
    filename: string;
  end;

  TSocketDataType = (sdtImage, sdtPDF);

  TForm1 = class(TWebForm)
    WebSocketClient1: TWebSocketClient;
    WebButton1: TWebButton;
    WebChatbox1: TWebChatbox;
    WebEdit1: TWebEdit;
    WebLabel1: TWebLabel;
    procedure WebButton1Click(Sender: TObject);
    procedure WebChatbox1SendMessage(Sender: TObject; Value: TMessage);
    procedure WebFormCreate(Sender: TObject);
    procedure WebSocketClient1Connect(Sender: TObject);
    procedure WebSocketClient1Disconnect(Sender: TObject);
    procedure WebSocketClient1DataReceived(Sender: TObject; Origin: string;
      SocketData: TJSObjectRecord);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  WEBLib.JSON, jsdelphisystem;

{$R *.dfm}

procedure TForm1.WebButton1Click(Sender: TObject);
begin
  if WebEdit1.Text = '' then
    ShowMessage('Username required')
  else
  begin
    WebChatbox1.Username := WebEdit1.Text;
    WebEdit1.Enabled := False;
    WebButton1.Enabled := False;
    if WebButton1.Caption = 'Connect' then
      WebSocketClient1.Connect
    else
      WebSocketClient1.Disconnect;
  end;
end;

procedure TForm1.WebChatbox1SendMessage(Sender: TObject; Value: TMessage);
var
  msg: TSocketMessage;
begin
  Value.ChatBubble.style.setProperty('width', '1000px');
  msg.user := Value.Sender;
  msg.message := Value.ChatMessage;
  msg.filename := Value.FileName;
  if Value.FileMimeType = 'application/pdf' then
    msg.pdf := Value.FileAsBase64
  else
    msg.image := Value.FileAsBase64;

  WebSocketClient1.Send(TJSJSON.stringify(msg));
end;

procedure TForm1.WebFormCreate(Sender: TObject);
begin
  TJSHTMLTextAreaElement(WebChatbox1.ElementHandle.getElementsByTagName('textarea').item(0)).disabled := True;
  WebSocketClient1.HostName := 'localhost';
  WebSocketClient1.Port := 5050;
end;

procedure TForm1.WebSocketClient1Connect(Sender: TObject);
begin
  WebButton1.Caption := 'Disconnect';
  WebButton1.Enabled := True;
  TJSHTMLTextAreaElement(WebChatbox1.ElementHandle.getElementsByTagName('textarea').item(0)).disabled := False;
end;

procedure TForm1.WebSocketClient1DataReceived(Sender: TObject; Origin: string;
  SocketData: TJSObjectRecord);
var
  m: TMessage;
  val: JSValue;
  msg: TSocketMessage;
begin
  val := TJSJSON.parse(SocketData.jsobject.toString);
  msg := TSocketMessage(val);
  m := WebChatbox1.Messages.Add;
  m.Outline := olLeft;
  m.ChatMessage := msg.message;
  m.Sender := msg.user;
  m.FileName := msg.filename;
  if msg.image <> '' then
    m.FileAsBase64 := msg.image
  else if msg.pdf <> '' then
    m.FileAsBase64 := msg.pdf;
  m.Timestamp := Now;
  m.ChatBubble.style.setProperty('width', '1000px');
end;

procedure TForm1.WebSocketClient1Disconnect(Sender: TObject);
begin
  if WebButton1.Enabled then
    ShowMessage('Connection lost with server unexpectedly.');

  WebButton1.Enabled := True;
  WebButton1.Caption := 'Connect';
  TJSHTMLTextAreaElement(WebChatbox1.ElementHandle.getElementsByTagName('textarea').item(0)).disabled := True;
end;

end.