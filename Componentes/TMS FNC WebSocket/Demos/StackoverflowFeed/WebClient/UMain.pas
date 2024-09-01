unit UMain;

interface

uses
  System.SysUtils, System.Classes, JS, Web, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.StdCtrls, WEBLib.StdCtrls, Vcl.Controls,
  WEBLib.WebSocketClient, WEBLib.ContinuousScroll, WEBLib.WebCtrls,
  WEBLib.ExtCtrls, WEBLib.Chatbox;

type
  TStackExchangeData = record
    siteBaseHostAddress: string;
    id: Integer;
    titleEncodedFancy: string;
    bodySummary: string;
    url: string;
    tags: TJSArray;
    ownerUrl: string;
    ownerDisplayName: string;
    apiSiteParameter: string;
    lastActivityDate: Integer;
  end;

  TStackExchangeAction = record
    action: string;
    data: string;
  end;

  TForm1 = class(TWebForm)
    WebSocketClient1: TWebSocketClient;
    WebButton1: TWebButton;
    WebScrollBox1: TWebScrollBox;
    WebListBox1: TWebListBox;
    WebLabel1: TWebLabel;
    WebLabel2: TWebLabel;
    WebTimer1: TWebTimer;
    procedure WebButton1Click(Sender: TObject);
    procedure WebSocketClient1DataReceived(Sender: TObject; Origin: string;
      SocketData: TJSObjectRecord);
    procedure WebSocketClient1Connect(Sender: TObject);
    procedure WebSocketClient1Disconnect(Sender: TObject);
    procedure WebTimer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddNewStackoverflowItem(const ATitle, ADescription, AUser, ASite, AURL, AUserURL: string);
  end;

var
  Form1: TForm1;

implementation

uses
  jsdelphisystem;

{$R *.dfm}

procedure TForm1.AddNewStackoverflowItem(const ATitle, ADescription, AUser,
  ASite, AURL, AUserURL: string);
var
  pnl: TWebPanel;
  usr, desc, title: TWebLabel;
begin
  WebListBox1.Items.Add(ASite);

  if ASite = 'stackoverflow.com' then
  begin
    pnl := TWebPanel.Create(WebScrollBox1);
    pnl.Parent := WebScrollBox1;
    pnl.Align := alTop;
    pnl.Height := 100;
    pnl.Color := clWhite;

    title := TWebLabel.Create(pnl);
    title.Height := 22;
    title.Align := alTop;
    title.Font.Size := 12;
    title.Parent := pnl;
    title.HTML := '<a target="_blank" href="' + AURL + '">' + ATitle + '</a>';
    title.EllipsisPosition := epWordEllipsis;

    usr := TWebLabel.Create(pnl);
    usr.Parent := pnl;
    usr.HTML := 'post by - <a target="_blank" href="' + AUserURL + '">' + AUser + '</a>';
    usr.Align := alBottom;
    usr.Alignment := taRightJustify;
    usr.Font.Size := 10;

    desc := TWebLabel.Create(pnl);
    desc.Parent := pnl;
    desc.HTML := ADescription;
    desc.Align := alClient;
    desc.WordWrap := True;
    desc.Font.Size := 10;

    WebScrollBox1.ScrollBy(0, 100);
  end;
  WebListBox1.ItemIndex := WebListBox1.Count - 1;
end;

procedure TForm1.WebButton1Click(Sender: TObject);
begin
  WebButton1.Enabled := False;
  if WebButton1.Caption = 'Connect' then
  begin
    WebSocketClient1.Connect;
    WebButton1.Caption := 'Disconnect';
  end
  else
  begin
    WebSocketClient1.Disconnect;
    WebButton1.Caption := 'Connect';
  end;
end;

procedure TForm1.WebSocketClient1Connect(Sender: TObject);
begin
  WebButton1.Enabled := True;
  WebTimer1.Enabled := True;
  WebSocketClient1.Send('155-questions-active');
end;

procedure TForm1.WebSocketClient1DataReceived(Sender: TObject; Origin: string;
  SocketData: TJSObjectRecord);
var
  msg: string;
  val: JSValue;
  act: TStackExchangeAction;
  data: TStackExchangeData;
begin
  msg := SocketData.jsobject.toString;
  if msg <> 'pong' then
  begin
    val := TJSJSON.parse(SocketData.jsobject.toString);
    act := TStackExchangeAction(val);
    //{action: "hb", data: "pong"} can arrive as message, we don't process that
    if act.action = '155-questions-active' then
    begin
      data := TStackExchangeData(TJSJSON.parse(act.data));
      AddNewStackoverflowItem(data.titleEncodedFancy, data.bodySummary, data.ownerDisplayName, data.siteBaseHostAddress, data.url, data.ownerUrl);
    end;
  end;
end;

procedure TForm1.WebSocketClient1Disconnect(Sender: TObject);
begin
  WebButton1.Enabled := True;
  WebTimer1.Enabled := False;
  WebButton1.Caption := 'Connect';
end;

procedure TForm1.WebTimer1Timer(Sender: TObject);
begin
  //Server will respond with "pong" message.
  //Do this to keep alive connection.
  WebSocketClient1.Send('ping');
end;

end.
