unit UAdvWebBrowserDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AdvCustomControl,
  AdvWebBrowser, Vcl.ExtCtrls, Vcl.Imaging.pngimage, JSON;

type
  TMyBridgeObject = class(TInterfacedPersistent, IAdvCustomWebBrowserBridge)
  private
    function GetObjectMessage: string;
    procedure SetObjectMessage(const Value: string);
  published
    property ObjectMessage: string read GetObjectMessage write SetObjectMessage;
  end;

  TForm3 = class(TForm)
    AdvWebBrowser1: TAdvWebBrowser;
    iBack: TImage;
    iForward: TImage;
    iReload: TImage;
    iGo: TImage;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    Button4: TButton;
    Edit2: TEdit;
    Button5: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure iBackClick(Sender: TObject);
    procedure iForwardClick(Sender: TObject);
    procedure iReloadClick(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure iGoClick(Sender: TObject);
    procedure AdvWebBrowser1NavigateComplete(Sender: TObject;
      var Params: TAdvCustomWebBrowserNavigateCompleteParams);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    o: TMyBridgeObject;
    yCnt: integer;
    nCnt: integer;
    procedure NavigateWebBrowser(AURL: string);
    procedure SetVisibleComponents(AValue: Boolean);
    procedure UpdateLabels(AYes: Boolean);
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.AdvWebBrowser1NavigateComplete(Sender: TObject;
  var Params: TAdvCustomWebBrowserNavigateCompleteParams);
begin
  Edit1.Text := Params.URL;

  ListBox1.Items.Add(Params.URL);

  iGo.Enabled := True;
  iBack.Enabled := AdvWebBrowser1.CanGoBack;
  iForward.Enabled := AdvWebBrowser1.CanGoForward;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  SetVisibleComponents(True);
end;

procedure TForm3.Button2Click(Sender: TObject);
const
  BridgeName = 'MyBridge';
var
  sHTML: string;
begin
  SetVisibleComponents(False);

  sHTML := '<html>' + #13 + ' <head>' + #13 +
  '<h1>Do you like tms software?</h1>' +  #13 + ' <script>' + #13 +
    AdvWebBrowser1.GetBridgeCommunicationLayer(BridgeName) + ' </script>' + #13 + ' </head>'
    + #13 + ' <body>' + #13 + ' <button onclick="send' + BridgeName +
    'ObjectMessage(''Yes'');">Yes!</button>' + #13 + '<button onclick="send' + BridgeName +
    'ObjectMessage(''No'');">No!</button><br><br>' + #13 + '<p id="ratioP"></p><br><br>'
    + #13 + '<div id="listD" style="width: 200px" >' + #13 + '</div>' + #13 + ' </body>' +
    #13 + '</html>';
  o := TMyBridgeObject.Create;
  AdvWebBrowser1.AddBridge(BridgeName, o);
  AdvWebBrowser1.LoadHTML(sHTML);
end;

procedure TForm3.Button3Click(Sender: TObject);
var
  r: Single;
  s: string;
begin
  if (yCnt + nCnt = 0) then
    s := 'No votes were submitted.'
  else
  begin
    r := yCnt / (yCnt + nCnt) * 100;
    s := 'Yes was clicked ' + Format('%.2f', [r]) +'% of the time!';
  end;

  AdvWebBrowser1.ExecuteJavascript('document.getElementById("ratioP").innerHTML = "' + s + '";');
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  AdvWebBrowser1.ExecuteJavascript('document.getElementById("listD").innerHTML = document.getElementById("listD").innerHTML + "<li>' + Edit2.Text + '</li>";');
end;

procedure TForm3.Button5Click(Sender: TObject);
begin
  AdvWebBrowser1.ExecuteJavascript('function GetHTML(){return document.documentElement.innerHTML;} GetHTML();',
   procedure(const AValue: string)
   begin
     memo1.Lines.Text:= TJSONObject.ParseJSONValue(AValue).Value;;
   end
   );
end;

procedure TForm3.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    NavigateWebBrowser(Edit1.Text);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  yCnt := 0;
  nCnt := 0;

  AdvWebBrowser1.Navigate;
end;

procedure TForm3.iBackClick(Sender: TObject);
begin
  AdvWebBrowser1.GoBack;
end;

procedure TForm3.iForwardClick(Sender: TObject);
begin
  AdvWebBrowser1.GoForward;
end;

procedure TForm3.iGoClick(Sender: TObject);
begin
  NavigateWebBrowser(Edit1.Text);
end;

procedure TForm3.iReloadClick(Sender: TObject);
begin
  AdvWebBrowser1.Reload;
end;

procedure TForm3.NavigateWebBrowser(AURL: string);
begin
  iGo.Enabled := False;
  if (Pos('http://', LowerCase(AURL)) > 0) or (Pos('https://', LowerCase(AURL)) > 0) then
    AdvWebBrowser1.Navigate(AURL)
  else
    AdvWebBrowser1.Navigate('https://'+AURL);
end;

procedure TForm3.SetVisibleComponents(AValue: Boolean);
begin
  Button1.Enabled := not AValue;
  ListBox1.Visible := AValue;
  Button2.Enabled := AValue;
  Panel1.Visible := not AValue;
end;

procedure TForm3.UpdateLabels(AYes: Boolean);
begin
  if AYes then
    Inc(yCnt)
  else
    Inc(nCnt);

  Label1.Caption := 'Yes clicked: ' + IntToStr(ycnt);
  Label2.Caption := 'No clicked: ' + IntToStr(ncnt);
end;

{ TMyBridgeObject }

function TMyBridgeObject.GetObjectMessage: string;
begin

end;

procedure TMyBridgeObject.SetObjectMessage(const Value: string);
begin
  if Value = 'Yes' then
    Form3.UpdateLabels(True)
  else if Value = 'No' then
    Form3.UpdateLabels(False);
end;

end.
