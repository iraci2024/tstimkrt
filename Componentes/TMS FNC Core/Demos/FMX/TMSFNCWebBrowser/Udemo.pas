unit Udemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics, FMX.TMSFNCGraphicsTypes,
  FMX.TMSFNCCustomControl, FMX.TMSFNCWebBrowser, FMX.Controls.Presentation,
  FMX.Edit, FMX.Objects, FMX.TMSFNCCustomComponent, FMX.TMSFNCBitmapContainer,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ListBox;

type
  TMyBridgeObject = class(TInterfacedPersistent, ITMSFNCCustomWebBrowserBridge)
  private
    function GetObjectMessage: string;
    procedure SetObjectMessage(const Value: string);
  published
    property ObjectMessage: string read GetObjectMessage write SetObjectMessage;
  end;

  TForm3 = class(TForm)
    Edit1: TEdit;
    TMSFNCWebBrowser1: TTMSFNCWebBrowser;
    iGo: TImage;
    TMSFNCBitmapContainer1: TTMSFNCBitmapContainer;
    iBack: TImage;
    iForward: TImage;
    iReload: TImage;
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    Edit2: TEdit;
    Button4: TButton;
    procedure TMSFNCWebBrowser1NavigateComplete(Sender: TObject;
      var Params: TTMSFNCCustomWebBrowserNavigateCompleteParams);
    procedure FormCreate(Sender: TObject);
    procedure iGoClick(Sender: TObject);
    procedure iReloadClick(Sender: TObject);
    procedure iBackClick(Sender: TObject);
    procedure iForwardClick(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    o: TMyBridgeObject;
    yCnt: integer;
    nCnt: integer;
    procedure NavigateFNCWebBrowser(AURL: string);
    procedure SetVisibleComponents(AValue: Boolean);
    procedure UpdateLabels(AYes: Boolean);
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.iBackClick(Sender: TObject);
begin
  TMSFNCWebBrowser1.GoBack;
end;

procedure TForm3.iForwardClick(Sender: TObject);
begin
  TMSFNCWebBrowser1.GoForward;
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
    TMSFNCWebBrowser1.GetBridgeCommunicationLayer(BridgeName) + ' </script>' + #13 + ' </head>'
    + #13 + ' <body>' + #13 + ' <button onclick="send' + BridgeName +
    'ObjectMessage(''Yes'');">Yes!</button>' + #13 + '<button onclick="send' + BridgeName +
    'ObjectMessage(''No'');">No!</button><br><br>' + #13 + '<p id="ratioP"></p><br><br>'
    + #13 + '<div id="listD" style="width: 200px" >' + #13 + '</div>' + #13 + ' </body>' +
    #13 + '</html>';
  o := TMyBridgeObject.Create;
  TMSFNCWebBrowser1.AddBridge(BridgeName, o);
  TMSFNCWebBrowser1.LoadHTML(sHTML);
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

  TMSFNCWebBrowser1.ExecuteJavascript('document.getElementById("ratioP").innerHTML = "' + s + '";');
end;

procedure TForm3.Button4Click(Sender: TObject);
begin
  TMSFNCWebBrowser1.ExecuteJavascript('document.getElementById("listD").innerHTML = document.getElementById("listD").innerHTML + "<li>' + Edit2.Text + '</li>";');
end;

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

procedure TForm3.Edit1KeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if Key = vkReturn then
    NavigateFNCWebBrowser(Edit1.Text);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  yCnt := 0;
  nCnt := 0;

  iGo.Bitmap.Assign(TMSFNCBitmapContainer1.Bitmaps[0]);
  iBack.Bitmap.Assign(TMSFNCBitmapContainer1.Bitmaps[3]);
  iForward.Bitmap.Assign(TMSFNCBitmapContainer1.Bitmaps[2]);
  iReload.Bitmap.Assign(TMSFNCBitmapContainer1.Bitmaps[4]);

  TMSFNCWebBrowser1.Navigate;
end;

procedure TForm3.iGoClick(Sender: TObject);
begin
  NavigateFNCWebBrowser(Edit1.Text);
end;

procedure TForm3.iReloadClick(Sender: TObject);
begin
  iReload.Enabled := False;
  iGo.Enabled := False;
  TMSFNCWebBrowser1.Reload;
end;

procedure TForm3.ListBox1ItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  NavigateFNCWebBrowser(Item.Text);
end;

procedure TForm3.NavigateFNCWebBrowser(AURL: string);
begin
  iGo.Bitmap.Assign(TMSFNCBitmapContainer1.Bitmaps[0]);
  iGo.Enabled := False;
  if (Pos('http://', LowerCase(AURL)) > 0) or (Pos('https://', LowerCase(AURL)) > 0) then
    TMSFNCWebBrowser1.Navigate(AURL)
  else
    TMSFNCWebBrowser1.Navigate('https://'+AURL);
end;

procedure TForm3.SetVisibleComponents(AValue: Boolean);
begin
  Button1.Enabled := not AValue;
  ListBox1.Visible := AValue;
  Button2.Enabled := AValue;
  Panel1.Visible := not AValue;
end;

procedure TForm3.TMSFNCWebBrowser1NavigateComplete(Sender: TObject;
  var Params: TTMSFNCCustomWebBrowserNavigateCompleteParams);
begin
  Edit1.Text := Params.URL;

  ListBox1.Items.Add(Params.URL);

  iGo.Bitmap.Assign(TMSFNCBitmapContainer1.Bitmaps[1]);
  iGo.Enabled := True;
  iReload.Enabled := True;
  iBack.Enabled := TMSFNCWebBrowser1.CanGoBack;
  iForward.Enabled := TMSFNCWebBrowser1.CanGoForward;
end;

procedure TForm3.UpdateLabels(AYes: Boolean);
begin
  if AYes then
    Inc(yCnt)
  else
    Inc(nCnt);

  Label1.Text := 'Yes clicked: ' + IntToStr(ycnt);
  Label2.Text := 'No clicked: ' + IntToStr(ncnt);
end;

end.
