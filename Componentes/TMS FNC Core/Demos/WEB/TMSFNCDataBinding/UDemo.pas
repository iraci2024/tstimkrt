unit UDemo;

interface

uses
  System.SysUtils, System.Classes, JS, Web, Types, WEBLib.Graphics, WEBLib.Controls,
  WEBLib.Forms, WEBLib.Dialogs, Vcl.Controls, Data.DB, WEBLib.CDS, WEBLib.DB,
  Vcl.Grids, WEBLib.Grids, WEBLib.ExtCtrls, Vcl.StdCtrls, WEBLib.StdCtrls,
  VCL.TMSFNCCustomComponent, VCL.TMSFNCDataBinding;

type
  THTMLPaintBox = class(TWebPaintBox)
  private
    FHTML: string;
    procedure SetHTML(const Value: string);
  protected
    procedure Paint; override;
  published
    property HTML: string read FHTML write SetHTML;
  end;

  TForm1 = class(TWebForm)
    WebPanel1: TWebPanel;
    WebButton1: TWebButton;
    WebButton2: TWebButton;
    WebListBox1: TWebListBox;
    WebStringGrid1: TWebStringGrid;
    WebDataSource1: TWebDataSource;
    WebClientConnection1: TWebClientConnection;
    WebClientDataSet1: TWebClientDataSet;
    TMSFNCDataBinder1: TTMSFNCDataBinder;
    procedure WebFormCreate(Sender: TObject);
    procedure WebButton1Click(Sender: TObject);
    procedure WebButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  WEBLib.TMSFNCGraphics, WEBLib.TMSFNCGraphicsTypes;

{ TForm1 }

procedure TForm1.WebButton1Click(Sender: TObject);
begin
  TMSFNCDataBinder1.Active := not TMSFNCDataBinder1.Active;
  if TMSFNCDataBinder1.Active then
    WebButton1.Caption := 'Deactivate'
  else
    WebButton1.Caption := 'Activate';
end;

procedure TForm1.WebButton2Click(Sender: TObject);
begin
  TMSFNCDataBinder1.StartEditor;
end;

procedure TForm1.WebFormCreate(Sender: TObject);
var
  p: THTMLPaintBox;
begin
  WebClientConnection1.URI := 'https://download.tmssoftware.com/tmsweb/fishfacti.json';
  WebClientConnection1.DataNode := 'ROW';
  WebClientDataSet1.FieldDefs.Clear;
  WebClientDataSet1.FieldDefs.Add('_Species_No',ftString,0);
  WebClientDataSet1.FieldDefs.Add('_Category',ftstring,50);
  WebClientDataSet1.FieldDefs.Add('_Common_Name',ftstring,50);
  WebClientDataSet1.FieldDefs.Add('_Species_Name',ftstring,50);
  WebClientDataSet1.FieldDefs.Add('_Length__cm_',ftInteger,0);
  WebClientDataSet1.FieldDefs.Add('_Length_In',ftString,30);
  WebClientDataSet1.FieldDefs.Add('_Notes',ftString,255);

  WebClientDataSet1.Connection := WebClientConnection1;
  WebDataSource1.DataSet := WebClientDataSet1;

  WebClientConnection1.Active := true;

  p := THTMLPaintBox.Create(Self);
  p.Parent := Self;
  p.Name := 'HTMLPaintBox1';

  p.Left := WebListBox1.Left + WebListBox1.Width + 20;
  p.Top := WebListBox1.Top;
  p.Width := 300;
  p.Height := WebListBox1.Height;

  TMSFNCDataBinder1.ConnectSingleHTMLTemplate(p, WebDataSource1, 'HTML', '<b>Name: <#_COMMON_NAME></b><br/><font size="10"><#_NOTES></font>');
  TMSFNCDataBinder1.ConnectList(WebListBox1, WebDataSource1, 'Items', '_Species_Name');
  TMSFNCDataBinder1.ConnectGrid(WebStringGrid1, WebDataSource1);

  TMSFNCDataBinder1.Active := True;
  WebButton1.Caption := 'Deactivate';
end;

{ THTMLPaintBox }

procedure THTMLPaintBox.Paint;
var
  g: TTMSFNCGraphics;
begin
  inherited;
  g := TTMSFNCGraphics.Create(Canvas);
  try
    g.DrawText(Bounds(0, 0, Width, Height), HTML, True, gtaLeading, gtaLeading);
  finally
    g.Free;
  end;
end;

procedure THTMLPaintBox.SetHTML(const Value: string);
begin
  FHTML := Value;
  Invalidate;
end;

end.
