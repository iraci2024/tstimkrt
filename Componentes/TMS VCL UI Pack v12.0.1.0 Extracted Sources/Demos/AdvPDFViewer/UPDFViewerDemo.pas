unit UPDFViewerDemo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AdvPDFViewer, AdvCustomControl,
  AdvCustomScrollControl, AdvGraphics, AdvTypes, Types, Vcl.ExtCtrls,
  Vcl.BaseImageCollection, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList;

type
  TForm22 = class(TForm)
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Button2: TButton;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    RadioGroup1: TRadioGroup;
    AdvPDFViewer1: TAdvPDFViewer;
    VirtualImageList1: TVirtualImageList;
    AdvSVGImageCollection1: TAdvSVGImageCollection;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure AdvPDFViewer1BeforeDrawHeader(Sender: TObject;
      APageIndex: Integer; AHeader: string; ARect: TRectF;
      AGraphics: TAdvGraphics; var ADefaultDraw: Boolean);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure AdvPDFViewer1Loaded(Sender: TObject);
    procedure AdvPDFViewer1PageChanged(Sender: TObject; APageIndex: Integer);
    procedure RadioGroup1Click(Sender: TObject);
    procedure AdvPDFViewer1BeforeDrawFooter(Sender: TObject;
      APageIndex: Integer; AFooter: string; ARect: TRectF;
      AGraphics: TAdvGraphics; var ADefaultDraw: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form22: TForm22;

implementation

{$R *.dfm}

uses
  AdvGraphicsTypes;

procedure TForm22.AdvPDFViewer1BeforeDrawFooter(Sender: TObject;
  APageIndex: Integer; AFooter: string; ARect: TRectF; AGraphics: TAdvGraphics;
  var ADefaultDraw: Boolean);
var
  b: TAdvBitmap;
  r: TRectF;
begin
  b := TAdvBitmap.CreateFromFile('logo.png');
  try
    r := RectF(ARect.Left, ARect.Top, ARect.Left + 200, ARect.Bottom);
    InflateREctEx(r, -2, -2);
    AGraphics.DrawBitmap(r, b);
  finally
    b.Free;
  end;
end;

procedure TForm22.AdvPDFViewer1BeforeDrawHeader(Sender: TObject;
  APageIndex: Integer; AHeader: string; ARect: TRectF; AGraphics: TAdvGraphics;
  var ADefaultDraw: Boolean);
begin
  AGraphics.Fill.Color := gcWhiteSmoke;
  AGraphics.Fill.ColorTo := Lighter(gcSilver, 30);
  AGraphics.Fill.Kind := gfkGradient;
  AGraphics.DrawRectangle(ARect);
end;

procedure TForm22.AdvPDFViewer1Loaded(Sender: TObject);
begin
  AdvPDFViewer1.Options.Header := ExtractFileName(AdvPDFViewer1.FileName);
end;

procedure TForm22.AdvPDFViewer1PageChanged(Sender: TObject;
  APageIndex: Integer);
begin
  AdvPDFViewer1.Options.Footer := 'Page Changed to ' + (APageIndex + 1).ToString;
end;

procedure TForm22.Button1Click(Sender: TObject);
begin
  AdvPDFViewer1.PreviousPage;
end;

procedure TForm22.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    AdvPDFViewer1.FileName := OpenDialog1.FileName;
end;

procedure TForm22.Button3Click(Sender: TObject);
begin
  AdvPDFViewer1.NextPage;
end;

procedure TForm22.Button4Click(Sender: TObject);
begin
  AdvPDFViewer1.FirstPage;
end;

procedure TForm22.Button5Click(Sender: TObject);
begin
  AdvPDFViewer1.LastPage;
end;

type
  T = class(TAdvPDFViewerOptions);

procedure TForm22.FormCreate(Sender: TObject);
begin
  AdvPDFViewer1.FileName := ExtractFilePath(ParamStr(0)) + 'RAD-Studio-Guide-for-Managers.pdf';
  AdvPDFViewer1.Options.Thumbnails := True;
  AdvPDFViewer1.Options.DisplayMode := dmContinuousTopToBottom;
end;

procedure TForm22.RadioGroup1Click(Sender: TObject);
begin
  AdvPDFViewer1.Options.DisplayMode := TAdvPDFViewerDisplayMode(RadioGroup1.ItemIndex);
end;

end.
