unit UEdgePrinting;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VCL.TMSFNCTypes, VCL.TMSFNCUtils,
  VCL.TMSFNCGraphics, VCL.TMSFNCGraphicsTypes, VCL.TMSFNCCustomControl,
  VCL.TMSFNCWebBrowser, VCL.TMSFNCEdgeWebBrowser, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    PrintUIBtn: TButton;
    PrintToPDFStreamBtn: TButton;
    PrintBtn: TButton;
    PrintToPDFBtn: TButton;
    NavigateBtn: TButton;
    URLEdit: TEdit;
    TMSFNCEdgeWebBrowser1: TTMSFNCEdgeWebBrowser;
    procedure NavigateBtnClick(Sender: TObject);
    procedure PrintUIBtnClick(Sender: TObject);
    procedure PrintToPDFStreamBtnClick(Sender: TObject);
    procedure TMSFNCEdgeWebBrowser1GetPrintPDFStream(Sender: TObject;
      AStream: TMemoryStream);
    procedure PrintToPDFBtnClick(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure TMSFNCEdgeWebBrowser1Initialized(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.NavigateBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.Navigate(URLEdit.Text);
end;

procedure TForm2.PrintBtnClick(Sender: TObject);
var
  ps: TTMSFNCWebBrowserPrintSettings;
begin
  ps := TMSFNCEdgeWebBrowser1.InitialPrintSettings;
  ps.PrintHeaderAndFooter := False;
  ps.Orientation := poLandscape;
  TMSFNCEdgeWebBrowser1.Print(ps);
end;

procedure TForm2.PrintToPDFBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.PrintToPDF(GetCurrentDir + '/test.pdf');
end;

procedure TForm2.PrintToPDFStreamBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.PrintToPDFStream;
end;

procedure TForm2.PrintUIBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.ShowPrintUI;
end;

procedure TForm2.TMSFNCEdgeWebBrowser1GetPrintPDFStream(Sender: TObject; AStream: TMemoryStream);
begin
  if Assigned(AStream) then
    AStream.SaveToFile('PDFStream.pdf');
end;

procedure TForm2.TMSFNCEdgeWebBrowser1Initialized(Sender: TObject);
begin
  if Pos('http', TMSFNCEdgeWebBrowser1.URL) > 0 then
    URLEdit.Text := TMSFNCEdgeWebBrowser1.URL;
end;

end.
