unit UEdgePrintDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TMSFNCTypes, FMX.TMSFNCUtils, FMX.TMSFNCGraphics, FMX.TMSFNCGraphicsTypes,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.TMSFNCCustomControl,
  FMX.TMSFNCWebBrowser.win, FMX.TMSFNCEdgeWebBrowser, FMX.Edit,
  FMX.TMSFNCWebBrowser;

type
  TForm2 = class(TForm)
    TMSFNCEdgeWebBrowser1: TTMSFNCEdgeWebBrowser;
    PrintUIBtn: TButton;
    NavigateBtn: TButton;
    PrintToPDFStreamBtn: TButton;
    PrintBtn: TButton;
    PrintToPDFBtn: TButton;
    URLEdit: TEdit;
    procedure NavigateBtnClick(Sender: TObject);
    procedure PrintUIBtnClick(Sender: TObject);
    procedure PrintToPDFStreamBtnClick(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure PrintToPDFBtnClick(Sender: TObject);
    procedure TMSFNCEdgeWebBrowser1GetPrintPDFStream(Sender: TObject;
      AStream: TMemoryStream);
    procedure TMSFNCEdgeWebBrowser1Initialized(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

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
  if TMSFNCEdgeWebBrowser1.URL <> '' then
    URLEdit.Text := TMSFNCEdgeWebBrowser1.URL;
end;

procedure TForm2.NavigateBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.Navigate(URLEdit.Text);
end;

procedure TForm2.PrintToPDFStreamBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.PrintToPDFStream;
end;

procedure TForm2.PrintBtnClick(Sender: TObject);
var
  ps: TTMSFNCWebBrowserPrintSettings;
begin
  ps := TMSFNCEdgeWebBrowser1.InitialPrintSettings;
  ps.PrintHeaderAndFooter := False;
  ps.PrintBackgrounds := True;
  ps.Orientation := poLandscape;
  TMSFNCEdgeWebBrowser1.Print(ps);
end;

procedure TForm2.PrintToPDFBtnClick(Sender: TObject);
begin
  TMSFNCEdgeWebBrowser1.PrintToPDF(GetCurrentDir + '/test.pdf');
end;

end.
