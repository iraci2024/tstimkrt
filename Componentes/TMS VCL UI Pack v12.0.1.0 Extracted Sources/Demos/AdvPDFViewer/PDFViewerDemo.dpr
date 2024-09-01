program PDFViewerDemo;

uses
  Vcl.Forms,
  UPDFViewerDemo in 'UPDFViewerDemo.pas' {Form22};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm22, Form22);
  Application.Run;
end.
