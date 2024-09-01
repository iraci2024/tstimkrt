program EdgePrintDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UEdgePrintDemo in 'UEdgePrintDemo.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
