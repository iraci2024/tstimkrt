program NavigateWithData;

uses
  Vcl.Forms,
  UNavigateWithData in 'UNavigateWithData.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
