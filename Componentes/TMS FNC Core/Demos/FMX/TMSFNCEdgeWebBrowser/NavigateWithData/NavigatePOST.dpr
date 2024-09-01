program NavigatePOST;

uses
  System.StartUpCopy,
  FMX.Forms,
  UNavigatePost in 'UNavigatePost.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
