program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDemo in 'UDemo.pas' {Form130};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm130, Form130);
  Application.Run;
end.
