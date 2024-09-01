program CookiesDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UCookiesDemo in 'UCookiesDemo.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
