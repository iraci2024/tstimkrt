program StackoverflowFeed;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMain in 'UMain.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
