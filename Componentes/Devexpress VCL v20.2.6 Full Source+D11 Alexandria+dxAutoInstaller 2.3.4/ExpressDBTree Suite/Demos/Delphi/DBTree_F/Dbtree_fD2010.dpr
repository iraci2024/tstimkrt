program Dbtree_fD2010;

uses
  Forms,
  main in 'main.pas' {Form1},
  dbview in 'dbview.pas' {Form2};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
