program Dbtree_fD102Tokyo;

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
