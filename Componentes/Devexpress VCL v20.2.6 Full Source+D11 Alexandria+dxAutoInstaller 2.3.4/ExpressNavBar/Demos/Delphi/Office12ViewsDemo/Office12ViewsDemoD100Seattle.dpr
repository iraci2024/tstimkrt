program Office12ViewsDemoD100Seattle;

uses
  Forms,
  Office12ViewsMain in 'Office12ViewsMain.pas' {fmMain};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
