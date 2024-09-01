program GroupControlDemoDXE2;

uses
  Forms,
  GroupControlMain in 'GroupControlMain.pas' {fmGroupControlMain};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmGroupControlMain, fmGroupControlMain);
  Application.Run;
end.
