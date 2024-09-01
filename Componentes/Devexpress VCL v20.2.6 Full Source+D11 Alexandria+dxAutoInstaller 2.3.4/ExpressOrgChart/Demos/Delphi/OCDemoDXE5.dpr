program OCDemoDXE5;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Options in 'Options.pas' {OptionsForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.
