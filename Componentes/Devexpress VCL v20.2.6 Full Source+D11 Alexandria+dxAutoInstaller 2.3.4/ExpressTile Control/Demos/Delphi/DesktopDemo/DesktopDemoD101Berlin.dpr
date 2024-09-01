program DesktopDemoD101Berlin;

uses
  Forms,
  DesktopDemoMain in 'DesktopDemoMain.pas' {DesktopDemoMainForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDesktopDemoMainForm, DesktopDemoMainForm);
  Application.Run;
end.
