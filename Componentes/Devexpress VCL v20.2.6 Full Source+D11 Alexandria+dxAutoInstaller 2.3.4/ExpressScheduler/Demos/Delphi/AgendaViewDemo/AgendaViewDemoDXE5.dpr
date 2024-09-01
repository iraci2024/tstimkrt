program AgendaViewDemoDXE5;

uses
  Forms,
  AgendaViewDemoMain in 'AgendaViewDemoMain.pas' {AgendaViewDemoMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressScheduler AgendaViewDemoDXE5';
  Application.CreateForm(TAgendaViewDemoMainForm, AgendaViewDemoMainForm);
  Application.Run;
end.
