program AgendaViewDemoD101Berlin;

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
  Application.Title := 'ExpressScheduler AgendaViewDemoD101Berlin';
  Application.CreateForm(TAgendaViewDemoMainForm, AgendaViewDemoMainForm);
  Application.Run;
end.
