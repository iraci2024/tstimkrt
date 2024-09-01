program GanttViewDemoD100Seattle;

uses
  Forms,
  GanttViewDemoMain in 'GanttViewDemoMain.pas' {GanttViewDemoMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressScheduler GanttViewDemoD100Seattle';
  Application.CreateForm(TGanttViewDemoMainForm, GanttViewDemoMainForm);
  Application.Run;
end.
