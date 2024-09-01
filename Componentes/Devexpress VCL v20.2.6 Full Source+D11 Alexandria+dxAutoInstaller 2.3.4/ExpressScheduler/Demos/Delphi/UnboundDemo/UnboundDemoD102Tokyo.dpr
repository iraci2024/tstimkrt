program UnboundDemoD102Tokyo;

uses
  Forms,
  UnboundDemoMain in 'UnboundDemoMain.pas' {UnboundDemoMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressScheduler UnboundDemoD102Tokyo';
  Application.CreateForm(TUnboundDemoMainForm, UnboundDemoMainForm);
  Application.Run;
end.
