program UnboundDemoDXE7;

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
  Application.Title := 'ExpressScheduler UnboundDemoDXE7';
  Application.CreateForm(TUnboundDemoMainForm, UnboundDemoMainForm);
  Application.Run;
end.
