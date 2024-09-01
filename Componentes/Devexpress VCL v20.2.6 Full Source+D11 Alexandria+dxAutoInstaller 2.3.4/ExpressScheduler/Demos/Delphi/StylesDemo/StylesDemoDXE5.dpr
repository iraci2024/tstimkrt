program StylesDemoDXE5;

uses
  Forms,
  StylesMainUnit in 'StylesMainUnit.pas' {StylesMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressScheduler StylesDemoDXE5';
  Application.CreateForm(TStylesMainForm, StylesMainForm);
  Application.Run;
end.
