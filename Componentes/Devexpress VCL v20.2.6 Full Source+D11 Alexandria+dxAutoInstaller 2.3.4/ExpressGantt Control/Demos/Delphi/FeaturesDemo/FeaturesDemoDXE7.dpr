program FeaturesDemoDXE7;

uses
  Forms,
  FeaturesDemoMain in 'FeaturesDemoMain.pas' {FeaturesDemoMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressGanttControl Features Demo';
  Application.CreateForm(TFeaturesDemoMainForm, FeaturesDemoMainForm);
  Application.Run;
end.
