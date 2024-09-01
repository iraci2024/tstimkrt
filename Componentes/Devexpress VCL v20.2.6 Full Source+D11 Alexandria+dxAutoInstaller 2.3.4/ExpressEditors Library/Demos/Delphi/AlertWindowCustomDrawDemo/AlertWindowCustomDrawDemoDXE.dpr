program AlertWindowCustomDrawDemoDXE;

uses
  Forms,
  DemoUtils in '..\DemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  AlertWindowCustomDrawDemoMain in 'AlertWindowCustomDrawDemoMain.pas' {fmAlertWindowCustomDraw};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmAlertWindowCustomDraw, fmAlertWindowCustomDraw);
  Application.Run;
end.
