program BreadcrumbEditDemoDXE6;

uses
  Forms,
  BaseForm in '..\BaseForm.pas',
  DemoUtils in '..\DemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas',
  BreadcrumbEditDemoMain in 'BreadcrumbEditDemoMain.pas' {dxBreadcrumbEditDemoForm},
  BreadcrumbEditDemoRecentPaths in 'BreadcrumbEditDemoRecentPaths.pas' {dxBreadcrumbEditDemoRecentPathsForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdxBreadcrumbEditDemoForm, dxBreadcrumbEditDemoForm);
  Application.Run;
end.
