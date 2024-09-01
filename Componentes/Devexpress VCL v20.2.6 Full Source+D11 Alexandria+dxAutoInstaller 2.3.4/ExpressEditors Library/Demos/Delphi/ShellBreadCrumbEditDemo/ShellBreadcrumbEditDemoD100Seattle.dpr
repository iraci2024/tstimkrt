program ShellBreadcrumbEditDemoD100Seattle;

uses
  Forms,
  AboutDemoForm in '..\AboutDemoForm.pas',
  ShellBreadcrumbEditDemoMain in 'ShellBreadcrumbEditDemoMain.pas' {dxBreadcrumbEditDemoForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdxBreadcrumbEditDemoForm, dxBreadcrumbEditDemoForm);
  Application.Run;
end.
