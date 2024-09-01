program EditorsValidationDemoDXE3;

uses
  Forms,
  EditorsValidationDemoMain in 'EditorsValidationDemoMain.pas' {EditorsValidationDemoMainForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  DemoUtils in '..\DemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressEditors Validation Demo';
  Application.CreateForm(TEditorsValidationDemoMainForm, EditorsValidationDemoMainForm);
  Application.Run;
end.
