program UnboundListDemoDXE3;

uses
  Forms,
  UnboundListDemoMain in 'UnboundListDemoMain.pas' {UnboundListDemoMainForm},
  UnboundListDemoClasses in 'UnboundListDemoClasses.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid UnboundList Demo';
  Application.CreateForm(TUnboundListDemoMainForm, UnboundListDemoMainForm);
  Application.Run;
end.
