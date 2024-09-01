program UnboundExternalDataDemoD100Seattle;

uses
  Forms,
  UnboundExternalDataDemoMain in 'UnboundExternalDataDemoMain.pas' {UnboundExternalDataDemoMainForm},
  UnboundExternalDataDemoClasses in 'UnboundExternalDataDemoClasses.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid UnboundExternalData Demo';
  Application.CreateForm(TUnboundExternalDataDemoMainForm, UnboundExternalDataDemoMainForm);
  Application.Run;
end.
