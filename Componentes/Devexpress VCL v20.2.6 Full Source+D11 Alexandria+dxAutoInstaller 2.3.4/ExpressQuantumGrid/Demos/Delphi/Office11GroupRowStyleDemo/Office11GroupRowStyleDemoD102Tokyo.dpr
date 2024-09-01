program Office11GroupRowStyleDemoD102Tokyo;

uses
  Forms,
  Office11GroupRowStyleDemoMain in 'Office11GroupRowStyleDemoMain.pas' {Office11GroupRowStyleDemoMainForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  DemoUtils in '..\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid Office11GroupRowStyle Demo';
  Application.CreateForm(TOffice11GroupRowStyleDemoMainForm, Office11GroupRowStyleDemoMainForm);
  Application.Run;
end.
