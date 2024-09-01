program ViewCardDemoD2010;

uses
  Forms,
  ViewCardDemoMain in 'ViewCardDemoMain.pas' {ViewCardDemoMainForm},
  ViewCardDemoData in 'ViewCardDemoData.pas' {ViewCardDemoDataDM: TDataModule},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid ViewCard Demo';
  Application.CreateForm(TViewCardDemoMainForm, ViewCardDemoMainForm);
  Application.CreateForm(TViewCardDemoDataDM, ViewCardDemoDataDM);
  Application.Run;
end.
