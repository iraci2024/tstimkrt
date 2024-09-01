program SummaryGroupDemoD2010;

uses
  Forms,
  SummaryGroupDemoMain in 'SummaryGroupDemoMain.pas' {SummaryGroupDemoMainForm},
  SummaryGroupDemoData in 'SummaryGroupDemoData.pas' {SummaryGroupDemoDataDM: TDataModule},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  DemoUtils in '..\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid SummaryGroupDemoD2010';
  Application.CreateForm(TSummaryGroupDemoDataDM, SummaryGroupDemoDataDM);
  Application.CreateForm(TSummaryGroupDemoMainForm, SummaryGroupDemoMainForm);
  Application.Run;
end.
