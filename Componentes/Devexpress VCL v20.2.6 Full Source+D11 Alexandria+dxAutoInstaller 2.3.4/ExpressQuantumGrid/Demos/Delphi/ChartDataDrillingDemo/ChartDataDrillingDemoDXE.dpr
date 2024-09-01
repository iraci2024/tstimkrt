program ChartDataDrillingDemoDXE;

uses
  Forms,
  ChartDataDrillingDemoMain in 'ChartDataDrillingDemoMain.pas' {frmMain},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid ChartDataDrilling Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
