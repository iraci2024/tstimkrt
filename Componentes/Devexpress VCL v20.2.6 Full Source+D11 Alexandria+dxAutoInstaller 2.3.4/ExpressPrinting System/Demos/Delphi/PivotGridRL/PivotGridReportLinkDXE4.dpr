program PivotGridReportLinkDXE4;

uses
  Forms,
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  PivotGridRLMain in 'PivotGridRLMain.pas' {PivotGridRLMainForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Express Report Link Demo - ExpressPivotGrid';
  Application.CreateForm(TPivotGridRLMainForm, PivotGridRLMainForm);
  Application.Run;
end.
