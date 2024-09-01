program OrgChartReportLinkD101Berlin;

uses
  Forms,
  OrgChartRLMain in 'OrgChartRLMain.pas' {OrgChartRLMainForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Report Links Demo - ExpressOrgChart & ExpressDBOrgChart';
  Application.CreateForm(TOrgChartRLMainForm, OrgChartRLMainForm);
  Application.Run;
end.
