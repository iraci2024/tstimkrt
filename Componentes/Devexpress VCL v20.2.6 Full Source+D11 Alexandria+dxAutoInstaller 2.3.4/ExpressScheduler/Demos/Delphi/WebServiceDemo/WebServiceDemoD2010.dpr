program WebServiceDemoD2010;

uses
  Forms,
  WebServiceDemoMain in 'WebServiceDemoMain.pas' {WebServiceDemoMainForm},
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  WebServiceDemoSetupForm in 'WebServiceDemoSetupForm.pas' {WebServiceDemoSetupWizard};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressScheduler WebServiceDemoD2010';
  Application.CreateForm(TWebServiceDemoMainForm, WebServiceDemoMainForm);
  Application.Run;
end.
