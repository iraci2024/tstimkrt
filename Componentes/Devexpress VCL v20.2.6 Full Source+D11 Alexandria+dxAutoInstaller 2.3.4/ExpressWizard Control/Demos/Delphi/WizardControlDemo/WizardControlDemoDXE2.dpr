program WizardControlDemoDXE2;

uses
  Forms,
  WizardControlDemoMainForm in 'WizardControlDemoMainForm.pas' {frmWizardControlDemoMain},
  WizardControlDemoSetupForm in 'WizardControlDemoSetupForm.pas' {frmWizardControlDemoSetup};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmWizardControlDemoMain, frmWizardControlDemoMain);
  Application.Run;
end.
