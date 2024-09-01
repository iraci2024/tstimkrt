program CalculatedFieldsDemoDXE3;

uses
  Forms,
  CalculatedFieldsDemoMain in 'CalculatedFieldsDemoMain.pas' {frmMain},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid Calculated Fields Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
