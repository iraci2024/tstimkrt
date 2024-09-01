program RatingControlDemoD104Sydney;

uses
  Forms,
  RatingControlDemoMain in 'RatingControlDemoMain.pas' {frmMain},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  RatingControlDemoImagePicker in 'RatingControlDemoImagePicker.pas' {frmImagePicker};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid Rating Control Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmImagePicker, frmImagePicker);
  Application.Run;
end.
