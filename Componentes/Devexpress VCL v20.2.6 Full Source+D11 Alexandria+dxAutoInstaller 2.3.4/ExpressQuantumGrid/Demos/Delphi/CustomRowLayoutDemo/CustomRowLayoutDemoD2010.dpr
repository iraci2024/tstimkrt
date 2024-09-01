program CustomRowLayoutDemoD2010;

uses
  Forms,
  DevAVData in '..\Common\DevAVData.pas' {dmDevAV: TDataModule},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  CustomRowLayoutMain in 'CustomRowLayoutMain.pas' {frmMain},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\SkinDemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid InlineEditor Demo';
  Application.CreateForm(TdmDevAV, dmDevAV);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
