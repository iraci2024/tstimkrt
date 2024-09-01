program MasterDetailEMFTableDemoD101Berlin;

uses
  Forms,
  MasterDetailEMFTableDemoMain in 'MasterDetailEMFTableDemoMain.pas' {MasterDetailEMFTableDemoMainForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  DemoUtils in '..\DemoUtils.pas',
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  NWindFoodsModel.Entities in 'NWindFoodsModel.Entities.pas',
  NWindFoodsModel.Linq in 'NWindFoodsModel.Linq.pas';

  {$R *.res}
  {$R WindowsXP.res}

  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid Master Detail Table Demo';
  Application.CreateForm(TMasterDetailEMFTableDemoMainForm, MasterDetailEMFTableDemoMainForm);
  Application.Run;
end.
