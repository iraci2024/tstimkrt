program GridViewADODemoDXE3;

uses
  Forms,
  GridViewDemoData in 'GridViewDemoData.pas' {GridViewDemoDataDM},
  GridViewDemoConnection in 'GridViewDemoConnection.pas' {GridViewDemoConnectionForm},
  GridViewDemoMain in 'GridViewDemoMain.pas' {GridViewDemoMainForm},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  EMFGridDemo.Entities in '..\Common\EMFGridDemo.Entities.pas',
  EMFGridDemo.Linq in '..\Common\EMFGridDemo.Linq.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TGridViewDemoConnectionForm, GridViewDemoConnectionForm);
  Application.CreateForm(TGridViewDemoDataDM, GridViewDemoDataDM);
  Application.CreateForm(TGridViewDemoMainForm, GridViewDemoMainForm);
  Application.Run;
end.
