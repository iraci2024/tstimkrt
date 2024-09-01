program FilterControlDemoDXE6;

uses
  Forms,
  CarsData in '..\Common\CarsData.pas' {dmCars},
  CarsDataForGrid in '..\Common\CarsDataForGrid.pas' {dmGridCars},
  FilterControlDemoMain in 'FilterControlDemoMain.pas' {FilterControlDemoMainForm},
  FilterControlDemoData in 'FilterControlDemoData.pas' {FilterControlDemoDataDM: TDataModule},
  FilterControlDemoFilterDialog in 'FilterControlDemoFilterDialog.pas' {FilterControlDemoFilterDialogForm},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid FilterControlDemoDXE6';
  Application.CreateForm(TdmGridCars, dmGridCars);
  Application.CreateForm(TFilterControlDemoDataDM, FilterControlDemoDataDM);
  Application.CreateForm(TFilterControlDemoMainForm, FilterControlDemoMainForm);
  Application.CreateForm(TFilterControlDemoFilterDialogForm, FilterControlDemoFilterDialogForm);
  Application.Run;
end.
