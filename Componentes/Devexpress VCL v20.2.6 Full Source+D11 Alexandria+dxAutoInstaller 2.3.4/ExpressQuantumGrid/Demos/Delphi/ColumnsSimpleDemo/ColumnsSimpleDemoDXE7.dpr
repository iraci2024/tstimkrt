program ColumnsSimpleDemoDXE7;

uses
  Forms,
  CarsData in '..\Common\CarsData.pas' {dmCars},
  CarsDataForGrid in '..\Common\CarsDataForGrid.pas' {dmGridCars},
  ColumnsSimpleDemoMain in 'ColumnsSimpleDemoMain.pas' {ColumnsSimpleDemoMainForm},
  ColumnsSimpleDemoData in 'ColumnsSimpleDemoData.pas' {ColumnsSimpleDemoDataDM: TDataModule},
  ColumnsSimpleDemoCities in 'ColumnsSimpleDemoCities.pas' {ColumnsSimpleDemoCitiesForm},
  ColumnsSimpleDemoCars in 'ColumnsSimpleDemoCars.pas' {ColumnSimpleDemoCarsForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  DemoUtils in '..\DemoUtils.pas',
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  BaseForm in '..\BaseForm.pas' {fmBaseForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid ColumnsSimpleDemoDXE7';
  Application.CreateForm(TdmGridCars, dmGridCars);
  Application.CreateForm(TColumnsSimpleDemoDataDM, ColumnsSimpleDemoDataDM);
  Application.CreateForm(TColumnsSimpleDemoMainForm, ColumnsSimpleDemoMainForm);
  Application.CreateForm(TColumnsSimpleDemoCitiesForm, ColumnsSimpleDemoCitiesForm);
  Application.CreateForm(TColumnSimpleDemoCarsForm, ColumnSimpleDemoCarsForm);
  Application.Run;
end.
