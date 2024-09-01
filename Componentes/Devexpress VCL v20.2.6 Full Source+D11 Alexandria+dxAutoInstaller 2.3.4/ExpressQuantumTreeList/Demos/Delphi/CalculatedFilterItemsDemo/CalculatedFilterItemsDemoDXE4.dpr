program CalculatedFilterItemsDemoDXE4;

uses
  Forms,
  CarsData in '..\Common\CarsData.pas' {dmCars: TDataModule},
  CalculatedFilterItemsDemoMain in 'CalculatedFilterItemsDemoMain.pas' {frmMain},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumTreeList Calculated Filter Items Demo';
  Application.CreateForm(TdmCars, dmCars);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
