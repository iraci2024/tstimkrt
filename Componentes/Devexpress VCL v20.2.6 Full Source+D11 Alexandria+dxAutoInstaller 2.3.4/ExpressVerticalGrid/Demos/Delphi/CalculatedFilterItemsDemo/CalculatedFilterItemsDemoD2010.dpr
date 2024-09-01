program CalculatedFilterItemsDemoD2010;

uses
  Forms,
  CarsData in '..\Common\CarsData.pas' {dmCars: TDataModule},
  CalculatedFilterItemsDemoMain in 'CalculatedFilterItemsDemoMain.pas' {frmMain},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicAbout in '..\Common\DemoBasicAbout.pas' {DemoBasicAboutForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid Calculated Filter Items Demo';
  Application.CreateForm(TdmCars, dmCars);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
