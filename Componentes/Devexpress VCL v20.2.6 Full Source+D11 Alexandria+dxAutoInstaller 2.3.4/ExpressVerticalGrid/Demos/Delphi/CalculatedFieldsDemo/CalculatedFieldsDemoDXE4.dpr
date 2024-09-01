program CalculatedFieldsDemoDXE4;

uses
  Forms,
  CalculatedFieldsDemoMain in 'CalculatedFieldsDemoMain.pas' {frmMain},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicAbout in '..\Common\DemoBasicAbout.pas' {DemoBasicAboutForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.Title := 'ExpressQuantumGrid Calculated Fields Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
