program BandedFixedDemoD100Seattle;

uses
  Forms,
  BandedFixedDemoMain in 'BandedFixedDemoMain.pas' {BandedFixedDemoMainForm},
  BandedFixedDemoData in 'BandedFixedDemoData.pas' {BandedFixedDemoDataDM: TDataModule},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumTreeList BandedFixedDemoD100Seattle';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TBandedFixedDemoMainForm, BandedFixedDemoMainForm);
  Application.CreateForm(TBandedFixedDemoDataDM, BandedFixedDemoDataDM);
  Application.Run;
end.
