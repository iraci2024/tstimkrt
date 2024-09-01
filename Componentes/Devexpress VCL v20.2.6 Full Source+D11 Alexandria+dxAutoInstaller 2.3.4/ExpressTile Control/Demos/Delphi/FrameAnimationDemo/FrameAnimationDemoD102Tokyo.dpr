program FrameAnimationDemoD102Tokyo;

uses
  Forms,
  DemoUtils in '..\Common\DemoUtils.pas',
  AboutDemoForm in '..\Common\AboutDemoForm.pas',
  FrameAnimationDemoMain in 'FrameAnimationDemoMain.pas',
  FrameAnimationDemoDM in 'FrameAnimationDemoDM.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressTileControl Frame Animation Demo';
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TfmFrameAnimationMain, fmFrameAnimationMain);
  Application.Run;
end.
