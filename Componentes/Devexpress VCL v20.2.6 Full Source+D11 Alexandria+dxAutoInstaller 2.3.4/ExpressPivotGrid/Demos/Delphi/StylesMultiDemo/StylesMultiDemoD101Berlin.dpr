program StylesMultiDemoD101Berlin;

uses
  Forms,
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {frmDemoBasicMain},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicDM in '..\Common\DemoBasicDM.pas' {dmOrders: TDataModule},
  StylesMultiMain in 'StylesMultiMain.pas' {frmStylesMulti},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmOrders, dmOrders);
  Application.CreateForm(TfrmStylesMulti, frmStylesMulti);
  Application.Run;
end.
