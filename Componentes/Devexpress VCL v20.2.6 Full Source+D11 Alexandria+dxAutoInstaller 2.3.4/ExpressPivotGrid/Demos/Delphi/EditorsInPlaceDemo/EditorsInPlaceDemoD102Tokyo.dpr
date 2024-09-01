program EditorsInPlaceDemoD102Tokyo;

uses
  Forms,
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {frmDemoBasicMain},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicDM in '..\Common\DemoBasicDM.pas' {dmOrders: TDataModule},
  EditorsInPlaceDemoMain in 'EditorsInPlaceDemoMain.pas' {frmEditorsInPlace},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmOrders, dmOrders);
  Application.CreateForm(TfrmEditorsInPlace, frmEditorsInPlace);
  Application.Run;
end.
