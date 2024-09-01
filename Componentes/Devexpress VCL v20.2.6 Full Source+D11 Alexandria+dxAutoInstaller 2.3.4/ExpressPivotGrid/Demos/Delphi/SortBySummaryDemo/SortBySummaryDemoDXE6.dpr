program SortBySummaryDemoDXE6;

uses
  Forms,
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {frmDemoBasicMain},
  AboutDemoForm in '..\Common\AboutDemoForm.pas' {formAboutDemo},
  DemoBasicDM in '..\Common\DemoBasicDM.pas' {dmOrders: TDataModule},
  SortBySummaryMain in 'SortBySummaryMain.pas' {frmSortBySummary},
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmOrders, dmOrders);
  Application.CreateForm(TfrmSortBySummary, frmSortBySummary);
  Application.Run;
end.
