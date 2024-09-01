program ColumnsMultiEditorsDemoDXE2;

uses
  Forms,
  ColumnsMultiEditorsDemoMain in 'ColumnsMultiEditorsDemoMain.pas' {ColumnsMultiEditorsDemoMainForm},
  ColumnsMultiEditorsDemoData in 'ColumnsMultiEditorsDemoData.pas' {ColumnsMultiEditorsDemoDataDM: TDataModule},
  DemoRating in '..\Common\DemoRating.pas' {DemoRatingForm},
  DemoBasicMain in '..\Common\DemoBasicMain.pas' {DemoBasicMainForm},
  ColumnsMultiEditorsDemoPopup in 'ColumnsMultiEditorsDemoPopup.pas' {ColumnsMultiEditorsDemoPopupForm},
  AboutDemoForm in '..\Common\AboutDemoForm.pas',
  SkinDemoUtils in '..\Common\SkinDemoUtils.pas',
  DemoUtils in '..\Common\DemoUtils.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumTreeList ColumnsMultiEditorsDemoDXE2 ';
  Application.HelpFile := '..\..\Help\ExpressQuantumTreeList.hlp';
  Application.CreateForm(TColumnsMultiEditorsDemoDataDM, ColumnsMultiEditorsDemoDataDM);
  Application.CreateForm(TColumnsMultiEditorsDemoMainForm, ColumnsMultiEditorsDemoMainForm);
  Application.Run;
end.
