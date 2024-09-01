program DragDropDemoDXE7;

uses
  Forms,
  DragDropMain in 'DragDropMain.pas' {fmDragDropMain},
  NavBarUtils in '..\Common\NavBarUtils.pas' {dmCommonData: TDataModule};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmCommonData, dmCommonData);
  Application.CreateForm(TfmDragDropMain, fmDragDropMain);
  Application.Run;
end.
