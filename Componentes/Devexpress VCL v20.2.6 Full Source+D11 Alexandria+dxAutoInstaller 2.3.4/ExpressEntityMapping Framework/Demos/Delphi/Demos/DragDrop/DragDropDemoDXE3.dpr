program DragDropDemoDXE3;

uses
  Forms,
  DragDropDemoDictionaries in 'DragDropDemoDictionaries.pas' {DragDropDemoDictionariesForm},
  AboutDemoForm in '..\AboutDemoForm.pas' {formAboutDemo},
  BaseForm in '..\BaseForm.pas' {fmBaseForm},
  DemoUtils in '..\DemoUtils.pas',
  SkinDemoUtils in '..\SkinDemoUtils.pas',
  FilmsDemoData in '..\Common\FilmsDemoData.pas' {FilmsDemoDM: TDataModule},
  DragDropDemoMain in 'DragDropDemoMain.pas' {DragDropDemoMainForm},
  Films.Entities in '..\Common\Films.Entities.pas',
  Films.Linq in '..\Common\Films.Linq.pas';

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ExpressQuantumGrid DragDropDemoDXE3';
  Application.CreateForm(TFilmsDemoDM, FilmsDemoDM);
  Application.CreateForm(TDragDropDemoMainForm, DragDropDemoMainForm);
  Application.CreateForm(TDragDropDemoDictionariesForm, DragDropDemoDictionariesForm);
  Application.Run;
end.
