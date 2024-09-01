program PuzzleDemoD2010;

uses
  Forms,
  Puzzle in 'Puzzle.pas' {frmPuzzle};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'PuzzleDemoD2010';
  Application.CreateForm(TfrmPuzzle, frmPuzzle);
  Application.Run;
end.
