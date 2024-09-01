program PuzzleDemoD102Tokyo;

uses
  Forms,
  Puzzle in 'Puzzle.pas' {frmPuzzle};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'PuzzleDemoD102Tokyo';
  Application.CreateForm(TfrmPuzzle, frmPuzzle);
  Application.Run;
end.
