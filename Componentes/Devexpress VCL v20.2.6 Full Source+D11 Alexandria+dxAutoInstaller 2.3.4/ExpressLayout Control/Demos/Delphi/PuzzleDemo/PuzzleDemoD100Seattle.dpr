program PuzzleDemoD100Seattle;

uses
  Forms,
  Puzzle in 'Puzzle.pas' {frmPuzzle};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'PuzzleDemoD100Seattle';
  Application.CreateForm(TfrmPuzzle, frmPuzzle);
  Application.Run;
end.
