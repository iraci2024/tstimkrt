program PuzzleDemoD104Sydney;

uses
  Forms,
  Puzzle in 'Puzzle.pas' {frmPuzzle};

  {$R *.res}
  {$R WindowsXP.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'PuzzleDemoD104Sydney';
  Application.CreateForm(TfrmPuzzle, frmPuzzle);
  Application.Run;
end.
